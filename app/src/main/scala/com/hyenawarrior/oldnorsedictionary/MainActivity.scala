package com.hyenawarrior.oldnorsedictionary

import java.io.{File, FileInputStream, FileOutputStream}

import android.content.Intent
import android.os.{Bundle, Environment}
import android.support.v7.app.AppCompatActivity
import android.util.Log
import android.view.View
import android.widget.{ListView, SearchView}
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{DUAL, PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.{ACTIVE, MEDIO_PASSIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.{Word => GWord, _}
import com.hyenawarrior.oldnorsedictionary.model.database.IGPersister
import com.hyenawarrior.oldnorsedictionary.model.persister.database.AndroidSDBLayer
import com.hyenawarrior.oldnorsedictionary.model.{DictionaryEntry, DictionaryListItem}
import com.hyenawarrior.oldnorsedictionary.modelview.DictionaryEntryAdapter
import com.hyenawarrior.oldnorsedictionary.new_word.{AddNewWordActivityPager, DetailedDictionaryEntry}
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef

import scala.language.postfixOps

object Orderings
{
	def sgnDiff[T](a: T, b: T)(implicit ordering: Ordering[T]): Int = ordering.compare(a, b)

	implicit object NumberOrdering extends Ordering[GNumber]
	{
		override def compare(x: GNumber, y: GNumber): Int = (x, y) match
		{
			case (SINGULAR, DUAL | PLURAL) => -1
			case (DUAL | PLURAL, SINGULAR) => 1
			case _ => 0
		}
	}

	implicit object CaseOrdering extends Ordering[Case]
	{
		override def compare(x: Case, y: Case): Int = math.signum(x.id - y.id)
	}

	implicit object WordOrdering extends Ordering[GWord]
	{
		override def compare(x: GWord, y: GWord): Int = (x, y) match
		{
			case (GWord(Noun(_, (n1, c1), _, _)), GWord(Noun(_, (n2, c2), _, _))) if n1 != n2 => sgnDiff(n1, n2)
			case (GWord(Noun(_, (n1, c1), _, _)), GWord(Noun(_, (n2, c2), _, _))) => sgnDiff(c1, c2)
			case _ => 0
		}
	}

	implicit object MeaningDefOrdering extends Ordering[MeaningDef]
	{
		override def compare(x: MeaningDef, y: MeaningDef): Int = x.meaning.compareTo(y.meaning)
	}
}

object MainActivity
{
	val EXTRA_MESSAGE: String = "abc"
}

class MainActivity extends AppCompatActivity
{
	outer =>

	object TypeListener extends SearchView.OnQueryTextListener
	{
		override def onQueryTextSubmit(s: String): Boolean = true

		override def onQueryTextChange(str: String): Boolean = {

      val fixedStr = str.replace("ö", "ǫ")

      val entries = if(fixedStr.isEmpty) List() else igPersister.lookup(fixedStr)
        .map {
          case DictionaryEntry(sv: StrongVerb, meanings) =>

            val matchingForms = sv.verbForms
              .filter {
                case (_, v) if v.strForm.startsWith(fixedStr) => true
                case _ => false
              }

            val INF_KEY = (INFINITIVE, ACTIVE, None, None)

            val priForm = sv.verbForms(INF_KEY).strForm -> abbrevationOf(INF_KEY)

            val formsToShow = filter(matchingForms) match {

              case Some((INF_KEY, f)) => Seq(priForm)
              case Some((k, v)) => Seq(priForm, v.strForm -> abbrevationOf(k))
              case None => Seq()
            }

            DictionaryListItem(formsToShow, "verb", sv, meanings)

          //case DictionaryEntry(noun, meanings) =>
        }
          .toList

			entryListAdapter resetItems entries
			listView.invalidateViews()
			true
		}
	}

	private def filter(forms: Map[VerbType, StrongVerbForm]): Option[(VerbType, StrongVerbForm)] = {

		val fs = forms.groupBy { case ((_, voice, _, _), _) => voice }

		val activeVoicedForms = fs.get(ACTIVE)
		val medioPassiveVoicedForms = fs.get(MEDIO_PASSIVE)

		activeVoicedForms.flatMap(splitByMood) orElse medioPassiveVoicedForms.flatMap(splitByMood)
	}

  private def splitByMood(forms: Map[VerbType, StrongVerbForm]): Option[(VerbType, StrongVerbForm)] = {

    val fs = forms.groupBy { case ((md, _, _, _), _) => md }

    val inf = fs.get(INFINITIVE).map(_.head)
		val prtcp = fs.get(PARTICIPLE).map(_.head)

    inf orElse fs.get(INDICATIVE).flatMap(splitByTense) orElse prtcp
  }

  private def splitByTense(forms: Map[VerbType, StrongVerbForm]): Option[(VerbType, StrongVerbForm)] = {

    val fs = forms.groupBy { case ((_, _, Some(t), _), _) => t }

    val present = fs.get(PRESENT).flatMap(splitByNumber)
    val past = fs.get(PAST).flatMap(splitByNumber)

    present orElse past
  }

  private def splitByNumber(forms: Map[VerbType, StrongVerbForm]): Option[(VerbType, StrongVerbForm)] = {

    val fs = forms.groupBy { case ((_, _, _, Some(Pronoun(n, _))), _) => n }

    val sg = fs.get(SINGULAR).flatMap(select)
    val pl = fs.get(PLURAL).flatMap(select)

    sg orElse pl
  }

  private def select(forms: Map[VerbType, StrongVerbForm]): Option[(VerbType, StrongVerbForm)] = forms
    .find {
      case ((_, _, _, Some(Pronoun(_, 3))), _) => true
      case _ => false
    }
    .orElse(forms.headOption)


  private def abbrevationOf(form: VerbType): String ={

    val (mood, voice, optTense, optPronoun) = form

    val md = Some(abbrevationOfMood(mood))
    val vc = Some(abbrevationOf(voice))
    val ts = optTense.map(abbrevationOfTense)
    val pr = optPronoun.map(abbrevationOfPronoun)

    Seq(ts, md, pr, vc).flatten.mkString(" ")
  }

  private def abbrevationOfMood(mood: VerbModeEnum): String = mood match {

    case INFINITIVE => "INF"
    case INDICATIVE => "IND"
    case SUBJUNCTIVE => "SBJV"
    case IMPERATIVE => "IMP"
		case PARTICIPLE => "PTCP"
  }

  private def abbrevationOf(voice: VerbVoice): String = voice match {

    case ACTIVE => "ACT"
    case MEDIO_PASSIVE => "MID-PAS"
  }

  private def abbrevationOfTense(tense: VerbTenseEnum): String = tense match {

    case PRESENT => "PRS"
    case PAST => "PST"
  }

  private def abbrevationOfPronoun(pronoun: Pronoun): String = pronoun match {

    case Pronoun.SG_1 => "SG1"
    case Pronoun.SG_2 => "SG2"
    case Pronoun.SG_3 => "SG3"

    case Pronoun.PL_1 => "PL1"
    case Pronoun.PL_2 => "PL2"
    case Pronoun.PL_3 => "PL3"
  }

  override protected def onBackPressed()
	{
		super.onBackPressed()
	}

	lazy val entryListAdapter = new DictionaryEntryAdapter(this)
	lazy val listView = findViewById(R.id.listView).asInstanceOf[ListView]

  lazy val igPersister = new IGPersister(getApplicationContext)

	private def installEventHandlers()
	{
		val sw = findViewById(R.id.searchView).asInstanceOf[SearchView]

		sw setOnQueryTextListener TypeListener
	}

	def clear(view: View)
	{
		getApplicationContext.deleteDatabase(AndroidSDBLayer.DATABASE_NAME)
	}

	def backup(view: View)
	{
		try
		{
			val sd = "/storage/extSdCard" //Environment.getExternalStorageDirectory
			val data = Environment.getDataDirectory

			val currentDBPath = "/data/data/" + getPackageName + "/databases/" + AndroidSDBLayer.DATABASE_NAME
			val backupDBPath =  AndroidSDBLayer.DATABASE_NAME + ".db"
			val currentDB = new File(currentDBPath)
			val backupDB = new File(sd, backupDBPath)

			if (currentDB.exists())
			{
				val src = new FileInputStream(currentDB).getChannel
				val dst = new FileOutputStream(backupDB).getChannel
				dst.transferFrom(src, 0, src.size)
				src.close()
				dst.close()
			}
		}
		catch
		{
			case e: Exception => Log.e("BACKUP", "Failed to backup: " + e.getMessage)
		}
	}

	def addNewWord(view: View)
	{
		val clazz: Class[AddNewWordActivityPager] = classOf[AddNewWordActivityPager]
		val intent = new Intent(this, clazz)

		intent.putExtra(MainActivity.EXTRA_MESSAGE, "data")

		startActivity(intent)
	}

  def onClickOnAnEntry(view: View): Unit = {

    val clazz = classOf[DetailedDictionaryEntry]
    val intent = new Intent(this, clazz)

    val obj = view.getTag.asInstanceOf[Serializable]

    intent.putExtra("entry", obj)

    startActivity(intent)
  }

	override protected def onCreate(savedInstanceState: Bundle)
	{
		super.onCreate(savedInstanceState)
		setContentView(R.layout.activity_main)

		Thread setDefaultUncaughtExceptionHandler uncaughtExceptionHandler

		installEventHandlers()

		listView setAdapter entryListAdapter
	}
}
