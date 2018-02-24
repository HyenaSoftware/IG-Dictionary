package com.hyenawarrior.oldnorsedictionary

import java.io.{File, FileInputStream, FileOutputStream}

import android.content.Intent
import android.os.{Bundle, Environment}
import android.support.v7.app.AppCompatActivity
import android.util.Log
import android.view.View
import android.widget.{ListView, SearchView}
import com.hyenawarrior.OldNorseGrammar.grammar.Case.{ACCUSATIVE, DATIVE, GENITIVE, NOMINATIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{DUAL, PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.{ACTIVE, MEDIO_PASSIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{Noun, NounForm, NounType}
import com.hyenawarrior.oldnorsedictionary.model.database.IGPersister
import com.hyenawarrior.oldnorsedictionary.model.persister.database.AndroidSDBLayer
import com.hyenawarrior.oldnorsedictionary.model.{DictionaryEntry, DictionaryListItem}
import com.hyenawarrior.oldnorsedictionary.modelview.DictionaryEntryAdapter
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef
import com.hyenawarrior.oldnorsedictionary.new_word.{AddNewWordActivityPager, DetailedDictionaryEntry}

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

						// keep only those forms of the verb which are matching on the search string
            val matchingForms = sv.verbForms
              .filter {
                case (_, v) if v.strForm.startsWith(fixedStr) => true
                case _ => false
              }

            val INF_KEY = (INFINITIVE, ACTIVE, None, None)

            val priForm = sv.verbForms(INF_KEY).strForm -> abbrevationOf(INF_KEY)

            // determine which form we want to show
            val formsToShow = filterVerbForms(matchingForms) match {

              case Some((INF_KEY, f)) => Seq(priForm)
              case Some((k, v)) => Seq(priForm, v.strForm -> abbrevationOf(k))
              case None => Seq()
            }

            DictionaryListItem(formsToShow, "verb", sv, meanings)

          case DictionaryEntry(noun: Noun, meanings) =>

						val matchingForm = noun.nounForms.filter { case (_, NounForm(strRepr, _)) => strRepr startsWith fixedStr }

						val SG_NOM_KEY = (SINGULAR, NOMINATIVE)

						val priForm = noun.nounForms(SG_NOM_KEY).strRepr -> abbrevationOf(SG_NOM_KEY)

            val formsToShow = filterNounForms(matchingForm) match {

							case Some((SG_NOM_KEY, f)) => Seq(priForm)
							case Some((k, f)) => Seq(priForm, f.strRepr -> abbrevationOf(k))
						}

            DictionaryListItem(formsToShow, "noun", noun, meanings)
        }
          .toList

			entryListAdapter resetItems entries

			true
		}
	}

	//
	private def filterVerbForms(forms: Map[VerbType, StrongVerbForm]): Option[(VerbType, StrongVerbForm)] = {

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

	//
	private def filterNounForms(forms: Map[NounType, NounForm]): Option[(NounType, NounForm)] = {

		val fs = forms.groupBy { case ((number, _), _) => number }

		val singularForms = fs get SINGULAR
		val pluralForms = fs get PLURAL

		singularForms.flatMap(splitByCase) orElse pluralForms.flatMap(splitByCase)
	}

	private def splitByCase(forms: Map[NounType, NounForm]): Option[(NounType, NounForm)] = {

		val fs = forms.groupBy { case ((_, caze), _) => caze }

		val nm = fs get NOMINATIVE flatMap(m => m.headOption)
		val ac = fs get ACCUSATIVE flatMap(m => m.headOption)
		val dt = fs get DATIVE 	   flatMap(m => m.headOption)
		val gn = fs get GENITIVE   flatMap(m => m.headOption)

		nm orElse ac orElse dt orElse gn
	}

	//
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

	//
	private def abbrevationOf(declension: NounType): String = {

		val (number, caze) = declension

		abbrevationOf(number) + " " + abbrevationOf(caze)
	}

	private def abbrevationOf(number: GNumber): String = number match {

		case SINGULAR => "SG"
		case PLURAL => "PL"
	}

	private def abbrevationOf(caze: Case): String = caze match {

		case NOMINATIVE => "NOM"
		case ACCUSATIVE => "NOM"
		case DATIVE => "NOM"
		case GENITIVE => "NOM"
	}


  override protected def onBackPressed()
	{
		super.onBackPressed()
	}

	lazy val listView = findViewById(R.id.listView).asInstanceOf[ListView]
	lazy val entryListAdapter = new DictionaryEntryAdapter(this, listView)

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
