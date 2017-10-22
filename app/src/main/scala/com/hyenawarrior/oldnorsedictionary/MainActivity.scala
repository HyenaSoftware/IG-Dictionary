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
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, StrongVerbClassEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.{Word => GWord, _}
import com.hyenawarrior.oldnorsedictionary.model.DictionaryEntry
import com.hyenawarrior.oldnorsedictionary.model.database.marshallers._
import com.hyenawarrior.oldnorsedictionary.model.database.{IGDatabase, SQLDatabaseHelper}
import com.hyenawarrior.oldnorsedictionary.modelview.DictionaryEntryAdapter
import com.hyenawarrior.oldnorsedictionary.new_word.AddNewWordActivityPager
import com.hyenawarrior.oldnorsedictionary.new_word.pages.{MeaningDef, WordData}

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

		override def onQueryTextChange(s: String): Boolean =
		{
			val words = igDatabase.findByStr(s)

			val list: List[DictionaryEntry] = words.map(toDictionaryEntry).toList

			val orderedList = list.sortWith
			{
				case (a, b) => a.dictWord.map(_.strForm()).getOrElse("") > b.dictWord.map(_.strForm()).getOrElse("")
			}

			entryListAdapter resetItems orderedList
			listView.invalidateViews()
			true
		}
	}

	private def toDictionaryEntry(wordForms: (WordData, Seq[MeaningDef])): DictionaryEntry =
	{
		/*
			ablauts of the 7th strong verb class are not stored we have to extract them
		 */

		val posType = wordForms._1.posType

		val words: Map[GWord, Boolean] = //null
			Some(posType)
			.collect { case vt: VerbType => vt.verbClass }
			.collect { case svc: StrongVerbClassEnum => svc }
			.map{ svc =>

				val verbForms = wordForms._1.forms.collect { case (vf: VerbForm, str) => vf -> str }

				val forms = verbForms.map {

					case (VerbForm(_, (md, oT, oP)), str) =>	verbs.stemFrom(oT, oP.map(_.number), md) -> str
				}
					.groupBy(_._1)
					.map { case(k, v) => k -> v.values.toSeq }

				val optSvd = verbs.getDescOfStrongVerbClassFor(svc, forms)

				val word = optSvd.map(svd => verbForms.map {

					case (VerbForm(_, vt @ (md, oT, oP)), rawStr) =>
					  val verb = StrongVerb.fromStringRepr(rawStr, svd.vClass, (md, oT, oP))
						val isPrimary = vt == VerbForm.VERB_INFINITIVE.vtype
						GWord(verb) -> isPrimary
				})

				word.getOrElse(Map())

			}.getOrElse(Map())

		val dictForm = words.find(_._2).map(_._1)

		import Orderings._

		val meanings = wordForms._2

		DictionaryEntry(words.keys.toList.sorted, dictForm, meanings.toList.sorted)
	}

	override protected def onBackPressed()
	{
		super.onBackPressed()
	}

	lazy val entryListAdapter = new DictionaryEntryAdapter(this)
	lazy val listView = findViewById(R.id.listView).asInstanceOf[ListView]

	lazy val igDatabase = IGDatabase(getApplicationContext)

	private def installEventHandlers()
	{
		val sw = findViewById(R.id.searchView).asInstanceOf[SearchView]

		sw setOnQueryTextListener TypeListener
	}

	private def initDatabase(): Unit = {

		igDatabase.addLanguage("Old Norse")
		igDatabase.addLanguage("English")
	}

	def clear(view: View)
	{
		igDatabase.clear
		initDatabase()
	}

	def backup(view: View)
	{
		try
		{
			val sd = "/storage/extSdCard" //Environment.getExternalStorageDirectory
			val data = Environment.getDataDirectory

			val currentDBPath = "/data/data/" + getPackageName + "/databases/" + SQLDatabaseHelper.DATABASE_NAME
			val backupDBPath =  SQLDatabaseHelper.DATABASE_NAME + ".db"
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

	override protected def onCreate(savedInstanceState: Bundle)
	{
		super.onCreate(savedInstanceState)
		setContentView(R.layout.activity_main)

		installEventHandlers()
		//initDatabase

		//igDatabase.clear
		//val languages = igDatabase.getLangauges

		listView setAdapter entryListAdapter

		//backup

		//val isReadable = Storage.isExternalStorageReadable
		//val isWriteable = Storage.isExternalStorageWritable

		/*
		val oldNorseWords = meanings("Old Norse")
		val englishWords = meanings("English")

		val ws1 = oldNorseWords.map(iw => iw.meaningId -> Meaning(iw._2.head))
		val ws2 = englishWords.map(id => id.meaningId -> id._2.map(Meaning(_)))

		val list2 = ws1
			.map(im => DictionaryRow(im._2, ws2.getOrElse(im._1, List.empty)))
		  .toList
			.sortBy(_.wordFrom.word)
		*/
/*
		val listView = findViewById(R.id.listView).asInstanceOf[ListView]
		listView setAdapter new EntryListAdapter(this, list2)
*/
	/*List(
			R.id.row_word -> "ROW_WORD",
			R.id.row_word_defs -> Range(0, rowView.getCount).map(rowView.getView(_, null, null)).toList //mobileArray // List[View]
		)*/

	}
}
