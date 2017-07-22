package com.example.hyenawarrior.myapplication

import java.io.{File, FileInputStream, FileOutputStream}

import android.content.Intent
import android.os.{Bundle, Environment}
import android.support.v7.app.AppCompatActivity
import android.util.Log
import android.view.View
import android.widget._
import com.example.hyenawarrior.dictionary.model.DictionaryEntry
import com.example.hyenawarrior.dictionary.model.database.marshallers.{NounForm, NounType, VerbForm, VerbType}
import com.example.hyenawarrior.dictionary.model.database.{IGDatabase, SQLDatabaseHelper, WordForm, Word}
import com.example.hyenawarrior.dictionary.modelview.DictionaryEntryAdapter
import com.example.hyenawarrior.myapplication.new_word.AddNewWordActivityPager
import com.example.hyenawarrior.myapplication.new_word.pages.MeaningDef
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{DUAL, PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.{Word => GWord}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun

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
			case (GWord(Noun(_, _, (n1, c1), _, _)), GWord(Noun(_, _, (n2, c2), _, _))) if n1 != n2 => sgnDiff(n1, n2)
			case (GWord(Noun(_, _, (n1, c1), _, _)), GWord(Noun(_, _, (n2, c2), _, _))) => sgnDiff(c1, c2)
			//case (Word(VerbForm(_, )), Word())
			case _ => 0
		}
	}

	implicit object MeaningDefOrdering extends Ordering[MeaningDef]
	{
		override def compare(x: MeaningDef, y: MeaningDef): Int = x.desc.compareTo(y.desc)
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

	private def toDictionaryEntry(wordForms: (Word, (Seq[WordForm], Seq[MeaningDef]))): DictionaryEntry =
	{
		val words = wordForms._2._1.map
		{
			case WordForm(str, wordId, vf: VerbForm, VerbType(_, verbClass)) =>
				val VerbForm(_, mode, optTense, optPronoun) = vf
				val verb = verbs.verbFrom(str, mode, verbClass, optTense, optPronoun)
				val isPrimary = vf == VerbForm.VERB_INFINITIVE
				GWord(verb) -> isPrimary

			case WordForm(str, wordId, nf: NounForm, NounType(_, nounClass)) =>
				val NounForm(_, num, caze) = nf
				val isPrimary = nf == NounForm.NOUN_NOM_SG
				val noun = Noun(str, -1, (num, caze), Root("???"), nounClass.nounStemClass)
				GWord(noun) -> isPrimary
		}

		val dictForm = words.find(_._2).map(_._1)

		import Orderings._

		val meanings = wordForms._2._2

		DictionaryEntry(words.map(_._1).toList.sorted, dictForm, meanings.toList.sorted)
	}

	override protected def onBackPressed()
	{
		super.onBackPressed()
	}

	lazy val entryListAdapter = new DictionaryEntryAdapter(this)
	lazy val listView = findViewById(R.id.listView).asInstanceOf[ListView]

	lazy val igDatabase = IGDatabase(getApplicationContext)

	private def installEventHandlers
	{
		val sw = findViewById(R.id.searchView).asInstanceOf[SearchView]

		sw setOnQueryTextListener TypeListener
	}

	private def initDatabase: Unit = {

		igDatabase.addLanguage("Old Norse")
		igDatabase.addLanguage("English")
	}

	def clear(view: View)
	{
		igDatabase.clear
		initDatabase
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

		installEventHandlers
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
