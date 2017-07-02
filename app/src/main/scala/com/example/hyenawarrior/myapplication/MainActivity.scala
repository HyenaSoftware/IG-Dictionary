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
import com.example.hyenawarrior.dictionary.model.database.{IGDatabase, SQLDatabaseHelper, WordForm}
import com.example.hyenawarrior.dictionary.modelview.DictionaryEntryAdapter
import com.example.hyenawarrior.myapplication.new_word.AddNewWordActivityPager
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.{Root, Word, verbs}


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
			val words = igDatabase.findByStr(s).groupBy(_.wordId).values

			val list: List[DictionaryEntry] = words.map(toDictionaryEntry).toList

			entryListAdapter resetItems list
			listView.invalidateViews()
			true
		}
	}

	private def toDictionaryEntry(wordForms: Seq[WordForm]): DictionaryEntry =
	{
		val words = wordForms.map
		{
			case WordForm(str, wordId, vf: VerbForm, VerbType(_, verbClass)) =>
				val VerbForm(_, mode, optTense, optPronoun) = vf
				Word(verbs.verbFrom(str, mode, verbClass, optTense, optPronoun))

			case WordForm(str, wordId, nf: NounForm, NounType(_, nounClass)) =>
				val NounForm(_, num, caze) = nf
				Word(new Noun(str, -1, (num, caze), Root("???"), nounClass.nounStemClass))
		}

		DictionaryEntry(words.toList, None, List())
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

	def clear(view: View) = igDatabase.clear

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
