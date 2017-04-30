package com.example.hyenawarrior.myapplication

import android.content.Intent
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.view.View
import android.widget._
import com.example.hyenawarrior.dictionary.model.database.IGDatabase
import com.example.hyenawarrior.dictionary.model.{AndroidStorage, Database, DictionaryEntry}
import com.example.hyenawarrior.dictionary.modelview.DictionaryEntryAdapter
import com.example.hyenawarrior.myapplication.new_word.AddNewWordActivityPager
import com.hyenawarrior.OldNorseGrammar.grammar.{Database, Language}
import com.hyenawarrior.dictionaryLoader.Storage


object MainActivity
{
	val EXTRA_MESSAGE: String = "abc"
}

class MainActivity extends AppCompatActivity
{
	outer =>

	/*
	implicit class DictEntryStringEx(val wDef: WordDefinition) //extends AnyVal
	{
		def toMeaning = Meaning(wDef.word, s"[...]")
	}
	*/

	object TypeListener extends SearchView.OnQueryTextListener
	{
		override def onQueryTextSubmit(s: String): Boolean = true

		override def onQueryTextChange(s: String): Boolean =
		{
			val meaningsToWords = Database.database.findBy(s)

			val list: List[DictionaryEntry] = meaningsToWords.map
			{
				case(wg, matchingWords) =>
					val optPriWord = if(!matchingWords.exists(w => wg.isPrimary(w))) Some(wg.primaryWord)	else None

					val meanings = Range(0, 5).map(i => s"Meaning$i").toList //wg.meaningId   ...

					DictionaryEntry(matchingWords, optPriWord, meanings)
			}.toList

			entryListAdapter resetItems list
			listView.invalidateViews()

			true
		}
	}

	override protected def onBackPressed()
	{
		super.onBackPressed()
	}

	object Storage extends Storage with AndroidStorage


	lazy val entryListAdapter = new DictionaryEntryAdapter(this) //new EntryListAdapter(this)
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

		val languages = igDatabase.getLangauges

		listView setAdapter entryListAdapter

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
