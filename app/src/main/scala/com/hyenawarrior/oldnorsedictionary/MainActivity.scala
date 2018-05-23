package com.hyenawarrior.oldnorsedictionary

import java.io.{File, FileInputStream, FileOutputStream}

import android.content.Intent
import android.os.{Bundle, Environment}
import android.support.v7.app.AppCompatActivity
import android.support.v7.widget.Toolbar
import android.util.Log
import android.view.{Menu, MenuItem, View}
import android.widget.{ListView, SearchView, Toast}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.{PoSForm, Pos}
import com.hyenawarrior.oldnorsedictionary.model.database.IGPersister
import com.hyenawarrior.oldnorsedictionary.model.persister.database.AndroidSDBLayer
import com.hyenawarrior.oldnorsedictionary.model.{DictionaryEntry, DictionaryListItem}
import com.hyenawarrior.oldnorsedictionary.modelview.DictionaryEntryAdapter
import com.hyenawarrior.oldnorsedictionary.modelview.helpers._
import com.hyenawarrior.oldnorsedictionary.new_word.{AddNewWordActivityPager, DetailedDictionaryEntry}

import scala.language.postfixOps

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
          case DictionaryEntry(verb: Verb, meanings) =>
            DictionaryListItem(getFormsToShowOf(fixedStr, verb), "verb", verb, meanings)

          case DictionaryEntry(noun: Noun, meanings) =>
            DictionaryListItem(getFormsToShowOf(fixedStr, noun), "noun", noun, meanings)
        }
          .toList

			entryListAdapter resetItems entries

			true
		}

    private def getFormsToShowOf[K, F <: PoSForm](searchString: String, obj: Pos[K, F])(implicit ordering: Ordering[K]): Seq[(String, String)] = {

      val PRI_FORM = obj.forms(obj.PRIMARY_KEY).strRepr

      // keep only those forms of the verb which are matching on the search string
      val matchingWords = obj.forms.toSeq.filter(_._2.strRepr startsWith searchString)

      val hasPrimaryForm = matchingWords.exists(_._2.strRepr == PRI_FORM)

      // return the selected forms
      if (hasPrimaryForm) {

        Seq(PRI_FORM -> "")

      } else {

        val optBestMatch = matchingWords
          .sortBy { case (vt, _) => vt }
          .headOption

        val bestMatch = optBestMatch.map { case (k, w) => w.strRepr -> s"[${abbrevationOf(k)}]" }.head

        Seq(bestMatch, s"of $PRI_FORM" -> "")
      }
    }
	}

  override protected def onBackPressed()
	{
		super.onBackPressed()
	}

	lazy val listView = findViewById[ListView](R.id.listView)
	lazy val entryListAdapter = new DictionaryEntryAdapter(this, listView)

  lazy val igPersister = new IGPersister(getApplicationContext)

	private def installEventHandlers()
	{
		val sw = findViewById[SearchView](R.id.searchView)

		sw setOnQueryTextListener TypeListener
	}

	private def clear()
	{
		getApplicationContext.deleteDatabase(AndroidSDBLayer.DATABASE_NAME)

		Toast.makeText(this, "The database has been purged.", Toast.LENGTH_SHORT)
	}

	private def backup()
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

		val myToolbar = findViewById[Toolbar](R.id.my_toolbar)
		setSupportActionBar(myToolbar)
	}

	override def onCreateOptionsMenu(menu: Menu): Boolean = {

		getMenuInflater.inflate(R.menu.main_menu_options, menu)

		super.onCreateOptionsMenu(menu)
	}

  override def onOptionsItemSelected(item: MenuItem): Boolean = {

    item.getItemId match {

      case R.id.main_menu_opt_newWord =>
        val clazz: Class[AddNewWordActivityPager] = classOf[AddNewWordActivityPager]
        val intent = new Intent(this, clazz)

        intent.putExtra(MainActivity.EXTRA_MESSAGE, "data")

        startActivity(intent)

      case R.id.main_menu_opt_clearDB => clear()
      case R.id.main_menu_opt_copyDB => backup()

      case _ => Toast
        .makeText(MainActivity.this, s"Don't know what to do with '${item.getTitle}'.", Toast.LENGTH_SHORT)
        .show()
    }

    super.onOptionsItemSelected(item)
  }
}
