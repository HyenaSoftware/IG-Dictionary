package com.example.hyenawarrior.dictionary.model.database

import android.content.Context
import android.database.Cursor
import com.example.hyenawarrior.dictionary.model.database.SQLDatabaseHelper.LANGUAGE_TABLE_NAME
import com.example.hyenawarrior.dictionary.model.database.marshallers.marshallers._
import com.example.hyenawarrior.dictionary.model.database.marshallers.{PosType, TMarshaller, VerbType}
import com.example.hyenawarrior.myapplication.new_word.pages.{NounData, VerbData, WordData}

/**
  * Created by HyenaWarrior on 2017.04.29..
  */

object IGDatabase
{
  private var database: IGDatabase = null

  def apply(ctx: Context): IGDatabase =
  {
    if(database == null)
    {
      database = new IGDatabase(ctx)
    }

    database
  }
}

class IGDatabase(ctx: Context)
{
  val database = new SQLDatabaseHelper(ctx)

	def clear
	{
		database.getWritableDatabase.delete("WordForms", null, null)
	}

	private def maxIdFor(column: String, table: String): Int =
	{
		val cr = database.getReadableDatabase.rawQuery(s"select max($column) from $table", null)

		cr.moveToNext()

		val maxId = cr.getInt(0)

		cr.close()

		maxId
	}

  def addLanguage(name: String): Unit = {

    val cr = database.getReadableDatabase.rawQuery(s"select count(1) from $LANGUAGE_TABLE_NAME", null)
    cr.moveToNext()
    val tableSize: java.lang.Integer = cr.getInt(0)
    cr.close()

		val newLang = Language(tableSize, name)
		execInsert(newLang)
  }

  def getLangauges: Seq[Language] = {

    val (_, columnsToType, _) = SQLDatabaseHelper.LANGUAGE_TABLE_DEF

    val columns = columnsToType.keys.toArray

    val cr: Cursor  = database.getReadableDatabase.query(
      LANGUAGE_TABLE_NAME,
      columns,
      null,
      null,
      null,
      null,
      null
      )

    val langs = Range(0, cr.getCount).map(_ =>
    {
      cr.moveToNext

			LanguageMarshaller.unapply(cr)
    })

    cr.close()

    langs.flatten
  }

	private def wordDataToWords(wordData: WordData): Word = wordData match
	{
		case _: VerbData => Word(-1, -1, PosType.VERB_STRONG_1ST)
		case _: NounData => Word(-1, -1, PosType.NOUN_STRONG_FEM_A)
	}

	def save(wordData: WordData, meanings: List[Meaning]) =
  {
		val wordForms = wordDataToWordForms(wordData)

		for(wordForm <- wordForms)
		{
			execInsert(wordForm)
		}
	}

  private def wordDataToWordForms(wordData: WordData): List[WordForm] = wordData match
  {
		case verb: VerbData =>
			val nextWordId = maxIdFor("WordId", "WordForms") + 1
			val verbType = VerbType.findByVerbClass(verb.verbClass)

			verb.forms
				.map
				{
					case (verbForm, str) => WordForm(str, nextWordId, verbForm, verbType)
				}
				.toList

		case noun: NounData => List()
  }

	private def execInsert[T <: dbrecord](data: T)(implicit marshaller: TMarshaller[T]): Unit =
	{
		val record = marshaller(data)

		database.getWritableDatabase.insert(marshaller.TABLE_NAME, null, record)
	}

	def findByStr(str: String): Seq[WordForm] =
	{
		val cr: Cursor  = database.getReadableDatabase.query(
			WordFormMarshaller.TABLE_NAME,
			null, //Array("Form", "WordId"), // all columns
			"Form like ?",	// where caluse
			Array(str),			// args for the arguments of the where clause
			null, // group by
			null,	// having
			null	// order by
		)

		val words: Seq[WordForm] = Range(0, cr.getCount).flatMap(_ =>
		{
			cr.moveToNext
			WordFormMarshaller.unapply(cr)
		})

		words
	}
}
