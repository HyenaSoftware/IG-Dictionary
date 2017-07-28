package com.example.hyenawarrior.dictionary.model.database

import android.content.Context
import android.database.Cursor
import com.example.hyenawarrior.dictionary.model.database.SQLDatabaseHelper.LANGUAGE_TABLE_NAME
import com.example.hyenawarrior.dictionary.model.database.marshallers._
import com.example.hyenawarrior.dictionary.model.database.marshallers.marshallers._
import com.example.hyenawarrior.myapplication.new_word.pages.{MeaningDef, WordData}

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
		database.getWritableDatabase.delete("Word", null, null)
		database.getWritableDatabase.delete("Meaning", null, null)
		database.getWritableDatabase.delete("Examples", null, null)
		database.getWritableDatabase.delete("Langs", null, null)
	}

	private def maxIdFor(column: String, table: String): Int =
	{
		val cr = database.getReadableDatabase.rawQuery(s"select max($column) from $table", null)

		cr.moveToNext()

		val maxId = cr.getInt(0)

		cr.close()

		maxId
	}

	def addLanguage(name: String): Unit =
	{

		val cr = database.getReadableDatabase.rawQuery(s"select count(1) from $LANGUAGE_TABLE_NAME", null)
		cr.moveToNext()
		val tableSize: java.lang.Integer = cr.getInt(0)
		cr.close()

		val newLang = Language(tableSize, name)
		execInsert(newLang)
	}

	def getLangauges: Seq[Language] =
	{

		val (_, columnsToType, _) = SQLDatabaseHelper.LANGUAGE_TABLE_DEF

		val columns = columnsToType.keys.toArray

		val cr: Cursor = database.getReadableDatabase.query(
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

	def save(wordData: WordData, meanings: List[MeaningDef]) =
	{
		val nextWordId = maxIdFor("WordId", "WordForms") + 1

		for ((verbForm, str) <- wordData.forms)
		{
			execInsert(WordForm(str, nextWordId, verbForm, wordData.posType))
		}

		//
		val langId = 1

		for(mDef <- meanings)
		{
			val nextMeaningId = maxIdFor("MeaningId", "Meaning") + 1

			val exGroupId = if(mDef.examples.nonEmpty) Some(maxIdFor("ExampleGroupId", "Examples") + 1) else None

			for(exStr <- mDef.examples)
			{
				execInsert(Example(exGroupId.get, exStr))
			}

			execInsert(Meaning(nextMeaningId, langId, mDef.meaning, mDef.note, exGroupId))
			execInsert(Word(nextWordId, nextMeaningId, wordData.posType))
		}
	}

	private def execInsert[T <: dbrecord](data: T)(implicit marshaller: TMarshaller[T]): Unit =
	{
		val record = marshaller(data)

		database.getWritableDatabase.insert(marshaller.TABLE_NAME, null, record)
	}

	private def loadWordToMeaningMapping(word: Word): IndexedSeq[MeaningDef] =
	{
		//val wordId = word.wordId
		//val posId = word.posType.id

		//val words = findWordBy(wordId, posId)

		val meanings =
		{
			val cr: Cursor = database.getReadableDatabase.query(
				MeaningMarshaller.TABLE_NAME,
				null, //Array("Form", "WordId"), // all columns
				s"MeaningId in (?)", // where caluse
				Array(word.meaningId.toString), // args for the arguments of the where clause
				null, // group by
				null, // having
				null // order by
			)

			val meanings = unmarshall[Meaning](cr)

			cr.close()

			meanings
		}

		val examples =
		{
			val exGIds = meanings.map(m => m.exampleId).collect{ case Some(x) => x.toString }.toArray
			val args = exGIds.map(_ => '?').mkString("(", ", ", ")")

			val cr: Cursor = database.getReadableDatabase.query(
				ExampleMarshaller.TABLE_NAME,
				null, //Array("Form", "WordId"), // all columns
				s"ExampleGroupId in $args", // where caluse
				exGIds, // args for the arguments of the where clause
				null, // group by
				null, // having
				null // order by
			)

			val exs = unmarshall[Example](cr)

			cr.close()

			exs.map(e => e.groupId -> e).toMap
		}

		def getExs(i: Int) = examples.get(i).map(e => e.text).toSeq

		meanings.map(m => MeaningDef(m.meaning, m.note, m.exampleId.map(getExs).getOrElse(Seq())))
	}

	private def findWordBy(wordId: Int, posId: Int): Option[Word] =
	{
		val cr: Cursor = database.getReadableDatabase.query(
			WordMarshaller.TABLE_NAME,
			null, //Array("Form", "WordId"), // all columns
			"WordId = ? AND PosId = ?", // where caluse
			Array(wordId.toString, posId.toString), // args for the arguments of the where clause
			null, // group by
			null, // having
			null // order by
		)

		val words = unmarshall[Word](cr)

		cr.close()

		words.headOption
	}

	// (Map[WordForm, Meaning])
	// * exemplar form
	def findByStr(str: String): Map[Word, (Seq[WordForm], Seq[MeaningDef])] = if(str.isEmpty) Map() else
	{
		val sqlStr = str.map
		{
			case '*' => '%'
			case c => c
		} + '%'

		val cr: Cursor  = database.getReadableDatabase.query(
			WordFormMarshaller.TABLE_NAME,
			null, //Array("Form", "WordId"), // all columns
			"Form like ?",	// where caluse
			Array(sqlStr),			// args for the arguments of the where clause
			null, // group by
			null,	// having
			null	// order by
		)

		val words = unmarshall[WordForm](cr)

		cr.close()

		val widPidS = words.groupBy(w => w.wordId -> w.posType)

		val data = widPidS.map
		{
			case (k, v) =>
				val w = findWordBy(k._1, k._2.id).get
				w -> (v, loadWordToMeaningMapping(w))
		}

		data
	}

	private def unmarshall[T <: dbrecord](cr: Cursor)(implicit marshaller: TMarshaller[T]): IndexedSeq[T] =
	{
		Range(0, cr.getCount).flatMap(_ =>
		{
			cr.moveToNext
			marshaller.unapply(cr)
		})
	}
}
