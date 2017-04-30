package com.example.hyenawarrior.dictionary.model.database

import android.content.{ContentValues, Context}
import android.database.Cursor
import com.example.hyenawarrior.dictionary.model.database.SQLDatabaseHelper.LANGUAGE_TABLE_NAME
import com.example.hyenawarrior.myapplication.new_word.pages.{NounData, VerbData, WordData}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum

/**
  * Created by HyenaWarrior on 2017.04.29..
  */

case class Language(langId: Int, langName: String)
case class Verb(langId: Int, verbId: Int, text: String, verbClass: VerbClassEnum)

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

  def addLanguage(name: String): Unit = {

    val cr = database.getReadableDatabase.rawQuery(s"select count(1) from $LANGUAGE_TABLE_NAME", null)
    cr.moveToNext()
    val tableSize: java.lang.Integer = cr.getInt(0)
    cr.close()

    val record = new ContentValues
    record.put("LangId", tableSize)
    record.put("LangName", name)

    database.getWritableDatabase.insert(
      LANGUAGE_TABLE_NAME,
      null,
      record)

  }

  def getLangauges: Seq[Language] = {

    val (_, mapDefs, _) = SQLDatabaseHelper.LANGUAGE_TABLE_DEF

    val columns = mapDefs.keys.toArray

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

      val id = cr.getInt(0)
      val name = cr.getString(1)

      Language(id, name)
    })

    cr.close()

    langs
  }

  def save(wordData: WordData) = wordData match
  {
    case verb: VerbData => saveVerb(verb)
    case noun: NounData => saveNoun(noun)
    case _ => ()
  }

  private def saveVerb(verb: VerbData) {

    val declToStr = verb.data
  }

  private def saveNoun(noun: NounData) {

  }
}
