package com.hyenawarrior.oldnorsedictionary.model.database

import android.content.Context
import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import com.hyenawarrior.oldnorsedictionary.model.database.SQLDatabaseHelper._

/**
  * Created by HyenaWarrior on 2017.04.29..
  */
object SQLDatabaseHelper {

  val DATABASE_VERSION = 6
  val DATABASE_NAME = "ig-dictionary"

  val LANGUAGE_TABLE_NAME = "Langs"
  val VERBS_TABLE_NAME = "Verbs"

  val LANGUAGE_TABLE_DEF = (LANGUAGE_TABLE_NAME,
    Map(
      "LangId" -> "INTEGER",
      "LangName" -> "TEXT"
    ), List("LangId"))

  val VERB_TABLE_DEF = (VERBS_TABLE_NAME,
    Map(
      "LangId" -> "INTEGER",
      "VerbId" -> "INTEGER"
    ),
    List("LangId", "VerbId"))

  val CREATE_EXAMPLE_TABLE	= """CREATE TABLE "Examples" ( `ExampleGroupId` INTEGER NOT NULL UNIQUE, `Example` TEXT NOT NULL )"""
	val CREATE_LANGS_TABLE		= """CREATE TABLE "Langs" ( `LangId` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE, `LangName` TEXT NOT NULL )"""
	val CREATE_MEANING_TABLE	= """CREATE TABLE "Meaning" ( `MeaningId` INTEGER NOT NULL, `LangId` INTEGER NOT NULL, `Meaning` TEXT, `Note` TEXT, `ExampleGroupId` INTEGER )"""
	val CREATE_WORD_TABLE 		= """CREATE TABLE "Word" ( `WordId` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE, `MeaningId` INTEGER NOT NULL, `PosId` INTEGER NOT NULL )"""
	val CREATE_WORD_FORMS_TABLE = """CREATE TABLE "WordForms" ( `Form` TEXT NOT NULL, `WordId` INTEGER NOT NULL, `FormId` INTEGER NOT NULL, `PosId` INTEGER NOT NULL, PRIMARY KEY(`WordId`, `FormId`) )"""

	val DROP_EXAMPLE_TABLE 		= """DROP TABLE IF EXISTS Examples"""
	val DROP_LANGS_TABLE 			= """DROP TABLE IF EXISTS Langs"""
	val DROP_MEANING_TABLE 		= """DROP TABLE IF EXISTS Meaning"""
	val DROP_WORD_TABLE 			= """DROP TABLE IF EXISTS Word"""
	val DROP_WORD_FORMS_TABLE = """DROP TABLE IF EXISTS WordForms"""
}


class SQLDatabaseHelper(ctx: Context) extends SQLiteOpenHelper(ctx, DATABASE_NAME, null, DATABASE_VERSION)
{

	override def onUpgrade(sqLiteDatabase: SQLiteDatabase, i: Int, i1: Int): Unit =
  {
    sqLiteDatabase.execSQL(DROP_EXAMPLE_TABLE)
		sqLiteDatabase.execSQL(DROP_LANGS_TABLE)
		sqLiteDatabase.execSQL(DROP_MEANING_TABLE)
		sqLiteDatabase.execSQL(DROP_WORD_FORMS_TABLE)
		sqLiteDatabase.execSQL(DROP_WORD_TABLE)

    onCreate(sqLiteDatabase)
  }

  override def onCreate(sqLiteDatabase: SQLiteDatabase): Unit =
  {
    sqLiteDatabase.execSQL(CREATE_EXAMPLE_TABLE)
		sqLiteDatabase.execSQL(CREATE_LANGS_TABLE)
		sqLiteDatabase.execSQL(CREATE_MEANING_TABLE)
		sqLiteDatabase.execSQL(CREATE_WORD_TABLE)
		sqLiteDatabase.execSQL(CREATE_WORD_FORMS_TABLE)
  }
}
