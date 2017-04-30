package com.example.hyenawarrior.dictionary.model.database

import android.content.Context
import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import com.example.hyenawarrior.dictionary.model.database.SQLDatabaseHelper.{DATABASE_NAME, DATABASE_VERSION, LANGUAGE_TABLE_DEF}

/**
  * Created by HyenaWarrior on 2017.04.29..
  */
object SQLDatabaseHelper {

  val DATABASE_VERSION = 1
  val DATABASE_NAME = "ig-dictionary"

  val LANGUAGE_TABLE_NAME = "Languages"
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
}


class SQLDatabaseHelper(ctx: Context) extends SQLiteOpenHelper(ctx, DATABASE_NAME, null, DATABASE_VERSION)
{
  private def dropTable(tableDef: (String, Map[String, String], List[String])) = tableDef match {

    case (tableName, _, _) => s"DROP TABLE IF EXISTS $tableName"
  }

  private def createTable(tableDef: (String, Map[String, String], List[String])) = tableDef match
  {
    case (tableName, fieldNameToType, keys) =>

      val fields = fieldNameToType.map{case (k, v) => s"$k $v" }.mkString(",")
      val priKeys = keys.mkString(s"PRIMARY KEY (", ", ", ")")

      s"CREATE TABLE $tableName ( $fields, $priKeys )"
  }

  override def onUpgrade(sqLiteDatabase: SQLiteDatabase, i: Int, i1: Int): Unit = {

    sqLiteDatabase.execSQL(dropTable(LANGUAGE_TABLE_DEF))

    onCreate(sqLiteDatabase)
  }

  override def onCreate(sqLiteDatabase: SQLiteDatabase): Unit = {

    sqLiteDatabase.execSQL(createTable(LANGUAGE_TABLE_DEF))
  }
}
