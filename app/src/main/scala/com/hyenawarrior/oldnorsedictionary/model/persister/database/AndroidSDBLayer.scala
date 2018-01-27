package com.hyenawarrior.oldnorsedictionary.model.persister.database

import java.lang.String.valueOf

import android.content.{ContentValues, Context}
import android.database.sqlite.{SQLiteDatabase, SQLiteOpenHelper}
import android.database.{Cursor, DatabaseUtils}
import com.hyenawarrior.oldnorsedictionary.model.database.IGPersister
import com.hyenawarrior.oldnorsedictionary.model.persister.Serializer._
import com.hyenawarrior.oldnorsedictionary.model.persister.database.AndroidSDBLayer._
import com.hyenawarrior.oldnorsedictionary.model.persister.database.DBLayer.ColumnDefinition

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
object AndroidSDBLayer {

  val DATABASE_NAME = "ig-dictionary"
  val DATABASE_VERSION = 8

}

class AndroidSDBLayer (ctx: Context) extends DBLayer {

  type jint =  java.lang.Integer

  private object SQLDatabaseHelper extends SQLiteOpenHelper(ctx, DATABASE_NAME, null, DATABASE_VERSION) {

    override def onUpgrade(sqLiteDatabase: SQLiteDatabase, i: Int, i1: Int): Unit = {

    }

    override def onCreate(sqLiteDatabase: SQLiteDatabase): Unit = {

      createTable(sqLiteDatabase, IGPersister.lookupTable)
      createTable(sqLiteDatabase, DatabasePersister.Texts)
      createTable(sqLiteDatabase, DatabasePersister.Objects)
      createTable(sqLiteDatabase, DatabasePersister.ObjTypes)
    }

    def createTable(sqLiteDatabase: SQLiteDatabase, table: Table): Unit = {

      val Table(name, columns) = table

      val colExprs = columns
        .map {

          case ColumnDefinition(c, ClassOfInt | ClassOfBoolean) => s"$c INTEGER"
          case ColumnDefinition(c, ClassOfString) => s"$c TEXT"
          case ColumnDefinition(c, ClassOfBlob) => s"$c BLOB"
        }
        .mkString(", ")

      val sql = s"CREATE TABLE $name ($colExprs)"

      sqLiteDatabase.execSQL(sql)
    }
  }

  override def select(table: String, colDefs: Seq[ColumnDefinition], whereArgs: Array[Any], whereClause: String, isDictinct: Boolean): Seq[List[Any]] = {

    val columns = colDefs.map(_.name).toArray

    val cr: Cursor = SQLDatabaseHelper.getReadableDatabase.query(
        isDictinct,
        table,
        columns, //Array("Form", "WordId"), // all columns
        whereClause,
        whereArgs.map(valueOf), // args for the arguments of the where clause
        null, // group by
        null, // having
        null, // order by
        null  // no limit
      )

    try {
      val results = Range(0, cr.getCount)
        .map(i => {

          cr.moveToNext

          val cv = new ContentValues
          // find a better way to read data by avoiding to convert everything to strings
          DatabaseUtils.cursorRowToContentValues(cr, cv)
          cv
        })
        .map(extract(_, colDefs.toList))

      results
    }
    finally { cr.close() }
  }

  private def extract(cv: ContentValues, colDefs: List[ColumnDefinition]): List[Any] = colDefs.map {

    case ColumnDefinition(name, ClassOfInt) => cv.getAsInteger(name)
    case ColumnDefinition(name, ClassOfString) => cv.getAsString(name)
    case ColumnDefinition(name, ClassOfBoolean) => cv.getAsBoolean(name)
    case ColumnDefinition(name, ClassOfBlob) => cv.getAsByteArray(name)
  }

  override def insert(tableName: String, columns: Seq[ColumnDefinition], record: Array[Any]): Unit = {

    val values = new ContentValues

    (columns zip record) foreach {

      case (ColumnDefinition(name, ClassOfInt), v: Int) => values.put(name, v: jint)
      case (ColumnDefinition(name, ClassOfBoolean), v: Boolean) => values.put(name, v)
      case (ColumnDefinition(name, ClassOfString), v: String) => values.put(name, v)
      case (ColumnDefinition(name, ClassOfBlob),   blob: Array[Byte]) => values.put(name, blob)
      case _ => throw new RuntimeException("Column type / record field type mismatch")
    }

    SQLDatabaseHelper.getWritableDatabase.insert(tableName, null, values)
  }
}
