package com.hyenawarrior.oldnorsedictionary.model.persister.database

import java.lang.String.valueOf
import java.sql.DriverManager

import com.hyenawarrior.oldnorsedictionary.model.persister.Serializer._
import com.hyenawarrior.oldnorsedictionary.model.persister.database.DBLayer.ColumnDefinition

import scala.collection.mutable

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
object SQLiteDBLayer extends DBLayer {

  Class.forName("org.sqlite.JDBC")
  private val con = DriverManager getConnection "jdbc:sqlite::memory:" /*"jdbc:sqlite:mydb.db"*/

  override def createTable(name: String, columns: Seq[ColumnDefinition]): Unit = {

    val colExprs = columns
      .map {

        case ColumnDefinition(c, ClassOfInt | ClassOfBoolean) => s"$c INTEGER"
        case ColumnDefinition(c, ClassOfString) => s"$c VARCHAR(20)"
      }
      .mkString(", ")

    val sql = s"CREATE TABLE $name ($colExprs)"

    con.createStatement().executeUpdate(sql)
  }

  override def select(table: String, colDefs: Seq[ColumnDefinition], whereArgs: Array[Any], whereClause: String): Seq[List[Any]] = {

    val cols = colDefs.map(_.name).mkString(", ")
    val whereExpr = (whereArgs, whereClause) match {

      case (Array(seq @ _*), clause) if clause.nonEmpty =>
        val args = whereArgs.foldLeft(clause) {
          case (cl, a: Int) => cl.replace("?", valueOf(a))
          case (cl, a: String) => cl.replace("?", s"'$a'")
        }
        s"where $args"

      case (Array(), "") => ""
    }

    val sql = s"select $cols from $table $whereExpr"

    val result = con.createStatement().executeQuery(sql)

    try {

      if (result.isClosed) { Seq() } else {

        val cols = colDefs.map(_.dataType).zipWithIndex.toList

        val results = mutable.ListBuffer.empty[List[Any]]

        while(result.next()) {

          val record = cols.map {

            case (ClassOfInt,     i) =>
              val v = result.getInt(i+1)
              if(result.wasNull()) None else v

            case (ClassOfBoolean, i) =>
              val v = result.getBoolean(i+1)
              if(result.wasNull()) None else v

            case (ClassOfString,  i) => result.getString(i+1)
          }

          results += record
        }

        results
      }

    } finally { result.close() }
  }

  override def insert(tableName: String, record: Array[Any]): Unit = {

    val vals = record.map {
      case i: Int => valueOf(i)
      case s: String => s"'$s'"
    }
        .mkString(", ")

    val sql = s"INSERT INTO $tableName VALUES($vals)"

    val result = con.createStatement().executeUpdate(sql)

    result

    ()
  }
}
