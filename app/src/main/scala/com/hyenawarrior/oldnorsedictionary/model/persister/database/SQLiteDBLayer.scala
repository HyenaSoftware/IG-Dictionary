package com.hyenawarrior.oldnorsedictionary.model.persister.database

import java.lang.String.valueOf
import java.sql.{DriverManager, SQLException}

import com.hyenawarrior.oldnorsedictionary.model.persister.Serializer._
import com.hyenawarrior.oldnorsedictionary.model.persister.database.DBLayer.ColumnDefinition

import scala.collection.mutable
import scala.language.postfixOps

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
        case ColumnDefinition(c, ClassOfBlob) => s"$c BLOB"
      }
      .mkString(", ")

    val sql = s"CREATE TABLE $name ($colExprs)"

    con.createStatement().executeUpdate(sql)
  }

  override def select(table: String, colDefs: Seq[ColumnDefinition], whereArgs: Array[Any], whereClause: String, isDictinct: Boolean): Seq[List[Any]] = {

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

    val optDistinct = if(isDictinct) " DISTINCT " else ""

    val sql = s"select $optDistinct $cols from $table $whereExpr"

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

            case (ClassOfBlob, i) => result.getBytes(i+1)
          }

          results += record
        }

        results
      }

    } finally { result.close() }
  }

  override def insert(tableName: String, columns: Seq[ColumnDefinition], record: Array[Any]): Unit = {

    val vals = record.map {
      case i: Int => valueOf(i)
      case s: String => s"'$s'"
      case b: Array[Byte] => s""
    }
        .mkString(", ")

    val args = record.map(_ => "?").mkString(", ")
    val sql = s"INSERT INTO $tableName VALUES($args)"

    val ps = con prepareStatement sql

    try {
      (record zipWithIndex) foreach {
        case (n: Int, i) => ps.setInt(i + 1, n)
        case (s: String, i) => ps.setString(i + 1, s)
        case (b: Array[Byte], i) => ps.setBytes(i + 1, b)
      }

      ps.executeUpdate()

    } catch {

      case e: SQLException if ps != null => ps.close()
    }
  }
}
