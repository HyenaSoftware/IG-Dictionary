package com.hyenawarrior.oldnorsedictionary.model.persister.database

import com.hyenawarrior.oldnorsedictionary.model.persister.database.DBLayer.ColumnDefinition

import scala.reflect.ClassTag

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
case class Table(tableName: String, columns: IndexedSeq[ColumnDefinition]) {

  def insert(record: Array[Any])(implicit dbLayer: DBLayer): Unit = dbLayer.insert(tableName, columns, record)

  def max[T](column: String)(implicit clazz: ClassTag[T], dbLayer: DBLayer): Option[T] = {

    val colDefs = Seq(ColumnDefinition(s"max($column)", clazz))
    val results = dbLayer.select(tableName, colDefs)

    val res = results.headOption.map{

      case List(i: T) => Some(i)
      case _ => None
    }

    res.flatten
  }

  def select(columnsToQuery: List[String], whereArgs: Array[Any], whereClause: String, isDictinct: Boolean = false)(implicit dbLayer: DBLayer): Seq[List[Any]] = {

    val columnsByName = columns.map(c => c.name -> c).toMap
    val colDefs = columnsToQuery.map(c => columnsByName(c))

    dbLayer.select(tableName, colDefs, whereArgs, whereClause)
  }
}
