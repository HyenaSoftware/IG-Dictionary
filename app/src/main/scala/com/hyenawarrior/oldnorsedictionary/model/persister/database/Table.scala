package com.hyenawarrior.oldnorsedictionary.model.persister.database

import com.hyenawarrior.oldnorsedictionary.model.persister.database.DBLayer.ColumnDefinition

import scala.reflect.ClassTag

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
case class Table(tableName: String, columns: IndexedSeq[ColumnDefinition])(implicit dbLayer: DBLayer) {

  dbLayer.createTable(tableName, columns)

  def insert(record: Array[Any]): Unit = dbLayer.insert(tableName, record)
  def max[T](column: String)(implicit clazz: ClassTag[T]): Option[T] = {

    val colDefs = Seq(ColumnDefinition(s"max($column)", clazz))
    val results = dbLayer.select(tableName, colDefs)

    val res = results.headOption.map{

      case List(i: T) => Some(i)
      case _ => None
    }

    res.flatten
  }

  def select(columnsToQuery: List[String], whereArgs: Array[String], whereClause: String): Seq[List[Any]] = {

    val columnsByName = columns.map(c => c.name -> c).toMap
    val colDefs = columnsToQuery.map(c => columnsByName(c))

    dbLayer.select(tableName, colDefs, whereArgs, whereClause)
  }
}
