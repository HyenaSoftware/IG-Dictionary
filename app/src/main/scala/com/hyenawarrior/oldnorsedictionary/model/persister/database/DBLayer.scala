package com.hyenawarrior.oldnorsedictionary.model.persister.database

import com.hyenawarrior.oldnorsedictionary.model.persister.database.DBLayer.ColumnDefinition

import scala.reflect.ClassTag

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
//
object DBLayer {

  case class Record(fields: Any*)
  case class ColumnDefinition(name: String, dataType: ClassTag[_])
}

trait DBLayer {

  def select(table: String, colDefs: Seq[ColumnDefinition], whereArgs: Array[Any] = Array(), whereClause: String = ""): Seq[List[Any]]

  def insert(tableName: String, record: Array[Any]): Unit

  def createTable(name: String, columns: Seq[ColumnDefinition])
}