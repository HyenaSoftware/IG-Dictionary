package com.hyenawarrior.oldnorsedictionary.model.persister.inmemory

import com.hyenawarrior.oldnorsedictionary.model.persister.SerData

import scala.collection.mutable

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
// InMemory
object InMemorySerData extends SerData {

  type ObjIdTypeId = (Int, Int)

  val fields: mutable.Map[ObjIdTypeId, Seq[Int]] = mutable.Map()

  override def store(typeId: Int, data: Seq[Int]): Int = {

    val nId = fields.size + 1

    fields += (nId, typeId) -> data

    nId
  }

  override def load(objId: Int, typeId: Int): Seq[Int] = fields(objId -> typeId)

  override def typeOf(objId: Int): Option[Int] = fields.keys.find { case (o, t) => o == objId }.map { case (_, t) => t }
}
