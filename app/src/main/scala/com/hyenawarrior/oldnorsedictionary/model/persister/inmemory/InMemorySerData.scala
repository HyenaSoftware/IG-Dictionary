package com.hyenawarrior.oldnorsedictionary.model.persister.inmemory

import java.io.{ByteArrayInputStream, DataInputStream}

import com.hyenawarrior.oldnorsedictionary.model.persister.SerData

import scala.collection.mutable

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
// InMemory
class InMemorySerData extends SerData {

  type ObjIdTypeId = (Int, Int)

  val fields: mutable.Map[ObjIdTypeId, Array[Byte]] = mutable.Map()

  override def store(typeId: Int, byteArray: Array[Byte]): Int = {

    val nId = fields.size + 1

    fields += (nId, typeId) -> byteArray

    nId
  }

  override def load(objId: Int, typeId: Int): DataInputStream = {

    val ba = fields(objId -> typeId)

    new DataInputStream(new ByteArrayInputStream(ba))
  }

  override def typeOf(objId: Int): Option[Int] = fields.keys.find { case (o, t) => o == objId }.map { case (_, t) => t }
}
