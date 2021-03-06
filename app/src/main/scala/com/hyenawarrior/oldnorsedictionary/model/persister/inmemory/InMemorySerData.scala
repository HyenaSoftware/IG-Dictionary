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

  override def store(blobId: Int, typeId: Int, byteArray: Array[Byte]): Int = {

    store(typeId, byteArray)

    blobId
  }

  override def load(objId: Int, typeId: Int): DataInputStream = {

    val ba = fields(objId -> typeId)

    new DataInputStream(new ByteArrayInputStream(ba))
  }

  override def typeOf(objId: Int): Option[Int] = fields.keys.find { case (o, t) => o == objId }.map { case (_, t) => t }

  override def deleteAll(): Unit = ()

  override def delete(objId: Int): Boolean = {

    val optE = fields.find(e => e._1._1 == objId)

    for((e, _) <- optE) {

      fields.remove(e)
    }

    optE.nonEmpty
  }
}
