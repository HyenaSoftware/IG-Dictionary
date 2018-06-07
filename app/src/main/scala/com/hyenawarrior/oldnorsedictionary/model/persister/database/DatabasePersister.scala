package com.hyenawarrior.oldnorsedictionary.model.persister.database

import java.io.{ByteArrayInputStream, DataInputStream}

import com.hyenawarrior.oldnorsedictionary.model.persister.Serializer._
import com.hyenawarrior.oldnorsedictionary.model.persister.database.DBLayer.ColumnDefinition
import com.hyenawarrior.oldnorsedictionary.model.persister.database.DatabasePersister.{ObjTypes, Objects, Texts}
import com.hyenawarrior.oldnorsedictionary.model.persister.{Persister, SerData, StringInterner}

import scala.language.postfixOps

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
object DatabasePersister {

  object Texts extends Table("Texts", IndexedSeq(
    ColumnDefinition("Text", ClassOfString),
    ColumnDefinition("Id", ClassOfInt)))

  object Objects extends Table("Objects", IndexedSeq(
    ColumnDefinition("ObjId", ClassOfInt),
    ColumnDefinition("RawData", ClassOfBlob)))

  object ObjTypes extends Table("ObjType", IndexedSeq(
    ColumnDefinition("ObjId", ClassOfInt),
    ColumnDefinition("ObjType", ClassOfInt)))
}

case class DatabasePersister(dBLayer: DBLayer) extends Persister {

  private implicit val dbLayer = dBLayer

  private object TextInterner extends StringInterner {

    override def apply(id: Int): String = Texts
      .select(List("Text"), Array(id), s"Id = ?")
      .map { case List(str: String) => str }
      .head

    override def indexOf(str: String): Option[Int] = Texts
      .select(List("Id"), Array(str), s"Text = ?")
      .map { case List(id: Int) => id }
      .headOption

    override def store(str: String): Int = {

      val optId = indexOf(str)

      optId match {

        case Some(id) => id

        case None =>
          val id = Texts.max[Int]("Id").getOrElse(-1) + 1
          Texts.insert(Array(str, id))
          id
      }
    }
  }

  private object ObjStorage extends SerData {

    override def typeOf(objId: Int): Option[Int] = ObjTypes.select(List("ObjType"), Array(objId), s"ObjId = ?")
      .map { case List(tId: Int) => tId }
      .headOption

    override def store(typeId: Int, byteArray: Array[Byte]): Int = {

      // BlobId, BlobType, FieldIndex, Rsrc
      // new      given    1..         given

      val blobId = ObjTypes.max[Int]("ObjId").getOrElse(-1) + 1

      store(blobId, typeId, byteArray)
    }

    override def store(blobId: Int, typeId: Int, byteArray: Array[Byte]): Int = {

      ObjTypes.insert(Array(blobId, typeId))

      Objects.insert(Array[Any](blobId, byteArray))

      blobId
    }

    def delete(objId: Int): Boolean = {

      val d1 = Objects.delete(Array(objId), "ObjId = ?")
      val d2 = ObjTypes.delete(Array(objId), "ObjId = ?")

      (d1 > 0) && (d2 > 0)
    }

    def deleteAll(): Unit = {

      Texts.delete(Array(), null)
      Objects.delete(Array(), null)
      ObjTypes.delete(Array(), null)
    }

    override def load(objId: Int, typeId: Int): DataInputStream = {

      val res = ObjTypes.select(List("ObjId"), Array(objId, typeId), s"ObjId = ? and ObjType = ?").toList

      res match {

        case ((id: Int) :: Nil) :: Nil if id == objId => load(objId)
        case ((id: Int) :: Nil) :: Nil => throw new RuntimeException("Type mismatch")
        case _ => throw new RuntimeException(s"Unknown object id $objId")
      }
    }

    private def load(objId: Int): DataInputStream = {

      val records = Objects.select(List("RawData"), Array(objId), "ObjId = ?")

      records match {

        case Seq(List(a: Array[Byte])) =>
          new DataInputStream(new ByteArrayInputStream(a))

        case _ => throw new RuntimeException("Unexpected result from the database")
      }
    }
  }

  override def stringInterner: StringInterner = TextInterner
  override def serData: SerData = ObjStorage
}
