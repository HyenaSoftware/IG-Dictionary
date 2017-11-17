package com.hyenawarrior.oldnorsedictionary.model.persister.database

import com.hyenawarrior.oldnorsedictionary.model.persister.database.DBLayer.ColumnDefinition
import com.hyenawarrior.oldnorsedictionary.model.persister.{Persister, SerData, StringInterner}
import com.hyenawarrior.oldnorsedictionary.model.persister.Serializer._

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
case class DatabasePersister(dBLayer: DBLayer) extends Persister {

  private implicit val dbLayer = dBLayer

  private object Texts extends Table("Texts", IndexedSeq(
    ColumnDefinition("Text", ClassOfString),
    ColumnDefinition("Id", ClassOfInt)))

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

  private object ObjFields extends Table("ObjFields", IndexedSeq(
    ColumnDefinition("ObjId", ClassOfInt),
    ColumnDefinition("FieldIndex", ClassOfInt),
    ColumnDefinition("Rsrc", ClassOfInt)))

  private object ObjTypes extends Table("ObjType", IndexedSeq(
    ColumnDefinition("ObjId", ClassOfInt),
    ColumnDefinition("ObjType", ClassOfInt)))

  private object ObjStorage extends SerData {

    override def typeOf(objId: Int): Option[Int] = ObjTypes.select(List("ObjType"), Array(objId), s"ObjId = ?")
      .map { case List(tId: Int) => tId }
      .headOption

    override def store(typeId: Int, data: Seq[Int]): Int = {

      // BlobId, BlobType, FieldIndex, Rsrc
      // new      given    1..         given

      val blobId = ObjTypes.max[Int]("ObjId").getOrElse(-1) + 1
      val records = data.zipWithIndex.map{ case (d, i) => Array[Any](blobId, i, d)}

      ObjTypes.insert(Array(blobId, typeId))

      for(r <- records) {

        ObjFields.insert(r)
      }

      blobId
    }

    override def load(objId: Int, typeId: Int): Seq[Int] = {

      val res = ObjTypes.select(List("ObjId"), Array(objId, typeId), s"ObjId = ? and ObjType = ?").toList

      res match {

        case List(id: Int) :: Nil =>
          val records = ObjFields.select(List("FieldIndex", "Rsrc"), Array(objId), s"ObjId = ?")
          val fields = records
            .map { case (idx: Int) :: (rsrc: Int) :: Nil => idx -> rsrc }
            .sortBy(_._1)
            .map(_._2)

          fields

        case _ => Seq()
      }
    }
  }

  override def stringInterner: StringInterner = TextInterner
  override def serData: SerData = ObjStorage
}
