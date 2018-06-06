package com.hyenawarrior.oldnorsedictionary.model.database

import com.hyenawarrior.oldnorsedictionary.model.database.serializers.HashCode
import com.hyenawarrior.oldnorsedictionary.model.persister.database.DatabasePersister.{ObjTypes, Objects, Texts}
import com.hyenawarrior.oldnorsedictionary.model.persister.database.{DatabasePersister, SQLiteDBLayer}
import com.hyenawarrior.oldnorsedictionary.model.persister.inmemory.InMemoryPersister
import com.hyenawarrior.oldnorsedictionary.model.persister.{Reader, Serializer}
import org.junit.Assert.assertEquals
import org.junit.Test

/**
  * Created by HyenaWarrior on 2017.11.11..
  */

class TestPersister {

  case class TestType(i: Int, s: String, j: Byte, l: List[String], b: Boolean)

  implicit object TestTypeSerializer extends Serializer[TestType] {

    override val typeId: Int = 0

    override def marshall(obj: TestType): List[Any] = List(obj.i, obj.s, obj.j, obj.l.size.toByte) ++ obj.l :+ obj.b

    override def unmarshall(reader: Reader): TestType = {

      val i = reader[Int]()
      val s = reader[String]()
      val j = reader[Byte]()
      val ls = reader[Byte]()
      val l = (0 until ls).map(i => reader[String]()).toList
      val b = reader[Boolean]()

      TestType(i, s, j, l, b)
    }
  }

  implicit object TestTypeHashCode extends HashCode[TestType] {

    override def apply(obj: TestType): Int = 1
  }

  @Test
  def testStore(): Unit = {

    implicit val map: Map[Class[_], (Serializer[Any], HashCode[Any])] = Map(classOf[TestType] -> (
      TestTypeSerializer.asInstanceOf[Serializer[Any]], TestTypeHashCode.asInstanceOf[HashCode[Any]]))

    val persister = new InMemoryPersister()

    val id = persister.store(TestType(8818, "a", 23, List("b", "c"), true))

    val obj = persister.load[TestType](id)

    assertEquals(8818, obj.i)
    assertEquals("a", obj.s)
    assertEquals(23, obj.j)
    assertEquals(List("b", "c"), obj.l)
    assertEquals(true, obj.b)
  }

  @Test
  def testDatabaseStore(): Unit = {

    implicit val map: Map[Class[_], Serializer[Any]] = Map(classOf[TestType] -> TestTypeSerializer.asInstanceOf[Serializer[Any]])

    SQLiteDBLayer.createTable(Texts.tableName, Texts.columns)
    SQLiteDBLayer.createTable(Objects.tableName, Objects.columns)
    SQLiteDBLayer.createTable(ObjTypes.tableName, ObjTypes.columns)

    val dbp = DatabasePersister(SQLiteDBLayer)

    val id = dbp.store(TestType(654, "a", 12, List("b", "c"), false))

    val obj = dbp.load[TestType](id)

    assertEquals(654, obj.i)
    assertEquals("a", obj.s)
    assertEquals(12, obj.j)
    assertEquals(List("b", "c"), obj.l)
    assertEquals(false, obj.b)
  }

}
