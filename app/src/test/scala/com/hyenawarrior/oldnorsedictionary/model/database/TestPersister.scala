package com.hyenawarrior.oldnorsedictionary.model.database

import com.hyenawarrior.oldnorsedictionary.model.persister.database.{DBLayer, DatabasePersister, SQLiteDBLayer}
import com.hyenawarrior.oldnorsedictionary.model.persister.inmemory.InMemoryPersister
import com.hyenawarrior.oldnorsedictionary.model.persister.{Reader, Serializer}
import org.junit.Assert.assertEquals
import org.junit.Test

/**
  * Created by HyenaWarrior on 2017.11.11..
  */

class TestPersister {

  case class TestType(i: Int, s: String, l: List[String])

  implicit object TestTypeSerializer extends Serializer[TestType] {

    override val typeId: Int = 0

    override def marshall(obj: TestType): List[Any] = List(obj.i, obj.s, obj.l.size) ++ obj.l

    override def unmarshall(reader: Reader): TestType = {

      val i = reader[Int](0)
      val s = reader[String](1)
      val ls = reader[Int](2)
      val l = (3 until 3 + ls).map(reader[String]).toList

      TestType(i, s, l)
    }
  }

  @Test
  def testStore(): Unit = {

    val id = InMemoryPersister.store(TestType(0, "a", List("b", "c")))

    val obj = InMemoryPersister.load[TestType](id)

    assertEquals(0, obj.i)
    assertEquals("a", obj.s)
    assertEquals(List("b", "c"), obj.l)
  }

  @Test
  def testDatabaseStore(): Unit = {

    val dbp = DatabasePersister(SQLiteDBLayer)

    val id = dbp.store(TestType(0, "a", List("b", "c")))

    val obj = dbp.load[TestType](id)

    assertEquals(0, obj.i)
    assertEquals("a", obj.s)
    assertEquals(List("b", "c"), obj.l)
  }

}
