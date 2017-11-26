package com.hyenawarrior.oldnorsedictionary.model.persister

import scala.reflect.ClassTag
import com.hyenawarrior.oldnorsedictionary.model.persister.Serializer._

import scala.language.postfixOps


/**
  * Created by HyenaWarrior on 2017.11.16..
  */
abstract class Persister(serializers: Map[Class[_], Serializer[_]]) {

  outer =>

  def stringInterner: StringInterner

  protected def serData: SerData

  private case class MyReader(data: IndexedSeq[Int]) extends Reader {

    override def apply[T](i: Int)(implicit clazz: ClassTag[T]): T = clazz match {

      case ClassOfString => stringInterner(data(i)).asInstanceOf[T]
      case ClassOfInt => data(i).asInstanceOf[T]
      case ClassOfBoolean => (data(i) != 0).asInstanceOf[T]
      case _ =>
        val objId = data(i)
        val typeId = typeOf(objId) get

        // find the approprieta serializer
        val ser: Serializer[T] = serializers.values
          .find(_.typeId == typeId)
          .head
          .asInstanceOf[Serializer[T]]

        load[T](objId)(ser)
    }
  }

  def typeOf(objId: Int): Option[Int] = serData.typeOf(objId)

  def resolveObjects(list: List[Any]): List[Int] = list map {

    case v: Int => v
    case str: String => stringInterner getOrStore str
    case b: Boolean => if(b) 1 else 0
    case obj =>
      val clazzOf = obj.getClass
      val ser = serializers(clazzOf).asInstanceOf[Serializer[Any]]

      // write object
      store(obj)(ser)
  }

  def store[T](obj: T)(implicit serializer: Serializer[T]): Int = {

    val data = resolveObjects(serializer marshall obj)

    serData.store(serializer.typeId, data)
  }

  private def internDataTypes(data: List[Any]): List[Int] = data.map {

    case s: String => stringInterner getOrStore s
    case i: Int => i
  }

  def load[T](objId: Int)(implicit serializer: Serializer[T]): T = {

    val intData = serData.load(objId, serializer.typeId).toArray

    serializer unmarshall MyReader(intData)
  }
}
