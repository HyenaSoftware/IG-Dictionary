package com.hyenawarrior.oldnorsedictionary.model.persister

import scala.reflect.ClassTag
import com.hyenawarrior.oldnorsedictionary.model.persister.Serializer._


/**
  * Created by HyenaWarrior on 2017.11.16..
  */
abstract class Persister {

  def stringInterner: StringInterner

  protected def serData: SerData

  private case class MyReader(data: IndexedSeq[Int]) extends Reader {

    override def apply[T](i: Int)(implicit clazz: ClassTag[T]): T = clazz match {

      case ClassOfString => stringInterner(data(i)).asInstanceOf[T]
      case ClassOfInt | ClassOfBoolean => data(i).asInstanceOf[T]
    }
  }

  def typeOf(objId: Int): Option[Int] = serData.typeOf(objId)

  def store[T](obj: T)(implicit serializer: Serializer[T]): Int = {

    val data: List[Any] = serializer.marshall(obj)

    val internedData = internDataTypes(data)

    serData.store(serializer.typeId, internedData)
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
