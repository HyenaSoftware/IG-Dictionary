package com.hyenawarrior.oldnorsedictionary.model.persister

import java.io.DataInputStream

import com.hyenawarrior.oldnorsedictionary.model.database.serializers.HashCode

import scala.reflect.ClassTag
import com.hyenawarrior.oldnorsedictionary.model.persister.Serializer._

import scala.language.postfixOps


/**
  * Created by HyenaWarrior on 2017.11.16..
  */
abstract class Persister {

  outer =>

  def stringInterner: StringInterner

  protected def serData: SerData

  private case class MyStreamReader(dis: DataInputStream) extends Reader {

    override def apply[T]()(implicit clazz: ClassTag[T]): T = clazz match {

      case ClassOfString => stringInterner(dis.readInt).asInstanceOf[T]
      case ClassOfInt => dis.readInt.asInstanceOf[T]
      case ClassOfByte => dis.readByte.asInstanceOf[T]
      case ClassOfBoolean => dis.readBoolean().asInstanceOf[T]
    }
  }

  def typeOf(objId: Int): Option[Int] = serData.typeOf(objId)

  def resolveObjects(list: List[Any]): List[AnyVal] = list map {

    case v: Int => v
    case b: Byte => b
    case str: String => stringInterner getOrStore str
    case b: Boolean => b
  }

  def deleteAll(): Unit = serData.deleteAll()

  def delete[T](obj: T)(implicit computeHashOf: HashCode[T]): Boolean = {

    val hashCode = computeHashOf(obj)

    serData.delete(hashCode)
  }

  def store[T](obj: T)(implicit serializer: Serializer[T], computeHashOf: HashCode[T]): Int = {

    val data = resolveObjects(serializer marshall obj)
    val hashCode = computeHashOf(obj)

    serData.store(hashCode, serializer.typeId, data)
  }

  def load[T](objId: Int)(implicit serializer: Serializer[T]): T = {

    val dataStream = serData.load(objId, serializer.typeId)

    serializer unmarshall MyStreamReader(dataStream)
  }
}
