package com.hyenawarrior.oldnorsedictionary.model.persister

import java.io.DataInputStream

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

  private case class MyStreamReader(dis: DataInputStream) extends Reader {

    override def apply[T]()(implicit clazz: ClassTag[T]): T = clazz match {

      case ClassOfString => stringInterner(dis.readInt).asInstanceOf[T]
      case ClassOfInt => dis.readInt.asInstanceOf[T]
      case ClassOfByte => dis.readByte.asInstanceOf[T]
      case ClassOfBoolean => dis.readBoolean().asInstanceOf[T]
      case _ =>
        val objId = dis.readInt
        val typeId = typeOf(objId) get

        // find the appropriate serializer
        val ser: Serializer[T] = serializers.values
          .find(_.typeId == typeId)
          .head
          .asInstanceOf[Serializer[T]]

        load[T](objId)(ser)
    }
  }

  def typeOf(objId: Int): Option[Int] = serData.typeOf(objId)

  def resolveObjects(list: List[Any]): List[AnyVal] = list map {

    case v: Int => v
    case b: Byte => b
    case str: String => stringInterner getOrStore str
    case b: Boolean => b
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

  def load[T](objId: Int)(implicit serializer: Serializer[T]): T = {

    val dataStream = serData.load(objId, serializer.typeId)

    serializer unmarshall MyStreamReader(dataStream)
  }
}
