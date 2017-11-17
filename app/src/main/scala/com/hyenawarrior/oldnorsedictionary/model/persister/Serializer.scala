package com.hyenawarrior.oldnorsedictionary.model.persister

import scala.reflect.ClassTag

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
trait Serializer[T] {

  val typeId: Int

  def marshall(obj: T): List[Any]
  def unmarshall(reader: Reader): T
}

/**
  * Created by HyenaWarrior on 2017.11.11..
  */

object Serializer {

  implicit val ClassOfInt = ClassTag.Int
  implicit val ClassOfBoolean = ClassTag.Boolean
  implicit val ClassOfString = ClassTag(classOf[String])
}