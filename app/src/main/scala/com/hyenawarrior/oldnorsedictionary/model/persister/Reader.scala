package com.hyenawarrior.oldnorsedictionary.model.persister

import scala.reflect.ClassTag

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
trait Reader {

  def apply[T](i: Int)(implicit clazz: ClassTag[T]): T
}
