package com.hyenawarrior.oldnorsedictionary.model.persister

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
trait SerData {

  def typeOf(objId: Int): Option[Int]

  def store(typeId: Int, data: Seq[Int]): Int

  def load(objId: Int, typeId: Int): Seq[Int]
}
