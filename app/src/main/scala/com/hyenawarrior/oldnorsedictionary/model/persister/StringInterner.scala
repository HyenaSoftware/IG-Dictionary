package com.hyenawarrior.oldnorsedictionary.model.persister

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
trait StringInterner {

  def apply(id: Int): String

  def indexOf(s: String): Option[Int]

  def store(s: String): Int

  def getOrStore(s: String): Int = indexOf(s) match {

    case Some(i) => i
    case None => store(s)
  }
}
