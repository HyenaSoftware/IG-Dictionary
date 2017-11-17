package com.hyenawarrior.oldnorsedictionary.model.persister.inmemory

import com.hyenawarrior.oldnorsedictionary.model.persister.StringInterner

import scala.collection.mutable

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
object InMemoryStringInterner extends StringInterner {

  val idToStr: mutable.Map[Int, String] = mutable.Map()
  val strToId: mutable.Map[String, Int] = mutable.Map()

  override def apply(id: Int): String = idToStr(id)

  override def indexOf(s: String): Option[Int] = strToId get s

  override def store(s: String): Int = {

    val nextId = idToStr.size + 1
    idToStr.+=((nextId, s))

    strToId += ((s, nextId))

    nextId
  }
}
