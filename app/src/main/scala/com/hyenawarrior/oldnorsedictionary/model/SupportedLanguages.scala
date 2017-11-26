package com.hyenawarrior.oldnorsedictionary.model

/**
  * Created by HyenaWarrior on 2017.11.16..
  */
case class Language(name: String, id: Int)

object SupportedLanguages {

  // English is by default so doesn't mention it
  val OldNorse = Language("Old Norse", 0)
}
