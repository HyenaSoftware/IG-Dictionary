package com.hyenawarrior.OldNorseGrammar.grammar

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
case class Syllable(onset: String, nucleus: String, coda: String, isStressed: Boolean) {

  val letters = onset + nucleus + coda
}
