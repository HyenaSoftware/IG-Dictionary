package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

/**
  * Created by HyenaWarrior on 2018.02.04..
  */
trait InvertableTransformation {

  def apply(str: String): Option[String]

  def unapply(str: String): Option[String]
}
