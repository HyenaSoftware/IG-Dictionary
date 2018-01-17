package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.specialtransforms

/**
  * Created by HyenaWarrior on 2018.01.17..
  */
object StrongVerbSecondClassIUmlaut {

  private val joForm = "^(.*)jó(.*)$".r
  private val yForm = "^(.*)ý(.*)$".r

  def apply(str: String): Option[String] = str match {

    case joForm(head, tail) => Some(head + "ý" + tail)
    case _ => None
  }

  def unapply(str: String): Option[String] = str match {

    case yForm(head, tail) => Some(head + "jó" + tail)
    case _ => None
  }
}
