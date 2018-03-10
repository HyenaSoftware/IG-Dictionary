package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import scala.language.implicitConversions

/**
  * Created by HyenaWarrior on 2017.04.19..
  */
case class AblautGrade(rootVowel: String) {

  def occuresIn(str: String): Boolean = str.contains(rootVowel)

  override def toString: String = rootVowel
}

object AblautGrade {

  implicit def AblautGradeFromString(str: String): AblautGrade = AblautGrade(str)
}
