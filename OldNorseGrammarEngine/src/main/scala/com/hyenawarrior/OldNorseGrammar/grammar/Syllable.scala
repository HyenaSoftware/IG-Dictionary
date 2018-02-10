package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable.Length
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.{isLong, isShort}
import com.hyenawarrior.auxiliary.EnumLike

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
object Syllable {

  def lengthOf(nucleus: String, coda: String): Length = {

    (nucleus, coda) match {

      case (v, "") if isShort(v) => Length.SHORT
      case (v, "") if isLong(v)  => Length.LONG
      case (v, cs) if isShort(v) => Length.LONG
      case (v, cs) if isLong(v)  => Length.OVERLONG
    }
  }

  case class Length(name: String, countOfMora: Int) {

    override def toString: String = name
  }

  object Length extends EnumLike[String, Length] {

    val SHORT = Length("Short", 1)
    val LONG = Length("Long", 2)
    val OVERLONG = Length("Overlong", 3)
  }
}

case class Syllable(onset: String, nucleus: String, coda: String, isStressed: Boolean, length: Length) {

  val letters = onset + nucleus + coda
}
