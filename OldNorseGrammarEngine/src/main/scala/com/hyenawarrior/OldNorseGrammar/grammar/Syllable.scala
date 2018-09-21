package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable.Length
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.{isLong, isShort}
import com.hyenawarrior.auxiliary.enum.{EnumConst, EnumLike}

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
object Syllable {

  def lengthOf(nucleus: String, coda: String): Length = {

    (nucleus, coda.toSeq) match {

      case (v, Seq()) if isShort(v) => Length.SHORT
      case (v, Seq()) if isLong(v)  => Length.LONG
      case (v, Seq(c))      if isShort(v) => Length.LONG
      case (v, Seq(c, _*))  if isShort(v) => Length.OVERLONG
      case (v, _)           if isLong(v)  => Length.OVERLONG
    }
  }

  case class Length(name: String, countOfMora: Int) extends EnumConst[Length] {

    override def toString: String = name
  }

  implicit object Length extends EnumLike[Length] {

    val SHORT = Length("Short", 1)
    val LONG = Length("Long", 2)
    val OVERLONG = Length("Overlong", 3)
  }
}

case class Syllable(onset: String, nucleus: String, coda: String, isStressed: Boolean, length: Length) {

  val letters = onset + nucleus + coda

  def replaceNucleus(newNucleus: String) = Syllable(onset, newNucleus, coda, isStressed, length)
}
