package com.hyenawarrior.OldNorseGrammar.grammar.adjectival

import com.hyenawarrior.auxiliary.enum.{EnumConst, EnumLike}

/**
  * Created by HyenaWarrior on 2018.06.11..
  */
object enums {

  class AdjectiveType(name: String) extends EnumConst[AdjectiveType] {

    override def toString: String = name
  }

  implicit object AdjectiveType extends EnumLike[AdjectiveType] {

    val POSITIVE_DEFINITE = new AdjectiveType("Positive definite")
    val POSITIVE_INDEFINITE = new AdjectiveType("Positive indefinite")
    val COMPARATIVE = new AdjectiveType("Comparative")
    val SUPERLATIVE_DEFINITE = new AdjectiveType("Superlative definite")
    val SUPERLATIVE_INDEFINITE = new AdjectiveType("Superlative indefinite")
    val DETERMINERS = new AdjectiveType("Strong N-type")
  }
}
