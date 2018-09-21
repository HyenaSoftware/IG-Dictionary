package com.hyenawarrior.OldNorseGrammar.grammar.adjectival

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber, Gender}

import scala.language.implicitConversions

/**
  * Created by HyenaWarrior on 2018.06.11..
  */
object core {

  case class AdjectiveFormType(adjType: AdjectiveType, number: GNumber, gender: Gender, caze: Case) {

    override def toString: String = Seq(number, caze, gender, adjType).map(ABBREVATIONS_OF).mkString(" ")
  }

  implicit def toTuple(adjForm: AdjectiveFormType): (AdjectiveType, GNumber, Gender, Case) = (adjForm.adjType, adjForm.number, adjForm.gender, adjForm.caze)
  implicit def fromTuple(tuple: (AdjectiveType, GNumber, Gender, Case)): AdjectiveFormType = AdjectiveFormType(tuple._1, tuple._2, tuple._3, tuple._4)

  private val ABBREVATIONS_OF = Map[Any, String](

    //
    POSITIVE_INDEFINITE -> "INDF",
    POSITIVE_DEFINITE -> "DEF",
    COMPARATIVE -> "COMP",
    SUPERLATIVE_INDEFINITE -> "SUPL INDEF",
    SUPERLATIVE_DEFINITE -> "SUPL DEF",
    DETERMINERS -> "DET",

    // numbers
    SINGULAR -> "SG",
    PLURAL   -> "PL",

    // genders
    MASCULINE -> "M",
    FEMININE -> "F",
    NEUTER -> "N",

    // cases
    NOMINATIVE -> "NOM",
    ACCUSATIVE -> "ACC",
    DATIVE     -> "DAT",
    GENITIVE   -> "GEN"
  )
}
