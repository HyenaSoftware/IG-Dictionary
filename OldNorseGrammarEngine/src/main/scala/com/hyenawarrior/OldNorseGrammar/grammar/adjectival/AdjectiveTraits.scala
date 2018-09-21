package com.hyenawarrior.OldNorseGrammar.grammar.adjectival

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology._

/**
  * Created by HyenaWarrior on 2018.06.12..
  */
object AdjectiveTraits {

  def inflectionWithComparsionFor(declension: AdjectiveFormType): String = {

    val inflPrefix = declension.adjType match {

      case COMPARATIVE => "ar"
      case SUPERLATIVE_INDEFINITE | SUPERLATIVE_DEFINITE => "ast"
      case _ => ""
    }

    inflPrefix + inflectionFor(declension)
  }

  def inflectionFor(declension: AdjectiveFormType): String = core.toTuple(declension) match {

    // indefinite adjectives in the positive and superlative
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE) => "r"
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, MASCULINE, ACCUSATIVE) => "an"
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, MASCULINE, DATIVE) => "um"
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, MASCULINE, GENITIVE) => "s"

    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, FEMININE, NOMINATIVE) => ""
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, FEMININE, ACCUSATIVE) => "a"
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, FEMININE, DATIVE) => "ri"
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, FEMININE, GENITIVE) => "rar"

    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, NEUTER, NOMINATIVE | ACCUSATIVE) => "t"
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, NEUTER, DATIVE) => "u"
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, NEUTER, GENITIVE) => "s"

      // plurals
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, PLURAL, MASCULINE, NOMINATIVE) => "ir"
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, PLURAL, MASCULINE, ACCUSATIVE) => "a"

    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, PLURAL, FEMININE, NOMINATIVE | ACCUSATIVE) => "ar"
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, PLURAL, NEUTER, NOMINATIVE | ACCUSATIVE) => ""

    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, PLURAL, _, GENITIVE) => "ra"
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, PLURAL, _, DATIVE) => "um"

    // determinters
    case ( DETERMINERS, SINGULAR, MASCULINE, ACCUSATIVE) => "n"
    case ( DETERMINERS, SINGULAR, NEUTER, NOMINATIVE | ACCUSATIVE) => "t" // + deletes 'n's

    // definite adjectives in the positive and superlative
    case (POSITIVE_DEFINITE | SUPERLATIVE_DEFINITE, SINGULAR, MASCULINE, NOMINATIVE) => "i"
    case (POSITIVE_DEFINITE | SUPERLATIVE_DEFINITE, SINGULAR, FEMININE, ACCUSATIVE | DATIVE | GENITIVE) => "u"
    case (POSITIVE_DEFINITE | SUPERLATIVE_DEFINITE, SINGULAR, _, _) => "a"

    case (POSITIVE_DEFINITE | SUPERLATIVE_DEFINITE, PLURAL, _, NOMINATIVE | ACCUSATIVE | GENITIVE) => "u"
    case (POSITIVE_DEFINITE | SUPERLATIVE_DEFINITE, PLURAL, _, DATIVE) => "um"

    // comparative
    case (COMPARATIVE, SINGULAR, MASCULINE, NOMINATIVE) => "i"
    case (COMPARATIVE, SINGULAR, FEMININE, _) => "i"
    case (COMPARATIVE, SINGULAR, _, _) => "a"

    case (COMPARATIVE, PLURAL, _, NOMINATIVE | ACCUSATIVE | GENITIVE) => "i"
    case (COMPARATIVE, PLURAL, _, DATIVE) => "um"
  }

  def transformationsFor(declension: AdjectiveFormType): Option[Umlaut] = core.toTuple(declension) match {

    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, SINGULAR, FEMININE, NOMINATIVE) => Some(U_Umlaut)
    case ( POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE, PLURAL, NEUTER, NOMINATIVE | ACCUSATIVE) => Some(U_Umlaut)
    case _ => None
  }

  def uninflect(str: String, declension: AdjectiveFormType): Option[String] = {

    val inflection = inflectionWithComparsionFor(declension)

    if(str endsWith inflection) Some(stripSuffix(str, inflection)) else None
  }

  def stemSuffixFor(adjType: AdjectiveType): String = adjType match {

    case COMPARATIVE => "ar"
    case SUPERLATIVE_DEFINITE | SUPERLATIVE_INDEFINITE => "ast"
    case _ => ""
  }
}
