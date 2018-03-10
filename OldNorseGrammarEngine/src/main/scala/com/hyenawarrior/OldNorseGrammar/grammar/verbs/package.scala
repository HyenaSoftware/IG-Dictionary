package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{GNumber, Pronoun}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.{ACTIVE, MEDIO_PASSIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{VerbModeEnum, VerbTenseEnum, VerbVoice}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem._

/**
  * Created by HyenaWarrior on 2017.04.22..
  *
  * I. "gefa" is a string representation of a verbform
  * II. StrongFinitiveVerb(stem, classDesc, pronoun, tense) is an object representation of a verbform
  *
  * III. every string representation of a verb has an object representation and vica-versa
  * IV. set of representation of verbforms is a verb
  * V. a verb has many verbforms, which have string and object representations
  *
  */
package object verbs {

  type VerbType = (VerbModeEnum, VerbVoice, Option[VerbTenseEnum], Option[Pronoun])

  def tenseToStem(tense: VerbTenseEnum, number: GNumber): EnumVerbStem = (tense, number) match	{

    case (PRESENT, _)			=> PRESENT_STEM
    case (PAST, SINGULAR) => PRETERITE_SINGULAR_STEM
    case (PAST, PLURAL)		=> PRETERITE_PLURAL_STEM
  }

  def stemFrom(verbType: VerbType): EnumVerbStem = verbType match {

    case (INFINITIVE,               _, None,          None   ) => PRESENT_STEM
    case (INDICATIVE | SUBJUNCTIVE, _, Some(PRESENT), Some(_)) => PRESENT_STEM
    case (INDICATIVE,  ACTIVE, 				Some(PAST), Some(Pronoun(SINGULAR, _))) => PRETERITE_SINGULAR_STEM
    case (INDICATIVE,  MEDIO_PASSIVE, Some(PAST), Some(Pronoun(SINGULAR, 1))) => PRETERITE_PLURAL_STEM
    case (INDICATIVE,  MEDIO_PASSIVE, Some(PAST), Some(Pronoun(SINGULAR, 2 | 3))) => PRETERITE_SINGULAR_STEM
    case (INDICATIVE,  _, 						Some(PAST), Some(Pronoun(PLURAL,   _))) => PRETERITE_PLURAL_STEM
    case (SUBJUNCTIVE, _, 						Some(PAST), Some(Pronoun(_,        _))) => PRETERITE_PLURAL_STEM

    case (PARTICIPLE,  _,             Some(PRESENT), None) => PRESENT_STEM
    case (PARTICIPLE,  _,             Some(PAST),    None) => PERFECT_STEM
    case (IMPERATIVE,  _,             Some(PRESENT), _   ) => PRESENT_STEM
    case _ => ???
  }

  def stemToTense(stem: EnumVerbStem): (VerbTenseEnum, Option[GNumber]) = stem match	{

    case PRESENT_STEM							=> PRESENT	-> None
    case PRETERITE_SINGULAR_STEM	=> PAST			-> Some(SINGULAR)
    case PRETERITE_PLURAL_STEM		=> PAST			-> Some(PLURAL)
  }
}
