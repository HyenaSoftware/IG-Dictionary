package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import java.lang.String._

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{NonFinitiveMood, VerbClassEnum, VerbModeEnum, VerbTenseEnum, VerbVoice, WeakVerbClassEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.WeakVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem

/**
  * Created by HyenaWarrior on 2017.04.19..
  */
class WeakVerbForm(strRepr: String, stem: WeakVerbStem, voice: VerbVoice) extends VerbForm(strRepr) {

  def getStem: WeakVerbStem = stem
}

object WeakVerbForm {

  // Stem -> VerbForm
  def verbFrom(stem: WeakVerbStem, vt: VerbType): WeakVerbForm = {

    val str = inflect(vt, stem.stringForm(), stem.verbClass)

    new WeakVerbForm(str, stem, vt._2)
  }

  // String -> VerbForm
  def fromStringRepr(verbStrRepr: String, verbClass: WeakVerbClassEnum, vt: VerbType): WeakVerbForm = {

    val stem = uninflect(verbStrRepr, verbClass, vt)

    val verb = verbFrom(stem, vt)

    // +1 do validation:
    if(verb.strRepr != verbStrRepr) {

      val (mood: VerbModeEnum, _, optTense, optPronoun) = vt

      val pronounStr = optPronoun.map(_.toString + " person, ").getOrElse("")
      val tenseStr = optTense.map(_.name + " tense, ").getOrElse("")

      throw new RuntimeException(format("The given '%s' verbform is not correct to be a %s, %s%s%s mood verb." +
        " The verb form should be '%s'.",
        verbStrRepr, verbClass.name, pronounStr, tenseStr, mood.name, verb.strRepr))
    }

    verb
  }

  private def inflect(verbType: VerbType, stemStr: String, verbClass: VerbClassEnum): String = {

    stemStr + inflectionFor(verbType, stemStr)
  }

  private def uninflect(verbStrRepr: String, verbClass: WeakVerbClassEnum, vt: VerbType): WeakVerbStem = {

    val verbStem: EnumVerbStem = EnumVerbStem.PRESENT_STEM

    new WeakVerbStem(verbStrRepr, verbClass, verbStem, TransformationMode.Undefined)
  }

  private def inflectionFor(verbType: VerbType, stemOrVerbStr: String): String = verbType match {

    case (INDICATIVE,  voice, Some(tense), Some(pronoun)) => inflectionForFinitive(voice, tense, pronoun, stemOrVerbStr)
    case (SUBJUNCTIVE, voice, Some(tense), Some(pronoun)) => inflectionForSubj(voice, tense, pronoun, stemOrVerbStr)
    case (mood: NonFinitiveMood, voice, optTense, None) => inflectionFor(optTense, voice, mood)
  }

  private def inflectionForFinitive(voice: VerbVoice, tense: VerbTenseEnum, pronoun: Pronoun, stemOrVerbStr: String)
    = (voice, tense, pronoun) match {

    case (ACTIVE, PRESENT, SG_1) => ""
    case (ACTIVE, PRESENT, SG_2 | SG_3) => "r"
    case (ACTIVE, PRESENT, PL_1) => "um"
    case (ACTIVE, PRESENT, PL_2) => "ið"
    case (ACTIVE, PRESENT, PL_3) => "a"

    case (ACTIVE, PAST, SG_1) => ""
    case (ACTIVE, PAST, SG_2) => "ir"
    case (ACTIVE, PAST, SG_3) => "i"
    case (ACTIVE, PAST, PL_1) => "um"
    case (ACTIVE, PAST, PL_2) => "uð"
    case (ACTIVE, PAST, PL_3) => "u"

    case (MEDIO_PASSIVE, PRESENT, SG_1) => "umk"
    case (MEDIO_PASSIVE, PRESENT, SG_2 | SG_3) => "sk"
    case (MEDIO_PASSIVE, PRESENT, PL_1) => "umsk"
    case (MEDIO_PASSIVE, PRESENT, PL_2) => "izk"
    case (MEDIO_PASSIVE, PRESENT, PL_3) => "ask"

    case (MEDIO_PASSIVE, PAST, SG_1) => "umk"
    case (MEDIO_PASSIVE, PAST, SG_2 | SG_3) => "isk"
    case (MEDIO_PASSIVE, PAST, PL_1) => "umsk"
    case (MEDIO_PASSIVE, PAST, PL_2) => "uzk"
    case (MEDIO_PASSIVE, PAST, PL_3) => "usk"
  }

  private def inflectionForSubj(voice: VerbVoice, tense: VerbTenseEnum, pronoun: Pronoun, stemOrVerbStr: String)
  = (voice, tense, pronoun) match {

    case (ACTIVE, PRESENT, SG_1) => "a"
    case (ACTIVE, PRESENT, SG_2) => "ir"
    case (ACTIVE, PRESENT, SG_3) => "i"
    case (ACTIVE, PRESENT, PL_1) => "im"
    case (ACTIVE, PRESENT, PL_2) => "ið"
    case (ACTIVE, PRESENT, PL_3) => "i"

    case (ACTIVE, PAST, SG_1) => "a"
    case (ACTIVE, PAST, SG_2)  => "ir"
    case (ACTIVE, PAST, SG_3)  => "i"
    case (ACTIVE, PAST, PL_1) => "im"
    case (ACTIVE, PAST, PL_2) => "ið"
    case (ACTIVE, PAST, PL_3) => "i"

    case (MEDIO_PASSIVE, PRESENT, SG_1) => "umk"
    case (MEDIO_PASSIVE, PRESENT, SG_2 | SG_3) => "isk"
    case (MEDIO_PASSIVE, PRESENT, PL_1) => "imsk"
    case (MEDIO_PASSIVE, PRESENT, PL_2) => "izk"
    case (MEDIO_PASSIVE, PRESENT, PL_3) => "isk"

    case (MEDIO_PASSIVE, PAST, SG_1) => "umk"
    case (MEDIO_PASSIVE, PAST, SG_2 | SG_3)  => "isk"
    case (MEDIO_PASSIVE, PAST, PL_1) => "imsk"
    case (MEDIO_PASSIVE, PAST, PL_2) => "izk"
    case (MEDIO_PASSIVE, PAST, PL_3) => "isk"
  }

  private def inflectionFor(optTense: Option[VerbTenseEnum], voice: VerbVoice, mood: NonFinitiveMood): String
  = (optTense, voice, mood) match {

    case (Some(PAST),			ACTIVE, PARTICIPLE) => "r"	// adjectival declension
    case (Some(PRESENT),	ACTIVE, PARTICIPLE) => "andi"	// -andi + adjectival declension?
    case (None,			      ACTIVE, INFINITIVE) => "a"

    case (Some(PAST),	    MEDIO_PASSIVE, PARTICIPLE) => "izk"
    case (Some(PRESENT),	MEDIO_PASSIVE, PARTICIPLE) => "andisk"
    case (None,			      MEDIO_PASSIVE, INFINITIVE) => "ask"
  }
}