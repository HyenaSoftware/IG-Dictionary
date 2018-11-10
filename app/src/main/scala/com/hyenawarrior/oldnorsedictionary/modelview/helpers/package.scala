package com.hyenawarrior.oldnorsedictionary.modelview

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{VerbModeEnum, VerbTenseEnum, VerbVoice}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber, Gender, Pronoun}
import com.hyenawarrior.auxiliary.enum.EnumConst

/**
  * Created by HyenaWarrior on 2018.02.24..
  */
package object helpers {

  private val ABBREVATIONS_OF = Map[Any, String](

    SG_1 -> "SG1",
    SG_2 -> "SG2",
    SG_3 -> "SG3",

    PL_1 -> "PL1",
    PL_2 -> "PL2",
    PL_3 -> "PL3",

    // Verb tenses
    PRESENT -> "PRS",
    PAST    -> "PST",

    // Voices
    ACTIVE        -> "ACT",
    MEDIO_PASSIVE -> "MID-PAS",

    // Moods
    INFINITIVE  -> "INF",
    INDICATIVE  -> "IND",
    SUBJUNCTIVE -> "SBJV",
    IMPERATIVE  -> "IMP",
    PARTICIPLE  -> "PTCP",

    // numbers
    SINGULAR -> "SG",
    PLURAL   -> "PL",

    // cases
    NOMINATIVE -> "NOM",
    ACCUSATIVE -> "ACC",
    DATIVE     -> "DAT",
    GENITIVE   -> "GEN"
  )

  def abbrevationOf[T](obj: T): String = obj match {

    case ((number: GNumber, caze: Case), definite: Boolean) =>
      abbrevationOf(number) + " " + abbrevationOf(caze) + (if(definite) " DEF" else "")

    case (mood: VerbModeEnum, voice: VerbVoice, optTense: Option[VerbTenseEnum], optPronoun: Option[Pronoun]) =>

      val md = Some(helpers.abbrevationOf(mood))
      val vc = Some(helpers.abbrevationOf(voice))
      val ts = optTense.map(helpers.abbrevationOf)
      val pr = optPronoun.map(helpers.abbrevationOf)

      Seq(ts, md, pr, vc).flatten.mkString(" ")

    case _ => ABBREVATIONS_OF getOrElse(obj, obj.toString)
  }

  trait EnumOrdering[T <: EnumConst[T]] extends Ordering[T] {

    override def compare(x: T, y: T): Int = x.id() - y.id()
  }

  implicit object VerbVoiceOrdering extends EnumOrdering[VerbVoice]
  implicit object VerbModeOrdering extends EnumOrdering[VerbModeEnum]
  implicit object VerbTenseOrdering extends EnumOrdering[VerbTenseEnum]
  implicit object PronounOrdering extends EnumOrdering[Pronoun]
  implicit object GNumberOrdering extends EnumOrdering[GNumber]
  implicit object CaseOrdering extends EnumOrdering[Case]
  implicit object GenderOrdering extends EnumOrdering[Gender]
  implicit object AdjectiveTypeOrdering extends EnumOrdering[AdjectiveType]
}
