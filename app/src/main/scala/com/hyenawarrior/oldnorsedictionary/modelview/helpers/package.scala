package com.hyenawarrior.oldnorsedictionary.modelview

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{VerbModeEnum, VerbTenseEnum, VerbVoice}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber, Pronoun}

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

    case (number: GNumber, caze: Case) =>
      abbrevationOf(number) + " " + abbrevationOf(caze)

    case (mood: VerbModeEnum, voice: VerbVoice, optTense: Option[VerbTenseEnum], optPronoun: Option[Pronoun]) =>

      val md = Some(helpers.abbrevationOf(mood))
      val vc = Some(helpers.abbrevationOf(voice))
      val ts = optTense.map(helpers.abbrevationOf)
      val pr = optPronoun.map(helpers.abbrevationOf)

      Seq(ts, md, pr, vc).flatten.mkString(" ")

    case _ => ABBREVATIONS_OF getOrElse(obj, obj.toString)
  }
}
