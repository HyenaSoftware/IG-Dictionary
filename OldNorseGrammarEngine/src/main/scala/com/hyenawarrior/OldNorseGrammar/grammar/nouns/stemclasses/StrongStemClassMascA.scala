package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum

/**
  * Created by HyenaWarrior on 2017.04.09..
  */
object StrongStemClassMascA extends NounStemClassEnum {

  override def toString = "Strong masculine A-class"

  override def thematicVowel = Some("a")

  override def inflection(decl: (GNumber, Case)) = decl match {

    case (SINGULAR, NOMINATIVE) => "r"
    case (SINGULAR, ACCUSATIVE)	=> ""
    case (SINGULAR, DATIVE)			=> "i"
    case (SINGULAR, GENITIVE)		=> "s"	 // "s"

    case (PLURAL, NOMINATIVE)		=> "ar"
    case (PLURAL, ACCUSATIVE)		=> "a"
    case (PLURAL, DATIVE)				=> "um"
    case (PLURAL, GENITIVE)			=> "a"	 // "s"
  }
}
