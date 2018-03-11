package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber, Gender}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns._

/**
  * Created by HyenaWarrior on 2018.03.11..
  */
trait FeminineNoun {

  def associatedGender = Gender.FEMININE

  def cliticFor(decl: NounType): String = decl match {

    case (SINGULAR, NOMINATIVE) => "in"
    case (SINGULAR, ACCUSATIVE) => "ina"
    case (SINGULAR, DATIVE)     => "inni"
    case (SINGULAR, GENITIVE)   => "innar"

    case (PLURAL, NOMINATIVE) => "inar"
    case (PLURAL, ACCUSATIVE) => "inar"
    case (PLURAL, DATIVE)     => "inum"
    case (PLURAL, GENITIVE)   => "inna"
  }
}
