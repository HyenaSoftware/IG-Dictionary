package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender
import com.hyenawarrior.OldNorseGrammar.grammar.nouns._

/**
  * Created by HyenaWarrior on 2018.03.11..
  */
trait NeuterNoun {

  def associatedGender = Gender.NEUTER

  def cliticFor(decl: NounType): String = decl match {

    case (SINGULAR, NOMINATIVE) => "it"
    case (SINGULAR, ACCUSATIVE) => "it"
    case (SINGULAR, DATIVE)     => "inu"
    case (SINGULAR, GENITIVE)   => "ins"

    case (PLURAL, NOMINATIVE) => "in"
    case (PLURAL, ACCUSATIVE) => "in"
    case (PLURAL, DATIVE)     => "inum"
    case (PLURAL, GENITIVE)   => "inna"
  }
}
