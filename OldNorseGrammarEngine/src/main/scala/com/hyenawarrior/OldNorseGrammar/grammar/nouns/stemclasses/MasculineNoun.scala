package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender
import com.hyenawarrior.OldNorseGrammar.grammar.nouns._

/**
  * Created by HyenaWarrior on 2018.03.11..
  */
trait MasculineNoun {

  def associatedGender = Gender.MASCULINE

  def cliticFor(decl: NounType): String = decl match {

    case (SINGULAR, NOMINATIVE) => "inn"
    case (SINGULAR, ACCUSATIVE) => "inn"
    case (SINGULAR, DATIVE)     => "inum"
    case (SINGULAR, GENITIVE)   => "ins"

    case (PLURAL, NOMINATIVE) => "inir"
    case (PLURAL, ACCUSATIVE) => "ina"
    case (PLURAL, DATIVE)     => "inum"
    case (PLURAL, GENITIVE)   => "inna"
  }
}
