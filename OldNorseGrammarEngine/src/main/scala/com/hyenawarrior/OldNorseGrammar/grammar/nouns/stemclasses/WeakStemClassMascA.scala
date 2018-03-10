package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum

/**
  * Created by HyenaWarrior on 2017.07.28..
  */
object WeakStemClassMascA extends NounStemClassEnum {

  override def toString = "Weak masculine A-class"

  override def inflection(decl: (GNumber, Case)) = decl match {

    case (SINGULAR, NOMINATIVE)											=> "i"
    case (SINGULAR, ACCUSATIVE | DATIVE | GENITIVE)	=> "a"

    case (PLURAL, NOMINATIVE)	=> "ar"
    case (PLURAL, ACCUSATIVE)	=> "a"

    case (PLURAL, DATIVE)										=> "um"
    case (PLURAL, GENITIVE)									=> "a"
  }
}
