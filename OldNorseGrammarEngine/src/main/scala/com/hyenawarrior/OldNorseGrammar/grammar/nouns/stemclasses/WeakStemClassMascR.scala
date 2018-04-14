package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.I_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounType
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum

/**
  * Created by HyenaWarrior on 2017.04.09..
  */
object WeakStemClassMascR extends NounStemClassEnum with MasculineNoun {

  override def toString = "Weak masculine R-class"

  override def transformationFor(decl: NounType) =  decl match {

    case (PLURAL, NOMINATIVE | ACCUSATIVE) => Some(I_Umlaut)
    case _ => None
  }

  override def inflection(decl: (GNumber, Case)) = decl match	{

    case (SINGULAR, NOMINATIVE)											=> "i"
    case (SINGULAR, ACCUSATIVE | DATIVE | GENITIVE)	=> "a"

    case (PLURAL, NOMINATIVE)	=> "r"
    case (PLURAL, ACCUSATIVE)	=> "r"

    case (PLURAL, DATIVE)										=> "um"
    case (PLURAL, GENITIVE)									=> "a"
  }
}
