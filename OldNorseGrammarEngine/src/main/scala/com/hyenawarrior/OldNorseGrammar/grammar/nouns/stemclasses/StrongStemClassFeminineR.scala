package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{I_Umlaut, U_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounType
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum

/**
  * Created by HyenaWarrior on 2017.04.09..
  */
object StrongStemClassFeminineR extends NounStemClassEnum with FeminineNoun
{
  override def toString = "Strong feminine R-class"

  override def transformationFor(decl: NounType) =  decl match {

    case (SINGULAR, cs) if cs != GENITIVE => Some(U_Umlaut)
    case (PLURAL, NOMINATIVE | ACCUSATIVE) => Some(I_Umlaut)
    case (PLURAL, DATIVE) => Some(U_Umlaut)
    case _ => None
  }

  override def inflection(decl: NounType) = decl match {

    // auto umlaut for SNG-NOM and SNG-ACC
    // root -> u-umlaut
    case (SINGULAR, cs) if cs != GENITIVE		=> ""
    case (PLURAL, NOMINATIVE | ACCUSATIVE)	=> "r"
    case (PLURAL, DATIVE)										=> "um"
    case (SINGULAR, GENITIVE)								=> "ar"
    case (PLURAL, GENITIVE)									=> "a"
  }
}
