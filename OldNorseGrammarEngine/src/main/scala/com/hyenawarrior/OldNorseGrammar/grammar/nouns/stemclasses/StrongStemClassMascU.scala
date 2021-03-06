package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{I_Umlaut, U_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounType
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum

/**
  * Created by HyenaWarrior on 2017.07.28..
  *
  * Nouns derived with the suffix -nað/nuð also originally belong to this class,
  */
object StrongStemClassMascU extends NounStemClassEnum with MasculineNoun {

  override def toString = "Strong masculine U-class"

  override def thematicVowel: Option[String] = Some("U")

  override def transformationFor(decl: NounType) =  decl match {

    case (SINGULAR, NOMINATIVE | ACCUSATIVE)	| (PLURAL, ACCUSATIVE | DATIVE)	=> Some(U_Umlaut)	// lost -u stem end
    case (SINGULAR, DATIVE) 									| (PLURAL, NOMINATIVE) 					=> Some(I_Umlaut)
    case _ => None
  }

  override def inflection(decl: NounType) = decl match {

    case (SINGULAR, NOMINATIVE) => "r"
    case (SINGULAR, ACCUSATIVE)	=> ""
    case (SINGULAR, DATIVE)			=> "i"
    case (SINGULAR, GENITIVE)		=> "ar"

    case (PLURAL, NOMINATIVE)		=> "ir"
    case (PLURAL, ACCUSATIVE)		=> "u"
    case (PLURAL, DATIVE)				=> "um"
    case (PLURAL, GENITIVE)			=> "a"
  }
}
