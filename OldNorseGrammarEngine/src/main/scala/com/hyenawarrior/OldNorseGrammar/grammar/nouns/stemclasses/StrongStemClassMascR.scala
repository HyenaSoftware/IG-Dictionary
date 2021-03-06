package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.I_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounType
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object StrongStemClassMascR extends NounStemClassEnum with MasculineNoun {

	override def toString = "Strong masculine R-class"

  override def transformationFor(decl: NounType) = decl match {

		case (SINGULAR, DATIVE) => Some(I_Umlaut)
		case (PLURAL, NOMINATIVE | ACCUSATIVE) => Some(I_Umlaut)
		case _ => None
	}

	override def inflection(decl: NounType) = decl match {

		case (SINGULAR | PLURAL, NOMINATIVE) => "r"
		case (SINGULAR, ACCUSATIVE)	=> ""
		case (SINGULAR, DATIVE)			=> "i"
		case (SINGULAR, GENITIVE)		=> "s"

		case (PLURAL, ACCUSATIVE)		=> "r"
		case (PLURAL, GENITIVE)		=> "a"
		case (PLURAL, DATIVE)		=> "um"
	}
}
