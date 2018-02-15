package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Explicit_I_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounType

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object StrongStemClassMascR extends NounStemClass
{
	override def transformationFor(decl: NounType) = decl match {

		case (SINGULAR, DATIVE) => Some(Explicit_I_Umlaut)
		case (PLURAL, NOMINATIVE | ACCUSATIVE) => Some(Explicit_I_Umlaut)
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
