package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.07.28..
	*/
object StrongStemClassMascI extends NounStemClass
{
	override def transformationsFor(decl: (GNumber, Case)) =  decl match
	{
		// long stems have i-umlaut
		case _ => List()
	}

	override protected def inflection(decl: (GNumber, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE) => "r"
		case (SINGULAR, ACCUSATIVE | DATIVE)	=> ""
		case (SINGULAR, GENITIVE)		=> "ar"	 // "s"

		case (PLURAL, NOMINATIVE)		=> "ir"
		case (PLURAL, ACCUSATIVE)		=> "i"
		case (PLURAL, DATIVE)				=> "um"
		case (PLURAL, GENITIVE)			=> "a"
	}
}
