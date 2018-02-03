package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Explicit_I_Umlaut, WordTransformation}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object StrongStemClassMascR extends NounStemClass
{
	def transformationsFor(decl: (GNumber, Case)): List[WordTransformation] = decl match
	{
		case (SINGULAR, DATIVE) => List(Explicit_I_Umlaut)
		case (PLURAL, NOMINATIVE | ACCUSATIVE) => List(Explicit_I_Umlaut)
		case _ => List()
	}

	override def inflection(decl: (GNumber, Case)) = decl match
	{
		case (SINGULAR | PLURAL, NOMINATIVE) => "r"
		case (SINGULAR, ACCUSATIVE)	=> ""
		case (SINGULAR, DATIVE)			=> "i"
		case (SINGULAR, GENITIVE)		=> "s"

		case (PLURAL, ACCUSATIVE)		=> "r"
		case (PLURAL, GENITIVE)		=> "a"
		case (PLURAL, DATIVE)		=> "um"
	}
}
