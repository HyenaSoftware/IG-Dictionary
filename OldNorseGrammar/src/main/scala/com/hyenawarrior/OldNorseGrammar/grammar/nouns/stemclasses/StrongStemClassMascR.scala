package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.Number._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, I_Umlaut, Number, WordTransformation}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object StrongStemClassMascR extends NounStemClass
{
	override def transformationsFor(decl: (Number, Case)): List[WordTransformation] = decl match
	{
		case (SINGULAR, DATIVE) => List(I_Umlaut)
		case (PLURAL, NOMINATIVE | ACCUSATIVE) => List(I_Umlaut)
		case _ => List.empty
	}

	override protected def inflection(decl: (Number, Case)) = decl match
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
