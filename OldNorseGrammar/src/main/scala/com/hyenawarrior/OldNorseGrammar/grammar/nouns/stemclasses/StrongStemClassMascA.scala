package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.Number._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object StrongStemClassMascA extends NounStemClass
{
	override protected def inflection(decl: (Number, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE) => "r"
		case (SINGULAR, ACCUSATIVE)	=> ""
		case (SINGULAR, DATIVE)			=> "i"
		case (SINGULAR, GENITIVE)		=> "s"	 // "s"

		case (PLURAL, NOMINATIVE)		=> "ar"
		case (PLURAL, ACCUSATIVE)		=> "a"
		case (PLURAL, DATIVE)				=> "um"
		case (PLURAL, GENITIVE)			=> "a"	 // "s"
	}
}
