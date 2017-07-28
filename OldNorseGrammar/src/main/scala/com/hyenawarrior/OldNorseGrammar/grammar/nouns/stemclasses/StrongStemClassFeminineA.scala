package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.07.28..
	*/
class StrongStemClassFeminineA extends NounStemClass
{
	override protected def inflection(decl: (GNumber, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE | ACCUSATIVE | DATIVE) => ???
		case (SINGULAR, GENITIVE)		=> "ar"

		case (PLURAL, NOMINATIVE | ACCUSATIVE)		=> "ar"
		case (PLURAL, DATIVE)				=> "um"
		case (PLURAL, GENITIVE)			=> "a"
	}
}