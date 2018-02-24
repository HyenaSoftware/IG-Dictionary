package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.{ACCUSATIVE, DATIVE, GENITIVE, NOMINATIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object WeakStemClassFeminineI extends NounStemClass
{
	override def inflection(decl: (GNumber, Case)) = decl match	{

		case (SINGULAR, _)	=> "i"

		// in most of the cases it's obsoleted
		case (PLURAL, NOMINATIVE | ACCUSATIVE) => "ar"
		case (PLURAL, GENITIVE)             	 => "a"
		case (PLURAL, DATIVE)               	 => "um"
	}
}
