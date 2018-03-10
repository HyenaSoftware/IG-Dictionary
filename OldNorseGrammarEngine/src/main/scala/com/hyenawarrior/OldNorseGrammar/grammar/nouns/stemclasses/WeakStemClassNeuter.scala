package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.07.28..
	*/
object WeakStemClassNeuter extends WeakStemClass
{
	override def toString = "Weak neuter class"

	override def inflection(decl: (GNumber, Case)) = decl match	{

		case (SINGULAR, _)	=> "a"
		case (PLURAL, NOMINATIVE | ACCUSATIVE)	=> "u"
		case (PLURAL, GENITIVE)	=> "na"

		case _ => super.inflection(decl)
	}
}
