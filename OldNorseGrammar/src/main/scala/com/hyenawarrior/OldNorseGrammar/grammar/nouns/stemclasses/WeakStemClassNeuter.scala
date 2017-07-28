package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.07.28..
	*/
object WeakStemClassNeuter extends WeakStemClass
{
	protected override def inflection(decl: (GNumber, Case)) = decl match
	{
		case (SINGULAR, _)	=> "a"
		case (PLURAL, NOMINATIVE | ACCUSATIVE)	=> "u"
		case (PLURAL, GENITIVE)	=> "na	"

		case _ => super.inflection(decl)
	}
}
