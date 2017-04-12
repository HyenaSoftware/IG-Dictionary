package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.Number._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number, U_Umlaut}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object WeakStemClassFeminineI extends NounStemClass
{
	protected override def inflection(decl: (Number, Case)) = decl match
	{
		case (SINGULAR, _)	=> "i"
		case (PLURAL, _)		=> ""	// it doesn't exist
	}
}
