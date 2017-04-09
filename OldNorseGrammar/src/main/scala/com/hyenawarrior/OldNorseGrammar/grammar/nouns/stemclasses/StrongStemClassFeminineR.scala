package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.Number._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, I_Umlaut, Number, U_Umlaut}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object StrongStemClassFeminineR extends NounStemClass
{
	override def transformationsFor(decl: (Number, Case)) =  decl match
	{
		case (SINGULAR, cs) if cs != GENITIVE => List(U_Umlaut)
		case (PLURAL, NOMINATIVE | ACCUSATIVE) => List(I_Umlaut)
		case (PLURAL, DATIVE) => List(U_Umlaut)
		case _ => List.empty
	}

	protected override def inflection(decl: (Number, Case)) = decl match
	{
		// auto umlaut for SNG-NOM and SNG-ACC
		// root -> u-umlaut
		case (SINGULAR, cs) if cs != GENITIVE		=> ""
		case (PLURAL, NOMINATIVE | ACCUSATIVE)	=> "r"
		case (PLURAL, DATIVE)										=> "um"
		case (SINGULAR, GENITIVE)								=> "ar"
		case (PLURAL, GENITIVE)									=> "a"
	}
}
