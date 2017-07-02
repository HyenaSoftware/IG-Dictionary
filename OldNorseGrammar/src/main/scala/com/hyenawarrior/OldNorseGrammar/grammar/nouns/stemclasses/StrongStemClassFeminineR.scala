package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Explicit_I_Umlaut, Explicit_U_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object StrongStemClassFeminineR extends NounStemClass
{
	override def transformationsFor(decl: (GNumber, Case)) =  decl match
	{
		case (SINGULAR, cs) if cs != GENITIVE => List(Explicit_U_Umlaut)
		case (PLURAL, NOMINATIVE | ACCUSATIVE) => List(Explicit_I_Umlaut)
		case (PLURAL, DATIVE) => List(Explicit_U_Umlaut)
		case _ => List()
	}

	protected override def inflection(decl: (GNumber, Case)) = decl match
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
