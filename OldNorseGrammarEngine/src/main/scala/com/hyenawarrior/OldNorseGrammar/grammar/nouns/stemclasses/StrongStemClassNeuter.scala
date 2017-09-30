package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Explicit_U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.07.28..
	*/
object StrongStemClassNeuter extends NounStemClass
{
	override def transformationsFor(decl: (GNumber, Case)) =  decl match
	{
		// long stems have i-umlaut
		case (PLURAL, NOMINATIVE | ACCUSATIVE | DATIVE) 	=> List(Explicit_U_Umlaut)
		case _ => List()
	}

	override protected def inflection(decl: (GNumber, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE | ACCUSATIVE) => ""
		case (SINGULAR, DATIVE)		=> "i"
		case (SINGULAR, GENITIVE)	=> "s"	// fé: -ar

		case (PLURAL, NOMINATIVE | ACCUSATIVE)		=> "ar"
		case (PLURAL, DATIVE)				=> "um"
		case (PLURAL, GENITIVE)			=> "a"
	}
}
