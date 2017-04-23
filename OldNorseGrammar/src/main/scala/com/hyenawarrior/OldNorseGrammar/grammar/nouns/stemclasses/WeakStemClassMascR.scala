package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{I_Umlaut, U_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber, GNumber$}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object WeakStemClassMascR extends NounStemClass
{
	override def transformationsFor(decl: (GNumber, Case)) =  decl match
	{
		case (PLURAL, NOMINATIVE | ACCUSATIVE) => List(I_Umlaut)
		case (PLURAL, DATIVE) => List(U_Umlaut)

		case _ => List.empty
	}

	protected override def inflection(decl: (GNumber, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE)											=> "i"
		case (SINGULAR, ACCUSATIVE | DATIVE | GENITIVE)	=> "a"

		case (PLURAL, NOMINATIVE)	=> "r"
		case (PLURAL, ACCUSATIVE)	=> "r"

		case (PLURAL, DATIVE)										=> "um"
		case (PLURAL, GENITIVE)									=> "a"
	}
}
