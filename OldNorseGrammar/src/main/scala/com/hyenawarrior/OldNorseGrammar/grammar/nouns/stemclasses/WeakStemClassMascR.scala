package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.Number._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, I_Umlaut, Number, U_Umlaut}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object WeakStemClassMascR extends NounStemClass
{
	override def transformationsFor(decl: (Number, Case)) =  decl match
	{
		case (PLURAL, NOMINATIVE | ACCUSATIVE) => List(I_Umlaut)
		case (PLURAL, DATIVE) => List(U_Umlaut)

		case _ => List.empty
	}

	protected override def inflection(decl: (Number, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE)											=> "i"
		case (SINGULAR, ACCUSATIVE | DATIVE | GENITIVE)	=> "a"

		case (PLURAL, NOMINATIVE)	=> "r"
		case (PLURAL, ACCUSATIVE)	=> "r"

		case (PLURAL, DATIVE)										=> "um"
		case (PLURAL, GENITIVE)									=> "a"
	}
}
