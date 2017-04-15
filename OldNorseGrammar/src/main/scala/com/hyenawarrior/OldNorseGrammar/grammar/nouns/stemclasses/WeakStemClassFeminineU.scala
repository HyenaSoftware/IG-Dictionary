package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.Number._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object WeakStemClassFeminineU extends NounStemClass
{
	override def transformationsFor(decl: (Number, Case)) =  decl match
	{
		case (PLURAL, GENITIVE) => Nil
		case (SINGULAR, NOMINATIVE) => Nil
		case _ => List(U_Umlaut)
	}

	protected override def inflection(decl: (Number, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE)											=> "a"
		case (SINGULAR, ACCUSATIVE | DATIVE | GENITIVE)	=> "u"

		case (PLURAL, NOMINATIVE | ACCUSATIVE)	=> "ur"
		case (PLURAL, DATIVE)										=> "um"
		case (PLURAL, GENITIVE)									=> "na"
	}
}
