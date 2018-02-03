package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Explicit_U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object WeakStemClassFeminineU extends NounStemClass
{
	def transformationsFor(decl: (GNumber, Case)) =  decl match
	{
		case (PLURAL, GENITIVE) => List()
		case (SINGULAR, NOMINATIVE) => List()
		case _ => List(Explicit_U_Umlaut)
	}

	override def inflection(decl: (GNumber, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE)											=> "a"
		case (SINGULAR, ACCUSATIVE | DATIVE | GENITIVE)	=> "u"

		case (PLURAL, NOMINATIVE | ACCUSATIVE)	=> "ur"
		case (PLURAL, DATIVE)										=> "um"
		case (PLURAL, GENITIVE)									=> "na"
	}
}
