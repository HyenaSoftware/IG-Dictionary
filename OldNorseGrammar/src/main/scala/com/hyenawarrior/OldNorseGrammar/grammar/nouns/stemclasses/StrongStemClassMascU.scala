package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Explicit_I_Umlaut, Explicit_U_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.07.28..
	*
	* Nouns derived with the suffix -nað/nuð also originally belong to this class,
	*/
object StrongStemClassMascU extends NounStemClass
{
	override def transformationsFor(decl: (GNumber, Case)) =  decl match
	{
		// long stems have i-umlaut
		case (SINGULAR, NOMINATIVE | ACCUSATIVE)	| (PLURAL, ACCUSATIVE | DATIVE)	=> List(Explicit_U_Umlaut)	// lost -u stem end
		case (SINGULAR, DATIVE) 									| (PLURAL, NOMINATIVE) 					=> List(Explicit_I_Umlaut)
		case _ => List()
	}

	override protected def inflection(decl: (GNumber, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE) => "r"
		case (SINGULAR, ACCUSATIVE)	=> ""
		case (SINGULAR, DATIVE)			=> "i"
		case (SINGULAR, GENITIVE)		=> "ar"

		case (PLURAL, NOMINATIVE)		=> "ir"
		case (PLURAL, ACCUSATIVE)		=> "u"
		case (PLURAL, DATIVE)				=> "um"
		case (PLURAL, GENITIVE)			=> "a"
	}
}
