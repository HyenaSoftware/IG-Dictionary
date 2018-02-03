package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Explicit_U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.07.28..
	*
	* This class includes feminine nouns derived by the suffix -an, which changes
  * to -un by u-umlaut in all cases in the singular except the genitive, and in the
  * dative plural. T
	*/
object StrongStemClassFeminineI extends NounStemClass
{
	def transformationsFor(decl: (GNumber, Case)) =  decl match
	{
		// long stems have i-umlaut
		case (SINGULAR, NOMINATIVE | ACCUSATIVE | DATIVE) | (PLURAL, DATIVE) => List(Explicit_U_Umlaut)	// lost -u stem end
		case _ => List()
	}

	override def inflection(decl: (GNumber, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE | ACCUSATIVE | DATIVE) => ""
		case (SINGULAR, GENITIVE)		=> "ar"

		case (PLURAL, NOMINATIVE | ACCUSATIVE)		=> "ir"
		case (PLURAL, DATIVE)				=> "um"
		case (PLURAL, GENITIVE)			=> "a"
	}
}
