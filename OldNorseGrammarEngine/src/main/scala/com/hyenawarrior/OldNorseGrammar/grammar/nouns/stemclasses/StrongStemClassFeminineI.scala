package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounType

/**
	* Created by HyenaWarrior on 2017.07.28..
	*
	* This class includes feminine nouns derived by the suffix -an, which changes
  * to -un by u-umlaut in all cases in the singular except the genitive, and in the
  * dative plural. T
	*/
object StrongStemClassFeminineI extends NounStemClass
{
	override def transformationFor(decl: NounType) = decl match {

		case (SINGULAR, NOMINATIVE | ACCUSATIVE | DATIVE) | (PLURAL, DATIVE) => Some(U_Umlaut)	// lost -u stem end
		case _ => None
	}

	override def inflection(decl: NounType) = decl match	{

		case (SINGULAR, NOMINATIVE | ACCUSATIVE | DATIVE) => ""
		case (SINGULAR, GENITIVE)		=> "ar"

		case (PLURAL, NOMINATIVE | ACCUSATIVE)		=> "ir"
		case (PLURAL, DATIVE)				=> "um"
		case (PLURAL, GENITIVE)			=> "a"
	}
}
