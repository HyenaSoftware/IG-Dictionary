package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{InvertableTransformation, U_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounType

/**
	* Created by HyenaWarrior on 2017.07.28..
	*/
object StrongStemClassNeuter extends NounStemClass
{
	override def transformationFor(decl: NounType): Option[InvertableTransformation] =  decl match {

		case (PLURAL, NOMINATIVE | ACCUSATIVE | DATIVE) 	=> Some(U_Umlaut)
		case _ => None
	}

	override def inflection(decl: NounType) = decl match {

		case (SINGULAR, NOMINATIVE | ACCUSATIVE) => ""
		case (SINGULAR, DATIVE)		=> "i"
		case (SINGULAR, GENITIVE)	=> "s"	// fé: -ar

		case (PLURAL, NOMINATIVE | ACCUSATIVE)		=> ""
		case (PLURAL, DATIVE)				=> "um"
		case (PLURAL, GENITIVE)			=> "a"
	}
}
