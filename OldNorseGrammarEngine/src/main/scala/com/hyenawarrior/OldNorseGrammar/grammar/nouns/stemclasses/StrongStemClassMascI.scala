package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounType
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum

/**
	* Created by HyenaWarrior on 2017.07.28..
	*/
object StrongStemClassMascI extends NounStemClassEnum {

  override def toString = "Strong masculine I-class"

	override def thematicVowel: Option[String] = Some("i")

	override def transformationFor(decl: NounType) = decl match {

		// long stems have i-umlaut, but probably it doesn't worth to deal with it
		// as it's quite generic
		case _ => None
	}

	override def inflection(decl: NounType) = decl match {

		case (SINGULAR, NOMINATIVE) => "r"
		case (SINGULAR, ACCUSATIVE | DATIVE)	=> ""
		case (SINGULAR, GENITIVE)		=> "s"

		case (PLURAL, NOMINATIVE)		=> "ir"
		case (PLURAL, ACCUSATIVE)		=> "i"
		case (PLURAL, DATIVE)				=> "um"
		case (PLURAL, GENITIVE)			=> "a"
	}
}
