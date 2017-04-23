package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
package object verbs
{
	def tenseToStem(tense: VerbTenseEnum, number: GNumber): VerbStemEnum = (tense, number) match	{

		case (PRESENT, _)			=> PRESENT_STEM
		case (PAST, SINGULAR) => PRETERITE_SINGULAR_STEM
		case (PAST, PLURAL)		=> PRETERITE_PLURAL_STEM
		case (PERFECT, _)			=> PERFECT_STEM
	}

	def stemToTense(stem: VerbStemEnum): (VerbTenseEnum, Option[GNumber]) = stem match	{

		case PRESENT_STEM							=> PRESENT	-> None
		case PRETERITE_SINGULAR_STEM	=> PAST			-> Some(SINGULAR)
		case PRETERITE_PLURAL_STEM		=> PAST			-> Some(PLURAL)
		case PERFECT_STEM							=> PERFECT	-> None
	}
}
