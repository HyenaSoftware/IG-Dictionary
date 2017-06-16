package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.{PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.{INDICATIVE, INFINITIVE, PARTICIPLE, SUBJUNCTIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum._

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

	def verbFrom(text: String, mode: VerbModeEnum, verbClass: VerbClassEnum, optTense: Option[VerbTenseEnum], optPronoun: Option[Pronoun]): Verb
		= (mode, optTense, optPronoun) match
	{
		case (INDICATIVE | SUBJUNCTIVE, Some(tense), Some(pronoun)) => FinitiveStrongVerb(text, verbClass, pronoun, tense)
		case (PARTICIPLE, Some(PRESENT),	None) => NonFinitiveStrongVerb(text, verbClass, PRESENT_PARTICIPLE)
		case (PARTICIPLE, Some(PAST),			None) => NonFinitiveStrongVerb(text, verbClass, PAST_PARTICIPLE)
		case (INFINITIVE, None,						None) => NonFinitiveStrongVerb(text, verbClass, NonFinitiveVerbType.INFINITIVE)
		case _ => ???
	}
}
