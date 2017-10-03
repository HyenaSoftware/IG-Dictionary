package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{CalculatedAblaut, StaticAblaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.{PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum._

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
package object verbs
{
	type VerbType = (VerbModeEnum, Option[VerbTenseEnum], Option[Pronoun])

	def tenseToStem(tense: VerbTenseEnum, number: GNumber): VerbStemEnum = (tense, number) match	{

		case (PRESENT, _)			=> PRESENT_STEM
		case (PAST, SINGULAR) => PRETERITE_SINGULAR_STEM
		case (PAST, PLURAL)		=> PRETERITE_PLURAL_STEM
	}

	def stemFrom(optTense: Option[VerbTenseEnum], optNumber: Option[GNumber], mood: VerbModeEnum): VerbStemEnum
		= (optTense, optNumber, mood) match {

		case (None,						None,		INFINITIVE) => PRESENT_STEM
		case (Some(PRESENT), 	Some(_),				INDICATIVE | SUBJUNCTIVE) => PRESENT_STEM
		case (Some(PAST), 		Some(SINGULAR),	INDICATIVE | SUBJUNCTIVE) => PRETERITE_SINGULAR_STEM
		case (Some(PAST), 		Some(PLURAL),		INDICATIVE | SUBJUNCTIVE) => PRETERITE_PLURAL_STEM
		case (Some(PRESENT), 	None,		PARTICIPLE) => PRESENT_STEM
		case (Some(PAST), 		None,		PARTICIPLE) => PERFECT_STEM
		case (_,							_,			IMPERATIVE) => PRESENT_STEM
		case _ => ???
	}

	def stemToTense(stem: VerbStemEnum): (VerbTenseEnum, Option[GNumber]) = stem match	{

		case PRESENT_STEM							=> PRESENT	-> None
		case PRETERITE_SINGULAR_STEM	=> PAST			-> Some(SINGULAR)
		case PRETERITE_PLURAL_STEM		=> PAST			-> Some(PLURAL)
	}

	def verbFrom(rawStr: String, verbClassDesc: VerbClassDesc, verbType: VerbType): Verb = verbClassDesc match
	{
		case svd: StrongVerbClassDesc => generateStrongVerbFromRawStr(rawStr, svd, verbType)
		case _ => ???
	}

	def generateStrongVerbFromRawStr(rawStr: String, verbClassDesc: StrongVerbClassDesc, verbType: VerbType): StrongVerb = verbType match
	{
		case (mood @ (INDICATIVE | SUBJUNCTIVE | IMPERATIVE), Some(tense), Some(pronoun)) =>
			FinitiveStrongVerb(rawStr, verbClassDesc, pronoun, tense, mood.asInstanceOf[FinitiveMood])

		case (PARTICIPLE, Some(PRESENT),	None) => NonFinitiveStrongVerb(rawStr, verbClassDesc, PRESENT_PARTICIPLE)

		case (PARTICIPLE, Some(PAST),			None) => NonFinitiveStrongVerb(rawStr, verbClassDesc, PAST_PARTICIPLE)

		case (INFINITIVE, None,						None) => NonFinitiveStrongVerb(rawStr, verbClassDesc, NonFinitiveVerbType.INFINITIVE)

		case _ => ???
	}

	def getDescOfStrongVerbClassFor(verbClass: StrongVerbClassEnum, verbForms: Map[VerbStemEnum, Seq[String]] = Map()): Option[StrongVerbClassDesc]
		= verbClass match
	{
		case STRONG_1ST_CLASS  => Some(StrongVerbClassDesc(STRONG_1ST_CLASS, StaticAblaut("í",	"ei", "i", "i")))
		case STRONG_2ND_CLASS  => Some(StrongVerbClassDesc(STRONG_2ND_CLASS, StaticAblaut("jú", "au", "u", "o")))
		case STRONG_3RD_CLASS  => Some(StrongVerbClassDesc(STRONG_3RD_CLASS, StaticAblaut("e", 	"a", 	"u", "o")))

		case STRONG_4TH_CLASS  => Some(StrongVerbClassDesc(STRONG_4TH_CLASS, StaticAblaut("e",	"a", 	"á", "o")))
		case STRONG_5TH_CLASS  => Some(StrongVerbClassDesc(STRONG_5TH_CLASS, StaticAblaut("e", 	"a", 	"á", "e")))
		case STRONG_6TH_CLASS  => Some(StrongVerbClassDesc(STRONG_6TH_CLASS, StaticAblaut("a", 	"ó", 	"ó", "a")))

		case STRONG_7TH_CLASS => CalculatedAblaut.extractAblautFrom(verbForms).map(a => StrongVerbClassDesc(STRONG_7TH_CLASS, a))
	}
}
