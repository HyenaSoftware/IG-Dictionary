package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Ablaut, StaticAblaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem._

/**
	* Created by HyenaWarrior on 2017.04.22..
	*
	* I. "gefa" is a string representation of a verbform
	* II. StrongFinitiveVerb(stem, classDesc, pronoun, tense) is an object representation of a verbform
	*
	* III. every string representation of a verb has an object representation and vica-versa
	* IV. set of representation of verbforms is a verb
	* V. a verb has many verbforms, which have string and object representations
	*
	*/
package object verbs
{
	type VerbType = (VerbModeEnum, Option[VerbTenseEnum], Option[Pronoun])

	def tenseToStem(tense: VerbTenseEnum, number: GNumber): EnumVerbStem = (tense, number) match	{

		case (PRESENT, _)			=> PRESENT_STEM
		case (PAST, SINGULAR) => PRETERITE_SINGULAR_STEM
		case (PAST, PLURAL)		=> PRETERITE_PLURAL_STEM
	}

	def stemFrom(optTense: Option[VerbTenseEnum], optNumber: Option[GNumber], mood: VerbModeEnum): EnumVerbStem
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

	def stemToTense(stem: EnumVerbStem): (VerbTenseEnum, Option[GNumber]) = stem match	{

		case PRESENT_STEM							=> PRESENT	-> None
		case PRETERITE_SINGULAR_STEM	=> PAST			-> Some(SINGULAR)
		case PRETERITE_PLURAL_STEM		=> PAST			-> Some(PLURAL)
	}

	def getDescOfStrongVerbClassFor(verbClass: StrongVerbClassEnum, verbForms: Map[EnumVerbStem, Seq[String]] = Map()): Option[StrongVerbClassDesc]
		= verbClass match
	{
		case STRONG_1ST_CLASS  => Some(StrongVerbClassDesc(STRONG_1ST_CLASS, StaticAblaut("í",	"ei", "i", "i")))
		case STRONG_2ND_CLASS  => Some(StrongVerbClassDesc(STRONG_2ND_CLASS, StaticAblaut("jú", "au", "u", "o")))
		case STRONG_3RD_CLASS  => Some(StrongVerbClassDesc(STRONG_3RD_CLASS, StaticAblaut("e", 	"a", 	"u", "o")))

		case STRONG_4TH_CLASS  => Some(StrongVerbClassDesc(STRONG_4TH_CLASS, StaticAblaut("e",	"a", 	"á", "o")))
		case STRONG_5TH_CLASS  => Some(StrongVerbClassDesc(STRONG_5TH_CLASS, StaticAblaut("e", 	"a", 	"á", "e")))
		case STRONG_6TH_CLASS  => Some(StrongVerbClassDesc(STRONG_6TH_CLASS, StaticAblaut("a", 	"ó", 	"ó", "a")))

		case STRONG_7TH_CLASS => Ablaut.extractAblautFrom(verbForms).map(a => StrongVerbClassDesc(STRONG_7TH_CLASS, a))
	}
}
