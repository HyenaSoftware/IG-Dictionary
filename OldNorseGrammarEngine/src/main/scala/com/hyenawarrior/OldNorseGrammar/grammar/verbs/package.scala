package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{CalculatedAblaut, StaticAblaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.{PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{StrongVerbStem, VerbStemEnum}
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

	def stemToTense(stem: VerbStemEnum): (VerbTenseEnum, Option[GNumber]) = stem match	{

		case PRESENT_STEM							=> PRESENT	-> None
		case PRETERITE_SINGULAR_STEM	=> PAST			-> Some(SINGULAR)
		case PRETERITE_PLURAL_STEM		=> PAST			-> Some(PLURAL)
	}

	def verbFrom(text: String, verbClass: VerbClassEnum, verbType: VerbType): Option[Verb] = verbClass match
	{
		case svc: StrongVerbClassEnum => generateStrongVerbFromRawStr(text, svc, verbType)
		case _ => ???
	}

	def generateStrongVerbFromRawStr(rawStr: String, strongVerbClass: StrongVerbClassEnum, verbType: VerbType): Option[StrongVerb] = {

		def generateNonFinitiveStrongVerb(verbType: NonFinitiveVerbType) = {

			val verbStem = verbType.verbStemBase

			getDescOfStrongVerbClassFor(strongVerbClass, Map(verbStem -> Seq(rawStr))).map(NonFinitiveStrongVerb(rawStr, _, verbType))
		}

		verbType match
		{
			case (mood @ (INDICATIVE | SUBJUNCTIVE | IMPERATIVE), Some(tense), Some(pronoun)) => {

					val rawStrPerStem = Map(tenseToStem(tense, pronoun.number) -> Seq(rawStr))

					val optVerbClassDesc = getDescOfStrongVerbClassFor(strongVerbClass, rawStrPerStem)

					optVerbClassDesc.map(verbClassDesc => FinitiveStrongVerb(rawStr, verbClassDesc, pronoun, tense, mood.asInstanceOf[FinitiveMood]))
				}
			case (PARTICIPLE, Some(PRESENT),	None) => generateNonFinitiveStrongVerb(PRESENT_PARTICIPLE)

			case (PARTICIPLE, Some(PAST),			None) => generateNonFinitiveStrongVerb(PAST_PARTICIPLE)

			case (INFINITIVE, None,						None) => generateNonFinitiveStrongVerb(NonFinitiveVerbType.INFINITIVE)

			case _ => ???
		}
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
