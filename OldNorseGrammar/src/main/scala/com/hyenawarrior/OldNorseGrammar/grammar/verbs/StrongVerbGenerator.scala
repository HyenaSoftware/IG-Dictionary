package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.I_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.{GNumber, Pronoun, Root}

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
object StrongVerbGenerator
{
	def verbFrom(stem: StrongVerbStem, verbClass: VerbClassEnum, pronoun: Pronoun, tense: VerbTenseEnum): StrongVerb = {

		val str = stem.stringForm + stemEnding(pronoun, tense)

		StrongVerb(str, verbClass, pronoun, tense)
	}

	def stemFrom(verb: StrongVerb) = {

		val strWithoutIUmlaut = verb.pronoun.number match
		{
			case GNumber.SINGULAR => I_Umlaut.unapply(verb.strForm)
			case _ => verb.strForm
		}

		val stemEnding = StrongVerbGenerator.stemEnding(verb.pronoun, verb.tense)
		val strWithoutInflection = strWithoutIUmlaut.stripSuffix(stemEnding)

		val root = Root(strWithoutInflection)
		val stemType = tenseToStem(verb.tense, verb.pronoun.number)

		StrongVerbStem(root, stemType)
	}

	def stemEnding(pronoun: Pronoun, tense: VerbTenseEnum) = tense match
	{
		case PRESENT => stemEndingForPresent(pronoun)
		case PAST => stemEndingForPreterite(pronoun)
	}

	private def stemEndingForPresent(pronoun: Pronoun) = pronoun match
	{
		case SG_1 => ""
		case SG_2 | SG_3_FEMN | SG_3_MASC | SG_3_NEUT => "r"
		case PL_1 | DL_1 => "um"
		case PL_2 | DL_2 => "ið"
		case PL_3_FEMN | PL_3_MASC | PL_3_NEUT => "a"
	}

	private def stemEndingForPreterite(pronoun: Pronoun) = pronoun match
	{
		case SG_1 | SG_3_FEMN | SG_3_MASC | SG_3_NEUT => ""
		case SG_2 => "t"
		case PL_1 | DL_1 => "um"
		case PL_2 | DL_2 => "uð"
		case PL_3_FEMN | PL_3_MASC | PL_3_NEUT => "u"
	}
}
