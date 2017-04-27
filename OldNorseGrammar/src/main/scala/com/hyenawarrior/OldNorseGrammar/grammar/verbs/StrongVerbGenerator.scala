package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.I_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum.{apply => _}
import com.hyenawarrior.OldNorseGrammar.grammar.{GNumber, Pronoun, Root}

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
object StrongVerbGenerator
{
	/*
			stem -> verb
	 */
	def verbFrom(stem: StrongVerbStem, verbClass: VerbClassEnum, pronoun: Pronoun, tense: VerbTenseEnum): StrongVerb = {

		val str = stem.stringForm + stemEnding(pronoun, tense)

		val strUmlauted = (pronoun.number, tense) match
		{
			case (SINGULAR, PRESENT) => I_Umlaut.forceApply(str)
			case _ => str
		}

		FinitiveStrongVerb(strUmlauted, verbClass, pronoun, tense)
	}

	def verbFrom(stem: StrongVerbStem, verbClass: VerbClassEnum, nonFinitiveForm: NonFinitiveVerbType): StrongVerb = {

		/*
				[Form]										[base stem]
				Infinitive								Present Stem
				Present Participle				Present Stem
				Past/Perfect Participle		Perfect Stem
				Supine										Perfect Stem
		 */

		nonFinitiveForm match	{

			case INFINITIVE=>
				val lastChar = stem.stringForm.last
				val newStr = lastChar match {

					case 'á' => stem.stringForm
					case _ => stem.stringForm :+ 'a'
				}

				NonFinitiveStrongVerb(newStr, verbClass, nonFinitiveForm)

			case PRESENT_PARTICIPLE =>	NonFinitiveStrongVerb(stem.stringForm + "andi", verbClass, nonFinitiveForm)
			case PAST_PARTICIPLE =>			NonFinitiveStrongVerb(stem.stringForm + "inn",	verbClass, nonFinitiveForm)
		}
	}

	/*
			verb -> stem
	 */
	def stemFrom(verb: StrongVerb): StrongVerbStem = verb match {

		case fVerb: FinitiveStrongVerb => stemFromFinitive(fVerb)
		case nVerb: NonFinitiveStrongVerb => null
	}

	def stemFromFinitive(verb: FinitiveStrongVerb) = {

		val gnum = verb.pronoun.number
		val stemType = tenseToStem(verb.tense, gnum)
		val hasAblautGrade = Option(verb.verbClass.ablaut).exists(a => a.VOWELS(stemType).occuresIn(verb.strForm))

		val strWithoutIUmlaut = (gnum, verb.tense) match
		{
			case (SINGULAR, PRESENT) if !hasAblautGrade => I_Umlaut.unapply(verb.strForm)
			case _ => verb.strForm
		}

		val stemEnding = StrongVerbGenerator.stemEnding(verb.pronoun, verb.tense)
		val strWithoutInflection = strWithoutIUmlaut.stripSuffix(stemEnding)

		val root = Root(strWithoutInflection)

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
