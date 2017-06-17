package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.I_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum.{apply => _}
import com.hyenawarrior.OldNorseGrammar.grammar.{Pronoun, Root}

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
object StrongVerbGenerator
{
	/*
			stem -> verb
	 */
	def verbFrom(stem: StrongVerbStem, verbClass: VerbClassEnum, pronoun: Pronoun, tense: VerbTenseEnum): StrongVerb = {

		val str = stem.stringForm + StrongVerb.stemEnding(pronoun, tense)

		FinitiveStrongVerb(str, verbClass, pronoun, tense)
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

			case INFINITIVE =>
				val lastChar = stem.stringForm.last
				val newStr = lastChar match
				{
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
	def stemFrom(verb: StrongVerb): StrongVerbStem = verb match
	{
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

		val stemEnding = StrongVerb.stemEnding(verb.pronoun, verb.tense)
		val strWithoutInflection = strWithoutIUmlaut.stripSuffix(stemEnding)

		val root = Root(strWithoutInflection)

		StrongVerbStem(root, stemType)
	}
}
