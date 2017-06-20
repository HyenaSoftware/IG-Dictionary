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
	def verbFrom(stem: StrongVerbStem, verbClass: VerbClassEnum, pronoun: Pronoun, tense: VerbTenseEnum): StrongVerb =
	{
		val str = stem.stringForm + StrongVerb.stemEnding(pronoun, tense)

		FinitiveStrongVerb(str, verbClass, pronoun, tense)
	}

	/*
				[Form]										[base stem]
				Infinitive								Present Stem
				Present Participle				Present Stem
				Past/Perfect Participle		Perfect Stem
				Supine										Perfect Stem
		 */
	def verbFrom(stem: StrongVerbStem, verbClass: VerbClassEnum, nonFinitiveForm: NonFinitiveVerbType): StrongVerb = nonFinitiveForm match
	{
		case INFINITIVE					=>	infinitiveVerbFrom(stem, verbClass)

		case PRESENT_PARTICIPLE =>	particibleVerbFrom(stem, verbClass, VerbTenseEnum.PRESENT)
		case PAST_PARTICIPLE 		=>	particibleVerbFrom(stem, verbClass, VerbTenseEnum.PAST)
	}

	def infinitiveVerbFrom(stem: StrongVerbStem, verbClass: VerbClassEnum): StrongVerb =
	{
		val lastChar = stem.stringForm.last
		val newStr = lastChar match
		{
			case 'á' => stem.stringForm
			case _ => stem.stringForm :+ 'a'
		}

		NonFinitiveStrongVerb(newStr, verbClass, NonFinitiveVerbType.INFINITIVE)
	}

	def particibleVerbFrom(stem: StrongVerbStem, verbClass: VerbClassEnum, verbTense: VerbTenseEnum): StrongVerb = verbTense match
	{
		case VerbTenseEnum.PAST			=> NonFinitiveStrongVerb(stem.stringForm + "inn",	verbClass, NonFinitiveVerbType.PAST_PARTICIPLE)
		case VerbTenseEnum.PRESENT	=> NonFinitiveStrongVerb(stem.stringForm + "andi",verbClass, NonFinitiveVerbType.PRESENT_PARTICIPLE)
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
