package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{AblautGrade, I_Umlaut, StaticAblaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum.{apply => _}
import com.hyenawarrior.OldNorseGrammar.grammar.{Pronoun, Root, Syllables}

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
object StrongVerbGenerator
{
	/*
			stem -> verb
	 */
	def verbFrom(stem: StrongVerbStem, verbClass: StrongVerbClassDesc, pronoun: Pronoun, tense: VerbTenseEnum, mood: FinitiveMood): StrongVerb =
	{
		val str = stem.stringForm + StrongVerb.stemEnding(pronoun, tense)

		FinitiveStrongVerb(str, verbClass, pronoun, tense, mood)
	}

	/*
				[Form]										[base stem]
				Infinitive								Present Stem
				Present Participle				Present Stem
				Past/Perfect Participle		Perfect Stem
				Supine										Perfect Stem
		 */
	def verbFrom(stem: StrongVerbStem, verbClass: StrongVerbClassDesc, nonFinitiveForm: NonFinitiveVerbType): StrongVerb = nonFinitiveForm match
	{
		case INFINITIVE					=>	infinitiveVerbFrom(stem, verbClass)

		case PRESENT_PARTICIPLE =>	particibleVerbFrom(stem, verbClass, VerbTenseEnum.PRESENT)
		case PAST_PARTICIPLE 		=>	particibleVerbFrom(stem, verbClass, VerbTenseEnum.PAST)
	}

	def infinitiveVerbFrom(stem: StrongVerbStem, verbClass: StrongVerbClassDesc): StrongVerb =
	{
		val lastChar = stem.stringForm.last
		val newStr = lastChar match
		{
			case 'á' => stem.stringForm
			case _ => stem.stringForm :+ 'a'
		}

		NonFinitiveStrongVerb(newStr, verbClass, NonFinitiveVerbType.INFINITIVE)
	}

	def particibleVerbFrom(stem: StrongVerbStem, verbClass: StrongVerbClassDesc, verbTense: VerbTenseEnum): StrongVerb = verbTense match
	{
		case VerbTenseEnum.PAST			=> NonFinitiveStrongVerb(stem.stringForm + "inn",	verbClass, NonFinitiveVerbType.PAST_PARTICIPLE)
		case VerbTenseEnum.PRESENT	=> NonFinitiveStrongVerb(stem.stringForm + "andi",verbClass, NonFinitiveVerbType.PRESENT_PARTICIPLE)
	}

	/*
			verb -> stem
	 */
	def stemFrom(verb: StrongVerb, ablaut: StaticAblaut): StrongVerbStem = verb match
	{
		case fVerb: FinitiveStrongVerb => stemFromFinitive(fVerb, ablaut)
		case nVerb: NonFinitiveStrongVerb => null
	}

	def getAblautGradeFrom(verb: StrongVerb): AblautGrade =
	{
		getAblautGradeFrom(verb.rawForm)
	}

	def getAblautGradeFrom(rawForm: String): AblautGrade =
	{
		val Syllables(syllables) = rawForm

		val firstSy = syllables.head

		val nucleus = firstSy.letters.filter(Syllables.isVowel)

		AblautGrade(nucleus)
	}

	def stemFromFinitive(verb: FinitiveStrongVerb, ablaut: StaticAblaut): StrongVerbStem = {

		val gnum = verb.pronoun.number
		val stemType = tenseToStem(verb.tense, gnum)
		val matchAblautGrade = ablaut.grades()(stemType).occuresIn(verb.strForm)

		val strWithoutIUmlaut = (gnum, verb.tense) match
		{
			case (SINGULAR, PRESENT) if !matchAblautGrade => I_Umlaut.unapply(verb.strForm)
			case _ => verb.strForm
		}

		val stemEnding = StrongVerb.stemEnding(verb.pronoun, verb.tense)
		val strWithoutInflection = strWithoutIUmlaut.stripSuffix(stemEnding)

		val root = Root(strWithoutInflection)

		StrongVerbStem(root, stemType)
	}
}
