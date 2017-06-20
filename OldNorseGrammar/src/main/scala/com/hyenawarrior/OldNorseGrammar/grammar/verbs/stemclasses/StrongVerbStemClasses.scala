package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautTransformation
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{StrongVerbStem, VerbStemEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.VerbStemClass.FinitiveVerbDesc
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbClassEnum, _}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
object StrongVerbStemClasses extends VerbStemClass[StrongVerb]
{
	override def convertTo(verb: StrongVerb, targetForm: Either[FinitiveVerbDesc, NonFinitiveVerbType]): Option[StrongVerb] = {

		val stem = StrongVerbGenerator.stemFrom(verb)

		targetForm match {

			case Left(fin) => generateFinitiveForm(stem, verb.verbClass, fin)
			case Right(nfvt) => generateNonFinitiveForms(stem, verb.verbClass, nfvt)
		}
	}

	private def generateFinitiveForm(srcStem: StrongVerbStem, currentClass: VerbClassEnum, finTrg: FinitiveVerbDesc): Option[StrongVerb] = {
		val (pron, verbModeEnum, verbTense) = finTrg

		val destVerbStemType = tenseToStem(verbTense, pron.number)
		val optDstVerbStem = changeStem(srcStem, currentClass, destVerbStemType)

		optDstVerbStem.map(stem => StrongVerbGenerator.verbFrom(stem, currentClass, pron, verbTense))
	}

	def changeStem(srcStem: StrongVerbStem, currentClass: VerbClassEnum, dstVerbStemType: VerbStemEnum): Option[StrongVerbStem] = {

		val CURRENT_ABLAUT = Option(currentClass.ablaut).map(_.VOWELS)

		val optSrcAblaut = CURRENT_ABLAUT.flatMap(_.get(srcStem.stemType))
		val optDstAblaut = CURRENT_ABLAUT.flatMap(_.get(dstVerbStemType))

		(optSrcAblaut, optDstAblaut) match
		{
			case (Some(srcAblaut), Some(dstAblaut)) =>
				// change the strong verb stem
				val newStr = AblautTransformation(srcStem.stringForm, srcAblaut, dstAblaut)
				newStr.map(str => StrongVerbStem(Root(str), dstVerbStemType))

			case _ => None
		}
	}

	def generateNonFinitiveForms(stem: StrongVerbStem, verbClass: VerbClassEnum, nonFinitiveForm: NonFinitiveVerbType): Option[StrongVerb] =
	{
		/*
				[Form]										[base stem]
				Infinitive								Present Stem
				Present Participle				Present Stem
				Past/Perfect Participle		Perfect Stem
				Supine										Perfect Stem
		 */

		val requiredStem = nonFinitiveForm match
		{
			case INFINITIVE | PRESENT_PARTICIPLE 	=> PRESENT_STEM
			case PAST_PARTICIPLE 									=> PERFECT_STEM
		}

		changeStem(stem, verbClass, requiredStem)
			.map(stem => StrongVerbGenerator.verbFrom(stem, verbClass, nonFinitiveForm))
	}
}









