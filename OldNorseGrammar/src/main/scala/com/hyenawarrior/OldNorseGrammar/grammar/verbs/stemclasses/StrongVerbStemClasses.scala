package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{AblautGrade, AblautTransformation}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{StrongVerbStem, VerbStemEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbClassEnum, _}

import scala.Option.option2Iterable

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
object StrongVerbStemClasses extends VerbStemClass
{
	val STEMCLASSES_TO_ABLAUT: Map[(VerbStemEnum, VerbClassEnum), AblautGrade] =
		Map(
			(PRESENT_STEM,						STRONG_1ST_CLASS) -> AblautGrade("í"),
			(PRETERITE_SINGULAR_STEM,	STRONG_1ST_CLASS) -> AblautGrade("ei"),
			(PRETERITE_PLURAL_STEM,		STRONG_1ST_CLASS) -> AblautGrade("i"),
			(PERFECT_STEM,						STRONG_1ST_CLASS) -> AblautGrade("i"),

			(PRESENT_STEM,						STRONG_3RD_CLASS) -> AblautGrade("e"),
			(PRETERITE_SINGULAR_STEM,	STRONG_3RD_CLASS) -> AblautGrade("a"),
			(PRETERITE_PLURAL_STEM,		STRONG_3RD_CLASS) -> AblautGrade("u"),
			(PERFECT_STEM,						STRONG_3RD_CLASS) -> AblautGrade("o")
		) ++
		genAblautFor(STRONG_2ND_CLASS, "jú", "au", "u", "o") ++
		genAblautFor(STRONG_4TH_CLASS, "e", "a", "á", "o") ++
		genAblautFor(STRONG_5TH_CLASS, "e", "a", "á", "e") ++
		genAblautFor(STRONG_6TH_CLASS, "a", "ó", "ó", "a")

	private def genAblautFor(vc: VerbClassEnum, vowels: String*): Map[(VerbStemEnum, VerbClassEnum), AblautGrade] =
		List(PRESENT_STEM, PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM, PERFECT_STEM)
		  .map(s => s -> vc)
		  .zipWithIndex
		  .map{ case (k, i) => k -> AblautGrade(vowels(i)) }
		  .toMap


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
			//case (Some(srcAblaut), Some(dstAblaut)) if srcAblaut == dstAblaut && srcAblaut. => Some(srcStem)

			case (Some(srcAblaut), Some(dstAblaut)) =>
				val newStr = AblautTransformation(srcStem.stringForm, srcAblaut, dstAblaut)
				newStr.map(str => StrongVerbStem(Root(str), dstVerbStemType))

			case _ => None
		}
	}

	def generateNonFinitiveForms(stem: StrongVerbStem, verbClass: VerbClassEnum, nonFinitiveForm: NonFinitiveVerbType): Option[StrongVerb] = {

		/*
				[Form]										[base stem]
				Infinitive								Present Stem
				Present Participle				Present Stem
				Past/Perfect Participle		Perfect Stem
				Supine										Perfect Stem
		 */

		val requiredStem = nonFinitiveForm match {

			case INFINITIVE | PRESENT_PARTICIPLE 	=> PRESENT_STEM
			case PAST_PARTICIPLE 									=> PERFECT_STEM
		}

		changeStem(stem, verbClass, requiredStem)
			.map(stem => StrongVerbGenerator.verbFrom(stem, verbClass, nonFinitiveForm))
	}
}









