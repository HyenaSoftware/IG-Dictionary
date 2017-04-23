package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Ablaut, AblautTransformation}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{StrongVerbStem, VerbStemEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbClassEnum, VerbTenseEnum, _}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
object StrongVerbStemClasses extends VerbStemClass
{
	val STEMCLASSES_TO_ABLAUT: Map[(VerbStemEnum, VerbClassEnum), Ablaut] = Map(

		(PRESENT_STEM,						STRONG_1ST_CLASS) -> Ablaut("í"),
		(PRETERITE_SINGULAR_STEM,	STRONG_1ST_CLASS) -> Ablaut("ei"),
		(PRETERITE_PLURAL_STEM,		STRONG_1ST_CLASS) -> Ablaut("i"),
		(PERFECT_STEM,						STRONG_1ST_CLASS) -> Ablaut("i"),

		(PRESENT_STEM,						STRONG_3RD_CLASS) -> Ablaut("e"),
		(PRETERITE_SINGULAR_STEM,	STRONG_3RD_CLASS) -> Ablaut("a"),
		(PRETERITE_PLURAL_STEM,		STRONG_3RD_CLASS) -> Ablaut("u"),
		(PERFECT_STEM,						STRONG_3RD_CLASS) -> Ablaut("o")
	)

	override def convertTo(verb: StrongVerb, targetForm: Either[FinitiveVerbDesc, NonFinitiveVerbType]): Option[StrongVerb] = {

		targetForm match {

			case Left(fin) => generateFinitiveForm(StrongVerbGenerator.stemFrom(verb), verb.verbClass, fin)
			case Right(nfvt) => throw new NotImplementedError("not yet")
		}
	}

	private def generateFinitiveForm(srcStem: StrongVerbStem, currentClass: VerbClassEnum, finTrg: FinitiveVerbDesc): Option[StrongVerb] =
	{
		val (pron, verbModeEnum, verbTense) = finTrg

		val destVerbStemType = tenseToStem(verbTense, pron.number)
		val optDstVerbStem = changeStem(srcStem, currentClass, destVerbStemType)

		optDstVerbStem.map(stem => StrongVerbGenerator.verbFrom(stem, currentClass, pron, verbTense))
	}

	private def changeStem(srcStem: StrongVerbStem, currentClass: VerbClassEnum, dstVerbStemType: VerbStemEnum): Option[StrongVerbStem] = {

		if(!STEMCLASSES_TO_ABLAUT.contains(srcStem.stemType, currentClass) ||
			!STEMCLASSES_TO_ABLAUT.contains(dstVerbStemType, currentClass))
		{
			return None
		}

		val srcAblaut = STEMCLASSES_TO_ABLAUT(srcStem.stemType, currentClass)
		val dstAblaut = STEMCLASSES_TO_ABLAUT(dstVerbStemType, currentClass)
		val newStr = AblautTransformation(srcStem.stringForm, srcAblaut, dstAblaut)

		newStr.map(str => StrongVerbStem(Root(str), dstVerbStemType))
	}
}









