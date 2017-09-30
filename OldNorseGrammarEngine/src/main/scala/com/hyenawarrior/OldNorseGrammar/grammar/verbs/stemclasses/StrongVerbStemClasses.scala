package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{StrongVerbStem, VerbStemEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.VerbStemClass.FinitiveVerbDesc
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbClassEnum, _}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
object StrongVerbStemClasses //extends VerbStemClass[StrongVerb]
{
	def convertTo(verb: StrongVerb, ablaut: StaticAblaut, fin: FinitiveVerbDesc): Option[StrongVerb] =
	{
		val stem = StrongVerbGenerator.stemFrom(verb, ablaut)

		generateFinitiveForm(stem, verb.verbClassDesc, ablaut, fin)
	}

	def convertTo(verb: StrongVerb, ablaut: StaticAblaut, nfvt: NonFinitiveVerbType): Option[StrongVerb] =
	{
		val stem = StrongVerbGenerator.stemFrom(verb, ablaut)

		val verbClass = verb.verbClassDesc
		val requiredStem = nfvt.verbStemBase

		changeStem(stem, ablaut, requiredStem).map(stem => StrongVerbGenerator.verbFrom(stem, verbClass, nfvt))
	}

	private def generateFinitiveForm(srcStem: StrongVerbStem, currentClass: StrongVerbClassDesc, ablaut: StaticAblaut, finTrg: FinitiveVerbDesc): Option[StrongVerb] =
	{
		val (pron, mood, verbTense) = finTrg

		val destVerbStemType = tenseToStem(verbTense, pron.number)
		val optDstVerbStem = changeStem(srcStem, ablaut, destVerbStemType)

		optDstVerbStem.map(stem => StrongVerbGenerator.verbFrom(stem, currentClass, pron, verbTense, mood))
	}

	private def changeStem(srcStem: StrongVerbStem, ablaut: StaticAblaut, dstVerbStemType: VerbStemEnum): Option[StrongVerbStem] =
	{
		val srcAblaut = ablaut.grades()(srcStem.stemType)
		val dstAblaut = ablaut.grades()(dstVerbStemType)

		// change the strong verb stem
		val newStr = AblautTransformation(srcStem.stringForm, srcAblaut, dstAblaut)
		newStr.map(str => StrongVerbStem(Root(str), dstVerbStemType))
	}
}









