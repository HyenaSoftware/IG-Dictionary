package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.Syllables
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem._

/**
	* Created by HyenaWarrior on 2017.04.25..
	*/
case class StaticAblaut(presentAblautGrade: AblautGrade, preteriteSingularAblautGrade: AblautGrade
												, preteritePluralAblautGrade: AblautGrade, perfectAblautGrade: AblautGrade)
{
	private val VOWELS = Map(
		PRESENT_STEM						-> presentAblautGrade,
		PRETERITE_SINGULAR_STEM	-> preteriteSingularAblautGrade,
		PRETERITE_PLURAL_STEM	 	-> preteritePluralAblautGrade,
		PERFECT_STEM						-> perfectAblautGrade
	)

	def grades(enumVerbStem: EnumVerbStem): AblautGrade = VOWELS(enumVerbStem)

	override def toString: String	= List(presentAblautGrade, preteriteSingularAblautGrade, preteritePluralAblautGrade, perfectAblautGrade)
		.mkString("(", " - ", ")")
}

object Ablaut
{
	def transform(str: String, srcAblaut: AblautGrade, dstAblaut: AblautGrade): Option[String] =
	{
		val where = str.indexOf(srcAblaut.rootVowel)

		if (where != -1)
		{

			val length = srcAblaut.rootVowel.length

			val prefixStr = str.substring(0, where)
			val suffixStr = str.substring(where + length)

			Some(s"$prefixStr${dstAblaut.rootVowel}$suffixStr")

		} else None
	}

	def getAblautGradeFrom(rawStr: String): Option[AblautGrade] =
	{
		val Syllables(syllables) = rawStr

		val firstSy = syllables.head

		val nucleus = firstSy.letters.filter(Syllables.isVowel)

		if(nucleus.nonEmpty) Some(AblautGrade(nucleus)) else None
	}

	def extractAblautFrom(verb: Map[EnumVerbStem, Seq[String]]): Option[StaticAblaut] =
	{
		val setPsSV		= verb.getOrElse(PRESENT_STEM, 						Seq()).flatMap(s => getAblautGradeFrom(s)).toSet
		val setPtSgSV	= verb.getOrElse(PRETERITE_SINGULAR_STEM, Seq()).flatMap(s => getAblautGradeFrom(s)).toSet
		val setPtPlSV	= verb.getOrElse(PRETERITE_PLURAL_STEM,		Seq()).flatMap(s => getAblautGradeFrom(s)).toSet
		val setPfSV		= verb.getOrElse(PERFECT_STEM,						Seq()).flatMap(s => getAblautGradeFrom(s)).toSet

		val psSV   = if(setPsSV.size == 1) setPsSV.headOption else None
		val ptSgSV = if(setPtSgSV.size == 1) setPtSgSV.headOption else None
		val ptPlSV = if(setPtPlSV.size == 1) setPtPlSV.headOption else None
		val pfSV   = if(setPfSV.size == 1) setPfSV.headOption else None

		// complete missing ablaut-grades
		val optFixedPsSv		= psSV		orElse pfSV 	orElse ptSgSV	orElse ptPlSV
		val optFixedPtSgSv	= ptSgSV	orElse ptPlSV orElse pfSV		orElse psSV
		val optFixedPtPlSv	= ptPlSV 	orElse ptSgSV orElse pfSV	 	orElse psSV
		val optFixedPfSv		= pfSV		orElse psSV 	orElse ptSgSV orElse ptPlSV

		(optFixedPsSv, optFixedPtSgSv, optFixedPtPlSv, optFixedPfSv) match
		{
			case (Some(fixedPsSv), Some(fixedPtSgSv), Some(fixedPtPlSv), Some(fixedPfSv)) =>
				Some(StaticAblaut(fixedPsSv, fixedPtSgSv, fixedPtPlSv, fixedPfSv))
			case _ =>  None
		}
	}
}