package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, StrongVerbGenerator}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum._

/**
	* Created by HyenaWarrior on 2017.04.25..
	*/
trait Ablaut

case class StaticAblaut(presentAblautGrade: AblautGrade, preteriteSingularAblautGrade: AblautGrade
												, preteritePluralAblautGrade: AblautGrade, perfectAblautGrade: AblautGrade) extends Ablaut
{
	private val VOWELS = Map(
		PRESENT_STEM						-> presentAblautGrade,
		PRETERITE_SINGULAR_STEM	-> preteriteSingularAblautGrade,
		PRETERITE_PLURAL_STEM	 	-> preteritePluralAblautGrade,
		PERFECT_STEM						-> perfectAblautGrade
	)

	def grades(): Map[VerbStemEnum, AblautGrade] = VOWELS

	override def toString: String	= List(presentAblautGrade, preteriteSingularAblautGrade, preteritePluralAblautGrade, perfectAblautGrade)
		.mkString("(", " - ", ")")
}

object CalculatedAblaut extends Ablaut
{
	/*def extractAblautFrom(verbs: Seq[StrongVerb]): Option[StaticAblaut] =
	{
		verbs.map(v => v.verbClassDesc. v.verbClassDesc.ablaut)

		val map: Map[VerbStemEnum, Seq[String]] = verb
			  .map(v => v.stemType -> v.rawForm)
				.groupBy(_._1)
		  	.map{case(k, v) => k -> v.map(w => w._2)}

		extractAblautFrom(map)
	}*/

	def extractAblautFrom(verb: Map[VerbStemEnum, Seq[String]]): Option[StaticAblaut] =
	{
		val psSV		= verb(PRESENT_STEM).headOption
		val ptSgSV	= verb(PRETERITE_SINGULAR_STEM).headOption
		val ptPlSV	= verb(PRETERITE_PLURAL_STEM).headOption
		val pfSV		= verb(PERFECT_STEM).headOption

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