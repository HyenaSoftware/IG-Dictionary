package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.{GNumber, Pronoun}
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Explicit_I_Umlaut, U_Umlaut, WordTransformation}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.FinitiveStrongVerb.tenseAndNumberToStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
abstract class StrongVerb(str: String, val verbClassDesc: StrongVerbClassDesc, val stemType: VerbStemEnum) extends Verb(str)

case class FinitiveStrongVerb(str: String, override val verbClassDesc: StrongVerbClassDesc, pronoun: Pronoun, tense: VerbTenseEnum, mood: FinitiveMood)
	extends StrongVerb(str, verbClassDesc, tenseAndNumberToStem(tense, pronoun.number))
{
	override def transformations: List[WordTransformation] = (pronoun.number, tense) match
	{
		case (SINGULAR, PRESENT) => List(Explicit_I_Umlaut)
		case _ => List(U_Umlaut)
	}
}

object FinitiveStrongVerb
{
	def tenseAndNumberToStem(tense: VerbTenseEnum, number: GNumber): VerbStemEnum = (tense, number) match
	{
		case (PRESENT,	_) 				=> VerbStemEnum.PRESENT_STEM
		case (PAST, 		SINGULAR) => VerbStemEnum.PRETERITE_SINGULAR_STEM
		case (PAST, 		PLURAL)		=> VerbStemEnum.PRETERITE_SINGULAR_STEM
	}
}

case class NonFinitiveStrongVerb(str: String, override val verbClassDesc: StrongVerbClassDesc, nonFinitiveVerbType: NonFinitiveVerbType)
	extends StrongVerb(str, verbClassDesc, nonFinitiveVerbType.verbStemBase)
{
	override def transformations: List[WordTransformation] = nonFinitiveVerbType match
	{
		case NonFinitiveVerbType.PRESENT_PARTICIPLE => List()
		case _ => List()
	}
}

object StrongVerb
{
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
