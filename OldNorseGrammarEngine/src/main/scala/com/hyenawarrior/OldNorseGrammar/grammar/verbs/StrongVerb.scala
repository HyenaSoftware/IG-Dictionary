package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Explicit_I_Umlaut, I_Umlaut, U_Umlaut, WordTransformation}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.{PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{CommonStrongVerbStem, EnumVerbStem, StrongVerbStem}
import com.hyenawarrior.OldNorseGrammar.grammar.{GNumber, Pronoun}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
abstract class StrongVerb(strRepr: String, stem: CommonStrongVerbStem) extends Verb(strRepr) {

	def getStem(): CommonStrongVerbStem = stem
}

case class FinitiveStrongVerb(strRepr: String, stem: CommonStrongVerbStem, pronoun: Pronoun
	, tense: VerbTenseEnum, mood: FinitiveMood)	extends StrongVerb(strRepr, stem) {

	override def transformations: List[WordTransformation] = (pronoun.number, tense) match {

		case (SINGULAR, PRESENT) => List(Explicit_I_Umlaut)
		case _ => List(U_Umlaut)
	}
}

object FinitiveStrongVerb {

	def tenseAndNumberToStem(tense: VerbTenseEnum, number: GNumber): EnumVerbStem = (tense, number) match {

		case (PRESENT,	_) 				=> EnumVerbStem.PRESENT_STEM
		case (PAST, 		SINGULAR) => EnumVerbStem.PRETERITE_SINGULAR_STEM
		case (PAST, 		PLURAL)		=> EnumVerbStem.PRETERITE_SINGULAR_STEM
	}
}

case class NonFinitiveStrongVerb(strRepr: String, stem: CommonStrongVerbStem, nonFinitiveVerbType: NonFinitiveVerbType)
	extends StrongVerb(strRepr, stem) {

	if(nonFinitiveVerbType.verbStemBase != stem.getStemType()) {

		throw new RuntimeException
	}

	override def transformations: List[WordTransformation] = nonFinitiveVerbType match {

		case PRESENT_PARTICIPLE => List()
		case _ => List()
	}
}

/**
	* Meta-class of object representation
	*/
object StrongVerb {

	def unapply(sv: StrongVerb): Option[CommonStrongVerbStem] = sv match {

		case FinitiveStrongVerb(_, stem, _, _, _)	=> Some(stem)
		case NonFinitiveStrongVerb(_, stem, _) 		=> Some(stem)
	}

	/**
		* It converts a string representation to an object representation
		*
		* Use it for:
		* 	- to figure out what is the stem of the verb
		*
		* Do not use it for:
		* 	- to create irruegular form of a verb
		*/
	def fromStringRepr(verbStrRepr: String, verbClass: StrongVerbClassEnum, pronoun: Pronoun, tense: VerbTenseEnum,
										 mood: FinitiveMood): StrongVerb = {

		val stemType: EnumVerbStem = FinitiveStrongVerb.tenseAndNumberToStem(tense, pronoun.number)

		// remove I-Umlaut from present/singular verbs
		val matchAblautGrade = verbClass match {
			case VerbClassEnum.STRONG_7TH_CLASS => true
			case _ =>
				val optGrade = StrongVerbStem.ABLAUTS.get(verbClass)
				optGrade.get.grades(stemType).occuresIn(verbStrRepr)
		}

		val gnum = pronoun.number

		val strReprWithoutIUmlaut = (gnum, tense) match
		{
			case (SINGULAR, PRESENT) if !matchAblautGrade => I_Umlaut.unapply(verbStrRepr)
			case _ => verbStrRepr
		}

		// remove inflection
		val inflection = StrongVerb.inflectionFor(pronoun, tense)

		val stemRepr = strReprWithoutIUmlaut stripSuffix inflection

		val stem = StrongVerbStem.fromStrRepr(stemRepr, verbClass, stemType)

		FinitiveStrongVerb(verbStrRepr, stem, pronoun, tense, mood)

		// +1 do validation:
		// sv.toString == strRepr
	}

	/**
		* Use it for:
		* 	- to create an object representation of this verb, and figure out what is its stem
		*
		* Do not use it:
		* 	- to create a custom, irregular verb form
		* strong verb stem -> FinitiveStrongVerb
		*
		* @return
		*/
	def verbFrom(stem: CommonStrongVerbStem, pronoun: Pronoun, tense: VerbTenseEnum, mood: FinitiveMood)
		: FinitiveStrongVerb = {

		val str = stem.stringForm + StrongVerb.inflectionFor(pronoun, tense)

		FinitiveStrongVerb(str, stem, pronoun, tense, mood)
	}

	/*
				[Form]										[base stem]
				Infinitive								Present Stem
				Present Participle				Present Stem
				Past/Perfect Participle		Perfect Stem
				Supine										Perfect Stem
		 */
	def verbFrom(stem: CommonStrongVerbStem, nonFinitiveForm: NonFinitiveVerbType)
		: StrongVerb = nonFinitiveForm match {

		case NonFinitiveVerbType.INFINITIVE	=>	infinitiveVerbFrom(stem)

		case PRESENT_PARTICIPLE =>
			val strRepr = stem.stringForm + StrongVerb.inflectionFor(Some(VerbTenseEnum.PRESENT), VerbModeEnum.PARTICIPLE)
			NonFinitiveStrongVerb(strRepr, stem,	PRESENT_PARTICIPLE)

		case PAST_PARTICIPLE 		=>
			val strRepr = stem.stringForm + StrongVerb.inflectionFor(Some(VerbTenseEnum.PAST), VerbModeEnum.PARTICIPLE)
			NonFinitiveStrongVerb(strRepr, stem,	PAST_PARTICIPLE)
	}

	def infinitiveVerbFrom(stem: CommonStrongVerbStem): StrongVerb = {
		val lastChar = stem.stringForm().last
		val strRepr = lastChar match
		{
			case 'á' => stem.stringForm()
			case _ => stem.stringForm :+ 'a'
		}

		NonFinitiveStrongVerb(strRepr, stem, NonFinitiveVerbType.INFINITIVE)
	}

	/**
		* Use it for:
		* 	- to create an object representation of this verb, and figure out what is its stem
		*
		* Do not use it:
		* 	- to create a custom, irregular verb form
		*
		* @param strRepr
		* @param verbClass
		* @param optTense
		* @param mood
		* @return
		*/

	def fromStringRepr(strRepr: String, verbClass: StrongVerbClassEnum, optTense: Option[VerbTenseEnum]
		, mood: NonFinitiveMood): StrongVerb = {

		val (verbType, stemType) = (mood, optTense) match {

			case (PARTICIPLE, Some(PRESENT))	=> PRESENT_PARTICIPLE -> EnumVerbStem.PRESENT_STEM
			case (PARTICIPLE, Some(PAST))			=> PAST_PARTICIPLE		-> EnumVerbStem.PERFECT_STEM
			case (INFINITIVE, None)						=> NonFinitiveVerbType.INFINITIVE					-> EnumVerbStem.PRESENT_STEM
		}

		val inflection = StrongVerb.inflectionFor(optTense, mood)

		val stemRepr = strRepr stripSuffix inflection

		val stem = StrongVerbStem.fromStrRepr(stemRepr, verbClass, stemType)

		NonFinitiveStrongVerb(strRepr, stem,	verbType)
	}

	def fromStringRepr(strRepr: String, verbClass: StrongVerbClassEnum, verbType: VerbType): StrongVerb = verbType match {

		case (mood @ (INDICATIVE | SUBJUNCTIVE | IMPERATIVE), Some(tense), Some(pronoun)) =>
			fromStringRepr(strRepr, verbClass, pronoun, tense, mood.asInstanceOf[FinitiveMood])

		case (mood @ (PARTICIPLE | INFINITIVE), optTense,	None)	=>
			fromStringRepr(strRepr, verbClass, optTense, mood.asInstanceOf[NonFinitiveMood])
	}

	def inflectionFor(pronoun: Pronoun, tense: VerbTenseEnum) = tense match {

		case PRESENT => inflectionForPresent(pronoun)
		case PAST => inflectionForPreterite(pronoun)
	}

	private def inflectionForPresent(pronoun: Pronoun) = pronoun match {

		case SG_1 => ""
		case SG_2 | SG_3_FEMN | SG_3_MASC | SG_3_NEUT => "r"
		case PL_1 | DL_1 => "um"
		case PL_2 | DL_2 => "ið"
		case PL_3_FEMN | PL_3_MASC | PL_3_NEUT => "a"
	}

	private def inflectionForPreterite(pronoun: Pronoun) = pronoun match {

		case SG_1 | SG_3_FEMN | SG_3_MASC | SG_3_NEUT => ""
		case SG_2 => "t"
		case PL_1 | DL_1 => "um"
		case PL_2 | DL_2 => "uð"
		case PL_3_FEMN | PL_3_MASC | PL_3_NEUT => "u"
	}

	/**
		* @param optTense
		* @param mood
		* @return
		*/
	// TODO: Rename it to 'inflect'
	def inflectionFor(optTense: Option[VerbTenseEnum], mood: NonFinitiveMood): String = (optTense, mood) match {

		case (Some(PAST),			PARTICIPLE) => "inn"	// adjectival declension
		case (Some(PRESENT),	PARTICIPLE) => "andi"	// -andi + adjectival declension?
		case (None,			INFINITIVE) => "a"
	}
}
