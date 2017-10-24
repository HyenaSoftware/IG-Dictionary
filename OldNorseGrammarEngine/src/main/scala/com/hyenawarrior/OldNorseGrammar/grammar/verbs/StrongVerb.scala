package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import java.lang.String.format

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Explicit_I_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.SemivowelDeletion
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
	, tense: VerbTenseEnum, mood: FinitiveMood)	extends StrongVerb(strRepr, stem)

object FinitiveStrongVerb {

	def tenseAndNumberToStem(tense: VerbTenseEnum, number: GNumber): EnumVerbStem = (tense, number) match {

		case (PRESENT,	_) 				=> EnumVerbStem.PRESENT_STEM
		case (PAST, 		SINGULAR) => EnumVerbStem.PRETERITE_SINGULAR_STEM
		case (PAST, 		PLURAL)		=> EnumVerbStem.PRETERITE_PLURAL_STEM
	}
}

case class NonFinitiveStrongVerb(strRepr: String, stem: CommonStrongVerbStem, nonFinitiveVerbType: NonFinitiveVerbType)
	extends StrongVerb(strRepr, stem) {

	if(nonFinitiveVerbType.verbStemBase != stem.getStemType()) {

		throw new RuntimeException(format("To create a verb from '%s', a %s stem is expected instead of %s.",
			strRepr, nonFinitiveVerbType.verbStemBase, stem.getStemType()))
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
  def fromStringRepr(strRepr: String, verbClass: StrongVerbClassEnum, verbType: VerbType): StrongVerb = verbType match {

    case (mood @ (INDICATIVE | SUBJUNCTIVE | IMPERATIVE), Some(tense), Some(pronoun)) =>
      fromStringRepr(strRepr, verbClass, pronoun, tense, mood.asInstanceOf[FinitiveMood])

    case (mood @ (PARTICIPLE | INFINITIVE), optTense,	None)	=>
      fromStringRepr(strRepr, verbClass, optTense, mood.asInstanceOf[NonFinitiveMood])
  }

  /**
    * Use it for:
    * 	- to create an object representation of this verb, and figure out what is its stem
    *
    * Do not use it:
    * 	- to create a custom, irregular verb form
    */

  private def fromStringRepr(verbStrRepr: String, verbClass: StrongVerbClassEnum, optTense: Option[VerbTenseEnum]
                             , mood: NonFinitiveMood): StrongVerb = {

    val stemType = moodAndTenseToStem(mood, optTense)

    val stem: CommonStrongVerbStem = uninflect(verbStrRepr, verbClass, optTense, mood, stemType)

    val verb = verbFrom(stem, optTense, mood)

    // +1 do validation:
    if(verb.strForm != verbStrRepr) {

      val verbType = toNonFiniteVerbType(optTense, mood)

      throw new RuntimeException(format("Unable to match the given '%s' verbform to the specifications: %s form of %s." +
        " The form should be '%s'.",
        verbStrRepr, verbType.name, verbClass.name, verb.strForm))
    }

    verb
  }

	private def fromStringRepr(verbStrRepr: String, verbClass: StrongVerbClassEnum, pronoun: Pronoun, tense: VerbTenseEnum,
										 mood: FinitiveMood): StrongVerb = {

		val stem = uninflect(verbStrRepr, verbClass, pronoun, tense)

		val verb = verbFrom(stem, pronoun, tense, mood)

		// +1 do validation:
		if(verb.strForm != verbStrRepr) {

			throw new RuntimeException(format("The given '%s' verbform is not correct to be a %s, %s person, %s tense, %s mood verb." +
				" The verb form should be '%s'.",
				verbStrRepr, verbClass.name, pronoun, tense.name, mood.name, verb.strForm))
		}

		verb
	}

  private def toNonFiniteVerbType(optTense: Option[VerbTenseEnum], mood: NonFinitiveMood): NonFinitiveVerbType
    = (mood, optTense) match {

    case (PARTICIPLE, Some(PRESENT)) => PRESENT_PARTICIPLE
    case (PARTICIPLE, Some(PAST)) => PAST_PARTICIPLE
    case (INFINITIVE, None) => NonFinitiveVerbType.INFINITIVE
  }

  private def moodAndTenseToStem(mood: NonFinitiveMood, optTense: Option[VerbTenseEnum]) = (mood, optTense) match {

    case (PARTICIPLE, Some(PRESENT))	=> EnumVerbStem.PRESENT_STEM
    case (PARTICIPLE, Some(PAST))			=> EnumVerbStem.PERFECT_STEM
    case (INFINITIVE, None)						=> EnumVerbStem.PRESENT_STEM
  }

  def verbFrom(stem: CommonStrongVerbStem, pronoun: Pronoun, tense: VerbTenseEnum, mood: FinitiveMood): StrongVerb = {

    val verbType: VerbType = (mood, Some(tense), Some(pronoun))

    verbFrom(stem, verbType)
  }

  def verbFrom(stem: CommonStrongVerbStem, optTense: Option[VerbTenseEnum], mood: NonFinitiveMood): StrongVerb = {

    val verbType: VerbType = (mood, optTense, None)

    verbFrom(stem, verbType)
  }

  /**
    * Use it for:
    * 	- to create an object representation of this verb, and figure out what is its stem
    *
    * Do not use it:
    * 	- to create a custom, irregular verb form
    * strong verb stem -> FinitiveStrongVerb/NonFinitiveStrongVerb
    *
    *		[Form]										[base stem]
    *		Infinitive								Present Stem
    *		Present Participle				Present Stem
    *		Past/Perfect Participle		Perfect Stem
    *		Supine										Perfect Stem
    */
  def verbFrom(stem: CommonStrongVerbStem, verbType: VerbType): StrongVerb = verbType match {

		case (mood: FinitiveMood, Some(tense), Some(pronoun)) =>
      val transforms: Seq[String => String] = Seq(
        inflect(verbType)
        // execute custom non-productive transformations
        , applyNonProductiveRules(verbType)
        // execute all productive transformations
        , applyProductiveTransformations)

      val stemStr = stem.stringForm()

      val str = transforms.foldLeft(stemStr)((s, f) => f(s))
      FinitiveStrongVerb(str, stem, pronoun, tense, mood)

    case (INFINITIVE, _, None)	=>	infinitiveVerbFrom(stem)

    case (VerbModeEnum.PARTICIPLE, optTense @ Some(_), None) =>
      val vt = toNonFiniteVerbType(optTense, VerbModeEnum.PARTICIPLE)
      val stemStr = stem.stringForm()
      val strRepr = inflect(verbType)(stemStr)

      NonFinitiveStrongVerb(strRepr, stem,	vt)
	}

  private def inflect(verbType: VerbType)(str: String): String = str + inflectionFor(verbType)

	private def infinitiveVerbFrom(stem: CommonStrongVerbStem): StrongVerb = {

		val lastChar = stem.stringForm().last
		val strRepr = lastChar match
		{
			case 'á' => stem.stringForm()
			case _ => stem.stringForm :+ 'a'
		}

    NonFinitiveStrongVerb(strRepr, stem, NonFinitiveVerbType.INFINITIVE)
  }

	private def uninflect(strRepr: String, verbClass: StrongVerbClassEnum, optTense: Option[VerbTenseEnum]
    , mood: NonFinitiveMood, stemType: EnumVerbStem): CommonStrongVerbStem = {

		val inflection = inflectionFor(optTense, mood)

    val stemRepr = uninflect(strRepr, inflection)

    StrongVerbStem.fromStrRepr(stemRepr, verbClass, stemType)
	}

  private def uninflect(verbStrRepr: String, verbClass: StrongVerbClassEnum, pronoun: Pronoun
                        , tense: VerbTenseEnum): CommonStrongVerbStem = {

    val stemType: EnumVerbStem = FinitiveStrongVerb.tenseAndNumberToStem(tense, pronoun.number)

    // remove I-Umlaut from present/singular verbs
    val matchAblautGrade = verbClass match {
      case VerbClassEnum.STRONG_7TH_CLASS => true
      case _ =>
        val optGrade = StrongVerbStem.ABLAUTS.get(verbClass)
        optGrade.get.grades(stemType).occuresIn(verbStrRepr)
    }

    val gnum = pronoun.number

    val strReprWithoutIUmlaut = (gnum, tense) match {
      case (SINGULAR, PRESENT) if !matchAblautGrade => Explicit_I_Umlaut.unapply(verbStrRepr)
      case _ => verbStrRepr
    }

    // remove inflection
    val inflection = inflectionFor(pronoun, tense)

    val stemRepr = uninflect(strReprWithoutIUmlaut, inflection)

    StrongVerbStem.fromStrRepr(stemRepr, verbClass, stemType)
  }

  private def uninflect(strRepr: String, inflection: String): String = {

    if (!strRepr.endsWith(inflection)) {

      throw new RuntimeException(format("Word '%s' doesn't end with -%s", strRepr, inflection))
    }

    strRepr stripSuffix inflection
  }

  private def inflectionFor(verbType: VerbType): String = verbType match {

    case (mood: FinitiveMood, Some(tense), Some(pronoun)) => inflectionFor(pronoun, tense)
    case (mood: NonFinitiveMood, optTense, None) => inflectionFor(optTense, mood)
  }

  private def inflectionFor(pronoun: Pronoun, tense: VerbTenseEnum) = tense match {

		case PRESENT => inflectionForPresent(pronoun)
		case PAST => inflectionForPreterite(pronoun)
	}

	private def inflectionForPresent(pronoun: Pronoun) = pronoun match {

		case SG_1 => ""
		case SG_2 | SG_3 => "r"
		case PL_1 => "um"
		case PL_2 => "ið"
		case PL_3 => "a"
	}

	private def inflectionForPreterite(pronoun: Pronoun) = pronoun match {

		case SG_1 | SG_3 => ""
		case SG_2 => "t"
		case PL_1 => "um"
		case PL_2 => "uð"
		case PL_3 => "u"
	}

	private def inflectionFor(optTense: Option[VerbTenseEnum], mood: NonFinitiveMood): String = (optTense, mood) match {

		case (Some(PAST),			PARTICIPLE) => "inn"	// adjectival declension
		case (Some(PRESENT),	PARTICIPLE) => "andi"	// -andi + adjectival declension?
		case (None,			INFINITIVE) => "a"
	}

  private def applyNonProductiveRules(verbType: VerbType)(str: String): String = verbType match {

    case (INDICATIVE, Some(PRESENT), Some(Pronoun(SINGULAR, _))) => Explicit_I_Umlaut.forceApply(str)
    case _ => str
  }

  private def applyProductiveTransformations(str: String): String = SemivowelDeletion(str)
}
