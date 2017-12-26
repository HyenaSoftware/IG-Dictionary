package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import java.lang.String.format

import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.{ConsonantAssimilation, Gemination, SemivowelDeletion, VowelDeletion}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Explicit_I_Umlaut, U_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveStrongVerbForm.toNonFiniteVerbType
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.{PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{EnumVerbStem, StrongVerbStem}
import com.hyenawarrior.OldNorseGrammar.grammar.{GNumber, Pronoun}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
abstract class StrongVerbForm(strRepr: String, stem: StrongVerbStem) extends VerbForm(strRepr) {

	def getStem: StrongVerbStem = stem
}

case class FinitiveStrongVerbForm(strRepr: String, stem: StrongVerbStem, pronoun: Pronoun
                                  , tense: VerbTenseEnum, mood: FinitiveMood)	extends StrongVerbForm(strRepr, stem)

object FinitiveStrongVerbForm {

	def tenseAndNumberToStem(tense: VerbTenseEnum, number: GNumber): EnumVerbStem = (tense, number) match {

		case (PRESENT,	_) 				=> EnumVerbStem.PRESENT_STEM
		case (PAST, 		SINGULAR) => EnumVerbStem.PRETERITE_SINGULAR_STEM
		case (PAST, 		PLURAL)		=> EnumVerbStem.PRETERITE_PLURAL_STEM
	}
}

case class NonFinitiveStrongVerbForm(strRepr: String, stem: StrongVerbStem, nonFinitiveVerbType: NonFinitiveVerbType)
	extends StrongVerbForm(strRepr, stem) {

  if (nonFinitiveVerbType.verbStemBase != stem.getStemType) {

    throw new RuntimeException(format("To create a verb from '%s', a %s stem is expected instead of %s.",
      strRepr, nonFinitiveVerbType.verbStemBase, stem.getStemType))
  }
}

object NonFinitiveStrongVerbForm {

  def toNonFiniteVerbType(optTense: Option[VerbTenseEnum], mood: NonFinitiveMood): NonFinitiveVerbType
    = (mood, optTense) match {

    case (PARTICIPLE, Some(PRESENT)) => PRESENT_PARTICIPLE
    case (PARTICIPLE, Some(PAST)) => PAST_PARTICIPLE
    case (INFINITIVE, None) => NonFinitiveVerbType.INFINITIVE
  }

  def moodAndTenseToStem(mood: NonFinitiveMood, optTense: Option[VerbTenseEnum]) = (mood, optTense) match {

    case (PARTICIPLE, Some(PRESENT))	=> EnumVerbStem.PRESENT_STEM
    case (PARTICIPLE, Some(PAST))			=> EnumVerbStem.PERFECT_STEM
    case (INFINITIVE, None)						=> EnumVerbStem.PRESENT_STEM
  }
}

/**
	* Meta-class of object representation
	*/
object StrongVerbForm {

	def unapply(sv: StrongVerbForm): Option[(String, StrongVerbStem)] = sv match {

		case FinitiveStrongVerbForm(repr, stem, _, _, _)	=> Some(repr -> stem)
		case NonFinitiveStrongVerbForm(repr, stem, _) 		=> Some(repr -> stem)
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
  def fromStringRepr(strRepr: String, verbClass: StrongVerbClassEnum, verbType: VerbType): StrongVerbForm = verbType match {

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
                             , mood: NonFinitiveMood): StrongVerbForm = {

    val stem = uninflect(verbStrRepr, verbClass, (mood, optTense, None))

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
										 mood: FinitiveMood): StrongVerbForm = {

		val stem = uninflect(verbStrRepr, verbClass, (mood, Some(tense), Some(pronoun)))

		val verb = verbFrom(stem, pronoun, tense, mood)

		// +1 do validation:
		if(verb.strForm != verbStrRepr) {

			throw new RuntimeException(format("The given '%s' verbform is not correct to be a %s, %s person, %s tense, %s mood verb." +
				" The verb form should be '%s'.",
				verbStrRepr, verbClass.name, pronoun, tense.name, mood.name, verb.strForm))
		}

		verb
	}



  def verbFrom(stem: StrongVerbStem, pronoun: Pronoun, tense: VerbTenseEnum, mood: FinitiveMood): StrongVerbForm = {

    val verbType: VerbType = (mood, Some(tense), Some(pronoun))

    verbFrom(stem, verbType)
  }

  def verbFrom(stem: StrongVerbStem, optTense: Option[VerbTenseEnum], mood: NonFinitiveMood): StrongVerbForm = {

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
  def verbFrom(stem: StrongVerbStem, verbType: VerbType): StrongVerbForm = verbType match {

		case (mood: FinitiveMood, Some(tense), Some(pronoun)) =>
      val str: String = inflect(verbType, stem.stringForm())

      FinitiveStrongVerbForm(str, stem, pronoun, tense, mood)

    case (mood: NonFinitiveMood, optTense, None) =>
      val strRepr = inflect(verbType, stem.stringForm())

      NonFinitiveStrongVerbForm(strRepr, stem, toNonFiniteVerbType(optTense, mood))
	}

  private def inflect(verbType: VerbType, stemStr: String): String = {

    val inflection = adjustedInflectionFor(verbType, stemStr)

    val isVAugmented = stemStr endsWith "v"
    val useUUmlaut = (U_Umlaut canTransform inflection) || isVAugmented

    val transforms: Seq[String => String] = Seq(
      s => applyNonProductiveRules(verbType)(s).orElse(if(useUUmlaut) U_Umlaut(s) else None).getOrElse(s)
      , Gemination(_, inflection)
      , SemivowelDeletion(_)
      , VowelDeletion(_)
      , ConsonantAssimilation(_)
    )

    transforms.foldLeft(stemStr)((s, f) => f(s))
  }

  private def uninflect(verbStrRepr: String, verbClass: StrongVerbClassEnum, vt: VerbType): StrongVerbStem = {

    val (mood, optTense, optPronoun) = vt
    val stemType: EnumVerbStem = stemFrom(optTense, optPronoun.map(_.number), mood)

    // remove I-Umlaut from present/singular verbs
    val matchAblautGrade = StrongVerbStem.ABLAUTS(verbClass).grades(stemType).occuresIn(verbStrRepr)

    val ConsonantAssimilation(restoredVerbStrRepr) = verbStrRepr

		// remove inflection
		val stemRepr = uninflect(restoredVerbStrRepr, vt)

		// remove non-productive changes
    val (stemReprWithoutUmlaut, appliedUmlaut) = (optPronoun.map(_.number), optTense, stemRepr) match {

      case (Some(SINGULAR), Some(PRESENT), Explicit_I_Umlaut(verbStrReprRevI)) if !matchAblautGrade =>
        verbStrReprRevI -> Some(Explicit_I_Umlaut)

      case (Some(SINGULAR), Some(PRESENT), _) if matchAblautGrade => stemRepr -> None

      case (_, _, U_Umlaut(unUmlautedStr)) => unUmlautedStr -> Some(U_Umlaut)

      case (_, _, sr) => sr -> None
		}

    StrongVerbStem.fromStrRepr(stemReprWithoutUmlaut, verbClass, stemType, appliedUmlaut)
  }

  private def uninflect(strRepr: String, vt: VerbType): String = {

    val inflection = adjustedInflectionFor(vt, strRepr)

    if (!strRepr.endsWith(inflection)) {

      val safeToIgnore = (strRepr endsWith "á") && inflection == "a"

      if(!safeToIgnore)
        throw new RuntimeException(format("Word '%s' doesn't end with -%s", strRepr, inflection))
    }

    strRepr stripSuffix inflection
  }

  private def adjustedInflectionFor(verbType: VerbType, stemOrVerbStr: String): String = verbType match {

    case (_, Some(PAST), Some(SG_2)) if stemOrVerbStr endsWith "t" => "st"
    case _ => inflectionFor(verbType)
  }

  private def inflectionFor(verbType: VerbType): String = verbType match {

    case (mood: FinitiveMood, Some(PRESENT), Some(pronoun)) => inflectionForPresent(pronoun)
    case (mood: FinitiveMood, Some(PAST),Some(pronoun)) => inflectionForPreterite(pronoun)
    case (mood: NonFinitiveMood, optTense, None) => inflectionFor(optTense, mood)
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

  private def applyNonProductiveRules(verbType: VerbType)(str: String): Option[String] = verbType match {

    case (INDICATIVE, Some(PRESENT), Some(Pronoun(SINGULAR, _))) => Explicit_I_Umlaut(str)
    // FIXME: apply U-umlaut here, to avoid interference with I-umlaut
    case _ => None
  }
}
