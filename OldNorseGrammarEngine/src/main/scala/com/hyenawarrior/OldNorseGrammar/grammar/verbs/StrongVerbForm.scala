package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import java.lang.String.format

import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{GNumber, Pronoun}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.specialtransforms.StrongVerbSecondClassIUmlaut
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{Explicit_I_Umlaut, U_Umlaut, Umlaut, V_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.theseCanCauseUUmlaut
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveStrongVerbForm.toNonFiniteVerbType
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.NonFinitiveVerbType.{PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.{ACTIVE, MEDIO_PASSIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{FinitiveMood, NonFinitiveMood, NonFinitiveVerbType, StrongVerbClassEnum, VerbClassEnum, VerbTenseEnum, VerbVoice}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem.fromStrRepr
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem._
import com.hyenawarrior.auxiliary.TryExtract

/**
  * Created by HyenaWarrior on 2017.04.19..
  */
abstract class StrongVerbForm(strRepr: String, stem: StrongVerbStem, voice: VerbVoice) extends VerbForm(strRepr) {

  def getStem: StrongVerbStem = stem
}

case class FinitiveStrongVerbForm(override val strRepr: String, stem: StrongVerbStem, pronoun: Pronoun
  , tense: VerbTenseEnum, mood: FinitiveMood, voice: VerbVoice)	extends StrongVerbForm(strRepr, stem, voice)

object FinitiveStrongVerbForm {

  def tenseAndNumberToStem(tense: VerbTenseEnum, number: GNumber): EnumVerbStem = (tense, number) match {

    case (PRESENT,	_) 				=> EnumVerbStem.PRESENT_STEM
    case (PAST, 		SINGULAR) => EnumVerbStem.PRETERITE_SINGULAR_STEM
    case (PAST, 		PLURAL)		=> EnumVerbStem.PRETERITE_PLURAL_STEM
  }
}

case class NonFinitiveStrongVerbForm(override val strRepr: String, stem: StrongVerbStem, nonFinitiveVerbType: NonFinitiveVerbType
  , voice: VerbVoice) extends StrongVerbForm(strRepr, stem, voice) {

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

    case FinitiveStrongVerbForm(repr, stem, _, _, _, _)	=> Some(repr -> stem)
    case NonFinitiveStrongVerbForm(repr, stem, _, _) 		=> Some(repr -> stem)
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

    case (mood @ (INDICATIVE | SUBJUNCTIVE | IMPERATIVE), voice, Some(tense), Some(pronoun)) =>
      fromStringRepr(strRepr, verbClass, pronoun, tense, mood.asInstanceOf[FinitiveMood], voice)

    case (mood @ (PARTICIPLE | INFINITIVE), voice, optTense,	None)	=>
      fromStringRepr(strRepr, verbClass, optTense, mood.asInstanceOf[NonFinitiveMood], voice)
  }

  /**
    * Use it for:
    * 	- to create an object representation of this verb, and figure out what is its stem
    *
    * Do not use it:
    * 	- to create a custom, irregular verb form
    */

  private def fromStringRepr(verbStrRepr: String, verbClass: StrongVerbClassEnum, optTense: Option[VerbTenseEnum]
                             , mood: NonFinitiveMood, voice: VerbVoice): StrongVerbForm = {

    val stem = uninflect(verbStrRepr, verbClass, (mood, voice, optTense, None))

    val verb = verbFrom(stem, optTense, mood, voice)

    // +1 do validation:
    if(verb.strRepr != verbStrRepr) {

      val verbType = toNonFiniteVerbType(optTense, mood)

      throw new RuntimeException(format("Unable to match the given '%s' verbform to the specifications: %s form of %s." +
        " The form should be '%s'.",
        verbStrRepr, verbType.name, verbClass.name, verb.strRepr))
    }

    verb
  }

  private def fromStringRepr(verbStrRepr: String, verbClass: StrongVerbClassEnum, pronoun: Pronoun, tense: VerbTenseEnum,
                     mood: FinitiveMood, voice: VerbVoice): StrongVerbForm = {

    val stem = uninflect(verbStrRepr, verbClass, (mood, voice, Some(tense), Some(pronoun)))

    val verb = verbFrom(stem, pronoun, tense, mood, voice)

    // +1 do validation:
    if(verb.strRepr != verbStrRepr) {

      throw new RuntimeException(format("The given '%s' verbform is not correct to be a %s, %s person, %s tense, %s mood verb." +
        " The verb form should be '%s'.",
        verbStrRepr, verbClass.name, pronoun, tense.name, mood.name, verb.strRepr))
    }

    verb
  }



  def verbFrom(stem: StrongVerbStem, pronoun: Pronoun, tense: VerbTenseEnum, mood: FinitiveMood, voice: VerbVoice): StrongVerbForm = {

    val verbType: VerbType = (mood, voice, Some(tense), Some(pronoun))

    verbFrom(stem, verbType)
  }

  def verbFrom(stem: StrongVerbStem, optTense: Option[VerbTenseEnum], mood: NonFinitiveMood, voice: VerbVoice): StrongVerbForm = {

    val verbType: VerbType = (mood, voice, optTense, None)

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

    case (mood: FinitiveMood, voice, Some(tense), Some(pronoun)) =>
      val str: String = inflect(verbType, stem.stringForm(), stem.verbClass)

      FinitiveStrongVerbForm(str, stem, pronoun, tense, mood, voice)

    case (mood: NonFinitiveMood, voice, optTense, None) =>
      val strRepr = inflect(verbType, stem.stringForm(), stem.verbClass)

      NonFinitiveStrongVerbForm(strRepr, stem, toNonFiniteVerbType(optTense, mood), voice)
  }

  private def inflect(verbType: VerbType, stemStr: String, verbClass: VerbClassEnum): String = {

    val inflection = inflectionFor(verbType, stemStr)

    val optUmlaut = theseCanCauseUUmlaut(stemStr + inflection)
    val isPastSingular = verbType match {
      case (INDICATIVE, _, Some(PAST), Some(Pronoun(SINGULAR, _))) => true
      case _ => false
    }

    val inflectedStem = Some((stemStr, inflection))
      .map {
        case (s, infl) => applyNonProductiveRules(verbType, verbClass)(s)
          .orElse(optUmlaut.flatMap(u => u(s)))
          .getOrElse(s) -> infl
      }
      .map { case (s, infl) => VowelLengthening(s) -> infl}
      .map {
        case (s, infl) if isPastSingular => Gemination(s, infl)
        case a => a
      }
      .map { case (s, infl) => StressShift(s + infl, infl.length)._1 }
      .map(SemivowelDeletion(_))
      .map(VowelDeletion(_))
      .map(ConsonantAssimilation(_))
      .get

    inflectedStem
  }

  private def uninflect(verbStrRepr: String, verbClass: StrongVerbClassEnum, vt: VerbType): StrongVerbStem = {

    val (mood, _, optTense, optPronoun) = vt

    val restoredVerbStrRepr = ConsonantAssimilation invert verbStrRepr

    // remove inflection
    val stemRepr = restoredVerbStrRepr.flatMap(uninflect(_, vt)).head

    val subInflectIumlautClass2 = SubInflect(verbClass, vt, Some(StrongVerbSecondClassIUmlaut))
    val subInflectIumlaut = SubInflect(verbClass, vt, Some(Explicit_I_Umlaut))
    val subInflectNo = SubInflect(verbClass, vt, None)

    // reverse non-productive I-umlaut ... mainly
    (mood, optTense, optPronoun, stemRepr) match {

      case (INDICATIVE,   Some(PRESENT), Some(Pronoun(SINGULAR, _)), StrongVerbSecondClassIUmlaut(subInflectIumlautClass2(stem)))
        if verbClass == STRONG_2ND_CLASS => stem
      case (INDICATIVE,   Some(PRESENT), Some(Pronoun(SINGULAR, _)), Explicit_I_Umlaut(subInflectIumlaut(stem))) => stem
      case (SUBJUNCTIVE,  Some(PAST),    Some(_),                    Explicit_I_Umlaut(subInflectIumlaut(stem))) => stem
      case (_, _, _, subInflectNo(stem)) => stem
    }
  }

  case class SubInflect(verbClass: StrongVerbClassEnum, vt: VerbType, optTransform: Option[Any]) {

    def unapply(strWithoutUmlaut: String): Option[StrongVerbStem] = {

      val (mood, _, optTense, optPronoun) = vt
      val stemType: EnumVerbStem = stemFrom(vt)

      // undo SemivowelDeletion
      val stemStrAugFixed = augment(strWithoutUmlaut, verbClass, stemType, optTransform)

      val createStemByFrontMutation = TryExtract[String, StrongVerbStem](fromStrRepr(_, verbClass, stemType, Some(Explicit_I_Umlaut)))
      val createStemByBackMutation = TryExtract[String, StrongVerbStem](fromStrRepr(_, verbClass, stemType, Some(U_Umlaut)))
      val createStem = TryExtract[String, StrongVerbStem](fromStrRepr(_, verbClass, stemType, None))

      (mood, optTense, optPronoun, stemStrAugFixed) match {

        case (INDICATIVE, Some(PRESENT), Some(Pronoun(SINGULAR, _)), createStemByFrontMutation(stem)) if verbClass == STRONG_2ND_CLASS => Some(stem)
        case (INDICATIVE, Some(PRESENT), Some(Pronoun(SINGULAR, _)), createStemByFrontMutation(stem)) => Some(stem)
        case (SUBJUNCTIVE, Some(PAST),   Some(_),                    createStemByFrontMutation(stem)) => Some(stem)

        case (_, _, _, U_Umlaut(createStemByBackMutation(stem))) => Some(stem)
        case (_, _, _, createStem(stem))                         => Some(stem)
        case (_, _, _, VowelLengthening(createStem(stem)))       => Some(stem)
        case _ => None
      }
    }
  }

  private object InvSVDForA {

    def unapply(str: String): Option[String] = Some(str.replace("a", "ja"))
  }

  /**
    * verb-form -> non-inflected, non-augmented stem -> denormalized stem
    */
  private def augment(stemStr: String, verbClass: StrongVerbClassEnum, stemType: EnumVerbStem, optTransform: Option[Any]): String
    = (verbClass, stemType, stemStr, optTransform) match {

    // helpr <-[I-umlaut + SVD]-- hjalp- --[breaking]-> help-
    // the effect of the next line is inverted back during the stem normalization, but the normalization also add a flag
    //  to indicate that this stem does have breaking - so, yes, it's redundant, but it's important to have the flag
    case (STRONG_3RD_CLASS, PRESENT_STEM, InvSVDForA(s @ Breaking(_)), Some(Explicit_I_Umlaut)) => s

    /* do not fix the augmentation in any other cases:
      FIN    PST-STEM    PRS-STEM
      lá  -> lág-     -> liggj-
      bjó -> bjó-     -> bú-
      hjó -> hjóggv-  -> haggv-

      FIN        denomarlized stem  umlaut
      spring  -> spring-            none
      heggr   -> haggv-             I-umlaut faded the effect of U-umlaut
      syngr   -> singv-             U-umlaut

      West Germanic gemination
        * does lágum (past) have only one 'g', because past stem is not J-augmented?
        *
        * https://lrc.la.utexas.edu/eieol/norol/70
        * The augment does not appear in the past forms; the past stems ended in a single consonant,
         * which disappeared in the singular forms by the time of the ON texts.
    */
    case (STRONG_3RD_CLASS | STRONG_7_2B_CLASS, _, V_Umlaut(FixVAugmentatAfterVelar(augmentedStemStr)),         _) => augmentedStemStr
    case (STRONG_3RD_CLASS | STRONG_7_2B_CLASS, _, V_Umlaut(FixVAugmentatAfterShortSyllable(augmentedStemStr)), _) => augmentedStemStr
    case (STRONG_5TH_CLASS,                     PRESENT_STEM, FixJAugmentation(augmentedStemStr), _) => augmentedStemStr

    case _ => stemStr
  }

  private def uninflect(strRepr: String, vt: VerbType): Option[String] = {

    val inflection = inflectionFor(vt, strRepr)

    (strRepr, inflection.length) match {

      case _ if strRepr endsWith inflection => Some(strRepr dropRight inflection.length)
      case StressShift(prevStrRepr, `inflection`) => Some(prevStrRepr dropRight inflection.length)

        // fá -> fá + a
      case (_, 1) if strRepr endsWith "á" => Some(strRepr)
      case _ => None
    }
  }

  private def inflectionFor(verbType: VerbType, stemOrVerbStr: String): String = verbType match {

    case (INDICATIVE,  voice, Some(tense), Some(pronoun)) => inflectionForFinitive(voice, tense, pronoun, stemOrVerbStr)
    case (SUBJUNCTIVE, voice, Some(tense), Some(pronoun)) => inflectionForSubj(voice, tense, pronoun, stemOrVerbStr)
    case (mood: NonFinitiveMood, voice, optTense, None) => inflectionFor(optTense, voice, mood)
  }

  private def inflectionForFinitive(voice: VerbVoice, tense: VerbTenseEnum, pronoun: Pronoun, stemOrVerbStr: String)
    = (voice, tense, pronoun) match {

    case (ACTIVE, PRESENT, SG_1) => ""
    case (ACTIVE, PRESENT, SG_2 | SG_3) => "r"
    case (ACTIVE, PRESENT, PL_1) => "um"
    case (ACTIVE, PRESENT, PL_2) => "ið"
    case (ACTIVE, PRESENT, PL_3) => "a"

    case (ACTIVE, PAST, SG_1 | SG_3) => ""
    case (ACTIVE, PAST, SG_2) if stemOrVerbStr endsWith "t" => "st"
    case (ACTIVE, PAST, SG_2)  => "t"
    case (ACTIVE, PAST, PL_1) => "um"
    case (ACTIVE, PAST, PL_2) => "uð"
    case (ACTIVE, PAST, PL_3) => "u"

    case (MEDIO_PASSIVE, PRESENT, SG_1) => "umk"
    case (MEDIO_PASSIVE, PRESENT, SG_2 | SG_3) => "sk"
    case (MEDIO_PASSIVE, PRESENT, PL_1) => "umsk"
    case (MEDIO_PASSIVE, PRESENT, PL_2) => "izk"
    case (MEDIO_PASSIVE, PRESENT, PL_3) => "ask"

    case (MEDIO_PASSIVE, PAST, SG_1) => "umk"
    case (MEDIO_PASSIVE, PAST, SG_2) if !(stemOrVerbStr endsWith "t") => "tsk"
    case (MEDIO_PASSIVE, PAST, SG_2 | SG_3) => "sk"
    case (MEDIO_PASSIVE, PAST, PL_1) => "umsk"
    case (MEDIO_PASSIVE, PAST, PL_2) => "uzk"
    case (MEDIO_PASSIVE, PAST, PL_3) => "usk"
  }

  private def inflectionForSubj(voice: VerbVoice, tense: VerbTenseEnum, pronoun: Pronoun, stemOrVerbStr: String)
    = (voice, tense, pronoun) match {

    case (ACTIVE, PRESENT, SG_1) => "a"
    case (ACTIVE, PRESENT, SG_2) => "ir"
    case (ACTIVE, PRESENT, SG_3) => "i"
    case (ACTIVE, PRESENT, PL_1) => "im"
    case (ACTIVE, PRESENT, PL_2) => "ið"
    case (ACTIVE, PRESENT, PL_3) => "i"

    case (ACTIVE, PAST, SG_1) => "a"
    case (ACTIVE, PAST, SG_2)  => "ir"
    case (ACTIVE, PAST, SG_3)  => "i"
    case (ACTIVE, PAST, PL_1) => "im"
    case (ACTIVE, PAST, PL_2) => "ið"
    case (ACTIVE, PAST, PL_3) => "i"

    case (MEDIO_PASSIVE, PRESENT, SG_1) => "umk"
    case (MEDIO_PASSIVE, PRESENT, SG_2 | SG_3) => "isk"
    case (MEDIO_PASSIVE, PRESENT, PL_1) => "imsk"
    case (MEDIO_PASSIVE, PRESENT, PL_2) => "izk"
    case (MEDIO_PASSIVE, PRESENT, PL_3) => "isk"

    case (MEDIO_PASSIVE, PAST, SG_1) => "umk"
    case (MEDIO_PASSIVE, PAST, SG_2 | SG_3)  => "isk"
    case (MEDIO_PASSIVE, PAST, PL_1) => "imsk"
    case (MEDIO_PASSIVE, PAST, PL_2) => "izk"
    case (MEDIO_PASSIVE, PAST, PL_3) => "isk"
  }

  private def inflectionFor(optTense: Option[VerbTenseEnum], voice: VerbVoice, mood: NonFinitiveMood): String
  = (optTense, voice, mood) match {

    case (Some(PAST),			ACTIVE, PARTICIPLE) => "inn"	// adjectival declension
    case (Some(PRESENT),	ACTIVE, PARTICIPLE) => "andi"	// -andi + adjectival declension?
    case (None,			      ACTIVE, INFINITIVE) => "a"

    case (Some(PAST),	    MEDIO_PASSIVE, PARTICIPLE) => "izk"
    case (Some(PRESENT),	MEDIO_PASSIVE, PARTICIPLE) => "andisk"
    case (None,			      MEDIO_PASSIVE, INFINITIVE) => "ask"
  }

  private def applyNonProductiveRules(verbType: VerbType, verbClass: VerbClassEnum)(str: String): Option[String]
    = verbType match {

    case (INDICATIVE,  ACTIVE, Some(PRESENT), Some(Pronoun(SINGULAR, _))) =>

      verbClass match {

        case STRONG_2ND_CLASS => StrongVerbSecondClassIUmlaut(str)
        case _ => Explicit_I_Umlaut(str)
      }

    case (SUBJUNCTIVE, _, Some(PAST),    Some(_)) => Explicit_I_Umlaut(str)

    case (INDICATIVE, MEDIO_PASSIVE, Some(PRESENT), Some(Pronoun(SINGULAR, 2 | 3))) =>

      verbClass match {

        case STRONG_2ND_CLASS => StrongVerbSecondClassIUmlaut(str)
        case _ => Explicit_I_Umlaut(str)
      }

    // FIXME: apply U-umlaut here, to avoid interference with I-umlaut
    case _ => None
  }
}
