package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import java.lang.String._

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable.Length
import com.hyenawarrior.OldNorseGrammar.grammar.Syllables
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.{ConsonantAssimilation, SemivowelDeletion, Syncope}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{I_Umlaut, U_Umlaut, adjustedSuffixFrom, stripSuffix}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.theseCanCauseUUmlaut
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.isVowel
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{NonFinitiveMood, VerbModeEnum, VerbTenseEnum, VerbVoice, WeakVerbClassEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.WeakVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.WeakVerbStem.{dentalSuffixFor, stemFormingSuffix}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem._

/**
  * Created by HyenaWarrior on 2017.04.19..
  */
class WeakVerbForm(strRepr: String, stem: WeakVerbStem, voice: VerbVoice) extends VerbForm(strRepr) {

  def getStem: WeakVerbStem = stem
}

object WeakVerbForm {

  def unapply(sv: WeakVerbForm): Option[(String, WeakVerbStem)] = Some(sv.strRepr -> sv.getStem)

  // Stem -> VerbForm
  def verbFrom(stem: WeakVerbStem, vt: VerbType): WeakVerbForm = {

    val str = inflect(vt, stem.stringForm(), stem.verbClass)

    new WeakVerbForm(str, stem, vt._2)
  }

  // String -> VerbForm
  def fromStringRepr(verbStrRepr: String, verbClass: WeakVerbClassEnum, vt: VerbType): WeakVerbForm = {

    val stem = uninflect(verbStrRepr, verbClass, vt)

    val verb = verbFrom(stem, vt)

    // +1 do validation:
    if(verb.strRepr != verbStrRepr) {

      val (mood: VerbModeEnum, _, optTense, optPronoun) = vt

      val pronounStr = optPronoun.map(_.toString + " person, ").getOrElse("")
      val tenseStr = optTense.map(_.name + " tense, ").getOrElse("")

      throw new RuntimeException(format("The given '%s' verbform is not correct to be a %s, %s%s%s mood verb." +
        " The verb form should be '%s'.",
        verbStrRepr, verbClass.name, pronounStr, tenseStr, mood.name, verb.strRepr))
    }

    verb
  }

  private def weakVerbStemEnumFrom(verbType: VerbType) = verbType match {

    case (_, _, Some(PRESENT) | None, _) => PRESENT_STEM
    case (_, _, Some(PAST), Some(Pronoun(SINGULAR, _))) => PRETERITE_SINGULAR_STEM
    case (_, _, Some(PAST), Some(Pronoun(PLURAL, _)))   => PRETERITE_PLURAL_STEM
    case (_, _, Some(PAST), None) => PERFECT_STEM
    case _ =>
      throw new RuntimeException("Verb can't be create from this stem.")
  }

  private def inflect(verbType: VerbType, stemStr: String, verbClass: WeakVerbClassEnum): String = {

    val infl = inflectionFor(verbType, stemStr)

    val verbStem = weakVerbStemEnumFrom(verbType)
    val (mood, _, _, _) = verbType

    // delete thematic vowel: kalla + um -> *k[ǫ->a]ll + um ->kǫllum
    // skip 'j', that will be eliminated by SVD if necessary
    val stemReduced = if(verbStem == PRESENT_STEM
      && stemStr.last != 'j'
      && infl.headOption.exists(isVowel)) {

      val stemSuffix = stemFormingSuffix(stemStr.init, verbClass)

      stripSuffix(stemStr, stemSuffix)

    } else stemStr

    val stemStrU = (theseCanCauseUUmlaut(infl), verbClass, mood, verbStem) match {

      case (Some(U_Umlaut), WEAK_A_STEM
                          | WEAK_I_STEM, INDICATIVE
                                       | INFINITIVE
                                       | PARTICIPLE, _          ) => U_Umlaut(stemReduced) getOrElse stemReduced
      case (_,              WEAK_J_STEM, INDICATIVE
                                       | INFINITIVE, PRESENT_STEM) => I_Umlaut(stemReduced) getOrElse stemReduced
      case (_,              WEAK_J_STEM, INDICATIVE
                                       | PARTICIPLE, PRETERITE_SINGULAR_STEM
                                                   | PRETERITE_PLURAL_STEM
                                                   | PERFECT_STEM ) if isOverlongVerb(stemReduced, verbStem)
                                                          => I_Umlaut(stemReduced) getOrElse stemReduced
      case (Some(U_Umlaut), WEAK_J_STEM, INDICATIVE
                                       | PARTICIPLE, PRETERITE_SINGULAR_STEM
                                                   | PRETERITE_PLURAL_STEM
                                                   | PERFECT_STEM ) => U_Umlaut(stemReduced) getOrElse stemReduced
      case (Some(U_Umlaut), WEAK_A_STEM, SUBJUNCTIVE, _) => U_Umlaut(stemReduced) getOrElse stemReduced
      case (_,              WEAK_I_STEM, SUBJUNCTIVE, PRETERITE_SINGULAR_STEM
                                                    | PRETERITE_PLURAL_STEM
                                                    | PERFECT_STEM ) => I_Umlaut(stemReduced) getOrElse stemReduced
      case (_,              WEAK_J_STEM, SUBJUNCTIVE, _) => I_Umlaut(stemReduced) getOrElse stemReduced
      case _ => stemReduced
    }

    val inflectedStem = stemStrU + infl
    val stemSvd = SemivowelDeletion(inflectedStem)
    ConsonantAssimilation(stemSvd)
  }

  private def uninflect(verbStrRepr: String, verbClass: WeakVerbClassEnum, verbType: VerbType): WeakVerbStem = {

    val verbStrReprs = ConsonantAssimilation transform(verbStrRepr, (a, b) => true)

    val results = (verbStrReprs :+ verbStrRepr).flatMap(s => try {
        Some(uninflect2(s, verbClass, verbType))
      } catch { case _: Exception => None })

    results.headOption.getOrElse {

      throw new RuntimeException(s"Can not be generated stem from $verbStrRepr")
    }
  }

  private def uninflect2(verbStrRepr: String, verbClass: WeakVerbClassEnum, verbType: VerbType): WeakVerbStem = {

    val verbStem = weakVerbStemEnumFrom(verbType)
    val (mood, _, _, _) = verbType

    val infl = inflectionFor(verbType, verbStrRepr)
    val adjInfl = adjustedSuffixFrom(verbStrRepr, infl)

    if(!(verbStrRepr endsWith adjInfl)) {

      throw new RuntimeException(s"The '$verbStrRepr' verb doesn't end with -$infl.")
    }

    val stemStr = verbStrRepr stripSuffix adjInfl

    // restore thematic vowel
    val stemWithSuffix = verbStem match {

      case PRESENT_STEM if (Consonant isConsonant stemStr.last) && stemStr.last != 'j' =>

        val stemSuffix = stemFormingSuffix(stemStr, verbClass)
        stemStr + stemSuffix

      // back vowels of inflections preserves the 'j' semivowel
      case PRESENT_STEM => stemStr
      case PRETERITE_SINGULAR_STEM | PRETERITE_PLURAL_STEM | PERFECT_STEM
        if stemStr endsWith dentalSuffixFor(stemStr.init) => stemStr

      case _ => throw new RuntimeException(s"Incorrect stem: $stemStr")
    }


    // restore thematic vowel: kǫllum -> *k[ǫ->a]ll + um -> kalla + um
    val stemStrNoU = (theseCanCauseUUmlaut(infl), stemWithSuffix, verbClass, mood, verbStem) match {

      case (_,              I_Umlaut(s), WEAK_J_STEM, INDICATIVE
                                                    | INFINITIVE, PRESENT_STEM) => s
      case (_,              I_Umlaut(s), WEAK_J_STEM, INDICATIVE
                                                    | PARTICIPLE, PRETERITE_SINGULAR_STEM
                                                                | PRETERITE_PLURAL_STEM
                                                                | PERFECT_STEM) if isOverlongVerb(s, verbStem) => s
      case (Some(U_Umlaut), U_Umlaut(s), WEAK_J_STEM, INDICATIVE
                                                    | PARTICIPLE, PRETERITE_SINGULAR_STEM
                                                                | PRETERITE_PLURAL_STEM
                                                                | PERFECT_STEM) => s
      case (Some(U_Umlaut), U_Umlaut(s), WEAK_A_STEM, SUBJUNCTIVE, _) => s
      case (Some(U_Umlaut), U_Umlaut(s), WEAK_A_STEM
                                       | WEAK_I_STEM, INDICATIVE
                                                    | INFINITIVE, _) => s
      case (_,              I_Umlaut(s), WEAK_J_STEM, SUBJUNCTIVE, _) => s
      case (_,              I_Umlaut(s), WEAK_I_STEM, SUBJUNCTIVE, PRETERITE_SINGULAR_STEM
                                                                   | PRETERITE_PLURAL_STEM
                                                                   | PERFECT_STEM) => s
      case (_, s, _, _, _) => s
    }

    new WeakVerbStem(stemStrNoU, verbClass, verbStem, TransformationMode.Undefined)
  }

  private def isOverlongVerb(strStem: String, verbStem: EnumVerbStem): Boolean = {

    val stemStr = if(verbStem == PRESENT_STEM) strStem else strStem.init

    stemStr match {

      case Syllables(sy :: _) => sy.length == Length.OVERLONG
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

    case (ACTIVE, PAST, SG_1) => "a"
    case (ACTIVE, PAST, SG_2) => "ir"
    case (ACTIVE, PAST, SG_3) => "i"
    case (ACTIVE, PAST, PL_1) => "um"
    case (ACTIVE, PAST, PL_2) => "uð"
    case (ACTIVE, PAST, PL_3) => "u"

    case (MEDIO_PASSIVE, PRESENT, SG_1) => "umk"
    case (MEDIO_PASSIVE, PRESENT, SG_2 | SG_3) => "sk" // rsk
    case (MEDIO_PASSIVE, PRESENT, PL_1) => "umsk"
    case (MEDIO_PASSIVE, PRESENT, PL_2) => "izk" // iðsk
    case (MEDIO_PASSIVE, PRESENT, PL_3) => "ask"

    case (MEDIO_PASSIVE, PAST, SG_1) => "umk"
    case (MEDIO_PASSIVE, PAST, SG_2 | SG_3) => "isk" // irsk
    case (MEDIO_PASSIVE, PAST, PL_1) => "umsk"
    case (MEDIO_PASSIVE, PAST, PL_2) => "uzk" // uðsk
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
    case (ACTIVE, PAST, SG_2) => "ir"
    case (ACTIVE, PAST, SG_3) => "i"
    case (ACTIVE, PAST, PL_1) => "im"
    case (ACTIVE, PAST, PL_2) => "ið"
    case (ACTIVE, PAST, PL_3) => "i"

    case (MEDIO_PASSIVE, PRESENT, SG_1) => "umk"
    case (MEDIO_PASSIVE, PRESENT, SG_2 | SG_3) => "isk" // irsk
    case (MEDIO_PASSIVE, PRESENT, PL_1) => "imsk"
    case (MEDIO_PASSIVE, PRESENT, PL_2) => "izk" // iðsk
    case (MEDIO_PASSIVE, PRESENT, PL_3) => "isk"

    case (MEDIO_PASSIVE, PAST, SG_1) => "umk"
    case (MEDIO_PASSIVE, PAST, SG_2 | SG_3)  => "isk"
    case (MEDIO_PASSIVE, PAST, PL_1) => "imsk"
    case (MEDIO_PASSIVE, PAST, PL_2) => "izk" // iðsk
    case (MEDIO_PASSIVE, PAST, PL_3) => "isk"
  }

  private def inflectionFor(optTense: Option[VerbTenseEnum], voice: VerbVoice, mood: NonFinitiveMood): String
  = (optTense, voice, mood) match {

    case (Some(PAST),			ACTIVE, PARTICIPLE) => "r"	// adjectival declension
    case (Some(PRESENT),	ACTIVE, PARTICIPLE) => "andi"	// -andi + adjectival declension?
    case (None,			      ACTIVE, INFINITIVE) => "a"

    case (Some(PAST),	    MEDIO_PASSIVE, PARTICIPLE) => "sk"
    case (Some(PRESENT),	MEDIO_PASSIVE, PARTICIPLE) => "andisk"
    case (None,			      MEDIO_PASSIVE, INFINITIVE) => "ask"
  }
}