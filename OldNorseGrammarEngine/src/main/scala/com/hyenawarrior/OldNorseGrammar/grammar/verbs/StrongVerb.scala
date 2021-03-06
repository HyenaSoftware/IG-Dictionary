package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{AblautGrade, I_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum.{INDICATIVE, INFINITIVE, PARTICIPLE, SUBJUNCTIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{StrongVerbClassEnum, VerbModeEnum, VerbTenseEnum, VerbVoice}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.StrongVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem._
import com.hyenawarrior.OldNorseGrammar.grammar.{Pos, verbs}

/**
  * Created by HyenaWarrior on 2017.10.08..
  *
  * TODO rename it to StrongVerb once StrongVerb and similar classes renamed to StrongVerbForm
  *
  */

object VerbContext {

  val ALL_VERB_FORMS: List[VerbType] = (List(INDICATIVE, SUBJUNCTIVE)
    .flatMap{ mood => VerbTenseEnum.values.map(tense => mood -> Some(tense)) }
    .flatMap{ case (mood, oTense) =>
      Pronoun.values.map(pronoun => (mood, oTense, Some(pronoun)))
    } ++
    List((INFINITIVE, None, None)) ++
    List((PARTICIPLE, Some(PRESENT), None)) ++
    List((PARTICIPLE, Some(PAST), None)))
    .flatMap { case (m, oT, oP) => VerbVoice.values.map(v => (m, v, oT, oP)) }
}

/**
  * Proposed:
  *   StrongVerb(StrongVerbClassEnum
  *     , Map[EnumVerbStem, AblautGrade]
  *     , Map[VerbType, StrongVerbForm] // restrictions
  *     , Map[VerbType, StrongVerbForm] // generated forms
  *     , Map[VerbType, StrongVerbForm] // direct overrides
  *
  * @param verbClass
  * @param ablautGrade
  * @param givenVerbForms
  * @param generatedVerbForms
  * @param overriddenVerbForms
  */
case class StrongVerb(verbClass: StrongVerbClassEnum, ablautGrade: Map[EnumVerbStem, AblautGrade]
                      , givenVerbForms: Map[VerbType, StrongVerbForm]
                      , generatedVerbForms: Map[VerbType, StrongVerbForm]
                      , overriddenVerbForms: Map[VerbType, StrongVerbForm]) extends Verb {

  lazy val verbForms = givenVerbForms ++ generatedVerbForms ++ overriddenVerbForms

  def overrideFormsOf(overriddenVerbForms: Map[VerbType, StrongVerbForm]): StrongVerb = {

    val generatedVerbForms2 = generatedVerbForms -- overriddenVerbForms.keys

    new StrongVerb(verbClass, ablautGrade, givenVerbForms, generatedVerbForms2, overriddenVerbForms)
  }

  override val forms: Map[VerbType, StrongVerbForm] = verbForms

  override val PRIMARY_KEY = (INFINITIVE, ACTIVE, None, None)
}

object StrongVerb {

  def apply(verbClass: StrongVerbClassEnum, givenVerbFormStrings: Map[VerbType, String]): StrongVerb = {

    val givenVerbForms: Map[VerbType, StrongVerbForm] = givenVerbFormStrings.map {

      case (vt, rawStr) => vt -> StrongVerbForm.fromStringRepr(rawStr, verbClass, vt)
    }

    val stems = extractStems(givenVerbForms.map { case (k, v) => k -> v.getStem }, verbClass)

    val ablautGrades = stems.map{ case (k, v) => k -> v.getAblautGrade }

    // exclude the base form definition and the overrides
    val missingDeclensions = VerbContext.ALL_VERB_FORMS.filterNot(givenVerbForms.contains)

    val generatedVerbForms = missingDeclensions.map { vt =>

        val expectedStemType = verbs.stemFrom(vt)
        vt -> StrongVerbForm.verbFrom(stems(expectedStemType), vt)
      }
      .toMap

    new StrongVerb(verbClass, ablautGrades, givenVerbForms, generatedVerbForms, Map())
  }

  private val STEM_PREFERENCES = Map(
    PRESENT_STEM ->             Seq(PRESENT_STEM, PERFECT_STEM, PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM),
    PRETERITE_SINGULAR_STEM ->  Seq(PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM, PRESENT_STEM, PERFECT_STEM),
    PRETERITE_PLURAL_STEM   ->  Seq(PRETERITE_PLURAL_STEM, PRETERITE_SINGULAR_STEM, PRESENT_STEM, PERFECT_STEM),
    PERFECT_STEM ->             Seq(PERFECT_STEM, PRESENT_STEM, PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM)
  )

  private def extractStems(pseudoVerbForms: Map[VerbType, StrongVerbStem], verbClassEnum: StrongVerbClassEnum):
    Map[EnumVerbStem, StrongVerbStem] = {

    // collect what we have
    val pseudoFormsToStem: Map[EnumVerbStem, Iterable[StrongVerbStem]] = pseudoVerbForms.map {

      case ((INDICATIVE, _, Some(VerbTenseEnum.PRESENT), Some(Pronoun(SINGULAR, _))), stem) =>
        StrongVerbStem.fromStrRepr(stem.stringForm(), verbClassEnum, EnumVerbStem.PRESENT_STEM, Some(I_Umlaut))

      case (vt, stem) =>
        // do a stem -> string -> conversion to force validate the inputs
        //	TODO: is this double conversion really necessary?
        val stemType = verbs.stemFrom(vt)
        StrongVerbStem.fromStrRepr(stem.stringForm(), verbClassEnum, stemType)
    }
      .groupBy ( _.getStemType )

    val pseudoStemsBy: Map[EnumVerbStem, StrongVerbStem] = pseudoFormsToStem.map { case(e, m) => e -> m.head }

    // build stems from the root
    val stemMap = STEM_PREFERENCES.flatMap {

      case (k, v) => getOrCreateStem(v, pseudoStemsBy, verbClassEnum).map(k -> _)
    }

    stemMap
  }

  private def getOrCreateStem(preferences: Seq[EnumVerbStem], pseudoStemsBy: Map[EnumVerbStem, StrongVerbStem]
                              , verbClassEnum: StrongVerbClassEnum): Option[StrongVerbStem] = {

    val primaryStem = preferences.head
    val secondaryStems = preferences.tail

    val optTransformedClosestStem = pseudoStemsBy
      // use the primary form
      .get(primaryStem)
      // or create it from another form
      .orElse(secondaryStems.collectFirst {

      case verbStem if pseudoStemsBy.contains(verbStem) =>
        val srcStem = pseudoStemsBy(verbStem)
        StrongVerbStem.fromRoot(srcStem.getRoot, verbClassEnum, preferences.head)
    })

    optTransformedClosestStem
  }
}
