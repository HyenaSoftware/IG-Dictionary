package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.WeakVerbClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.WeakVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.{Pos, verbs}

/**
  * Created by HyenaWarrior on 2018.04.15..
  */
case class WeakVerb(verbClass: WeakVerbClassEnum
                    , givenVerbForms: Map[VerbType, WeakVerbForm]
                    , generatedVerbForms: Map[VerbType, WeakVerbForm]
                    , overriddenVerbForms: Map[VerbType, WeakVerbForm]) extends Verb {

  lazy val verbForms = givenVerbForms ++ generatedVerbForms ++ overriddenVerbForms

  override val forms: Map[VerbType, WeakVerbForm] = verbForms

  override val PRIMARY_KEY = (INFINITIVE, ACTIVE, None, None)
}

object WeakVerb {


  private val STEM_PREFERENCES = Map(
    PRESENT_STEM ->             Seq(PRESENT_STEM, PERFECT_STEM, PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM),
    PRETERITE_SINGULAR_STEM ->  Seq(PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM, PRESENT_STEM, PERFECT_STEM),
    PRETERITE_PLURAL_STEM   ->  Seq(PRETERITE_PLURAL_STEM, PRETERITE_SINGULAR_STEM, PRESENT_STEM, PERFECT_STEM),
    PERFECT_STEM ->             Seq(PERFECT_STEM, PRESENT_STEM, PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM)
  )

  def apply(verbClass: WeakVerbClassEnum, givenVerbFormStrings: Map[VerbType, String]): WeakVerb = {

    val givenVerbForms: Map[VerbType, WeakVerbForm] = givenVerbFormStrings.map {

      case (vt, rawStr) => vt -> WeakVerbForm.fromStringRepr(rawStr, verbClass, vt)
    }

    val stems = extractStems(givenVerbForms.map { case (k, v) => k -> v.getStem }, verbClass)

    // exclude the base form definition and the overrides
    val missingDeclensions = VerbContext.ALL_VERB_FORMS.filterNot(givenVerbForms.contains)

    val generatedVerbForms = missingDeclensions.map { vt =>

      val expectedStemType = verbs.stemFrom(vt)
      vt -> WeakVerbForm.verbFrom(stems(expectedStemType), vt)
    }
      .toMap

    new WeakVerb(verbClass, givenVerbForms, generatedVerbForms, Map())
  }

  /**
    * [(vt1, s1), (vt2, s2), ...] -> (PRESENT, s1)
    *
    * @param pseudoVerbForms
    * @param verbClassEnum
    * @return
    */
  private def extractStems(pseudoVerbForms: Map[VerbType, WeakVerbStem], verbClassEnum: WeakVerbClassEnum)
    : Map[EnumVerbStem, WeakVerbStem] = {

    // collect what we have
    val pseudoFormsToStem: Map[EnumVerbStem, Iterable[WeakVerbStem]] = pseudoVerbForms.map {

      case (vt, stem) =>
        val stemType = verbs.stemFrom(vt)
        WeakVerbStem.fromStrRepr(stem.stringForm(), verbClassEnum, stemType)
    }
      .groupBy ( _.getStemType )

    val pseudoStemsBy: Map[EnumVerbStem, WeakVerbStem] = pseudoFormsToStem.map { case(e, m) => e -> m.head }

    // build stems from the root
    val stemMap = STEM_PREFERENCES.flatMap {

      case (k, stemPrefList) => getOrCreateStem(stemPrefList, pseudoStemsBy, verbClassEnum).map(k -> _)
    }

    stemMap
  }

  /**
    *
    *
    * @param preferences
    * @param pseudoStemsBy
    * @param verbClassEnum verb class of these stems
    * @return the 'preferences.head' element from 'pseudoStemsBy' or a generated from the closest stem
    */
  private def getOrCreateStem(preferences: Seq[EnumVerbStem], pseudoStemsBy: Map[EnumVerbStem, WeakVerbStem]
                              , verbClassEnum: WeakVerbClassEnum): Option[WeakVerbStem] = {

    val primaryStem = preferences.head
    val secondaryStems = preferences.tail

    val optTransformedClosestStem = pseudoStemsBy
      // use the primary form
      .get(primaryStem)
      // or create it from another form
      .orElse(secondaryStems.collectFirst {

      case verbStem if pseudoStemsBy.contains(verbStem) =>
        // stem -> root -> stem'
        val srcStem: WeakVerbStem = pseudoStemsBy(verbStem)
        WeakVerbStem.fromRoot(srcStem.getRoot, verbClassEnum, preferences.head)
    })

    optTransformedClosestStem
  }
}