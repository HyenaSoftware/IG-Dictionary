package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{AblautGrade, Explicit_I_Umlaut}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.{INDICATIVE, INFINITIVE, PARTICIPLE, SUBJUNCTIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{PAST, PRESENT}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{EnumVerbStem, StrongVerbStem}
import com.hyenawarrior.OldNorseGrammar.grammar.{GNumber, Pronoun, verbs}

/**
	* Created by HyenaWarrior on 2017.10.08..
	*
	* TODO rename it to StrongVerb once StrongVerb and similar classes renamed to StrongVerbForm
	*
	*/

object VerbContext {

	//def product[T, U](seq: Seq[T], seq2: Seq[U]) = seq.flatMap(e => seq2.map(???))

	val ALL_VERB_FORMS: List[VerbType] = List(INDICATIVE, SUBJUNCTIVE)
		.flatMap(mood => VerbTenseEnum.values.map(tense => mood -> Some(tense)))
	  .flatMap{ case (mood, oTense) =>
      Pronoun.values.map(pronoun => (mood, oTense, Some(pronoun)))
    } ++
    List((INFINITIVE, None, None)) ++
    List((PARTICIPLE, Some(PRESENT), None)) ++
    List((PARTICIPLE, Some(PAST), None))
}

case class StrongVerb(verbClass: StrongVerbClassEnum, ablautGrade: Map[EnumVerbStem, AblautGrade]
                      , verbForms: Map[VerbType, StrongVerbForm])

object StrongVerb {

  def apply(verbClass: StrongVerbClassEnum, givenVerbFormStrings: Map[VerbType, String]): StrongVerb = {

    val givenVerbForms: Map[VerbType, StrongVerbForm] = givenVerbFormStrings.map {

      case (vt, rawStr) => vt -> StrongVerbForm.fromStringRepr(rawStr, verbClass, vt)
    }

    val stems = extractStems(givenVerbForms.map { case (k, v) => k -> v.getStem }, verbClass)

    val ablautGrades = stems.map{ case (k, v) => k -> v.getAblautGrade }

    // exclude the base form definition and the overrides
    val missingDeclensions = VerbContext.ALL_VERB_FORMS.filterNot(givenVerbForms.contains)

    val missingVerbForms = missingDeclensions
      .map {
        case vt@(mood, optTense, optPronoun) =>
          val expectedStemType = verbs.stemFrom(optTense, optPronoun.map(_.number), mood)
          vt -> StrongVerbForm.verbFrom(stems(expectedStemType), vt)
      }
      .toMap

    val verbForms = givenVerbForms ++ missingVerbForms //++ overriddenVerbForms

    new StrongVerb(verbClass, ablautGrades, verbForms)
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

			case ((INDICATIVE, Some(VerbTenseEnum.PRESENT), Some(Pronoun(GNumber.SINGULAR, _))), stem) =>
				StrongVerbStem.fromStrRepr(stem.stringForm(), verbClassEnum, EnumVerbStem.PRESENT_STEM, Some(Explicit_I_Umlaut))

			case (vt, stem) =>
				// do a stem -> string -> conversion to force validate the inputs
				//	TODO: is this double conversion really necessary?
				val stemType = verbs.stemFrom(vt._2, vt._3.map(_.number), vt._1)
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
