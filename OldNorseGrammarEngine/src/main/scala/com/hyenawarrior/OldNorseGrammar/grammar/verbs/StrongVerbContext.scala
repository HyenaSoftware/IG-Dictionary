package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.{INDICATIVE, INFINITIVE, PARTICIPLE, SUBJUNCTIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{PAST, PRESENT}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{CommonStrongVerbStem, EnumVerbStem, StrongVerbStem, StrongVerbStemClass7th}
import com.hyenawarrior.OldNorseGrammar.grammar.{Pronoun, Root, verbs}

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

case class StrongVerbContext(verbClass: StrongVerbClassEnum, ablautGrade: Map[EnumVerbStem, AblautGrade]
                             , verbForms: Map[VerbType, StrongVerb])

object StrongVerbContext {

  def apply(verbClass: StrongVerbClassEnum, givenVerbFormStrings: Map[VerbType, String]): StrongVerbContext = {

    val givenVerbForms: Map[VerbType, StrongVerb] = givenVerbFormStrings.map {

      case (vt, rawStr) => vt -> StrongVerb.fromStringRepr(rawStr, verbClass, vt)
    }

    val stems = extractStems(givenVerbForms.map { case (k, v) => k -> v.getStem() }, verbClass)

    val ablautGrades = stems.map{ case (k, v) => k -> v.getAblautGrade() }

    // exclude the base form definition and the overrides
    val missingDeclensions = VerbContext.ALL_VERB_FORMS.filterNot(givenVerbForms.contains)

    val missingVerbForms = missingDeclensions
      .map {
        case vt@(mood, optTense, optPronoun) =>
          val expectedStemType = verbs.stemFrom(optTense, optPronoun.map(_.number), mood)
          vt -> StrongVerb.verbFrom(stems(expectedStemType), vt)
      }
      .toMap

    val verbForms = givenVerbForms ++ missingVerbForms //++ overriddenVerbForms

    new StrongVerbContext(verbClass, ablautGrades, verbForms)
  }

  private val STEM_PREFERENCES = Map(
    PRESENT_STEM ->             Seq(PRESENT_STEM, PERFECT_STEM, PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM),
    PRETERITE_SINGULAR_STEM ->  Seq(PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM, PERFECT_STEM, PRESENT_STEM),
    PRETERITE_PLURAL_STEM   ->  Seq(PRETERITE_PLURAL_STEM, PRETERITE_SINGULAR_STEM, PERFECT_STEM, PRESENT_STEM),
    PERFECT_STEM ->             Seq(PERFECT_STEM, PRESENT_STEM, PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM)
  )

	private def extractStems(pseudoVerbForms: Map[VerbType, CommonStrongVerbStem], verbClassEnum: StrongVerbClassEnum):
		Map[EnumVerbStem, CommonStrongVerbStem] = {

		// collect what we have
		val pseudoFormsToStem: Map[EnumVerbStem, Iterable[CommonStrongVerbStem]] = pseudoVerbForms.map {

			case (vt, stem) =>
				// do a stem -> string -> conversion to force validate the inputs
				//	TODO: is this double conversion really necessary?
				val stemType = verbs.stemFrom(vt._2, vt._3.map(_.number), vt._1)
				StrongVerbStem.fromStrRepr(stem.stringForm(), verbClassEnum, stemType)
		}
			.groupBy ( _.getStemType() )

		val pseudoStemsBy: Map[EnumVerbStem, CommonStrongVerbStem] = pseudoFormsToStem.map { case(e, m) => e -> m.head }

		// 1) *** GENERIC *** (1-7)
		// now based on StrongVerbClass choose either of:
		// build stems from the root

		val stemMap: Map[EnumVerbStem, CommonStrongVerbStem] = if(verbClassEnum != VerbClassEnum.STRONG_7TH_CLASS) {

      // recreate the present stem
			// 2a) *** Class 1st-6th Specific ***
      STEM_PREFERENCES.flatMap { case (k, v) => getOrCreateStem(v, pseudoStemsBy, verbClassEnum).map(k -> _) }

		} else {

      // determine the present stem
      //	a) from the present stem
      //	b) from the perfect stem
      //	c) from the either past stem (are these the same always?)
      val pseudoPresentStem =	pseudoStemsBy.getOrElse(PRESENT_STEM,
        pseudoStemsBy.getOrElse(PERFECT_STEM,
          pseudoStemsBy.getOrElse(PRETERITE_SINGULAR_STEM,
            pseudoStemsBy(PRETERITE_PLURAL_STEM))))


      // build a root from the present stem
      val CommonStrongVerbStem(root, _, _) = pseudoPresentStem

      val ablautGrade = pseudoPresentStem.getAblautGrade()

      // recreate the present stem
      val presentStem = StrongVerbStemClass7th(root.word, PRESENT_STEM, ablautGrade)

			// 2b) *** Class 7th Specific ***
			Map(PRESENT_STEM -> presentStem) ++ extractStemsOfClass7thVerbs(pseudoStemsBy, root)
		}

		stemMap
	}

  private def getOrCreateStem(preferences: Seq[EnumVerbStem], pseudoStemsBy: Map[EnumVerbStem, CommonStrongVerbStem]
                              , verbClassEnum: StrongVerbClassEnum): Option[CommonStrongVerbStem] = {

    val primaryStem = preferences.head
    val secondaryStems = preferences.tail

    val optTransformedClosestStem = pseudoStemsBy.get(primaryStem).orElse(secondaryStems.collectFirst {

      case verbStem if pseudoStemsBy.contains(verbStem) =>
        val srcStem = pseudoStemsBy(verbStem)
        StrongVerbStem.fromRoot(srcStem.getRoot(), verbClassEnum, preferences.head)
    })

    optTransformedClosestStem
  }

	private def extractStemsOfClass7thVerbs(pseudoStemsBy: Map[EnumVerbStem, CommonStrongVerbStem], root: Root)
  : Map[EnumVerbStem, StrongVerbStemClass7th] = {

		// determine the other (missing) stems:
		//  a) if the perfect stem is missing than that from the present or past stem
		//	b) if the past tenses missing then
		// 		- from the other past tense
		// 		-	or from the perfect stem
		//		- from the present stem
		val perfectStAbGr = pseudoStemsBy.getOrElse(PERFECT_STEM,
			pseudoStemsBy.getOrElse(PRESENT_STEM,
				pseudoStemsBy.getOrElse(PRETERITE_SINGULAR_STEM,
					pseudoStemsBy(PRETERITE_PLURAL_STEM))))
			.getAblautGrade()

		val perfectStem = StrongVerbStemClass7th(root.word, PERFECT_STEM, perfectStAbGr)

		//
		val preteriteSgStAbGr = pseudoStemsBy.getOrElse(PRETERITE_SINGULAR_STEM,
			pseudoStemsBy.getOrElse(PRETERITE_PLURAL_STEM,
				pseudoStemsBy.getOrElse(PERFECT_STEM,
					pseudoStemsBy(PRESENT_STEM)))).getAblautGrade()

		val preteriteSgStem = StrongVerbStemClass7th(root.word, PRETERITE_SINGULAR_STEM, preteriteSgStAbGr)

		//
		val preteritePlStAbGr = pseudoStemsBy.getOrElse(PRETERITE_SINGULAR_STEM,
			pseudoStemsBy.getOrElse(PRETERITE_PLURAL_STEM,
				pseudoStemsBy.getOrElse(PERFECT_STEM,
					pseudoStemsBy(PRESENT_STEM)))).getAblautGrade()

		val preteritePlStem = StrongVerbStemClass7th(root.word, PRETERITE_SINGULAR_STEM, preteritePlStAbGr)

		Map(PRETERITE_SINGULAR_STEM -> preteriteSgStem,
			PRETERITE_PLURAL_STEM -> preteritePlStem,
			PERFECT_STEM -> perfectStem)
	}

}
