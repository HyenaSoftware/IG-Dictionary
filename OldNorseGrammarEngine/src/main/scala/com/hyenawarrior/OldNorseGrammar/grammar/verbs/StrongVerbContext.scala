package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun.{SG_1, SG_2, SG_3_FEMN}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.{INDICATIVE, SUBJUNCTIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.{Pronoun, Root, verbs}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{CommonStrongVerbStem, EnumVerbStem, StrongVerbStem, StrongVerbStemClass7th}

/**
	* Created by HyenaWarrior on 2017.10.08..
	*
	* TODO rename it to StrongVerb once StrongVerb and similar classes renamed to StrongVerbForm
	*
	*/

object VerbContext {

	//def product[T, U](seq: Seq[T], seq2: Seq[U]) = seq.flatMap(e => seq2.map(???))

	val ALL_VERB_FORMS: List[VerbType] = List(INDICATIVE, SUBJUNCTIVE)
		.flatMap(e => VerbTenseEnum.values.map(e -> Some(_)))
	  .flatMap(e => Seq(SG_1, SG_2, SG_3_FEMN).map(f => (e._1, e._2, Some(f))))
}

class StrongVerbContext(verbClass: StrongVerbClassEnum, verbForms: Map[VerbType, String]) {

	val ablautGrades: Map[EnumVerbStem, AblautGrade] = Map()
	val allVerbForms: Map[VerbType, StrongVerb] = complete(verbForms)

	private def complete(givenVerbForms: Map[VerbType, String]): Map[VerbType, StrongVerb] = {

		// exclude the base form definition and the overrides
		val missingDeclensions = VerbContext.ALL_VERB_FORMS.filterNot(givenVerbForms.contains)

		try {

			val givenVerbs: Map[VerbType, StrongVerb] = givenVerbForms.map {

				case (vt, rawStr) => vt -> StrongVerb.fromStringRepr(rawStr, verbClass, vt)
			}

			val stems = extractStems(givenVerbs.map { case (k, v) => k -> v.getStem() }, verbClass)

			// determine missing stems from existing ones
			val missingVerbs: Map[VerbType, StrongVerb] = missingDeclensions
				.map {
					case vt @ (mood, optTense, optPronoun) =>
						val expectedStemType = verbs.stemFrom(optTense, optPronoun.map(_.number), mood)
						vt -> StrongVerb.verbFrom(stems(expectedStemType), vt)
				}
				.toMap

			val forms = if (missingVerbs.isEmpty) Map[VerbType, StrongVerb]() else givenVerbs ++ missingVerbs

			forms
		}
		catch {

			case e: RuntimeException =>
				// TODO: use some kind of "LOG-context" here
				//val msg = e.getMessage
				//android.util.Log.w(AddNewVerbHelper.getClass.getSimpleName, msg)
			Map()
		}
	}

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

		// determine the present stem
		//	a) from the present stem
		//	b) from the perfect stem
		//	c) from the either past stem (are these the same always?)
		val presentStem =	pseudoStemsBy.getOrElse(PRESENT_STEM,
			pseudoStemsBy.getOrElse(PERFECT_STEM,
				pseudoStemsBy.getOrElse(PRETERITE_SINGULAR_STEM,
					pseudoStemsBy(PRETERITE_PLURAL_STEM))))

		// build a root from the present stem
		val CommonStrongVerbStem(root, _, _) = presentStem

		// now based on StrongVerbClass choose either of:
		// build stems from the root

		val stemMap: Map[EnumVerbStem, CommonStrongVerbStem] = if(verbClassEnum != VerbClassEnum.STRONG_7TH_CLASS) {

			// 2a) *** Class 1st-6th Specific ***
			val preteriteSgStem = StrongVerbStem(root, verbClassEnum, PRETERITE_SINGULAR_STEM)
			val preteritePlStem = StrongVerbStem(root, verbClassEnum, PRETERITE_PLURAL_STEM)
			val perfectStem = StrongVerbStem(root, verbClassEnum, PERFECT_STEM)

			Map(PRESENT_STEM -> presentStem,
				PRETERITE_SINGULAR_STEM -> preteriteSgStem,
				PRETERITE_PLURAL_STEM -> preteritePlStem,
				PERFECT_STEM -> perfectStem)

		} else {

			// 2b) *** Class 7th Specific ***
			Map(PRESENT_STEM -> presentStem) ++ extractStemsOfClass7thVerbs(pseudoStemsBy, root)
		}

		stemMap
	}

	private def extractStemsOfClass7thVerbs(pseudoStemsBy: Map[EnumVerbStem, CommonStrongVerbStem], root: Root): Map[EnumVerbStem, StrongVerbStemClass7th] = {
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

		val perfectStem = StrongVerbStemClass7th(root, PERFECT_STEM, perfectStAbGr)


		//
		val preteriteSgStAbGr = pseudoStemsBy.getOrElse(PRETERITE_SINGULAR_STEM,
			pseudoStemsBy.getOrElse(PRETERITE_PLURAL_STEM,
				pseudoStemsBy.getOrElse(PERFECT_STEM,
					pseudoStemsBy(PRESENT_STEM)))).getAblautGrade()

		val preteriteSgStem = StrongVerbStemClass7th(root, PRETERITE_SINGULAR_STEM, preteriteSgStAbGr)

		//
		val preteritePlStAbGr = pseudoStemsBy.getOrElse(PRETERITE_SINGULAR_STEM,
			pseudoStemsBy.getOrElse(PRETERITE_PLURAL_STEM,
				pseudoStemsBy.getOrElse(PERFECT_STEM,
					pseudoStemsBy(PRESENT_STEM)))).getAblautGrade()

		val preteritePlStem = StrongVerbStemClass7th(root, PRETERITE_SINGULAR_STEM, preteritePlStAbGr)

		Map(PRETERITE_SINGULAR_STEM -> preteriteSgStem,
			PRETERITE_PLURAL_STEM -> preteritePlStem,
			PERFECT_STEM -> perfectStem)
	}

}
