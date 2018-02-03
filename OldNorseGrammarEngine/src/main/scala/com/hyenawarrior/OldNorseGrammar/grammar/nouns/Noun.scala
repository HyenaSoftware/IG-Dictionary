package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass

/**
	* Created by HyenaWarrior on 2017.03.20..
	*/
class Noun(stemClass: NounStemClass, givenForms: Map[NounType, NounForm]
					 , generatedForms: Map[NounType, NounForm]
					 , overridenForms: Map[NounType, NounForm]) {

	lazy val nounForms = givenForms ++ generatedForms ++ overridenForms

 }


object Noun {

	val ALL_FORMS = GNumber.conventionalValues.flatMap(n => Case.values.map(c => n -> c)).toSet

	def apply(stemClass: NounStemClass, givenForms: Map[NounType, String]): Noun = {

		val givenNounForms = givenForms.map {

			case (nt, f) => nt -> NounForm.fromStringRepr(f, stemClass, nt)
		}

		val missingDeclensions = ALL_FORMS -- givenForms.keys
		val nounStem = extractPrimaryStem(stemClass, givenForms)

		val missingForms = missingDeclensions.map { nt =>

			nt -> NounForm.fromStringRepr(nounStem, nt)

		}.toMap

		new Noun(stemClass, givenNounForms, missingForms, Map())
	}

	private def extractPrimaryStem(stemClass: NounStemClass, givenForms: Map[NounType, String]): NounStem = {

		val (decl, str) = givenForms.head

		val form = NounForm.fromStringRepr(str, stemClass, decl)

		form.stem
	}
}