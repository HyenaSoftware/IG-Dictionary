package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.Pos
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass

/**
  * Created by HyenaWarrior on 2017.03.20..
  */
case class Noun(stem: NounStem, givenForms: Map[NounFormType, NounForm]
                , generatedForms: Map[NounFormType, NounForm]
                , overridenForms: Map[NounFormType, NounForm]) extends Pos[NounFormType, NounForm] {

  lazy val nounForms = givenForms ++ generatedForms ++ overridenForms

  override val forms: Map[NounFormType, NounForm] = nounForms

  override val PRIMARY_KEY = (SINGULAR, NOMINATIVE) -> false
}


object Noun {

  val ALL_FORMS = Seq(false, true)
    .flatMap(d => GNumber.conventionalValues
    .flatMap(n => Case.values
    .map(c => (n, c) -> d))).toSet

  def apply(stemClass: NounStemClass, givenForms: Map[NounFormType, String]): Noun = {

    if(givenForms.isEmpty) {

      throw new RuntimeException("There was no forms given.")
    }

    val givenNounForms = givenForms.map {

      case (k @ (declension, isDef), strRepr) => k -> NounForm(strRepr, declension, isDef)
    }

    val missingDeclensions = ALL_FORMS -- givenForms.keys
    val nounStem = NounStem.from(givenNounForms.values.head, stemClass)

    val missingForms = missingDeclensions.map {

      case k @ (declension, isDef) => k -> NounForm.fromStem(nounStem, declension, isDef)

    }.toMap

    new Noun(nounStem, givenNounForms, missingForms, Map())
  }
}
