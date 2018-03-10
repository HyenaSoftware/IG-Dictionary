package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.Pos
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass

/**
  * Created by HyenaWarrior on 2017.03.20..
  */
case class Noun(stem: NounStem, givenForms: Map[NounType, NounForm]
                , generatedForms: Map[NounType, NounForm]
                , overridenForms: Map[NounType, NounForm]) extends Pos[NounType, NounForm] {

  lazy val nounForms = givenForms ++ generatedForms ++ overridenForms

  override val forms: Map[(GNumber, Case), NounForm] = nounForms

  override val PRIMARY_KEY = (SINGULAR, NOMINATIVE)
}


object Noun {

  val ALL_FORMS = GNumber.conventionalValues.flatMap(n => Case.values.map(c => n -> c)).toSet

  def apply(stemClass: NounStemClass, givenForms: Map[NounType, String]): Noun = {

    if(givenForms.isEmpty) {

      throw new RuntimeException("There was no forms given.")
    }

    val givenNounForms = givenForms.map {

      case (declension, strRepr) => declension -> NounForm(strRepr, declension)
    }

    val missingDeclensions = ALL_FORMS -- givenForms.keys
    val nounStem = NounStem.from(givenNounForms.values.head, stemClass)

    val missingForms = missingDeclensions.map { declension =>

      declension -> NounForm.fromStem(nounStem, declension)

    }.toMap

    new Noun(nounStem, givenNounForms, missingForms, Map())
  }
}
