package com.hyenawarrior.OldNorseGrammar.grammar.adjectival

import com.hyenawarrior.OldNorseGrammar.grammar.Pos
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.{CalcEngine, CalcResult, Stage}
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.NOMINATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender.MASCULINE
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber, Gender}
import com.hyenawarrior.OldNorseGrammar.grammar.nominal._

import scala.collection.Set

/**
  * Created by HyenaWarrior on 2018.06.11..
  */
case class Adjective(stem: AdjectiveStem, givenForms: Map[AdjectiveFormType, AdjectiveForm]
                     , generatedForms: Map[AdjectiveFormType, AdjectiveForm]
                     , overridenForms: Map[AdjectiveFormType, AdjectiveForm]) extends Pos[AdjectiveFormType, AdjectiveForm] {

  private lazy val adjectiveForms = givenForms ++ generatedForms ++ overridenForms

  override val forms: Map[AdjectiveFormType, AdjectiveForm] = adjectiveForms
  override val PRIMARY_KEY: AdjectiveFormType = (POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE)
}

object Adjective {

  val ALL_FORMS: Set[AdjectiveFormType] = Seq(POSITIVE_INDEFINITE ,POSITIVE_DEFINITE, COMPARATIVE, SUPERLATIVE_INDEFINITE, SUPERLATIVE_DEFINITE)
    .flatMap(t => GNumber.conventionalValues
      .flatMap(n => Gender.values
        .flatMap(g => Case.values.map(c => core.fromTuple(t, n, g, c)))))
    .toSet

  /**
    * Generate the missing forms by the given forms to complete the adjective
    *
    * @param givenForms Seq of highly reduced forms, which are identical to stem string representations
    * @return
    */
  def from(givenForms: Map[AdjectiveFormType, String], forTypes: Set[AdjectiveType] = AdjectiveType.values.toSet): Adjective = {

    if(givenForms.isEmpty) {

      throw new RuntimeException("There was no forms given.")
    }

    val givenAdjForms = givenForms.map { case (f, s) => f -> AdjectiveForm(s, f) }

    val stemsToForms = run(givenAdjForms.values.toSeq, forTypes)


    val (stem, forms) = stemsToForms
    val missingAdjForms = forms.map(f => f.declension -> f).toMap

    new Adjective(stem, givenAdjForms, missingAdjForms, Map())
  }

  private def run(givenAdjForms: Seq[AdjectiveForm], types: Set[AdjectiveType]): (AdjectiveStem, Seq[AdjectiveForm]) = {

    val calculators = List[Calculator[String, AdjectiveFormType]](
      // form
      ConsonantAssimilationCalculator,
      GeminationCalculator,
      // restoring the unstressed vowel might split the geminated consonants
      // BackwardInflectionCalculator,
      SyncopeCalculator,
      SemivowelDeletionCalculator,
      InflectionCalculator,
      // ForwardInflectionCalculator,
      UmlautCalculator
      // stem
    )

    import com.hyenawarrior.OldNorseGrammar.grammar.nominal.helpers._

    val forms: Seq[CalcResult[String, AdjectiveFormType]] = givenAdjForms.map(f => CalcResult.from(f.strRepr, f.declension))

    val calcinfra = new CalcEngine[String, AdjectiveFormType]()

    val formsToCalculate = ALL_FORMS.filter(f => types.contains(f.adjType))
    val outputContext = calcinfra.calculate(forms, calculators, formsToCalculate)

    //
    val optIOStage: Option[Stage[String, AdjectiveFormType]] = outputContext.stages.lastOption.map(_._2)
    val adjForms = optIOStage match {

      case Some(stage) => stage.calcResults.flatMap(cr => cr.declensions.map(decl => AdjectiveForm(cr.data, decl)))
      case None => Seq()
    }

    //
    val topStage = outputContext.stages.head._2
    val adjStems = {

      val cr = topStage.calcResults.head
      AdjectiveStem(cr.data)
    }

    adjStems -> adjForms
  }
}