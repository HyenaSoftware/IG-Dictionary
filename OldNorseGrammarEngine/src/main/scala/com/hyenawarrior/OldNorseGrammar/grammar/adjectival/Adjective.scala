package com.hyenawarrior.OldNorseGrammar.grammar.adjectival

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.GenericCalculator
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.{CalcEngine, CalcResult, Stage}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.NOMINATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender.MASCULINE
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber, Gender}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.ProductiveTransforms.FormExtender
import com.hyenawarrior.OldNorseGrammar.grammar.nominal._
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.MorphemeProperty
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.PhonemeProperty._
import com.hyenawarrior.OldNorseGrammar.grammar.{Pos, phonology}

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

    stemsToForms match {

      case (Some(stem), forms) =>
        val missingAdjForms = forms.map(f => f.declension -> f).toMap
        new Adjective(stem, givenAdjForms, missingAdjForms, Map())

      case (None, _) => throw new RuntimeException("Unable to infer stem")
    }
  }

  private def run(givenAdjForms: Seq[AdjectiveForm], types: Set[AdjectiveType]): (Option[AdjectiveStem], Seq[AdjectiveForm]) = {

    import com.hyenawarrior.OldNorseGrammar.grammar.nominal.helpers._

    val calcinfra = new CalcEngine[phonology.Word, AdjectiveFormType]()

    //
    val formsToCalculate = ALL_FORMS.filter(f => types.contains(f.adjType))
    val forms = givenAdjForms.map(f => CalcResult.from(phonology.Word(f.strRepr, MorphemeProperty.Stem), f.declension))
    val missingDeclensions = formsToCalculate -- forms.flatMap(_.declensions)

    val calculators = List[GenericCalculator[phonology.Word, AdjectiveFormType]](
      //
      InflectionWordCalculator,
      ConsonantWordAssimilationCalculator,
      SyncopeWordCalculator,
      SemivowelWordDeletionCalculator,
      UmlautWordCalculator,
      DropInflectionCalculator,
      StemReduceCalculator,
      FormExtender(missingDeclensions)
    )

    val outputContext2 = calcinfra.calculate(forms, calculators, formsToCalculate)

    //
    val optIOStage: Option[Stage[phonology.Word, AdjectiveFormType]] = outputContext2.stages.lastOption
    val adjForms = optIOStage match {

      case Some(stage) => stage.calcResults.flatMap(cr => cr.declensions.map(decl => {

          val strRepr = cr.data.asString {

            case _: DisappearingPhoneme => false
            case _ => true
          }

          AdjectiveForm(strRepr, decl)
        }))
      case None => Seq()
    }

    val optAdjStems2 = outputContext2.stem match {

      case cr: CalcResult[phonology.Word, AdjectiveFormType] => cr

        val stemMorpheme = cr.data.selectMorpheme(MorphemeProperty.Stem)
        Some(AdjectiveStem(stemMorpheme.map(_.asString).getOrElse("")))
    }

    optAdjStems2 -> adjForms
  }
}