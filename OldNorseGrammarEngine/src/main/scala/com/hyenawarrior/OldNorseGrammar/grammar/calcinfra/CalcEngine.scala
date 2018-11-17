package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators._

import scala.collection.Set

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
class CalcEngine[D, F](implicit noOpCalculator: NoOpCalculator[D, F]) {

  def calculate(forms: Seq[CalcResult[D, F]], calculators: List[GenericCalculator[D, F]], formsToCalcualte: Set[F]): Context[D, F] = {

    val inputStage = Stage[D, F](forms, noOpCalculator)

    // 1st step: reduce the individual forms to stem by going through the calculators
    val stages = calcThrough(inputStage, calculators, CALC_UP_TO_STEM)

    val setOfStages = split(stages)

    // 2nd step: generating individual forms from the stem by going through the calculators in reversed order
    val outputContexts = setOfStages.map { firstStages =>

      val allStages = firstStages ++ calcThrough(firstStages.last, calculators.reverse, CALC_DOWN_FROM_STEM)

      val lastStage = Stage(allStages.last.forms ++ forms, noOpCalculator)

      Context(allStages :+ lastStage, firstStages.last.forms.head)
    }


    outputContexts.head
  }




  private def calcThrough(stage: Stage[D, F], calculators: List[GenericCalculator[D, F]], calcDirection: CalcDirection): List[Stage[D, F]] = calculators match {

    case (currentCalculator :: otherCalculators) =>

      val targetStage = currentCalculator match {

        case calculator: Calculator[D, F] =>
          val derivedCalcItems = calcFor(calculator, stage, calcDirection)

          Stage[D, F](derivedCalcItems, calculator)

        case calculator: StageCalculator[D, F] =>

          val result = calcDirection match {
            case CALC_UP_TO_STEM      => calculator.reverseCompute(stage)
            case CALC_DOWN_FROM_STEM  => calculator.compute(stage)
          }

          result match {

            case Left(dstStage) => dstStage
            case Right(message) => Stage[D, F](Seq(), calculator)
          }
      }

      targetStage :: calcThrough(targetStage, otherCalculators, calcDirection)

    case List() => List()
  }

  private def calcFor(calculator: Calculator[D, F], stage : Stage[D, F], calcDirection: CalcDirection): Seq[CalcItem] = {

    stage.forms.flatMap {

      case calcResult: CalcResult[D, F] =>
        calcResult.declensions.toSeq.flatMap(decl => {

          val result = calcDirection match {
            case CALC_UP_TO_STEM      => calculator.reverseCompute(calcResult.data, decl, stage)
            case CALC_DOWN_FROM_STEM  => calculator.compute(calcResult.data, decl, stage)
          }

          result match {

            case Left(sq) => sq.map(CalcResult[D, F](_, calcDirection, Set(calcResult), Set(decl), calculator))
            case Right(errorMessage) => Seq(CalcError(errorMessage, calcResult))
          }
        })

      case ce: CalcError => Seq(ce)
    }
  }

  private def split(stages: List[Stage[D, F]]): Seq[List[Stage[D, F]]] = {

    val inputStage :: _ = stages

    val stage :: _ = stages.reverse

    val newCtxs = for(form <- stage.forms) yield {

      val newStemStage = Stage[D, F](List(form), stage.calculator)

      val parentCalcResults = newStemStage.parentCalcResults

      if(parentCalcResults.isEmpty) None else {

        val tail = recCollect(parentCalcResults.toSet)

        val stages = (newStemStage :: tail).reverse

        val newInputStage = stages.head
        val isComplete = inputStage.forms.size == newInputStage.forms.size

        // prohibit to emit incomplete stages
        if(isComplete) {

          Some(stages)

        } else None
      }
    }

    newCtxs.flatten
  }

  private def recCollect(sourceCalcResults: Set[CalcResult[D, F]]): List[Stage[D, F]] = {

    val newStageCalculator :: Nil = sourceCalcResults.map(pcr => pcr.calculator).toList
    val newStage = Stage[D, F](sourceCalcResults.toSeq, newStageCalculator)

    val parentCalcResults = sourceCalcResults
      .collect { case cr: CalcResult[D, F] => cr.parentCalcItems }
      .flatten

    newStage :: (if(parentCalcResults.isEmpty) {

      List()

    } else {

      recCollect(parentCalcResults)

    })
  }
}
