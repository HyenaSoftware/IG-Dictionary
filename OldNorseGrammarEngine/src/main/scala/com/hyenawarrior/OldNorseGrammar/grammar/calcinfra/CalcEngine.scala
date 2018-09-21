package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.{Calculator, NoOpCalculator, UnitCalculator}
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.levels.{InputLevel, IntermediateLevel, Level, StemLevel}

import scala.collection.Set

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
class CalcEngine[F](implicit noOpCalculator: NoOpCalculator[F], unitCalculator: UnitCalculator[F]) {

  def calculate(forms: Seq[CalcResult[String, F]], calculators: List[Calculator[F]], formsToCalcualte: Set[F]): Context[F] = {

    val context = calculateStem(forms, calculators)
    val splitContexts = split(context)

    val outputContexts = splitContexts.map(downCalc(_, calculators.reverse, formsToCalcualte))
    val outputContext = selector(outputContexts)

    outputContext
  }

  // FIXME: drop this dumb selector of 1st item, and return all of them
  private def selector(contexts: Seq[Context[F]]): Context[F] = contexts.head

  private def calculateStem(forms: Seq[CalcResult[String, F]], calculators: Seq[Calculator[F]]): Context[F] = {

    val inputStage = Stage[F](forms, noOpCalculator)

    def loop(sourceStage : Stage[F], levels: List[(Level, Calculator[F])]): List[(Level, Stage[F])] = levels match {
      case ((sourceLevel, calculator) :: (targetLevel2Calc @ (targetLevel, _)) :: others) =>

      val derivedCalcItems: Seq[CalcItem] = sourceStage.forms.flatMap {

        case srcCalcItem: CalcResult[String, F] => calcFor(srcCalcItem, calculator, sourceStage)
        case ce: CalcError => Seq(ce)
      }

      val mergedCalcItems = mergeCalcItems(derivedCalcItems, calculator, CALC_UP_TO_STEM)

      val targetStage = Stage[F](mergedCalcItems, calculator)
      targetLevel -> targetStage :: loop(targetStage, targetLevel2Calc :: others)

      case (lastLevel :: Nil) => Nil
    }

    val levelsToCalculators = (calculators.map(IntermediateLevel  ->_) :+ IntermediateLevel -> noOpCalculator).toList
    val levelToStages = InputLevel -> inputStage :: loop(inputStage, levelsToCalculators)

    // <editor-fold desc="print the stages">
    for((_, stage) <- levelToStages) {

      val items = stage.forms.map {
        case cr: CalcResult[String, F] => cr.data
        case ce: CalcError => ce.error
      }

      //println(items.mkString("; "))
    }
    // </editor-fold>

    Context(levelToStages)
  }

  private def calcFor(calcResult: CalcResult[String, F], calculator: Calculator[F], sourceStage : Stage[F]): Seq[CalcItem] = {

    calcResult.declensions.toSeq.flatMap(decl => {

      val result: Either[Seq[String], String] = calculator.reverseCompute(calcResult.data, decl, sourceStage)

      result match {

        case Left(sq) => sq.map(CalcResult[String, F](_, CALC_UP_TO_STEM, Set(calcResult), Set(decl), calculator))
        case Right(errorMessage) => Seq(CalcError(errorMessage, calcResult))
      }
    })
  }

  private def mergeCalcItems(calcItems: Seq[CalcItem], calculator: Calculator[F], calcDirection: CalcDirection): Seq[CalcItem] = {

    val calcItemsByType = calcItems.groupBy(_.getClass)
    val calcResults = calcItemsByType.get(classOf[CalcResult[String, F]]).map(_.asInstanceOf[Seq[CalcResult[String, F]]])
    val calcErrors = calcItemsByType.get(classOf[CalcError]).map(_.asInstanceOf[Seq[CalcError]])

    val mergedCalcResults = calcResults.map(cr => mergeCalcResults(cr, calculator, calcDirection)).getOrElse(Seq())

    mergedCalcResults ++ calcErrors.getOrElse(Seq())
  }

  private def mergeCalcResults(calcItems: Seq[CalcResult[String, F]], calculator: Calculator[F], calcDirection: CalcDirection): Seq[CalcResult[String, F]] = {

    val distinctCalcGroups = calcItems.groupBy(_.data)

    distinctCalcGroups.map { case (computedDerivedStr, derivedCalcItemsOfGroup) =>

      val allParent = derivedCalcItemsOfGroup.flatMap(_.parentCalcItems).toSet
      val allDecl = derivedCalcItemsOfGroup.flatMap(_.declensions).toSet

      CalcResult(computedDerivedStr, calcDirection, allParent, allDecl, calculator)

    }.toSeq
  }

  private def split(context: Context[F]): Seq[Context[F]] = {

    val (level, stage) :: _ = context.stages.reverse

    val newCtxs = for(form <- stage.forms) yield {

      val newStemStage = Stage[F](List(form), stage.calculator)

      val parentCalcResults = newStemStage.parentCalcResults

      if(parentCalcResults.isEmpty) None else {

        val tail = recCollect(parentCalcResults)
        val head = level -> newStemStage

        val stages = (head :: tail).reverse

        Some(Context(stages))
      }
    }

    newCtxs.flatten
  }

  private def recCollect(sourceCalcresults: Seq[CalcResult[String, F]]): List[(Level, Stage[F])] = {

    val newStageCalculator :: Nil = sourceCalcresults.map(pcr => pcr.calculator).distinct
    val newStage = Stage[F](sourceCalcresults, newStageCalculator)

    val parentCalcResults = sourceCalcresults
      .collect { case cr: CalcResult[String, F] => cr.parentCalcItems }
      .flatten

    if(parentCalcResults.isEmpty) {

      (InputLevel, newStage) :: List()

    } else {

      (IntermediateLevel, newStage) :: recCollect(parentCalcResults)

    }
  }

  private def downCalc(context: Context[F], calculators: List[Calculator[F]], FORMS_TO_CALCULATE: Set[F]): Context[F] = {

    val SUPPORTED_CALC_DIRECTION = Set(CALC_DOWN_FROM_STEM, NO_CALC_ON_STEM)
    //val FORMS_TO_CALCULATE = ALL_FORMS.filter(f => types.contains(f.adjType))

    def runCalculatorsFor(calculator: Calculator[F], sourceStage: Stage[F], missingDeclensions: Set[F]): Seq[CalcItem] = {
      sourceStage.forms.flatMap {

        case sourceCalcItem: CalcResult[String, F] if SUPPORTED_CALC_DIRECTION.contains(sourceCalcItem.calcDirection) =>

        val declensionsToCompute = missingDeclensions intersect sourceCalcItem.declensions

        // avoid to derivate a form from a different declension
        declensionsToCompute.flatMap(targetDecl => {
          calculator.compute(sourceCalcItem.data, targetDecl, sourceStage) match {
            case Left(Seq()) => Seq(CalcError(s"${calculator.shortCode} did not emit any output.", sourceCalcItem))
            case Left(sq) => sq.map(CalcResult[String, F](_, CALC_DOWN_FROM_STEM, Set(sourceCalcItem), Set(targetDecl), calculator))
            case Right(s) => Seq(CalcError(s, sourceCalcItem))
          }
        })
        case calcError: CalcError => Seq(calcError)
        case _ => Seq()
      }
    }

    def loop(calculators: List[Calculator[F]], stages: List[Stage[F]]): List[(Level, Stage[F])] = (calculators, stages) match {

      case (currentCalculator :: remainedTailCalculators, sourceStage :: targetStage :: furtherStages) =>

        // determine which declensions should be computed
      val alreadyExistingDeclensionsInTheTargetStage = targetStage.calcResults.flatMap(_.declensions)
      val missingDeclensions = FORMS_TO_CALCULATE -- alreadyExistingDeclensionsInTheTargetStage

      // do the calculations
      val derivedCalcItems = runCalculatorsFor(currentCalculator, sourceStage, missingDeclensions)

      // merge the results if for multiple input a calculator has the same output
      val mergedCalcItems = mergeCalcItems(derivedCalcItems, currentCalculator, CALC_DOWN_FROM_STEM) ++ targetStage.forms

      // create the stage object
      val enrichedTargetStage = Stage[F](mergedCalcItems, currentCalculator)

      // determine the correct level indicator
      val currentLevel = if(furtherStages == Nil) InputLevel else IntermediateLevel

      currentLevel -> enrichedTargetStage :: loop(remainedTailCalculators, enrichedTargetStage :: furtherStages)

      case (Nil, sourceStage :: Nil) => Nil
    }

    // from top-level calcresults, every other can inherit
    val (_, stage) = context.stages.head
    val distinctDeclensions = stage.calcResults.flatMap(_.declensions).distinct.size

    val extStages = extend(context.stages.map(_._2).reverse, distinctDeclensions, FORMS_TO_CALCULATE)

    val stages = (StemLevel -> extStages.head) :: loop(unitCalculator :: calculators, extStages)

    Context(stages)
  }

  private def extend(stages: List[Stage[F]], expectedCountOfDeclensions: Int, forms: Set[F]): List[Stage[F]] = {

    val topStage = stages.head

    val newCalcItems = topStage.calcResults.map {

      cr => CalcResult[String, F](cr.data, NO_CALC_ON_STEM, Set(cr), forms, noOpCalculator)
    }

    Stage[F](newCalcItems, noOpCalculator) :: stages
  }
}
