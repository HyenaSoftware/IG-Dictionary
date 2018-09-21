package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
object GeminationCalculator extends Calculator[AdjectiveFormType] {

  // TODO: move these to a common place
  private val LV = "(?:[áæéíóœúý]|oe|ae|au|ey|ei)"
  private val SV = "[aeioøǫuy]"
  private val C = "[bdðfghjklmnprstvxzþ]"
  private val pattern = s"^($C*$LV)([tr])(?!\\2)(.*)$$".r
  private val revPattern = s"^($C*$LV)([tr])\\2(.*)$$".r

  override def compute(str: String, declension: AdjectiveFormType, stage: Stage[AdjectiveFormType]): Either[Seq[String], String] = {

    //val inflection = AdjectivalOperations.inflectionWithComparsionFor(declension)
    //val adjustedInflection = Syncope.adjustSyncopatedInflection(inflection)

    //val (word, _) = ProductiveTransforms.Gemination(str, adjustedInflection)
    val rs = str match {

      case pattern(p1, c, p2) => p1 + (c*2) + p2
      case _ => str
    }

    Left(Seq(rs))
  }

  override def reverseCompute(str: String, declension: AdjectiveFormType, stage: Stage[AdjectiveFormType]): Either[Seq[String], String] = {

    //val inflection = AdjectivalOperations.inflectionWithComparsionFor(declension)
    //val adjustedInflection = Syncope.adjustSyncopatedInflection(inflection)

    val str2 = revPattern.replaceSomeIn(str, m => {

      val rng = 1 to m.groupCount

      Some(rng.map(m.group).mkString)
    })

    Left(Seq(str2))
  }

  override def shortCode: String = "GM"
}