package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.{ShortCoded, Stage}

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
trait Calculator[F] extends ShortCoded {

  def compute(str: String, declension: F, stage: Stage[F]): Either[Seq[String], String]

  def reverseCompute(str: String, declension: F, stage: Stage[F]): Either[Seq[String], String]
}
