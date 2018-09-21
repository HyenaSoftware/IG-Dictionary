package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
trait NoOpCalculator[T] extends Calculator[T] {

  override def compute(str: String, declension: T, stage: Stage[T]) = Left(Seq())

  override def reverseCompute(str: String, declension: T, stage: Stage[T]) = Left(Seq())

  override def shortCode: String = "NOP"
}