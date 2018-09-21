package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
abstract class CalcDirection(val shortCode: String)
object CALC_UP_TO_STEM extends CalcDirection("-")
object CALC_DOWN_FROM_STEM extends CalcDirection("+")
object NO_CALC_ON_STEM extends CalcDirection("*")
object NO_CALC_ON_INPUT extends CalcDirection("*")