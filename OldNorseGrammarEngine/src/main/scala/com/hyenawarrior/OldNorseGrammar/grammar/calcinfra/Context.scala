package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
case class Context[D, F](stages: List[Stage[D, F]], stem: CalcItem)
