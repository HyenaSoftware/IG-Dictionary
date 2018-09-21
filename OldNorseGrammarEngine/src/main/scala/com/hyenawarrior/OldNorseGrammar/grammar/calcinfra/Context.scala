package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra

import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.levels.Level

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
case class Context[F](stages: List[(Level, Stage[F])])
