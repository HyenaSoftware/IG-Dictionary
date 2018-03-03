package com.hyenawarrior.OldNorseGrammar.grammar

/**
  * Created by HyenaWarrior on 2018.02.26..
  */
trait Pos[K, F <: PoSForm] {

  val forms: Map[K, F]

  val PRIMARY_KEY: K
}