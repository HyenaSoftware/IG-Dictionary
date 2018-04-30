package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.Pos

/**
  * Created by HyenaWarrior on 2018.04.25..
  */
trait Verb extends Pos[VerbType, VerbForm] {

  override val forms: Map[VerbType, VerbForm]
}