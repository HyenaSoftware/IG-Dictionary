package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
object VerbModeEnum extends EnumLike[Int, (String, VerbModeEnum)] {

  val INDICATIVE = VerbModeEnum("Indicative", 0)
  val SUBJUNCTIVE = VerbModeEnum("Subjunctive", 1)
  val IMPERATIVE = VerbModeEnum("Imperative", 2)
  val INFINITIVE = VerbModeEnum("Infinitive", 3)
  val PARTICIPLE = VerbModeEnum("Participle", 4)
}

case class VerbModeEnum(name: String, id: Int) {

	VerbModeEnum.add(id -> (name -> this))
}