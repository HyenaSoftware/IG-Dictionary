package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
object VerbModeEnum extends EnumLike[String, VerbModeEnum] {

	val INDICATIVE = VerbModeEnum("Indicative")
	val SUBJUNCTIVE = VerbModeEnum("Indicative")
}

case class VerbModeEnum(name: String) {

	VerbModeEnum.add(name -> this)
}