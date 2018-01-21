package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
object VerbModeEnum extends EnumLike[Int, VerbModeEnum] {

	val INDICATIVE	=	FinitiveMood("Indicative", 0)
	val SUBJUNCTIVE	= FinitiveMood("Subjunctive", 1)
	val IMPERATIVE	= FinitiveMood("Imperative", 2)
	val INFINITIVE 	= NonFinitiveMood("Infinitive", 3)
	val PARTICIPLE 	= NonFinitiveMood("Participle", 4)
}

class VerbModeEnum(name: String, id: Int) extends Serializable {

	VerbModeEnum.add(id -> this)
}

case class FinitiveMood(name: String, id: Int) 		extends VerbModeEnum(name, id)
case class NonFinitiveMood(name: String, id: Int) extends VerbModeEnum(name, id)
