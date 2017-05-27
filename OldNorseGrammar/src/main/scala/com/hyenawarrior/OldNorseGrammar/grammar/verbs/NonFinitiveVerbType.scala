package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
case class NonFinitiveVerbType(name: String) {

	NonFinitiveVerbType.add(name -> this)
}

object NonFinitiveVerbType extends EnumLike[String, NonFinitiveVerbType] {

	val INFINITIVE = NonFinitiveVerbType("Infinitive")
	val PRESENT_PARTICIPLE = NonFinitiveVerbType("Present Participle")
	val PAST_PARTICIPLE = NonFinitiveVerbType("Past Participle")
}