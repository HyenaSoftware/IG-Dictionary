package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.{DescriptorFlag, PoS, Syllables}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
abstract class Verb(rawForm: String) extends PoS
{
	override def strForm: String = rawForm

	override def descriptorFlags: List[DescriptorFlag] = List()

	override def toString: String = strForm
}
