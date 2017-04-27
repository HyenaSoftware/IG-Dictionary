package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.{DescriptorFlag, PoS}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
abstract class Verb(str: String) extends PoS
{
	override def strForm: String = str

	override def descriptorFlags: List[DescriptorFlag] = List()
}
