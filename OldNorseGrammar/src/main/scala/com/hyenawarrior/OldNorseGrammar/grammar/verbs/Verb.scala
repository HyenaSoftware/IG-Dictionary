package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.{DescriptorFlag, PoS}

/**
	* Created by HyenaWarrior on 2017.03.21..
	*/
class Verb(val str: String, val meaningId: Int) extends PoS
{
	override def strForm = str

	override def descriptorFlags: List[DescriptorFlag] = List()
}
