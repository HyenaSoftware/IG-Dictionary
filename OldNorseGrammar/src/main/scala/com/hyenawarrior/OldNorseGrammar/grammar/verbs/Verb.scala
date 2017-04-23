package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.{DescriptorFlag, GNumber, PoS, Pronoun}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
abstract class Verb(str: String, verbClass: VerbClassEnum, pronoun: Pronoun, tense: VerbTenseEnum) extends PoS
{
	override def strForm: String = str

	override def descriptorFlags: List[DescriptorFlag] = List()
}
