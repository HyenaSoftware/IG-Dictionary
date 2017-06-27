package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.{DescriptorFlag, PoS, Syllables}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
abstract class Verb(str: String) extends PoS
{
	override def strForm: String =
	{
		val Syllables(syllables) = str

		val transformedSyllables = transformations.foldLeft(syllables){ (sys, trn) => trn(sys) }

		Syllables(transformedSyllables)
	}

	override def descriptorFlags: List[DescriptorFlag] = List()
}
