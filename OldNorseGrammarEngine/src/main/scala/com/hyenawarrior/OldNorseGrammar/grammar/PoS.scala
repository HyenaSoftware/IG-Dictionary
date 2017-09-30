package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{I_Umlaut, U_Umlaut, WordTransformation}

/**
	* Created by HyenaWarrior on 2017.03.20..
	*
	* Part of Speech - Szófaj
	*/
trait PoS
{
	def strForm: String

	def descriptorFlags: List[DescriptorFlag]

	def transformations: List[WordTransformation] = List(U_Umlaut)
}
