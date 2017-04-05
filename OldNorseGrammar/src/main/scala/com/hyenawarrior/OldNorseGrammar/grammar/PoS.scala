package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.03.20..
	*
	* Part of Speech - Szófaj
	*/
trait PoS
{
	def strForm: String

	def descriptorFlags: List[DescriptorFlag]
}
