package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/
final case class Number(str: String) extends DescriptorFlag
{
	Number.add(str -> this)

	override def toString = str
}

object Number extends EnumLike[Number]
{
	val SINGULAR = Number("Singular")
	val DUAL = Number("Dual")
	val PLURAL = Number("Plural")

	def conventionalValues = List(SINGULAR, PLURAL)
}
