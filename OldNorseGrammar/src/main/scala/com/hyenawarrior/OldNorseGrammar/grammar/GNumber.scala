package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/
final case class GNumber(str: String) extends DescriptorFlag
{
	GNumber.add(str -> this)

	override def toString = str
}

object GNumber extends EnumLike[String, GNumber]
{
	val SINGULAR = GNumber("Singular")
	val DUAL = GNumber("Dual")
	val PLURAL = GNumber("Plural")

	def conventionalValues = List(SINGULAR, PLURAL)
}
