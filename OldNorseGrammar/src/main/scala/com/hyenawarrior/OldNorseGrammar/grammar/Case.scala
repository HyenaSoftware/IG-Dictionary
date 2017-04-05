package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/
final case class Case(name: String) extends DescriptorFlag
{
	Case.add(name -> this)

	override def toString = name
}

object Case extends EnumLike[Case]
{
	val NOMINATIVE = Case("Nominative")
	val ACCUSATIVE = Case("Accusative")
	val DATIVE = Case("Dative")
	val GENITIVE = Case("Genitive")

}