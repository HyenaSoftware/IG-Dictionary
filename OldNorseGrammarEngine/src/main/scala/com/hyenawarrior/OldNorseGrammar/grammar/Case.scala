package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/
final case class Case(id: Int, name: String)
{
	Case.add(name -> this)

	override def toString = name
}

object Case extends EnumLike[String, Case]
{
	val NOMINATIVE = Case(0, "Nominative")
	val ACCUSATIVE = Case(1, "Accusative")
	val DATIVE = Case(2, "Dative")
	val GENITIVE = Case(3, "Genitive")

}