package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/

case class Gender(id: Int)
{
	Gender.add(id -> this)
}

object Gender extends EnumLike[Int, Gender]
{
	val MASCULINE = Gender(0)
	val FEMININE = Gender(1)
	val NEUTER = Gender(2)
}