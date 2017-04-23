package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
case class VerbTenseEnum(name: String)
{
	VerbTenseEnum.add(name -> this)
}

object VerbTenseEnum extends EnumLike[VerbTenseEnum]
{
	val PRESENT = VerbTenseEnum("Present")
	val PAST = VerbTenseEnum("Past")
	val PERFECT = VerbTenseEnum("Perfect")
}
