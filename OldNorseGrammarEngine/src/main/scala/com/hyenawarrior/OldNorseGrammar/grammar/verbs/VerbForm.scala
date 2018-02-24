package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.PoS

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
abstract class VerbForm(rawForm: String) extends PoS
{
	override def strForm: String = rawForm

	override def toString: String = strForm
}
