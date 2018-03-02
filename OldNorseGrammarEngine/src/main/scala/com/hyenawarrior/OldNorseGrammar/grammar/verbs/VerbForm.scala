package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.PoSForm

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
abstract class VerbForm(rawForm: String) extends PoSForm
{
	override def strForm: String = rawForm

	override def toString: String = strForm
}
