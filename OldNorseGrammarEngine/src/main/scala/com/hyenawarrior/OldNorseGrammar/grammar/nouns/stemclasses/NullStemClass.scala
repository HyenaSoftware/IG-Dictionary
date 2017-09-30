package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object NullStemClass extends NounStemClass
{
	override protected def inflection(decl: (GNumber, Case)): String = ""
}