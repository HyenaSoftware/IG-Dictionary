package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar._

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
trait NounStemClass {

	def thematicVowel: Option[String] = None

	def inflection(decl: (GNumber, Case)): String
}




