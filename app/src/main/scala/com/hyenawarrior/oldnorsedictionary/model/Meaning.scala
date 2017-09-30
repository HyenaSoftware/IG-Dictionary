package com.hyenawarrior.oldnorsedictionary.model

import com.hyenawarrior.OldNorseGrammar.grammar.Word

/**
	* Created by HyenaWarrior on 2017.03.09..
	*/
case class Meaning
(
	word: String,
	wordType: String
)

object Meaning
{
	def apply(word: Word): Meaning = Meaning(word.strForm(), word.description)
}