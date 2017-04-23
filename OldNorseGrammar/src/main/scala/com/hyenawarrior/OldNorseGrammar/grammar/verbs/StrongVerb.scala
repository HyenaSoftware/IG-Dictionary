package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
case class StrongVerb(str: String, verbClass: VerbClassEnum, pronoun: Pronoun, tense: VerbTenseEnum) extends Verb(str, verbClass, pronoun, tense)
