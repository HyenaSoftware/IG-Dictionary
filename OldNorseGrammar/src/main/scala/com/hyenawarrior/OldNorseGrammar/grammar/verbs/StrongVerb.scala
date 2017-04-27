package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
abstract class StrongVerb(str: String, val verbClass: VerbClassEnum) extends Verb(str)

case class FinitiveStrongVerb(str: String, override val verbClass: VerbClassEnum, pronoun: Pronoun, tense: VerbTenseEnum) extends StrongVerb(str, verbClass)

case class NonFinitiveStrongVerb(str: String, override val verbClass: VerbClassEnum, nonFinitiveVerbType: NonFinitiveVerbType) extends StrongVerb(str, verbClass)
