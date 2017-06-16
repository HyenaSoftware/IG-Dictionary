package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
trait VerbStemClass[+V <: Verb]
{
	type FinitiveVerbDesc = (Pronoun, VerbModeEnum, VerbTenseEnum)

	def convertTo(verb: StrongVerb, targetForm: Either[FinitiveVerbDesc, NonFinitiveVerbType]): Option[V]
}
