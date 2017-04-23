package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{NonFinitiveVerbType, StrongVerb, VerbModeEnum, VerbTenseEnum}

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
trait VerbStemClass
{
	type FinitiveVerbDesc = (Pronoun, VerbModeEnum, VerbTenseEnum)

	def convertTo(verb: StrongVerb, targetForm: Either[FinitiveVerbDesc, NonFinitiveVerbType]): Option[StrongVerb]
}
