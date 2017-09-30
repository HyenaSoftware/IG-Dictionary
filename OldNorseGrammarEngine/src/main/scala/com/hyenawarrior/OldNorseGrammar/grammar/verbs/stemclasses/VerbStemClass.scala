package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.VerbStemClass.FinitiveVerbDesc

/**
	* Created by HyenaWarrior on 2017.04.19..
	*/
object VerbStemClass
{
	type FinitiveVerbDesc = (Pronoun, FinitiveMood, VerbTenseEnum)
}

trait VerbStemClass[+V <: Verb]
{
	def convertTo(verb: StrongVerb, verbClass: VerbClassEnum, targetForm: Either[FinitiveVerbDesc, NonFinitiveVerbType]): Option[V]
}
