package com.example.hyenawarrior.dictionary.modelview

import com.hyenawarrior.OldNorseGrammar.grammar.{Case, DescriptorFlag, GNumber}

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object GrammarHelpers
{
	def shortCut(df: DescriptorFlag) = df match
	{
		case Case.NOMINATIVE => "nom"
		case Case.ACCUSATIVE => "acc"
		case Case.DATIVE => "dat"
		case Case.GENITIVE => "gen"
		case GNumber.SINGULAR => "sg"
		case GNumber.PLURAL => "pl"
	}
}
