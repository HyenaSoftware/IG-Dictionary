package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number, PoS, Root, Stem}

/**
	* Created by HyenaWarrior on 2017.03.20..
	*/
class Noun(str: String, meaningId: Int, val decl: (Number, Case), root: Option[Root]) extends Stem(root) with PoS
{
	// mostly for debug
	override def toString = s"$str (Noun) [${decl._1}, ${decl._2}] [root:${super.toString}]"

	// testing or searching, perhaps it will be deprecated
	override def strForm = str

	override def descriptorFlags = List(decl._1, decl._2)
}
