package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass

/**
	* Created by HyenaWarrior on 2017.03.20..
	*/
class Noun(str: String, meaningId: Int, val decl: (GNumber, Case), root: Root, stemClass: NounStemClass) extends Stem(root) with PoS
{
	// mostly for debug
	override def toString = s"$str (Noun) [${decl._1}, ${decl._2}] [root:${super.toString}]"

	// testing or searching, perhaps it will be deprecated
	override def strForm: String = str

	override def descriptorFlags = List(decl._1, decl._2)

	override def transformations = stemClass.transformationsFor(decl)
}
