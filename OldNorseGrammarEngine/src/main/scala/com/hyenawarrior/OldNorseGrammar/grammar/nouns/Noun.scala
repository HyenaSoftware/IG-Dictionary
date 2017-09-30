package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass

/**
	* Created by HyenaWarrior on 2017.03.20..
	*/
case class Noun(str: String, meaningId: Int, decl: (GNumber, Case), override val root: Root, stemClass: NounStemClass) extends Stem(root) with PoS
{
	// mostly for debug
	override def toString = s"$str (Noun) [${decl._1}, ${decl._2}] [root:${super.toString}]"

	override def strForm: String =
	{
		val Syllables(syllables) = str

		val transformedSyllables = transformations.foldLeft(syllables){ (sys, trn) => trn(sys) }

		Syllables(transformedSyllables)
	}

	override def descriptorFlags = List(decl._1, decl._2)

	override def transformations = super.transformations ++ stemClass.transformationsFor(decl)
}
