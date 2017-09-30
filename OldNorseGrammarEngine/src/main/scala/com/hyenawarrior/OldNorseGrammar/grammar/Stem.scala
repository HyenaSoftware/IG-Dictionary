package com.hyenawarrior.OldNorseGrammar.grammar

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/

abstract class Stem(val root: Root)
{
	override def toString: String = root.toString
}

