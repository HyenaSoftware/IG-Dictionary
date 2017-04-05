package com.hyenawarrior.OldNorseGrammar.grammar

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/

abstract class Stem(val root: Option[Root])
{
	override def toString = root.map(_.toString).getOrElse("Unknown")
}

