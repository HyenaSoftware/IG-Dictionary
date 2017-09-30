package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem

import com.hyenawarrior.OldNorseGrammar.grammar.Root

/**
	* Created by HyenaWarrior on 2017.04.22..
	*/
class VerbStem(root: Root, val stemType: VerbStemEnum)

case class StrongVerbStem(stemedRoot: Root, override val stemType: VerbStemEnum = VerbStemEnum.PRESENT_STEM) extends VerbStem(stemedRoot, stemType)
{
	def stringForm: String = stemedRoot.word
}

