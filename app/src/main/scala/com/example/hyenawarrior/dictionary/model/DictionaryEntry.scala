package com.example.hyenawarrior.dictionary.model

import com.hyenawarrior.OldNorseGrammar.grammar.{DescriptorFlag, Word}

/**
	* Created by HyenaWarrior on 2017.03.09..
	*/
case class DictionaryEntry(word: List[Word], dictWord: Option[Word], meanings: List[String])
