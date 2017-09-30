package com.hyenawarrior.oldnorsedictionary.model

import com.hyenawarrior.OldNorseGrammar.grammar.Word
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef

/**
	* Created by HyenaWarrior on 2017.03.09..
	*/
case class DictionaryEntry(word: List[Word], dictWord: Option[Word], meanings: List[MeaningDef])
