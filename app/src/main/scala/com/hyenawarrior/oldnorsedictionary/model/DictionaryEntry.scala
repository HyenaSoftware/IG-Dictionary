package com.hyenawarrior.oldnorsedictionary.model

import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef

/**
	* Created by HyenaWarrior on 2017.03.09..
	*/
case class DictionaryEntry(word: AnyRef, meanings: List[MeaningDef])
