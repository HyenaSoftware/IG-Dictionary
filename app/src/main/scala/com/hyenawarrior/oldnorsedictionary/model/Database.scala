package com.hyenawarrior.oldnorsedictionary.model

import com.hyenawarrior.OldNorseGrammar.grammar.{Language, Database => UDatabase}
import com.hyenawarrior.dictionaryLoader.Storage


/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object Database
{
	object Storage extends Storage with AndroidStorage

	val database: UDatabase =
	{
		val langs = Storage.availableLanguages

		new UDatabase(langs.map(e => Language(e) -> Storage.meanings(e)).toMap)

	}
}
