package com.example.hyenawarrior.dictionary.model

import com.hyenawarrior.OldNorseGrammar.grammar.{Database, Language}
import com.hyenawarrior.dictionaryLoader.Storage

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
object Database
{
	object Storage extends Storage with AndroidStorage

	val database =
	{
		val langs = Storage.availableLanguages

		new Database(langs.map(e => Language(e) -> Storage.meanings(e)).toMap)
	}
}
