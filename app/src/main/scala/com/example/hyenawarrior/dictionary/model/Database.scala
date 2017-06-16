package com.example.hyenawarrior.dictionary.model

import com.hyenawarrior.OldNorseGrammar.grammar.{Database => UDatabase, Language}
import com.hyenawarrior.dictionaryLoader.Storage


import android.database.sqlite.SQLiteDatabase
import android.database.sqlite.SQLiteOpenHelper


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
