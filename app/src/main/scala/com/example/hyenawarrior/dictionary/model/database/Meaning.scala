package com.example.hyenawarrior.dictionary.model.database

/**
	* Created by HyenaWarrior on 2017.06.07..
	*/
case class Meaning(wordId: Int, meaningId: Int, langId: Int, context: String, exampleId: Int) extends dbrecord
