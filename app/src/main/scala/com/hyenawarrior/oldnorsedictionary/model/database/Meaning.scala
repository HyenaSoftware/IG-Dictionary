package com.hyenawarrior.oldnorsedictionary.model.database

/**
	* Created by HyenaWarrior on 2017.06.07..
	*/
case class Meaning(meaningId: Int, langId: Int, meaning: String, note: String, exampleId: Option[Int]) extends dbrecord