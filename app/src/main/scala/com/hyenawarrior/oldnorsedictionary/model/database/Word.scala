package com.hyenawarrior.oldnorsedictionary.model.database

import com.hyenawarrior.oldnorsedictionary.model.database.marshallers.PosType

/**
	* Created by HyenaWarrior on 2017.06.07..
	*/
case class Word(wordId: Int, meaningId: Int, pos: PosType) extends dbrecord

