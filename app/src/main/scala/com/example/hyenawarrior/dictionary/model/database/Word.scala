package com.example.hyenawarrior.dictionary.model.database

import com.example.hyenawarrior.dictionary.model.database.marshallers.PosType

/**
	* Created by HyenaWarrior on 2017.06.07..
	*/
case class Word(wordId: Int, meaningId: Int, pos: PosType) extends dbrecord

