package com.example.hyenawarrior.dictionary.model.database

import com.example.hyenawarrior.dictionary.model.database.marshallers.{PosForm, PosType}

/**
	* Created by HyenaWarrior on 2017.06.07..
	*/
case class WordForm(form: String, wordId: Int, posForm: PosForm, posType: PosType) extends dbrecord
