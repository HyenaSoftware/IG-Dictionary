package com.hyenawarrior.oldnorsedictionary.model.database

import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}
import com.hyenawarrior.oldnorsedictionary.model.database.marshallers.{NounForm, PosForm, PosType, VerbForm}

/**
	* Created by HyenaWarrior on 2017.06.07..
	*/
case class WordForm(form: String, wordId: Int, posForm: PosForm, posType: PosType) extends dbrecord
{
	def isPrimaryForm: Boolean = posForm match
	{
		case NounForm(_, GNumber.SINGULAR, Case.NOMINATIVE) => true
		case VerbForm(_, (VerbModeEnum.INFINITIVE, _, _)) => true
		case _ => false
	}
}
