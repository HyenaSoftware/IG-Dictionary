package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.TextView
import com.hyenawarrior.OldNorseGrammar.grammar._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.model.DictionaryListItem

/**
	* Created by HyenaWarrior on 2017.04.04..
	*/
class WordFormAdapter(activity: Activity, listView: ViewGroup)
	extends CustomAdapter[DictionaryListItem.DescedString](activity, listView, R.layout.word_form_entry)
{
	protected def resetView(i: Int, view: View): Unit = {

		val (word, desc) = itemAt(i)

		val tvWordForm = view.findViewById(R.id.tvWordForm).asInstanceOf[TextView]
		val tvWordDesc = view.findViewById(R.id.tvWordDesc).asInstanceOf[TextView]

		tvWordForm setText word
		tvWordDesc setText desc
	}

	def formatTraits(trs: List[DescriptorFlag]): String = trs.map(t => shortCut(t)).mkString("[", ", ", "]")

	def shortCut(df: Any) = df match
	{
		case Case.NOMINATIVE => "nom"
		case Case.ACCUSATIVE => "acc"
		case Case.DATIVE => "dat"
		case Case.GENITIVE => "gen"
		case GNumber.SINGULAR => "sg"
		case GNumber.PLURAL => "pl"
		case NonFinitiveVerbType.INFINITIVE => "inf"
		case NonFinitiveVerbType.PRESENT_PARTICIPLE => "present part."
		case NonFinitiveVerbType.PAST_PARTICIPLE => "past part."
	}
}
