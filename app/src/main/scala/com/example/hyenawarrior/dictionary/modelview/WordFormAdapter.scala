package com.example.hyenawarrior.dictionary.modelview

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.TextView
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar._

/**
	* Created by HyenaWarrior on 2017.04.04..
	*/
class WordFormAdapter(activity: Activity) extends CustomAdapter[Word](activity)
{
	override def getNewView(i: Int, viewGroup: ViewGroup): View =
	{
		val view = inflater.inflate(R.layout.word_form_entry, viewGroup)
		//val rootLayout = view.asInstanceOf[GridLayout]

		val item = itemAt(i)

		val tvWordForm = view.findViewById(R.id.tvWordForm).asInstanceOf[TextView]
		val tvWordDesc = view.findViewById(R.id.tvWordDesc).asInstanceOf[TextView]

		tvWordForm setText item.strForm
		tvWordDesc setText formatTraits(item.traits)

		view
	}

	def formatTraits(trs: List[DescriptorFlag]): String = trs.map(t => shortCut(t)).mkString("[", ", ", "]")

	def shortCut(df: DescriptorFlag) = df match
	{
		case Case.NOMINATIVE => "nom"
		case Case.ACCUSATIVE => "acc"
		case Case.DATIVE => "dat"
		case Case.GENITIVE => "gen"
		case GNumber.SINGULAR => "sg"
		case GNumber.PLURAL => "pl"
	}
}
