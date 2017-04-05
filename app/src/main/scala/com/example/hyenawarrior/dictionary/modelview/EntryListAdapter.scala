package com.example.hyenawarrior.dictionary.modelview

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget._
import com.example.hyenawarrior.dictionary.model.DictionaryEntry
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, DescriptorFlag, Number}

/**
	* Created by HyenaWarrior on 2017.03.09..
	*/
class EntryListAdapter(activity: Activity) extends CustomAdapter[DictionaryEntry](activity)
{
	def getNewView(i: Int, viewGroup: ViewGroup): View =
	{
		val view = inflater.inflate(R.layout.dictionary_row, null)
		val rootLayout = view.asInstanceOf[LinearLayout]

		val item = itemAt(i)

		val tvFoundWord = rootLayout.findViewById(R.id.tvFoundWord).asInstanceOf[TextView]
		val strForm = item.word.head.strForm
		tvFoundWord.setText(item.word.head.strForm)

		val tvRealWordDesc = rootLayout.findViewById(R.id.tvRealWordDesc).asInstanceOf[TextView]
		val wordTraits = formatTraits(item.word.head.traits)
		val dictWordRef = item.dictWord.map(w => s" -> ${w.strForm}").getOrElse("")
		val text = wordTraits + dictWordRef
		tvRealWordDesc setText text

		val ma = new MeaningAdapter(activity)

		//ma.reset(item.wordTo)

		Range(0, ma.getCount).foreach(i =>
			rootLayout.findViewById(R.id.rowDefList).asInstanceOf[LinearLayout].addView(ma.getView(i, null, null)))

		view
	}

	def formatTraits(trs: List[DescriptorFlag]): String = trs.map(t => shortCut(t)).mkString("[", ", ", "]")

	def shortCut(df: DescriptorFlag) = df match
	{
		case Case.NOMINATIVE => "nom"
		case Case.ACCUSATIVE => "acc"
		case Case.DATIVE => "dat"
		case Case.GENITIVE => "gen"
		case Number.SINGULAR => "sg"
		case Number.PLURAL => "pl"
	}
}
