package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum.{unapply => _}
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.model.DictionaryListItem

/**
	* Created by HyenaWarrior on 2017.04.04..
	*/
class DictionaryEntryAdapter(activity: Activity, listView: ViewGroup)
	extends CustomAdapter[DictionaryListItem](activity, listView, R.layout.dictionary_entry)
{
	protected def resetView(i: Int, view: View): Unit = {

		val item = itemAt(i)

		val tvDesc = view.findViewById(R.id.tvDesc).asInstanceOf[TextView]
		val text = s"[${item.posType}]"

		tvDesc setText text

		// set metadata
		view setTag item

		// set words
		val llWordForms = view.findViewById(R.id.llWordForms).asInstanceOf[LinearLayout]

		val wordFormAdapter = new WordFormAdapter(activity, llWordForms)

		wordFormAdapter resetItems item.otherForms.toList

		// set meanings
		val llMeanings = view.findViewById(R.id.llMeanings).asInstanceOf[LinearLayout]

		val meaningAdapter = new MeaningAdapter(activity, llMeanings)

		meaningAdapter resetItems item.meanings
	}
}
