package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.TextView
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.model.DictionaryListItem

/**
	* Created by HyenaWarrior on 2017.04.04..
	*/
class WordFormAdapter(activity: Activity, listView: ViewGroup)
	extends CustomAdapter[DictionaryListItem.DescedString](activity, listView, R.layout.word_form_entry)
{
	protected def resetView(i: Int, item: DictionaryListItem.DescedString, view: View): Unit = {

		val (word, desc) = item

		val tvWordForm = view.findViewById[TextView](R.id.tvWordForm)
		val tvWordDesc = view.findViewById[TextView](R.id.tvWordDesc)

		tvWordForm setText word
		tvWordDesc setText desc
	}
}
