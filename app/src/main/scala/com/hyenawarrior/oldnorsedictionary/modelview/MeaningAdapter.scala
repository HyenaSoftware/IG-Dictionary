package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.TextView
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef



/**
	* Created by HyenaWarrior on 2016.10.26..
	*
	* + - - - -+ - - - - - +
	* | to fly | meaning1  |
	* |        | meaning2  |
	* + - - - -+ - - - - - +
	*/
class MeaningAdapter(activity: Activity, listView: ViewGroup)
	extends CustomAdapter[MeaningDef](activity, listView, R.layout.meanings) {

	protected  def resetView(i: Int, view: View): Unit = {

		val meaning = itemAt(i)

    val tvIndex = view.findViewById[TextView](R.id.tvIndex)
    tvIndex setText s"${i+1}."

		val tvMeaning = view.findViewById[TextView](R.id.tvDesc)
		tvMeaning.setText(meaning.meaning + " (" + meaning.note + ")")

		val tvExample = view.findViewById[TextView](R.id.tvMeaning)
		tvExample.setText(meaning.examples.mkString("\n"))
	}
}

