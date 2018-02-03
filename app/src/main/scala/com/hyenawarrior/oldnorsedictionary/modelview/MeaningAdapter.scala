package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.view.View
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
class MeaningAdapter(activity: Activity) extends CustomAdapter[MeaningDef](activity, R.layout.meanings)
{
	protected  def resetView(i: Int, view: View): Unit = {

		val meaning = itemAt(i)

    val tvIndex = view.findViewById(R.id.tvIndex).asInstanceOf[TextView]
    tvIndex setText s"$i."

		val tvMeaning = view.findViewById(R.id.tvDesc).asInstanceOf[TextView]
		tvMeaning.setText(meaning.meaning + " (" + meaning.note + ")")

		val tvExample = view.findViewById(R.id.tvMeaning).asInstanceOf[TextView]
		tvExample.setText(meaning.examples.mkString("\n"))
	}
}

