package com.example.hyenawarrior.dictionary.modelview

import android.app.Activity
import android.view.ViewGroup
import android.widget.{LinearLayout, TextView}
import com.example.hyenawarrior.dictionary.model.Meaning
import com.example.hyenawarrior.myapplication.R



/**
	* Created by HyenaWarrior on 2016.10.26..
	*
	* + - - - -+ - - - - - +
	* | to fly | meaning1  |
	* |        | meaning2  |
	* + - - - -+ - - - - - +
	*/
class MeaningAdapter(activity: Activity) extends CustomAdapter[String](activity)
{
	def getNewView(i: Int, viewGroup: ViewGroup) =
	{
		val view = inflater.inflate(R.layout.meanings, null)

		//val linLayout = view.asInstanceOf[LinearLayout]

		val meaning = itemAt(i)

		val textView = view.findViewById(R.id.tvMeaning).asInstanceOf[TextView]

		textView.setText(meaning)

		view
	}
}

