package com.example.hyenawarrior.dictionary.modelview.meaning_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.{EditText, TextView}
import com.example.hyenawarrior.dictionary.modelview.{DynamicListView, EditTextTypeListener}
import com.example.hyenawarrior.myapplication.R

/**
	* Created by HyenaWarrior on 2017.07.24..
	*/
class ExampleRecordView(activity: Activity, hostView: ViewGroup) extends DynamicListView[String](hostView, R.layout.example_record, activity)
{
	private var examples: Map[View, String] = Map()

	ensureToHaveExtraRecord()

	override protected def applyToView(optElem: Option[String], recordView: View): Unit =
	{
		val elem = optElem getOrElse ""

		val idx = examples.size + 1
		val etExample = recordView.findViewById(R.id.et_setmeaning_Example).asInstanceOf[EditText]
		etExample addTextChangedListener new EditTextTypeListener(onMeaningChange(recordView))
		etExample.setText(elem, TextView.BufferType.EDITABLE)

		val tvIndex = recordView.findViewById(R.id.tv_setmeaning_Example_Index).asInstanceOf[TextView]
		tvIndex setText s"$idx"
	}

	private def onMeaningChange(recordView: View)(text: String): Unit =
	{
		examples = (examples - recordView) + ((recordView, text))

		ensureToHaveExtraRecord()
	}


	private def ensureToHaveExtraRecord(): Unit =
	{
		val countOfEmptyRecords = examples.collect { case (_, str) if str.isEmpty => 1 }.sum

		countOfEmptyRecords match
		{
			case 0 => add("")
			case n if n > 1 =>
				for (v <- examples.collectFirst	{ case (v, str) if str.isEmpty => v })
				{
					examples = examples - v
					remove(v)
				}

			case _ => ()
		}
	}

	def fetch(): Seq[String] = examples.values.filter(_.nonEmpty).toSeq
}
