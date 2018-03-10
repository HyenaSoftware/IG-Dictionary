package com.hyenawarrior.oldnorsedictionary.modelview.meaning_panel

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget.{EditText, TextView}
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.modelview.{ClickListener, DynamicListView, EditTextTypeListener}
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef

/**
	* Created by HyenaWarrior on 2017.07.22..
	*/
class MeaningDefListView(activity: Activity, hostView: ViewGroup)
	extends DynamicListView[MeaningDef](hostView, R.layout.meaning_record, activity)
{
	private var meanings: Map[View, (String, String, ExampleRecordView)] = Map()

	override protected def applyToView(optElem: Option[MeaningDef], meaningRecordView: View): Unit =
	{
		val elem = optElem getOrElse MeaningDef("", "", Seq())

		val btnRemove = meaningRecordView.findViewById[View](R.id.ibRemove)
		btnRemove setOnClickListener ClickListener(onRemoveMeaningDef(meaningRecordView))

		val etMeaning = meaningRecordView.findViewById[EditText](R.id.et_setmeaning_Desc)
		etMeaning.setText(elem.meaning, TextView.BufferType.EDITABLE)
		etMeaning addTextChangedListener new EditTextTypeListener(onMeaningChange(meaningRecordView))

		val etNote = meaningRecordView.findViewById[EditText](R.id.et_setmeaning_Note)
		etNote.setText(elem.note, TextView.BufferType.EDITABLE)
		etNote addTextChangedListener new EditTextTypeListener(onNoteChange(meaningRecordView))

		val subControlOfHost = meaningRecordView.findViewById[ViewGroup](R.id.tl_setmeaning_Examples)
		val exRecHandler = new ExampleRecordView(activity, subControlOfHost)
	 	for(exStr <- elem.examples)
		{
			exRecHandler add exStr
		}

		meanings = meanings + ((meaningRecordView, (elem.meaning, elem.note, exRecHandler)))
	}

	private def onRemoveMeaningDef(meaningRecordView: View)(): Unit =
	{
		meanings = meanings - meaningRecordView
		remove(meaningRecordView)
	}

	private def onMeaningChange(meaningRecordView: View)(text: String): Unit =
	{
		val item = meanings(meaningRecordView)
		val newEntry = (meaningRecordView, (text, item._2, item._3))

		meanings = (meanings - meaningRecordView) + newEntry
	}

	private def onNoteChange(meaningRecordView: View)(text: String): Unit =
	{
		val item = meanings(meaningRecordView)
		val newEntry = (meaningRecordView, (item._1, text, item._3))

		meanings = (meanings - meaningRecordView) + newEntry
	}

	def fetch(): List[MeaningDef] = meanings.values.map
	{
		case (meaning, note, exs) => MeaningDef(meaning, note, exs.fetch())
	}
	  .toList
}
