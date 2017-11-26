package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget._
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.model.DictionaryListItem

/**
	* Created by HyenaWarrior on 2017.04.04..
	*/
class DictionaryEntryAdapter(activity: Activity) extends CustomAdapter[DictionaryListItem](activity)
{
	def getNewView(i: Int, viewGroup: ViewGroup): View =
	{
		val view = inflater.inflate(R.layout.dictionary_entry, null)

		// set metadata
		val item = itemAt(i)

		val tvDesc = view.findViewById(R.id.tvDesc).asInstanceOf[TextView]
		val text = s"[${item.posType}]"

		tvDesc setText text

		// set words
		val wordFormAdapter = new WordFormAdapter(activity)
		val forms = item.otherForms match {

			case Seq() => Seq(item.priForm)
			case list => list
		}

		wordFormAdapter resetItems forms.toList

		val llWordForms = view.findViewById(R.id.llWordForms).asInstanceOf[LinearLayout]
		extractViewsInto(wordFormAdapter, llWordForms)

		// set meanings
		val meaningAdapter = new MeaningAdapter(activity)
		meaningAdapter resetItems item.meanings

		val llMeanings = view.findViewById(R.id.llMeanings).asInstanceOf[LinearLayout]
		extractViewsInto(meaningAdapter, llMeanings)

		view
	}

  def extractViewsInto(adapter: Adapter, layout: LinearLayout): Unit =
	{
		Range(0, adapter.getCount)
			//.foreach(i => wordFormAdapter.getView(i, null, llWordForms))
			.map(i => adapter.getView(i, null, null))
			.foreach(v => layout.addView(v))
	}
}
