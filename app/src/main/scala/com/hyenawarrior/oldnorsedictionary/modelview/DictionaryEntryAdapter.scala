package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbModeEnum, VerbVoice}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{unapply => _}
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

		val item = itemAt(i)

		val tvDesc = view.findViewById(R.id.tvDesc).asInstanceOf[TextView]
		val text = s"[${item.posType}]"

		tvDesc setText text

		// set metadata
		view setTag item.posObj

		// set words
		val wordFormAdapter = new WordFormAdapter(activity)

		wordFormAdapter resetItems item.otherForms.toList

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

  def onClickOnAnEntry(view: View): Unit = {

    val llFormView = view.findViewById(R.id.llFormTable)

    llFormView.getVisibility match {
      case View.VISIBLE => llFormView.setVisibility(View.GONE)
      case View.GONE =>
        llFormView.setVisibility(View.VISIBLE)

        view.getTag match {

          case sv: StrongVerb => setDeclensionsTo(sv, view, VerbModeEnum.INDICATIVE, VerbVoice.ACTIVE)
        }
    }
  }
}
