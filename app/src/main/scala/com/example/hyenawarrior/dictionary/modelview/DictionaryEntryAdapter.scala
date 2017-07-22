package com.example.hyenawarrior.dictionary.modelview

import android.app.Activity
import android.view.{View, ViewGroup}
import android.widget._
import com.example.hyenawarrior.dictionary.model.DictionaryEntry
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar.Word
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.Verb


case class MetaData(words: List[String])


/**
	* Created by HyenaWarrior on 2017.04.04..
	*/
class DictionaryEntryAdapter(activity: Activity) extends CustomAdapter[DictionaryEntry](activity)
{
	def getNewView(i: Int, viewGroup: ViewGroup): View =
	{
		val view = inflater.inflate(R.layout.dictionary_entry, null)

		// set metadata
		val item = itemAt(i)
		view.setTag(MetaData(item.word.map(_.toString)))

		val tvDesc = view.findViewById(R.id.tvDesc).asInstanceOf[TextView]
		val dictWordRef = item.dictWord.map(w => s" -> ${w.strForm()}").getOrElse("")
		val text = s"[${posTypeOf(item)}] $dictWordRef"

		tvDesc setText text

		// set words
		val wordFormAdapter = new WordFormAdapter(activity)
		wordFormAdapter resetItems item.word

		val llWordForms = view.findViewById(R.id.llWordForms).asInstanceOf[LinearLayout]
		extractViewsInto(wordFormAdapter, llWordForms)

		// set meanings
		val meaningAdapter = new MeaningAdapter(activity)
		meaningAdapter resetItems item.meanings

		val llMeanings = view.findViewById(R.id.llMeanings).asInstanceOf[LinearLayout]
		extractViewsInto(meaningAdapter, llMeanings)

		view
	}

	private def posTypeOf(de: DictionaryEntry): String =
	{
		val types = de.word.map
		{
			case Word(_: Verb) => "verb"
			case Word(_: Noun) => "noun"
			case _ => "???"
		}.toSet

		if(types.size == 1) types.head else "mixed"
	}

	def extractViewsInto(adapter: Adapter, layout: LinearLayout): Unit =
	{
		Range(0, adapter.getCount)
			//.foreach(i => wordFormAdapter.getView(i, null, llWordForms))
			.map(i => adapter.getView(i, null, null))
			.foreach(v => layout.addView(v))
	}
}
