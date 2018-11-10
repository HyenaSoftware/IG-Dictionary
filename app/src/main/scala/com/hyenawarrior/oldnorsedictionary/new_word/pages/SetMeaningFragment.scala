package com.hyenawarrior.oldnorsedictionary.new_word.pages

import android.os.Bundle
import android.support.v4.app.Fragment
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget._
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.Adjective
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.Verb
import com.hyenawarrior.OldNorseGrammar.grammar.{PoSForm, Pos}
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.model.DictionaryEntry
import com.hyenawarrior.oldnorsedictionary.model.database.IGPersister
import com.hyenawarrior.oldnorsedictionary.modelview.meaning_panel.MeaningDefListView

/**
	* Created by HyenaWarrior on 2017.04.27..
	*/
object SetMeaningFragment extends Fragment
{
  lazy val igPersister = new IGPersister(getContext)
  var optWordData: Option[WordData] = None

  class DataContext(val rootView: View)
	{
		private val hostView = rootView.findViewById[ViewGroup](R.id.llMeanings)

		val meaningDefListView = new MeaningDefListView(getActivity, hostView)
	}

  var dataContext: DataContext = _

	override def onCreateView(inflater: LayoutInflater, container: ViewGroup, savedInstanceState: Bundle): View =
  {
		val rootView = inflater.inflate(R.layout.add_meaning_for_new_word, container, false)

    dataContext = new DataContext(rootView)

		rootView
	}

	def setWordData(wordData: WordData): Unit =
  {
    optWordData = Option(wordData)

    val (posTypeName, posSubType) = wordData.word match
    {
      case v: Verb   => "verb" -> v.verbClass.toString
      case n: Noun   => "noun" -> n.stem.stemClass.toString
      case a: Adjective => "adjective" -> ""
      case _ => "???" -> "???"
    }

    val tv_setmeaning_PosType = dataContext.rootView.findViewById[TextView](R.id.tv_setmeaning_PosType)
    tv_setmeaning_PosType.setText(posTypeName)

    val tv_setmeaning_ClassType = dataContext.rootView.findViewById[TextView](R.id.tv_setmeaning_ClassType)
    tv_setmeaning_ClassType.setText(posSubType)

    val word = optWordData match {

      case Some(WordData(obj: Pos[Any, PoSForm], _)) =>
        obj.forms.get(obj.PRIMARY_KEY).map(_.strRepr).getOrElse("???")

      case _ => "???"
    }

    val tv_setmeaning_Word = dataContext.rootView.findViewById[TextView](R.id.tv_setmeaning_Word)
    tv_setmeaning_Word.setText(word)
  }

  def saveDefinitionInto(): Unit = optWordData match
  {

    case Some(WordData(word, _)) =>
      val meaning = dataContext.meaningDefListView.fetch()
      igPersister.save(DictionaryEntry(word, meaning))

    case None => ()
  }

	def onNewMeaning(view: View): Unit =
	{
		dataContext.meaningDefListView.add()
	}
}
