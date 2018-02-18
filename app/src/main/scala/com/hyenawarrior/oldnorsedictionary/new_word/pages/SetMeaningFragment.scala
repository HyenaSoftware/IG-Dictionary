package com.hyenawarrior.oldnorsedictionary.new_word.pages

import android.os.Bundle
import android.support.v4.app.Fragment
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerb
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.INFINITIVE
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.ACTIVE
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
		private val hostView = rootView.findViewById(R.id.llMeanings).asInstanceOf[ViewGroup]

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
      case sv: StrongVerb => "verb" -> sv.verbClass.name
      case nn: Noun =>
          val optName = NounStemClassEnum.values.collectFirst { case NounStemClassEnum(n, _) => n }
          "noun" -> (optName getOrElse "???")

      case _ => "???" -> "???"
    }

    val tv_setmeaning_PosType = dataContext.rootView.findViewById(R.id.tv_setmeaning_PosType).asInstanceOf[TextView]
    tv_setmeaning_PosType.setText(posTypeName)

    val tv_setmeaning_ClassType = dataContext.rootView.findViewById(R.id.tv_setmeaning_ClassType).asInstanceOf[TextView]
    tv_setmeaning_ClassType.setText(posSubType)

    val word = optWordData match {

      case Some(WordData(sv: StrongVerb, _)) =>
        sv.verbForms.get((INFINITIVE, ACTIVE, None, None)).map(_.strForm).getOrElse("???")

      case Some(WordData(noun: Noun, _)) =>
        noun.nounForms.get(GNumber.SINGULAR -> Case.NOMINATIVE).map(_.strRepr).getOrElse("???")

      case _ => "???"
    }

    val tv_setmeaning_Word = dataContext.rootView.findViewById(R.id.tv_setmeaning_Word).asInstanceOf[TextView]
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
