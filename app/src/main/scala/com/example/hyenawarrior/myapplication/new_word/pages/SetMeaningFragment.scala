package com.example.hyenawarrior.myapplication.new_word.pages

import android.os.Bundle
import android.support.v4.app.Fragment
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.TextView
import com.example.hyenawarrior.dictionary.model.database.IGDatabase
import com.example.hyenawarrior.myapplication.R
import com.example.hyenawarrior.dictionary.model.{database => orm}
import com.hyenawarrior.OldNorseGrammar.grammar.Case.NOMINATIVE
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.SINGULAR
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.INFINITIVE
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{NonFinitiveStrongVerb, NonFinitiveVerbType}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.04.27..
	*/
object SetMeaningFragment extends Fragment
{
  lazy val igDatabase = IGDatabase(getContext)
  var optWordData: Option[WordData] = None

  class DataContext(val rootView: View)

  var dataContext: DataContext = null

	override def onCreateView(inflater: LayoutInflater, container: ViewGroup, savedInstanceState: Bundle): View =
  {
		val rootView = inflater.inflate(R.layout.add_meaning_for_new_word, container, false)

    dataContext = new DataContext(rootView)

		rootView
	}

  def setWordData(wordData: WordData): Unit =
  {
    optWordData = Option(wordData)

    val (posTypeName, className) = wordData match
    {
      case VerbData(verbClass, _) => ("verb", verbClass.name)
      case NounData(nounClass, _) => ("noun", nounClass.name)
      case _ => ("???", "???")
    }

    val tv_setmeaning_PosType = dataContext.rootView.findViewById(R.id.tv_setmeaning_PosType).asInstanceOf[TextView]
    tv_setmeaning_PosType.setText(posTypeName)

    val tv_setmeaning_ClassType = dataContext.rootView.findViewById(R.id.tv_setmeaning_ClassType).asInstanceOf[TextView]
    tv_setmeaning_ClassType.setText(className)

    val word = optWordData match
    {
      case Some(NounData(_, map)) => map.getOrElse(SINGULAR -> NOMINATIVE, "???")
      case Some(VerbData(_, map)) => map.getOrElse(Right(INFINITIVE), "???")
      case _ => "???"
    }

    val tv_setmeaning_Word = dataContext.rootView.findViewById(R.id.tv_setmeaning_Word).asInstanceOf[TextView]
    tv_setmeaning_Word.setText(word)
  }

  def saveDefinitionInto(): Unit = optWordData match
  {
    case Some(data) => igDatabase.save(data)
    case None => ()
  }
}
