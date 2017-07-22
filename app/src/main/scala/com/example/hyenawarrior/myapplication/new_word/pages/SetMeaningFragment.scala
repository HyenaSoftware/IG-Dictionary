package com.example.hyenawarrior.myapplication.new_word.pages

import android.os.Bundle
import android.support.v4.app.Fragment
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.{EditText, TextView}
import com.example.hyenawarrior.dictionary.model.database.IGDatabase
import com.example.hyenawarrior.dictionary.model.database.marshallers.{NounForm, NounType, VerbForm, VerbType}
import com.example.hyenawarrior.dictionary.modelview.EditTextTypeListener
import com.example.hyenawarrior.myapplication.R

/**
	* Created by HyenaWarrior on 2017.04.27..
	*/
object SetMeaningFragment extends Fragment
{
  lazy val igDatabase = IGDatabase(getContext)
  var optWordData: Option[WordData] = None
  var meanings: List[MeaningDef] = List(MeaningDef("", List()))

  class DataContext(val rootView: View)

  var dataContext: DataContext = null

	override def onCreateView(inflater: LayoutInflater, container: ViewGroup, savedInstanceState: Bundle): View =
  {
		val rootView = inflater.inflate(R.layout.add_meaning_for_new_word, container, false)

    installTypeListener(rootView, R.id.et_setmeaning_Context, onChangeContext)
		installTypeListener(rootView, R.id.et_setmeaning_Meaning, onChangeMeaning)

    dataContext = new DataContext(rootView)

		rootView
	}

	private def installTypeListener(rootView: View, id: Int, callback: (String) => Unit): Unit = rootView
		.findViewById(id)
		.asInstanceOf[EditText]
		.addTextChangedListener(new EditTextTypeListener(callback))

	private def onChangeContext(desc: String) = meanings = meanings match
	{
		case List(MeaningDef(_, ex)) => List(MeaningDef(desc, ex))
	}

	private def onChangeMeaning(str: String) = meanings = meanings match
	{
		case List(MeaningDef(desc, _)) => List(MeaningDef(desc, Seq(str)))
	}

  def setWordData(wordData: WordData): Unit =
  {
    optWordData = Option(wordData)

    val posTypeName = wordData.posType match
    {
      case _: VerbType => "verb"
      case _: NounType => "noun"
      case _ => "???"
    }

    val tv_setmeaning_PosType = dataContext.rootView.findViewById(R.id.tv_setmeaning_PosType).asInstanceOf[TextView]
    tv_setmeaning_PosType.setText(posTypeName)

    val tv_setmeaning_ClassType = dataContext.rootView.findViewById(R.id.tv_setmeaning_ClassType).asInstanceOf[TextView]
    tv_setmeaning_ClassType.setText(wordData.posType.toString)

    val word = optWordData match
    {
      case Some(WordData(_: NounType, map, _)) => map.getOrElse(NounForm.NOUN_NOM_SG, "???")
      case Some(WordData(_: VerbType, map, _)) => map.getOrElse(VerbForm.VERB_INFINITIVE, "???")
      case _ => "???"
    }

    val tv_setmeaning_Word = dataContext.rootView.findViewById(R.id.tv_setmeaning_Word).asInstanceOf[TextView]
    tv_setmeaning_Word.setText(word)
  }

  def saveDefinitionInto(): Unit = optWordData match
  {
    case Some(data) => igDatabase.save(data, meanings)
    case None => ()
  }
}
