package com.example.hyenawarrior.myapplication.new_word

import android.app.Activity
import android.view.View
import android.widget.{LinearLayout, Spinner, TableRow}
import com.example.hyenawarrior.dictionary.modelview.add_new_word_panel.NounDeclensionAdapter
import com.example.hyenawarrior.myapplication.R
import com.example.hyenawarrior.myapplication.new_word.AddNewNounHelper.NOUN_DECLENSIONS
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.{NounStemClass, NounStemClassEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number}

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
object AddNewNounHelper
{
	val NOUN_DECLENSIONS: Vector[(Number, Case)] = Number.conventionalValues.flatMap(n => Case.values.map(cs => (n, cs))).toVector
}

class AddNewNounHelper(activity: Activity, stemClassSpinner: Spinner) extends AbstractAddNewPosHelper(activity, stemClassSpinner, R.array.noun_types)
{
	type Override = (Option[(Number, Case)], Option[String])
	type Parameters = (List[NounStemClassEnum], Override, Map[AnyRef, Override])

	var selectedNounParameters: Parameters = (List(), (None, None), Map())

	val NounDeclensionAdapter = new NounDeclensionAdapter(activity)
	val LL_DECL_LIST = activity.findViewById(R.id.llDeclensionList).asInstanceOf[LinearLayout]

	override def activate(): Unit = {

		super.activate()

		LL_DECL_LIST.setVisibility(View.VISIBLE)
	}

	override def deactivate(): Unit = {

		super.activate()

		LL_DECL_LIST.setVisibility(View.GONE)
	}

	def onRemoveOverride(tableRow: TableRow) = {

		selectedNounParameters = selectedNounParameters match
		{
			case (nc, baseDef, map) => (nc, baseDef, map - tableRow)
		}

		fillNounForms()
	}

	//
	override def onPrimaryTextChange(str: String): Unit =
	{
		val (stemClass, (givenCaseNum, _), map) = selectedNounParameters

		val strFixed = Option(str).filter(s => s.trim.nonEmpty)

		selectedNounParameters = (stemClass, (givenCaseNum, strFixed), map)

		fillNounForms()
	}

	override def onDeclensionSelected(index: Int): Unit =
	{
		onNounDeclensionSelected(NOUN_DECLENSIONS(index))
	}

	private def onNounDeclensionSelected(item: (Number, Case)): Unit =
	{
		val (stemClass, (_, givenBaseForm), map) = selectedNounParameters

		selectedNounParameters = (stemClass, (Some(item), givenBaseForm), map)

		fillNounForms()
	}

	override def onStemClassSelected(index: Int): Unit =
	{
		val newStemClassList = LOAD_STEM_CLASS_ENUMS(index)

		val (_, givenBaseForm, map) = selectedNounParameters

		selectedNounParameters = (newStemClassList, givenBaseForm, map)

		fillNounForms()
	}

	//
	override def onTextFormOverride(overridingView: View)(str: String): Unit =
	{
		val (stemClass, givenForm, map) = selectedNounParameters

		val strFixed = Option(str).filter(s => s.trim.nonEmpty)

		val overrideData = map.get(overridingView)

		val newData = overrideData match
		{
			case Some((optNumCase, _)) => (optNumCase, strFixed)
			case None => (None, strFixed)
		}

		selectedNounParameters = (stemClass, givenForm, map + (overridingView -> newData))

		fillNounForms()
	}

	override def onNounDeclensionSelected(overridingView: View)(item: (Number, Case)): Unit =
	{
		val (stemClass, givenBaseForm, map) = selectedNounParameters

		val overrideData = map.get(overridingView)

		val newData = overrideData match
		{
			case Some((_, optStr)) => (Some(item), optStr)
			case None => (Some(item), None)
		}

		selectedNounParameters = (stemClass, givenBaseForm, map + (overridingView -> newData))

		fillNounForms()
	}

	val LOAD_STEM_CLASS_ENUMS: Vector[List[NounStemClassEnum]] =	activity.getResources
		.getStringArray(R.array.noun_types)
		.map
		{
			case "Undefined" => List()
			case "Feminine" => List(STRONG_FEMININE_A, STRONG_FEMININE_I, STRONG_FEMININE_R, WEAK_FEMININE_I, WEAK_FEMININE_U)
			case "Masculine" => List(STRONG_MASCULINE_A, STRONG_MASCULINE_I, STRONG_MASCULINE_R, STRONG_MASCULINE_U, WEAK_MASCULINE_A, WEAK_MASCULINE_R)
			case "Neuter" => List(STRONG_NEUTER, WEAK_NEUTER_U)
			case str => NounStemClassEnum.findByName[NounStemClassEnum](str).toList
		}
		.toVector

	private def generateFormsFrom(stemClass: NounStemClass, baseDef: ((Number, Case), String), map: Map[AnyRef, Override]):	Map[(Number, Case), String] =
	{
		val overridingDefs = map.values.flatMap
		{
			case (Some(numCase), Some(str)) => Some((numCase, str))
			case _ => None
		}.toMap

		val root = baseDef match
		{
			case (numCase, str) if stemClass != null => stemClass.unapply(str, numCase)
			case _ => None
		}

		val optWordsOfRoot = root.map(r => NOUN_DECLENSIONS.map(nd => nd -> stemClass(r, -1, nd).strForm).toMap)

		val wordMap = optWordsOfRoot.getOrElse(Map())

		wordMap ++ overridingDefs
	}

	private def fillNounForms(): Unit = selectedNounParameters match
	{
		case (maybeEmptyList, (Some(numCase), Some(str)), map) =>
		val listOfNSCE = if(maybeEmptyList.isEmpty) NounStemClassEnum.values else maybeEmptyList

		val wordMaps = listOfNSCE.map(n => n -> generateFormsFrom(n.nounStemClass, (numCase, str), map))
		setInflectedFormsToUI(wordMaps)

		case _ => ()
	}

	private def setInflectedFormsToUI(map: List[(NounStemClassEnum, Map[(Number, Case), String])]): Unit =
	{
		NounDeclensionAdapter.resetItems(map)

		LL_DECL_LIST.removeAllViews()

		Range(0, NounDeclensionAdapter.getCount)
			.map(i => NounDeclensionAdapter.getView(i, null, LL_DECL_LIST))
			.foreach(v => LL_DECL_LIST.addView(v))
	}
}
