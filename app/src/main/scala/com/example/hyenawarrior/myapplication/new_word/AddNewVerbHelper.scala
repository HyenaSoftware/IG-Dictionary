package com.example.hyenawarrior.myapplication.new_word

import android.app.Activity
import android.view.View
import android.widget.{LinearLayout, Spinner, TableRow}
import com.example.hyenawarrior.dictionary.modelview.add_new_word_panel.VerbDeclensionAdapter
import com.example.hyenawarrior.myapplication.R
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{PAST, PRESENT}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.{StrongVerbStem, VerbStem}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.StrongVerbStemClasses
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber, Pronoun}

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
class AddNewVerbHelper(activity: Activity, stemClassSpinner: Spinner) extends AbstractAddNewPosHelper(activity, stemClassSpinner, R.array.verb_types)
{
	type Override = (Option[Pronoun], Option[VerbTenseEnum], Option[String])
	type Parameters = (List[VerbClassEnum], Override, Map[AnyRef, Override])

	var selectedNounParameters: Parameters = (List(), (Some(Pronoun.SG_1), Some(VerbTenseEnum.PRESENT), None), Map())

	val VerbDeclensionAdapter = new VerbDeclensionAdapter(activity)
	val LL_DECL_LIST = activity.findViewById(R.id.llVerbDeclensions).asInstanceOf[LinearLayout]

	val LOAD_STEM_CLASS_ENUMS: Vector[List[VerbClassEnum]] = activity.getResources
		.getStringArray(R.array.verb_types)
		.map
		{
			case "Undefined" => List()
			case "Strong" => List(STRONG_1ST_CLASS, STRONG_2ND_CLASS, STRONG_3RD_CLASS, STRONG_4TH_CLASS, STRONG_5TH_CLASS, STRONG_6TH_CLASS, STRONG_7TH_CLASS)
			case "Weak" => List(WEAK_A_STEM, WEAK_I_STEM, WEAK_J_STEM)
			case str => VerbClassEnum.findByName(str).toList
		}
		.toVector

	override def activate(): Unit = {

		super.activate()

		LL_DECL_LIST.setVisibility(View.VISIBLE)
	}

	override def deactivate(): Unit = {

		super.activate()

		LL_DECL_LIST.setVisibility(View.GONE)
	}

	override def onStemClassSelected(index: Int): Unit = {

		val stemClass = LOAD_STEM_CLASS_ENUMS(index)

		val (_, givenBaseForm, map) = selectedNounParameters

		selectedNounParameters = (stemClass, givenBaseForm, map)

		fillForms()
	}

	override def onRemoveOverride(view: TableRow): Unit = {

	}

	override def onPrimaryTextChange(str: String): Unit = {

		val optStrFixed = Option(str).filter(s => s.trim.nonEmpty)

		val (classes, (primaryDecl, optTense, _), map) = selectedNounParameters

		selectedNounParameters = (classes, (primaryDecl, optTense, optStrFixed), map)

		fillForms()
	}

	override def onDeclensionSelected(index: Int): Unit = {

	}

	override def onNounDeclensionSelected(overridingView: View)(item: (GNumber, Case)): Unit = {


	}

	override def onTextFormOverride(overridingView: View)(str: String): Unit = {

		//val (_, givenBaseForm, map) = selectedNounParameters

		//selectedNounParameters = (stemClass, givenBaseForm, map)
	}

	private def generateForm(verb: Verb, verbClass: VerbClassEnum, pronoun: Pronoun, tense: VerbTenseEnum): Option[StrongVerb] = verb match {

		case strongVerb: StrongVerb =>
			val optVerb = StrongVerbStemClasses.convertTo(strongVerb, Left(pronoun, VerbModeEnum.INDICATIVE, tense))

			optVerb
	}

	private def generateFormsFrom(verbClass: VerbClassEnum, baseDef: (Pronoun, VerbTenseEnum, String), map: Map[AnyRef, Override]):	Map[(Pronoun, VerbTenseEnum), String] =	{

		val overridingDefs = map.values.flatMap
		{
			case (Some(pronoun), Some(tense), Some(str)) => Some(pronoun -> str)
			case _ => None
		}.toMap

		val missingPronouns = Pronoun.values.filterNot(overridingDefs.keySet.contains(_))
		val TENSES = List(PRESENT, PAST)

		val (basePronoun, baseTense, baseStr) = baseDef
		val baseVerb = StrongVerb(baseStr, verbClass, basePronoun, baseTense)
		//val baseStem = StrongVerbGenerator.stemFrom(baseVerb)

		val missingVerbs = missingPronouns
			.flatMap(p => TENSES.map(t => p -> t))
		  .flatMap
			{
				case(pn, tn) => generateForm(baseVerb, verbClass, pn, tn).map(v => pn -> tn -> v.strForm)
			}
			.toMap

		if(missingVerbs.nonEmpty)
		{
			Map(basePronoun -> baseTense -> baseStr) ++ missingVerbs
		}
		else
		{
			Map()
		}
	}

	private def fillForms(): Unit = selectedNounParameters match {

		case (maybeEmptyList, (Some(pronoun), Some(tense), Some(str)), map) =>
			val verbClasses = if(maybeEmptyList.isEmpty) VerbClassEnum.values else maybeEmptyList

			val wordMaps = verbClasses
				.map(vc => vc -> generateFormsFrom(vc, (pronoun, tense, str), map))
				.filter { case (_, forms) => forms.nonEmpty }

			setInflectedFormsToUI(wordMaps)

		case _ => ()
	}

	private def setInflectedFormsToUI(map: List[(VerbClassEnum, Map[(Pronoun, VerbTenseEnum), String])]): Unit =
	{
		VerbDeclensionAdapter.resetItems(map)

		LL_DECL_LIST.removeAllViews()

		Range(0, VerbDeclensionAdapter.getCount)
			.map(i => VerbDeclensionAdapter.getView(i, null, LL_DECL_LIST))
			.foreach(v => LL_DECL_LIST.addView(v))
	}


}
