package com.example.hyenawarrior.myapplication.new_word.new_pos_helpers

import android.app.Activity
import android.view.View
import android.widget.{LinearLayout, Spinner, TableRow}
import com.example.hyenawarrior.dictionary.modelview.add_new_word_panel.VerbDeclensionAdapter
import com.example.hyenawarrior.myapplication.R
import com.example.hyenawarrior.myapplication.new_word.new_pos_helpers.AddNewVerbHelper.{Declension, NON_FINITIVE_VERB_TYPES}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.{INFINITIVE, PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{PAST, PRESENT}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.StrongVerbStemClasses
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber, Pronoun}

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
object AddNewVerbHelper
{
	val NON_FINITIVE_VERB_TYPES = List(INFINITIVE, PRESENT_PARTICIPLE, PAST_PARTICIPLE)
	val TENSES = List(PRESENT, PAST)

	val ALL_DECLENSION = Pronoun.values.flatMap(p => TENSES.map(t => p -> t))

	type Declension = Either[(Pronoun, VerbTenseEnum), NonFinitiveVerbType]
}

class AddNewVerbHelper(rootView: View, activity: Activity, stemClassSpinner: Spinner) extends AbstractAddNewPosHelper(activity, stemClassSpinner, R.array.verb_types)
{
	type OptDeclension = Either[(Option[Pronoun], Option[VerbTenseEnum]), Option[NonFinitiveVerbType]]
	type Override = (Option[Pronoun], Option[VerbTenseEnum], Option[String])
	type Parameters = (List[VerbClassEnum], Override, Map[AnyRef, Override])

	var selectedNounParameters: Parameters = (List(), (Some(Pronoun.SG_1), Some(VerbTenseEnum.PRESENT), None), Map())

	val VerbDeclensionAdapter = new VerbDeclensionAdapter(activity)
	val LL_DECL_LIST = rootView.findViewById(R.id.llVerbDeclensions).asInstanceOf[LinearLayout]

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

		case strongVerb: StrongVerb => StrongVerbStemClasses.convertTo(strongVerb, Left(pronoun, VerbModeEnum.INDICATIVE, tense))
		case _ => None
	}

	private def generateNonFinitiveForm(verb: Verb, verbClass: VerbClassEnum, givenNonFinVerbTypes: List[NonFinitiveVerbType]): Map[NonFinitiveVerbType, StrongVerb] = verb match {

		case strongVerb: StrongVerb => givenNonFinVerbTypes
				.flatMap
				{
					nonFinVerbType => StrongVerbStemClasses
						.convertTo(strongVerb, Right(nonFinVerbType))
						.map(x => nonFinVerbType -> x)
				}
			  .toMap

		case _ => Map()
	}

	private def generateFormsFrom(verbClass: VerbClassEnum, baseDef: (Pronoun, VerbTenseEnum, String), map: Map[AnyRef, Override]):	Map[Declension, String] =	{

		val (basePronoun, baseTense, baseStr) = baseDef

		val overridingDefs = map.values.flatMap
		{
			case (Some(pronoun), Some(tense), Some(str)) => Some(pronoun -> tense -> str)
			case _ => None
		}.toMap

		val missingDeclensions = AddNewVerbHelper.ALL_DECLENSION
			.filterNot(overridingDefs.keySet.contains(_))
		  //.filterNot{ case (p, t) => p == basePronoun && t == baseTense }

		val baseVerb = FinitiveStrongVerb(baseStr, verbClass, basePronoun, baseTense)
		//val baseStem = StrongVerbGenerator.stemFrom(baseVerb)

		// TODO: Word() should be used instead of verb.strForm to handle umlauts and other transformations correctly

		val missingVerbs: Map[Declension, String] = missingDeclensions
		  .flatMap
			{
				case(pn, tn) =>	generateForm(baseVerb, verbClass, pn, tn).map(v => Left(pn -> tn) -> v.strForm)
			}
			.toMap

		val missingNonFinVerbs: Map[Declension, String] = generateNonFinitiveForm(baseVerb, verbClass, NON_FINITIVE_VERB_TYPES)
			.map { case (k, v) => Right(k) -> v.strForm }

		if(missingVerbs.nonEmpty)
		{
			//Map(Left(basePronoun -> baseTense) -> baseStr) ++
				missingVerbs ++ missingNonFinVerbs
		}
		else
		{
			Map()
		}
	}

	private def fillForms(): Unit = selectedNounParameters match {

		case (maybeEmptyList, (Some(pronoun), Some(tense), Some(str)), map) =>
			val verbClasses = if(maybeEmptyList.isEmpty) VerbClassEnum.values else maybeEmptyList

			val sortedVerbClasses = verbClasses.sortWith { case(a, b) => a.name < b.name }

			val wordMaps = sortedVerbClasses
				.map(vc => vc -> generateFormsFrom(vc, (pronoun, tense, str), map))
				.filter { case (_, forms) => forms.nonEmpty }

			setInflectedFormsToUI(wordMaps)

		case _ => ()
	}

	private def setInflectedFormsToUI(map: List[(VerbClassEnum, Map[Declension, String])]): Unit =
	{
		VerbDeclensionAdapter.resetItems(map)

		LL_DECL_LIST.removeAllViews()

		Range(0, VerbDeclensionAdapter.getCount)
			.map(i => VerbDeclensionAdapter.getView(i, null, LL_DECL_LIST))
			.foreach(v => LL_DECL_LIST.addView(v))
	}


}
