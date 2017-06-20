package com.example.hyenawarrior.myapplication.new_word.new_pos_helpers

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View}
import android.widget.{EditText, LinearLayout, Spinner}
import com.example.hyenawarrior.dictionary.model.database.marshallers.VerbForm
import com.example.hyenawarrior.dictionary.modelview.EditTextTypeListener
import com.example.hyenawarrior.dictionary.modelview.add_new_word_panel.VerbDeclensionAdapter
import com.example.hyenawarrior.myapplication.R
import com.example.hyenawarrior.myapplication.new_word.VerbDeclPreferencesDialog
import com.example.hyenawarrior.myapplication.new_word.pages.AddNewWordActivity._
import com.example.hyenawarrior.myapplication.new_word.pages.{VerbData, WordData}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.{INFINITIVE, PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.{IMPERATIVE, INDICATIVE, PARTICIPLE, SUBJUNCTIVE}
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
	type Override = (VerbForm, Option[String])
	type Parameters = (List[VerbClassEnum], Override, Map[View, Override])

	// what we define in the UI
	var selectedVerbParameters: Parameters = (List(), (VerbForm.VERB_INDICATIVE_PRESENT_1ST_SG, None), Map())

	// all the generated forms
	var latestVerbData: Map[VerbClassEnum, Map[VerbForm, String]] = Map()

	val VerbDeclensionAdapter = new VerbDeclensionAdapter(activity)

	// panel for showing the verb forms
	val LL_DECL_LIST = rootView.findViewById(R.id.llVerbDeclensions).asInstanceOf[LinearLayout]

	// verb class spinner in the UI
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

	val verbDeclPreferencesDialog = new VerbDeclPreferencesDialog(activity)

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

		val (_, givenBaseForm, map) = selectedVerbParameters

		selectedVerbParameters = (stemClass, givenBaseForm, map)

		tryCompleteForms()
	}

	override def onRemoveOverride(tableRow: View): Unit =
	{
		selectedVerbParameters = selectedVerbParameters match
		{
			case (nc, baseDef, map) => (nc, baseDef, map - tableRow)
		}

		tryCompleteForms()
	}

	override def onPrimaryTextChange(str: String): Unit = {

		val optStrFixed = Option(str).filter(s => s.trim.nonEmpty)

		val (classes, (verbForm, _), map) = selectedVerbParameters

		selectedVerbParameters = (classes, (verbForm, optStrFixed), map)

		tryCompleteForms()
	}

	override def createOverrideFormSetter(isPrimary: Boolean) : View =
	{
		val inflater = getActivity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]
		val rowView = inflater.inflate(R.layout.new_verb_overriding_def_row, null)

		// add hint, it's necessary to be able to remove the overrides
		val btnRemoveView = rowView.findViewById(R.id.ibRemove)
		btnRemoveView.setTag(rowView)
		btnRemoveView.setVisibility(if(isPrimary) View.GONE else View.VISIBLE)

		val btnPref = rowView.findViewById(R.id.ibPreferences)
		btnPref.setOnClickListener(ShowVerbDeclDialogListener)

		// add type listeners
		val etView = rowView.findViewById(R.id.etNewWord_Text).asInstanceOf[EditText]
		val etListener = new EditTextTypeListener(
			if(isPrimary)	onPrimaryTextChange
			else					onTextFormOverride(rowView))

		etView.addTextChangedListener(etListener)

		rowView
	}

	object ShowVerbDeclDialogListener extends View.OnClickListener
	{
		override def onClick(view: View): Unit = verbDeclPreferencesDialog.show(onDeclensionSelected)
	}

	private def onDeclensionSelected(optState: Option[VerbForm]): Unit = optState match
	{
		case Some(verbForm) =>

			val (classes, (_, optStrFixed), map) = selectedVerbParameters

			selectedVerbParameters = (classes, (verbForm, optStrFixed), map)

			tryCompleteForms()

		case None => ()
	}

	override def onDeclensionSelected(index: Int): Unit = {

	}

	override def onNounDeclensionSelected(overridingView: View)(item: (GNumber, Case)): Unit = {


	}

	override def onTextFormOverride(overridingView: View)(str: String): Unit = {

		//val (_, givenBaseForm, map) = selectedNounParameters

		//selectedNounParameters = (stemClass, givenBaseForm, map)
	}

	private def tryCompleteForms(): Unit = selectedVerbParameters match {

		case (maybeEmptyList, (vf, Some(str)), map) =>

			val verbClasses = if(maybeEmptyList.isEmpty) VerbClassEnum.values else maybeEmptyList

			val sortedVerbClasses = verbClasses.sortWith { case(a, b) => a.name < b.name }

			val overridingMap = map.values
				.collect { case (k, Some(v)) => k -> v }
				.toMap

			val wordMaps = sortedVerbClasses
				.map(vc => vc -> generateAllFormsFrom(vc, (vf, str), overridingMap))
				.filter { case (_, forms) => forms.nonEmpty }
				.map{ case(vc, svByVf) => vc -> svByVf.map{ case (k,v) => k -> v.strForm } }

			setInflectedFormsToUI(wordMaps)

			//
			//val optVerbForm = VerbForm.values.find(vf => vf.optPronoun.contains(pronoun) && vf.tense.contains(tense))

			latestVerbData = wordMaps.toMap

		case _ => ()
	}

	private def generateAllFormsFrom(verbClass: VerbClassEnum, baseDef: (VerbForm, String), overrides: Map[VerbForm, String]):	Map[VerbForm, StrongVerb] =
	{
		val (baseVerbForm, baseStr) = baseDef

		// exclude the base form definition and the overrides
		val missingDeclensions = VerbForm.values.filterNot(overrides.contains).filterNot(_ == baseVerbForm)

		val baseVerb: StrongVerb = baseVerbForm match
		{
			case VerbForm(_, INDICATIVE | SUBJUNCTIVE, Some(tense), Some(pronoun)) =>
				FinitiveStrongVerb(baseStr, verbClass, pronoun, tense)

			case _ => null
		}

		// TODO: Word() should be used instead of verb.strForm to handle umlauts and other transformations correctly

		val missingVerbs: Map[VerbForm, StrongVerb] = missingDeclensions
			.flatMap(vf => generateForm(baseVerb, verbClass, vf).map(sv => vf -> sv))
			.toMap

		if(missingVerbs.isEmpty)
		{
			Map()
		}
		else
		{
			missingVerbs + (baseVerbForm -> baseVerb)
		}
	}

	private def generateForm(verb: Verb, verbClass: VerbClassEnum, verbForm: VerbForm): Option[StrongVerb] = (verb, verbForm) match
	{
		// indicative @ subjunctive
		case (sv: StrongVerb, VerbForm(_, mode @ (INDICATIVE | SUBJUNCTIVE), Some(tense), Some(pronoun))) =>
			StrongVerbStemClasses.convertTo(sv, Left(pronoun, mode, tense))

		// participles
		case (sv: StrongVerb, VerbForm(_, PARTICIPLE, Some(PAST), 		None)) =>
			StrongVerbStemClasses.convertTo(sv, Right(PAST_PARTICIPLE))

		case (sv: StrongVerb, VerbForm(_, PARTICIPLE, Some(PRESENT),	None)) =>
			StrongVerbStemClasses.convertTo(sv, Right(PRESENT_PARTICIPLE))

		// infinitive
		case (sv: StrongVerb, VerbForm(_, VerbModeEnum.INFINITIVE, None, 					None)) =>
			StrongVerbStemClasses.convertTo(sv, Right(INFINITIVE))

		// imperative
		case (sv: StrongVerb, VerbForm(_, IMPERATIVE, None, 					Some(pronoun))) =>
			//StrongVerbStemClasses.convertTo(sv, Right(NonFinitiveVerbType.IMPERATIVE))
			None

		case (weakVerb: WeakVerb, _)     => None
		case _ => None
	}

	private def setInflectedFormsToUI(map: List[(VerbClassEnum, Map[VerbForm, String])]): Unit =
	{
		VerbDeclensionAdapter.resetItems(map)

		LL_DECL_LIST.removeAllViews()

		Range(0, VerbDeclensionAdapter.getCount)
			.map(i => VerbDeclensionAdapter.getView(i, null, LL_DECL_LIST))
			.foreach(v => LL_DECL_LIST.addView(v))
	}

  override def getWordFormsBy(view: View): WordData =
  {
    val optVerbClassE = VerbDeclensionAdapter.getSelectorTagOf(view)

    optVerbClassE match
    {
      case Some(verbClassE) => VerbData(verbClassE, latestVerbData(verbClassE))
      case _ => throw new IllegalStateException("Unknown UI control")
    }
  }
}
