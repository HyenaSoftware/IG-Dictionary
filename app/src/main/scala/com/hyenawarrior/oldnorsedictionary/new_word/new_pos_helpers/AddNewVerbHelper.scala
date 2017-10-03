package com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View}
import android.widget.{EditText, LinearLayout, Spinner}
import com.hyenawarrior.OldNorseGrammar.grammar.{Pronoun, verbs}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StaticAblaut
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.{INFINITIVE, PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.{IMPERATIVE, INDICATIVE, PARTICIPLE, SUBJUNCTIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{PAST, PRESENT}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.VerbStemEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.StrongVerbStemClasses
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.model.database.marshallers.{PosForm, VerbForm, VerbType}
import com.hyenawarrior.oldnorsedictionary.modelview.EditTextTypeListener
import com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel.VerbDeclensionAdapter
import com.hyenawarrior.oldnorsedictionary.new_word.VerbDeclPreferencesDialog
import com.hyenawarrior.oldnorsedictionary.new_word.pages.AddNewWordActivity._
import com.hyenawarrior.oldnorsedictionary.new_word.pages.WordData

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
	type Parameters = (List[VerbClassEnum], Map[View, Override])

	// what we define in the UI
	var selectedVerbParameters: Parameters = (List(), Map())

	// all the generated forms
	var latestVerbData: Map[VerbClassDesc, Map[VerbForm, String]] = Map()

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

		val (_, map) = selectedVerbParameters

		selectedVerbParameters = (stemClass, map)

		tryCompleteForms()
	}

	override def onRemoveOverride(tableRow: View): Unit =
	{
		selectedVerbParameters = selectedVerbParameters match
		{
			case (nc, map) => (nc, map - tableRow)
		}

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
		btnPref.setOnClickListener(ShowVerbDeclDialogListener(rowView))

		// add type listeners
		val etView = rowView.findViewById(R.id.etNewWord_Text).asInstanceOf[EditText]
		val etListener = new EditTextTypeListener(onTextFormOverride(rowView))

		etView.addTextChangedListener(etListener)

		rowView
	}

	case class ShowVerbDeclDialogListener(rowView: View) extends View.OnClickListener
	{
		override def onClick(view: View): Unit = verbDeclPreferencesDialog.show(onDeclensionSelected(rowView)(_))
	}

	private def onDeclensionSelected(rowView: View)(verbForm: VerbForm): Unit =
	{
		val (classes, map) = selectedVerbParameters

		val declStr = map.get(rowView).map(e => (verbForm, e._2)).getOrElse((verbForm, None))
		val newMap = (map - rowView) + ((rowView, declStr))

		selectedVerbParameters = (classes, newMap)

		tryCompleteForms()
	}

	override def onTextFormOverride(rowView: View)(str: String): Unit =
	{
		val optStr = if(str.isEmpty) None else Some(str)

		val (classes, map) = selectedVerbParameters

		val declStr = map.get(rowView).map(e => (e._1, optStr)).getOrElse((VerbForm.VERB_INDICATIVE_PRESENT_1ST_SG, optStr))
		val newMap = (map - rowView) + ((rowView, declStr))

		selectedVerbParameters = (classes, newMap)

		tryCompleteForms()
	}

	private def tryCompleteForms(): Unit = selectedVerbParameters match {

		case (listOfClasses, viewToVerbForms) =>

		val listOfVerbClasses = if(listOfClasses.isEmpty) VerbClassEnum.values else listOfClasses

		val sortedListOfVerbClasses = listOfVerbClasses
			.filter
			{
				case VerbClassEnum.IRREGULAR | VerbClassEnum.WEAK_A_STEM | VerbClassEnum.WEAK_I_STEM | VerbClassEnum.WEAK_J_STEM => false
				case _ => true
			}
			.sortWith { case (a, b) => a.toString() < b.toString() }

		val overridingMap = viewToVerbForms.values
			.collect { case (k, Some(v)) => k -> v }
			.toMap

		val wordMaps = sortedListOfVerbClasses
			.flatMap(vc => generateAllFormsFrom(vc, overridingMap))
			.filter { case (_, forms) => forms.nonEmpty }
			.map{ case(vcd, verbByForm) => vcd -> verbByForm.map{ case (k,v) => k -> v.strForm } }

		setInflectedFormsToUI(wordMaps)

		//
		//val optVerbForm = VerbForm.values.find(vf => vf.optPronoun.contains(pronoun) && vf.tense.contains(tense))

		latestVerbData = wordMaps.toMap

		case _ => ()
	}

	private def generateAllFormsFrom(verbClass: VerbClassEnum, overrides: Map[VerbForm, String]):
		Option[(VerbClassDesc, Map[VerbForm, Verb])] = verbClass match {

		case svc: StrongVerbClassEnum => generateMissingFormsOfStrongVerbsFrom(svc, overrides)
		case wvc: WeakVerbClassEnum => ???
	}

	private def generateMissingFormsOfStrongVerbsFrom(verbClass: StrongVerbClassEnum, overrides: Map[VerbForm, String]): Option[(StrongVerbClassDesc, Map[VerbForm, StrongVerb])] =	{
		// exclude the base form definition and the overrides
		val missingDeclensions = VerbForm.values.filterNot(overrides.contains)

		def stemFrom(vf: VerbForm): VerbStemEnum = verbs.stemFrom(vf.tense, vf.optPronoun.map(_.number), vf.vtype)

		val formsPerStem = overrides
			.map { case (vf, rawStr) => stemFrom(vf) -> rawStr }
			.groupBy(_._1)
			.map{ case (k,v) => k -> v.values.toSeq }

		val optVerbClassDesc = getDescOfStrongVerbClassFor(verbClass, formsPerStem)

		optVerbClassDesc.map(verbClassDesc => {

			val givenVerbs: Map[VerbForm, StrongVerb] = overrides.map {

				case (vf, rawStr) => vf -> generateStrongVerbsFromRawString(verbClassDesc, vf, rawStr)
			}

			val baseVerbs = givenVerbs
				.groupBy{ case (vf, sv) => stemFrom(vf) }
				.map{ case (vf, m) => vf -> m.values.head }

			// determine missing stems from existing ones

			val missingVerbs: Map[VerbForm, StrongVerb] = missingDeclensions
				.map(vf => vf -> stemFrom(vf))
				.collect { case (vf, st) if baseVerbs.contains(st) => vf -> baseVerbs(st) }
				.flatMap { case(vf, sv) => generateStrongVerbFormAnotherAs(sv, verbClassDesc.ablaut, vf).map(vf -> _) }
				.toMap

			val forms = if (missingVerbs.isEmpty) Map[VerbForm, StrongVerb]() else givenVerbs ++ missingVerbs

			verbClassDesc -> forms
		})
	}

	private def generateStrongVerbsFromRawString(strongVerbClass: StrongVerbClassDesc, verbForm: VerbForm, rawStr: String): StrongVerb = {

		val VerbForm(_, mood, optTense, optPronoun) = verbForm

		generateStrongVerbFromRawStr(rawStr, strongVerbClass, (mood, optTense, optPronoun))
	}

	private def generateStrongVerbFormAnotherAs(sourceVerb: StrongVerb, ablaut: StaticAblaut, targetVerbForm: VerbForm): Option[StrongVerb] = targetVerbForm match
	{
		// indicative @ subjunctive
		case VerbForm(_, mode @ (INDICATIVE | SUBJUNCTIVE | IMPERATIVE), Some(tense), Some(pronoun)) =>
			StrongVerbStemClasses.convertTo(sourceVerb, ablaut, (pronoun, mode.asInstanceOf[FinitiveMood], tense))

		// participles
		case VerbForm(_, PARTICIPLE, Some(PAST), 		None) => StrongVerbStemClasses.convertTo(sourceVerb, ablaut, PAST_PARTICIPLE)

		case VerbForm(_, PARTICIPLE, Some(PRESENT),	None) => StrongVerbStemClasses.convertTo(sourceVerb, ablaut, PRESENT_PARTICIPLE)

		// infinitive
		case VerbForm(_, VerbModeEnum.INFINITIVE, None, None) => StrongVerbStemClasses.convertTo(sourceVerb, ablaut, INFINITIVE)

		// imperative
		case VerbForm(_, IMPERATIVE, None, Some(pronoun)) =>
			//StrongVerbStemClasses.convertTo(sv, Right(NonFinitiveVerbType.IMPERATIVE))
			None

		case _ => None
	}

	private def setInflectedFormsToUI(listOfClassesAndVerbForms: List[(VerbClassDesc, Map[VerbForm, String])]): Unit =
	{
		VerbDeclensionAdapter.resetItems(listOfClassesAndVerbForms)

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
			case Some(verbClassDesc) =>
				val forms = latestVerbData(verbClassDesc).map
				{
					case (k,v) => k.asInstanceOf[PosForm] -> v
				}

				WordData(VerbType.findByVerbClass(verbClassDesc.vClass), forms, List())

			case _ => throw new IllegalStateException("Unknown UI control")
		}
	}
}
