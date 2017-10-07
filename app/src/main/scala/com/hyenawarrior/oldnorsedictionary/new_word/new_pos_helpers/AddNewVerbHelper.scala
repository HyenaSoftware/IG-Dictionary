package com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View}
import android.widget.{EditText, LinearLayout, Spinner}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.{INFINITIVE, PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.{IMPERATIVE, INDICATIVE, PARTICIPLE, SUBJUNCTIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{PAST, PRESENT}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.EnumVerbStem.{PERFECT_STEM, PRESENT_STEM, PRETERITE_PLURAL_STEM, PRETERITE_SINGULAR_STEM}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem._
import com.hyenawarrior.OldNorseGrammar.grammar.{Pronoun, Root, verbs}
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

		val wordMaps: List[(VerbClassEnum, Map[VerbForm, String])] = sortedListOfVerbClasses
				.map(vc => vc -> generateAllFormsFrom(vc, overridingMap).map { case (k, v) => k -> v.strForm })
			  .filter{ case (_, forms) => forms.nonEmpty }

		setInflectedFormsToUI(wordMaps)

		latestVerbData = wordMaps.toMap

		case _ => ()
	}

	private def generateAllFormsFrom(verbClass: VerbClassEnum, overrides: Map[VerbForm, String]):
		Map[VerbForm, Verb] = verbClass match {

		case svc: StrongVerbClassEnum => generateMissingFormsOfStrongVerbsFrom(svc, overrides)
		case wvc: WeakVerbClassEnum => ???
	}

	private def generateMissingFormsOfStrongVerbsFrom(verbClass: StrongVerbClassEnum, overrides: Map[VerbForm, String]): Map[VerbForm, StrongVerb] =	{
		// exclude the base form definition and the overrides
		val missingDeclensions = VerbForm.values.filterNot(overrides.contains)

		try {

			val givenVerbs: Map[VerbForm, StrongVerb] = overrides.map {

				case (vf, rawStr) =>
					val VerbForm(_, mood, optTense, optPronoun) = vf
					vf -> StrongVerb.fromStringRepr(rawStr, verbClass, (mood, optTense, optPronoun))
			}

			//
			val stems = extractStems(givenVerbs.map { case (k, v) => k -> v.getStem() }, verbClass)

			// determine missing stems from existing ones
			val missingVerbs: Map[VerbForm, StrongVerb] = missingDeclensions
				.map(vf => {
					val expectedStemType = verbs.stemFrom(vf.tense, vf.optPronoun.map(_.number), vf.vtype)
					vf -> generateStrongVerbFormAnotherAs(stems(expectedStemType), vf)
				})
				.collect{ case(k, Some(v)) => k -> v }
				.toMap

			val forms = if (missingVerbs.isEmpty) Map[VerbForm, StrongVerb]() else givenVerbs ++ missingVerbs

			forms
		}
		catch {

			case e: NoSuchElementException => Map()
			case e: RuntimeException => Map()
		}
	}

	/**
		*
		* @param pseudoVerbForms	These are NOT the final verbs as its root might be incorrect
		* @param verbClassEnum
		* @return
		*/
	def extractStems(pseudoVerbForms: Map[VerbForm, CommonStrongVerbStem], verbClassEnum: StrongVerbClassEnum):
		Map[EnumVerbStem, CommonStrongVerbStem] = {

		// collect what we have
		val pseudoFormsToStem: Map[EnumVerbStem, Iterable[CommonStrongVerbStem]] = pseudoVerbForms.map {

				case (vf, stem) =>
					val stemType = verbs.stemFrom(vf.tense, vf.optPronoun.map(_.number), vf.vtype)
					StrongVerbStem.fromStrRepr(stem.stringForm(), verbClassEnum, stemType)
			}
		  .groupBy ( _.getStemType() )

		val pseudoStemsBy: Map[EnumVerbStem, CommonStrongVerbStem] = pseudoFormsToStem.map { case(e, m) => e -> m.head }

		// 1) *** GENERIC *** (1-7)

		// determine the present stem
		//	a) from the present stem
		//	b) from the perfect stem
		//	c) from the either past stem (are these the same always?)
		val presentStem =	pseudoStemsBy.getOrElse(PRESENT_STEM,
											pseudoStemsBy.getOrElse(PERFECT_STEM,
											pseudoStemsBy.getOrElse(PRETERITE_SINGULAR_STEM,
											pseudoStemsBy(PRETERITE_PLURAL_STEM))))

		// build a root from the present stem
		val CommonStrongVerbStem(root, _, _) = presentStem

		// now based on StrongVerbClass choose either of:
		// build stems from the root

		val stemMap: Map[EnumVerbStem, CommonStrongVerbStem] = if(verbClassEnum != STRONG_7TH_CLASS) {

			// 2a) *** Class 1st-6th Specific ***
			val preteriteSgStem = StrongVerbStem(root, verbClassEnum, PRETERITE_SINGULAR_STEM)
			val preteritePlStem = StrongVerbStem(root, verbClassEnum, PRETERITE_PLURAL_STEM)
			val perfectStem = StrongVerbStem(root, verbClassEnum, PRESENT_STEM)

			Map(PRESENT_STEM -> presentStem,
				PRETERITE_SINGULAR_STEM -> preteriteSgStem,
				PRETERITE_PLURAL_STEM -> preteritePlStem,
				PERFECT_STEM -> perfectStem)

		} else {

			// 2b) *** Class 7th Specific ***
			Map(PRESENT_STEM -> presentStem) ++ extractStemsOfClass7thVerbs(pseudoStemsBy, root)
		}

		stemMap
	}

	def extractStemsOfClass7thVerbs(pseudoStemsBy: Map[EnumVerbStem, CommonStrongVerbStem], root: Root): Map[EnumVerbStem, StrongVerbStemClass7th] = {
		// determine the other (missing) stems:
		//  a) if the perfect stem is missing than that from the present or past stem
		//	b) if the past tenses missing then
		// 		- from the other past tense
		// 		-	or from the perfect stem
		//		- from the present stem
		val perfectStAbGr = pseudoStemsBy.getOrElse(PERFECT_STEM,
			pseudoStemsBy.getOrElse(PRESENT_STEM,
				pseudoStemsBy.getOrElse(PRETERITE_SINGULAR_STEM,
					pseudoStemsBy(PRETERITE_PLURAL_STEM))))
			.getAblautGrade()

		val perfectStem = StrongVerbStemClass7th(root, PERFECT_STEM, perfectStAbGr)


		//
		val preteriteSgStAbGr = pseudoStemsBy.getOrElse(PRETERITE_SINGULAR_STEM,
			pseudoStemsBy.getOrElse(PRETERITE_PLURAL_STEM,
				pseudoStemsBy.getOrElse(PERFECT_STEM,
					pseudoStemsBy(PRESENT_STEM)))).getAblautGrade()

		val preteriteSgStem = StrongVerbStemClass7th(root, PRETERITE_SINGULAR_STEM, preteriteSgStAbGr)

		//
		val preteritePlStAbGr = pseudoStemsBy.getOrElse(PRETERITE_SINGULAR_STEM,
			pseudoStemsBy.getOrElse(PRETERITE_PLURAL_STEM,
				pseudoStemsBy.getOrElse(PERFECT_STEM,
					pseudoStemsBy(PRESENT_STEM)))).getAblautGrade()

		val preteritePlStem = StrongVerbStemClass7th(root, PRETERITE_SINGULAR_STEM, preteritePlStAbGr)

		Map(PRETERITE_SINGULAR_STEM -> preteriteSgStem,
			PRETERITE_PLURAL_STEM -> preteritePlStem,
			PERFECT_STEM -> perfectStem)
	}

	private def generateStrongVerbFormAnotherAs(newStem: CommonStrongVerbStem, targetVerbForm: VerbForm): Option[StrongVerb]
		= targetVerbForm match {
		// indicative @ subjunctive
		case VerbForm(_, mood @ (INDICATIVE | SUBJUNCTIVE | IMPERATIVE), Some(tense), Some(pronoun)) =>
			Some(StrongVerb.verbFrom(newStem, pronoun, tense, mood.asInstanceOf[FinitiveMood]))

		// participles
		case VerbForm(_, PARTICIPLE, Some(PAST), 		None) =>
			Some(StrongVerb.verbFrom(newStem, PAST_PARTICIPLE))

		case VerbForm(_, PARTICIPLE, Some(PRESENT),	None) =>
			Some(StrongVerb.verbFrom(newStem, PRESENT_PARTICIPLE))

		// infinitive
		case VerbForm(_, VerbModeEnum.INFINITIVE, None, None) =>
			Some(StrongVerb.verbFrom(newStem, NonFinitiveVerbType.INFINITIVE))

		// imperative
		case VerbForm(_, IMPERATIVE, Some(tense), Some(pronoun)) =>
			//StrongVerbStemClasses.convertTo(sv, Right(NonFinitiveVerbType.IMPERATIVE))
			//Some(FinitiveStrongVerb(newStem, pronoun, tense, IMPERATIVE))
			None

		case _ => None
	}

	private def setInflectedFormsToUI(listOfClassesAndVerbForms: List[(VerbClassEnum, Map[VerbForm, String])]): Unit = {

		VerbDeclensionAdapter.resetItems(listOfClassesAndVerbForms)

		LL_DECL_LIST.removeAllViews()

		Range(0, VerbDeclensionAdapter.getCount)
			.map(i => VerbDeclensionAdapter.getView(i, null, LL_DECL_LIST))
			.foreach(v => LL_DECL_LIST.addView(v))
	}

	override def getWordFormsBy(view: View): WordData = {

		val optVerbClassE = VerbDeclensionAdapter.getSelectorTagOf(view)

		optVerbClassE match
		{
			case Some(verbClass) =>
				val forms = latestVerbData(verbClass).map
				{
					case (k,v) => k.asInstanceOf[PosForm] -> v
				}

				WordData(VerbType.findByVerbClass(verbClass), forms, List())

			case _ => throw new IllegalStateException("Unknown UI control")
		}
	}
}
