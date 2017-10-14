package com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers

import java.lang.String.format

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View}
import android.widget.{EditText, LinearLayout, Spinner}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.{INFINITIVE, PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
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
	var latestVerbData: Map[VerbClassEnum, StrongVerbContext] = Map()

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

	override def onRemoveOverride(tableRow: View): Unit = {

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

	case class ShowVerbDeclDialogListener(rowView: View) extends View.OnClickListener {

		override def onClick(view: View): Unit = verbDeclPreferencesDialog.show(onDeclensionSelected(rowView)(_))
	}

	private def onDeclensionSelected(rowView: View)(verbForm: VerbForm): Unit = {

		val (classes, map) = selectedVerbParameters

		val declStr = map.get(rowView).map(e => (verbForm, e._2)).getOrElse((verbForm, None))
		val newMap = (map - rowView) + ((rowView, declStr))

		selectedVerbParameters = (classes, newMap)

		tryCompleteForms()
	}

	override def onTextFormOverride(rowView: View)(str: String): Unit = {

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

		val wordMaps: List[(VerbClassEnum, StrongVerbContext)] = sortedListOfVerbClasses
				.map(vc => vc -> generateAllFormsFrom(vc, overridingMap))
			  .collect{ case (k, Some(sv)) => k -> sv }

		setInflectedFormsToUI(wordMaps)

		latestVerbData = wordMaps.toMap

		case _ => ()
	}

	private def generateAllFormsFrom(verbClass: VerbClassEnum, overrides: Map[VerbForm, String]):
		Option[StrongVerbContext] = verbClass match {

		case svc: StrongVerbClassEnum => generateMissingFormsOfStrongVerbsFrom(svc, overrides)
		case wvc: WeakVerbClassEnum => ???
	}

	private def generateMissingFormsOfStrongVerbsFrom(verbClass: StrongVerbClassEnum, overrides: Map[VerbForm, String])
		: Option[StrongVerbContext] = {

      val givenVerbForms: Map[verbs.VerbType, String] = overrides.map {

        case (vf, rawStr) =>
          val VerbForm(_, mood, optTense, optPronoun) = vf
          (mood, optTense, optPronoun) -> rawStr
      }

      try {

        Some(StrongVerbContext(verbClass, givenVerbForms))

      } catch {

        case e: RuntimeException =>
        val msg = e.getMessage
        android.util.Log.w(AddNewVerbHelper.getClass.getSimpleName, msg)

        None
      }
    }

  private def setInflectedFormsToUI(listOfClassesAndVerbForms: List[(VerbClassEnum, StrongVerbContext)]): Unit = {

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
				val forms: Map[PosForm, String] = latestVerbData(verbClass).verbForms.map {

          case ((md, oT, oP), sv) => VerbForm(0, md, oT, oP) -> sv.strForm
        }

				WordData(VerbType.findByVerbClass(verbClass), forms, List())

			case _ => throw new IllegalStateException("Unknown UI control")
		}
	}
}
