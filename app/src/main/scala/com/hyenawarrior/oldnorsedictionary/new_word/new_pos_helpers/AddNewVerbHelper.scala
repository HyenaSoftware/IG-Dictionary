package com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View}
import android.widget._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.NonFinitiveVerbType.{INFINITIVE, PAST_PARTICIPLE, PRESENT_PARTICIPLE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum.{IMPERATIVE, INDICATIVE, PARTICIPLE, SUBJUNCTIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum.{PAST, PRESENT}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbVoice.{ACTIVE, MEDIO_PASSIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.model.database.marshallers.VerbForm
import com.hyenawarrior.oldnorsedictionary.modelview.EditTextTypeListener
import com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel.VerbDeclensionAdapter
import com.hyenawarrior.oldnorsedictionary.new_word.VerbDeclPreferencesDialog
import com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers.AddNewVerbHelper.{GRAY, RED}
import com.hyenawarrior.oldnorsedictionary.new_word.pages.AddNewWordActivity._
import com.hyenawarrior.oldnorsedictionary.new_word.pages.WordData

import scala.language.postfixOps

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
object AddNewVerbHelper
{
	val NON_FINITIVE_VERB_TYPES = List(INFINITIVE, PRESENT_PARTICIPLE, PAST_PARTICIPLE)
	val TENSES = List(PRESENT, PAST)

	val ALL_DECLENSION = Pronoun.values.flatMap(p => TENSES.map(t => p -> t))

	val RED = 0xffcc0000
	val GRAY = 0xffaaaaaa

	type Declension = Either[(Pronoun, VerbTenseEnum), NonFinitiveVerbType]
}

class AddNewVerbHelper(rootView: View, activity: Activity, stemClassSpinner: Spinner) extends AbstractAddNewPosHelper(activity, stemClassSpinner, R.array.verb_types)
{
	type Override = (VerbType, Option[String])
	type Parameters = (List[VerbClassEnum], Map[View, Override])

	// what we define in the UI
	var selectedVerbParameters: Parameters = (List(), Map())

	// all the generated forms
	var latestVerbData: Map[VerbClassEnum, StrongVerb] = Map()

	val VerbDeclensionAdapter = new VerbDeclensionAdapter(activity)

	// panel for showing the verb forms
	val LL_DECL_LIST = rootView.findViewById(R.id.llVerbDeclensions).asInstanceOf[LinearLayout]

	val verbMoodPanel = rootView.findViewById(R.id.glVerbMood).asInstanceOf[GridLayout]
	verbMoodPanel findViewById R.id.rbInd setOnClickListener MoodSelector
	verbMoodPanel findViewById R.id.rbSubj setOnClickListener MoodSelector
	verbMoodPanel findViewById R.id.cbMedioPassive setOnClickListener VoiceSelector

	// verb class spinner in the UI, it should be the same as verb_types.xml
	val LOAD_STEM_CLASS_ENUMS: IndexedSeq[List[VerbClassEnum]] = IndexedSeq(
			List(), // undefined
			List(STRONG_1ST_CLASS, STRONG_2ND_CLASS, STRONG_3RD_CLASS, STRONG_4TH_CLASS, STRONG_5TH_CLASS
				, STRONG_6TH_CLASS
				, STRONG_7_1_CLASS
				, STRONG_7_2A_CLASS
				, STRONG_7_2B_CLASS
				, STRONG_7_3_CLASS
				, STRONG_7_4_CLASS
				, STRONG_7_5_CLASS), // Strong
			List(WEAK_A_STEM, WEAK_I_STEM, WEAK_J_STEM), // Weak
			List(IRREGULAR),
			List(STRONG_1ST_CLASS),
			List(STRONG_2ND_CLASS),
			List(STRONG_3RD_CLASS),
			List(STRONG_4TH_CLASS),
			List(STRONG_5TH_CLASS),
			List(STRONG_6TH_CLASS),
			List(STRONG_7_1_CLASS),
			List(STRONG_7_2A_CLASS),
			List(STRONG_7_2B_CLASS),
			List(STRONG_7_3_CLASS),
			List(STRONG_7_4_CLASS),
			List(STRONG_7_5_CLASS),
			List(WEAK_A_STEM),
			List(WEAK_I_STEM),
			List(WEAK_J_STEM)
		)

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

		var previousMoodLED = R.id.tvVerbInd
		var currentMoodLED = R.id.tvVerbInd

		var previousTenseLED: Option[Int] = None
		var currentTenseLED: Option[Int] = None

		// make sure the other fields are already initialized when this method is called
		onDeclensionSelected(VerbForm.VERB_INDICATIVE_PRESENT_1ST_SG.vtype)

		override def onClick(view: View): Unit = {

			//
			val map = selectedVerbParameters._2
			val myState = map.get(rowView).map(_._1).getOrElse(VerbForm.VERB_INDICATIVE_PRESENT_1ST_SG.vtype)

			//
			verbDeclPreferencesDialog.show(onDeclensionSelected)
		}

		private def onDeclensionSelected(verbType: VerbType): Unit = {

			val (classes, map) = selectedVerbParameters

			val declAndStr = map.get(rowView).map(e => verbType -> e._2).getOrElse(verbType -> None)
			val newMap = (map - rowView) + (rowView -> declAndStr)

			selectedVerbParameters = (classes, newMap)

			updateMoodLED(verbType)
      updateVoiceLED(verbType)
			updateTenseLED(verbType)
			updatePronounLED(verbType)

			previousMoodLED = currentMoodLED
			previousTenseLED = currentTenseLED

			tryCompleteForms()
		}

		def updatePronounLED(verbType: VerbType): Unit = {

			val (clr, text) = verbType match {

				case (_, _, _, Some(Pronoun(SINGULAR, p))) =>	RED -> s"S$p"
				case (_, _, _, Some(Pronoun(PLURAL,   p))) =>	RED -> s"P$p"
				case (_, _, _, None) 											=>	GRAY ->"S1"
			}

			val tvVerbPronoun = rowView.findViewById(R.id.tvVerbPronoun).asInstanceOf[TextView]

			tvVerbPronoun.setTextColor(clr)
			tvVerbPronoun.setText(text)
		}

		def updateTenseLED(verbType: VerbType): Unit = {

			currentTenseLED = verbType match {

				case (_, _, Some(PRESENT), _) => Some(R.id.tvVerbPresent)
				case (_, _, Some(PAST), _) => Some(R.id.tvVerbPast)
				case (_, _, None, _) => None
			}

			previousTenseLED.foreach(rowView.findViewById(_).asInstanceOf[TextView].setTextColor(GRAY))
			currentTenseLED.foreach(rowView.findViewById(_).asInstanceOf[TextView].setTextColor(RED))
		}

		def updateMoodLED(verbType: VerbType): Unit = {

			currentMoodLED = verbType match {

				case (INDICATIVE, _, _, _) => R.id.tvVerbInd
				case (SUBJUNCTIVE, _, _, _) => R.id.tvVerbSubj
				case (VerbModeEnum.INFINITIVE, _, _, _) => R.id.tvVerbInfinitive
				case (PARTICIPLE, _, _, _) => R.id.tvVerbParticiple
				case (IMPERATIVE, _, _, _) => R.id.tvVerbImperative
			}

			rowView.findViewById(previousMoodLED).asInstanceOf[TextView].setTextColor(GRAY)
			rowView.findViewById(currentMoodLED).asInstanceOf[TextView].setTextColor(RED)
		}

    def updateVoiceLED(verbType: VerbType): Unit = {

      val colour = verbType match {

        case (_, ACTIVE,        _, _) => GRAY
        case (_, MEDIO_PASSIVE, _, _) => RED
      }

      rowView.findViewById(R.id.tvVerbMedioPassive).asInstanceOf[TextView].setTextColor(colour)
    }
	}

  object MoodSelector extends View.OnClickListener {

    override def onClick(v: View): Unit = {

			uncheckOthers(v)
			VerbDeclensionAdapter setFinitiveMood moodOf(v)
		}

		private def uncheckOthers(v: View): Unit = {

			val selectedRb = v.getId
			val otherRbs = Seq(R.id.rbInd, R.id.rbSubj).filterNot(_ == selectedRb)

			for(id <- otherRbs) {

				verbMoodPanel.findViewById(id).asInstanceOf[RadioButton].setChecked(false)
			}
		}

    private def moodOf(v: View): FinitiveMood = v.getId match {

      case R.id.rbInd => INDICATIVE
      case R.id.rbSubj => SUBJUNCTIVE
    }
  }

	object VoiceSelector extends View.OnClickListener {

		override def onClick(v: View): Unit = VerbDeclensionAdapter showMediopassives isChecked(v)

		private def isChecked(v: View): Boolean = {

			val cb = v.asInstanceOf[CheckBox]

			cb.isChecked
		}
	}

	override def onTextFormOverride(rowView: View)(str: String): Unit = {

		val optStr = if(str.isEmpty) None else Some(str.replace("ö", "ǫ"))

		val (classes, map) = selectedVerbParameters

		val declStr = map.get(rowView).map(e => (e._1, optStr))
      .getOrElse((VerbForm.VERB_INDICATIVE_PRESENT_1ST_SG.vtype, optStr))

		val newMap = (map - rowView) + ((rowView, declStr))

		selectedVerbParameters = (classes, newMap)

		tryCompleteForms()
	}

	private def tryCompleteForms(): Unit = selectedVerbParameters match {

		case (listOfClasses, viewToVerbForms) =>

      val listOfVerbClasses = if(listOfClasses.isEmpty) VerbClassEnum.values else listOfClasses

      val sortedListOfVerbClasses = listOfVerbClasses.sortWith { case (a, b) => a.toString() < b.toString() }

      val overridingMap = viewToVerbForms.values
        .collect { case (k, Some(v)) => k -> v }
        .toMap

      val wordMaps: List[(VerbClassEnum, StrongVerb)] = sortedListOfVerbClasses
          .map(vc => vc -> generateAllFormsFrom(vc, overridingMap))
          .collect{ case (k, Some(sv)) => k -> sv }

      setInflectedFormsToUI(wordMaps)

      latestVerbData = wordMaps.toMap

		case _ => ()
	}

  private def generateAllFormsFrom(verbClass: VerbClassEnum, overrides: Map[VerbType, String])
    : Option[StrongVerb] = verbClass match {

		case svc: StrongVerbClassEnum => generateMissingFormsOfStrongVerbsFrom(svc, overrides)
		case wvc: WeakVerbClassEnum => None
	}

	private def generateMissingFormsOfStrongVerbsFrom(verbClass: StrongVerbClassEnum, givenVerbForms: Map[VerbType, String])
		: Option[StrongVerb] = try {

      Some(StrongVerb(verbClass, givenVerbForms))

    } catch {

      case e: RuntimeException =>
      val msg = e.getMessage
      android.util.Log.w(AddNewVerbHelper.getClass.getSimpleName, msg)

      None
    }

  private def setInflectedFormsToUI(listOfClassesAndVerbForms: List[(VerbClassEnum, StrongVerb)]): Unit = {

		VerbDeclensionAdapter.resetItems(listOfClassesAndVerbForms)

		val declensionList = LL_DECL_LIST.findViewById(R.id.llVerbDeclensionList).asInstanceOf[LinearLayout]

		declensionList.removeAllViews()

		Range(0, VerbDeclensionAdapter.getCount)
			.map(i => VerbDeclensionAdapter.getView(i, null, declensionList))
			.foreach(v => declensionList.addView(v))
	}

	override def getWordFormsBy(view: View): WordData = {

		val optVerbClassE = VerbDeclensionAdapter.getSelectorTagOf(view)

		optVerbClassE match
		{
			case Some(verbClass) =>
				val selectedForm = latestVerbData(verbClass)
				WordData(selectedForm, List())

			case _ => throw new IllegalStateException("Unknown UI control")
		}
	}
}
