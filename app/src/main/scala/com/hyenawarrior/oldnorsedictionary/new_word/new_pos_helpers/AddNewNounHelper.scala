package com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View}
import android.widget._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.{PLURAL, SINGULAR}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{Noun, NounType}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel.NounDeclensionAdapter
import com.hyenawarrior.oldnorsedictionary.modelview.{EditTextTypeListener, ItemListener}
import com.hyenawarrior.oldnorsedictionary.new_word.pages.AddNewWordActivity._
import com.hyenawarrior.oldnorsedictionary.new_word.pages.WordData

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
object AddNewNounHelper
{
	val NOUN_DECLENSIONS = Vector(
		(SINGULAR, NOMINATIVE),
		(SINGULAR, ACCUSATIVE),
		(SINGULAR, DATIVE),
		(SINGULAR, GENITIVE),

		(PLURAL, NOMINATIVE),
		(PLURAL, ACCUSATIVE),
		(PLURAL, DATIVE),
		(PLURAL, GENITIVE)
	)

  type Declension = (GNumber, Case)
}

class AddNewNounHelper(rootView: View, activity: Activity, stemClassSpinner: Spinner)
  extends AbstractAddNewPosHelper(activity, stemClassSpinner, R.array.noun_types) {

	type Override = (Option[NounType], Option[String])
	type Parameters = (List[NounStemClassEnum], Override, Map[View, Override])

	var selectedNounParameters: Parameters = (List(), (None, None), Map())
  var latestNounData: Map[NounStemClassEnum, Noun] = Map()

	val LL_NOUN_DECLS = rootView.findViewById[LinearLayout](R.id.llNounDeclensions)
	val LL_DECL_LIST = LL_NOUN_DECLS.findViewById[LinearLayout](R.id.llNounDeclensionList)

	val NounDeclensionAdapter = new NounDeclensionAdapter(activity, LL_DECL_LIST)

	LL_NOUN_DECLS findViewById[View] R.id.rbIndef setOnClickListener DefinitenessListener
	LL_NOUN_DECLS findViewById[View] R.id.rbDef   setOnClickListener DefinitenessListener

	//
	override def activate(): Unit = {

		super.activate()

		LL_NOUN_DECLS.setVisibility(View.VISIBLE)
	}

	override def deactivate(): Unit = {

		super.activate()

		LL_NOUN_DECLS.setVisibility(View.GONE)
	}

	object DefinitenessListener extends View.OnClickListener {

		override def onClick(v: View): Unit = v.getId match {

			case R.id.rbIndef => ()
			case R.id.rbDef => ()
		}
	}

	def onRemoveOverride(tableRow: View) =
	{
		selectedNounParameters = selectedNounParameters match
		{
			case (nc, baseDef, map) => (nc, baseDef, map - tableRow)
		}

		fillNounForms()
	}

	//
	def onPrimaryTextChange(str: String): Unit =
	{
		val (stemClass, (givenCaseNum, _), map) = selectedNounParameters

		val strFixed = Option(str).filter(s => s.trim.nonEmpty)

		selectedNounParameters = (stemClass, (givenCaseNum, strFixed), map)

		fillNounForms()
	}

	override def createOverrideFormSetter(isPrimary: Boolean): View =
	{
		val inflater = getActivity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]
		val rowView = inflater.inflate(R.layout.new_word_overriding_def_row, null)

		// add hint, it's necessary to be able to remove the overrides
		val btnRemoveView = rowView.findViewById[View](R.id.ibRemove)
		btnRemoveView.setTag(rowView)
		btnRemoveView.setVisibility(if(isPrimary) View.GONE else View.VISIBLE)

		// add text listeners
		val etView = rowView.findViewById[EditText](R.id.etNewWord_Text)
		val etListener = new EditTextTypeListener(
			if(isPrimary)	onPrimaryTextChange
			else					onTextFormOverride(rowView))

		etView.addTextChangedListener(etListener)

		//
		val spNounDecl = rowView.findViewById[Spinner](R.id.spNounDecl)
		val spListener = new ItemListener(
			if(isPrimary)	i => onNounDeclensionSelected(AddNewNounHelper.NOUN_DECLENSIONS(i))
			else					i => onNounDeclensionSelected2(rowView)(AddNewNounHelper.NOUN_DECLENSIONS(i)))

		spNounDecl.setOnItemSelectedListener(spListener)

		rowView
	}

	private def onNounDeclensionSelected(item: NounType): Unit =
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

	def onNounDeclensionSelected2(overridingView: View)(item: NounType): Unit =
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
			case "Feminine" => List(STRONG_FEMININE_A1, STRONG_FEMININE_A2, STRONG_FEMININE_I, STRONG_FEMININE_R, WEAK_FEMININE_I, WEAK_FEMININE_U)
			case "Masculine" => List(STRONG_MASCULINE_A, STRONG_MASCULINE_I, STRONG_MASCULINE_R, STRONG_MASCULINE_U, WEAK_MASCULINE_A, WEAK_MASCULINE_R)
			case "Neuter" => List(STRONG_NEUTER, WEAK_NEUTER_U)
			case str => NounStemClassEnum.findBy(str).toList
		}
		.toVector

	private def generateFormsFrom(stemClass: NounStemClass, baseDef: (NounType, String), map: Map[View, Override])
		:	Option[Noun] = try {

		val mapForms: Map[NounType, String] = map.values.map {

			case (Some(nf), Some(str)) => nf -> str

		}.toMap

		val baseForm: Map[NounType, String] = Map(baseDef)

		Some(Noun(stemClass, baseForm ++ mapForms))

	} catch {

		case e: RuntimeException =>
			val msg = e.getMessage
			android.util.Log.w(AddNewVerbHelper.getClass.getSimpleName, msg)

			None
	}

	private def fillNounForms(): Unit = selectedNounParameters match
	{
		case (maybeEmptyList, (Some(numCase), Some(str)), map) =>
      val listOfNSCE = if(maybeEmptyList.isEmpty) NounStemClassEnum.values.toList else maybeEmptyList

      val wordMaps: List[(NounStemClassEnum, Noun)] = listOfNSCE
        .map(nsce => nsce -> generateFormsFrom(nsce.nounStemClass, (numCase, str), map))
        .collect{ case (k, Some(noun)) => k -> noun }

      NounDeclensionAdapter resetItems wordMaps

      latestNounData = wordMaps.toMap

		case _ => ()
	}

	override def getWordFormsBy(view: View): WordData =
  {
    val optNounStemClassE = NounDeclensionAdapter.getSelectorTagOf(view)

    optNounStemClassE match
    {
      case Some(nounStemClassE) =>
				val selectedNoun = latestNounData(nounStemClassE)
				WordData(selectedNoun, List())

      case _ => throw new IllegalStateException("Unknown control")
    }
  }
}
