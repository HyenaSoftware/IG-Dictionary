package com.example.hyenawarrior.myapplication.new_word

import android.content.Context
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.view.{LayoutInflater, View}
import android.widget._
import com.example.hyenawarrior.dictionary.modelview.add_new_word_panel.NounDeclensionAdapter
import com.example.hyenawarrior.myapplication.new_word.AddNewWordActivity.{INDEFINITE_NOUN_EDIT_TEXTS, NOUN_DECLENSIONS}
import com.example.hyenawarrior.myapplication.{MainActivity, R}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.{NounStemClass, NounStemClassEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.VerbStemClassEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.{VerbStemClass, VerbStemClassEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number}

object AddNewWordActivity
{
	val INDEFINITE_NOUN_EDIT_TEXTS = List(
			(R.id.tvNewWord_Nom_Sg, Case.NOMINATIVE,	Number.SINGULAR)
		, (R.id.tvNewWord_Acc_Sg, Case.ACCUSATIVE,	Number.SINGULAR)
		, (R.id.tvNewWord_Dat_Sg, Case.DATIVE,			Number.SINGULAR)
		, (R.id.tvNewWord_Gen_Sg, Case.GENITIVE,		Number.SINGULAR)

		, (R.id.tvNewWord_Nom_Pl, Case.NOMINATIVE,	Number.PLURAL)
		, (R.id.tvNewWord_Acc_Pl, Case.ACCUSATIVE,	Number.PLURAL)
		, (R.id.tvNewWord_Dat_Pl, Case.DATIVE,			Number.PLURAL)
		, (R.id.tvNewWord_Gen_Pl, Case.GENITIVE,		Number.PLURAL)
	)

	val NOUN_DECLENSIONS: Vector[(Number, Case)] = Number.conventionalValues.flatMap(n => Case.values.map(cs => (n, cs))).toVector
}

class AddNewWordActivity extends AppCompatActivity
{
	outer =>

	type Override = (Option[(Number, Case)], Option[String])
	type Parameters = (List[NounStemClassEnum], Override, Map[AnyRef, Override])

	var selectedNounParameters: Parameters = (List(), (None, None), Map())
	var currentPosHelper: AddNewPosHelper = AddNewNullHelper

	//
	class LazyPostInit
	{
		val tlOverrides = findViewById(R.id.tlOverrides).asInstanceOf[TableLayout]
		val addNewNounHelper = new AddNewNounHelper(outer)
		val addNewVerbHelper = new AddNewVerbHelper(outer)

		val NounDeclensionAdapter = new NounDeclensionAdapter(outer)

		val POS_TYPES = Vector(addNewNounHelper, addNewVerbHelper, AddNewNullHelper)
		val LL_DECL_LIST = findViewById(R.id.llDeclensionList).asInstanceOf[LinearLayout]
	}

	lazy val postInitContext = new LazyPostInit

	protected override def onCreate(savedInstanceState: Bundle)
	{
		super.onCreate(savedInstanceState)

    setContentView(R.layout.activity_add_new_word)

		//https://developer.android.com/training/basics/firstapp/starting-activity.html
		// Get the Intent that started this activity and extract the string
		val intent = getIntent
		val message = intent.getStringExtra(MainActivity.EXTRA_MESSAGE)

		//
		val posTypeSpinnerListener = new SpinnerListener(postInitContext.POS_TYPES, onPosTypeSelected)
		val spSelectPoS = findViewById(R.id.spSelectPoS).asInstanceOf[Spinner]
		spSelectPoS.setOnItemSelectedListener(posTypeSpinnerListener)

		//
		val etPriText = findViewById(R.id.etNewWord_PriText).asInstanceOf[EditText]
		etPriText.addTextChangedListener(new EditTextTypeListener(this, onPrimaryTextChange))

		//
		val spNounDecl = findViewById(R.id.spNounDecl).asInstanceOf[Spinner]
		spNounDecl.setOnItemSelectedListener(new SpinnerListener(NOUN_DECLENSIONS, onNounDeclensionSelected))

  }

	def onRemoveOverride(view: View) = view.getTag match
	{
		case tableRow: TableRow =>
			postInitContext.tlOverrides.removeView(tableRow)

			selectedNounParameters = selectedNounParameters match
			{
				case (nc, baseDef, map) => (nc, baseDef, map - tableRow)
			}

			fillNounForms()

		case _ => ()
	}

	//
	def makeFormOverrideTextListener(view: View) = new EditTextTypeListener(this, onTextFormOverride(view))
	def makeSpinnerNounDeclListener(view: View) = new SpinnerListener(NOUN_DECLENSIONS, onNounDeclensionSelected(view))


	def loadStemClassEnums: Vector[List[NounStemClassEnum]] =	getResources
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

	//
	private def onPrimaryTextChange(str: String): Unit =
	{
		val (stemClass, (givenCaseNum, _), map) = selectedNounParameters

		val strFixed = Option(str).filter(s => s.trim.nonEmpty)

		selectedNounParameters = (stemClass, (givenCaseNum, strFixed), map)

		fillNounForms()
	}

	private def onNounDeclensionSelected(item: (Number, Case)): Unit =
	{
		val (stemClass, (_, givenBaseForm), map) = selectedNounParameters

		selectedNounParameters = (stemClass, (Some(item), givenBaseForm), map)

		fillNounForms()
	}

	private def onNounStemClassSelected(newStemClassList: List[NounStemClassEnum])
	{
		val (_, givenBaseForm, map) = selectedNounParameters

		selectedNounParameters = (newStemClassList, givenBaseForm, map)

		fillNounForms()
	}

	private def onPosTypeSelected(newPosType: AddNewPosHelper) {

		currentPosHelper.deactivate()

		currentPosHelper = newPosType

		currentPosHelper.activate()
	}



	//
	private def onTextFormOverride(overridingView: View)(str: String)
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

	private def onNounDeclensionSelected(overridingView: View)(item: (Number, Case)): Unit =
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
		postInitContext.NounDeclensionAdapter.resetItems(map)

		postInitContext.LL_DECL_LIST.removeAllViews()

		Range(0, postInitContext.NounDeclensionAdapter.getCount)
			.map(i => postInitContext.NounDeclensionAdapter.getView(i, null, postInitContext.LL_DECL_LIST))
		  .foreach(v => postInitContext.LL_DECL_LIST.addView(v))
	}

	//
	def addNewOverride(view: View) = if(postInitContext.tlOverrides.getChildCount < 8)
	{
		val inflater = getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]
		val rowView = inflater.inflate(R.layout.new_word_overriding_def_row, null)

		// add hint, it's necessary to be able to remove the overrides
		val btnView = rowView.findViewById(R.id.ibRemove)
		btnView.setTag(rowView)

		// add type listeners
		val etView = rowView.findViewById(R.id.etNewWord_Text).asInstanceOf[EditText]
		etView.addTextChangedListener(makeFormOverrideTextListener(rowView))

		//
		val spNounDecl = rowView.findViewById(R.id.spNounDecl).asInstanceOf[Spinner]
		spNounDecl.setOnItemSelectedListener(makeSpinnerNounDeclListener(rowView))

		postInitContext.tlOverrides.addView(rowView)
	}
}
