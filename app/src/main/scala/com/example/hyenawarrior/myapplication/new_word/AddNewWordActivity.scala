package com.example.hyenawarrior.myapplication.new_word

import android.content.Context
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.view.{LayoutInflater, View}
import android.widget._
import com.example.hyenawarrior.dictionary.modelview.add_new_word_panel.NounDeclensionAdapter
import com.example.hyenawarrior.myapplication.new_word.AddNewWordActivity.{INDEFINITE_NOUN_EDIT_TEXTS, NOUN_DECLENSIONS}
import com.example.hyenawarrior.myapplication.{MainActivity, R}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.{NounStemClass, NounStemClassEnum}
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

	val NOUN_DECLENSIONS: List[(Number, Case)] = Number.conventionalValues.flatMap(n => Case.values.map(cs => (n, cs)))
}

class AddNewWordActivity extends AppCompatActivity
{
	lazy val tlOverrides = findViewById(R.id.tlOverrides).asInstanceOf[TableLayout]

	type Override = (Option[(Number, Case)], Option[String])
	type Parameters = (Option[NounStemClassEnum], Override, Map[AnyRef, Override])

	var selectedNounParameters: Parameters = (None, (None, None), Map())

	lazy val NounDeclensionAdapter = new NounDeclensionAdapter(this)

	lazy val LL_DECL_LIST = findViewById(R.id.llDeclensionList).asInstanceOf[LinearLayout]

	protected override def onCreate(savedInstanceState: Bundle)
	{
		super.onCreate(savedInstanceState)

    setContentView(R.layout.activity_add_new_word)

		//https://developer.android.com/training/basics/firstapp/starting-activity.html
		// Get the Intent that started this activity and extract the string
		val intent = getIntent
		val message = intent.getStringExtra(MainActivity.EXTRA_MESSAGE)

		//
		val stemClassSpinnerListener = new SpinnerListener(loadStemClassEnums, onNounStemClassSelected)
		val spSelectStemClass = findViewById(R.id.spSelectStemClass).asInstanceOf[Spinner]
		spSelectStemClass.setOnItemSelectedListener(stemClassSpinnerListener)


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
			tlOverrides.removeView(tableRow)

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


	def loadStemClassEnums: List[Option[NounStemClassEnum]] =	getResources
			.getStringArray(R.array.noun_types)
			.map(NounStemClassEnum.findByName[NounStemClassEnum])
		  .toList

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

	private def onNounStemClassSelected(newOptStemClass: Option[NounStemClassEnum])
	{
		val (_, givenBaseForm, map) = selectedNounParameters

		selectedNounParameters = (newOptStemClass, givenBaseForm, map)

		fillNounForms()
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

	private def fillNounForms(): Unit = selectedNounParameters match
	{
		case (Some(nsce), baseDef, map) => fillNounForms((List(nsce), baseDef, map))

		case (None, baseDef @ (Some(_), Some(_)), map) =>
			val nsces = NounStemClassEnum.values
			fillNounForms(nsces, baseDef, map)

		case _ => ()
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

	private def fillNounForms(parameters: (List[NounStemClassEnum], Override, Map[AnyRef, Override])): Unit = parameters match
	{
		case (nsce, (Some(numCase), Some(str)), map) =>
			val wordMaps = nsce.map(n => n -> generateFormsFrom(n.nounStemClass, (numCase, str), map))
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

	//
	def addNewOverride(view: View) = if(tlOverrides.getChildCount < 8)
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

		tlOverrides.addView(rowView)
	}
}
