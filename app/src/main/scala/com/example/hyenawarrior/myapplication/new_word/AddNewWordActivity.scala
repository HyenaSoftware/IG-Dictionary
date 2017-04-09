package com.example.hyenawarrior.myapplication.new_word

import android.app.Activity
import android.content.Context
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.view.{LayoutInflater, View}
import android.widget.AdapterView.OnItemSelectedListener
import android.widget._
import com.example.hyenawarrior.dictionary.model.Database
import com.example.hyenawarrior.dictionary.modelview.GrammarHelpers
import com.example.hyenawarrior.myapplication.new_word.AddNewWordActivity.{INDEFINITE_NOUN_EDIT_TEXTS, NOUN_DECLENSIONS}
import com.example.hyenawarrior.myapplication.{MainActivity, R}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number, Root, Word}

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

	var selectedNounParameters: (Option[NounStemClassEnum], Map[AnyRef, (Option[(Number, Case)], Option[String])]) = (None, Map())


	protected override def onCreate(savedInstanceState: Bundle)
	{
		super.onCreate(savedInstanceState);

    setContentView(R.layout.activity_add_new_word);

		//https://developer.android.com/training/basics/firstapp/starting-activity.html
		// Get the Intent that started this activity and extract the string
		val intent = getIntent
		val message = intent.getStringExtra(MainActivity.EXTRA_MESSAGE)

		//
		val trPrimaryNounForm = findViewById(R.id.trPrimaryNounForm)

		//
		val etPriText = findViewById(R.id.etNewWord_PriText).asInstanceOf[EditText]
		etPriText.addTextChangedListener(makeFormOverrideTextListener(trPrimaryNounForm))

		val spSelectStemClass = findViewById(R.id.spSelectStemClass).asInstanceOf[Spinner]
		spSelectStemClass.setOnItemSelectedListener(makeSpinnerNounStemClassListener(trPrimaryNounForm))

		//
		val spNounDecl = findViewById(R.id.spNounDecl).asInstanceOf[Spinner]
		spNounDecl.setOnItemSelectedListener(makeSpinnerNounDeclListener(trPrimaryNounForm))
  }

	def onRemoveOverride(view: View) = view.getTag match
	{
		case tableRow: TableRow => tlOverrides.removeView(tableRow)
		case _ => ()
	}

	//
	def makeFormOverrideTextListener(view: View) = new EditTextTypeListener(this, onTextFormOverride(view))
	def makeSpinnerNounDeclListener(view: View) = new SpinnerListener(NOUN_DECLENSIONS, onNounDeclensionSelected(view))
	def makeSpinnerNounStemClassListener(view: View) = new SpinnerListener(loadStemClassEnums, onNounStemClassSelected(view))


	def loadStemClassEnums: List[NounStemClassEnum] =	getResources
			.getStringArray(R.array.noun_types)
			.map(NounStemClassEnum.findByName[NounStemClassEnum](_).get)
		  .toList


	def onTextFormOverride(overridingView: View)(str: String)
	{
		val (stemClass, map) = selectedNounParameters

		val strFixed = Option(str).filter(s => s.trim.nonEmpty)

		val overrideData = map.get(overridingView)

		val newData = overrideData match
		{
			case Some((optNumCase, _)) => (optNumCase, strFixed)
			case None => (None, strFixed)
		}

		selectedNounParameters = (stemClass, map + (overridingView -> newData))

		fillNounForms
	}

	def onNounStemClassSelected(overridingView: View)(newOptStemClass: Option[NounStemClassEnum]) =
	{
		val (_, map) = selectedNounParameters

		selectedNounParameters = (newOptStemClass, map)

		fillNounForms
	}

	def onNounDeclensionSelected(overridingView: View)(item: Option[(Number, Case)]): Unit = item match
	{
		case Some((num, cs)) =>
			val (stemClass, map) = selectedNounParameters

			val overrideData = map.get(overridingView)

			val newData = overrideData match
			{
				case Some((_, optStr)) => (Some(num, cs), optStr)
				case None => (Some(num, cs), None)
			}

			selectedNounParameters = (stemClass, map + (overridingView -> newData))

			fillNounForms

		case None => ()
	}

	private def fillNounForms = selectedNounParameters match
	{
		case (Some(NounStemClassEnum(_, stemClass)), map) =>
			{
				val overridingDefs = map.values

				val roots = overridingDefs
					.filter(e => e._1.nonEmpty && e._2.nonEmpty)
					.filter(e => stemClass != null) // TODO remove it
					.flatMap { case (Some(numCase), Some(str)) => stemClass.unapply(str, numCase) }
				  .toSet

				val words = roots.map(r => r -> NOUN_DECLENSIONS.map(nd => nd -> stemClass(r, -1, nd)).toMap)

				val wordMap = words.headOption.map(_._2).getOrElse(Map())

				setInflectedFormsToUI(wordMap)
			}
		case (None, map) => ()
		case _ => ()
	}

	private def setInflectedFormsToUI(map: Map[(Number, Case), Word])
	{
		INDEFINITE_NOUN_EDIT_TEXTS
			.foreach
			{
				case (id, cs, num) =>
					val text = map.get(num, cs).map(_.strForm).getOrElse("...")
					val textView = findViewById(id).asInstanceOf[TextView]
					textView.setText(text)
			}
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
