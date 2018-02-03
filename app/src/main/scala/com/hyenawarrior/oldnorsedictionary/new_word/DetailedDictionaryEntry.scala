package com.hyenawarrior.oldnorsedictionary.new_word

import android.graphics.drawable.Drawable
import android.os.Bundle
import android.support.v4.content.ContextCompat
import android.support.v7.app.AppCompatActivity
import android.view.View
import android.widget.{LinearLayout, TextView}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{StrongVerb, VerbModeEnum, VerbVoice}
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.model.DictionaryListItem
import com.hyenawarrior.oldnorsedictionary.modelview._

/**
  * Created by HyenaWarrior on 2018.01.20..
  */
class DetailedDictionaryEntry extends AppCompatActivity {

  protected override def onCreate(savedInstanceState: Bundle): Unit = {

    super.onCreate(savedInstanceState)

    setContentView(R.layout.detailed_dictionary_entry)

    val serializable = getIntent.getSerializableExtra("entry")

    //
    serializable match {

      case di: DictionaryListItem =>

      val llMeanings = findViewById(R.id.llMeanings).asInstanceOf[LinearLayout]

        // set meanings
        val meaningAdapter = new MeaningAdapter(this, llMeanings)
        meaningAdapter resetItems di.meanings

        showVerbs(di.posObj)

      case _ => ()
    }
  }

  private def showVerbs(obj: Any): Unit = obj match {

    case sv: StrongVerb =>
      //
      val view = findViewById(R.id.verb_conjugation_viewer)
      setDeclensionsTo(sv, view)

      val tvWord = findViewById(R.id.tvWord).asInstanceOf[TextView]
      val priForm = sv.verbForms
        .get((VerbModeEnum.INFINITIVE, VerbVoice.ACTIVE, None, None))
        .map(_.strForm)
        .getOrElse("???")
      tvWord.setText(priForm)
  }

  def onCollapseView(view: View): Unit = {

    val (imgViewId, viewPanelId) = view.getId match {

      case R.id.rl_toggle_active_voice_view       => R.id.iv_toggle_active_voice_view        -> R.id.ll_active_voice_view
      case R.id.rl_toggle_mediopassive_voice_view => R.id.iv_toggle_mediopassive_voice_view  -> R.id.ll_mediopassive_voice_view
      case R.id.rl_toggle_past_participle_view    => R.id.iv_toggle_past_participle_view     -> R.id.ll_past_participle_view
    }

    val pic = invertVisibilityAndGetNewPicRsrc(viewPanelId)
    val imgView = findViewById(imgViewId)
    imgView setBackground pic
  }

  private def invertVisibilityAndGetNewPicRsrc(viewId: Int): Drawable = {

    val panel = findViewById(viewId)
    val (nv, picRsrc) = panel.getVisibility match {

      case View.VISIBLE => View.GONE -> R.drawable.ic_keyboard_arrow_down_black_18dp
      case View.GONE => View.VISIBLE -> R.drawable.ic_keyboard_arrow_up_black_18dp
    }

    panel setVisibility nv

    ContextCompat.getDrawable(getApplicationContext, picRsrc)
  }
}
