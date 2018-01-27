package com.hyenawarrior.oldnorsedictionary.new_word

import android.os.Bundle
import android.support.v4.content.ContextCompat
import android.support.v7.app.AppCompatActivity
import android.view.View
import android.widget.{Adapter, LinearLayout}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.StrongVerb
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

        // set meanings
        val meaningAdapter = new MeaningAdapter(this)
        meaningAdapter resetItems di.meanings

        val llMeanings = findViewById(R.id.llMeanings).asInstanceOf[LinearLayout]
        extractViewsInto(meaningAdapter, llMeanings)

        showVerbs(di.posObj)

      case _ => ()
    }
  }

  private def extractViewsInto(adapter: Adapter, layout: LinearLayout): Unit = {

    Range(0, adapter.getCount)
      .map(i => adapter.getView(i, null, null))
      .foreach(v => layout.addView(v))
  }

  private def showVerbs(obj: Any): Unit = obj match {

    case sv: StrongVerb =>
      //
      val view = findViewById(R.id.verb_conjugation_viewer)
      setDeclensionsTo(sv, view)
  }

  def onCollapseView(view: View): Unit = {

    val picRsrc = view.getId match {

      case R.id.iv_toggle_active_voice_view       => invertVisibilityAndGetNewPicRsrc(R.id.ll_active_voice_view)
      case R.id.iv_toggle_mediopassive_voice_view => invertVisibilityAndGetNewPicRsrc(R.id.ll_mediopassive_voice_view)
      case R.id.iv_toggle_past_participle_view    => invertVisibilityAndGetNewPicRsrc(R.id.ll_past_participle_view)
    }

    val pic = ContextCompat.getDrawable(getApplicationContext, picRsrc)
    view setBackground pic
  }

  private def invertVisibilityAndGetNewPicRsrc(viewId: Int): Int = {

    val panel = findViewById(viewId)
    val (nv, picRsrc) = panel.getVisibility match {

      case View.VISIBLE => View.GONE -> R.drawable.ic_keyboard_arrow_down_black_18dp
      case View.GONE => View.VISIBLE -> R.drawable.ic_keyboard_arrow_up_black_18dp
    }

    panel setVisibility nv

    picRsrc
  }
}
