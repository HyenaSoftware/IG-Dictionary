package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StemTransform.Transformation

/**
  * Created by HyenaWarrior on 2017.12.08..
  */
abstract class TransformationMode extends Serializable

object TransformationMode {

  /**
    * Used when for example the present stem is computed from the past stem, and it's unknown that how the
    * present stem forms behaves
    */
  object Undefined extends TransformationMode

  /**
    * In the given present stem forms neither of the stem transformations were applied. So ignore them
    */
  object Disabled extends TransformationMode

  /**
    * During the normalization the transformation was performed so, it must be applied to the other forms of the verb.
    * @param t Stem transformation, which is used in the current stem.
    */
  case class EnabledFor(t: Transformation) extends TransformationMode
}
