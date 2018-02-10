package com.hyenawarrior

/**
  * Created by HyenaWarrior on 2017.12.26..
  */
package object auxiliary {

  object & {

    def unapply[T](arg: T): Option[(T, T)] = Some((arg, arg))
  }
}
