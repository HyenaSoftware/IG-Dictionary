package com.hyenawarrior.oldnorsedictionary.model

/**
  * Created by HyenaWarrior on 2018.01.27..
  */
package object auxiliary {

  def autoClose[A <: AutoCloseable, R](closeable: A)(f: A => R): R = try {

    f(closeable)

  } finally {

    closeable.close()
  }
}
