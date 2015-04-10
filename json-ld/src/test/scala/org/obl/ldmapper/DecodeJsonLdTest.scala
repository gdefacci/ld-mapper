package org.obl.ldmapper

import org.obl.raz._

import org.junit.Test
import junit.framework.TestCase


class DecodeJsonLdTest {

  def fail[T](e: Throwable): T = throw e

  lazy val reader = LdReader.fromString(LdReadOptions("@", LdReadStrategy.Expanded))

  @Test
  def decodeComposite(): Unit = {
    
    val base = HTTP("www.mysite.com") / "base"

    val cd = LdDecode(
      LdFieldDecode[Int](base / "p1"),
      LdFieldDecode[String](base / "p2"),
      LdFieldDecode[Boolean](base / "p3"))

    val rawLd = """
    {
    "http://www.mysite.com/base/p1":12,
    "http://www.mysite.com/base/p2":["nmfds fdsfsd"],
    "http://www.mysite.com/base/p3":true
    }
      """

    reader.decode(rawLd)(cd).fold(
      err => fail(err),
      v => assert((12, "nmfds fdsfsd", true) == v))

    val rawLd1 = """
    {
    "http://www.mysite.com/base/p1":[12],
    "http://www.mysite.com/base/p2":["nmfds fdsfsd"],
    "http://www.mysite.com/base/p3":[true]
    }
      """

    reader.decode(rawLd1)(cd).fold(
      err => fail(err),
      v => assert((12, "nmfds fdsfsd", true) == v))

    val rawLd2 = """
    {
    "http://www.mysite.com/base/p1":[12,13],
    "http://www.mysite.com/base/p2":["nmfds fdsfsd"],
    "http://www.mysite.com/base/p3":[true]
    }
      """

    reader.decode(rawLd2)(cd).fold(
      err => (),
      v => ???)

  
  }
}