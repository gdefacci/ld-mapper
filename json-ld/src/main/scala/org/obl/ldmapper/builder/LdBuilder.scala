package org.obl.ldmapper
package builder

import org.obl.raz
import shapeless._

object LdBuilder extends LdBuilder[HNil.type, HNil.type](HNil, HNil)

class LdBuilder[HI <: HList, HO <: HList](input: HI, output: HO) {

  def add[O](p: LdFieldEncode[O]) = new LdBuilder[HI, LdFieldEncode[O] :: HO](input, p :: output)
  def add[I](p: LdFieldDecode[I]) = new LdBuilder[LdFieldDecode[I] :: HI, HO](p :: input, output)
  
  private def add[I, O](pd: LdFieldDecode[I], pe: LdFieldEncode[O]) = new LdBuilder[LdFieldDecode[I] :: HI, LdFieldEncode[O] :: HO](pd :: input, pe :: output)

  def add[T](path: raz.Path)(implicit dec: LdDecode[T], enc: LdEncode[T]): LdBuilder[LdFieldDecode[T] :: HI, LdFieldEncode[T] :: HO] =
    add(LdFieldDecode(path), LdFieldEncode(path))

  def add[T](f:JsonLdField[T])(implicit pd: LdDecode[T], pe: LdEncode[T]): LdBuilder[LdFieldDecode[T] :: HI, LdFieldEncode[T] :: HO] = {
    add[T,T](LdFieldDecode.jsonldField(f), LdFieldEncode.jsonLdField(f))
  }  
    
  def toLdEncode[T](implicit tej: HWrite[HO, T]): LdEncode[T] = tej(output)
  def toLdDecode[T](implicit tej: HRead[HI, T]): LdDecode[T] = tej(input)
}