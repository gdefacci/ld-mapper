package org.obl.ldmapper
package builder

import org.obl.raz
import shapeless._

object LdBuilder extends LdBuilder[HNil.type, HNil.type](HNil, HNil)

class LdBuilder[HL <: HList, HR <: HList](left: HL, right: HR) {

  def add[T](p: LdFieldEncode[T]) = new LdBuilder[LdFieldEncode[T] :: HL, HR](p :: left, right)
  def add[T](p: LdFieldDecode[T]) = new LdBuilder[HL, LdFieldDecode[T] :: HR](left, p :: right)
  
  /**
   * FIXME: make private
   */
  def add[I, O](pd: LdFieldDecode[I], pe: LdFieldEncode[O]) = new LdBuilder[LdFieldEncode[O] :: HL, LdFieldDecode[I] :: HR](pe :: left, pd :: right)

  def add[T](path: raz.Path)(implicit dec: LdDecode[T], enc: LdEncode[T]): LdBuilder[LdFieldEncode[T] :: HL, LdFieldDecode[T] :: HR] =
    add(LdFieldDecode(path), LdFieldEncode(path))

  def add[T](f:JsonLdField[T])(implicit pd: LdDecode[T], pe: LdEncode[T]): LdBuilder[LdFieldEncode[T] :: HL, LdFieldDecode[T] :: HR] = {
    add[T,T](LdFieldDecode.jsonldField(f), LdFieldEncode.jsonLdField(f))
  }  
    
  def toLdEncode[T](implicit tej: HWrite[HL, T]): LdEncode[T] = tej(left)
  def toLdDecode[T](implicit tej: HRead[HR, T]): LdDecode[T] = tej(right)
}


// def apply[T](fld: Path)(implicit dec: LdDecode[T]): GenericLdFieldDecode[Seq[JsonLdModel], T] = {
// def apply[T](nd:Path)(implicit ec:LdEncode[T]):LdFieldEncode[T] 
