package org.obl.ldmapper

trait LdEncodes {

  def apply[T1](e1:LdFieldEncode[T1]):LdEncode[T1] = {
    LdEncode[T1] { p:T1 =>
      for (f1 <- e1.encode(p)) yield {
        LdObject(f1)
      }
    }
  }

  def apply[T1,T2](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2]):LdEncode[(T1,T2)] = {
    LdEncode[(T1,T2)] { p:(T1,T2) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2)) yield {
        LdObject(f1 ++ f2)
      }
    }
  }

  def apply[T1,T2,T3](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3]):LdEncode[(T1,T2,T3)] = {
    LdEncode[(T1,T2,T3)] { p:(T1,T2,T3) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3)) yield {
        LdObject(f1 ++ f2 ++ f3)
      }
    }
  }

  def apply[T1,T2,T3,T4](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4]):LdEncode[(T1,T2,T3,T4)] = {
    LdEncode[(T1,T2,T3,T4)] { p:(T1,T2,T3,T4) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5]):LdEncode[(T1,T2,T3,T4,T5)] = {
    LdEncode[(T1,T2,T3,T4,T5)] { p:(T1,T2,T3,T4,T5) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6]):LdEncode[(T1,T2,T3,T4,T5,T6)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6)] { p:(T1,T2,T3,T4,T5,T6) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7]):LdEncode[(T1,T2,T3,T4,T5,T6,T7)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7)] { p:(T1,T2,T3,T4,T5,T6,T7) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8)] { p:(T1,T2,T3,T4,T5,T6,T7,T8) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11], e12:LdFieldEncode[T12]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11); f12 <- e12.encode(p._12)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11 ++ f12)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11], e12:LdFieldEncode[T12], e13:LdFieldEncode[T13]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11); f12 <- e12.encode(p._12); f13 <- e13.encode(p._13)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11 ++ f12 ++ f13)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11], e12:LdFieldEncode[T12], e13:LdFieldEncode[T13], e14:LdFieldEncode[T14]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11); f12 <- e12.encode(p._12); f13 <- e13.encode(p._13); f14 <- e14.encode(p._14)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11 ++ f12 ++ f13 ++ f14)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11], e12:LdFieldEncode[T12], e13:LdFieldEncode[T13], e14:LdFieldEncode[T14], e15:LdFieldEncode[T15]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11); f12 <- e12.encode(p._12); f13 <- e13.encode(p._13); f14 <- e14.encode(p._14); f15 <- e15.encode(p._15)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11 ++ f12 ++ f13 ++ f14 ++ f15)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11], e12:LdFieldEncode[T12], e13:LdFieldEncode[T13], e14:LdFieldEncode[T14], e15:LdFieldEncode[T15], e16:LdFieldEncode[T16]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11); f12 <- e12.encode(p._12); f13 <- e13.encode(p._13); f14 <- e14.encode(p._14); f15 <- e15.encode(p._15); f16 <- e16.encode(p._16)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11 ++ f12 ++ f13 ++ f14 ++ f15 ++ f16)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11], e12:LdFieldEncode[T12], e13:LdFieldEncode[T13], e14:LdFieldEncode[T14], e15:LdFieldEncode[T15], e16:LdFieldEncode[T16], e17:LdFieldEncode[T17]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11); f12 <- e12.encode(p._12); f13 <- e13.encode(p._13); f14 <- e14.encode(p._14); f15 <- e15.encode(p._15); f16 <- e16.encode(p._16); f17 <- e17.encode(p._17)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11 ++ f12 ++ f13 ++ f14 ++ f15 ++ f16 ++ f17)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11], e12:LdFieldEncode[T12], e13:LdFieldEncode[T13], e14:LdFieldEncode[T14], e15:LdFieldEncode[T15], e16:LdFieldEncode[T16], e17:LdFieldEncode[T17], e18:LdFieldEncode[T18]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11); f12 <- e12.encode(p._12); f13 <- e13.encode(p._13); f14 <- e14.encode(p._14); f15 <- e15.encode(p._15); f16 <- e16.encode(p._16); f17 <- e17.encode(p._17); f18 <- e18.encode(p._18)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11 ++ f12 ++ f13 ++ f14 ++ f15 ++ f16 ++ f17 ++ f18)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11], e12:LdFieldEncode[T12], e13:LdFieldEncode[T13], e14:LdFieldEncode[T14], e15:LdFieldEncode[T15], e16:LdFieldEncode[T16], e17:LdFieldEncode[T17], e18:LdFieldEncode[T18], e19:LdFieldEncode[T19]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11); f12 <- e12.encode(p._12); f13 <- e13.encode(p._13); f14 <- e14.encode(p._14); f15 <- e15.encode(p._15); f16 <- e16.encode(p._16); f17 <- e17.encode(p._17); f18 <- e18.encode(p._18); f19 <- e19.encode(p._19)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11 ++ f12 ++ f13 ++ f14 ++ f15 ++ f16 ++ f17 ++ f18 ++ f19)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11], e12:LdFieldEncode[T12], e13:LdFieldEncode[T13], e14:LdFieldEncode[T14], e15:LdFieldEncode[T15], e16:LdFieldEncode[T16], e17:LdFieldEncode[T17], e18:LdFieldEncode[T18], e19:LdFieldEncode[T19], e20:LdFieldEncode[T20]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11); f12 <- e12.encode(p._12); f13 <- e13.encode(p._13); f14 <- e14.encode(p._14); f15 <- e15.encode(p._15); f16 <- e16.encode(p._16); f17 <- e17.encode(p._17); f18 <- e18.encode(p._18); f19 <- e19.encode(p._19); f20 <- e20.encode(p._20)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11 ++ f12 ++ f13 ++ f14 ++ f15 ++ f16 ++ f17 ++ f18 ++ f19 ++ f20)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11], e12:LdFieldEncode[T12], e13:LdFieldEncode[T13], e14:LdFieldEncode[T14], e15:LdFieldEncode[T15], e16:LdFieldEncode[T16], e17:LdFieldEncode[T17], e18:LdFieldEncode[T18], e19:LdFieldEncode[T19], e20:LdFieldEncode[T20], e21:LdFieldEncode[T21]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11); f12 <- e12.encode(p._12); f13 <- e13.encode(p._13); f14 <- e14.encode(p._14); f15 <- e15.encode(p._15); f16 <- e16.encode(p._16); f17 <- e17.encode(p._17); f18 <- e18.encode(p._18); f19 <- e19.encode(p._19); f20 <- e20.encode(p._20); f21 <- e21.encode(p._21)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11 ++ f12 ++ f13 ++ f14 ++ f15 ++ f16 ++ f17 ++ f18 ++ f19 ++ f20 ++ f21)
      }
    }
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22](e1:LdFieldEncode[T1], e2:LdFieldEncode[T2], e3:LdFieldEncode[T3], e4:LdFieldEncode[T4], e5:LdFieldEncode[T5], e6:LdFieldEncode[T6], e7:LdFieldEncode[T7], e8:LdFieldEncode[T8], e9:LdFieldEncode[T9], e10:LdFieldEncode[T10], e11:LdFieldEncode[T11], e12:LdFieldEncode[T12], e13:LdFieldEncode[T13], e14:LdFieldEncode[T14], e15:LdFieldEncode[T15], e16:LdFieldEncode[T16], e17:LdFieldEncode[T17], e18:LdFieldEncode[T18], e19:LdFieldEncode[T19], e20:LdFieldEncode[T20], e21:LdFieldEncode[T21], e22:LdFieldEncode[T22]):LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)] = {
    LdEncode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)] { p:(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22) =>
      for (f1 <- e1.encode(p._1); f2 <- e2.encode(p._2); f3 <- e3.encode(p._3); f4 <- e4.encode(p._4); f5 <- e5.encode(p._5); f6 <- e6.encode(p._6); f7 <- e7.encode(p._7); f8 <- e8.encode(p._8); f9 <- e9.encode(p._9); f10 <- e10.encode(p._10); f11 <- e11.encode(p._11); f12 <- e12.encode(p._12); f13 <- e13.encode(p._13); f14 <- e14.encode(p._14); f15 <- e15.encode(p._15); f16 <- e16.encode(p._16); f17 <- e17.encode(p._17); f18 <- e18.encode(p._18); f19 <- e19.encode(p._19); f20 <- e20.encode(p._20); f21 <- e21.encode(p._21); f22 <- e22.encode(p._22)) yield {
        LdObject(f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10 ++ f11 ++ f12 ++ f13 ++ f14 ++ f15 ++ f16 ++ f17 ++ f18 ++ f19 ++ f20 ++ f21 ++ f22)
      }
    }
  }

}

