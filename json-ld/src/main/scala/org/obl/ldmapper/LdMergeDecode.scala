package org.obl.ldmapper


object LdMergeDecode {

  def mergeDecode[T1,T2](d1:LdDecode[T1], d2:LdDecode[T2]): LdDecode[(T1,T2)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm)
      ) yield (o1, o2)
    }
  }  
  def mergeDecode[T1,T2,T3](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3]): LdDecode[(T1,T2,T3)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm)
      ) yield (o1, o2, o3)
    }
  }  
  def mergeDecode[T1,T2,T3,T4](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4]): LdDecode[(T1,T2,T3,T4)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm)
      ) yield (o1, o2, o3, o4)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5]): LdDecode[(T1,T2,T3,T4,T5)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm)
      ) yield (o1, o2, o3, o4, o5)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6]): LdDecode[(T1,T2,T3,T4,T5,T6)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7]): LdDecode[(T1,T2,T3,T4,T5,T6,T7)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11], d12:LdDecode[T12]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm); o12 <- d12.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11], d12:LdDecode[T12], d13:LdDecode[T13]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm); o12 <- d12.decode(jsm); o13 <- d13.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11], d12:LdDecode[T12], d13:LdDecode[T13], d14:LdDecode[T14]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm); o12 <- d12.decode(jsm); o13 <- d13.decode(jsm); o14 <- d14.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11], d12:LdDecode[T12], d13:LdDecode[T13], d14:LdDecode[T14], d15:LdDecode[T15]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm); o12 <- d12.decode(jsm); o13 <- d13.decode(jsm); o14 <- d14.decode(jsm); o15 <- d15.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11], d12:LdDecode[T12], d13:LdDecode[T13], d14:LdDecode[T14], d15:LdDecode[T15], d16:LdDecode[T16]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm); o12 <- d12.decode(jsm); o13 <- d13.decode(jsm); o14 <- d14.decode(jsm); o15 <- d15.decode(jsm); o16 <- d16.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11], d12:LdDecode[T12], d13:LdDecode[T13], d14:LdDecode[T14], d15:LdDecode[T15], d16:LdDecode[T16], d17:LdDecode[T17]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm); o12 <- d12.decode(jsm); o13 <- d13.decode(jsm); o14 <- d14.decode(jsm); o15 <- d15.decode(jsm); o16 <- d16.decode(jsm); o17 <- d17.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11], d12:LdDecode[T12], d13:LdDecode[T13], d14:LdDecode[T14], d15:LdDecode[T15], d16:LdDecode[T16], d17:LdDecode[T17], d18:LdDecode[T18]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm); o12 <- d12.decode(jsm); o13 <- d13.decode(jsm); o14 <- d14.decode(jsm); o15 <- d15.decode(jsm); o16 <- d16.decode(jsm); o17 <- d17.decode(jsm); o18 <- d18.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11], d12:LdDecode[T12], d13:LdDecode[T13], d14:LdDecode[T14], d15:LdDecode[T15], d16:LdDecode[T16], d17:LdDecode[T17], d18:LdDecode[T18], d19:LdDecode[T19]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm); o12 <- d12.decode(jsm); o13 <- d13.decode(jsm); o14 <- d14.decode(jsm); o15 <- d15.decode(jsm); o16 <- d16.decode(jsm); o17 <- d17.decode(jsm); o18 <- d18.decode(jsm); o19 <- d19.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18, o19)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11], d12:LdDecode[T12], d13:LdDecode[T13], d14:LdDecode[T14], d15:LdDecode[T15], d16:LdDecode[T16], d17:LdDecode[T17], d18:LdDecode[T18], d19:LdDecode[T19], d20:LdDecode[T20]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm); o12 <- d12.decode(jsm); o13 <- d13.decode(jsm); o14 <- d14.decode(jsm); o15 <- d15.decode(jsm); o16 <- d16.decode(jsm); o17 <- d17.decode(jsm); o18 <- d18.decode(jsm); o19 <- d19.decode(jsm); o20 <- d20.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18, o19, o20)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11], d12:LdDecode[T12], d13:LdDecode[T13], d14:LdDecode[T14], d15:LdDecode[T15], d16:LdDecode[T16], d17:LdDecode[T17], d18:LdDecode[T18], d19:LdDecode[T19], d20:LdDecode[T20], d21:LdDecode[T21]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm); o12 <- d12.decode(jsm); o13 <- d13.decode(jsm); o14 <- d14.decode(jsm); o15 <- d15.decode(jsm); o16 <- d16.decode(jsm); o17 <- d17.decode(jsm); o18 <- d18.decode(jsm); o19 <- d19.decode(jsm); o20 <- d20.decode(jsm); o21 <- d21.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18, o19, o20, o21)
    }
  }  
  def mergeDecode[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22](d1:LdDecode[T1], d2:LdDecode[T2], d3:LdDecode[T3], d4:LdDecode[T4], d5:LdDecode[T5], d6:LdDecode[T6], d7:LdDecode[T7], d8:LdDecode[T8], d9:LdDecode[T9], d10:LdDecode[T10], d11:LdDecode[T11], d12:LdDecode[T12], d13:LdDecode[T13], d14:LdDecode[T14], d15:LdDecode[T15], d16:LdDecode[T16], d17:LdDecode[T17], d18:LdDecode[T18], d19:LdDecode[T19], d20:LdDecode[T20], d21:LdDecode[T21], d22:LdDecode[T22]): LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)] = {
    LdDecode { jsm: JsonLdModel =>
      for ( o1 <- d1.decode(jsm); o2 <- d2.decode(jsm); o3 <- d3.decode(jsm); o4 <- d4.decode(jsm); o5 <- d5.decode(jsm); o6 <- d6.decode(jsm); o7 <- d7.decode(jsm); o8 <- d8.decode(jsm); o9 <- d9.decode(jsm); o10 <- d10.decode(jsm); o11 <- d11.decode(jsm); o12 <- d12.decode(jsm); o13 <- d13.decode(jsm); o14 <- d14.decode(jsm); o15 <- d15.decode(jsm); o16 <- d16.decode(jsm); o17 <- d17.decode(jsm); o18 <- d18.decode(jsm); o19 <- d19.decode(jsm); o20 <- d20.decode(jsm); o21 <- d21.decode(jsm); o22 <- d22.decode(jsm)
      ) yield (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18, o19, o20, o21, o22)
    }
  }  
}

