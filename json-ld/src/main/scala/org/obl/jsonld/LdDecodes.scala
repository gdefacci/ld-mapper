package org.obl.jsonld

trait LdDecodes {

  def apply[T1](p1:LdFieldDecode[T1]):LdDecode[T1] = {
    LdDecode[T1] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel)) yield {
        r1
      }
    } 
  }

  def apply[T1,T2](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2]):LdDecode[(T1,T2)] = {
    LdDecode[(T1,T2)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel)) yield {
        (r1,r2)
      }
    } 
  }

  def apply[T1,T2,T3](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3]):LdDecode[(T1,T2,T3)] = {
    LdDecode[(T1,T2,T3)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel)) yield {
        (r1,r2,r3)
      }
    } 
  }

  def apply[T1,T2,T3,T4](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4]):LdDecode[(T1,T2,T3,T4)] = {
    LdDecode[(T1,T2,T3,T4)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel)) yield {
        (r1,r2,r3,r4)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5]):LdDecode[(T1,T2,T3,T4,T5)] = {
    LdDecode[(T1,T2,T3,T4,T5)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel)) yield {
        (r1,r2,r3,r4,r5)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6]):LdDecode[(T1,T2,T3,T4,T5,T6)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7]):LdDecode[(T1,T2,T3,T4,T5,T6,T7)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11], p12:LdFieldDecode[T12]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel); r12 <- p12(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11], p12:LdFieldDecode[T12], p13:LdFieldDecode[T13]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel); r12 <- p12(jsonModel); r13 <- p13(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11], p12:LdFieldDecode[T12], p13:LdFieldDecode[T13], p14:LdFieldDecode[T14]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel); r12 <- p12(jsonModel); r13 <- p13(jsonModel); r14 <- p14(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11], p12:LdFieldDecode[T12], p13:LdFieldDecode[T13], p14:LdFieldDecode[T14], p15:LdFieldDecode[T15]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel); r12 <- p12(jsonModel); r13 <- p13(jsonModel); r14 <- p14(jsonModel); r15 <- p15(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11], p12:LdFieldDecode[T12], p13:LdFieldDecode[T13], p14:LdFieldDecode[T14], p15:LdFieldDecode[T15], p16:LdFieldDecode[T16]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel); r12 <- p12(jsonModel); r13 <- p13(jsonModel); r14 <- p14(jsonModel); r15 <- p15(jsonModel); r16 <- p16(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11], p12:LdFieldDecode[T12], p13:LdFieldDecode[T13], p14:LdFieldDecode[T14], p15:LdFieldDecode[T15], p16:LdFieldDecode[T16], p17:LdFieldDecode[T17]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel); r12 <- p12(jsonModel); r13 <- p13(jsonModel); r14 <- p14(jsonModel); r15 <- p15(jsonModel); r16 <- p16(jsonModel); r17 <- p17(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11], p12:LdFieldDecode[T12], p13:LdFieldDecode[T13], p14:LdFieldDecode[T14], p15:LdFieldDecode[T15], p16:LdFieldDecode[T16], p17:LdFieldDecode[T17], p18:LdFieldDecode[T18]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel); r12 <- p12(jsonModel); r13 <- p13(jsonModel); r14 <- p14(jsonModel); r15 <- p15(jsonModel); r16 <- p16(jsonModel); r17 <- p17(jsonModel); r18 <- p18(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11], p12:LdFieldDecode[T12], p13:LdFieldDecode[T13], p14:LdFieldDecode[T14], p15:LdFieldDecode[T15], p16:LdFieldDecode[T16], p17:LdFieldDecode[T17], p18:LdFieldDecode[T18], p19:LdFieldDecode[T19]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel); r12 <- p12(jsonModel); r13 <- p13(jsonModel); r14 <- p14(jsonModel); r15 <- p15(jsonModel); r16 <- p16(jsonModel); r17 <- p17(jsonModel); r18 <- p18(jsonModel); r19 <- p19(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11], p12:LdFieldDecode[T12], p13:LdFieldDecode[T13], p14:LdFieldDecode[T14], p15:LdFieldDecode[T15], p16:LdFieldDecode[T16], p17:LdFieldDecode[T17], p18:LdFieldDecode[T18], p19:LdFieldDecode[T19], p20:LdFieldDecode[T20]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel); r12 <- p12(jsonModel); r13 <- p13(jsonModel); r14 <- p14(jsonModel); r15 <- p15(jsonModel); r16 <- p16(jsonModel); r17 <- p17(jsonModel); r18 <- p18(jsonModel); r19 <- p19(jsonModel); r20 <- p20(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11], p12:LdFieldDecode[T12], p13:LdFieldDecode[T13], p14:LdFieldDecode[T14], p15:LdFieldDecode[T15], p16:LdFieldDecode[T16], p17:LdFieldDecode[T17], p18:LdFieldDecode[T18], p19:LdFieldDecode[T19], p20:LdFieldDecode[T20], p21:LdFieldDecode[T21]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel); r12 <- p12(jsonModel); r13 <- p13(jsonModel); r14 <- p14(jsonModel); r15 <- p15(jsonModel); r16 <- p16(jsonModel); r17 <- p17(jsonModel); r18 <- p18(jsonModel); r19 <- p19(jsonModel); r20 <- p20(jsonModel); r21 <- p21(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21)
      }
    } 
  }

  def apply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22](p1:LdFieldDecode[T1], p2:LdFieldDecode[T2], p3:LdFieldDecode[T3], p4:LdFieldDecode[T4], p5:LdFieldDecode[T5], p6:LdFieldDecode[T6], p7:LdFieldDecode[T7], p8:LdFieldDecode[T8], p9:LdFieldDecode[T9], p10:LdFieldDecode[T10], p11:LdFieldDecode[T11], p12:LdFieldDecode[T12], p13:LdFieldDecode[T13], p14:LdFieldDecode[T14], p15:LdFieldDecode[T15], p16:LdFieldDecode[T16], p17:LdFieldDecode[T17], p18:LdFieldDecode[T18], p19:LdFieldDecode[T19], p20:LdFieldDecode[T20], p21:LdFieldDecode[T21], p22:LdFieldDecode[T22]):LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)] = {
    LdDecode[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)] { (jsonModel:JsonLdModel) =>
      for (r1 <- p1(jsonModel); r2 <- p2(jsonModel); r3 <- p3(jsonModel); r4 <- p4(jsonModel); r5 <- p5(jsonModel); r6 <- p6(jsonModel); r7 <- p7(jsonModel); r8 <- p8(jsonModel); r9 <- p9(jsonModel); r10 <- p10(jsonModel); r11 <- p11(jsonModel); r12 <- p12(jsonModel); r13 <- p13(jsonModel); r14 <- p14(jsonModel); r15 <- p15(jsonModel); r16 <- p16(jsonModel); r17 <- p17(jsonModel); r18 <- p18(jsonModel); r19 <- p19(jsonModel); r20 <- p20(jsonModel); r21 <- p21(jsonModel); r22 <- p22(jsonModel)) yield {
        (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21,r22)
      }
    } 
  }

}

