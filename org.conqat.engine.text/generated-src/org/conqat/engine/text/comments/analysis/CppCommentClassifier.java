package org.conqat.engine.text.comments.analysis;

/*package*/ class CppCommentClassifier extends CommentClassifierBase  {

  @Override protected double classify(Object[] i)
     {

    double p = Double.NaN;
    p = CppCommentClassifier.N37263a930(i);
    return p;
  }
  static double N37263a930(Object []i) {
    double p = Double.NaN;
    if (i[4] == null) {
      p = 2;
    } else if (((Double) i[4]).doubleValue() <= 1.0) {
    p = CppCommentClassifier.N2ecfa52e1(i);
    } else if (((Double) i[4]).doubleValue() > 1.0) {
    p = CppCommentClassifier.N1878d4e610(i);
    } 
    return p;
  }
  static double N2ecfa52e1(Object []i) {
    double p = Double.NaN;
    if (i[0] == null) {
      p = 2;
    } else if (((Double) i[0]).doubleValue() <= 0.0) {
    p = CppCommentClassifier.N426587042(i);
    } else if (((Double) i[0]).doubleValue() > 0.0) {
      p = 0;
    } 
    return p;
  }
  static double N426587042(Object []i) {
    double p = Double.NaN;
    if (i[1] == null) {
      p = 2;
    } else if (((Double) i[1]).doubleValue() <= 0.0) {
    p = CppCommentClassifier.N178339aa3(i);
    } else if (((Double) i[1]).doubleValue() > 0.0) {
    p = CppCommentClassifier.N6e5ee6a69(i);
    } 
    return p;
  }
  static double N178339aa3(Object []i) {
    double p = Double.NaN;
    if (i[15] == null) {
      p = 2;
    } else if (((Double) i[15]).doubleValue() <= 0.0) {
    p = CppCommentClassifier.N283ed3f54(i);
    } else if (((Double) i[15]).doubleValue() > 0.0) {
    p = CppCommentClassifier.N789e60f8(i);
    } 
    return p;
  }
  static double N283ed3f54(Object []i) {
    double p = Double.NaN;
    if (i[12] == null) {
      p = 2;
    } else if (((Double) i[12]).doubleValue() <= 70.0) {
    p = CppCommentClassifier.N34cd8e775(i);
    } else if (((Double) i[12]).doubleValue() > 70.0) {
      p = 4;
    } 
    return p;
  }
  static double N34cd8e775(Object []i) {
    double p = Double.NaN;
    if (i[8] == null) {
      p = 2;
    } else if (((Double) i[8]).doubleValue() <= 2.0) {
    p = CppCommentClassifier.N234441b66(i);
    } else if (((Double) i[8]).doubleValue() > 2.0) {
      p = 6;
    } 
    return p;
  }
  static double N234441b66(Object []i) {
    double p = Double.NaN;
    if (i[16] == null) {
      p = 2;
    } else if (((Double) i[16]).doubleValue() <= 0.0) {
      p = 2;
    } else if (((Double) i[16]).doubleValue() > 0.0) {
    p = CppCommentClassifier.N563625d07(i);
    } 
    return p;
  }
  static double N563625d07(Object []i) {
    double p = Double.NaN;
    if (i[6] == null) {
      p = 4;
    } else if (((Double) i[6]).doubleValue() <= 0.809524) {
      p = 4;
    } else if (((Double) i[6]).doubleValue() > 0.809524) {
      p = 2;
    } 
    return p;
  }
  static double N789e60f8(Object []i) {
    double p = Double.NaN;
    if (i[5] == null) {
      p = 0;
    } else if (((Double) i[5]).doubleValue() <= 0.0) {
      p = 0;
    } else if (((Double) i[5]).doubleValue() > 0.0) {
      p = 2;
    } 
    return p;
  }
  static double N6e5ee6a69(Object []i) {
    double p = Double.NaN;
    if (i[14] == null) {
      p = 1;
    } else if (((Double) i[14]).doubleValue() <= 0.0) {
      p = 1;
    } else if (((Double) i[14]).doubleValue() > 0.0) {
      p = 0;
    } 
    return p;
  }
  static double N1878d4e610(Object []i) {
    double p = Double.NaN;
    if (i[6] == null) {
      p = 3;
    } else if (((Double) i[6]).doubleValue() <= 0.555556) {
      p = 3;
    } else if (((Double) i[6]).doubleValue() > 0.555556) {
    p = CppCommentClassifier.N24e1549711(i);
    } 
    return p;
  }
  static double N24e1549711(Object []i) {
    double p = Double.NaN;
    if (i[12] == null) {
      p = 6;
    } else if (((Double) i[12]).doubleValue() <= 3.0) {
    p = CppCommentClassifier.N4578986b12(i);
    } else if (((Double) i[12]).doubleValue() > 3.0) {
    p = CppCommentClassifier.N705ebc6e13(i);
    } 
    return p;
  }
  static double N4578986b12(Object []i) {
    double p = Double.NaN;
    if (i[9] == null) {
      p = 3;
    } else if (((Double) i[9]).doubleValue() <= 0.111607) {
      p = 3;
    } else if (((Double) i[9]).doubleValue() > 0.111607) {
      p = 6;
    } 
    return p;
  }
  static double N705ebc6e13(Object []i) {
    double p = Double.NaN;
    if (i[7] == null) {
      p = 3;
    } else if (((Double) i[7]).doubleValue() <= 3.0) {
      p = 3;
    } else if (((Double) i[7]).doubleValue() > 3.0) {
      p = 6;
    } 
    return p;
  }
}

