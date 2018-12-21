package org.conqat.engine.text.comments.analysis;

/*package*/ class JavaCommentClassifier extends CommentClassifierBase  {

  @Override protected double classify(Object[] i)
     {

    double p = Double.NaN;
    p = JavaCommentClassifier.N30a1718e0(i);
    return p;
  }
  static double N30a1718e0(Object []i) {
    double p = Double.NaN;
    if (i[0] == null) {
      p = 2;
    } else if (((Double) i[0]).doubleValue() <= 0.0) {
    p = JavaCommentClassifier.N37263a931(i);
    } else if (((Double) i[0]).doubleValue() > 0.0) {
      p = 0;
    } 
    return p;
  }
  static double N37263a931(Object []i) {
    double p = Double.NaN;
    if (i[4] == null) {
      p = 2;
    } else if (((Double) i[4]).doubleValue() <= 1.0) {
    p = JavaCommentClassifier.N2ecfa52e2(i);
    } else if (((Double) i[4]).doubleValue() > 1.0) {
    p = JavaCommentClassifier.N24e1549712(i);
    } 
    return p;
  }
  static double N2ecfa52e2(Object []i) {
    double p = Double.NaN;
    if (i[4] == null) {
      p = 1;
    } else if (((Double) i[4]).doubleValue() <= 0.0) {
    p = JavaCommentClassifier.N426587043(i);
    } else if (((Double) i[4]).doubleValue() > 0.0) {
    p = JavaCommentClassifier.N283ed3f55(i);
    } 
    return p;
  }
  static double N426587043(Object []i) {
    double p = Double.NaN;
    if (i[8] == null) {
      p = 1;
    } else if (((Double) i[8]).doubleValue() <= 6.0) {
    p = JavaCommentClassifier.N178339aa4(i);
    } else if (((Double) i[8]).doubleValue() > 6.0) {
      p = 6;
    } 
    return p;
  }
  static double N178339aa4(Object []i) {
    double p = Double.NaN;
    if (i[16] == null) {
      p = 1;
    } else if (((Double) i[16]).doubleValue() <= 0.0) {
      p = 1;
    } else if (((Double) i[16]).doubleValue() > 0.0) {
      p = 4;
    } 
    return p;
  }
  static double N283ed3f55(Object []i) {
    double p = Double.NaN;
    if (i[9] == null) {
      p = 2;
    } else if (((Double) i[9]).doubleValue() <= 0.309231) {
    p = JavaCommentClassifier.N5ea479626(i);
    } else if (((Double) i[9]).doubleValue() > 0.309231) {
      p = 4;
    } 
    return p;
  }
  static double N5ea479626(Object []i) {
    double p = Double.NaN;
    if (i[2] == null) {
      p = 2;
    } else if (((Double) i[2]).doubleValue() <= 0.0) {
    p = JavaCommentClassifier.N1d41797(i);
    } else if (((Double) i[2]).doubleValue() > 0.0) {
    p = JavaCommentClassifier.N6e5ee6a610(i);
    } 
    return p;
  }
  static double N1d41797(Object []i) {
    double p = Double.NaN;
    if (i[16] == null) {
      p = 2;
    } else if (((Double) i[16]).doubleValue() <= 0.0) {
    p = JavaCommentClassifier.Nb56efe8(i);
    } else if (((Double) i[16]).doubleValue() > 0.0) {
      p = 6;
    } 
    return p;
  }
  static double Nb56efe8(Object []i) {
    double p = Double.NaN;
    if (i[6] == null) {
      p = 4;
    } else if (((Double) i[6]).doubleValue() <= 0.4) {
    p = JavaCommentClassifier.N789e60f9(i);
    } else if (((Double) i[6]).doubleValue() > 0.4) {
      p = 2;
    } 
    return p;
  }
  static double N789e60f9(Object []i) {
    double p = Double.NaN;
    if (i[5] == null) {
      p = 4;
    } else if (((Double) i[5]).doubleValue() <= 4.0) {
      p = 4;
    } else if (((Double) i[5]).doubleValue() > 4.0) {
      p = 2;
    } 
    return p;
  }
  static double N6e5ee6a610(Object []i) {
    double p = Double.NaN;
    if (i[5] == null) {
      p = 2;
    } else if (((Double) i[5]).doubleValue() <= -1.0) {
    p = JavaCommentClassifier.N1878d4e611(i);
    } else if (((Double) i[5]).doubleValue() > -1.0) {
      p = 2;
    } 
    return p;
  }
  static double N1878d4e611(Object []i) {
    double p = Double.NaN;
    if (i[14] == null) {
      p = 2;
    } else if (((Double) i[14]).doubleValue() <= 0.0) {
      p = 2;
    } else if (((Double) i[14]).doubleValue() > 0.0) {
      p = 4;
    } 
    return p;
  }
  static double N24e1549712(Object []i) {
    double p = Double.NaN;
    if (i[6] == null) {
      p = 3;
    } else if (((Double) i[6]).doubleValue() <= 0.307692) {
    p = JavaCommentClassifier.N4578986b13(i);
    } else if (((Double) i[6]).doubleValue() > 0.307692) {
    p = JavaCommentClassifier.N705ebc6e14(i);
    } 
    return p;
  }
  static double N4578986b13(Object []i) {
    double p = Double.NaN;
    if (i[10] == null) {
      p = 3;
    } else if (((Double) i[10]).doubleValue() <= 2.0) {
      p = 3;
    } else if (((Double) i[10]).doubleValue() > 2.0) {
      p = 6;
    } 
    return p;
  }
  static double N705ebc6e14(Object []i) {
    double p = Double.NaN;
    if (i[9] == null) {
      p = 3;
    } else if (((Double) i[9]).doubleValue() <= 0.099174) {
      p = 3;
    } else if (((Double) i[9]).doubleValue() > 0.099174) {
      p = 6;
    } 
    return p;
  }
}

