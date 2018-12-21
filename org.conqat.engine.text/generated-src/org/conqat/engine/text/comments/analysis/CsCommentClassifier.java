package org.conqat.engine.text.comments.analysis;

/*package*/ class CsCommentClassifier extends CommentClassifierBase  {

  @Override protected double classify(Object[] i)
     {

    double p = Double.NaN;
    p = CsCommentClassifier.N30a1718e0(i);
    return p;
  }
  static double N30a1718e0(Object []i) {
    double p = Double.NaN;
    if (i[4] == null) {
      p = 2;
    } else if (((Double) i[4]).doubleValue() <= 1.0) {
    p = CsCommentClassifier.N37263a931(i);
    } else if (((Double) i[4]).doubleValue() > 1.0) {
    p = CsCommentClassifier.N789e60f9(i);
    } 
    return p;
  }
  static double N37263a931(Object []i) {
    double p = Double.NaN;
    if (i[0] == null) {
      p = 2;
    } else if (((Double) i[0]).doubleValue() <= 0.0) {
    p = CsCommentClassifier.N2ecfa52e2(i);
    } else if (((Double) i[0]).doubleValue() > 0.0) {
      p = 0;
    } 
    return p;
  }
  static double N2ecfa52e2(Object []i) {
    double p = Double.NaN;
    if (i[4] == null) {
      p = 1;
    } else if (((Double) i[4]).doubleValue() <= 0.0) {
      p = 1;
    } else if (((Double) i[4]).doubleValue() > 0.0) {
    p = CsCommentClassifier.N426587043(i);
    } 
    return p;
  }
  static double N426587043(Object []i) {
    double p = Double.NaN;
    if (i[16] == null) {
      p = 2;
    } else if (((Double) i[16]).doubleValue() <= 0.0) {
    p = CsCommentClassifier.N178339aa4(i);
    } else if (((Double) i[16]).doubleValue() > 0.0) {
    p = CsCommentClassifier.N234441b67(i);
    } 
    return p;
  }
  static double N178339aa4(Object []i) {
    double p = Double.NaN;
    if (i[2] == null) {
      p = 2;
    } else if (((Double) i[2]).doubleValue() <= 0.0) {
    p = CsCommentClassifier.N283ed3f55(i);
    } else if (((Double) i[2]).doubleValue() > 0.0) {
      p = 2;
    } 
    return p;
  }
  static double N283ed3f55(Object []i) {
    double p = Double.NaN;
    if (i[5] == null) {
      p = 3;
    } else if (((Double) i[5]).doubleValue() <= 0.0) {
      p = 3;
    } else if (((Double) i[5]).doubleValue() > 0.0) {
    p = CsCommentClassifier.N34cd8e776(i);
    } 
    return p;
  }
  static double N34cd8e776(Object []i) {
    double p = Double.NaN;
    if (i[5] == null) {
      p = 2;
    } else if (((Double) i[5]).doubleValue() <= 44.0) {
      p = 2;
    } else if (((Double) i[5]).doubleValue() > 44.0) {
      p = 3;
    } 
    return p;
  }
  static double N234441b67(Object []i) {
    double p = Double.NaN;
    if (i[6] == null) {
      p = 4;
    } else if (((Double) i[6]).doubleValue() <= 0.5) {
      p = 4;
    } else if (((Double) i[6]).doubleValue() > 0.5) {
    p = CsCommentClassifier.Nb56efe8(i);
    } 
    return p;
  }
  static double Nb56efe8(Object []i) {
    double p = Double.NaN;
    if (i[8] == null) {
      p = 2;
    } else if (((Double) i[8]).doubleValue() <= 0.0) {
      p = 2;
    } else if (((Double) i[8]).doubleValue() > 0.0) {
      p = 6;
    } 
    return p;
  }
  static double N789e60f9(Object []i) {
    double p = Double.NaN;
    if (i[9] == null) {
      p = 3;
    } else if (((Double) i[9]).doubleValue() <= 0.098361) {
      p = 3;
    } else if (((Double) i[9]).doubleValue() > 0.098361) {
    p = CsCommentClassifier.N6e5ee6a610(i);
    } 
    return p;
  }
  static double N6e5ee6a610(Object []i) {
    double p = Double.NaN;
    if (i[6] == null) {
      p = 3;
    } else if (((Double) i[6]).doubleValue() <= 0.333333) {
      p = 3;
    } else if (((Double) i[6]).doubleValue() > 0.333333) {
      p = 6;
    } 
    return p;
  }
}

