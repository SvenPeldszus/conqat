*&---------------------------------------------------------------------*
*& Report  Y_DEMO_B_PROG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  y_demo_b_prog.


* include from other package
INCLUDE y_demo_include.


DATA:
* this object is in the same package, it does not have any method
  obj_b TYPE REF TO ycl_dependency_b,
* this object is in an other package it's a subclass of ycl_dependcy_b
* and has method my_method
  obj_a TYPE REF TO ycl_dependency_a.


* instantiate the object in the other package *
CREATE OBJECT obj_a.
* we instantiate the object with a sub-type in an other packge
CREATE OBJECT obj_b TYPE ycl_dependency_a.

* thas the usual method call
CALL METHOD obj_a->my_method
  EXPORTING
    p1 = 'obj_a in y_demo_b_prog'.

* that's actually a dynamic method call
* (but also some kind of abap cast work-around)
CALL METHOD obj_b->('MY_METHOD')
  EXPORTING
    p1 = 'obj_b in y_demo_b_prog'.


* function call to other package
* note: function module name must be given as a literal (and in upercase)
CALL FUNCTION 'Y_DEMO_B_FUNCTION'
  EXPORTING
    called_from = 'Y_DEMO_B_PROG'.