*&---------------------------------------------------------------------*
*& Report  Y_DEMO_B_PROG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  y_demo_a_prog.

* include from own package
INCLUDE y_demo_include.

* function call to other package
CALL FUNCTION 'Y_DEMO_B_FUNCTION'
  EXPORTING
    called_from = 'Y_DEMO_A_PROG'.