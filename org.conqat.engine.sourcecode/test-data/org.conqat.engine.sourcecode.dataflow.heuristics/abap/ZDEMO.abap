*&---------------------------------------------------------------------*
*& Report  ZDEMO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zdemo.

DATA:
       entry TYPE tadir,
       itab TYPE STANDARD TABLE OF tadir-obj_name,
       object type tadir-obj_name.

SELECT * FROM tadir INTO entry
  WHERE devclass LIKE 'ZCQSE%'.

  WRITE: / entry-object, entry-obj_name.

ENDSELECT.



SELECT obj_name FROM tadir INTO TABLE itab.

loop at itab into object.
  " do something with line of itab
endloop.