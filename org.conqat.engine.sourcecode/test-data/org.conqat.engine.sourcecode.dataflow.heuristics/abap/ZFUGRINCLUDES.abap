*&---------------------------------------------------------------------*
*& Report  ZFUGRINCLUDES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfugrincludes.

DATA: includes TYPE prognames,
      include TYPE progname.


CALL FUNCTION 'GET_INCLUDES'
  EXPORTING
    progname = 'SAPLZCQSE_CONQAT_RFC'
  TABLES
    incltab  = includes.



*SELECT progname INTO TABLE includes
*  FROM reposrc
*  FOR ALL ENTRIES IN includes
*  WHERE progname EQ includes-table_line
*  AND progname LIKE 'LZCQSE_CONQAT_RFC%'
*  AND progname NOT LIKE '%UXX'
*  AND progname NOT LIKE '%$__'
*  AND r3state EQ 'A'.


LOOP AT includes INTO include.
  WRITE / include.
ENDLOOP.