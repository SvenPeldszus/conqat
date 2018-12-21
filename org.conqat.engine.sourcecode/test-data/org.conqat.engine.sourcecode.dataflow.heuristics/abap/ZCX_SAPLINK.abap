class ZCX_SAPLINK definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  constants EXISTING type SOTR_CONC value 'E80867F4AE0B1EE2A1CE5A77D844FCC9'. "#EC NOTEXT
  constants SYSTEM_ERROR type SOTR_CONC value 'E80867F4AE0B1EE2A1CE5A77D8459CC9'. "#EC NOTEXT
  constants NOT_AUTHORIZED type SOTR_CONC value 'E80867F4AE0B1EE2A1CE5A77D8453CC9'. "#EC NOTEXT
  constants ERROR_MESSAGE type SOTR_CONC value 'E80867F4AE0B1EE2A1CE5A77D844DCC9'. "#EC NOTEXT
  constants ZCX_SAPLINK type SOTR_CONC value 'E80867F4AE0B1EE2A1CE5A77D845BCC9'. "#EC NOTEXT
  data MSG type STRING value '44F7518323DB08BC02000000A7E42BB6'. "#EC NOTEXT .
  constants NOT_FOUND type SOTR_CONC value 'E80867F4AE0B1EE2A1CE5A77D8455CC9'. "#EC NOTEXT
  constants LOCKED type SOTR_CONC value 'E80867F4AE0B1EE2A1CE5A77D8451CC9'. "#EC NOTEXT
  constants NO_PLUGIN type SOTR_CONC value 'E80867F4AE0B1EE2A1CE5A77D8457CC9'. "#EC NOTEXT
  data OBJECT type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !MSG type STRING default '44F7518323DB08BC02000000A7E42BB6'
      !OBJECT type STRING optional .
protected section.
private section.
endclass. "ZCX_SAPLINK definition



*----------------------------------------------------------------------*
*       class ZCX_SAPLINK implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCX_SAPLINK implementation.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_SAPLINK .
 ENDIF.
me->MSG = MSG .
me->OBJECT = OBJECT .
endmethod.
endclass. "ZCX_SAPLINK implementation