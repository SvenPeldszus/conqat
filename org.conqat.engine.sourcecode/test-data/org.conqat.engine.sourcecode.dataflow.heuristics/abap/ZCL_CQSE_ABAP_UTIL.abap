*----------------------------------------------------------------------*
*       CLASS zcl_cqse_abap_util DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_cqse_abap_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS strip_namespace
        IMPORTING
            !qualified_name TYPE csequence
        EXPORTING
            !value(namespace) TYPE string
            !value(name) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
endclass. "ZCL_CQSE_ABAP_UTIL definition



*----------------------------------------------------------------------*
*       class ZCL_CQSE_ABAP_UTIL implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_ABAP_UTIL implementation.


  METHOD strip_namespace.

    DATA p TYPE string.

    name = qualified_name.

    IF name CP '/*/*'.
      SPLIT name AT '/' INTO p namespace name.
      IF namespace IS NOT INITIAL.
        CONCATENATE '/' namespace '/' INTO namespace.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "strip_namespace
endclass. "ZCL_CQSE_ABAP_UTIL implementation