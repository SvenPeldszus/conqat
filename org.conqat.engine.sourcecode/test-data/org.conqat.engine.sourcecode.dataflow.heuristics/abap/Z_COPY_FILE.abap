*&---------------------------------------------------------------------*
*& Report  ZCQSE_MRG_ABAP_EXPORT
*&
*&---------------------------------------------------------------------*
*& This is a template for exporting ABAP code.
*& Uncomment the variant to use for ABAP export
*& set file_name and method parameters
*&---------------------------------------------------------------------*

REPORT  zcqse_mrg_abap_export.

*----------------------------------------------------------------------------
* Parmeters which must be configured for the analyzed MR system
*----------------------------------------------------------------------------

CONSTANTS:
 machine TYPE string VALUE 'abap',
 system TYPE string VALUE 'Exporter',
 package_pattern TYPE string VALUE 'ZCQSE%'.

*----------------------------------------------------------------------------



TYPE-POOLS abap.

DATA:
  file_name TYPE string,
  tmp_file_name TYPE string,
  exporter TYPE REF TO zcl_cqse_abap_exporter.

PERFORM init_filenames.

IF sy-batch EQ abap_true.
  CREATE OBJECT exporter
    EXPORTING
      file_name = tmp_file_name.
ELSE.
  CREATE OBJECT exporter
    EXPORTING
      file_name = file_name.
ENDIF.


exporter->add_by_package_pattern(
  pattern = package_pattern
  include_subpackages = abap_false
  ).

exporter->save( ).

IF sy-batch EQ abap_true.
  PERFORM  rename_tmp_to_zip.
ENDIF.




*&---------------------------------------------------------------------*
*&      Form  init_filenames
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM init_filenames.

  CONSTANTS:
   basedir_gui TYPE string VALUE '\\mucs801046.dev.munich.munichre.com\conqat_inbox',
   basedir_batch TYPE string VALUE 'c:\temp\abap\batch'.

  IF sy-batch EQ abap_true.
    CONCATENATE basedir_batch '\' machine '\soqou\zip\'
    INTO file_name.
  ELSE.
    CONCATENATE basedir_gui '\SAP_' system '\' machine '\'
    INTO file_name.
  ENDIF.

  CONCATENATE
    file_name 'abap_' machine '_' system '_' sy-datum '-' sy-uzeit
    INTO file_name.

  CONCATENATE
    file_name '.tmp' INTO tmp_file_name.
  CONCATENATE
    file_name '.zip' INTO file_name.

ENDFORM.                    "build_filenames


*&---------------------------------------------------------------------*
*&      Form  rename_tmp_to_zip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM rename_tmp_to_zip.

  DATA:
     ls_sxpgcolist     TYPE sxpgcolist,
     lt_protocol       TYPE STANDARD TABLE OF btcxpm,
     tmp_string        TYPE string.

  CONCATENATE tmp_file_name file_name INTO ls_sxpgcolist-parameters SEPARATED BY space.

  CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
    EXPORTING
      commandname           = 'Y_MOVE'
      additional_parameters = ls_sxpgcolist-parameters
    TABLES
      exec_protocol         = lt_protocol.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_protocol INTO tmp_string.
    WRITE: 'mv prot:'.
    WRITE / tmp_string.

  ENDLOOP.

ENDFORM.                    "rename_tmp_to_zip