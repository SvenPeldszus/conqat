*&---------------------------------------------------------------------*
*& Report  ZCQSE_MRG_ABAP_EXPORT
*&
*&---------------------------------------------------------------------*
*& Exports ABAP Code for ConQAT analysis.
*& (c) 2013 CQSE GmbH
*&---------------------------------------------------------------------*

REPORT  zcqse_mrg_abap_export.

TYPE-POOLS abap.

*----------------------------------------------------------------------------
* Parmeters which must be configured for the analyzed MR system
*----------------------------------------------------------------------------

CONSTANTS:
  export_all(3) TYPE c VALUE  'all',

  " if true, always write output to application server, otherwise
  " file is saved directly to conqat server (does not work in batch mode)
  force_save_to_as TYPE abap_bool VALUE abap_false.


PARAMETERS:
  " name of the application, if 'all' all custom code /MRG/*, Y*, Z* will be
  " exported
  applname TYPE string DEFAULT export_all LOWER CASE,

  " pattern for package names to export, ignored if application is 'all'
  packages TYPE string,

  " enable code inspector analysis
  exec_ci TYPE abap_bool DEFAULT abap_true AS CHECKBOX,

  " variant for code inspector
  ci_var TYPE sci_chkv DEFAULT 'TQE_RULESET',
  ci_user TYPE syuname DEFAULT 'NY25163'.

DATA:
    machine TYPE string.

machine = sy-sysid.

*----------------------------------------------------------------------------


PERFORM main.


*&---------------------------------------------------------------------*
*&      Form  main
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM main.

  DATA:
    file_name TYPE string,
    tmp_file_name TYPE string,
    exporter TYPE REF TO zcl_cqse_abap_exporter,
    save_to_as TYPE abap_bool.

  IF force_save_to_as EQ abap_true.
    save_to_as = abap_true.
  ELSE.
    save_to_as = sy-batch.
  ENDIF.

  PERFORM init_filenames
    USING save_to_as
    CHANGING file_name tmp_file_name.

  IF save_to_as EQ abap_true.
    CREATE OBJECT exporter
      EXPORTING
        file_name = tmp_file_name.
  ELSE.
    CREATE OBJECT exporter
      EXPORTING
        file_name = file_name.
  ENDIF.

  IF applname EQ export_all OR packages IS INITIAL.
    exporter->add_by_name_pattern('/MRG/%').
    exporter->add_by_name_pattern('Y%').
    exporter->add_by_name_pattern('Z%').
  ELSE.
    exporter->add_by_package_pattern(
      pattern = packages
      include_subpackages = abap_false
      ).
  ENDIF.

  IF exec_ci EQ abap_true.
    exporter->add_ci_variant(
        variant      = ci_var
        variant_user = ci_user
    ).
  ENDIF.

  exporter->save(
    save_to_server = save_to_as
  ).

  IF save_to_as EQ abap_true.
    PERFORM  rename_tmp_to_zip
        USING tmp_file_name file_name.
  ENDIF.

ENDFORM.                    "main




*&---------------------------------------------------------------------*
*&      Form  init_filenames
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM init_filenames
    USING
        value(save_to_server) TYPE abap_bool
    CHANGING
        file_name TYPE string
        tmp_file_name TYPE string.

  CONSTANTS:
   basedir_gui TYPE string VALUE '\\mucs801046.dev.munich.munichre.com\conqat_inbox',
   basedir_batch TYPE string VALUE '/sapexpimp/all'.

  IF save_to_server EQ abap_true.
    IF applname EQ export_all.
      CONCATENATE basedir_batch '/tqe/' INTO file_name .
    ELSE.
      CONCATENATE basedir_batch '/' machine '/soquo/zip/' INTO file_name.
    ENDIF.
  ELSE.
    IF applname EQ export_all.
      CONCATENATE basedir_gui '\abap\' INTO file_name.
    ELSE.
      CONCATENATE basedir_gui '\SAP_' applname '\' machine '\' INTO file_name.
    ENDIF.
  ENDIF.

  CONCATENATE
    file_name 'abap_' machine '_' applname '_' sy-datum '-' sy-uzeit INTO file_name.

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
FORM rename_tmp_to_zip
    USING
        value(tmp_file_name) TYPE string
        value(file_name) TYPE string.

  DATA: cmd_mv(180) TYPE c,
        response TYPE TABLE OF char200.

  CONCATENATE 'mv ' tmp_file_name ' ' file_name INTO cmd_mv RESPECTING BLANKS.

  CALL 'SYSTEM' ID 'COMMAND' FIELD cmd_mv
                ID 'TAB'     FIELD response.
ENDFORM.                    "rename_tmp_to_zip