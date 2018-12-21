*&---------------------------------------------------------------------*
*& Report  ZCQSE_ABAP_EXPORT
*&
*&---------------------------------------------------------------------*
*& This is a template for exporting ABAP code.
*& Uncomment the variant to use for ABAP export
*& set file_name and method parameters
*&---------------------------------------------------------------------*

REPORT  zcqse_abap_export.

TYPE-POOLS: abap.

DATA:
      file_name TYPE string,
      object_patterns TYPE string_table,
      package_patterns TYPE string_table,
      package_names TYPE trdevclasses1,
      test type string.

* TODO (CP): Provide selection screen for parameters.

test = 'Das ist ein &#0; dummy text soll code inspector finden'.

PERFORM main.

*&---------------------------------------------------------------------*
*&      Form  main
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM main.
  DATA:
        exporter TYPE REF TO zcl_cqse_abap_exporter.


  CREATE OBJECT exporter
    EXPORTING
      file_name = 'D:\svn_local\conqat-cqse\engine\eu.cqse.conqat.engine.abap\test-abap-sources\zy_all.zip'.

* exporter->is_toplevel_dir_by_progtype = abap_true.

  exporter->add_ci_variant(
      variant_user = 'BCUSER'
      variant      = 'TQE_RULESET'
  ).

  exporter->add_by_name_pattern(
    pattern = 'Z%'
    ).
  exporter->add_by_name_pattern(
    pattern = 'Y%'
    ).

  exporter->save( ).


ENDFORM.                    "main