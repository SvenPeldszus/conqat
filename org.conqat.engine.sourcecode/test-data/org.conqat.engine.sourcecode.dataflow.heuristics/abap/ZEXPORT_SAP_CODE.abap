*&---------------------------------------------------------------------*
*& Report  ZEXPORT_SAP_CODE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZEXPORT_SAP_CODE.


type-pools abap.
data:
      exporter type ref to zcl_cqse_abap_exporter.


CREATE OBJECT exporter
  EXPORTING
    file_name = 'c:\temp\abap\basis.zip'.


* exporter->is_toplevel_dir_by_progtype = abap_true.

*exporter->add_ci_variant(
*    variant_user = 'BCUSER'
*    variant      = 'DEMO_DEFAULT'
*).

exporter->add_by_package(
 package = 'BASIS'
).


*exporter->add_by_name_pattern(
*  pattern = 'Z%'
*  ).
*exporter->add_by_name_pattern(
*  pattern = 'Y%'
*  ).

exporter->save( ).