*&---------------------------------------------------------------------*
*& Report  ZCQSE_ABAP_EXPORT
*&
*&---------------------------------------------------------------------*
*& This is a template for exporting ABAP code.
*& See in-line comments for export configuration
*&---------------------------------------------------------------------*

report  zcqse_swm_abap_export.

type-pools abap.
data:
      exporter type ref to zcl_cqse_abap_exporter.


* Adjust file_name path if appropriate
CREATE OBJECT exporter
  EXPORTING
    file_name = '\\svconqat01\export\zisu\abap_src.zip'.

* comment the following line, if devclasses should be the top-level directory
exporter->is_toplevel_dir_by_progtype = abap_true.

* uncomment if code inspector should run
*exporter->add_ci_variant(
*    variant_user = 'BCUSER'
*    variant      = 'DEMO_DEFAULT'
*).


* Select ABAP source code by package (incl. child packages)
*exporter->add_by_package(
* package = 'ZISU'
*).


* Select ABAP source by development objects name pattern
* Example all Z* and Y*  objects
exporter->add_by_name_pattern(
  pattern = 'Z%'
  ).
exporter->add_by_name_pattern(
  pattern = 'Y%'
  ).


exporter->save(
  save_to_server = abap_false
).