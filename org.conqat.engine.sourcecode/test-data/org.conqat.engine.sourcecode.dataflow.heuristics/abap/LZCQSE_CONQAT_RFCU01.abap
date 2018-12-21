FUNCTION Z_CQSE_EXPORT_PACKAGE.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(ZIP_CONTENT) TYPE  XSTRING
*"  TABLES
*"      PACKAGES TYPE  TRDEVCLASSES1
*"--------------------------------------------------------------------
data:
        exporter type ref to zcl_cqse_abap_exporter.

  create object exporter.

* exporter->is_toplevel_dir_by_progtype = abap_true.

  exporter->add_by_package_table(
    packages = packages[]
*  include_subpackages = abap_true "default: true
    ).

  zip_content = exporter->save( ).





ENDFUNCTION.