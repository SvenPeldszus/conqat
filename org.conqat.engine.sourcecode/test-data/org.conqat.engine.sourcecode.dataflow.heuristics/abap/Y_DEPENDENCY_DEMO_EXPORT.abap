*&---------------------------------------------------------------------*
*& Report  Y_DEPENDENCY_DEMO_EXPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  y_dependency_demo_export.


TYPE-POOLS abap.

PERFORM main.

*&---------------------------------------------------------------------*
*&      Form  main
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM main.

  DATA:
        exporter TYPE REF TO zcl_cqse_abap_exporter.

  INCLUDE y_demo_include.

  CREATE OBJECT exporter
    EXPORTING
      file_name = 'D:\svn_local\conqat-cqse\engine\eu.cqse.conqat.engine.abap\test-data\dependency_demo.zip'.

  exporter->add_by_package(
    package = 'ZCQSE'
    ).

  PERFORM add_package_hierarchy USING exporter.
*  PERFORM add_function_module_packages USING exporter.

  exporter->save( ).

ENDFORM.                    "main


*&---------------------------------------------------------------------*
*&      Form  ADD_PACKAGE_HIERARCHY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EXPORTER  text
*----------------------------------------------------------------------*
FORM add_package_hierarchy  USING    value(exporter) TYPE REF TO zcl_cqse_abap_exporter.
  DATA:
        BEGIN OF package,
          devclass LIKE tdevc-devclass,
          parent LIKE tdevc-parentcl,
        END OF package,
        packages LIKE TABLE OF package.

  SELECT devclass parentcl FROM tdevc INTO TABLE packages.


  exporter->add_table(
    EXPORTING
      table     = packages
      file_name = 'packages'
  ).



ENDFORM.                    " ADD_PACKAGE_HIERARCHY


*&---------------------------------------------------------------------*
*&      Form  add_function_module_packages
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE      text
*      -->(EXPORTER) text
*----------------------------------------------------------------------*
FORM add_function_module_packages USING value(exporter) TYPE REF TO zcl_cqse_abap_exporter.
  DATA:
       BEGIN OF fmod,
         function LIKE info_func-funcname,
         fgroup LIKE info_func-area,
         devclass LIKE info_func-devclass,
       END OF fmod,
       function_modules LIKE TABLE OF fmod.

  SELECT funcname area devclass FROM info_func INTO TABLE function_modules.


  exporter->add_table(
    EXPORTING
      table     = function_modules
      file_name = 'function_modules'
  ).

ENDFORM.                    "add_function_module_packages