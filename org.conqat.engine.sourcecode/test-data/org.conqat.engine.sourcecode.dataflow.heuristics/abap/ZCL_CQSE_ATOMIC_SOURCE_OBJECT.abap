*----------------------------------------------------------------------*
*       CLASS ZCL_CQSE_ATOMIC_SOURCE_OBJECT DEFINITION
*----------------------------------------------------------------------*
* A ABAP source code object which should be exported which consists
* of only one object in table REPOSRC.
*----------------------------------------------------------------------*
CLASS zcl_cqse_atomic_source_object DEFINITION
  PUBLIC
  INHERITING FROM zcl_cqse_source_object_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !tadir_entry TYPE tadir
        !exporter TYPE REF TO zcl_cqse_abap_exporter
        !file_object_name TYPE string OPTIONAL
      RAISING
        zcx_cqse_srcobj_not_exists .

    METHODS zif_cqse_source_object~get_meta_data
      REDEFINITION .
    METHODS zif_cqse_source_object~get_source
      REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA reposrc_object TYPE REF TO zcl_cqse_reposrc_object .
endclass. "ZCL_CQSE_ATOMIC_SOURCE_OBJECT definition



*----------------------------------------------------------------------*
*       class ZCL_CQSE_ATOMIC_SOURCE_OBJECT implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_ATOMIC_SOURCE_OBJECT implementation.


  METHOD constructor.
    data:
        program_name type sobj_name.

    super->constructor(
      tadir_entry = tadir_entry
      exporter = exporter
      file_object_name = file_object_name
    ).

    program_name = zif_cqse_source_object~get_name( ).

    CREATE OBJECT me->reposrc_object
      EXPORTING
        program_name = program_name
        tadir_entry  = tadir_entry
        exporter     = exporter.
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD zif_cqse_source_object~get_meta_data.
    meta_data = me->reposrc_object->zif_cqse_source_object~get_meta_data( ).
  ENDMETHOD.                    "ZIF_CQSE_SOURCE_OBJECT~GET_META_DATA


  METHOD zif_cqse_source_object~get_source.

    source = me->reposrc_object->zif_cqse_source_object~get_source( ).
  ENDMETHOD.                    "zif_cqse_source_object~get_source
endclass. "ZCL_CQSE_ATOMIC_SOURCE_OBJECT implementation