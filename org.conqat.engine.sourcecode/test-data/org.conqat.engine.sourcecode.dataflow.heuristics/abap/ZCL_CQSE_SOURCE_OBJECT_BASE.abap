*----------------------------------------------------------------------*
*       CLASS ZCL_CQSE_SOURCE_OBJECT_BASE DEFINITION
*----------------------------------------------------------------------*
* Base class for source code objects which should be exported
* and which refer to an entry in TADIR.
*----------------------------------------------------------------------*
CLASS zcl_cqse_source_object_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_cqse_source_object .

    METHODS constructor
      IMPORTING
        !tadir_entry TYPE tadir
        !exporter TYPE REF TO zcl_cqse_abap_exporter
        !file_object_name TYPE string OPTIONAL .
  PROTECTED SECTION.

    DATA tadir_entry TYPE tadir .
    DATA exporter TYPE REF TO zcl_cqse_abap_exporter .
  PRIVATE SECTION.

    DATA file_object_name TYPE string .
endclass. "ZCL_CQSE_SOURCE_OBJECT_BASE definition



*----------------------------------------------------------------------*
*       class ZCL_CQSE_SOURCE_OBJECT_BASE implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_SOURCE_OBJECT_BASE implementation.


  METHOD constructor.
    me->tadir_entry = tadir_entry.
    me->exporter = exporter.
    me->file_object_name = file_object_name.
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD zif_cqse_source_object~get_name.
    name = tadir_entry-obj_name.
  ENDMETHOD.                    "zif_cqse_source_object~get_name


  METHOD zif_cqse_source_object~get_source_file_name.

    DATA:
        tidy_devclass TYPE string,
        tidy_file_name TYPE string,
        base_file_name TYPE string.

    base_file_name = me->tadir_entry-obj_name.

    IF ( me->file_object_name IS NOT INITIAL ).
      base_file_name = me->file_object_name.
    ENDIF.

    tidy_devclass = exporter->tidy_file_name( me->tadir_entry-devclass ).
    tidy_file_name = exporter->tidy_file_name( base_file_name ).

    file_name = exporter->build_file_name(
      package = tidy_devclass
      source_object_type = tadir_entry-object
      base_file_name = tidy_file_name
    ).

  ENDMETHOD.                    "zif_cqse_source_object~get_source_file_name
endclass. "ZCL_CQSE_SOURCE_OBJECT_BASE implementation