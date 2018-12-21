*----------------------------------------------------------------------*
*       CLASS ZCL_CQSE_REPOSRC_OBJECT DEFINITION
*----------------------------------------------------------------------*
* An ABAP source object as it is stored in table REPOSRC.
* In contrast to ZCL_CQSE_SOURCE_OBJECT_BASE, these source objects
* may not have an own entry in TADIR.
*----------------------------------------------------------------------*
class ZCL_CQSE_REPOSRC_OBJECT definition
  public
  final
  create public .

public section.

  interfaces ZIF_CQSE_DATATYPES .
  interfaces ZIF_CQSE_SOURCE_OBJECT .

  data PROGRAM_NAME type PROGNAME read-only .

  methods CONSTRUCTOR
    importing
      !PROGRAM_NAME type PROGNAME
      !EXPORTER type ref to ZCL_CQSE_ABAP_EXPORTER
      !TADIR_ENTRY type TADIR
    raising
      ZCX_CQSE_SRCOBJ_NOT_EXISTS .
  PROTECTED SECTION.
private section.

  data TADIR_ENTRY type TADIR .
  data EXPORTER type ref to ZCL_CQSE_ABAP_EXPORTER .

  methods OBJECT_EXISTS
    importing
      !PROGRAM_NAME type PROGNAME
    returning
      value(EXISTS) type ABAP_BOOL .
endclass. "ZCL_CQSE_REPOSRC_OBJECT definition



*----------------------------------------------------------------------*
*       class ZCL_CQSE_REPOSRC_OBJECT implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_REPOSRC_OBJECT implementation.


  METHOD constructor.

    IF object_exists( program_name ) EQ abap_false.
      RAISE EXCEPTION TYPE zcx_cqse_srcobj_not_exists.
    ENDIF.

    me->program_name = program_name.
    me->tadir_entry = tadir_entry.
    me->exporter = exporter.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD object_exists.
    DATA count TYPE i.
    SELECT COUNT(*) FROM reposrc INTO count
      WHERE progname = program_name
      AND r3state = 'A'.
    IF count < 1.
      exists = abap_false.
    ELSE.
      exists = abap_true.
    ENDIF.
  ENDMETHOD.                    "object_exists


  METHOD zif_cqse_source_object~get_meta_data.
    SELECT SINGLE * FROM reposrc INTO CORRESPONDING FIELDS OF meta_data
      WHERE progname = me->program_name
      AND r3state = 'A'.
    meta_data-objname = me->tadir_entry-obj_name.
    meta_data-objtype = me->tadir_entry-object.
    meta_data-devclass = me->tadir_entry-devclass.
    meta_data-tadir_objname = me->tadir_entry-obj_name.
  ENDMETHOD.                    "ZIF_CQSE_SOURCE_OBJECT~GET_DATE_INFOS


  METHOD zif_cqse_source_object~get_name.
    name = me->program_name.
  ENDMETHOD.                    "ZIF_CQSE_SOURCE_OBJECT~GET_NAME


  METHOD zif_cqse_source_object~get_source.
    READ REPORT me->program_name INTO source STATE 'A'.
  ENDMETHOD.                    "zif_cqse_source_object~get_source


  METHOD zif_cqse_source_object~get_source_file_name.

    DATA:
        tidy_obj_name TYPE string,
        tidy_program_name TYPE string,
        tidy_devclass TYPE string,
        base_file_name TYPE string.

    IF ( me->tadir_entry IS NOT INITIAL ).

      tidy_obj_name = exporter->tidy_file_name( me->tadir_entry-obj_name ).
      tidy_program_name = exporter->tidy_file_name( me->program_name ).
      CONCATENATE tidy_obj_name '/' tidy_program_name INTO base_file_name.

      tidy_devclass = exporter->tidy_file_name( me->tadir_entry-devclass ).

      file_name = exporter->build_file_name(
        package            = tidy_devclass
        source_object_type = tadir_entry-object
        base_file_name     = base_file_name
      ).

    ELSE.
      " TODO
    ENDIF.


  ENDMETHOD.                    "zif_cqse_source_object~get_source_file_name
endclass. "ZCL_CQSE_REPOSRC_OBJECT implementation