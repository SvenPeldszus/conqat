*----------------------------------------------------------------------*
*       CLASS ZCL_CQSE_COMPOSITE_SOURCE_OBJ DEFINITION
*----------------------------------------------------------------------*
* Base class for ABAP source code objects which should be exported and
* which is constructed from several objects in table REPOSRC.
*----------------------------------------------------------------------*
CLASS zcl_cqse_composite_source_obj DEFINITION
  PUBLIC
  INHERITING FROM zcl_cqse_source_object_base
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.


    METHODS zif_cqse_source_object~get_meta_data
      REDEFINITION .
    METHODS zif_cqse_source_object~get_source
      REDEFINITION .
    METHODS get_atomic_source_objects
        IMPORTING
            include_empty TYPE abap_bool DEFAULT abap_true
        RETURNING
            value(result) TYPE zif_cqse_datatypes=>t_source_object_tab.
protected section.

  data:
    children TYPE TABLE OF REF TO zcl_cqse_reposrc_object .

  methods GET_CHILDREN_REPOSRC_NAMES
  abstract
    returning
      value(PROG_NAMES) type PROGNAMES .
  methods INITIALIZE
    raising
      ZCX_CQSE_SRCOBJ_NOT_EXISTS .
  PRIVATE SECTION.
endclass. "ZCL_CQSE_COMPOSITE_SOURCE_OBJ definition



*----------------------------------------------------------------------*
*       class ZCL_CQSE_COMPOSITE_SOURCE_OBJ implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_COMPOSITE_SOURCE_OBJ implementation.


  METHOD initialize.
    DATA prog_name TYPE progname.
    DATA children_names TYPE prognames.
    DATA child TYPE REF TO zcl_cqse_reposrc_object.
    DATA child_count TYPE i.

    children_names = get_children_reposrc_names( ).

    LOOP AT children_names INTO prog_name.
      TRY.
          CLEAR child.
          CREATE OBJECT child
            EXPORTING
              program_name = prog_name
              exporter     = exporter
              tadir_entry  = me->tadir_entry.
          APPEND child TO me->children.
        CATCH zcx_cqse_srcobj_not_exists.
          " ignore if a single child does not exist
      ENDTRY.
    ENDLOOP.

    DESCRIBE TABLE me->children LINES child_count.
    IF child_count < 1.
      RAISE EXCEPTION TYPE zcx_cqse_srcobj_not_exists.
    ENDIF.

  ENDMETHOD.                    "initialize


  METHOD zif_cqse_source_object~get_meta_data.
    DATA reposrc_object TYPE REF TO zcl_cqse_reposrc_object.
    DATA child_meta_data TYPE zif_cqse_datatypes=>t_source_metadata.

    LOOP AT me->children INTO reposrc_object.
      IF meta_data IS INITIAL .
        meta_data = reposrc_object->zif_cqse_source_object~get_meta_data( ).
      ELSE.
        child_meta_data = reposrc_object->zif_cqse_source_object~get_meta_data( ).
        IF child_meta_data-cdat < meta_data-cdat.
          meta_data-cdat = child_meta_data-cdat.
        ENDIF.
        IF child_meta_data-udat > meta_data-udat.
          meta_data-udat = child_meta_data-udat.
          meta_data-utime = child_meta_data-utime.
        ELSEIF child_meta_data-udat = meta_data-udat
          AND child_meta_data-utime > meta_data-utime.
          meta_data-utime = child_meta_data-utime.
        ENDIF.
      ENDIF.
    ENDLOOP.

    meta_data-objname = me->tadir_entry-obj_name.
    meta_data-objtype = me->tadir_entry-object.
    meta_data-devclass = me->tadir_entry-devclass.

  ENDMETHOD.                    "ZIF_CQSE_SOURCE_OBJECT~GET_META_DATA


  METHOD zif_cqse_source_object~get_source.
    DATA reposrc_object TYPE REF TO zcl_cqse_reposrc_object.
    DATA object_source TYPE string_table.
    DATA sep_line(80) TYPE c.

    LOOP AT children INTO reposrc_object.
      object_source = reposrc_object->zif_cqse_source_object~get_source( ).
      IF zcl_cqse_abap_exporter=>is_empty_source( object_source ) = 'X'.
        CONTINUE.
      ENDIF.

      CONCATENATE
        '* --> '
        reposrc_object->program_name
        ' <--------------------------------------------------------------------------------'
        INTO sep_line.

      APPEND '' TO source.
      APPEND '' TO source.
      APPEND sep_line TO source.
      APPEND '' TO source.
      APPEND LINES OF object_source TO source.
    ENDLOOP.
  ENDMETHOD.                    "ZIF_CQSE_SOURCE_OBJECT~GET_SOURCE


  METHOD get_atomic_source_objects.
    DATA:
        child TYPE REF TO zcl_cqse_reposrc_object,
        source TYPE string_table,
        is_empty TYPE abap_bool.

    LOOP AT children INTO child.
      IF include_empty = abap_true.
        APPEND child TO result.
      ELSE.
        CLEAR source.
        source = child->zif_cqse_source_object~get_source( ).
        is_empty = zcl_cqse_abap_exporter=>is_empty_source( source ).
        IF is_empty = abap_false.
          APPEND child TO result.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "get_atomic_source_objects
endclass. "ZCL_CQSE_COMPOSITE_SOURCE_OBJ implementation