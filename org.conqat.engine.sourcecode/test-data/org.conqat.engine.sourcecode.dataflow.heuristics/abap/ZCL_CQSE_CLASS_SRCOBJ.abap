*----------------------------------------------------------------------*
*       CLASS ZCL_CQSE_CLASS_SRCOBJ DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_cqse_class_srcobj DEFINITION
  PUBLIC
  INHERITING FROM zcl_cqse_source_object_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


    DATA method_includes TYPE zif_cqse_datatypes=>t_method_include_tab.


    METHODS constructor
      IMPORTING
        !tadir_entry TYPE tadir
        !exporter TYPE REF TO zcl_cqse_abap_exporter .
    METHODS get_plain_source
       RETURNING
        value(source) TYPE string_table .
    METHODS get_editor_source
      RETURNING
        value(source) TYPE string_table
      RAISING
        zcx_cqse_srcobj_not_exists .
    CLASS-METHODS build_program_name
      IMPORTING
        !class_name TYPE csequence
        !suffix TYPE csequence
      RETURNING
        value(prog_name) TYPE string .

*      RAISING
*        zcx_cqse_srcobj_not_exists .
    METHODS zif_cqse_source_object~get_meta_data
      REDEFINITION .
    METHODS zif_cqse_source_object~get_source
      REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA
        source TYPE string_table.

    METHODS build_method_includes
        RETURNING
            value(result) TYPE zif_cqse_datatypes=>t_method_include_tab .

    METHODS method_begin_line
        IMPORTING
            !source TYPE string_table
        RETURNING
            value(result) TYPE i.


*    DATA oo_source_object TYPE REF TO cl_oo_source_object .
endclass. "ZCL_CQSE_CLASS_SRCOBJ definition



*----------------------------------------------------------------------*
*       class ZCL_CQSE_CLASS_SRCOBJ implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_CLASS_SRCOBJ implementation.


  METHOD constructor.
    super->constructor(
      tadir_entry = tadir_entry
      exporter = exporter
      ).

    me->method_includes = build_method_includes( ).

    TRY.
        IF exporter->use_class_editor_source_format = abap_true.
          source = get_editor_source(  ).
        ELSE.
          source = get_plain_source(  ).
        ENDIF.
      CATCH zcx_cqse_srcobj_not_exists.
        exporter->logger->warn( msg1 = 'No source found for class ' msg2 = tadir_entry-obj_name ).
    ENDTRY.


  ENDMETHOD.                    "constructor


  METHOD zif_cqse_source_object~get_meta_data.
    DATA source_object_name TYPE sobj_name.
    DATA basename_pattern TYPE string.
    DATA obj_date_infos TYPE zif_cqse_datatypes=>t_source_metadata.
    DATA reposrc_results TYPE STANDARD TABLE OF reposrc.
    DATA reposrc_line TYPE reposrc.

    " pattern for entries for the class in REPOSRC table
    source_object_name = zif_cqse_source_object~get_name( ).
    basename_pattern = build_program_name(
        class_name = source_object_name
        suffix = 'C%'
    ).

    SELECT * FROM reposrc INTO TABLE reposrc_results
      WHERE progname LIKE basename_pattern
      AND r3state EQ 'A'.


    " determine oldest creation date and last update date/tiem for the class
    " from all entries of that class in REPOSRC table

    LOOP AT reposrc_results INTO reposrc_line.
      MOVE-CORRESPONDING reposrc_line TO obj_date_infos.
      IF meta_data IS INITIAL .
        meta_data = obj_date_infos.
      ELSE.
        IF obj_date_infos-cdat < meta_data-cdat.
          meta_data-cdat = obj_date_infos-cdat.
        ENDIF.
        IF obj_date_infos-udat > meta_data-udat.
          meta_data-udat = obj_date_infos-udat.
          meta_data-utime = obj_date_infos-utime.
        ELSEIF obj_date_infos-udat = meta_data-udat
          AND obj_date_infos-utime > meta_data-utime.
          meta_data-utime = obj_date_infos-utime.
        ENDIF.
      ENDIF.
    ENDLOOP.

    meta_data-objname = me->tadir_entry-obj_name.
    meta_data-objtype = me->tadir_entry-object.
    meta_data-devclass = me->tadir_entry-devclass.
    meta_data-tadir_objname = me->tadir_entry-obj_name.

  ENDMETHOD.                    "ZIF_CQSE_SOURCE_OBJECT~GET_META_DATA


  METHOD zif_cqse_source_object~get_source.

    source = me->source.

  ENDMETHOD.                    "zif_cqse_source_object~get_source


  METHOD build_program_name.
    DATA:
        basename TYPE seoclsname.

    CONCATENATE class_name '==============================' INTO basename.
    CONCATENATE basename suffix INTO prog_name.

  ENDMETHOD.                    "build_program_name


  METHOD get_editor_source.

    DATA:
        " oo_source_object is dynamically instantiated as cl_oo_source_object
        " since this class is only available on newer SAP version 7x on
        oo_source_object TYPE REF TO object,
        class_key TYPE seoclskey,
        exc_sy_no_handler TYPE REF TO cx_sy_no_handler,
        " From NetWeaver 730 on, a special exception exists which could be used
        " to check that the object does not exist. For earlier system we just use cx_root.
        " data exc_oo_clif_not_exists type ref to cx_oo_clif_not_exists.
        exc_oo_clif_not_exists TYPE REF TO cx_root.

    class_key-clsname = zif_cqse_source_object~get_name( ).

    TRY.

        CREATE OBJECT oo_source_object TYPE ('cl_oo_source_object')
          EXPORTING
            clskey = class_key.

      CATCH cx_sy_create_object_error.
        " obviously cl_oo_source_object does not exist on the current SAP System,
        " thus plain source is exported
        exporter->logger->warn( 'Sourcecode class editor not available, plain source code exporteted for class.' ).
        source = get_plain_source( ).
        RETURN.

      CATCH cx_sy_no_handler INTO exc_sy_no_handler.
        TRY.
            " this chekc is only relevant if cx_oo_clif_not_exists is used. See comment above
            exc_oo_clif_not_exists ?= exc_sy_no_handler->previous.
            RAISE EXCEPTION TYPE zcx_cqse_srcobj_not_exists
              EXPORTING
                previous = exc_oo_clif_not_exists.
          CATCH cx_sy_move_cast_error.
            RAISE EXCEPTION exc_sy_no_handler.
        ENDTRY.
    ENDTRY.



    CALL METHOD oo_source_object->('read_source')
      EXPORTING
        status = 'A'.

    CALL METHOD oo_source_object->('get_source_tab')
      IMPORTING
        source = source.

  ENDMETHOD.                    "get_editor_source


  METHOD get_plain_source.

    DATA:
        class_pool_name TYPE progname,
        class_pool_src TYPE string_table,
        include_name TYPE progname,
        include_src TYPE string_table,
        line TYPE string,
        out_line TYPE string,
        length TYPE i,
        method_include TYPE zif_cqse_datatypes=>t_method_include,
        method_offset TYPE i.

    IF me->tadir_entry-object EQ 'INTF'.
      class_pool_name = build_program_name(
          class_name = me->tadir_entry-obj_name
          suffix = 'IP'
      ).
    ELSE.
      class_pool_name = build_program_name(
           class_name = me->tadir_entry-obj_name
           suffix = 'CP'
       ).
    ENDIF.

    READ REPORT class_pool_name INTO class_pool_src STATE 'A'.

    LOOP AT class_pool_src INTO line.
      CONDENSE line.
      length = strlen( line ).

      " skip empty, comment lines and introduction statement
      IF length < 1 OR line(1) = '*' OR line(1) = '"' OR line CP 'class-pool*'.
        CONTINUE.
      ENDIF.

      " add includes of public, protected and private sections
      IF line CP 'include *.'.
        REPLACE 'include' IN line WITH ''.
        REPLACE '.' IN line WITH ''.
        CONDENSE line.

        IF line CP '*CU' OR line CP '*CO' OR line CP '*CI' OR line CP '*IU'.
          include_name = line.
          CLEAR include_src.
          READ REPORT include_name INTO include_src STATE 'A'.
          APPEND LINES OF include_src TO source.
        ELSEIF line = 'methods'.
          LOOP AT me->method_includes INTO method_include.
            CLEAR include_src.
            READ REPORT method_include-incname INTO include_src STATE 'A'.
            method_include-offset = method_begin_line( include_src ).
            MODIFY me->method_includes INDEX sy-tabix FROM method_include.
            APPEND '' TO source.
            APPEND '' TO source.
            APPEND LINES OF include_src TO source.
          ENDLOOP.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF line CP 'CLASS * IMPLEMENTATION.'.
        APPEND '' TO source.
        APPEND '' TO source.
        APPEND '' TO source.
        APPEND '*----------------------------------------------------------------------*' TO source.
        CONCATENATE '*       ' line INTO out_line RESPECTING BLANKS.
        APPEND out_line TO source.
        APPEND '*----------------------------------------------------------------------*' TO source.
        APPEND '*' TO source.
        APPEND '*----------------------------------------------------------------------*' TO source.
        APPEND line TO source.
        CONTINUE.
      ENDIF.

      APPEND line TO source.

    ENDLOOP.

  ENDMETHOD.                    "read_class_source


  METHOD build_method_includes.

    " only works for actual classes, not interfaces.
    IF me->tadir_entry-object <> 'CLAS'.
      RETURN.
    ENDIF.

    DATA:
        lv_classname TYPE seoclname.


    lv_classname = me->zif_cqse_source_object~get_name( ).

    cl_oo_classname_service=>get_all_method_includes(
      EXPORTING
        clsname = lv_classname
      RECEIVING
        result  = result
      EXCEPTIONS
        class_not_existing = 1
     ).

    IF sy-subrc <> 0.
      exporter->logger->warn( msg1 = 'Unable to read methods for class ' msg2 = lv_classname ).
    ENDIF.

  ENDMETHOD.                    "get_method_includes


  METHOD method_begin_line.
    DATA:
        line TYPE string.

    LOOP AT source INTO line.
      CONDENSE line.
      IF line CP 'method *.'.
        result = sy-tabix - 1.
        RETURN.
      ENDIF.
    ENDLOOP.

    result = 0.
  ENDMETHOD.                    "method_begin_line
endclass. "ZCL_CQSE_CLASS_SRCOBJ implementation