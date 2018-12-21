*----------------------------------------------------------------------*
*       CLASS ZCL_CQSE_FUGR_SRCOBJ DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_cqse_fugr_srcobj DEFINITION
  PUBLIC
  INHERITING FROM zcl_cqse_composite_source_obj
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !tadir_entry TYPE tadir
        !exporter TYPE REF TO zcl_cqse_abap_exporter
        !file_object_name TYPE string OPTIONAL
      RAISING
        zcx_cqse_srcobj_not_exists .
  PROTECTED SECTION.

    METHODS get_children_reposrc_names
      REDEFINITION .
  PRIVATE SECTION.

    METHODS get_includes
      IMPORTING
        value(suffix) TYPE char1 OPTIONAL
      RETURNING
        value(includes) TYPE prognames .
endclass. "ZCL_CQSE_FUGR_SRCOBJ definition



*----------------------------------------------------------------------*
*       class ZCL_CQSE_FUGR_SRCOBJ implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_FUGR_SRCOBJ implementation.


  METHOD constructor.
    super->constructor(
      tadir_entry = tadir_entry
      exporter = exporter
      file_object_name = file_object_name
      ).
    initialize( ) .
  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_children_reposrc_names.

    DATA fgrp TYPE string.
    DATA namespace TYPE string.
    DATA name TYPE string.
    DATA include_names TYPE prognames.

    fgrp = me->zif_cqse_source_object~get_name( ).

    zcl_cqse_abap_util=>strip_namespace(
        EXPORTING
            qualified_name = fgrp
        IMPORTING
            namespace = namespace
            name = fgrp
    ).


    " main program
    CONCATENATE namespace 'SAPL' fgrp INTO name.
    APPEND name TO prog_names.

    " all includes
    include_names = me->get_includes( ).
    APPEND LINES OF include_names TO prog_names.

*    " top-include (gloabl definitions)
*    CONCATENATE namespace 'L' fgrp 'TOP' INTO name.
*    APPEND name TO prog_names.
*
*    " declarations of local classes
*    include_names = me->get_includes( 'D' ).
*    APPEND LINES OF include_names TO prog_names.
*
*    " includes for function modules
*    include_names = me->get_includes( 'U' ).
*    APPEND LINES OF include_names TO prog_names.
*
*    " includes for method implmentations of local classes
*    include_names = me->get_includes( 'P' ).
*    APPEND LINES OF include_names TO prog_names.
*
*    " includs for implmentations of PBO mdules
*    include_names = me->get_includes( 'O' ).
*    APPEND LINES OF include_names TO prog_names.
*
*    " includs for implmentations of PAI mdules
*    include_names = me->get_includes( 'I' ).
*    APPEND LINES OF include_names TO prog_names.
*
*    " includs for implmentations of event blocks
*    include_names = me->get_includes( 'E' ).
*    APPEND LINES OF include_names TO prog_names.
*
*    " includs for implmentations of procdures
*    include_names = me->get_includes( 'F' ).
*    APPEND LINES OF include_names TO prog_names.
*
*    " includs for implmentations of (tables?)
*    include_names = me->get_includes( 'T' ).
*    APPEND LINES OF include_names TO prog_names.

  ENDMETHOD.                    "get_children_reposrc_names


  METHOD get_includes.

    DATA:
        fugr_name TYPE string,
        namespace TYPE string,
        start TYPE string,
        end TYPE string,
        xx_include TYPE string,
        include_pattern TYPE string,
        main_prog TYPE progname.

    fugr_name = zif_cqse_source_object~get_name( ).


    zcl_cqse_abap_util=>strip_namespace(
        EXPORTING
            qualified_name = fugr_name
        IMPORTING
            namespace = namespace
            name = fugr_name
    ).

    IF suffix IS INITIAL.

      CONCATENATE namespace 'SAPL' fugr_name INTO main_prog.

      CALL FUNCTION 'GET_INCLUDES'
        EXPORTING
          progname = main_prog
        TABLES
          incltab  = includes.

      " remove all includes which do not belong to this function group
      CONCATENATE namespace 'L' fugr_name '*' INTO include_pattern.
      DELETE includes WHERE table_line NP include_pattern.

      " remove the signature includes, since these are not visible to developers
      DELETE includes WHERE table_line CP '*$++'.

    ELSE.

      CONCATENATE namespace 'L' fugr_name suffix '00' INTO start.
      CONCATENATE namespace 'L' fugr_name suffix '99' INTO end.
      CONCATENATE namespace 'L' fugr_name suffix 'XX' INTO xx_include.

      SELECT progname INTO TABLE includes
        FROM reposrc
        WHERE ( progname BETWEEN start AND end ) OR ( progname EQ xx_include )
        AND r3state EQ 'A'.

    ENDIF.

  ENDMETHOD.                    "get_includes
endclass. "ZCL_CQSE_FUGR_SRCOBJ implementation