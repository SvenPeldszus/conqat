*----------------------------------------------------------------------*
*       CLASS ZCL_CQSE_CLASSENH_SRCOBJ DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_CLASSENH_SRCOBJ definition
  public
  inheriting from ZCL_CQSE_COMPOSITE_SOURCE_OBJ
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TADIR_ENTRY type TADIR
      !FILE_OBJECT_NAME type STRING
      !EXPORTER type ref to ZCL_CQSE_ABAP_EXPORTER
    raising
      ZCX_CQSE_SRCOBJ_NOT_EXISTS .

  methods ZIF_CQSE_SOURCE_OBJECT~GET_META_DATA
    redefinition .
protected section.

  methods GET_CHILDREN_REPOSRC_NAMES
    redefinition .
  PRIVATE SECTION.
endclass. "ZCL_CQSE_CLASSENH_SRCOBJ definition



*----------------------------------------------------------------------*
*       class ZCL_CQSE_CLASSENH_SRCOBJ implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_CLASSENH_SRCOBJ implementation.


  METHOD constructor.
    super->constructor(
      tadir_entry = tadir_entry
      exporter = exporter
      file_object_name = file_object_name
      ).
    initialize( ) .
  ENDMETHOD.                    "CONSTRUCTOR


method GET_CHILDREN_REPOSRC_NAMES.
    DATA source_object_name TYPE sobj_name.
    DATA basename(30) TYPE c.
    DATA pattern TYPE string.
    DATA prog_name TYPE progname.

    source_object_name = zif_cqse_source_object~get_name( ).
    CONCATENATE source_object_name '==============================' INTO basename.
    CONCATENATE basename 'CC%' INTO pattern.

    SELECT progname FROM reposrc INTO TABLE prog_names
        WHERE progname LIKE pattern AND r3state EQ 'A'.

*    CONCATENATE basename 'CCDEF' INTO prog_name.
*    APPEND prog_name TO prog_names.
*    CONCATENATE basename 'CCIMP' INTO prog_name.
*    APPEND prog_name TO prog_names.
*    CONCATENATE basename 'CCMAC' INTO prog_name.
*    APPEND prog_name TO prog_names.
endmethod.


  METHOD zif_cqse_source_object~get_meta_data.
* do nothing
  ENDMETHOD.                    "ZIF_CQSE_SOURCE_OBJECT~GET_META_DATA
endclass. "ZCL_CQSE_CLASSENH_SRCOBJ implementation