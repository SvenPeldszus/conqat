interface-pool.
*----------------------------------------------------------------------*
*       INTERFACE ZIF_CQSE_SOURCE_OBJECT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
interface ZIF_CQSE_SOURCE_OBJECT
  public .


  interfaces ZIF_CQSE_DATATYPES .

  methods GET_SOURCE
    returning
      value(SOURCE) type STRING_TABLE .
  methods GET_SOURCE_FILE_NAME
    returning
      value(FILE_NAME) type STRING .
  methods GET_NAME
    returning
      value(NAME) type SOBJ_NAME .
  methods GET_META_DATA
    returning
      value(META_DATA) type ZIF_CQSE_DATATYPES~T_SOURCE_METADATA .
endinterface.