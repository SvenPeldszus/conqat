CLASS cl_scv_load_table_repository DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    INTERFACES if_scv_load_table_repository .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS CL_SCV_LOAD_TABLE_REPOSITORY IMPLEMENTATION.

  METHOD if_scv_load_table_repository~load_trig_and_lref BY KERNEL MODULE ab_kmloadparttrigandlref fail.

  ENDMETHOD.

  METHOD if_scv_load_table_repository~trig_cont_2_lref_offset.
    DATA: lref_offset TYPE n LENGTH 10.
    lref_offset = i_trig_cont. "Convert offset like 47 to 0000000047
    r_lref_offset = lref_offset.
  ENDMETHOD.
ENDCLASS.