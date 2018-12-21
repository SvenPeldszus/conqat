*&---------------------------------------------------------------------*
*& Report  ZTEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ztest.

DATA:
      check_result TYPE REF TO cl_ci_check_result,
      check_variant TYPE REF TO cl_ci_checkvariant,
      variant_tests TYPE sci_tstvar,
      test TYPE LINE OF sci_tstvar,
      inspection TYPE REF TO cl_ci_inspection,
      obj_set TYPE REF TO cl_ci_objectset,
      results type scit_rest,
      result_line type line of scit_rest,
      do type string
      .

check_variant = cl_ci_checkvariant=>create(
    p_user              = ' '
    p_name              = ''
*    p_called_internal   =
).

test-testname = 'CL_CI_TEST_EXTENDED_CHECK'.
INSERT test INTO TABLE variant_tests.

check_variant->set_variant(
     p_variant       =     variant_tests ).

obj_set = cl_ci_objectset=>get_ref(
    p_type                    = 'PROG'
    p_sglobj                  = 'ZSAPLINK'
).


inspection = cl_ci_inspection=>create(
    p_user              = ' '
    p_name              = ' ' ).

inspection->set(
  EXPORTING
    p_chkv       =    check_variant
    p_objs       =   obj_set
*    p_text       =     " Code Inspector: Element Text (Chk, ChkV, ObjM, Inspec)
*    p_deldate    =     " Code Inspector: Deletion Date
*    p_sotables   =     " Code Inspector: Selection Options for Table Restriction
*    p_nosuppress =     " CHAR01 Data Element for SYST
*    p_noaunit    =     " General Flag
*    p_ocignore   =     " General Flag
*  EXCEPTIONS
*    not_enqueued = 1
*    others       = 2
).
IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

inspection->run(
*  EXPORTING
*    p_howtorun             = 'R'    " CHAR01 Data Element for SYST
*  EXCEPTIONS
*    missing_information    = 1
*    cancel_popup           = 2
*    insp_already_run       = 3
*    no_object              = 4
*    too_many_objects       = 5
*    could_not_read_variant = 6
*    locked                 = 7
*    objs_locked            = 8
*    error_in_objs_build    = 9
*    invalid_check_version  = 10
*    just_running           = 11
*    error_in_batch         = 12
*    not_authorized         = 13
*    no_server_found        = 14
*    others                 = 15
).
IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

inspection->get_results(
*  EXPORTING
*    p_max_lines              = 200000    " Number of Entries to Be Read by Database
*    p_sotest                 =     " Code Inspector: Selection Option for Classes/Interfaces
*    p_socode                 =     " Code Inspector: Selection Options for Message Code
*    p_sokind                 =     " Code Inspector: Selection Options for the Error Type
*    p_objtype                =     " Code Inspector: Selection Options for the Object Types
*    p_objname                =     " Code Inspector: Selection Options for the Object Name
*    p_sobjtype               =
*    p_sobjname               =
*    p_devclass               =     " Code Inspector: Selection Options for the Package
*    p_responsibl             =     " Code Inspector: Selection Options for the Responsible User
*    p_chksum                 =
*    p_reqst_include_msg_only = 'X'    " General Flag
  IMPORTING
    p_scirest_ps             =     results
*    p_scirest_hd             =     " Code Inspector: Results of Inspection (Header Entries)
*  EXCEPTIONS
*    insp_not_yet_executed    = 1
*    overflow                 = 2
*    error_in_list_creating   = 3
*    others                   = 4
).
IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.


loop at results into result_line.
  write: / result_line-objname, result_line-code.
endloop.