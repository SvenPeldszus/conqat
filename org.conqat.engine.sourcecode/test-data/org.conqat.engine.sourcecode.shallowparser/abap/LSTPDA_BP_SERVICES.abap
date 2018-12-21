MODULE get_focus INPUT.
  CALL FUNCTION 'TPDA_GET_FOCUS'.
ENDMODULE.                 " get_focus  INPUT

MODULE exit INPUT.
  CLEAR dynp_vars-set_bp.
  lcl_dynpro_0300=>clear( ).
  CLEAR tpda_bp_cr.
  CLEAR: it_statements, it_functions, it_bp_set.
  CLEAR ok_code.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " exit  INPUT

MODULE init OUTPUT.
  IF init IS INITIAL.
    init = 'X'.
    tab_ctrl_bp_create-activetab = start_tab.
    CLEAR wa_condition_0210.
    CLEAR wa_condition_0220.
    CLEAR wa_condition_0230.
    CLEAR wa_condition_0250.
  ENDIF.
ENDMODULE.                 " init  OUTPUT

MODULE exit_800 INPUT.
  cancel_800 = 'X'.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " exit_800  INPUT
