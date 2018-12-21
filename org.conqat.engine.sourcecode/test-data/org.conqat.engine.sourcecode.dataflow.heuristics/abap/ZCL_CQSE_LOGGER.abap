*----------------------------------------------------------------------*
*       CLASS zcl_cqse_logger DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_cqse_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    METHODS constructor
      IMPORTING
        !external_id TYPE string
        !def_msgid TYPE symsgid
        !def_msgno TYPE symsgno .
    METHODS error
      IMPORTING
        !msgno TYPE symsgno OPTIONAL
        !msg1 TYPE simple
        !msg2 TYPE simple OPTIONAL
        !msg3 TYPE simple OPTIONAL
        !msg4 TYPE simple OPTIONAL
        !show_message TYPE abap_bool DEFAULT abap_false .
    METHODS warn
      IMPORTING
        !msgno TYPE symsgno OPTIONAL
        !msg1 TYPE simple
        !msg2 TYPE simple OPTIONAL
        !msg3 TYPE simple OPTIONAL
        !msg4 TYPE simple OPTIONAL
        !show_message TYPE abap_bool DEFAULT abap_false .
    METHODS info
      IMPORTING
        !msgno TYPE symsgno OPTIONAL
        !msg1 TYPE simple
        !msg2 TYPE simple OPTIONAL
        !msg3 TYPE simple OPTIONAL
        !msg4 TYPE simple OPTIONAL
        !show_message TYPE abap_bool DEFAULT abap_false .
    METHODS save .
    METHODS get_log_table
      RETURNING
        value(log) TYPE string_table .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA
        log_messages_table TYPE string_table.
    DATA
        log_handle TYPE balloghndl.
    DATA
        default_msgid TYPE symsgid.
    DATA
        log_handle_tab TYPE bal_t_logh.
    DATA
        default_msgno TYPE symsgno.
    DATA
        error_count TYPE i.
    DATA
        warn_count TYPE i.

    METHODS log_message
        IMPORTING
            msgty  TYPE  symsgty
            msgno  TYPE  symsgno OPTIONAL
            msg1 TYPE simple
            msg2 TYPE simple OPTIONAL
            msg3 TYPE simple OPTIONAL
            msg4 TYPE simple OPTIONAL
            show_message TYPE abap_bool DEFAULT abap_false.

endclass. "ZCL_CQSE_LOGGER definition



*----------------------------------------------------------------------*
*       class ZCL_CQSE_LOGGER implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_LOGGER implementation.


  METHOD constructor.
    DATA: log_header TYPE bal_s_log.

    default_msgid = def_msgid.
    default_msgno = default_msgno.

    log_header-extnumber = external_id.
*    log_header-object = 'ZCQSE'.
    log_header-aluser = sy-uname.
    log_header-alprog = sy-repid.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = log_header
      IMPORTING
        e_log_handle = log_handle.

  ENDMETHOD.                    "constructor


  METHOD get_log_table.
    log = log_messages_table.
  ENDMETHOD.                    "get_log_table


  METHOD log_message.

    DATA:
        bal_message TYPE bal_s_msg,
        message TYPE string,
        msg_handle TYPE  balmsghndl,
        len TYPE i,
        msg1_str TYPE string,
        msg2_str TYPE string,
        msg3_str TYPE string,
        msg4_str TYPE string,
        fullmsg TYPE string.

    " cast to string values
    msg1_str = msg1.
    msg2_str = msg2.
    msg3_str = msg3.
    msg4_str = msg4.

    bal_message-msgty = msgty.
    bal_message-msgid = default_msgid.
    IF msgno IS INITIAL.
      bal_message-msgno = default_msgno.
    ELSE.
      bal_message-msgno = msgno.
    ENDIF.

    IF bal_message-msgno = default_msgno.
      " for the default message type, adjust strings accroding
      " message variable length of 50 chars
      CONCATENATE msg1_str ' ' msg2_str ' ' msg3_str ' ' msg4_str
        INTO fullmsg RESPECTING BLANKS.

      len = strlen( fullmsg ).

      IF len > 150.
        bal_message-msgv4 = fullmsg+150(*).
        fullmsg = fullmsg(150).
      ENDIF.
      IF len > 100.
        bal_message-msgv3 = fullmsg+100(*).
        fullmsg = fullmsg(100).
      ENDIF.
      IF len > 50.
        bal_message-msgv2 = fullmsg+50(*).
        fullmsg = fullmsg(50).
      ENDIF.
      bal_message-msgv1 = fullmsg.
    ELSE.
      bal_message-msgv1 = msg1_str.
      bal_message-msgv2 = msg2_str.
      bal_message-msgv3 = msg3_str.
      bal_message-msgv4 = msg4_str.
    ENDIF.


* SAP application log disabled since it increases execution time drastically

*    CALL FUNCTION 'BAL_LOG_MSG_ADD'
*      EXPORTING
*        i_log_handle   = me->log_handle
*        i_s_msg        = bal_message
*      IMPORTING
*        e_s_msg_handle = msg_handle.
*
*    APPEND msg_handle TO log_handle_tab.

    MESSAGE ID bal_message-msgid TYPE msgty NUMBER bal_message-msgno
      INTO message
      WITH bal_message-msgv1 bal_message-msgv2 bal_message-msgv3 bal_message-msgv4.

    IF msgty EQ 'E'.
      CONCATENATE 'ERROR : ' message INTO message RESPECTING BLANKS.
    ELSEIF msgty EQ 'W'.
      CONCATENATE 'WARN : ' message INTO message RESPECTING BLANKS.
    ELSE.
      CONCATENATE 'INFO : ' message INTO message RESPECTING BLANKS.
    ENDIF.

    APPEND message TO log_messages_table.

    WRITE / message.

    IF show_message EQ abap_true.
      MESSAGE s000(zcqse) DISPLAY LIKE msgty
        WITH bal_message-msgv1 bal_message-msgv2 bal_message-msgv3 bal_message-msgv4.
    ENDIF.

  ENDMETHOD.                    "log_message


  METHOD error.

    error_count = error_count + 1.

    log_message(
      EXPORTING
        msgty = 'E'
        msgno = msgno
        msg1  = msg1
        msg2  = msg2
        msg3  = msg3
        msg4  = msg4
        show_message = show_message
    ).

  ENDMETHOD.                    "error


  METHOD info.

    log_message(
      EXPORTING
        msgty = 'I'
        msgno = msgno
        msg1  = msg1
        msg2  = msg2
        msg3  = msg3
        msg4  = msg4
        show_message = show_message
    ).

  ENDMETHOD.                    "info


  METHOD warn.

    warn_count = warn_count + 1.

    log_message(
      EXPORTING
        msgty = 'W'
        msgno = msgno
        msg1  = msg1
        msg2  = msg2
        msg3  = msg3
        msg4  = msg4
        show_message = show_message
    ).

  ENDMETHOD.                    "warn


  METHOD save.

    DATA:
        status_msg TYPE string.

    IF error_count > 0.
      MESSAGE s000(zcqse) DISPLAY LIKE 'E' WITH 'Export failed: ' error_count ' erros, see log.'.
    ELSEIF warn_count > 0.
      MESSAGE s000(zcqse) DISPLAY LIKE 'W' WITH 'Export successful (had ' warn_count ' warnings).'.
    ELSE.
      MESSAGE s000(zcqse) DISPLAY LIKE 'I' WITH 'Export successful.'.
    ENDIF.

*   SAP application log disabled (see method log_message)

*    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
*      EXPORTING
*        i_t_log_handle   = log_handle_tab
*      EXCEPTIONS
*        log_not_found    = 1
*        save_not_allowed = 2
*        numbering_error  = 3
*        OTHERS           = 4.
*
*    CALL FUNCTION 'BAL_LOG_REFRESH'
*      EXPORTING
*        i_log_handle = log_handle.


  ENDMETHOD.                    "save
endclass. "ZCL_CQSE_LOGGER implementation