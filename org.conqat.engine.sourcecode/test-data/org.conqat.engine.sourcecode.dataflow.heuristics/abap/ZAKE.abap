class ZAKE definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools STMS .

  data DOWNLOAD_NUGGET_TO_LM type FLAG value 'X'. "#EC NOTEXT .
  data DOWNLOAD_SLINKEES_TO_LM type FLAG value 'X'. "#EC NOTEXT .
  data DOWNLOAD_TRANSPORT_TO_LM type FLAG value ' '. "#EC NOTEXT .
  data DOWNLOAD_ZIP_TO_LM_FLAG type FLAG value 'X'. "#EC NOTEXT .
  data ZAKE_HOME type STRING value 'C:\Projects\'. "#EC NOTEXT .

  class-methods SET_BREAK_TO_CR_LF
    importing
      !I_STRING type STRING
    returning
      value(R_STRING) type STRING .
  methods UNSET_CHECKIN_OBJECT_LIST .
  methods UNSET_CHECKIN_OBJECT
    importing
      value(I_OBJECT) type TROBJTYPE .
  methods UNSET_CHECKIN_OBJECT_BY_NAME
    importing
      !I_OBJECT type TROBJTYPE
      !I_OBJECT_NAME type SOBJ_NAME .
  class-methods GET_FILENAME_FROM_FULLPATH
    importing
      !I_FULLPATH type STRING
    returning
      value(R_FILENAME) type STRING .
  methods INSTALL_SLINKEE_FROM_LM
    importing
      !I_FULLPATH type STRING
    raising
      ZCX_SAPLINK .
  methods DOWNLOAD_XML_TO_LM
    importing
      !I_FULLPATH type STRING
      !I_XML_STRING type STRING
    raising
      ZCX_SAPLINK .
  methods UPDATE
    returning
      value(R_COMMAND_OUTPUT) type STRING_TABLE
    raising
      ZCX_SAPLINK .
  methods ACTIVATE_SLINKEES
    importing
      !I_SLINKEES type IGS_CE_CU_TAB
    raising
      ZCX_SAPLINK .
  methods ADD_FILES_TO_ZIP
    importing
      !I_FILES type STRING_TABLE .
  methods SET_TESTRUN
    importing
      !I_TESTRUN type FLAG .
  methods INSTALL_OBJECTS
    importing
      !I_OBJECTS type SCTS_TADIR
    raising
      ZCX_SAPLINK .
  methods SET_CHECKIN_OBJECTS
    importing
      !I_OBJECTS type SCTS_TADIR .
  methods INSTALL_SLINKEES_FROM_LM
    importing
      !I_TESTRUN type FLAG default ' '
    raising
      ZCX_SAPLINK .
  methods CHECKOUT
    importing
      !I_REVISION type I optional
    preferred parameter I_REVISION
    returning
      value(R_COMMAND_OUTPUT) type STRING_TABLE
    raising
      ZCX_SAPLINK .
  methods CHECKIN
    importing
      !I_COMMENT type STRING optional
    raising
      ZCX_SAPLINK .
  methods ACTIVATE_PACKAGE_OBJECTS
    raising
      ZCX_SAPLINK .
  methods ACTIVATE
    importing
      !I_OBJTYPE type STRING
      !I_OBJNAME type STRING
    raising
      ZCX_SAPLINK .
  methods ADD_OBJECT_TO_NUGGET
    importing
      !I_NUGGET_NAME type STRING
      !I_XML_DOCUMENT type ref to IF_IXML_DOCUMENT
    raising
      ZCX_SAPLINK .
  methods BUILD_NUGGET_FROM_FILES
    importing
      !I_NUGGET_NAME type STRING
    raising
      ZCX_SAPLINK .
  methods CREATE_NUGGET
    importing
      !I_NUGGET_NAME type STRING .
  methods CREATE_SLINKEES
    importing
      !I_NUGGET_NAME type STRING optional
    raising
      ZCX_SAPLINK .
  methods GET_ALL_FILES
    raising
      ZCX_SAPLINK .
  methods GET_NUGGET_STRING
    importing
      !I_NUGGET_NAME type STRING
    returning
      value(R_NUGGET_STRING) type STRING .
  methods SAVE_NUGGET
    importing
      !I_NUGGET_NAME type STRING
      !I_TARGET type STRING .
  methods SYNTAXCHECK
    importing
      !I_OBJNAME type STRING
      !I_OBJTYPE type STRING .
  methods SET_PACKAGE
    importing
      !I_PACKAGE type DEVCLASS .
  methods SET_PACKAGE_OBJECTS
    importing
      !I_PACKAGE type DEVCLASS
    raising
      ZCX_SAPLINK .
  methods DELETE
    importing
      !I_XML_DOCUMENT type ref to IF_IXML_DOCUMENT .
  methods GET_FILE
    importing
      !I_FILENAME type STRING
      !I_NEWCLIENT type ref to IF_HTTP_CLIENT optional
    returning
      value(R_XML_DOCUMENT) type ref to IF_IXML_DOCUMENT
    raising
      ZCX_SAPLINK .
  methods CONSTRUCTOR
    importing
      !I_SVNPATH type STRING
      !I_LOCALPATH type STRING
      !I_USERNAME type STRING optional
      !I_PASSWORD type STRING optional
    raising
      ZCX_SAPLINK .
  methods INSTALL
    importing
      !I_XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !I_DEVCLASS type DEVCLASS default '$TMP'
    exporting
      !E_OBJECTTYPE type STRING
      !E_OBJECTNAME type STRING .
  methods GET_SLINKEE
    importing
      !I_OBJECTTYPE type STRING
      !I_OBJECTNAME type STRING
    exporting
      !E_SLINKEE_XML type STRING
      !E_SLINKEE_IXML type ref to IF_IXML_DOCUMENT
    raising
      ZCX_SAPLINK .
  methods CREATE_TRANSPORT_REQUEST
    importing
      !I_TEXT type AS4TEXT optional
      !I_AUTHOR type TR_AS4USER default SY-UNAME
    returning
      value(R_TRANSPORT) type TRKORR .
  methods ADD_PACKAGE_OBJECTS_TO_TR
    importing
      !I_TRANSPORT type TRKORR optional
    raising
      ZCX_SAPLINK .
  methods SET_PACKAGE_OF_PACKAGE_OBJECTS
    raising
      ZCX_SAPLINK .
  methods RELEASE_TRANSPORT_REQUEST
    importing
      !I_TRANSPORT type TRKORR optional
    raising
      ZCX_SAPLINK .
protected section.

  data TESTRUN type FLAG .
  data PASSWORD type STRING .
  data USERNAME type STRING .
  data PATH type STRING .
  data URL type STRING .
  data FILE_SEPARATOR type CHAR1 .

  methods INSTALL_SLINKEES
    importing
      !I_SLINKEES type IGS_CE_CU_TAB
    raising
      ZCX_SAPLINK .
  methods GET_SLINKEES_FROM_LM
    importing
      !I_FULLPATH type STRING
    changing
      value(C_SLINKEES) type IGS_CE_CU_TAB
    exceptions
      ZCX_SAPLINK .
  methods ADD_SLINKEE_TO_VERSIONCONTROL
    importing
      !I_FULLPATH type STRING
      !I_FOLDERPATH type STRING optional
    raising
      ZCX_SAPLINK .
private section.

  data FILELIST_ZIP type STRING_TABLE .
  data IV_PACKAGE type DEVCLASS .
  data HTTPCLIENT type ref to IF_HTTP_CLIENT .
  data:
    L_OBJTABLE type standard table of t_objectTable .
  data:
    CURRENTNUGGETS type standard table of nuggetRow .
  data:
    FILELIST type standard table of fileListRowType .
  data:
    package_objects TYPE STANDARD TABLE OF t_packageObjectTable .
  data TRANSPORT type TRKORR .

  methods ADD_TRANSPORT_TO_FILELIST
    raising
      ZCX_SAPLINK .
  methods READ_XML_FROM_LM
    importing
      !I_FULLPATH type STRING
    returning
      value(R_XML_STRING) type STRING
    raising
      ZCX_SAPLINK .
  methods DELETE_FILE
    importing
      !I_FULLPATH type STRING .
  methods CREATE_SLINKEE
    importing
      !I_NUGGET_NAME type STRING optional
      !I_PACKAGE_OBJECT type T_PACKAGEOBJECTTABLE
    raising
      ZCX_SAPLINK .
  methods DOWNLOAD_ZIP_TO_LM
    importing
      !I_FULLPATH type STRING
      !I_STRINGTOZIP type STRING
    raising
      ZCX_SAPLINK .
endclass. "ZAKE definition



*----------------------------------------------------------------------*
*       class ZAKE implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZAKE implementation.


method ACTIVATE.

  DATA: objects TYPE dwinactiv_tab.
  DATA: msg TYPE string.

  FIELD-SYMBOLS: <object> LIKE LINE OF objects.

  APPEND INITIAL LINE TO objects ASSIGNING <object>.

  <object>-object = i_objtype.
  <object>-obj_name = i_objname.

  CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
    EXPORTING
      suppress_syntax_check  = 'X'
*    suppress_generation    = suppress_generation
*    p_wb_manager           = p_wb_manager
*    suppress_insert        = suppress_insert
*    activate_ddic_objects  = SPACE
*    with_popup             = SPACE
*    cwb_mode               = cwb_mode
*    display_sysid          = SPACE
*    suppress_corr_insert   = SPACE
*  IMPORTING
*    p_checklist            = p_checklist
    TABLES
      objects                = objects
    EXCEPTIONS
      excecution_error       = 1
      cancelled              = 2
      insert_into_corr_error = 3
      OTHERS                 = 4
    .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               INTO msg.

    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = msg.
  ENDIF.

*  DATA: object TYPE trobjtype,
*        obj_name TYPE trobj_name.
*  DATA: objects TYPE dwinactiv_tab.
*
*  object   = i_objtype.
*  obj_name = i_objname.
*
*  CALL FUNCTION 'RS_WORKING_OBJECT_ACTIVATE'
*    EXPORTING
*      object                     = object
*      obj_name                   = obj_name
*      activate_only_this_object  = 'X'
*    TABLES
*      OBJECTS                    = OBJECTS
*    exceptions
*      object_not_in_working_area = 1
*      execution_error            = 2
*      cancelled                  = 3
*      insert_into_corr_error     = 4
*      OTHERS                     = 5.
*
*  IF sy-subrc <> 0.
*    msg = 'Error during activation'.
*    RAISE EXCEPTION TYPE zcx_saplink
*      EXPORTING
*        textid = zcx_saplink=>error_message
*        msg    = msg.
*  ENDIF.

endmethod.


method ACTIVATE_PACKAGE_OBJECTS.
  DATA: object   TYPE trobjtype,
        obj_name TYPE trobj_name.
  DATA: objects TYPE dwinactiv_tab.
  DATA: msg TYPE string.
  FIELD-SYMBOLS: <package_object> LIKE LINE OF me->package_objects.

  LOOP AT me->package_objects ASSIGNING <package_object>
    WHERE object = 'DTEL'
       OR object = 'VIEW'
       OR object = 'DOMA'
       OR object = 'TTYP'
       OR object = 'INDX'
       OR object = 'XINX'
       OR object = 'SHLP'
       OR object = 'ENQU'
       OR object = 'MCOB'
       OR object = 'TABL'
       OR object = 'SQLT'
       OR object = 'STRU'
       OR object = 'SFBF'
       OR object = 'SFSW'
       OR object = 'SFBS'
       OR object = 'SF01'
       OR object = 'METH'.
    object   = <package_object>-object.
    obj_name = <package_object>-obj_name.
    IF sy-batch = abap_true.
      CONCATENATE 'Try to activate:' <package_object>-object <package_object>-obj_name
        INTO msg SEPARATED BY space.
      MESSAGE msg TYPE 'I'.
    ENDIF.
    CALL FUNCTION 'RS_WORKING_OBJECT_ACTIVATE'
      EXPORTING
        object                     = object
        obj_name                   = obj_name
        activate_only_this_object  = 'X'
        dictionary_only            = 'X'
      TABLES
        OBJECTS                    = OBJECTS
      exceptions
        object_not_in_working_area = 1
        execution_error            = 2
        cancelled                  = 3
        insert_into_corr_error     = 4
        OTHERS                     = 5.

    IF sy-subrc <> 0.
      msg = 'Error during activation'.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = msg.
    ENDIF.
  ENDLOOP.
  LOOP AT me->package_objects ASSIGNING <package_object>
    WHERE object <> 'DTEL'
       AND object <> 'VIEW'
       AND object <> 'DOMA'
       AND object <> 'TTYP'
       AND object <> 'INDX'
       AND object <> 'XINX'
       AND object <> 'SHLP'
       AND object <> 'ENQU'
       AND object <> 'MCOB'
       AND object <> 'TABL'
       AND object <> 'SQLT'
       AND object <> 'STRU'
       AND object <> 'SFBF'
       AND object <> 'SFSW'
       AND object <> 'SFBS'
       AND object <> 'SF01'
       AND object <> 'METH'.
    object   = <package_object>-object.
    obj_name = <package_object>-obj_name.
    IF sy-batch = abap_true.
      CONCATENATE 'Try to activate:' <package_object>-object <package_object>-obj_name
        INTO msg SEPARATED BY space.
      MESSAGE msg TYPE 'I'.
    ENDIF.
    CALL FUNCTION 'RS_WORKING_OBJECT_ACTIVATE'
      EXPORTING
        object                     = object
        obj_name                   = obj_name
        activate_only_this_object  = 'X'
        dictionary_only            = ' '
      TABLES
        OBJECTS                    = OBJECTS
      exceptions
        object_not_in_working_area = 1
        execution_error            = 2
        cancelled                  = 3
        insert_into_corr_error     = 4
        OTHERS                     = 5.

    IF sy-subrc <> 0.
      msg = 'Error during activation'.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = msg.
    ENDIF.
  ENDLOOP.

endmethod.


method ACTIVATE_SLINKEES.

  DATA slinkee LIKE LINE OF i_slinkees.
  DATA objecttype TYPE string.
  DATA objectname TYPE string.

* Activate all the Slinkee's
  IF me->testrun IS INITIAL.
    LOOP AT i_slinkees INTO slinkee.
      me->activate(
          i_objtype = objecttype
          i_objname = objectname
      ).
    ENDLOOP.
  ENDIF.

endmethod.


method ADD_FILES_TO_ZIP.
  me->filelist_zip[] = i_files[].
endmethod.


method ADD_OBJECT_TO_NUGGET.

  DATA name TYPE string.
  DATA nugget TYPE nuggetrow.
  DATA objname TYPE string.
  DATA objtype TYPE string.
  DATA msg TYPE string.

  name = i_nugget_name.
  TRANSLATE name TO UPPER CASE.
  READ TABLE currentnuggets INTO nugget WITH KEY name = name.

  IF sy-subrc = 0.
    nugget-nuggetobject->addobjecttonugget( xmldocument = i_xml_document ).
  ELSE.
    CONCATENATE 'Nugget' name 'not found in Current Nuggets'
      INTO msg SEPARATED BY space.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = msg.
  ENDIF.

endmethod.


method ADD_PACKAGE_OBJECTS_TO_TR.
  DATA: objects TYPE TABLE OF trexreqob.
  DATA: msg TYPE string.

  FIELD-SYMBOLS: <package_object> LIKE LINE OF me->package_objects.
  FIELD-SYMBOLS: <object> LIKE LINE OF objects.

  IF i_transport IS INITIAL AND me->transport IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = 'You have to specify a transport request'.
  ELSEIF NOT i_transport IS INITIAL.
    me->transport = i_transport.
  ENDIF.

  " Add Package first
  APPEND INITIAL LINE TO objects ASSIGNING <object>.
  <object>-pgmid    = 'R3TR'.
  <object>-object   = 'DEVC'.
  <object>-obj_name = me->iv_package.

  LOOP AT me->package_objects ASSIGNING <package_object>.
    APPEND INITIAL LINE TO objects ASSIGNING <object>.
    <object>-object   = <package_object>-object.
    <object>-obj_name = <package_object>-obj_name.

    SELECT SINGLE pgmid FROM tadir INTO <object>-pgmid
      WHERE object = <package_object>-object
        AND obj_name = <package_object>-obj_name.
  ENDLOOP.

  DELETE objects WHERE pgmid = ''.

  DATA: ev_exception TYPE tr007-exception,
        es_msg       TYPE tr004-msgtext.

  CALL FUNCTION 'TR_EXT_INSERT_IN_REQUEST'
    EXPORTING
      iv_req_id    = me->transport
    IMPORTING
      ev_exception = ev_exception
      es_msg       = es_msg
    TABLES
      it_objects   = objects.
  IF NOT ev_exception IS INITIAL.
    msg = es_msg.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = msg.
  ENDIF.
endmethod.


method ADD_SLINKEE_TO_VERSIONCONTROL.
endmethod.


method ADD_TRANSPORT_TO_FILELIST.

  DATA: system     TYPE TMSSYSNAM,
        tp_dirtsts TYPE stms_tp_dirtsts,
        tefi       TYPE stmsttefi.

  DATA: data_dir    TYPE string,
        cofiles_dir TYPE string.

  FIELD-SYMBOLS: <tp_dirtst> LIKE LINE OF tp_dirtsts .

  IF me->transport IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = 'You have to specify a transport request'.
  ENDIF.

  system = sy-sysid.

  CALL FUNCTION 'TMS_MGR_CHECK_TRANSPORT_DIR'
   EXPORTING
     iv_system                = system
*     IV_DOMAIN                = IV_DOMAIN
*     IV_USE_LIST              = IV_USE_LIST
*     IV_KEEP_FILES            = IV_KEEP_FILES
*     IV_MONITOR               = 'X'
*     IV_VERBOSE               = IV_VERBOSE
   IMPORTING
     es_tp_dirtsts            = tp_dirtsts
*   TABLES
*     TT_SYS_CREATE            = TT_SYS_CREATE
*     TT_SYS_READ              = TT_SYS_READ
*     TT_DIR_TAB               = TT_DIR_TAB
   EXCEPTIONS
     read_config_failed       = 1
     OTHERS                   = 2.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = 'Error reading transport directory'.
  ENDIF.

  READ TABLE tp_dirtsts ASSIGNING <tp_dirtst> INDEX 1.
  READ TABLE <tp_dirtst>-tefi INTO tefi WITH KEY dir = 'data'.
  data_dir = tefi-path.
  READ TABLE <tp_dirtst>-tefi INTO tefi WITH KEY dir = 'cofiles'.
  cofiles_dir = tefi-path.

  CONCATENATE cofiles_dir '\' me->transport+3(7) '.' me->transport(3)
    INTO cofiles_dir.
  CONCATENATE data_dir '\R' me->transport+4(6) '.' me->transport(3)
    INTO data_dir.

  APPEND cofiles_dir to me->filelist_zip.
  APPEND data_dir to me->filelist_zip.

endmethod.


method BUILD_NUGGET_FROM_FILES.

  DATA afile TYPE filelistrowtype.
  DATA stemp TYPE string.
  DATA newnugget TYPE nuggetrow.

  stemp = i_nugget_name.
  TRANSLATE stemp TO UPPER CASE.
  READ TABLE currentnuggets TRANSPORTING NO FIELDS WITH KEY name = stemp.
  IF sy-subrc = 0.
    DELETE currentnuggets WHERE name = stemp.
  ENDIF.
  me->create_nugget( i_nugget_name ).

  LOOP AT filelist INTO afile.
    me->add_object_to_nugget(
      i_nugget_name = i_nugget_name
      i_xml_document = afile-xmldoc
    ).
  ENDLOOP.

endmethod.


method CHECKIN.
endmethod.


method CHECKOUT.
endmethod.


method CONSTRUCTOR.
  DATA plugininfo TYPE t_objecttable.
  DATA scheme TYPE string.
  DATA msg TYPE string.
  DATA zake_home_env TYPE string.

  CALL FUNCTION 'SWLWP_URI_PARSE'
    EXPORTING
      uri         = i_svnpath
    IMPORTING
      scheme      = scheme
    EXCEPTIONS
      uri_no_path = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    msg = 'The provided URL is not a valid URL'.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = msg.
  ENDIF.

  TRANSLATE scheme TO UPPER CASE.
  IF scheme = 'HTTP' OR scheme = 'HTTPS'.
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url    = i_svnpath
      IMPORTING
        client = httpclient.
  ENDIF.

  " Get file separator in batch and gui mode
  IF sy-batch = abap_true.
    IF sy-opsys = 'Windows NT'.
      file_separator = '\'.
    ELSE.
      file_separator = '/'.
    ENDIF.
  ELSE.
    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = me->file_separator
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4
    ).
    IF sy-subrc <> 0.
      " Default to Windows file separator
      me->file_separator = '\'.
    ENDIF.
  ENDIF.

  " Get the Path to the CMD script controlling SVN from
  cl_gui_frontend_services=>environment_get_variable(
    EXPORTING
      variable             = 'ZAKE_HOME'
    CHANGING
      value                = zake_home_env
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
  ).
  cl_gui_cfw=>flush( ).

  IF NOT zake_home_env IS INITIAL.
    DATA len TYPE i.

    len = strlen( zake_home_env ) - 1.

    IF zake_home_env+len(1) <> me->file_separator.
      CONCATENATE zake_home_env file_separator INTO zake_home_env.
    ENDIF.

    me->zake_home = zake_home_env.
  ELSE.
    DATA: platform TYPE i.
    cl_gui_frontend_services=>get_platform(
      RECEIVING
        platform             = platform
      EXCEPTIONS
        error_no_gui         = 1
        cntl_error           = 2
        not_supported_by_gui = 3
        OTHERS               = 4
    ).
    IF sy-subrc <> 0.
    ENDIF.
    IF platform >= 7 " Linux
      AND platform < 13. " Windows XP
      " applies to all unix like plattforms
      DATA: upload_path   TYPE string,
            download_path TYPE string.
      cl_gui_frontend_services=>get_upload_download_path(
        CHANGING
          upload_path                 = upload_path
          download_path               = download_path
        EXCEPTIONS
          cntl_error                  = 1
          error_no_gui                = 2
          not_supported_by_gui        = 3
          gui_upload_download_path    = 4
          upload_download_path_failed = 5
          OTHERS                      = 6
              ).
      IF sy-subrc <> 0.
      ENDIF.
      CONCATENATE download_path 'projects/' INTO me->zake_home.
    ENDIF.
  ENDIF.

  me->url      = i_svnpath.
  me->path     = i_localpath.

  IF i_username IS INITIAL AND
     i_password IS INITIAL.
    " Gregor Wolf, 2012-07-10
    " To allow ZAKE installation using AiE class resource
    " we must get rid of dictionary types
*    " Try to read username and password from the userdata table
*    SELECT SINGLE username password
*      FROM zake_userdata
*      INTO (me->username, me->password)
*      WHERE url = i_svnpath.
  ELSE.
    me->username = i_username.
    me->password = i_password.
  ENDIF.

  TRY.
      zsaplink=>getplugins(
        CHANGING
          objecttable = l_objtable
      ).
    CATCH cx_root.
      plugininfo-classname = 'ZSAPLINK_CLASS'.
      plugininfo-object    = 'CLAS'.
      APPEND plugininfo TO l_objtable.
      plugininfo-classname = 'ZSAPLINK_PROG'.
      plugininfo-object    = 'PROG'.
      APPEND plugininfo TO l_objtable.
  ENDTRY.

endmethod.


method CREATE_NUGGET.
  DATA newnugget TYPE nuggetrow.
  DATA xmldoc TYPE REF TO if_ixml_document.

  newnugget-name = i_nugget_name.
  TRANSLATE newnugget-name TO UPPER CASE.
  CREATE OBJECT newnugget-nuggetobject
    EXPORTING
      name = i_nugget_name.
  xmldoc = newnugget-nuggetobject->createemptyxml( i_nugget_name ).

  APPEND newnugget TO currentnuggets.

endmethod.


method CREATE_SLINKEE.

  DATA _objname TYPE string.
  DATA _objtype TYPE string.
  DATA xml_document TYPE REF TO if_ixml_document.
  DATA xml TYPE string.
  DATA deffilename TYPE string.
  DATA packagepath TYPE string.
  DATA folderpath TYPE string.
  DATA fullpath TYPE string.

  " Create the Filename, Folderpath and FullPath
  CONCATENATE i_package_object-obj_name '.slnk' INTO deffilename.
  CONCATENATE path
    me->file_separator iv_package INTO packagepath.
  CONCATENATE packagepath
    me->file_separator i_package_object-object
    me->file_separator INTO folderpath.
  CONCATENATE folderpath deffilename INTO fullpath.

  " Don't export if the Deletion Flag is set
  " Instead we delete the file
  IF i_package_object-delflag = 'X'.
    me->delete_file( fullpath ).
  ELSE.

    _objtype = i_package_object-object.
    _objname = i_package_object-obj_name.

    me->get_slinkee(
      EXPORTING
        i_objecttype   = _objtype
        i_objectname   = _objname
      IMPORTING
        e_slinkee_xml  = xml
        e_slinkee_ixml = xml_document
    ).

    IF me->download_slinkees_to_lm = abap_true.
      me->download_xml_to_lm(
          i_fullpath   = fullpath
          i_xml_string = xml
      ).
    ENDIF.

    IF NOT i_nugget_name IS INITIAL.
      me->add_object_to_nugget(
          i_nugget_name  = i_nugget_name
          i_xml_document = xml_document
      ).
    ENDIF.
  ENDIF.

endmethod.


method CREATE_SLINKEES.

  DATA objectline LIKE LINE OF l_objtable.
  DATA msg TYPE string.
  DATA nugget_string  TYPE string.
  DATA zipfile TYPE xstring.

  FIELD-SYMBOLS: <package_object> LIKE LINE OF package_objects.

  " Check if all the plugins we need are installed
  LOOP AT package_objects ASSIGNING <package_object>.
    READ TABLE l_objtable INTO objectline WITH KEY object = <package_object>-object.
    IF sy-subrc <> 0.
      msg = <package_object>-object.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>no_plugin
          msg    = msg.
    ENDIF.
  ENDLOOP.
  IF NOT i_nugget_name IS INITIAL.
    me->create_nugget( i_nugget_name = i_nugget_name ).
  ENDIF.
  " now we can export all the objects
  LOOP AT package_objects ASSIGNING <package_object>.
    me->create_slinkee(
      i_nugget_name = i_nugget_name
      i_package_object = <package_object>
    ).
  ENDLOOP.
  IF NOT i_nugget_name IS INITIAL.
    nugget_string = me->get_nugget_string( i_nugget_name   = i_nugget_name ).

    IF me->download_nugget_to_lm = abap_true.
      me->download_xml_to_lm(
        EXPORTING
          i_fullpath   = i_nugget_name
          i_xml_string = nugget_string
      ).
    ENDIF.
    IF me->download_zip_to_lm_flag = abap_true.
      me->download_zip_to_lm(
          i_fullpath    = i_nugget_name
          i_stringtozip = nugget_string
      ).
    ENDIF.
  ENDIF.
endmethod.


method CREATE_TRANSPORT_REQUEST.
  DATA: lv_text TYPE as4text.
  DATA: return TYPE  bapiret2,
        task_list TYPE TABLE OF bapiscts07,
        authorlist TYPE TABLE OF bapiscts12.

  FIELD-SYMBOLS: <task> LIKE LINE OF task_list,
                 <author> LIKE LINE OF authorlist.

  IF i_text IS INITIAL.
    CONCATENATE 'ZAKE Transport of package' me->iv_package
      INTO lv_text SEPARATED BY space.
  ELSE.
    lv_text = i_text.
  ENDIF.
*
*  This Function Module does not create tasks
*  CALL FUNCTION 'TR_EXT_CREATE_REQUEST'
*    EXPORTING
*     iv_request_type       = 'T' " Transport of Copies
**     IV_TARGET             = ' '
*      iv_author             = i_author
*      iv_text               = lv_text
**     IV_REQ_ATTR           = IV_REQ_ATTR
**     IV_ATTR_REF           = IV_ATTR_REF
*   IMPORTING
*     es_req_id             = me->transport
**     ES_REQ_HEADER         = ES_REQ_HEADER
**     ES_MSG                = ES_MSG
**     EV_EXCEPTION          = EV_EXCEPTION
*            .

  CALL FUNCTION 'BAPI_CTREQUEST_CREATE'
    EXPORTING
      author    = i_author
      text      = lv_text
    IMPORTING
      requestid = me->transport
      return    = return
    TABLES
      task_list = task_list.
** In the moment we don't need a task.
*  APPEND INITIAL LINE TO task_list ASSIGNING <task>.
*  <task>-author    = i_author.
*  <task>-text      = lv_text.
*  <task>-requestid = me->transport.
*  <task>-type      = 'S'. " Development/Correction
*
*  APPEND INITIAL LINE TO authorlist ASSIGNING <author>.
*  <author>-task_owner = i_author.
*
*  CALL FUNCTION 'BAPI_CTREQUEST_CREATE_TASKS'
*    EXPORTING
*      requestid  = me->transport
*    IMPORTING
*      return     = return
*    TABLES
*      authorlist = authorlist
*      task_list  = task_list.
*
*  READ TABLE task_list ASSIGNING <task> INDEX 2.
*
*  me->task = <task>-taskid.

  r_transport = me->transport.
endmethod.


method DELETE.

  DATA typename TYPE string.
  DATA objname TYPE string.
  DATA l_objline TYPE t_objecttable.

  CALL METHOD zsaplink=>getobjectinfofromixmldoc
    EXPORTING
      ixmldocument = i_xml_document
    IMPORTING
      objtypename  = typename
      objname      = objname.

endmethod.


method DELETE_FILE.
  DATA: rc TYPE i,
        file_exists TYPE ABAP_BOOL.

  file_exists = cl_gui_frontend_services=>FILE_EXIST( i_fullpath ).

  CHECK file_exists = abap_true.

  cl_gui_frontend_services=>file_delete(
    EXPORTING
      filename = i_fullpath
    CHANGING
      rc       = rc
  ).
endmethod.


method DOWNLOAD_XML_TO_LM.

  DATA xlm_xstring  TYPE xstring.
  DATA temptable    TYPE w3mimetabtype.
  DATA bin_filesize TYPE i.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = i_xml_string
    IMPORTING
      buffer = xlm_xstring.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = xlm_xstring
    IMPORTING
      output_length = bin_filesize
    TABLES
      binary_tab    = temptable.

  cl_gui_frontend_services=>gui_download(
     EXPORTING
       bin_filesize = bin_filesize
       filename     = i_fullpath
       filetype     = 'BIN'
     CHANGING
       data_tab     = temptable
     EXCEPTIONS
       file_write_error          = 1
       no_batch                  = 2
       gui_refuse_filetransfer   = 3
       invalid_type              = 4
       no_authority              = 5
       unknown_error             = 6
       header_not_allowed        = 7
       separator_not_allowed     = 8
       filesize_not_allowed      = 9
       header_too_long           = 10
       dp_error_create           = 11
       dp_error_send             = 12
       dp_error_write            = 13
       unknown_dp_error          = 14
       access_denied             = 15
       dp_out_of_memory          = 16
       disk_full                 = 17
       dp_timeout                = 18
       file_not_found            = 19
       dataprovider_exception    = 20
       control_flush_error       = 21
       not_supported_by_gui      = 22
       error_no_gui              = 23
       OTHERS                    = 24
  ).
  IF sy-subrc <> 0.
    DATA: msg TYPE string.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO msg.
    CONCATENATE msg i_fullpath INTO msg SEPARATED BY space.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = msg.
  ENDIF.
endmethod.


method DOWNLOAD_ZIP_TO_LM.

  DATA xstringtozip TYPE xstring.
  DATA temptable TYPE w3mimetabtype.
  DATA zip TYPE REF TO cl_abap_zip.
  DATA content TYPE xstring.
  DATA zipfilename TYPE string.
  DATA zipfile TYPE xstring.
  DATA filename TYPE string.
  DATA bin_filesize TYPE i.

  DATA file_to_zip LIKE LINE OF me->filelist_zip.
  DATA bin_tab TYPE TABLE OF x255.
  DATA filelength TYPE i.
  DATA tmpfullpath TYPE string.
  DATA slash TYPE string.
  " Background processing
  DATA bin_data LIKE LINE OF bin_tab.
  DATA len TYPE i.
  DATA alen TYPE i.
  DATA templine LIKE LINE OF temptable.

  filename = zake=>get_filename_from_fullpath( i_fullpath ).

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = i_stringtozip
    IMPORTING
      buffer = xstringtozip.

  CREATE OBJECT zip.
  zip->add(
    EXPORTING
      name    = filename
      content = xstringtozip
  ).

  IF me->download_transport_to_lm = abap_true.
    me->add_transport_to_filelist( ).
  ENDIF.

  IF NOT me->filelist_zip IS INITIAL.
    LOOP AT me->filelist_zip INTO file_to_zip.
      CLEAR: xstringtozip, content, bin_tab.
      tmpfullpath = file_to_zip.
      slash = '/'.
      REPLACE ALL OCCURRENCES OF slash IN tmpfullpath WITH me->file_separator.

      IF sy-batch = abap_true.

        DESCRIBE FIELD bin_data LENGTH len IN BYTE MODE.
        OPEN DATASET tmpfullpath  FOR INPUT IN BINARY MODE.
        WHILE sy-subrc = 0.
          READ DATASET tmpfullpath INTO bin_data MAXIMUM LENGTH len ACTUAL LENGTH alen.
          APPEND bin_data TO bin_tab.
          filelength = filelength + alen.
        ENDWHILE.

        CLOSE DATASET tmpfullpath.
      ELSE.
        cl_gui_frontend_services=>gui_upload(
          EXPORTING
            filename                = tmpfullpath    " Name of file
            filetype                = 'BIN'    " File Type (ASCII, Binary)
          IMPORTING
            filelength              = filelength
          CHANGING
            data_tab                = bin_tab
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            not_supported_by_gui    = 17
            error_no_gui            = 18
            OTHERS                  = 19
        ).
        IF sy-subrc <> 0.
          DATA: msg TYPE string.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO msg.
          CONCATENATE msg tmpfullpath INTO msg SEPARATED BY space.
          RAISE EXCEPTION TYPE zcx_saplink
            EXPORTING
              textid = zcx_saplink=>error_message
              msg    = msg.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = filelength
        IMPORTING
          buffer       = xstringtozip
        TABLES
          binary_tab   = bin_tab.

      filename = zake=>get_filename_from_fullpath( file_to_zip ).
      zip->add(
        EXPORTING
          name    = filename
          content = xstringtozip
      ).
    ENDLOOP.
  ENDIF.

  zipfile = zip->save( ).

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = zipfile
    IMPORTING
      output_length = bin_filesize
    TABLES
      binary_tab    = temptable.

  CONCATENATE i_fullpath '.zip' INTO zipfilename.
  IF sy-batch = abap_true.
    OPEN DATASET zipfilename FOR OUTPUT IN BINARY MODE.
    LOOP AT temptable INTO templine.
      TRANSFER templine TO zipfilename.
    ENDLOOP.
    CLOSE DATASET zipfilename.
  ELSE.
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize = bin_filesize
        filename     = zipfilename
        filetype     = 'BIN'
      CHANGING
        data_tab     = temptable
     ).
  ENDIF.

endmethod.


method GET_ALL_FILES.

  DATA xmldocument TYPE REF TO if_ixml_document.
  DATA httpstatuscode TYPE i.
  DATA httpstatusreason TYPE string.
  DATA responsedata TYPE string.
  DATA iterator TYPE REF TO if_ixml_node_iterator.
  DATA lowercaseanchors TYPE REF TO if_ixml_node_filter.
  DATA uppercaseanchors TYPE REF TO if_ixml_node_filter.
  DATA anchorfilter TYPE REF TO if_ixml_node_filter.
  DATA element TYPE REF TO if_ixml_element.
  DATA filename TYPE string.

  httpclient->send( ).
  httpclient->receive( ).
  httpclient->refresh_request( ).

  httpclient->response->get_status( IMPORTING code = httpstatuscode reason = httpstatusreason ).
  IF httpstatuscode = 200.
    responsedata = httpclient->response->get_cdata( ).
    xmldocument = zsaplink=>convertstringtoixmldoc( responsedata ).
    lowercaseanchors = xmldocument->create_filter_name( 'a' ).
    uppercaseanchors = xmldocument->create_filter_name( 'A' ).
    anchorfilter = xmldocument->create_filter_or( filter1 = lowercaseanchors filter2 = uppercaseanchors ).
    iterator = xmldocument->create_iterator_filtered( anchorfilter ).
    element ?= iterator->get_next( ).
    WHILE element IS NOT INITIAL.
      filename = element->get_attribute( 'href' ).
      IF filename IS INITIAL.
        filename = element->get_attribute( 'HREF' ).
      ENDIF.
      IF NOT filename CO './'.
        CONCATENATE url filename INTO filename.
        me->get_file( i_filename = filename ).
      ENDIF.
      element ?= iterator->get_next( ).
    ENDWHILE.
    httpclient->refresh_request( ).
  ELSE.
    RAISE EXCEPTION TYPE zcx_saplink.
  ENDIF.

endmethod.


method GET_FILE.

  DATA httpstatuscode TYPE i.
  DATA httpstatusreason TYPE string.
  DATA responsedata TYPE string.
  DATA filerow TYPE filelistrowtype.
  DATA tableostrings TYPE table_of_strings.
  DATA stemp TYPE string.

  httpclient->request->set_header_field(
    name  = '~request_uri'
    value = i_filename
  ).
  httpclient->request->set_header_field(
    name  = 'Cache-Control'
    value = 'no-cache'
  ).
  httpclient->send( ).
  httpclient->receive( ).

  httpclient->response->get_status( IMPORTING code = httpstatuscode reason = httpstatusreason ).
  IF httpstatuscode = 200.
    responsedata = httpclient->response->get_cdata( ).
    SPLIT responsedata AT cl_abap_char_utilities=>cr_lf INTO TABLE tableostrings.
    CLEAR responsedata.
    LOOP AT tableostrings INTO stemp.
      CONCATENATE responsedata stemp cl_abap_char_utilities=>newline INTO responsedata.
    ENDLOOP.
    r_xml_document = zsaplink=>convertstringtoixmldoc( responsedata ).
    filerow-name = i_filename.
    filerow-xmldoc = r_xml_document.
    APPEND filerow TO filelist.
    httpclient->refresh_request( ).
  ELSE.
    RAISE EXCEPTION TYPE zcx_saplink.
  ENDIF.

endmethod.


method GET_FILENAME_FROM_FULLPATH.
  DATA pathcomponents TYPE string_table.
  DATA l TYPE i.
  DATA slash TYPE string VALUE '/'.
  DATA tmpfullpath TYPE string.
  tmpfullpath = i_fullpath.
  REPLACE ALL OCCURRENCES OF slash IN tmpfullpath WITH '\'.

  SPLIT tmpfullpath AT '\' INTO TABLE pathcomponents.
  DESCRIBE TABLE pathcomponents LINES l.
  READ TABLE pathcomponents INTO r_filename INDEX l.
endmethod.


method GET_NUGGET_STRING.

  DATA name TYPE string.
  DATA nugget TYPE nuggetrow.
  DATA xml_doc TYPE REF TO if_ixml_document.

  name = i_nugget_name.
  TRANSLATE name TO UPPER CASE.
  READ TABLE currentnuggets INTO nugget WITH KEY name = name.
  IF sy-subrc = 0.
    xml_doc = nugget-nuggetobject->createixmldocfromnugget( ).
    r_nugget_string = zsaplink=>convertixmldoctostring( xml_doc ).
    r_nugget_string = zake=>set_break_to_cr_lf( r_nugget_string ).
  ENDIF.

endmethod.


method GET_SLINKEE.

  DATA objectline LIKE LINE OF l_objtable.
  DATA targetobject TYPE REF TO zsaplink.

  " Read the needed Class for the Object type
  READ TABLE l_objtable INTO objectline WITH KEY object = i_objecttype.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>not_found
        object = i_objecttype.
  ENDIF.

  CREATE OBJECT targetobject
    TYPE
    (objectline-classname)
    EXPORTING
      name = i_objectname.

  e_slinkee_ixml = targetobject->createixmldocfromobject( ).
  e_slinkee_xml  = zsaplink=>convertixmldoctostring( e_slinkee_ixml ).
  e_slinkee_xml  = zake=>set_break_to_cr_lf( e_slinkee_xml ).

endmethod.


method GET_SLINKEES_FROM_LM.

  DATA: result TYPE abap_bool.

  DATA: file_table TYPE TABLE OF file_info,
        file LIKE LINE OF file_table,
        count	TYPE i.
  DATA: tmpfullpath TYPE string.
  DATA: tmppath TYPE string.
  DATA: slash          TYPE string.
  DATA: xmlstring TYPE string.
  DATA: slinkee LIKE LINE OF c_slinkees.
  DATA: msg TYPE string.
  " For Batch processing
  DATA: name     TYPE pfeflnamel,
        dir_name TYPE epsdirnam,
        file_tbl TYPE TABLE OF salfldir,
        dir_list TYPE TABLE OF epsfili.

  DATA: file_type TYPE epsfiltyp,
        file_name TYPE epsfilnam.

  FIELD-SYMBOLS: <file> LIKE LINE OF file_tbl,
                 <dir_list> LIKE LINE OF dir_list.

  tmpfullpath = i_fullpath.
  slash = '/'.

  REPLACE ALL OCCURRENCES OF slash IN tmpfullpath WITH me->file_separator.
  " When running as a Batch Job we had to read from the server
  IF sy-batch = abap_true.
    CONCATENATE 'Read content of directory:' i_fullpath
       INTO msg SEPARATED BY space.
    MESSAGE msg TYPE 'I'.
    name = i_fullpath.
    dir_name = i_fullpath.

    " Only this Function module provides also directory names
    " but filename length is limited to 32 characters
    CALL FUNCTION 'RZL_READ_DIR_LOCAL'
      EXPORTING
        name           = name
      TABLES
        file_tbl       = file_tbl
      EXCEPTIONS
        argument_error = 1
        not_found      = 2
        OTHERS         = 3.

    " Check which of the returned names are folders
    LOOP AT file_tbl ASSIGNING <file>
      WHERE name <> '.' AND name <> '..'
        AND NOT name CP '.*'.
      CLEAR file.
      file_name = <file>-name.
      CALL FUNCTION 'EPS_GET_FILE_ATTRIBUTES'
        EXPORTING
          file_name              = file_name
          dir_name               = dir_name
        IMPORTING
          file_type              = file_type
        EXCEPTIONS
          read_directory_failed  = 1
          read_attributes_failed = 2
          OTHERS                 = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      " Add only directories to the file table
      IF file_type = 'directory'.
        CONCATENATE 'File was identified as directory: ' file_name
          INTO msg SEPARATED BY space.
        MESSAGE msg TYPE 'I'.
        file-isdir = '1'.
        file-filename = <file>-name.
        APPEND file TO file_table.
      ENDIF.

    ENDLOOP.
    " Use this function to read all files.
    " File Name lengh maximum 40 chars
    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        dir_name               = dir_name
      TABLES
        dir_list               = dir_list
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.

    LOOP AT dir_list ASSIGNING <dir_list>.
      CLEAR file.
      file-filename = <dir_list>-name.
      APPEND file TO file_table.
    ENDLOOP.

  ELSE.
    cl_gui_frontend_services=>directory_list_files(
      EXPORTING
        directory                   = tmpfullpath   " Directory To Search
      CHANGING
        file_table                  = file_table    " Return Table for the Found Files
        count                       = count    " Number of Files/Dir Found
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        OTHERS                      = 6
    ).
    IF sy-subrc <> 0.

    ENDIF.
  ENDIF.

  DELETE file_table WHERE ishidden = '1'.

  LOOP AT file_table INTO file.
    CONCATENATE tmpfullpath file_separator file-filename INTO tmppath.
    IF file-isdir = '1'.
      me->get_slinkees_from_lm(
        EXPORTING
          i_fullpath    = tmppath
        CHANGING
          c_slinkees    = c_slinkees
      ).
    ELSE.
      IF file-filename CP '*.slnk'.
        xmlstring = me->read_xml_from_lm( i_fullpath  = tmppath ).
        slinkee = zsaplink=>convertstringtoixmldoc( xmlstring ).
        APPEND slinkee TO c_slinkees.
      ENDIF.
    ENDIF.
  ENDLOOP.

endmethod.


method INSTALL.

  DATA: l_excclass TYPE REF TO zcx_saplink.
  DATA: l_message TYPE string.

  DATA installer TYPE REF TO zsaplink.
  DATA l_objline TYPE t_objecttable.

  CALL METHOD zsaplink=>getobjectinfofromixmldoc
    EXPORTING
      ixmldocument = i_xml_document
    IMPORTING
      objtypename  = e_objecttype
      objname      = e_objectname.

  " Grab the right plugin from the cache built durning the constructor
  READ TABLE l_objtable INTO l_objline WITH KEY object = e_objecttype.
  IF sy-subrc = 0.
    CREATE OBJECT installer
      TYPE
      (l_objline-classname)
      EXPORTING
        name = e_objectname.
  ELSE.
    " TODO: Make the package management work to make this work:
    "
    " Seems that we don't have the plugin installed,
    " but what if zake installed it previously.
    " Re-read the plugin hierarchy
    "
    CALL METHOD zsaplink=>getplugins( CHANGING objecttable = l_objtable ).
    READ TABLE l_objtable INTO l_objline WITH KEY object = e_objecttype.
    IF sy-subrc = 0.
      CREATE OBJECT installer
        TYPE
        (l_objline-classname)
        EXPORTING
          name = e_objectname.
    ENDIF.
  ENDIF.
  IF installer IS BOUND.
    " Attempt to install
    TRY.
        installer->createobjectfromixmldoc(
          ixmldocument = i_xml_document
          devclass     = i_devclass
          overwrite    = 'X'
        ).
        " bad times
      CATCH zcx_saplink INTO l_excclass.
        l_message = l_excclass->get_text( ).
        WRITE: / l_message.
    ENDTRY.
  ELSE.
    WRITE: / 'No plugin installed for', e_objecttype.
  ENDIF.

endmethod.


method INSTALL_OBJECTS.

  DATA: slinkees TYPE igs_ce_cu_tab,
        slinkees_to_install TYPE igs_ce_cu_tab.
  FIELD-SYMBOLS: <slinkee> LIKE LINE OF slinkees,
                 <object> LIKE LINE OF i_objects.
  DATA: objecttype TYPE string.
  DATA: objectname TYPE string.

  " Read all the Slinkee's
  me->get_slinkees_from_lm(
    EXPORTING
      i_fullpath = path
    CHANGING
      c_slinkees = slinkees
  ).

  LOOP AT slinkees ASSIGNING <slinkee>.
    zsaplink=>getobjectinfofromixmldoc(
      EXPORTING
        ixmldocument = <slinkee>
      IMPORTING
        objtypename  = objecttype
        objname      = objectname
    ).
    READ TABLE i_objects ASSIGNING <object>
      WITH KEY object = objecttype
               obj_name = objectname.
    IF <object> IS ASSIGNED.
      APPEND <slinkee> TO slinkees_to_install.
    ENDIF.
  ENDLOOP.

  me->install_slinkees( slinkees_to_install ).

endmethod.


method INSTALL_SLINKEES.

  DATA: slinkee LIKE LINE OF i_slinkees.
  DATA: objecttype TYPE string.
  DATA: objectname TYPE string.
  FIELD-SYMBOLS: <package_object> LIKE LINE OF me->package_objects.

  " Install all the Slinkee's
  LOOP AT i_slinkees INTO slinkee.
    IF me->testrun = abap_false.
      me->install(
        EXPORTING
          i_xml_document = slinkee
        IMPORTING
          e_objecttype  = objecttype
          e_objectname  = objectname
      ).
    ELSE.
      " Even in the testrun we want to fill the Objectlist
      CALL METHOD zsaplink=>getobjectinfofromixmldoc
        EXPORTING
          ixmldocument = slinkee
        IMPORTING
          objtypename  = objecttype
          objname      = objectname.
    ENDIF.

    APPEND INITIAL LINE TO me->package_objects ASSIGNING <package_object>.
    <package_object>-object   = objecttype.
    <package_object>-obj_name = objectname.
  ENDLOOP.

endmethod.


method INSTALL_SLINKEES_FROM_LM.

  DATA: slinkees TYPE igs_ce_cu_tab.

  me->testrun = i_testrun.
* Read all the Slinkee's
  me->get_slinkees_from_lm(
    EXPORTING
      i_fullpath = path
    CHANGING
      c_slinkees = slinkees
  ).

  me->install_slinkees( slinkees ).

endmethod.


method INSTALL_SLINKEE_FROM_LM.

  DATA: xmlstring TYPE string,
        slinkee TYPE REF TO if_ixml_document.

  xmlstring = me->read_xml_from_lm( i_fullpath = i_fullpath ).
  slinkee = zsaplink=>convertstringtoixmldoc( xmlstring ).
  me->install( slinkee ).

endmethod.


method READ_XML_FROM_LM.

  DATA: temptable_char TYPE table_of_strings,
        bin_tab TYPE TABLE OF x255,
        tempstring TYPE string.
  DATA filelength TYPE i.
  " Background processing
  DATA bin_data LIKE LINE OF bin_tab.
  DATA len TYPE i.
  DATA alen TYPE i.

  IF sy-batch = abap_true.
    DESCRIBE FIELD bin_data LENGTH len IN BYTE MODE.
    OPEN DATASET i_fullpath FOR INPUT IN BINARY MODE.
    WHILE sy-subrc = 0.
      READ DATASET i_fullpath INTO bin_data MAXIMUM LENGTH len ACTUAL LENGTH alen.
      APPEND bin_data TO bin_tab.
      filelength = filelength + alen.
    ENDWHILE.
    CLOSE DATASET i_fullpath.
*    " Text Mode
*    OPEN DATASET i_fullpath  FOR INPUT IN TEXT MODE
*                             ENCODING DEFAULT
*                             WITH SMART LINEFEED.
*    WHILE sy-subrc = 0.
*      READ DATASET i_fullpath INTO tempstring.
*      APPEND tempstring TO temptable_char.
*    ENDWHILE.
*
*    CLOSE DATASET i_fullpath.
  ELSE.
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
         filename               = i_fullpath  " Name of file
        filetype                = 'BIN'       " File Type (ASCII, Binary)
      IMPORTING
        filelength              = filelength
      CHANGING
        data_tab                = bin_tab     " Transfer table for file contents
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>system_error.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
    EXPORTING
      input_length = filelength
    IMPORTING
      text_buffer  = r_xml_string
    TABLES
      binary_tab   = bin_tab.

  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
    IN r_xml_string WITH cl_abap_char_utilities=>newline.
*  LOOP AT temptable_char INTO tempstring.
*    CONCATENATE r_xml_string tempstring cl_abap_char_utilities=>newline
*      INTO r_xml_string.
*  ENDLOOP.

endmethod.


method RELEASE_TRANSPORT_REQUEST.

  DATA: return TYPE bapiret2,
        msg    TYPE string.

  DATA: author TYPE tr_as4user,
        requests TYPE TABLE OF trexreqhd.

  DATA: released TYPE boolean VALUE abap_false.

  FIELD-SYMBOLS: <request> LIKE LINE OF requests.

  IF i_transport IS INITIAL AND me->transport IS INITIAL.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = 'You have to specify a transport request'.
  ELSEIF NOT i_transport IS INITIAL.
    me->transport = i_transport.
  ENDIF.

  CALL FUNCTION 'BAPI_CTREQUEST_RELEASE'
    EXPORTING
      requestid = me->transport
      complete  = 'X'
    IMPORTING
      return    = return.

  IF return-type = 'E'.
    msg = return-message.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = msg.
  ENDIF.

  author = sy-uname.

  WHILE released = abap_false.
    CLEAR: requests.
    CALL FUNCTION 'TR_EXT_GET_REQUESTS'
      EXPORTING
        iv_author     = author
        iv_req_status = 'R'
      TABLES
        et_requests   = requests.

    READ TABLE requests ASSIGNING <request>
      WITH KEY req_id = me->transport
               status = 'R'. " Released

    IF sy-subrc <> 0.
      WAIT UP TO 5 SECONDS.
    ELSE.
      released = abap_true.
    ENDIF.
  ENDWHILE.
endmethod.


method SAVE_NUGGET.

  DATA stemp TYPE string.
  DATA nugget TYPE nuggetrow.
  DATA nugget_name TYPE string.
  DATA xmlstring TYPE string.

  TYPES: BEGIN OF t_char,
           maxchar(65535) TYPE c,
         END OF t_char.

  DATA: temptable_char TYPE TABLE OF t_char.

  nugget_name = i_nugget_name.
  TRANSLATE nugget_name TO UPPER CASE.
  READ TABLE currentnuggets INTO nugget WITH KEY name = nugget_name.

  stemp = i_target.
  TRANSLATE stemp TO UPPER CASE.

  IF stemp(4) = 'HTTP'.
* Post to SVN
  ELSE.
    xmlstring = me->get_nugget_string( nugget_name ).

    SPLIT xmlstring AT cl_abap_char_utilities=>newline
    INTO TABLE temptable_char.

    cl_gui_frontend_services=>gui_download(
          EXPORTING
            filename = i_target
            filetype = 'DAT'
          CHANGING
            data_tab = temptable_char
    ).
  ENDIF.

endmethod.


method SET_BREAK_TO_CR_LF.
  r_string = i_string.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
        IN r_string WITH cl_abap_char_utilities=>newline.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
        IN r_string WITH cl_abap_char_utilities=>cr_lf.
endmethod.


method SET_CHECKIN_OBJECTS.

  FIELD-SYMBOLS: <object> LIKE LINE OF i_objects.
  DATA: package_object LIKE LINE OF me->package_objects.

  LOOP AT i_objects ASSIGNING <object>.
    MOVE-CORRESPONDING <object> TO package_object.
    APPEND package_object TO me->package_objects.
  ENDLOOP.

endmethod.


method SET_PACKAGE.
  me->iv_package = i_package.
endmethod.


METHOD set_package_objects.
  DATA: msg   TYPE string,
        where TYPE string.

  DATA: objects TYPE TABLE OF t_packageobjecttable.

  me->iv_package = i_package.

  IF sy-saprl >= 700.
    where = `devclass EQ me->iv_package AND pgmid EQ 'R3TR' AND delflag NE 'X'`.
  ELSE.
    "//-> Mar: Added logic discard deleted objects from Package - 08/20/2008
    " Comment this line when ABAP Release <= 6.40
    where = `devclass EQ me->iv_package AND pgmid EQ 'R3TR'`.
  ENDIF.

  SELECT object obj_name srcsystem
      FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE objects " ##TOO_MANY_ITAB_FIELDS
      WHERE (where).
  "//<- Mar: Added logic discard deleted objects from Package - 08/20/2008
  IF sy-subrc <> 0.
    MESSAGE s208(00) WITH 'Package does not exist or is empty' INTO msg.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = msg.
  ENDIF.

  APPEND LINES OF objects TO package_objects.
ENDMETHOD.


method SET_PACKAGE_OF_PACKAGE_OBJECTS.
  DATA: pgmid TYPE pgmid.
  DATA: msg TYPE string.
  DATA: devclass TYPE devclass.

  FIELD-SYMBOLS: <package_object> LIKE LINE OF me->package_objects.

  SELECT SINGLE pgmid FROM tadir INTO pgmid
    WHERE pgmid = 'R3TR'
      AND object = 'DEVC'
      AND obj_name = me->iv_package.

  IF sy-subrc <> 0.
    CONCATENATE 'The package' me->iv_package 'does not exist, please create it first'
      INTO msg SEPARATED BY space.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = msg.
  ENDIF.

  LOOP AT me->package_objects ASSIGNING <package_object>.

    SELECT SINGLE pgmid devclass FROM tadir INTO (pgmid, devclass)
      WHERE object = <package_object>-object
        AND obj_name = <package_object>-obj_name.

    IF devclass <> me->iv_package.
      CALL FUNCTION 'TRINT_TADIR_MODIFY'
        EXPORTING
*     AUTHOR                     = '~'
*     CPROJECT                   = '~'
          devclass                   =  me->iv_package
*     EDTFLAG                    = '~'
*     EXISTS                     = '~'
*     GENFLAG                    = '~'
*     MASTERLANG                 = '~'
          object                     = <package_object>-object
          obj_name                   = <package_object>-obj_name
          pgmid                      = pgmid
*     SRCDEP                     = '~'
*     SRCSYSTEM                  = '~'
*     VERSID                     = '~'
*     CHANGE_MASTERLANG          = '~'
*     FORCE_MODE                 = '~'
*     PAKNOCHECK                 = '~'
*     OBJSTABLTY                 = '~'
*     DELFLAG                    = '~'
*     TRANSL_TECH_TEXT           = '~'
*   IMPORTING
*     ES_TADIR                   = ES_TADIR
       EXCEPTIONS
         object_exists_global       = 1
         object_exists_local        = 2
         object_has_no_tadir        = 3
         OTHERS                     = 4
                .
      IF sy-subrc <> 0.
        msg = sy-subrc.
        CONCATENATE 'An error with sy-subrc =' msg 'occured'
          INTO msg SEPARATED BY space.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = msg.
      ENDIF.
    ENDIF.
  ENDLOOP.
endmethod.


method SET_TESTRUN.
  me->testrun = i_testrun.
endmethod.


method SYNTAXCHECK.
endmethod.


method UNSET_CHECKIN_OBJECT.
  DELETE me->package_objects WHERE object = i_object.
endmethod.


method UNSET_CHECKIN_OBJECT_BY_NAME.
  DELETE me->package_objects
  WHERE object = i_object
  AND obj_name = i_object_name.
endmethod.


method UNSET_CHECKIN_OBJECT_LIST.
  CLEAR me->package_objects.
endmethod.


method UPDATE.
endmethod.
endclass. "ZAKE implementation