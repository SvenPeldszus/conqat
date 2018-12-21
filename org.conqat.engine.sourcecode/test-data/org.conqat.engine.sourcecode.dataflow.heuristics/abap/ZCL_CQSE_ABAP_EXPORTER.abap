*----------------------------------------------------------------------*
*       CLASS ZCL_CQSE_ABAP_EXPORTER DEFINITION
*----------------------------------------------------------------------*
* ABAP Exporter, which exports ABAP code to a single ZIP file.
* Furthermore Code Inspector analysis can be performed on the exported
* source code objects.
*
* The resulting ZIP file is by default organized as follows:
* - 1st level directory: development class (package)
* - 2nd level directory: program type (PROG, FUGR, CLAS, INTF)
* - 3rd level directory:
*       *.abap files for programs, includes, classes and interfaces
*       *.enh.abap files for local definitions in classes
*       in FUGR: separate directory for each function group
* - 4th level directory (only FUGR): includes of a function group
*
* The order of the 1st and 2nd level directory may be changed by using
* attribute is_toplevel_dir_by_progtype.
*
* Besides the ABAP source code the following tables are exported
* as XML and CSV:
* - object_meta_data: holds file path in the zip, object name,
*                     object type, development class, creation date,
*                     last update date / time
* - method_includes: maps method names of a class to their include name
* - ci_results: results of Code Inspector analysis
*----------------------------------------------------------------------*
CLASS zcl_cqse_abap_exporter DEFINITION
  PUBLIC
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_cqse_atomic_source_object
                 zcl_cqse_classenh_srcobj
                 zcl_cqse_class_srcobj
                 zcl_cqse_composite_source_obj
                 zcl_cqse_fugr_srcobj
                 zcl_cqse_reposrc_object
                 zcl_cqse_source_object_base .

  PUBLIC SECTION.
    TYPE-POOLS abap .
    TYPE-POOLS seop .

    TYPES:
      BEGIN OF t_ci_variant,
          variant_user TYPE syuname,
          variant TYPE sci_chkv,
      END OF t_ci_variant .

    DATA is_toplevel_dir_by_progtype TYPE abap_bool VALUE abap_false. "#EC NOTEXT .  .  . " .
    DATA use_class_editor_source_format TYPE abap_bool VALUE abap_false. "#EC NOTEXT .  .  . " .
    CLASS-DATA:
      ci_messages_tab TYPE STANDARD TABLE OF scimessage .
    CLASS-DATA logger TYPE REF TO zcl_cqse_logger .

    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        !file_name TYPE string OPTIONAL .
    METHODS add_source_objects
      IMPORTING
        !selected_objects TYPE siw_tab_tadir .
    METHODS add_by_package_table
      IMPORTING
        !include_subpackages TYPE abap_bool DEFAULT abap_true
        !packages TYPE trdevclasses1 .
    METHODS add_by_package
      IMPORTING
        !include_subpackages TYPE abap_bool DEFAULT abap_true
        !package TYPE devclass .
    CLASS-METHODS select_by_package
      IMPORTING
        !include_subpackages TYPE abap_bool DEFAULT abap_true
        !packages TYPE trdevclasses1
      RETURNING
        value(selected_objects) TYPE siw_tab_tadir .
    METHODS add_by_package_pattern
      IMPORTING
        !include_subpackages TYPE abap_bool DEFAULT abap_true
        !pattern TYPE string .
    CLASS-METHODS select_by_package_pattern
      IMPORTING
        !include_subpackages TYPE abap_bool DEFAULT abap_true
        !pattern TYPE string
      RETURNING
        value(selected_objects) TYPE siw_tab_tadir .
    METHODS add_by_name_pattern
      IMPORTING
        !pattern TYPE string .
    CLASS-METHODS select_by_name_pattern
      IMPORTING
        !pattern TYPE string
      RETURNING
        value(selected_objects) TYPE siw_tab_tadir .
    METHODS save
      IMPORTING
        !save_to_server TYPE abap_bool DEFAULT abap_false
      RETURNING
        value(zip_content) TYPE xstring .
    METHODS add_ci_variant
      IMPORTING
        !variant_user TYPE syuname OPTIONAL
        !variant TYPE sci_chkv .
    CLASS-METHODS tidy_file_name
      IMPORTING
        !file_name TYPE csequence
      RETURNING
        value(tidy_file_name) TYPE string .
    METHODS add_table
      IMPORTING
        !table TYPE ANY TABLE
        !file_name TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES BEGIN OF t_ci_result.
            INCLUDE TYPE scir_rest.
    TYPES text TYPE string.
    TYPES END OF t_ci_result .

    TYPES t_ci_result_tab TYPE STANDARD TABLE OF t_ci_result WITH DEFAULT KEY.

    DATA:
     zip_file TYPE REF TO zcl_cqse_source_zip .
    DATA:
      meta_data_table TYPE STANDARD TABLE OF zif_cqse_datatypes=>t_source_metadata .
    DATA:
      ci_variants TYPE STANDARD TABLE OF t_ci_variant .
    DATA:
      method_includes TYPE zif_cqse_datatypes=>t_method_include_tab .
    DATA:
      ci_objects TYPE scit_objs .

    CLASS-METHODS init_code_inspector_messages .
    METHODS build_file_name
      IMPORTING
        !package TYPE csequence
        !source_object_type TYPE char4
        !base_file_name TYPE string
      RETURNING
        value(file_name) TYPE string .
    CLASS-METHODS is_empty_source
      IMPORTING
        !source TYPE string_table
      RETURNING
        value(is_empty) TYPE xfeld .
    CLASS-METHODS get_package_subtree
      IMPORTING
        !root_node TYPE devclass
        !ancestors TYPE REF TO trdevclasses1 .
    METHODS create_source_objects
      IMPORTING
        !tadir_entry TYPE tadir
      RETURNING
        value(source_objects) TYPE zif_cqse_datatypes=>t_source_object_tab.
    METHODS run_code_inspector
      RETURNING
        value(ci_results) TYPE t_ci_result_tab .
    METHODS run_code_inspecotr_single
      IMPORTING
        !variant TYPE t_ci_variant
      EXPORTING
        result_table TYPE scit_rest.
    METHODS run_code_inspecotr_object_list
      IMPORTING
        !variant TYPE t_ci_variant
      EXPORTING
        result_table TYPE scit_rest.
endclass. "ZCL_CQSE_ABAP_EXPORTER definition



*----------------------------------------------------------------------*
*       class ZCL_CQSE_ABAP_EXPORTER implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_ABAP_EXPORTER implementation.


  METHOD add_by_name_pattern.
    DATA:
        selected_objects TYPE siw_tab_tadir.

    logger->info(
        msg1 = 'Add objects by name pattern:'(001)
        msg2 = pattern
    ).

    selected_objects = select_by_name_pattern( pattern ).
    add_source_objects( selected_objects ).

  ENDMETHOD.                    "add_by_name_pattern


  METHOD add_by_package.
    DATA packages TYPE trdevclasses1.

    logger->info(
        msg1 = 'Add objects by package:'(003)
        msg2 = package
    ).

    APPEND package TO packages.
    add_by_package_table( packages ).
  ENDMETHOD.                    "add_by_package


  METHOD add_by_package_pattern.
    DATA:
        selected_objects TYPE siw_tab_tadir.

    logger->info(
        msg1 = 'Add objects by package pattern:'(004)
        msg2 = pattern
        msg3 = '/ sub packages:'(005)
        msg4 = include_subpackages
    ).


    selected_objects = select_by_package_pattern(
         include_subpackages = include_subpackages
         pattern = pattern
       ).
    add_source_objects( selected_objects ).
  ENDMETHOD.                    "EXPORT_PACKAGE


  METHOD add_by_package_table.
    DATA:
        selected_objects TYPE siw_tab_tadir.

    selected_objects = selected_objects = select_by_package(
        include_subpackages = include_subpackages
        packages = packages
      ).
    add_source_objects( selected_objects ).
  ENDMETHOD.                    "EXPORT_PACKAGE


  METHOD add_ci_variant.
    DATA ci_variant TYPE t_ci_variant.

    ci_variant-variant_user = variant_user.
    ci_variant-variant = variant.

    cl_ci_checkvariant=>get_ref(
      EXPORTING
        p_user                   =     variant_user
        p_name                   =     variant
      EXCEPTIONS
        chkv_not_exists          = 1
        missing_parameter        = 2
        OTHERS                   = 3
    ).

    IF sy-subrc <> 0.
      logger->warn(
        msg1 = 'Unable to add Code Inspector variant'(006)
        msg2 = ci_variant
        msg3 = '. Variant not executed.'(007)
        show_message = abap_true
       ).
      RETURN.
    ENDIF.

    APPEND ci_variant TO ci_variants.

  ENDMETHOD.                    "ADD_CI_VARIANT


  METHOD add_source_objects.
    DATA:
        tadir_entry TYPE tadir,
        source_object TYPE REF TO zif_cqse_source_object,
        source_objects TYPE zif_cqse_datatypes=>t_source_object_tab,
        meta_data TYPE zif_cqse_datatypes=>t_source_metadata,
        file_name TYPE string,
        ci_object TYPE scir_objs,
        line_count TYPE i,
        export_count TYPE i VALUE 0.

    DESCRIBE TABLE selected_objects LINES line_count.
    logger->info(
        msg1 = line_count
        msg2 = 'TADIR objects selected.'
        show_message = abap_true
    ).

    LOOP AT selected_objects INTO tadir_entry.

      IF tadir_entry-delflag EQ 'X'.
        CONTINUE.
      ENDIF.

      CLEAR source_objects.
      source_objects = create_source_objects(
            tadir_entry =  tadir_entry
        ).

      LOOP AT source_objects INTO source_object.
        CLEAR file_name.
        file_name = zip_file->add_source_object( source_object ).

        export_count = export_count + 1.

        CLEAR meta_data.
        meta_data = source_object->get_meta_data( ).
        IF meta_data IS NOT INITIAL.
          meta_data-objname = source_object->get_name( ).
          meta_data-path = file_name.
          APPEND meta_data TO meta_data_table.
        ENDIF.

      ENDLOOP.

      IF source_objects IS NOT INITIAL.
        ci_object-objtype = tadir_entry-object.
        ci_object-devclass = tadir_entry-devclass.
        ci_object-objname = tadir_entry-obj_name.
        APPEND ci_object TO ci_objects.
      ENDIF.

    ENDLOOP.

    logger->info(
        msg1 = export_count
        msg2 = 'source files added to zip.'
        show_message = abap_true
    ).
  ENDMETHOD.                    "export_tadir_objects


  METHOD build_file_name.

    IF is_toplevel_dir_by_progtype = abap_true.
      CONCATENATE
        'sources/'
        source_object_type
        '/'
        package
        '/'
        base_file_name
        '.abap'
      INTO file_name.
    ELSE.
      CONCATENATE
        'sources/'
        package
        '/'
        source_object_type
        '/'
        base_file_name
        '.abap'
       INTO file_name.
    ENDIF.

  ENDMETHOD.                    "BUILD_FILE_NAME


  METHOD constructor.

    DATA:
        timestamp TYPE tzonref-tstamps,
        date_string(19) TYPE c.

    CREATE OBJECT zip_file
      EXPORTING
        file_name = file_name.

    GET TIME STAMP FIELD timestamp.
    WRITE timestamp TO date_string TIME ZONE sy-zonlo MM/DD/YYYY.

    logger->info(
      EXPORTING
        msg1         = 'Export date: '(008)
        msg2         = date_string
    ).

    logger->info(
      EXPORTING
        msg1         = 'Export ABAP source code to '(009)
        msg2         = file_name
        show_message = abap_true
    ).

  ENDMETHOD.                    "constructor


  METHOD create_source_objects.
    DATA:
        source_object TYPE REF TO zif_cqse_source_object,
        comp_source_object TYPE REF TO zcl_cqse_composite_source_obj,
        class_source_object TYPE REF TO zcl_cqse_class_srcobj,
        class_method_includes TYPE zif_cqse_datatypes=>t_method_include_tab,
        fugr_source_object TYPE REF TO zcl_cqse_fugr_srcobj,
        enh_file_name TYPE string,
        atomic_source_objects TYPE zif_cqse_datatypes=>t_source_object_tab.

    CLEAR source_object.
    TRY.
        CASE tadir_entry-object.
          WHEN 'PROG'.
            CREATE OBJECT source_object TYPE zcl_cqse_atomic_source_object
              EXPORTING
                exporter    = me
                tadir_entry = tadir_entry.
            APPEND source_object TO source_objects.
          WHEN 'CLAS' OR 'INTF'.
            TRY.
                CONCATENATE tadir_entry-obj_name '.enh' INTO enh_file_name.
                CREATE OBJECT comp_source_object TYPE zcl_cqse_classenh_srcobj
                  EXPORTING
                    exporter         = me
                    tadir_entry      = tadir_entry
                    file_object_name = enh_file_name.
*                APPEND comp_source_object TO source_objects.
                CLEAR atomic_source_objects.
                atomic_source_objects = comp_source_object->get_atomic_source_objects(
                    include_empty = abap_false
                ).
                APPEND LINES OF atomic_source_objects TO source_objects.

              CATCH zcx_cqse_srcobj_not_exists.
                " ignore, classenh source object are optional for classes
            ENDTRY.

            CLEAR source_object.
            CLEAR class_source_object.
            CREATE OBJECT class_source_object
              EXPORTING
                exporter    = me
                tadir_entry = tadir_entry.
            APPEND class_source_object TO source_objects.

            class_method_includes = class_source_object->method_includes.
            APPEND LINES OF class_method_includes TO method_includes.

          WHEN 'FUGR'.
            CREATE OBJECT fugr_source_object
              EXPORTING
                exporter    = me
                tadir_entry = tadir_entry.

            CLEAR atomic_source_objects.
            atomic_source_objects = fugr_source_object->get_atomic_source_objects( ).
            APPEND LINES OF atomic_source_objects TO source_objects.

          WHEN OTHERS.
            " do nothing
        ENDCASE.
      CATCH zcx_cqse_srcobj_not_exists.
        logger->warn(
            msg1  = 'Source object '(014)
            msg2  = tadir_entry-obj_name
            msg3  = ' does not exist.'(015)
        ).
    ENDTRY.

  ENDMETHOD.                    "CREATE_SOURCE_OBJECTS


  METHOD get_package_subtree.

    DATA child_name TYPE devclass.


    APPEND root_node TO ancestors->*.

    SELECT devclass FROM tdevc INTO child_name
      WHERE parentcl EQ  root_node.
      get_package_subtree(
        root_node = child_name
        ancestors = ancestors ).
    ENDSELECT.

  ENDMETHOD.                    "get_package_subtree


  METHOD is_empty_source.
    DATA line TYPE string.
    DATA line_condensed TYPE string.
    DATA length TYPE i.

    is_empty = 'X'.

    LOOP AT source INTO line.
      line_condensed = line.
      CONDENSE line_condensed NO-GAPS.
      length = strlen( line_condensed ).
      IF length  > 0 AND line(1) <> '*'.
        is_empty = ' '.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "is_empty_source


  METHOD run_code_inspector.

    DATA:
           variant TYPE t_ci_variant,
           result_table TYPE scit_rest,
           result_line TYPE scir_rest,
           result_line_with_text TYPE t_ci_result,
           message TYPE scimessage,
           n TYPE i.


    LOOP AT ci_variants INTO variant.

      CALL METHOD run_code_inspecotr_single
        EXPORTING
          variant      = variant
        IMPORTING
          result_table = result_table.

      DESCRIBE TABLE result_table LINES n.

      logger->info(
        msg1 = n
        msg2 = ' issues for Code Inspector variant '(022)
        msg3 = variant
        show_message = abap_true
      ).

      LOOP AT result_table INTO result_line.
        MOVE-CORRESPONDING result_line TO result_line_with_text.

        CLEAR message.
        READ TABLE ci_messages_tab
          WITH KEY
            test = result_line-test
            code = result_line-code
          INTO message.

        result_line_with_text-text = message-text.

        APPEND result_line_with_text TO ci_results.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.                    "run_code_inspector


  METHOD save.

    DATA:
        ci_results TYPE t_ci_result_tab,
        log_messages TYPE string_table,
        timestamp TYPE tzonref-tstamps,
        date_string(19) TYPE c,
        e TYPE REF TO cx_root,
        e_text TYPE string.



    zip_file->add_table(
      table = meta_data_table
      file_name = 'object_meta_data'
    ).

    GET TIME STAMP FIELD timestamp.
    WRITE timestamp TO date_string TIME ZONE sy-zonlo MM/DD/YYYY.
    logger->info(
        msg1 = 'Code Inspector started:'(010)
        msg2 = date_string
        show_message = abap_true
    ).

    TRY.

        ci_results =  run_code_inspector( ).




        zip_file->add_table(
          table = ci_results
          file_name = 'ci_results'
        ).

        GET TIME STAMP FIELD timestamp.
        WRITE timestamp TO date_string TIME ZONE sy-zonlo MM/DD/YYYY.
        logger->info(
            msg1 = 'Code Inspector finished:'(011)
            msg2 = date_string
            show_message = abap_false
        ).

      CATCH cx_root INTO e.
        e_text = e->get_text( ).
        logger->warn(
            msg1 = 'Excpetion occured during code inspector run: '(012)
            msg2 = e_text
            show_message = abap_true
        ).
    ENDTRY.

    zip_file->add_table(
      table = method_includes
      file_name = 'method_includes'
    ).


    GET TIME STAMP FIELD timestamp.
    WRITE timestamp TO date_string TIME ZONE sy-zonlo MM/DD/YYYY.
    logger->info(
        msg1 = 'Export finished:'(013)
        msg2 = date_string
    ).


    log_messages = logger->get_log_table( ).
    zip_file->add_string_table(
     table = log_messages
     file_name = 'export_log.txt'
    ).


    zip_content = zip_file->save(
      save_to_server = save_to_server
    ).

    logger->save( ).
  ENDMETHOD.                    "SAVE


  METHOD select_by_name_pattern.
    SELECT * FROM tadir INTO TABLE selected_objects
    WHERE object IN ('PROG', 'CLAS', 'INTF', 'FUGR') AND
      obj_name LIKE pattern.
  ENDMETHOD.                    "select_objects_by_name_pattern


  METHOD select_by_package.
    DATA:
     selected_packages TYPE REF TO trdevclasses1,
     package TYPE devclass.

    IF include_subpackages = abap_true.
      CREATE DATA selected_packages TYPE trdevclasses1.
      LOOP AT packages INTO package.
        get_package_subtree(
            root_node = package
            ancestors = selected_packages ).
      ENDLOOP.
    ELSE.
      GET REFERENCE OF packages INTO selected_packages.
    ENDIF.

    SELECT * FROM tadir INTO TABLE selected_objects
      FOR ALL ENTRIES IN selected_packages->*
      WHERE object IN ('PROG', 'CLAS', 'INTF', 'FUGR') AND
        devclass = selected_packages->*-devclass.

  ENDMETHOD.                    "select_by_package


  METHOD select_by_package_pattern.
    DATA packages TYPE trdevclasses1.

    SELECT devclass FROM tdevc INTO TABLE packages
    WHERE devclass LIKE pattern.

    selected_objects = select_by_package(
      packages = packages
      include_subpackages = include_subpackages
    ).

  ENDMETHOD.                    "select_by_package_pattern


  METHOD tidy_file_name.

    " when change this method also change java Method AbapUtils.revertTidyFileName(..)
    " accordingly

    tidy_file_name = file_name.

    REPLACE ALL OCCURRENCES OF '!' IN tidy_file_name WITH '#excl#'.
    REPLACE ALL OCCURRENCES OF '/' IN tidy_file_name WITH '!'.
    REPLACE ALL OCCURRENCES OF '\' IN tidy_file_name WITH '#rsol#'.
    REPLACE ALL OCCURRENCES OF '<' IN tidy_file_name WITH '#lt#'.
    REPLACE ALL OCCURRENCES OF '>' IN tidy_file_name WITH '#gt#'.
    REPLACE ALL OCCURRENCES OF ':' IN tidy_file_name WITH '#colon#'.
    REPLACE ALL OCCURRENCES OF '|' IN tidy_file_name WITH '#verbar#'.
    REPLACE ALL OCCURRENCES OF '?' IN tidy_file_name WITH '#quest#'.
    REPLACE ALL OCCURRENCES OF '*' IN tidy_file_name WITH '#ast#'.
    REPLACE ALL OCCURRENCES OF '"' IN tidy_file_name WITH '#quot#'.

  ENDMETHOD.                    "TIDY_FILE_NAME


  METHOD class_constructor.

    DATA:
      timestamp TYPE tzonref-tstamps,
      date_string(19) TYPE c.

    " US time formatting for log
    SET COUNTRY 'US'.

    CREATE OBJECT logger
      EXPORTING
        external_id = 'CQSE ABAP Exporter'
        def_msgid   = 'ZCQSE'
        def_msgno   = 0.

    GET TIME STAMP FIELD timestamp.
    WRITE timestamp TO date_string TIME ZONE sy-zonlo MM/DD/YYYY.
    logger->info(
        msg1 = 'Export started:'(002)
        msg2 = date_string
    ).

    init_code_inspector_messages( ).

  ENDMETHOD.                    "CLASS_CONSTRUCTOR


  METHOD init_code_inspector_messages.
    DATA:
        lo_root_test_class TYPE REF TO cl_oo_class,
        lt_all_test_classes TYPE seo_relkeys,
        ls_class_key TYPE seorelkey,
        lo_test_class TYPE REF TO cl_oo_class,
        lo_test_instance TYPE REF TO cl_ci_test_root.


    TRY.
        lo_root_test_class ?= cl_oo_class=>get_instance( 'CL_CI_TEST_ROOT' ).
      CATCH cx_class_not_existent.
        logger->warn('Can not extract Code Inspector messages, test root class not found.'(016) ).
        RETURN.
    ENDTRY.

    lt_all_test_classes = lo_root_test_class->get_subclasses( ).

    LOOP AT lt_all_test_classes INTO ls_class_key.
      TRY.
          lo_test_class ?= cl_oo_class=>get_instance( ls_class_key-clsname ).
          IF lo_test_class->is_abstract( ) = abap_false.
            CREATE OBJECT lo_test_instance TYPE (ls_class_key-clsname).
            APPEND LINES OF lo_test_instance->scimessages TO ci_messages_tab.
          ENDIF.
        CATCH cx_root.
          logger->warn(
             msg1 = 'Unable to extract messages for Code Inspector test '(017)
             msg2 = ls_class_key-clsname
           ).
      ENDTRY.

    ENDLOOP.


  ENDMETHOD.                    "export_code_inspector_messages


  METHOD add_table.
    me->zip_file->add_table(
      EXPORTING
        table     = table
        file_name =  file_name
*      separator = ';'
    ).
  ENDMETHOD.                    "add_table


  METHOD run_code_inspecotr_object_list.
    DATA:
     ok TYPE sychar01,
     result_obj TYPE REF TO cl_ci_check_result,
     inspection TYPE REF TO cl_ci_inspection,
     e TYPE REF TO cx_root,
     e_text TYPE string.


    TRY.
        CALL METHOD cl_ci_check=>object_list
          EXPORTING
            p_variant_user          = variant-variant_user
            p_variant               = variant-variant
            p_objects               = ci_objects
*           p_insp_user             =     " Code Inspector: Person Responsible
*           p_insp_name             =     " Code Inspector: Element Name of an Inspection
*           p_objs_user             =     " Code Inspector: Person Responsible
*           p_objs_name             =     " Object Name in Object Directory
*           p_text                  =     " Code Inspector: Element Text (Chk, ChkV, ObjS, INSP)
*           p_execute_parallel      = abap_true
*           p_ignore_pseudocomments =     " Ignore Pseudo-Comments
          IMPORTING
            p_ok                    = ok
            p_result                = result_obj.
      CATCH cx_ci_invalid_variant.
        logger->error( msg1 = 'Code Inspector: Invalid Variant '(018) msg2 = variant-variant ) .
        RETURN.
      CATCH cx_ci_invalid_object.
        logger->error( msg1 = 'Code Inspector: Invalid Object '(019) msg2 = variant-variant ) .
        RETURN.
      CATCH cx_ci_check_error.
        logger->error( msg1 = 'Code Inspector: Check Error '(020) msg2 = variant-variant ) .
        RETURN.
      CATCH cx_root INTO e.
        e_text = e->get_text( ).
        logger->error(
          msg1 = 'Code Inspector: Exception '(021)
          msg2 = e_text
          msg3 = variant-variant ) .
        RETURN.

    ENDTRY.

    IF result_obj IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR inspection.
    CLEAR result_table.
    inspection = result_obj->get_inspection( ).
    result_table = inspection->scirestps.

  ENDMETHOD.                    "run_code_inspecotr_object_list


  METHOD run_code_inspecotr_single.
    DATA:
     ci_object TYPE LINE OF scit_objs,
     ok TYPE sychar01,
     result_obj TYPE REF TO cl_ci_check_result,
     inspection TYPE REF TO cl_ci_inspection,
     e TYPE REF TO cx_root,
     e_text TYPE string.

    CLEAR result_table.

    LOOP AT ci_objects INTO ci_object.
      TRY.
          CALL METHOD cl_ci_check=>single
            EXPORTING
              p_variant_user = variant-variant_user
              p_variant      = variant-variant
              p_obj_type     = ci_object-objtype
              p_obj_name     = ci_object-objname
            IMPORTING
              p_ok           = ok
              p_result       = result_obj.
        CATCH cx_ci_invalid_variant.
          logger->error(
            msg1 = 'Code Inspector: Invalid Variant '(018)
            msg2 = ci_object-objname
            msg3 = variant-variant ) .
          CONTINUE.
        CATCH cx_ci_invalid_object.
          logger->error(
            msg1 = 'Code Inspector: Invalid Object '(019)
            msg2 = ci_object-objname
            msg3 = variant-variant ) .
          CONTINUE.
        CATCH cx_ci_check_error.
          logger->error(
            msg1 = 'Code Inspector: Check Error '(020)
            msg2 = ci_object-objname
            msg3 = variant-variant ) .
          CONTINUE.
        CATCH cx_root INTO e.
          e_text = e->get_text( ).
          logger->error(
            msg1 = 'Code Inspector: Exception '(021)
            msg2 = e_text
            msg3 = ci_object-objname
            msg4 = variant-variant ) .
          CONTINUE.

      ENDTRY.

      IF result_obj IS INITIAL.
        CONTINUE.
      ENDIF.

      CLEAR inspection.
      inspection = result_obj->get_inspection( ).
      APPEND LINES OF inspection->scirestps TO result_table.
    ENDLOOP.

  ENDMETHOD.                    "run_code_inspecotr_object_list
endclass. "ZCL_CQSE_ABAP_EXPORTER implementation