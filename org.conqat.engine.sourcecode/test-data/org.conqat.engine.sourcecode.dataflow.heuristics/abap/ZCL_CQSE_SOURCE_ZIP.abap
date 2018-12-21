*----------------------------------------------------------------------*
*       CLASS ZCL_CQSE_SOURCE_ZIP DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_cqse_source_zip DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        value(file_name) TYPE string OPTIONAL .
    METHODS add_source_object
      IMPORTING
        !source_object TYPE REF TO zif_cqse_source_object
      RETURNING
        value(file_name) TYPE string .
    METHODS add_table
      IMPORTING
        !table TYPE STANDARD TABLE
        !file_name TYPE string
        !separator TYPE c DEFAULT ';' .
    TYPE-POOLS abap .
    METHODS save
      IMPORTING
        !save_to_server TYPE abap_bool DEFAULT abap_false
      RETURNING
        value(zip_content) TYPE xstring .
    METHODS add_string_table
      IMPORTING
        !table TYPE string_table
        !file_name TYPE string .
    CLASS-METHODS convert_table_to_csv
      IMPORTING
        !table TYPE ANY TABLE
        !separator TYPE c DEFAULT ';'
      RETURNING
        value(csv_table) TYPE string_table .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA zip_file TYPE REF TO cl_abap_zip .
    DATA file_name TYPE string .

    CLASS-METHODS get_leave_components
      IMPORTING
        !datadesc TYPE REF TO cl_abap_datadescr
      CHANGING
        !components TYPE abap_component_tab .
    CLASS-METHODS get_table_header
      IMPORTING
        !struct TYPE any
        !separator TYPE c DEFAULT ';'
      RETURNING
        value(field_names) TYPE string .
    METHODS add_table_to_csv_xml
      IMPORTING
        !table TYPE ANY TABLE
        !file_name TYPE string
        !separator TYPE c DEFAULT ';' .

endclass. "ZCL_CQSE_SOURCE_ZIP definition



*----------------------------------------------------------------------*
*       class ZCL_CQSE_SOURCE_ZIP implementation.
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CQSE_SOURCE_ZIP implementation.


  METHOD add_source_object.

    DATA:
          source_table TYPE string_table,
          line_count TYPE i.

    source_table = source_object->get_source( ).

    DESCRIBE TABLE source_table LINES line_count.

    IF line_count = 0.
      RETURN.
    ENDIF.

    file_name = source_object->get_source_file_name( ).
    zcl_cqse_abap_exporter=>logger->info(
      msg1 = file_name
      msg2 = 'added to zip.'
    ).

    me->add_string_table(
      table = source_table
      file_name = file_name
    ).

  ENDMETHOD.                    "add_source_object


  METHOD add_string_table.

    CONSTANTS:
       encoding TYPE abap_encoding VALUE '1100'.


    DATA:
          text TYPE string,
          xsource TYPE xstring.

    CONCATENATE LINES OF table INTO text SEPARATED BY cl_abap_char_utilities=>cr_lf.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text     = text
        encoding = encoding
      IMPORTING
        buffer   = xsource.

    me->zip_file->add(
      name = file_name
      content = xsource
    ).


  ENDMETHOD.                    "add_string_table


  METHOD add_table.

    CONSTANTS:
     max_size TYPE i VALUE 100000.

    DATA:
         size TYPE i,
         rest TYPE i,
         full_parts TYPE i,
         start TYPE i,
         end TYPE i,
         file_no(3) TYPE n,
         part_file_name TYPE string,
         dref TYPE REF TO data.

    FIELD-SYMBOLS:
        <tmp_table> LIKE table.

    CREATE DATA dref LIKE table.
    ASSIGN dref->* TO <tmp_table>.

    DESCRIBE TABLE table LINES size.

    full_parts = size DIV max_size.

    " ensure that there is content for the last part
    " even if size is a multiple of max_size
    rest = size MOD max_size.
    IF rest EQ 0 AND full_parts > 0.
      full_parts = full_parts - 1.
    ENDIF.


    DO full_parts TIMES.
      CLEAR <tmp_table>.
      start = ( sy-index - 1 ) * max_size + 1.
      end = sy-index  * max_size.
      file_no = sy-index.
      CONCATENATE file_name '_' file_no INTO part_file_name.

      INSERT LINES OF table FROM start TO end INTO TABLE <tmp_table>.

      add_table_to_csv_xml(
         EXPORTING
           table     = <tmp_table>
           file_name = part_file_name
           separator = separator
       ).
    ENDDO.

    CLEAR <tmp_table>.
    start = full_parts * max_size + 1.

    INSERT LINES OF table FROM start INTO TABLE <tmp_table>.

    add_table_to_csv_xml(
       EXPORTING
         table     = <tmp_table>
         file_name = file_name
         separator = separator
     ).
  ENDMETHOD.                    "add_table


  METHOD constructor.
    me->file_name = file_name.
    CREATE OBJECT me->zip_file.
  ENDMETHOD.                    "constructor


  METHOD convert_table_to_csv.

    DATA:
          csv_line TYPE string,
          comp_string TYPE string,
          descr TYPE REF TO cl_abap_structdescr,
          columns TYPE TABLE OF abap_componentdescr,
          column TYPE abap_componentdescr,
          comp_type(1) TYPE c.

    FIELD-SYMBOLS:
                   <table_line> TYPE any,
                   <comp> TYPE any.

    LOOP AT table ASSIGNING <table_line>.
      IF sy-tabix = 1.
        csv_line = get_table_header(
          struct = <table_line>
          separator = separator
          ).
        APPEND csv_line TO csv_table.
      ENDIF.

      CLEAR csv_line.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <table_line> TO <comp>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        CLEAR comp_string.
        comp_string = <comp>.

*       if type is not numeric, quote value string
        DESCRIBE FIELD <comp> TYPE comp_type.
        IF comp_type <> 'b'
           AND comp_type <> 's'
           AND comp_type <> 'I'
           AND comp_type <> 'P'
           AND comp_type <> 'a'
           AND comp_type <> 'e'
           AND comp_type <> 'F'.
          CONCATENATE '"' comp_string '"' INTO comp_string.
        ENDIF.

        IF sy-index = 1.
          csv_line = comp_string.
        ELSE.
          CONCATENATE csv_line comp_string INTO csv_line SEPARATED BY separator.
        ENDIF.
      ENDDO.
      APPEND csv_line TO csv_table.
    ENDLOOP.

  ENDMETHOD.                    "convert_table_to_csv


  METHOD get_leave_components.
    DATA:
          structdesc TYPE REF TO cl_abap_structdescr,
          ctab TYPE abap_component_tab,
          c TYPE abap_componentdescr.

    TRY.
        structdesc ?= datadesc.
        ctab = structdesc->get_components( ).

        LOOP AT ctab INTO c.
          IF c-name IS INITIAL.
            CALL METHOD get_leave_components
              EXPORTING
                datadesc   = c-type
              CHANGING
                components = components.
          ELSE.
            APPEND c TO components.
          ENDIF.
        ENDLOOP.
      CATCH cx_sy_move_cast_error.
        zcl_cqse_abap_exporter=>logger->warn( 'Unable to determine table header for CSV file.' ).
    ENDTRY.
  ENDMETHOD.                    "GET_COMPONENT_NAMES


  METHOD get_table_header.
    DATA:
          descr TYPE REF TO cl_abap_structdescr,
          columns TYPE abap_component_tab,
          column TYPE abap_componentdescr.
    .

    descr ?= cl_abap_typedescr=>describe_by_data( struct ).
    CALL METHOD get_leave_components
      EXPORTING
        datadesc   = descr
      CHANGING
        components = columns.
    LOOP AT columns INTO column.
      IF sy-tabix = 1.
        field_names = column-name.
      ELSE.
        CONCATENATE field_names column-name INTO field_names SEPARATED BY separator.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "get_table_header


  METHOD save.

    DATA lv_zip_size TYPE i.
    DATA lt_zip_bin_data TYPE STANDARD TABLE OF raw255.
    DATA zip_bin_line TYPE raw255.
    DATA exception TYPE REF TO cx_root.

    zip_content = zip_file->save( ).

    IF me->file_name IS INITIAL.
      RETURN.
    ENDIF.

*   Convert the XSTRING to Binary table
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = zip_content
      IMPORTING
        output_length = lv_zip_size
      TABLES
        binary_tab    = lt_zip_bin_data.

    IF sy-batch = abap_true OR save_to_server = abap_true.
*      download the zip on server (in batch mode always required)

      OPEN DATASET me->file_name FOR OUTPUT IN BINARY MODE.
      IF sy-subrc <> 0.
        zcl_cqse_abap_exporter=>logger->error( 'ERROR: Unable to open ZIP file for output.' ).
      ENDIF.

      LOOP AT lt_zip_bin_data INTO zip_bin_line.
        TRANSFER zip_bin_line TO me->file_name.
      ENDLOOP.

      CLOSE DATASET me->file_name.
      IF sy-subrc <> 0.
        zcl_cqse_abap_exporter=>logger->error(  'ERROR: Unable to close ZIP file.' ).
      ENDIF.

    ELSE.
*     download the zip file locally

      cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize              = lv_zip_size
          filename                  = me->file_name
          filetype                  = 'BIN'
         CHANGING
          data_tab                  = lt_zip_bin_data
        EXCEPTIONS
           OTHERS                    = 4
             ).
      IF sy-subrc <> 0.
        zcl_cqse_abap_exporter=>logger->error(  'ERROR: Unable to write ZIP file.' ).
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "save


  METHOD add_table_to_csv_xml.
    DATA:
          csv_file TYPE string,
          csv_table TYPE string_table,
          xml_file TYPE string,
          xml_xstring TYPE xstring.

    "export as CSV
    CONCATENATE file_name '.csv' INTO csv_file.
    csv_table = convert_table_to_csv( table ).

    me->add_string_table(
      file_name = csv_file
      table = csv_table
    ).

    " export as XML
    CONCATENATE file_name '.xml' INTO xml_file.
    CALL TRANSFORMATION ('ID') SOURCE tab = table RESULT XML xml_xstring.

    me->zip_file->add(
      EXPORTING
        name    =     xml_file
        content = xml_xstring
    ).

  ENDMETHOD.                    "add_table
endclass. "ZCL_CQSE_SOURCE_ZIP implementation