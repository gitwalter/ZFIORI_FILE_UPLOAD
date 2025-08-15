CLASS lhc_zi_file_buffer DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    CONSTANTS sc_text_csv TYPE string VALUE 'text/csv' ##NO_TEXT.
    CONSTANTS sc_xlsx TYPE string VALUE 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' ##NO_TEXT.
    CONSTANTS sc_utf_8 TYPE abap_encod VALUE 'UTF-8' ##NO_TEXT.
    CONSTANTS sc_endian_l TYPE abap_endia VALUE 'L' ##NO_TEXT.
    CONSTANTS sc_replacement TYPE abap_repl VALUE '#' ##NO_TEXT.
    TYPES: ty_t_line TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    TYPES: ty_s_response_file_buffer  TYPE RESPONSE FOR REPORTED LATE zi_file_buffer.
    TYPES: ty_t_file_line_read_result TYPE TABLE FOR READ RESULT zi_file_buffer\_fileline.

    "! Reads the file lines of a file buffer
    "! @parameter iv_uuid | UUID of the file buffer
    "! @parameter rt_file_line_read_result | Table of file lines read from the file buffer
    METHODS read_file_lines
      IMPORTING
        iv_uuid                         TYPE zi_file_buffer-uuid
      RETURNING
        VALUE(rt_file_line_read_result) TYPE ty_t_file_line_read_result.

    "! Field property handling
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zi_file_buffer RESULT result.

    "! Authorization handling not implemented
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_file_buffer RESULT result.

    "! Processes the file content of a file buffer by reading the file lines,
    "! parsing the content based on the MIME type and calling a file import handler
    "! depending on the file upload process and the file name.
    METHODS process_file_content FOR DETERMINE ON SAVE
      IMPORTING keys FOR zi_file_buffer~process_file_content.

    "! Gets the file lines as string table based on the content and MIME type.
    "! @parameter iv_content | Content of the file buffer in xstring format
    "! @parameter iv_mimetype | MIME type of the file buffer
    "! @parameter rt_lines | Table of lines read from the file buffer
    "! @raising zcx_excel | Exception raised for errors in Excel processing
    METHODS get_file_lines
      IMPORTING iv_content      TYPE xstring
                iv_mimetype     TYPE string
      RETURNING VALUE(rt_lines) TYPE ty_t_line
      RAISING   zcx_excel.

    "! Gets the lines from an Excel table as string table.
    "! @parameter ir_excel_table | Reference to the Excel table object
    "! @parameter rt_lines | Table of lines read from the Excel table
    METHODS get_xlsx_tab_table
      IMPORTING
        ir_excel_table  TYPE REF TO data
      RETURNING
        VALUE(rt_lines) TYPE ty_t_line.


    "! Executes the file import process for a file buffer.
    "! @parameter iv_uuid | UUID of the file buffer
    "! @parameter iv_process | File upload process type
    "! @parameter rs_response_file_buffer | Response structure containing the reported messages
    METHODS execute_file_import IMPORTING iv_uuid                        TYPE zi_file_buffer-uuid
                                          iv_process                     TYPE zfile_upload_process
                                RETURNING VALUE(rs_response_file_buffer) TYPE ty_s_response_file_buffer.

    "! Gets the file lines from a CSV file content.
    "! @parameter iv_content | Content of the CSV file in xstring format
    "! @parameter rt_lines | Table of lines read from the CSV file
    METHODS get_csv_lines
      IMPORTING
        iv_content      TYPE xstring
      RETURNING
        VALUE(rt_lines) TYPE ty_t_line.


    "! Gets the file lines from an XLSX file content.
    "! @parameter iv_content | Content of the XLSX file in xstring format
    "! @parameter rt_lines | Table of lines read from the XLSX file
    "! @raising zcx_excel | Exception raised for errors in Excel processing
    METHODS get_xlsx_lines
      IMPORTING iv_content      TYPE xstring
      RETURNING VALUE(rt_lines) TYPE ty_t_line
      RAISING   zcx_excel.

    "! Fills the response structure with reported messages based on BAPIRET2 table.
    "! @parameter it_bapiret2 | Table of BAPIRET2 messages
    "! @parameter rs_response_file_buffer | Response structure containing the reported messages
    METHODS fill_reported_from_bapiret
      IMPORTING
        it_bapiret2                    TYPE bapiret2_t
      RETURNING
        VALUE(rs_response_file_buffer) TYPE ty_s_response_file_buffer.

    "! Reads the file name from a file buffer based on its UUID.
    "! @parameter iv_uuid | UUID of the file buffer
    "! @parameter rv_file_name | File name read from the file buffer
    METHODS read_file_name
      IMPORTING
        iv_uuid             TYPE zi_file_buffer-uuid
      RETURNING
        VALUE(rv_file_name) TYPE string.

    "! Gets the data from an XLSX file content.
    "! @parameter iv_content | Content of the XLSX file in xstring format
    "! @parameter er_data | Reference to the data object containing the XLSX data
    "! @raising zcx_excel | Exception raised for errors in Excel processing
    METHODS get_xlsx_data
      IMPORTING
                iv_content     TYPE xstring
      RETURNING VALUE(er_data) TYPE REF TO data
      RAISING   zcx_excel.

ENDCLASS.

CLASS lhc_zi_file_buffer IMPLEMENTATION.

  METHOD get_instance_features.
    "Sets non initial fields to read only,
    "because it is not useful to change the file or the process
    "after the file has been uploaded and saved
    "or the process has been executed.

    DATA lv_initial_file_upload_process TYPE zfile_upload_process.
    DATA lv_initial_string TYPE string.

    READ ENTITY IN LOCAL MODE zi_file_buffer
       FIELDS ( fileuploadprocess filename mimetype )
       WITH CORRESPONDING #( keys )
       RESULT DATA(lt_file_upload_process).

    result = VALUE #( FOR ls_file_upload_process IN lt_file_upload_process
                        ( %tky = ls_file_upload_process-%tky
                          %features-%field-fileuploadprocess = COND #( WHEN ls_file_upload_process-fileuploadprocess = lv_initial_file_upload_process
                                                                THEN if_abap_behv=>fc-f-unrestricted
                                                                ELSE if_abap_behv=>fc-f-read_only )
                          %features-%field-filename = COND #( WHEN ls_file_upload_process-filename = lv_initial_string
                                                               THEN if_abap_behv=>fc-f-mandatory
                                                               ELSE if_abap_behv=>fc-f-read_only )
                          %features-%field-content = COND #( WHEN ls_file_upload_process-filename = lv_initial_string
                                                               THEN if_abap_behv=>fc-f-mandatory
                                                               ELSE if_abap_behv=>fc-f-read_only )
                          %features-%field-mimetype = COND #( WHEN ls_file_upload_process-mimetype = lv_initial_string
                                                               THEN if_abap_behv=>fc-f-mandatory
                                                               ELSE if_abap_behv=>fc-f-read_only ) ) ).
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD process_file_content.
    DATA lt_keys_read TYPE TABLE FOR READ IMPORT zi_file_buffer.
    DATA lt_file_line_create TYPE TABLE FOR CREATE zi_file_buffer\_fileline.

    lt_keys_read = CORRESPONDING #( keys ).

    "read root nodes
    READ ENTITY IN LOCAL MODE zi_file_buffer
      ALL FIELDS WITH lt_keys_read
      RESULT DATA(lt_file_buffer_read_result).



    LOOP AT lt_file_buffer_read_result ASSIGNING FIELD-SYMBOL(<ls_file_buffer_read_result>).

      "read child nodes
      READ ENTITY IN LOCAL MODE zi_file_buffer
        BY \_fileline
        FIELDS ( linecontent )
        WITH VALUE #( ( uuid = <ls_file_buffer_read_result>-uuid ) )
        RESULT DATA(lt_file_line_read_result).

      IF lt_file_line_read_result IS INITIAL.
        TRY.
            "parse file lines depending on mime type
            DATA(lt_lines) = get_file_lines(
                               iv_content  = <ls_file_buffer_read_result>-content
                               iv_mimetype = <ls_file_buffer_read_result>-mimetype ).

            "build table for child node creation
            lt_file_line_create = VALUE #( FOR lv_line IN lt_lines INDEX INTO lv_line_number (
                                        uuid = <ls_file_buffer_read_result>-uuid
                                        %target =  VALUE #( ( linenumber = lv_line_number
                                                              linecontent = lv_line
                                                              %control-linenumber = if_abap_behv=>mk-on
                                                              %control-linecontent = if_abap_behv=>mk-on ) ) ) ).
          CATCH zcx_excel INTO DATA(lx_excel).
            DATA(lt_bapiret2) = VALUE bapiret2_t( ( message = lx_excel->get_text( )
                                                    id = '00'
                                                    number = '398'
                                                    type = 'E'
                                                    message_v1 = lx_excel->get_text( ) ) ).
            reported = fill_reported_from_bapiret( lt_bapiret2 ).
            RETURN.
        ENDTRY.
      ENDIF.

      "create file lines
      IF lines( lt_file_line_create ) > 0.
        MODIFY ENTITY IN LOCAL MODE zi_file_buffer
           CREATE BY \_fileline
           AUTO FILL CID WITH lt_file_line_create.
      ENDIF.

      "call file import handler
      IF <ls_file_buffer_read_result>-fileuploadprocess IS NOT INITIAL.
        reported = execute_file_import( iv_uuid    = <ls_file_buffer_read_result>-uuid
                                        iv_process = <ls_file_buffer_read_result>-fileuploadprocess ).
      ENDIF.

    ENDLOOP.


  ENDMETHOD.

  METHOD get_file_lines.

    CASE iv_mimetype.

      WHEN sc_text_csv.
        rt_lines = get_csv_lines( iv_content ).

      WHEN sc_xlsx.
        rt_lines = get_xlsx_lines( iv_content ).

    ENDCASE.
  ENDMETHOD.

  METHOD execute_file_import.
    DATA lt_bapiret2 TYPE bapiret2_t.

    DATA(lv_file_name) = read_file_name( iv_uuid ).

    DATA(lt_file_line_read_result) = read_file_lines( iv_uuid ).

    "set file utility fiori
    "for upload of file content inside the import handler
    "and interpretation of filenames inside the factory method
    zcl_ca_file_utility_fiori=>get_file_utility_fiori( ).

    TRY.
        DATA(lo_file_import_handler) = zcf_file_import_handler=>get_instance( iv_file_upload_process = iv_process
                                                                              iv_file_name = lv_file_name ).

        lt_bapiret2 = lo_file_import_handler->run( lt_file_line_read_result ).

      CATCH zcx_file_import_handler INTO DATA(lx_file_import_handler).
        lt_bapiret2 = lx_file_import_handler->mt_return.
    ENDTRY.

    rs_response_file_buffer = fill_reported_from_bapiret( lt_bapiret2 ).
  ENDMETHOD.


  METHOD get_xlsx_tab_table.
    DATA lv_line TYPE string.
    ASSIGN ir_excel_table->* TO FIELD-SYMBOL(<lt_excel>).
    LOOP AT <lt_excel> ASSIGNING FIELD-SYMBOL(<ls_excel>).

      "process fields of excel row
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <ls_excel> TO FIELD-SYMBOL(<lv_cell_value>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        "build line from excel cells using horizontal tab as separator
        lv_line = COND string(
                    WHEN lv_line IS INITIAL
                    THEN CONV string( <lv_cell_value> )
                    ELSE lv_line && cl_abap_char_utilities=>horizontal_tab && CONV string( <lv_cell_value> ) ).

      ENDDO.

      rt_lines = VALUE #( BASE rt_lines ( lv_line ) ).
      CLEAR lv_line.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_csv_lines.
    DATA(lo_csv_converter) = cl_abap_conv_in_ce=>create(
                                     encoding     = sc_utf_8
                                     endian       = sc_endian_l
                                     ignore_cerr  = abap_true
                                     replacement  = sc_replacement
                                     input        = iv_content ).

    DATA lv_csv_string TYPE string.
    lo_csv_converter->read( IMPORTING data = lv_csv_string ).


    SPLIT lv_csv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_lines.

  ENDMETHOD.


  METHOD get_xlsx_lines.

    DATA(lr_data) = get_xlsx_data( iv_content ).

    rt_lines = get_xlsx_tab_table( lr_data ).

  ENDMETHOD.


  METHOD fill_reported_from_bapiret.

    DATA lv_severity TYPE if_abap_behv_message=>t_severity.

    IF line_exists( it_bapiret2[ type = 'E' ] ) OR
       line_exists( it_bapiret2[ type = 'A' ] ) OR
       line_exists( it_bapiret2[ type = 'X' ] ).
      lv_severity = if_abap_behv_message=>severity-error.
    ELSE.
      lv_severity = if_abap_behv_message=>severity-success.
    ENDIF.

    rs_response_file_buffer-zi_file_buffer = VALUE #( FOR ls_return IN it_bapiret2 (
                                   %msg = new_message( severity = lv_severity
                                              id       = ls_return-id
                                              number   = ls_return-number
                                              v1       = ls_return-message_v1
                                              v2       = ls_return-message_v2
                                              v3       = ls_return-message_v3
                                              v4       = ls_return-message_v4 ) ) ).
  ENDMETHOD.

  METHOD read_file_name.

    READ ENTITY IN LOCAL MODE zi_file_buffer
          FIELDS ( filename )
          WITH VALUE #( ( uuid = iv_uuid ) )
          RESULT DATA(lt_file_buffer_read_result).

    rv_file_name = lt_file_buffer_read_result[ 1 ]-filename.

  ENDMETHOD.

  METHOD read_file_lines.

    READ ENTITY IN LOCAL MODE zi_file_buffer
           BY \_fileline
           FIELDS ( linecontent )
           WITH VALUE #( ( uuid = iv_uuid ) )
           RESULT rt_file_line_read_result.

    SORT rt_file_line_read_result BY linenumber ASCENDING.

  ENDMETHOD.

  METHOD get_xlsx_data.

    DATA lo_reader TYPE REF TO zif_excel_reader.

    lo_reader = NEW zcl_excel_reader_2007( ).

    DATA(lo_excel) = lo_reader->load( i_excel2007 = iv_content ).
    DATA(lo_worksheet) = lo_excel->get_active_worksheet( ).

    lo_worksheet->convert_to_table(
      EXPORTING iv_begin_row = 1
      IMPORTING er_data =      er_data  ).

  ENDMETHOD.

ENDCLASS.
