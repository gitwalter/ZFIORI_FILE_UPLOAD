CLASS zcf_file_import_handler DEFINITION
  PUBLIC
  FINAL
  ABSTRACT.

  PUBLIC SECTION.
    CLASS-METHODS get_instance IMPORTING iv_file_upload_process        TYPE zfile_upload_process
                                         iv_file_name                  TYPE string
                               RETURNING VALUE(ro_file_import_handler) TYPE REF TO zif_file_import_handler
                               RAISING   zcx_file_import_handler.
    CONSTANTS: BEGIN OF sc_file_upload_process,
                 auto_coding TYPE zfile_upload_process VALUE 'AUTO_CODE',
                 on_debiting TYPE zfile_upload_process VALUE 'ON_DEBIT',
               END OF sc_file_upload_process.

ENDCLASS.



CLASS zcf_file_import_handler IMPLEMENTATION.
  METHOD get_instance.
    CASE iv_file_upload_process.
      WHEN sc_file_upload_process-auto_coding.
        ro_file_import_handler = NEW zcl_file_import_auto_code( ).
      WHEN sc_file_upload_process-on_debiting.
        TRY.
            ro_file_import_handler ?= zcf_on_debiting_import_handler=>get_instance_by_filename( iv_file_name ).
          CATCH zcx_on_debiting_import INTO DATA(lx_on_debiting_import).

            DATA(lt_bapiret2) = VALUE bapiret2_t( ( lx_on_debiting_import->convert_message_to_bapiret2( ) ) ).

            RAISE EXCEPTION TYPE zcx_file_import_handler
              EXPORTING
                mt_return = lt_bapiret2.

        ENDTRY.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_file_import_handler.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
