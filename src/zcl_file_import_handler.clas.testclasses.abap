"! Tests for file import auto coding
CLASS lct_file_import_auto_coding DEFINITION
  INHERITING FROM zcl_abap_unit_test
  FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES: ty_t_file_line_read_result TYPE TABLE FOR READ RESULT zi_file_buffer\_fileline.

    CONSTANTS sc_file_name_auto_coding TYPE string VALUE 'auto_coding_2_2.csv'.

    CONSTANTS sc_determination_type TYPE zfi_vim_dedet_type VALUE 'RtQ'.

    "! Creates auto coding configuration RtQ
    "! with 2 determination fields and 2 target fields
    METHODS auto_coding_2_det_2_tar FOR TESTING.


    "! Asserts that the auto coding configuration for RtQ
    "! has been created successfully
    METHODS assert_rtq_config_created.

    "! Asserts that the auto coding configuration for RtQ
    "! does not exist
    METHODS assert_rtq_config_not_exists.

    "! Prepare database for test by deleting
    METHODS setup.

    "! Clean up database after test by deleting
    METHODS teardown.

    "! Deletes the auto coding configuration for RtQ
    METHODS delete_auto_coding_config.

ENDCLASS.

CLASS lct_file_import_auto_coding IMPLEMENTATION.


  METHOD auto_coding_2_det_2_tar.
    "given

    assert_rtq_config_not_exists( ).

    DATA lt_file_content TYPE ty_t_file_line_read_result.

    lt_file_content = VALUE #( ( linecontent = 'BUKRS_HD_DT;LIFNR_HD_DT;BLART_HD_TA;EXPENSE_TYPE_HD_TA;DET_TYPE_CU'
                                 linenumber = '0000000001' )
                               ( linecontent = '0002;4711;Z1;T1;RtQ'
                                 linenumber = '0000000002' ) ).
    "when
    TRY.
        DATA(lo_file_import_handler) = zcf_file_import_handler=>get_instance(
                                                   iv_file_name = sc_file_name_auto_coding
                                                   iv_file_upload_process = zcf_file_import_handler=>sc_file_upload_process-auto_coding ).

        DATA(lt_bapiret2) = lo_file_import_handler->run( lt_file_content ).

        COMMIT ENTITIES.

      CATCH zcx_file_import_handler INTO DATA(lx_file_import_handler).
        assert_not_bound( lx_file_import_handler ).
    ENDTRY.


    "then
    assert_not_bound( lx_file_import_handler ).

    assert_rtq_config_created( ).

  ENDMETHOD.


  METHOD assert_rtq_config_created.

    SELECT determinationuuid
          FROM zi_fi_vim_auto_coding
          WHERE dettypecu = @sc_determination_type
          INTO TABLE @DATA(lt_vim_auto_coding_rtq).

    assert_not_initial( lt_vim_auto_coding_rtq ).

  ENDMETHOD.


  METHOD assert_rtq_config_not_exists.
    SELECT determinationuuid
       FROM zi_fi_vim_auto_coding
       WHERE dettypecu = @sc_determination_type
       INTO TABLE @DATA(lt_vim_auto_coding_rtq).

    assert_initial( lt_vim_auto_coding_rtq ).
  ENDMETHOD.

  METHOD setup.
    delete_auto_coding_config( ).
  ENDMETHOD.


  METHOD delete_auto_coding_config.

    SELECT determinationuuid
       FROM zi_fi_vim_auto_coding
       WHERE dettypecu = @sc_determination_type
       INTO TABLE @DATA(lt_vim_auto_coding_rtq).

    IF lt_vim_auto_coding_rtq IS NOT INITIAL.
      MODIFY ENTITY zi_fi_vim_auto_coding
        DELETE FROM VALUE #( (  determinationuuid = lt_vim_auto_coding_rtq[ 1 ]-determinationuuid ) ).

      COMMIT ENTITIES.
    ENDIF.
  ENDMETHOD.

  METHOD teardown.
    delete_auto_coding_config( ).
  ENDMETHOD.
ENDCLASS.


"! Tests for file import on debiting
CLASS lct_file_import_on_debiting DEFINITION
  INHERITING FROM zcl_abap_unit_test
  FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: ty_t_file_line_read_result TYPE TABLE FOR READ RESULT zi_file_buffer\_fileline.

    CONSTANTS sc_file_name_on_debiting TYPE string VALUE 'aktiveMAlg_20250101.xlsx'.

    CONSTANTS sc_posting_date TYPE zi_ondebiting-postingdate VALUE '20250101'.

    "! Test file import on debiting for employees country
    "! for contract process Z045(national) and Z039(companies)
    METHODS on_debit_employees_count FOR TESTING.

    "! Asserts that on debiting entries have been created
    METHODS assert_on_debiting_created.

    "! Asserts that on debiting entries do not exist
    METHODS assert_on_debiting_not_exists.

    "! Prepare database for test by deleting
    METHODS setup.

    "! Clean up database after test by deleting
    METHODS teardown.

    "! Deletes on debiting entries for employees country
    METHODS delete_on_debiting.

ENDCLASS.

CLASS lct_file_import_on_debiting IMPLEMENTATION.

  METHOD assert_on_debiting_created.
    SELECT ondebitinguuid
         FROM zi_ondebiting
         INTO TABLE @DATA(lt_on_debiting_created)
         WHERE ( contractprocess = @zif_on_debiting_proc_var_const=>sc_pay_employees_national OR
                 contractprocess = @zif_on_debiting_proc_var_const=>sc_pay_act_employees_companies )
           AND postingdate = @sc_posting_date.

    assert_not_initial( lt_on_debiting_created ).

  ENDMETHOD.

  METHOD assert_on_debiting_not_exists.
    SELECT ondebitinguuid
          FROM zi_ondebiting
          INTO TABLE @DATA(lt_on_debiting)
          WHERE ( contractprocess = @zif_on_debiting_proc_var_const=>sc_pay_employees_national OR
                  contractprocess = @zif_on_debiting_proc_var_const=>sc_pay_act_employees_companies )
            AND postingdate = @sc_posting_date.

    assert_initial( lt_on_debiting ).
  ENDMETHOD.

  METHOD delete_on_debiting.
    DATA lt_on_debiting_delete TYPE TABLE FOR DELETE zi_ondebiting.

    SELECT ondebitinguuid
      FROM zi_ondebiting
      INTO CORRESPONDING FIELDS OF TABLE @lt_on_debiting_delete
      WHERE ( contractprocess = @zif_on_debiting_proc_var_const=>sc_pay_employees_national OR
              contractprocess = @zif_on_debiting_proc_var_const=>sc_pay_act_employees_companies )
        AND postingdate = @sc_posting_date.


    MODIFY ENTITY zi_ondebiting
      DELETE FROM lt_on_debiting_delete.

    COMMIT ENTITIES.
  ENDMETHOD.

  METHOD on_debit_employees_count.
    "given
    assert_on_debiting_not_exists( ).

    DATA lt_file_content TYPE ty_t_file_line_read_result.

    DATA lv_content1 TYPE string.
    DATA lv_content2 TYPE string.
    DATA lv_content3 TYPE string.

    lv_content1 = `Austria` && cl_abap_char_utilities=>horizontal_tab &&
                  `0700 Fielmann GmbH` && cl_abap_char_utilities=>horizontal_tab &&
                 `701` && cl_abap_char_utilities=>horizontal_tab &&
                 `7011000` && cl_abap_char_utilities=>horizontal_tab &&
                 `20`.


    lv_content2 = `Austria` && cl_abap_char_utilities=>horizontal_tab &&
                 `0700 Fielmann GmbH` && cl_abap_char_utilities=>horizontal_tab &&
                 `702` && cl_abap_char_utilities=>horizontal_tab &&
                 `7021000` && cl_abap_char_utilities=>horizontal_tab &&
                 `25`.

    lv_content3 = `Switzerland` && cl_abap_char_utilities=>horizontal_tab &&
                 `0585 Fielmann GmbH` && cl_abap_char_utilities=>horizontal_tab &&
                 `585` && cl_abap_char_utilities=>horizontal_tab &&
                 `585100` && cl_abap_char_utilities=>horizontal_tab &&
                 `15`.

    lt_file_content = VALUE #( ( linecontent = lv_content1
                                 linenumber = '0000000001' )
                               ( linecontent = lv_content2
                                 linenumber = '0000000002' )
                               ( linecontent = lv_content3
                                 linenumber = '0000000003' ) ).
    "when
    TRY.
        DATA(lo_file_import_handler) = zcf_file_import_handler=>get_instance(
                                                   iv_file_name = sc_file_name_on_debiting
                                                   iv_file_upload_process = zcf_file_import_handler=>sc_file_upload_process-on_debiting ).

        DATA(lt_bapiret2) = lo_file_import_handler->run( lt_file_content ).

        IF line_exists( lt_bapiret2[ type = 'E' ] ).
          abort( iv_msg = lt_bapiret2[ type = 'E' ]-message ).
        ENDIF.


        COMMIT ENTITIES.

      CATCH zcx_file_import_handler INTO DATA(lx_file_import_handler).
        assert_not_bound( iv_act = lx_file_import_handler iv_msg = lx_file_import_handler->get_text( ) ).
    ENDTRY.


    "then
    assert_not_bound( lx_file_import_handler ).

    assert_on_debiting_created( ).

  ENDMETHOD.

  METHOD setup.
    delete_on_debiting( ).
    zcl_ca_file_utility_fiori=>get_file_utility_fiori( ).
  ENDMETHOD.

  METHOD teardown.
    delete_on_debiting( ).
  ENDMETHOD.

ENDCLASS.
