CLASS lct_bp_i_file_buffer
DEFINITION INHERITING FROM zcl_abap_unit_test
FOR TESTING
RISK LEVEL HARMLESS
DURATION SHORT.
  PRIVATE SECTION.
    METHODS read_file_buffer FOR TESTING.

ENDCLASS.

CLASS lct_bp_i_file_buffer IMPLEMENTATION.

  METHOD read_file_buffer.
    SELECT * FROM zfile_buffer
      UP TO 1 ROWS
      INTO TABLE @DATA(file_buffer).

    DATA: convin   TYPE REF TO cl_abap_conv_in_ce,
          l_data   TYPE string,
          l_buffer TYPE xstring.


    convin = cl_abap_conv_in_ce=>create(
                         encoding = 'UTF-8'
                         endian = 'L'
                         ignore_cerr = 'X'
                         replacement = '#'
                         input = file_buffer[ 1 ]-content ).


    convin->read( IMPORTING data = l_data ).

  ENDMETHOD.

ENDCLASS.
