"! <p class="shorttext synchronized" lang="en">Superclass File Import CSV</p>
CLASS zcl_file_import_handler DEFINITION
  PUBLIC
  ABSTRACT.

  PUBLIC SECTION.

    INTERFACES zif_file_import_handler .

  PROTECTED SECTION.

    "! <p class="shorttext synchronized" lang="en">File Table Reference</p>
    DATA mr_file_table TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Indicator Header Line</p>
    DATA mv_file_has_header_line TYPE abap_bool VALUE abap_true ##NO_TEXT.
  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">Semicolon</p>
    CONSTANTS sc_semi_colon TYPE string VALUE ';' ##NO_TEXT.
    "! <p class="shorttext synchronized" lang="en">Content Field Name</p>
    CONSTANTS sc_content_field_name TYPE string VALUE 'LINECONTENT' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">Get Field Separator from Content</p>
    "!
    "! @parameter iv_header_line     | <p class="shorttext synchronized" lang="en">Header Line</p>
    "! @parameter rv_field_separator | <p class="shorttext synchronized" lang="en">Separator</p>
    METHODS get_field_separator
      IMPORTING
        !iv_header_line           TYPE data
      RETURNING
        VALUE(rv_field_separator) TYPE string .
    "! <p class="shorttext synchronized" lang="en">Get Components of Table for File</p>
    "!
    "! @parameter iv_header_line     | <p class="shorttext synchronized" lang="en">Header Line</p>
    "! @parameter iv_field_separator | <p class="shorttext synchronized" lang="en">Field Separator</p>
    "! @parameter rt_components      | <p class="shorttext synchronized" lang="en">Components</p>
    METHODS get_components
      IMPORTING
        !iv_header_line      TYPE data
        !iv_field_separator  TYPE string
      RETURNING
        VALUE(rt_components) TYPE abap_component_tab .
    "! <p class="shorttext synchronized" lang="en">Get Content of Header Line</p>
    "!
    "! @parameter it_file_content        | <p class="shorttext synchronized" lang="en">File Content</p>
    "! @parameter rv_header_line_content | <p class="shorttext synchronized" lang="en">Header Line Content</p>
    METHODS get_header_line_content
      IMPORTING
        !it_file_content              TYPE STANDARD TABLE
      RETURNING
        VALUE(rv_header_line_content) TYPE string .
    "! <p class="shorttext synchronized" lang="en">Creates File Structure for Import</p>
    "!
    "! @parameter it_components     | <p class="shorttext synchronized" lang="en">Components</p>
    "! @parameter er_file_structure | <p class="shorttext synchronized" lang="en">Reference to File Structure</p>
    METHODS create_file_structure
      IMPORTING
        !it_components           TYPE abap_component_tab
      RETURNING
        VALUE(er_file_structure) TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Fill typed Table from File Content</p>
    "!
    "! @parameter it_file_content    | <p class="shorttext synchronized" lang="en">File Content</p>
    "! @parameter iv_field_separator | <p class="shorttext synchronized" lang="en">Field Separator</p>
    "! @parameter it_components      | <p class="shorttext synchronized" lang="en">Components</p>
    METHODS fill_file_table
      IMPORTING
        !it_file_content    TYPE STANDARD TABLE
        !iv_field_separator TYPE string
        !it_components      TYPE abap_component_tab .
ENDCLASS.



CLASS zcl_file_import_handler IMPLEMENTATION.

  METHOD zif_file_import_handler~run.

    IF lines( it_file_content ) = 0.
      RETURN.
    ENDIF.

    DATA(lv_header_line_content) = get_header_line_content( it_file_content ).

    DATA(lv_field_separator) = get_field_separator( lv_header_line_content ).


    DATA(lt_components) = get_components( iv_header_line = lv_header_line_content
                                          iv_field_separator = lv_field_separator ).


    fill_file_table( it_file_content = it_file_content
                     iv_field_separator = lv_field_separator
                     it_components = lt_components ).


  ENDMETHOD.


  METHOD get_field_separator.
    DATA(lv_semicolon_found) = find_any_of( val = iv_header_line sub = sc_semi_colon ).

    IF lv_semicolon_found = -1.
      rv_field_separator = cl_abap_char_utilities=>horizontal_tab.
    ELSE.
      rv_field_separator = sc_semi_colon.
    ENDIF.

  ENDMETHOD.


  METHOD get_components.

    SPLIT iv_header_line AT iv_field_separator INTO TABLE DATA(lt_fieldnames).

    DATA lt_components TYPE abap_component_tab.

    IF mv_file_has_header_line = abap_true.
      rt_components = VALUE #( FOR lv_fieldname IN lt_fieldnames
                                ( name = lv_fieldname
                                  type = cl_abap_elemdescr=>get_string( ) ) ).
    ELSE.
      rt_components = VALUE #( FOR lv_value IN lt_fieldnames INDEX INTO lv_field_index
                                     ( name = 'F_' && lv_field_index
                                       type = cl_abap_elemdescr=>get_string( ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_header_line_content.
    READ TABLE it_file_content INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_header_line>).

    ASSIGN COMPONENT sc_content_field_name OF STRUCTURE <ls_header_line> TO FIELD-SYMBOL(<lv_header_line_content>).

    rv_header_line_content = <lv_header_line_content>.

  ENDMETHOD.


  METHOD create_file_structure.

    DATA lr_file_structure TYPE REF TO data.

    TRY.
        "create a structure description for string fields
        "for all field names of the header line of the file.
        DATA(lo_structure_description) = cl_abap_structdescr=>get( it_components ).

        "create a table description based on the structure description
        DATA(lo_table_description) = cl_abap_tabledescr=>get( lo_structure_description ).

        CREATE DATA er_file_structure TYPE HANDLE lo_structure_description.
        CREATE DATA mr_file_table TYPE HANDLE lo_table_description.
      CATCH cx_sy_struct_creation INTO DATA(lx_sy_struct_creation).
    ENDTRY.

  ENDMETHOD.


  METHOD fill_file_table.

    DATA(lr_file_structure) = create_file_structure( it_components ).


    "assign file structure and file table
    FIELD-SYMBOLS <lt_file_table> TYPE STANDARD TABLE.
    ASSIGN lr_file_structure->* TO FIELD-SYMBOL(<ls_file_structure>).
    ASSIGN mr_file_table->* TO <lt_file_table>.

    DATA(lo_csv_converter) = cl_rsda_csv_converter=>create( i_separator = CONV #( iv_field_separator ) ).

    "Process file content and build
    "table with field values of the file content.
    "The table has field names which the posting subclass
    "takes as input for an entity or a different API.
    LOOP AT it_file_content ASSIGNING FIELD-SYMBOL(<ls_file_content>).

      "skip header line with field names
      AT FIRST.
        IF mv_file_has_header_line = abap_true.
          CONTINUE.
        ENDIF.
      ENDAT.

      ASSIGN COMPONENT sc_content_field_name OF STRUCTURE <ls_file_content> TO FIELD-SYMBOL(<lv_line_content>).
      lo_csv_converter->csv_to_structure(
         EXPORTING
           i_data   = <lv_line_content>
         IMPORTING
           e_s_data = <ls_file_structure> ).

      <lt_file_table> = VALUE #( BASE <lt_file_table> ( <ls_file_structure> ) ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
