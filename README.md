## ABAP File Import – Technical Documentation

This repository contains an ABAP-based file import framework that parses uploaded CSV/XLSX files and routes the parsed content to process-specific handlers. It is integrated with a RAP business object (`zi_file_buffer`) to support file uploads from UI (e.g., SAP Fiori/UI5) and back-end processing.

- **Primary interface**: `zif_file_import_handler`
- **Abstract base**: `zcl_file_import_handler`
- **Factory**: `zcf_file_import_handler`
- **Exception**: `zcx_file_import_handler`
- **RAP behavior**: `lhc_zi_file_buffer` inside `zbp_i_file_buffer`

## Architecture

- **RAP Entity `zi_file_buffer`**: Holds the uploaded file (fields include `filename`, `mimetype`, `content`, `fileuploadprocess`). Child entity `zi_file_buffer_fileline` stores individual text lines (`linenumber`, `linecontent`).
- **Behavior implementation `lhc_zi_file_buffer`**: Orchestrates line extraction from the uploaded file and triggers the file import handler based on the selected process.
- **Factory `zcf_file_import_handler`**: Creates a concrete handler instance for a given file upload process and filename.
- **Interface `zif_file_import_handler`**: Defines the `run` method that processes file content and returns `bapiret2_t` messages.
- **Abstract base `zcl_file_import_handler`**: Provides robust CSV/TSV header parsing and table-building utilities that concrete handlers reuse.
- **Exception `zcx_file_import_handler`**: BAPI-style exception carrying `BAPIRETTAB` for uniform error reporting.

## Public API

### `zif_file_import_handler`

- **Method**: `run`
  - **Signature**:
    - `IMPORTING it_file_content TYPE STANDARD TABLE`
    - `RETURNING VALUE(rt_bapiret2) TYPE bapiret2_t`
    - `RAISING zcx_file_import_handler`
  - **Semantics**:
    - Process the file content (as a table of lines with a `LINECONTENT` component) and return messages in `BAPIRET2_T` format.
    - Concrete handlers typically call the base implementation to parse lines into a typed table and then perform business posting, validations, etc.

### `zcf_file_import_handler`

- **Method**: `get_instance`
  - **Signature**:
    - `IMPORTING iv_file_upload_process TYPE zfile_upload_process`
    - `IMPORTING iv_file_name TYPE string`
    - `RETURNING VALUE(ro_file_import_handler) TYPE REF TO zif_file_import_handler`
    - `RAISING zcx_file_import_handler`
  - **Constants** `sc_file_upload_process`:
    - `auto_coding = 'AUTO_CODE'`
    - `on_debiting = 'ON_DEBIT'`
  - **Behavior**:
    - Returns a handler instance based on process and, for on-debiting, the filename. Converts any process-specific exceptions into a `zcx_file_import_handler` carrying `BAPIRETTAB`.

### `zcx_file_import_handler`

- BAPI-style exception class inheriting from `ZCX_BAPI_RETURN_EXCEPTION` and implementing `IF_T100_MESSAGE`.
- Used to surface errors consistently to both classic code and RAP behavior.

## Base parsing utilities (`zcl_file_import_handler`)

These methods implement the core parsing logic used by all concrete handlers. The base class is abstract and implements `zif_file_import_handler~run` to prepare data for business processing. Subclasses typically redefine the interface method and call `super->zif_file_import_handler~run( )` first.

- `get_header_line_content( it_file_content TYPE STANDARD TABLE ) RETURNING string`
  - Reads the first line's `LINECONTENT`.

- `get_field_separator( iv_header_line TYPE data ) RETURNING string`
  - If a semicolon is present in the header, uses `;`; otherwise uses horizontal tab (`cl_abap_char_utilities=>horizontal_tab`).

- `get_components( iv_header_line TYPE data, iv_field_separator TYPE string ) RETURNING abap_component_tab`
  - Builds a component list of `string` fields. If `mv_file_has_header_line = abap_true` (default), uses header field names; otherwise generates `F_1`, `F_2`, ...

- `create_file_structure( it_components TYPE abap_component_tab ) RETURNING REF TO data`
  - Uses `cl_abap_structdescr`/`cl_abap_tabledescr` to create a typed row structure and a typed internal table (`mr_file_table`).

- `fill_file_table( it_file_content TYPE STANDARD TABLE, iv_field_separator TYPE string, it_components TYPE abap_component_tab )`
  - Converts each `LINECONTENT` CSV/TSV line into the typed structure using `cl_rsda_csv_converter`. Skips the header row when `mv_file_has_header_line = abap_true`.

- Additional members:
  - `mr_file_table TYPE REF TO data` – reference to the typed table filled by `fill_file_table`.
  - `mv_file_has_header_line TYPE abap_bool DEFAULT abap_true` – controls header handling.
  - `sc_content_field_name = 'LINECONTENT'` – structure component name holding the raw content.

## RAP behavior: `lhc_zi_file_buffer`

Key methods (private to the behavior implementation) that enable end-to-end upload and processing:

- `process_file_content FOR DETERMINE ON SAVE`
  - Reads the root `zi_file_buffer`, loads or creates its child `zi_file_buffer_fileline` if missing, and triggers `execute_file_import` when `fileuploadprocess` is set.

- `get_file_lines( iv_content TYPE xstring, iv_mimetype TYPE string ) RETURNING ty_t_line RAISING zcx_excel`
  - Dispatches by MIME type:
    - `text/csv`: decodes `xstring` to `string` via `cl_abap_conv_in_ce` and splits by `CR_LF`.
    - `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet`: extracts worksheet to an internal table and converts each row to a tab-separated line.

- `get_csv_lines( iv_content TYPE xstring ) RETURNING ty_t_line`
- `get_xlsx_lines( iv_content TYPE xstring ) RETURNING ty_t_line RAISING zcx_excel`
- `get_xlsx_data( iv_content TYPE xstring ) RETURNING REF TO data RAISING zcx_excel`
- `get_xlsx_tab_table( ir_excel_table TYPE REF TO data ) RETURNING ty_t_line`

- `execute_file_import( iv_uuid TYPE zi_file_buffer-uuid, iv_process TYPE zfile_upload_process ) RETURNING ty_s_response_file_buffer`
  - Reads `filename` and existing file lines, initializes `zcl_ca_file_utility_fiori`, resolves a handler via `zcf_file_import_handler=>get_instance`, runs it, and converts `BAPIRET2_T` to RAP messages via `fill_reported_from_bapiret`.

- `read_file_name( iv_uuid ) RETURNING string`
- `read_file_lines( iv_uuid ) RETURNING TABLE FOR READ RESULT zi_file_buffer_fileline`
- `fill_reported_from_bapiret( it_bapiret2 TYPE bapiret2_t ) RETURNING ty_s_response_file_buffer`

## Usage

### Direct ABAP usage (outside RAP)

```abap
DATA lt_file_content TYPE TABLE FOR READ RESULT zi_file_buffer_fileline.

" Example: CSV with header and one data row
lt_file_content = VALUE #(
  ( linecontent = 'COL_A;COL_B;DET_TYPE' linenumber = '0000000001' )
  ( linecontent = 'A1;B1;RtQ'           linenumber = '0000000002' ) ).

DATA(lo_handler) = zcf_file_import_handler=>get_instance(
  iv_file_upload_process = zcf_file_import_handler=>sc_file_upload_process-auto_coding
  iv_file_name           = 'auto_coding_2_2.csv' ).

DATA(lt_bapiret2) = lo_handler->run( lt_file_content ).
IF line_exists( lt_bapiret2[ type = 'E' ] ).
  " handle error
ENDIF.
COMMIT ENTITIES.
```

### On-debiting example (TSV)

```abap
DATA lt_file_content TYPE TABLE FOR READ RESULT zi_file_buffer_fileline.
DATA lv_line1 TYPE string.

lv_line1 = `Austria` && cl_abap_char_utilities=>horizontal_tab &&
           `0700 Fielmann GmbH` && cl_abap_char_utilities=>horizontal_tab &&
           `701` && cl_abap_char_utilities=>horizontal_tab &&
           `7011000` && cl_abap_char_utilities=>horizontal_tab &&
           `20`.

lt_file_content = VALUE #(
  ( linecontent = lv_line1 linenumber = '0000000001' ) ).

DATA(lo_handler) = zcf_file_import_handler=>get_instance(
  iv_file_upload_process = zcf_file_import_handler=>sc_file_upload_process-on_debiting
  iv_file_name           = 'aktiveMAlg_20250101.xlsx' ).

DATA(lt_bapiret2) = lo_handler->run( lt_file_content ).
IF line_exists( lt_bapiret2[ type = 'E' ] ).
  " handle error
ENDIF.
COMMIT ENTITIES.
```

### RAP-triggered processing (DETERMINE ON SAVE)

The RAP behavior auto-executes import when you create or update a `zi_file_buffer` instance with `fileuploadprocess`, `filename`, `mimetype`, and `content`.

```abap
DATA lt_create TYPE TABLE FOR CREATE zi_file_buffer.

lt_create = VALUE #(
  ( %cid               = 'NEW1'
    filename           = 'myfile.csv'
    mimetype           = 'text/csv'
    content            = <xstring_content>
    fileuploadprocess  = zcf_file_import_handler=>sc_file_upload_process-auto_coding
    %control-filename          = if_abap_behv=>mk-on
    %control-mimetype          = if_abap_behv=>mk-on
    %control-content           = if_abap_behv=>mk-on
    %control-fileuploadprocess = if_abap_behv=>mk-on ) ).

MODIFY ENTITY zi_file_buffer CREATE FROM lt_create REPORTED DATA(lt_reported).
COMMIT ENTITIES.
" lt_reported contains the messages generated by the handler
```

## Error handling

- Handlers return messages via `BAPIRET2_T`:
  - Success and error states are mapped to RAP severities in `fill_reported_from_bapiret`.
- To bubble up structured errors, raise `zcx_file_import_handler` with `MT_RETURN = lt_bapiret2`.
- The factory catches process-specific exceptions (e.g., on-debiting) and rethrows them as `zcx_file_import_handler` with the converted return table.

## Extensibility

### Add a new file upload process

1. Add a new constant to `zcf_file_import_handler=>sc_file_upload_process`.
2. Implement a concrete handler class that implements `zif_file_import_handler` (preferably inheriting from `zcl_file_import_handler`).
3. In the factory `get_instance`, route the new process to your handler (optionally dispatch by filename patterns).
4. If using the RAP/UI flow, surface the new process value in UI and validations.

#### Handler skeleton

```abap
CLASS zcl_file_import_myprocess DEFINITION
  PUBLIC
  INHERITING FROM zcl_file_import_handler.
  PUBLIC SECTION.
    INTERFACES zif_file_import_handler.
    METHODS zif_file_import_handler~run REDEFINITION.
ENDCLASS.

CLASS zcl_file_import_myprocess IMPLEMENTATION.
  METHOD zif_file_import_handler~run.
    " Pre-parse and build mr_file_table
    super->zif_file_import_handler~run( it_file_content ).

    FIELD-SYMBOLS <lt_parsed> TYPE STANDARD TABLE.
    ASSIGN mr_file_table->* TO <lt_parsed>.

    DATA lt_bapiret2 TYPE bapiret2_t.

    LOOP AT <lt_parsed> ASSIGNING FIELD-SYMBOL(<ls_row>).
      " Perform validations/postings and fill lt_bapiret2
    ENDLOOP.

    rt_bapiret2 = lt_bapiret2.
  ENDMETHOD.
ENDCLASS.
```

### Support additional MIME types

- Extend `get_file_lines` in `lhc_zi_file_buffer` to handle your MIME type and produce a table of `string` lines.
- Ensure your handler expects the same line format (header in first line when `mv_file_has_header_line = abap_true`).

## Notes and conventions

- Header detection: If the header line contains `;`, fields are parsed as CSV; otherwise, tab-separated values are assumed.
- Field names are derived from the header. When no header is present, fields are named `F_1`, `F_2`, ...
- The raw line field name is `LINECONTENT`; child lines carry `LINENUMBER` for stable ordering.
- External dependencies (not included here) referenced by the code:
  - `zcl_excel_reader_2007`, `zif_excel_reader` for XLSX reading
  - `zcl_ca_file_utility_fiori` for file utility setup
  - On-debiting: `zcf_on_debiting_import_handler`, `zcx_on_debiting_import`, `zif_on_debiting_proc_var_const`

## Tests

- See `src/zcl_file_import_handler.clas.testclasses.abap` for executable examples of both processes:
  - Auto coding: `lct_file_import_auto_coding`
  - On debiting: `lct_file_import_on_debiting`

## Repository structure (excerpt)

- `src/` – ABAP sources (classes, interfaces, behavior, table types, services)
- UI5 artifacts under `src/zfile_upload.wapa.*` integrate with the RAP service (not covered in detail here).
