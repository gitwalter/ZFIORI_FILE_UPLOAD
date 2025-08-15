"! <p class="shorttext synchronized" lang="en">File Import Handler</p>
INTERFACE zif_file_import_handler
  PUBLIC.


  "! <p class="shorttext synchronized" lang="en">Execute File Import</p>
  "!
  "! @parameter it_file_content         | <p class="shorttext synchronized" lang="en">File Content</p>
  "! @parameter rt_bapiret2             | <p class="shorttext synchronized" lang="en">Return table</p>
  "! @raising   zcx_file_import_handler | <p class="shorttext synchronized" lang="en">Exceptions File Import Handler</p>
  METHODS run
    IMPORTING
      !it_file_content   TYPE STANDARD TABLE
    RETURNING
      VALUE(rt_bapiret2) TYPE bapiret2_t
    RAISING
      zcx_file_import_handler .
ENDINTERFACE.
