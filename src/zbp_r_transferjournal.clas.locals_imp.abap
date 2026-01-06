CLASS LHC_ZR_TRANSFERJOURNAL DEFINITION INHERITING FROM CL_ABAP_BEHAVIOR_HANDLER.
  PRIVATE SECTION.
    METHODS:
      GET_GLOBAL_AUTHORIZATIONS FOR GLOBAL AUTHORIZATION
        IMPORTING
           REQUEST requested_authorizations FOR ZrTransferjournal
        RESULT result,
      ClearErrorlog FOR MODIFY
      IMPORTING keys FOR ACTION ZrTransferjournal~ClearErrorlog.
ENDCLASS.

CLASS LHC_ZR_TRANSFERJOURNAL IMPLEMENTATION.
  METHOD GET_GLOBAL_AUTHORIZATIONS.
  ENDMETHOD.

  METHOD ClearErrorlog.

    LOOP AT keys INTO DATA(ls_key).
        MODIFY ENTITIES OF zr_transferjournal IN LOCAL MODE
        ENTITY ZrTransferjournal
        UPDATE FIELDS ( Errorlog )
        WITH VALUE #( (
           Errorlog = ''
           %key = ls_key-%key
        ) ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
