CLASS LHC_ZR_TEXTURIZATION DEFINITION INHERITING FROM CL_ABAP_BEHAVIOR_HANDLER.
  PRIVATE SECTION.
    METHODS:
      GET_GLOBAL_AUTHORIZATIONS FOR GLOBAL AUTHORIZATION
        IMPORTING
           REQUEST requested_authorizations FOR ZrTexturization
        RESULT result.
       METHODS deletebyTransId FOR MODIFY
      IMPORTING keys FOR ACTION ZrTexturization~deletebyTransId RESULT result.

ENDCLASS.

CLASS LHC_ZR_TEXTURIZATION IMPLEMENTATION.
  METHOD GET_GLOBAL_AUTHORIZATIONS.
  ENDMETHOD.

    METHOD deletebytransid.

      READ TABLE keys INTO DATA(ls_key) INDEX 1.
      DATA(transid) = ls_key-%param-transid.


      SELECT FROM zr_texturization
      FIELDS productionorder,
             orderdate,
             shift,
             type,
             sno,
             transid
      WHERE transid = @transid
      INTO TABLE @DATA(deletionlines).

      DATA lt_delete TYPE TABLE FOR DELETE zr_texturization.

      LOOP AT deletionlines INTO DATA(ls_delete).
        APPEND VALUE #( productionorder = ls_delete-productionorder
                              orderdate = ls_delete-orderdate
                              shift = ls_delete-shift
                              type = ls_delete-type
                              sno = ls_delete-sno
                              transid = ls_delete-transid ) TO lt_delete.
      ENDLOOP.

      MODIFY ENTITIES OF zr_texturization IN LOCAL MODE
      ENTITY zrtexturization
      DELETE FROM lt_delete
      FAILED   DATA(ls_failed)
      REPORTED DATA(ls_reported).

      APPEND VALUE #(
              %cid = ls_key-%cid
              %msg = new_message_with_text(
                        severity = if_abap_behv_message=>severity-success
                        text     = |Deleted successfully.| )
           ) TO reported-zrtexturization.

    ENDMETHOD.

ENDCLASS.
