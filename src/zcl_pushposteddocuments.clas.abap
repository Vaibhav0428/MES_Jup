CLASS zcl_pushposteddocuments DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    INTERFACES if_apj_dt_exec_object .
    INTERFACES if_apj_rt_exec_object .


    CLASS-METHODS runjob
      IMPORTING paramcmno TYPE c.

    CLASS-METHODS generate_payload_texturization
      RETURNING VALUE(lv_payload) TYPE string.

    CLASS-METHODS generate_payload_transferjnl
        RETURNING VALUE(lv_payload) TYPE string.

    CLASS-METHODS pushTexturization.
    CLASS-METHODS pushtransferjournal.

    CLASS-METHODS parse_response
      IMPORTING lv_response TYPE string
                tabletype TYPE string.

    TYPES: BEGIN OF ty_texturization,
             materialdocnumber TYPE zr_texturization-document,
             documentNumber TYPE zr_texturization-document,
             transactionid     TYPE zr_texturization-transid,
             linenumber        TYPE i,
             documentYear      TYPE C LENGTH 4,
           END OF ty_texturization.


    CLASS-DATA: lt_texturization TYPE TABLE OF ty_texturization,
          ls_texturization TYPE ty_texturization.

PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PUSHPOSTEDDOCUMENTS IMPLEMENTATION.


  METHOD if_apj_dt_exec_object~get_parameters.
    " Return the supported selection parameters here
    et_parameter_def = VALUE #(
      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     datatype = 'C' length = 80 param_text = ''   lowercase_ind = abap_true changeable_ind = abap_true )
    ).

    " Return the default parameters values here
    et_parameter_val = VALUE #(
      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = '' )
    ).

  ENDMETHOD.


  METHOD if_apj_rt_exec_object~execute.
    DATA p_descr TYPE c LENGTH 80.

    " Getting the actual parameter values
    LOOP AT it_parameters INTO DATA(ls_parameter).
      CASE ls_parameter-selname.
        WHEN 'P_DESCR'. p_descr = ls_parameter-low.
      ENDCASE.
    ENDLOOP.
    runjob( p_descr ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    runjob( '' ).
  ENDMETHOD.


    METHOD runjob.
      pushtexturization(  ).
      pushtransferjournal(  ).
    ENDMETHOD.


     METHOD pushtransferjournal.

       SELECT SINGLE FROM zr_transferjournal
         FIELDS COUNT( headerid ) AS line
         WHERE posted = 'X' AND sendtomes = ''
               AND headerid IS NOT INITIAL
               AND documentnumber IS NOT INITIAL
         INTO @DATA(texturization).

       IF texturization IS INITIAL.
         RETURN.
       ENDIF.

       DATA integration_url TYPE string.
       DATA lv_client2 TYPE REF TO if_web_http_client.

       DATA req TYPE REF TO if_web_http_client.


       SELECT SINGLE FROM zr_integration_tab
       FIELDS intgpath
       WHERE intgmodule = 'JUPITER-MATDOC-HOTO-POST-URL'
       INTO @integration_url.

       IF integration_url IS INITIAL.
         RETURN.
       ENDIF.

       TRY.
           DATA(dest2) = cl_http_destination_provider=>create_by_url( integration_url ).
           lv_client2 = cl_web_http_client_manager=>create_by_http_destination( dest2 ).

         CATCH cx_static_check INTO DATA(lv_cx_static_check2).
           RETURN.
       ENDTRY.

       DATA(req4) = lv_client2->get_http_request( ).

       SELECT SINGLE FROM zr_integration_tab
       FIELDS intgpath
       WHERE intgmodule = 'JUPITER-MATDOC-POST-KEY'
       INTO @DATA(auth_req).

       SPLIT auth_req AT ':' INTO DATA(head1name) DATA(head1val).
       req4->set_header_field(
           i_name =  head1name
           i_value = head1val
       ).

       req4->set_content_type( 'application/json' ).
       req4->append_text( EXPORTING data = generate_payload_transferjnl( ) ).


       TRY.
           DATA(lv_response) = lv_client2->execute( if_web_http_client=>post ).
           DATA(st_code) = lv_response->get_status( )-code.
           DATA(resp_text) = lv_response->get_text( ).

           IF st_code NE '200'.
             RETURN.
           ENDIF.

           parse_response( lv_response = resp_text tabletype = 'TRANSJOUR' ).

         CATCH cx_root INTO DATA(lx_http_err).
           DATA(response) = lx_http_err->get_text( ).
       ENDTRY.

     ENDMETHOD.


    METHOD pushtexturization.

      SELECT SINGLE FROM zr_texturization
           FIELDS COUNT( transid ) AS transid
           WHERE posted = 'X' AND sendtomes = ''
                 AND transid IS NOT INITIAL
                 AND document IS NOT INITIAL
           INTO @DATA(texturization).

      IF texturization IS INITIAL.
        RETURN.
      ENDIF.


      DATA integration_url TYPE string.
      DATA lv_client2 TYPE REF TO if_web_http_client.

      DATA req TYPE REF TO if_web_http_client.


      SELECT SINGLE FROM zr_integration_tab
      FIELDS intgpath
      WHERE intgmodule = 'JUPITER-MATDOC-POST-URL'
      INTO @integration_url.

      IF integration_url IS INITIAL.
        RETURN.
      ENDIF.

      TRY.
          DATA(dest2) = cl_http_destination_provider=>create_by_url( integration_url ).
          lv_client2 = cl_web_http_client_manager=>create_by_http_destination( dest2 ).

        CATCH cx_static_check INTO DATA(lv_cx_static_check2).
          RETURN.
      ENDTRY.

      DATA(req4) = lv_client2->get_http_request( ).

      SELECT SINGLE FROM zr_integration_tab
      FIELDS intgpath
      WHERE intgmodule = 'JUPITER-MATDOC-POST-KEY'
      INTO @DATA(auth_req).

      SPLIT auth_req AT ':' INTO DATA(head1name) DATA(head1val).
      req4->set_header_field(
          i_name =  head1name
          i_value = head1val
      ).



      req4->set_content_type( 'application/json' ).
      req4->append_text( EXPORTING data = generate_payload_texturization( ) ).


      TRY.
          DATA(lv_response) = lv_client2->execute( if_web_http_client=>post ).
          DATA(st_code) = lv_response->get_status( )-code.
          DATA(resp_text) = lv_response->get_text( ).

          IF st_code NE '200'.
            RETURN.
          ENDIF.

          parse_response( lv_response = resp_text tabletype = 'TEXTURE' ).

        CATCH cx_root INTO DATA(lx_http_err).
          DATA(response) = lx_http_err->get_text( ).
      ENDTRY.

    ENDMETHOD.


    METHOD parse_response.

      TYPES: BEGIN OF ty_data,
               materialdocnumber TYPE string,
               documentnumber    TYPE string,
               documentyear      TYPE string,
               transactionid     TYPE string,
               linenumber        TYPE i,
               issuccess         TYPE string,
               message           TYPE string,
             END OF ty_data.

      TYPES:BEGIN OF ty_response,
              issuccess TYPE string,
              message   TYPE string,
              data      TYPE STANDARD TABLE OF ty_data WITH EMPTY KEY,
            END OF ty_response.

      DATA: ls_response TYPE ty_response.
      TRY.
          xco_cp_json=>data->from_string( lv_response )->write_to( REF #( ls_response ) ).
        CATCH cx_web_http_client_error INTO DATA(lv_error_response2).
          DATA(error_res) = lv_error_response2->get_longtext( ).
      ENDTRY.

      IF ls_response-issuccess = 'true'.
        LOOP AT ls_response-data INTO DATA(ls_data).
          IF ls_data-issuccess = 'true'.
            IF tabletype = 'TRANSJOUR'.
              UPDATE ztransferjournal SET
               sendtomes = @abap_true
               WHERE document_number = @ls_data-documentnumber
               AND header_id = @ls_data-transactionid
               AND document_year = @ls_data-documentyear.
            ELSE.
              UPDATE ztexturization SET
                sendtomes = @abap_true
                WHERE document = @ls_data-materialdocnumber
                AND transid = @ls_data-transactionid
                AND sno = @ls_data-linenumber.
            ENDIF.
          ELSE.
            IF tabletype = 'TRANSJOUR'.
              UPDATE ztransferjournal SET
               errorlog = @ls_data-message
               WHERE document_number = @ls_data-documentnumber
               AND header_id = @ls_data-transactionid
               AND document_year = @ls_data-documentyear.
            ELSE.
              UPDATE ztexturization SET
                errorlog = @ls_data-message
                WHERE document = @ls_data-materialdocnumber
                AND transid = @ls_data-transactionid
                AND sno = @ls_data-linenumber.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDMETHOD.


    METHOD generate_payload_texturization.

      CLEAR: lt_texturization, ls_texturization.
      SELECT FROM zr_texturization
       FIELDS document, transid, sno
       WHERE posted = 'X' AND sendtomes = ''
             AND transid IS NOT INITIAL
             AND document IS NOT INITIAL
       ORDER BY document, transid, sno
       INTO TABLE @DATA(texturization)
       UP TO 100 ROWS.

      LOOP AT texturization INTO DATA(ls_texturization_db).
        ls_texturization-materialdocnumber = ls_texturization_db-document.
        ls_texturization-transactionid = ls_texturization_db-transid.
        ls_texturization-linenumber = ls_texturization_db-sno.
        APPEND ls_texturization TO lt_texturization.
        CLEAR ls_texturization.
      ENDLOOP.

      DATA:json TYPE REF TO if_xco_cp_json_data.

      xco_cp_json=>data->from_abap(
        EXPORTING
          ia_abap      = lt_texturization
        RECEIVING
          ro_json_data = json   ).
      json->to_string(
        RECEIVING
          rv_string =   lv_payload ).

      REPLACE ALL OCCURRENCES OF 'MATERIALDOCNUMBER' IN lv_payload WITH 'materialDocNumber'.
      REPLACE ALL OCCURRENCES OF 'TRANSACTIONID' IN lv_payload WITH 'transactionId'.
      REPLACE ALL OCCURRENCES OF 'LINENUMBER' IN lv_payload WITH 'lineNumber'.

    ENDMETHOD.


      METHOD generate_payload_transferjnl.

        CLEAR: lt_texturization, ls_texturization.

        SELECT FROM zr_transferjournal
         FIELDS headerid,documentnumber,documentyear
         WHERE posted = 'X' AND sendtomes = ''
               AND headerid IS NOT INITIAL
               AND documentnumber IS NOT INITIAL
         ORDER BY headerid,documentnumber,documentyear
         INTO TABLE @DATA(texturization)
         UP TO 100 ROWS.

        LOOP AT texturization INTO DATA(ls_texturization_db).
          ls_texturization-documentnumber = ls_texturization_db-documentnumber.
          ls_texturization-transactionid = ls_texturization_db-headerid.
          ls_texturization-documentyear = ls_texturization_db-documentyear.

          APPEND ls_texturization TO lt_texturization.
          CLEAR ls_texturization.
        ENDLOOP.

        DATA:json TYPE REF TO if_xco_cp_json_data.

        xco_cp_json=>data->from_abap(
          EXPORTING
            ia_abap      = lt_texturization
          RECEIVING
            ro_json_data = json   ).
        json->to_string(
          RECEIVING
            rv_string =   lv_payload ).

        REPLACE ALL OCCURRENCES OF 'TRANSACTIONID' IN lv_payload WITH 'transactionId'.
        REPLACE ALL OCCURRENCES OF 'LINENUMBER' IN lv_payload WITH 'lineNumber'.
        REPLACE ALL OCCURRENCES OF 'DOCUMENTYEAR' IN lv_payload WITH 'documentYear'.
        REPLACE ALL OCCURRENCES OF 'DOCUMENTNUMBER' IN lv_payload WITH 'documentNumber'.

      ENDMETHOD.
ENDCLASS.
