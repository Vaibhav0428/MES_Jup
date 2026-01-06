class ZCL_HTTP_DELIVERY_QTY definition
  public
  create public .

public section.

  interfaces IF_HTTP_SERVICE_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HTTP_DELIVERY_QTY IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
  CASE request->get_method(  ).
     WHEN CONV string( if_web_http_client=>get ).
        DATA(req) = request->get_form_fields( ).

        response->set_header_field( i_name = 'Access-Control-Allow-Origin' i_value = '*' ).
        response->set_header_field( i_name = 'Access-Control-Allow-Credentials' i_value = 'true' ).

        DATA(lv_del) = VALUE #( req[ name = 'deliverydocument' ]-value OPTIONAL ).

        data : delivery_doc type I_DeliveryDocument-DeliveryDocument.
        delivery_doc = |{ lv_del ALPHA = IN }|.

       select from I_DeliveryDocumentItem as a
       left join I_UnitOfMeasureText as b on a~DeliveryQuantityUnit = b~UnitOfMeasure and b~Language = 'E'
       fields a~DeliveryDocument, a~DeliveryDocumentItem, a~Material,a~Batch,a~ActualDeliveryQuantity,b~UnitOfMeasure_E as DeliveryUnit, a~Plant
       where a~DeliveryDocument = @delivery_doc
       and a~DeliveryDocumentItemCategory = 'TAN'
       into table @data(it_qty)
       PRIVILEGED ACCESS.

        /ui2/cl_json=>serialize(
            EXPORTING
              data        = it_qty
              pretty_name = /ui2/cl_json=>pretty_mode-camel_case
            RECEIVING
              r_json      = DATA(lv_json)
          ).

        response->set_text( lv_json ).
   ENDCASE.
  endmethod.
ENDCLASS.
