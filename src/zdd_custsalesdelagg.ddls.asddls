@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'aggregation cds'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zdd_custsalesdelagg as select from I_DeliveryDocumentItem as ddi
{
    ddi.ReferenceSDDocument as SalesOrder,
    ddi.ReferenceSDDocumentItem as SalesOrderItem,
    @Semantics.quantity.unitOfMeasure: 'DeliveryQuantityUnit'
    sum(ddi.ActualDeliveryQuantity) as DeliveredQuantity,
    ddi.DeliveryQuantityUnit
}
group by
    ddi.ReferenceSDDocument,
    ddi.ReferenceSDDocumentItem,
    ddi.DeliveryQuantityUnit
