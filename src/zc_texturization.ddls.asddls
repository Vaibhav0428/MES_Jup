@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTEXTURIZATION'
}
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZC_TEXTURIZATION
  provider contract transactional_query
  as projection on ZR_TEXTURIZATION{
  key Productionorder,
  key Orderdate,
  key Shift,
  key Type,
  key Sno,
  key Transid,
  Toolid,
  Refdocno,
  Material,
  Materialdescription,
  UOM,
  Quantity,
  Batch,
  Storagelocation,
  Tolocation,
  Errorlog,
  Posted,
  Sendtomes,
  Document,
  @Semantics: {
    user.createdBy: true
  }
  CreatedBy,
  @Semantics: {
    systemDateTime.createdAt: true
  }
  CreatedAt,
  @Semantics: {
    user.localInstanceLastChangedBy: true
  }
  LastChangedBy,
  @Semantics: {
    systemDateTime.localInstanceLastChangedAt: true
  }
  LastChangedAt,
  @Semantics: {
    systemDateTime.lastChangedAt: true
  }
  LocalLastChangedAt
}
