@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZTRANSFERJOURNAL'
}
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZC_TRANSFERJOURNAL
  provider contract transactional_query
  as projection on ZR_TRANSFERJOURNAL
  
{
  key HeaderID,
  key Material,
  key Plant,
  key Sno,
  FromLocation,
  ToLocation,
  FromBatch,
  ToBatch,
  Quantity,
  EntryUnit,
  Errorlog,
  Posted,
  PostingUOM,
  DocumentNumber,
  DocumentYear,
  SendToMES,
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
