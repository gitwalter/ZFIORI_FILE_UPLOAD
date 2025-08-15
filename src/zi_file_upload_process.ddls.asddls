@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'File Upload Process'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS

@Search.searchable: true
define view entity ZI_File_Upload_Process
  as select from dd07l
    inner join   dd07t on  dd07t.domname    = dd07l.domname
                       and dd07t.domvalue_l = dd07l.domvalue_l
{
      @Search.defaultSearchElement: true
      @EndUserText.label: 'File Upload Process'
      @ObjectModel.text.element: [ 'Description' ]
      @UI.textArrangement: #TEXT_LAST
  key dd07l.domvalue_l as FileUploadProcess,
      @EndUserText.label: 'Description'
      dd07t.ddtext     as Description
}
where
      dd07l.domname    = 'ZFILE_UPLOAD_PROCESS'
  and dd07t.ddlanguage = $session.system_language
