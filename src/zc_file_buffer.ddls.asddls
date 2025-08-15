@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Projection for file buffer'
@Metadata.allowExtensions: true
define root view entity ZC_FILE_BUFFER
  provider contract transactional_query
  as projection on ZI_FILE_BUFFER

{
  key Uuid,
      FileUploadProcess,
      FileName,
      @Semantics.largeObject:
      { mimeType: 'MimeType',
      fileName: 'FileName',
      contentDispositionPreference: #INLINE }
      Content,
      @Semantics.mimeType: true
      MimeType,
      LastChangedAt,
      LastChangedBy,
      _FileLine : redirected to composition child ZC_File_Line 
}
