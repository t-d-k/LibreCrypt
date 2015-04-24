unit pkcs11_object;

interface

uses
  pkcs11t,
  pkcs11_api,
  pkcs11_attribute;

type
  TPKCS11Object = class(TPKCS11API)
  private
    function GetSize(): integer;

  public
    FSessionHandle: CK_SESSION_HANDLE;
    FHandle: CK_OBJECT_HANDLE;

    property Size: integer read GetSize;

    // Note: *Caller* is responsible for freeing returned attribute object
    function GetAttribute(attrType: CK_ATTRIBUTE_TYPE): TPKCS11Attribute;
    function SetAttribute(attrObj: TPKCS11Attribute): boolean;
  end;

  TPKCS11ObjectArray = array of TPKCS11Object;

implementation

// Returns -1 on failure
function TPKCS11Object.GetSize(): integer;
var

  ulSize: CK_ULONG;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetObjectSize, FN_NAME_C_GetObjectSize);

  Result := -1;

  LastRV := LibraryFunctionList.CK_C_GetObjectSize(FSessionHandle, FHandle, @ulSize);
  if RVSuccess(LastRV) then  Result := ulSize;
end;

// Important: It is the CALLERS responsibility to free off the object returned
function TPKCS11Object.GetAttribute(attrType: CK_ATTRIBUTE_TYPE): TPKCS11Attribute;
const
  BUFFER_SIZE = 1024;
var
  attrStruct: CK_ATTRIBUTE;
  bufferStr: string;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetAttributeValue, FN_NAME_C_GetAttributeValue);

  Result := nil;

  // Get attribute size (if attr present)
  attrStruct.attrType := attrType;
  attrStruct.pValue := nil;
  attrStruct.ulValueLen := 0;
  LastRV := LibraryFunctionList.CK_C_GetAttributeValue(
                                                      FSessionHandle,
                                                      FHandle,
                                                      @attrStruct,
                                                      1
                                                     );
  if (
      RVSuccess(LastRV) and
      (CK_LONG(attrStruct.ulValueLen) > -1)
     ) then
    begin
    // Get attribute value
    bufferStr := StringOfChar(#0, attrStruct.ulValueLen);
    attrStruct.pValue := PChar(bufferStr);

    LastRV := LibraryFunctionList.CK_C_GetAttributeValue(
                                                        FSessionHandle,
                                                        FHandle,
                                                        @attrStruct,
                                                        1
                                                       );
    if RVSuccess(LastRV) then
      begin
      Result := TPKCS11Attribute.Create();
      Result.LibraryFunctionList := LibraryFunctionList;
      Result.AttrStruct := attrStruct;
      end;
    end;
end;


function TPKCS11Object.SetAttribute(attrObj: TPKCS11Attribute): boolean;
var
  attrStruct: CK_ATTRIBUTE;
  bufferStr: AnsiString;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_SetAttributeValue, FN_NAME_C_SetAttributeValue);

  bufferStr := attrObj.ValueAsRawData;

  attrStruct.attrType := attrObj.AttribType;
  attrStruct.pValue := PAnsiChar(bufferStr);
  attrStruct.ulValueLen := length(bufferStr);

  LastRV := LibraryFunctionList.CK_C_SetAttributeValue(
                                                      FSessionHandle,
                                                      FHandle,
                                                      @attrStruct,
                                                      1
                                                     );

  Result := RVSuccess(LastRV);
end;

END.

