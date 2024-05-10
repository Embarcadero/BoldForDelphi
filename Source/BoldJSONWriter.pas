unit BoldJSONWriter;

interface

uses
  Classes,
  SysUtils,

  BoldElements,
  BoldSystem,
  BoldValueSpaceInterfaces,
  BoldValueInterfaces,

  JSON,
  System.JSON.Readers,
  System.JSON.Writers;

type
//  TBoldJSONOptions = (jo);
// only dirty/modified
// only if diffferent than default value
//
// representation ?
// all members ?

  TBoldJSONwriter = class(TObject)
  private
    class procedure WriteBoldObjectContent(const ABoldObjectContents: IBoldObjectContents; AWriter: TJsonTextWriter);
    class procedure WriteBoldValue(const ABoldValue: IBoldValue; AWriter: TJsonTextWriter);
    class procedure WriteBoldElement(ABoldElement: TBoldElement; AWriter: TJsonTextWriter);
    class procedure WriteBoldMember(ABoldMember: TBoldMember; AWriter: TJsonTextWriter);
    class procedure WriteBoldAttribute(ABoldAttribute: TBoldAttribute; AWriter: TJsonTextWriter);
    class procedure WriteBoldList(ABoldList: TBoldList; AWriter: TJsonTextWriter);
    class procedure WriteBoldMultilink(ABoldList: TBoldObjectList; AWriter: TJsonTextWriter);
    class procedure WriteBoldSingleLink(ABoldObjectReference: TBoldObjectReference; AWriter: TJsonTextWriter);
    class procedure WriteBoldObject(ABoldObject: TBoldObject; AWriter: TJsonTextWriter);
    class procedure WriteBoldSystem(ABoldSystem: TBoldSystem; AWriter: TJsonTextWriter);
    class procedure WriteBoldType(ABoldType: TBoldElementTypeInfo; AWriter: TJsonTextWriter);
  public
    class function BoldElementToJsonString(ABoldElement : TBoldElement) : String;
    class function BoldElementToJson(ABoldElement : TBoldElement) : TJSonObject;
    class function BoldValueSpaceToJSON(const AValueSpace: IBoldValueSpace; AName: string = ''): string;
//    procedure JsonStringToValueSpace(const AJSON: string; const AValueSpace: IBoldValueSpace);
//    procedure JsonToBoldElement(AJSON: TJSONObject; ABoldElement : TBoldElement);
//    procedure JsonStringToBoldObject(const AJsonString: String; aBoldObject : TBoldObject);
//    function BoldObjectStringRepresentation(ABoldObject: TBoldObject) : String;
  end;

implementation

uses
  System.JSON.Types,
  System.TypInfo,

  BoldDefs,
  BoldID;

{ TBoldJSONwriter }

class function TBoldJSONwriter.BoldElementToJson(ABoldElement: TBoldElement): TJSonObject;
begin
  Result := nil;    // Remove warning
  assert(false, 'not implemented');
end;

class function TBoldJSONwriter.BoldElementToJsonString(
  ABoldElement: TBoldElement): String;
var
  StringWriter: TStringWriter;
  StringReader: TStringReader;
  Writer: TJsonTextWriter;
  Reader: TJsonTextReader;
begin
  StringWriter := TStringWriter.Create();
  Writer := TJsonTextWriter.Create(StringWriter, true);
  try
    Writer.Formatting := TJsonFormatting.Indented;
    WriteBoldElement(ABoldElement, Writer);
    result := StringWriter.ToString;
{    StringReader := TStringReader.Create(result);
    Reader := TJsonTextReader.Create(StringReader);
    Reader.Read;}
  finally
    Writer.free;
  end;
end;

class procedure TBoldJSONwriter.WriteBoldAttribute(ABoldAttribute: TBoldAttribute;
  AWriter: TJsonTextWriter);
begin
  if Assigned(ABoldAttribute.BoldMemberRTInfo) then
    Awriter.WritePropertyName(ABoldAttribute.BoldMemberRTInfo.ExpressionName )
  else
    Awriter.WritePropertyName(ABoldAttribute.BoldType.ExpressionName);
  Awriter.WriteValue(ABoldAttribute.AsString);
end;

class procedure TBoldJSONwriter.WriteBoldElement(ABoldElement: TBoldElement;
  AWriter: TJsonTextWriter);
begin
  case ABoldElement.BoldType.BoldValueType of
    bvtList: WriteBoldList(TBoldList(ABoldElement), AWriter);
    bvtClass: WriteBoldObject(TBoldObject(ABoldElement), AWriter);
    bvtAttr: WriteBoldAttribute(TBoldAttribute(ABoldElement), AWriter);
    bvtSystem: WriteBoldSystem(TBoldSystem(ABoldElement), AWriter);
    bvtType: WriteBoldType(TBoldElementTypeInfo(ABoldElement), AWriter);
  end;
end;

class procedure TBoldJSONwriter.WriteBoldList(ABoldList: TBoldList;
  AWriter: TJsonTextWriter);
var
  vBoldElement: TBoldElement;
begin
  AWriter.WriteStartArray;
  for vBoldElement in ABoldList do
    WriteBoldElement(vBoldElement, AWriter);
  AWriter.WriteEndArray;
end;

class procedure TBoldJSONwriter.WriteBoldMember(ABoldMember: TBoldMember;
  AWriter: TJsonTextWriter);
begin
  case ABoldmember.BoldType.BoldValueType of
    bvtAttr:
      WriteBoldAttribute(ABoldmember as TBoldAttribute, AWriter);
    bvtList:
      WriteBoldMultilink(ABoldmember as TBoldObjectList, AWriter);
    bvtClass:
      WriteBoldSingleLink(ABoldmember as TBoldObjectReference, AWriter)
  end;
end;

class procedure TBoldJSONwriter.WriteBoldObject(ABoldObject: TBoldObject;
  AWriter: TJsonTextWriter);
var
  i: integer;
begin
  if not Assigned(ABoldObject) then
    exit;
  AWriter.WriteStartObject;
  // write internals
  AWriter.WritePropertyName('Internal');
//  AWriter.WriteStartArray;
  AWriter.WriteStartObject;
  AWriter.WritePropertyName('id');
  AWriter.WriteValue(ABoldObject.BoldObjectLocator.BoldObjectID.AsString);
  AWriter.WritePropertyName('BoldPersistenceState');
  AWriter.WriteValue(System.TypInfo.GetEnumName(TypeInfo(TBoldValuePersistenceState), Ord(ABoldObject.BoldPersistenceState)));
  AWriter.WritePropertyName('BoldExistenceState');
  AWriter.WriteValue(System.TypInfo.GetEnumName(TypeInfo(TBoldExistenceState), Ord(ABoldObject.BoldExistenceState)));
  AWriter.WritePropertyName('BoldTimeStamp');
  AWriter.WriteValue(ABoldObject.BoldTimeStamp);
  AWriter.WriteEndObject;
//  AWriter.WriteEndArray;
  AWriter.WritePropertyName('Attributes');
//  AWriter.WriteStartArray;
  AWriter.WriteStartObject;
  for i := 0 to ABoldObject.BoldMemberCount -1 do
  begin
    if {ForceLoad} ABoldObject.BoldMemberAssigned[i] then
    begin
      if ABoldObject.BoldMembers[i].BoldMemberRTInfo.IsAttribute then
        WriteBoldMember(ABoldObject.BoldMembers[i], AWriter);
    end;
  end;
  AWriter.WriteEndObject;
  AWriter.WritePropertyName('Single links');
//  AWriter.WriteStartArray;
  AWriter.WriteStartObject;
  for i := 0 to ABoldObject.BoldMemberCount -1 do
  begin
    if {ForceLoad} ABoldObject.BoldMemberAssigned[i] then
    begin
      if ABoldObject.BoldMembers[i].BoldMemberRTInfo.IsSingleRole and
        TBoldObjectReference(ABoldObject.BoldMembers[i]).BoldRoleRTInfo.IsNavigable then
          WriteBoldMember(ABoldObject.BoldMembers[i], AWriter);
    end;
  end;
  AWriter.WriteEndObject;
  AWriter.WritePropertyName('Multi links');
//  AWriter.WriteStartArray;
  AWriter.WriteStartObject;
  for i := 0 to ABoldObject.BoldMemberCount -1 do
  begin
    if {ForceLoad} ABoldObject.BoldMemberAssigned[i] then
    begin
      if ABoldObject.BoldMembers[i].BoldMemberRTInfo.IsMultiRole and
        TBoldObjectList(ABoldObject.BoldMembers[i]).BoldRoleRTInfo.IsNavigable then
          WriteBoldMember(ABoldObject.BoldMembers[i], AWriter);
    end;
  end;
  AWriter.WriteEndObject;
  AWriter.WriteEndObject;
end;

class procedure TBoldJSONwriter.WriteBoldObjectContent(
  const ABoldObjectContents: IBoldObjectContents; AWriter: TJsonTextWriter);
var
  i: integer;
  BoldValue: IBoldValue;
begin
  {
  BoldObjectContents.IsModified
  BoldObjectContents.TimeStamp
  BoldObjectContents.IsReadOnly
  BoldObjectContents.ObjectId
  BoldObjectContents.BoldExistenceState
  BoldObjectContents.BoldExistenceState
  BoldObjectContents.GlobalId
  }
  for i := 0 to ABoldObjectContents.MemberCount -1 do
  begin
    BoldValue := ABoldObjectContents.ValueByIndex[i];
    if Assigned(BoldValue) then
      WriteBoldValue(BoldValue, AWriter);
  end;
end;

class procedure TBoldJSONwriter.WriteBoldSingleLink(ABoldObjectReference: TBoldObjectReference;
  AWriter: TJsonTextWriter);
begin
  AWriter.WritePropertyName(ABoldObjectReference.BoldMemberRTInfo.ExpressionName);
  if Assigned(ABoldObjectReference.Locator) then
    AWriter.WriteValue(StrToInt(ABoldObjectReference.Locator.AsString))
  else
    AWriter.WriteNull;
end;

class procedure TBoldJSONwriter.WriteBoldMultilink(ABoldList: TBoldObjectList;
  AWriter: TJsonTextWriter);
var
  vLocator : TBoldObjectLocator;
  I : Integer;
begin
  AWriter.WritePropertyName(ABoldList.BoldRoleRTInfo.ExpressionName);
  AWriter.WriteStartArray;
  for i := 0 to ABoldList.Count -1 do
    AWriter.WriteValue(StrToInt(ABoldList.Locators[i].AsString));
  AWriter.WriteEndArray;
end;

class procedure TBoldJSONwriter.WriteBoldSystem(ABoldSystem: TBoldSystem;
  AWriter: TJsonTextWriter);
var
  vLocators: TBoldLocatorListTraverser;
begin
  AWriter.WriteStartObject;
  // write internals
  AWriter.WritePropertyName('Internal');
  AWriter.WriteStartArray;

  vLocators := ABoldSystem.Locators.CreateTraverser;
  while vLocators.MoveNext do
    if Assigned(vLocators.Locator.BoldObject) {or not ALoadedOnly} then
    begin
      WriteBoldObject(vLocators.Locator.EnsuredBoldObject, AWriter); // ? or write as list ?
    end;
  AWriter.WriteEndArray;
  AWriter.WriteEndObject;
end;

class procedure TBoldJSONwriter.WriteBoldType(ABoldType: TBoldElementTypeInfo;
  AWriter: TJsonTextWriter);
begin
  AWriter.WritePropertyName('Type');
  AWriter.Writevalue(ABoldType.ExpressionName);
end;

class procedure TBoldJSONwriter.WriteBoldValue(const ABoldValue: IBoldValue;
  AWriter: TJsonTextWriter);
var
  s: IBoldStringRepresentable;
begin
  if ABoldValue.QueryInterface(IBoldStringRepresentable, S) = S_OK then
    AWriter.WriteValue(s.StringRepresentation[brJSON])
  else
  // TODO Links
end;

class function TBoldJSONwriter.BoldValueSpaceToJSON(
  const AValueSpace: IBoldValueSpace; AName: string = ''): string;
var
  IDList: TBoldObjectIDList;
  i,j: integer;
  StringWriter: TStringWriter;
  Writer: TJsonTextWriter;
  BoldObjectContents: IBoldObjectContents;
begin
  StringWriter := TStringWriter.Create();
  Writer := TJsonTextWriter.Create(StringWriter, true);
  try
    Writer.Formatting := TJsonFormatting.Indented;
    if AName <> '' then
      Writer.WritePropertyName(AName);
    AValueSpace.AllObjectIds(IDList, true);
    for i := 0 to IDList.Count-1 do
    begin
      BoldObjectContents := AValueSpace.ObjectContentsByObjectId[IDList[i]];
      if Assigned(BoldObjectContents) then
        WriteBoldObjectContent(BoldObjectContents, Writer);
    end;
    result := Writer.ToString;
  finally
    Writer.free;
  end;
end;

end.
