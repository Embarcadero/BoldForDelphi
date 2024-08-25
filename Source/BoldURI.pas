{ Global compiler directives }
{$include bold.inc}
unit BoldURI;

interface
uses
  BoldElements,
  BoldSystem;

function BoldElementURI( aBoldElement: TBoldElement): String;
function URIBoldElement(aSystem: TBoldSystem; aResourceId: string): TBoldIndirectElement;
function ResourceNameToModelName(aRequestedResource: String) : String;
function ModelNameToResourceName(aModelName: String) : String;

implementation

uses
  SysUtils,
  BoldSystemRT;



function BoldElementURI( aBoldElement: TBoldElement): String;
var
  vBoldMemberIndex : Integer;
begin
  if aBoldElement is TBoldObject then
    with aBoldElement as TBoldObject do
    begin
      Result := '/'+ ModelNameToResourceName(BoldClassTypeInfo.ModelName) + '/';
      vBoldMemberIndex := BoldMemberIndexByExpressionName['objectGUID'];
      if vBoldMemberIndex <> -1 then
        Result := Result + BoldMembers[vBoldMemberIndex].AsString
      else
        Result := Result + BoldObjectLocator.AsString;
    end;
end;

function URIBoldElement(aSystem: TBoldSystem; aResourceId: string): TBoldIndirectElement;
var
  aResourceIdInt: Integer;
  vResourceName : String;
  vResourceId : String;
  vSlashPos : Integer;
  vModelName: string;
  vOcl : String;
  vUnderlyingClass: string;
  vObject: TBoldObject;
  vUnderlyingClassTypeInfo : TBoldClassTypeInfo;
begin
  Result := TBoldIndirectElement.Create;
  vSlashPos := Pos('/',Copy(aResourceId,2,Maxint));
  vResourceName := Copy(aResourceId,2,vSlashPos-1);
  vModelName := ResourceNameToModelName(vResourceName);
  vResourceId := Copy(aResourceId,vSlashPos+2, Maxint);

  vUnderlyingClass := vModelName;
  vUnderlyingClassTypeInfo := aSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[vUnderlyingClass];

  if TryStrToInt(aResourceId,aResourceIdInt) then
    //Result.SetReferenceValue(BoldManipulator.ElementForIdString(aResourceId))
  else if (Copy(aResourceId,1,1) = '/') and (vUnderlyingClass <> '') and (vResourceId <> '') then begin
    aSystem.EvaluateExpression(vUnderlyingClass + '->AllLoadedObjects->select(objectGUID=''' + vResourceId + ''')->first', Result, False {inPS});
    if Result.Value = nil then
    begin
      vOcl := vUnderlyingClass + '.AllInstances->select(objectGUID=''' + vResourceId + ''')';
      try
        aSystem.EvaluateExpression(vOcl, Result, True {inPS});
      except
        aSystem.EvaluateExpression(vOcl, Result, False {inPS});
      end;
      If (Result.value is TBoldObjectList) and (TBoldObjectList(Result.value).Count>0) then
      begin
        vObject := TBoldObjectList(Result.Value).Elements[0] as TBoldObject;
        Result.SetReferenceValue(vObject)
      end else Result.SetReferenceValue(nil);
    end;

  end;

  if (Result.Value <> nil) and (Result.Value.BoldType is TBoldClassTypeInfo) and (not TBoldClassTypeInfo(Result.Value.BoldType).BoldIsA(vUnderlyingClassTypeInfo)) then
    Result.SetReferenceValue(nil);
end;

function ResourceNameToModelName(aRequestedResource: String) : String;
var
  vAnEnding : String;
  vUppercasedRequestedResource : String;
begin
  vAnEnding := '';
  vUppercasedRequestedResource := UpperCase(aRequestedResource);
  if Length(vUppercasedRequestedResource) > 3 then vAnEnding :=Copy(vUppercasedRequestedResource,Length(vUppercasedRequestedResource)-2,3);
  if vAnEnding = 'IES' then
    Result := Copy(vUppercasedRequestedResource,1, Length(vUppercasedRequestedResource)-3) + 'Y'
  else if vAnEnding = 'SES' then
      Result := Copy(vUppercasedRequestedResource,1, Length(vUppercasedRequestedResource)-2)
  else if Length(vUppercasedRequestedResource) > 1 then
  begin
    vAnEnding := UpperCase(Copy(vUppercasedRequestedResource,Length(vUppercasedRequestedResource),1));
    if vAnEnding = 'S' then
      Result := Copy(vUppercasedRequestedResource,1, Length(vUppercasedRequestedResource)-1)
    else
      Result := vUpperCasedRequestedResource;
  end;
end;

function ModelNameToResourceName(aModelName: String) : String;
var
  vAnEnding : String;
  vUppercasedModelName : String;
begin
  vAnEnding := '';
  vUppercasedModelName := UpperCase(aModelName);
  if Length(vUppercasedModelName) > 1 then vAnEnding :=Copy(vUppercasedModelName,Length(vUppercasedModelName),1);
  if vAnEnding = 'Y' then
    Result := Copy(aModelName,1, Length(vUppercasedModelName)-1) + 'ies'
  else if vAnEnding = 'S' then
      Result := aModelName + 'es'
  else if Length(vUppercasedModelName) > 0 then
      Result := aModelName + 's'
end;

end.
