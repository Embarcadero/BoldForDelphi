
{ Global compiler directives }
{$include bold.inc}
unit BoldWACustomAttr;

interface

uses
  classes,
  BoldWAMethodInfo,
  dialogs,
  BoldTemplateExpander;

type
  TAccessType = (atReadOnly, atWriteOnly, atReadWrite);
  TPropertyInfo = class
    Name: string;
    pType: string;
    AccessType: TAccessType;
  public
    constructor Create(id: string; PropertyType: string; AType: TAccessType);
  end;

 TPropertyInfoList = class
 private
  fList: TList;
  function GetValue(Index: integer): TPropertyInfo;
  procedure SetValue(Index: integer; Value: TPropertyInfo);
  function GetCount: Integer;
 public
  constructor Create;
  destructor Destroy; override;
  function Add(Item: TPropertyInfo): Integer;
  procedure Delete(Index: Integer);
  function First: TPropertyInfo;
  procedure Insert(Index: Integer; Item: TPropertyInfo);
  function Remove(Item: TPropertyInfo): Integer;
  function Last: TPropertyInfo;
  procedure Clear;
  property Items[Index: integer]: TPropertyInfo read GetValue write SetValue ; default;
  property Count: Integer read getCount;
 end;

 TCustomAttribute = class
 public
   Parent: string;
   DelphiName: string;
   ExpressionName: string;
   UnitName: string;
   Properties: TPropertyInfoList;
   Methods: TMethodInfoList;
   constructor Create;
   destructor Destroy; override;
   {this generic method is used to handle the attributes that generate code for 'SPECIAL' overriden
    methods, namely the SubClassed and Custom attributes, note that the BoldTemplateHolders for
    these should have the same variable names otherwise it won't work}
   procedure AssignMethodsToTemplate(var template: TBoldTemplateHolder);
 end;

function StrToAccessType(str: string): TAccessType;

implementation

uses
  SysUtils,
  BoldUtils;

function StrToAccessType(str: string): TAccessType;
begin
  if (AnsiCompareText(str,'READONLY') = 0) then
    Result := atReadOnly
  else if (AnsiCompareText(str,'WRITEONLY') = 0) then
    Result := atWriteOnly
  else if (AnsiCompareText(str,'READ/WRITE') = 0) then
    Result := atReadWrite
  else Result := atReadOnly ;

end;

        {TPropertyInfo}
constructor TPropertyInfo.Create(id: string; PropertyType: string; AType: TAccessType);
begin
  inherited Create;
  Name := id;
  pType := PropertyType;
  AccessType := AType;
end;

        {TCustomAttribute}
constructor TCustomAttribute.Create;
begin
  inherited Create;
  Properties := TPropertyInfoList.Create;
  Methods := TMethodInfoList.Create;
  UnitName := '';
end;

destructor TCustomAttribute.Destroy;
begin
  FreeAndNil(Properties);
  FreeAndNil(Methods);
  inherited Destroy;
end;


procedure TCustomAttribute.AssignMethodsToTemplate(var Template: TBoldTemplateHolder);
var
  i: integer;
  v: TVisibility;
  vasString, Signature: string;
  aCount: array[stPrivate..stPublished] of Integer;
  Item: TMethodInfo;

  function BooleanToStr(value: Boolean): string;
  begin
    if value then
      Result := 'true'
    else
      Result := 'false';
  end;

begin
  if Methods.Count = 0 then Exit;
    for v:= stPrivate to stPublished do
      aCount[v]:= 0;
    for i:= 0 to Methods.Count - 1 do
    begin
      Item := Methods[i];
      v := Item.Visibility;
      vasString := Item.TVisibilityToStr(Item.Visibility);
      if Item.IsOverriden then
      begin
        Template.Variables.Add(Format('%s',[Item.Name]), 'true', []);
        Template.Variables.Add(Format('%s',[vasString]), 'true', []);
      end
      else
      begin
        if (Item.IsFunction) then
          Signature := Item.Params + ': ' + Item.ReturnType + ';'
        else
          Signature := Item.Params + ';';
        Template.Variables.Add(format('%sMETHODTYPE.%d',[vAsString,aCount[v]]), Item.methodTypeAsString, []);
        Template.Variables.Add(format('%sMETHODNAME.%d',[vAsString,aCount[v]]), Item.Name, []);
        Template.Variables.Add(format('%sMETHODSIGNATURE.%d',[vAsString,aCount[v]]), Signature , []);
        if mdOverride in Methods[i].mDirectives then
          Template.Variables.Add(format('%sMETHODDIRECTIVES.%d', [vAsString,aCount[v]]), ' override;', []);
        Template.Variables.Add(Format('%s',[vasString]), 'true', []);
        Inc(aCount[v]);
      end;
    end;
    for v:= stPrivate to stPublished do
    begin
      Template.Variables.Add(TMethodInfo.TVisibilityToStr(v)+'METHODCOUNT', IntToStr(aCount[v]), []);
      Template.Variables.Add(TMethodInfo.TVisibilityToStr(v), BooleanToStr(aCount[v] <> 0), []);
    end;
end;

      { TPropertyInfoList }
constructor TPropertyInfoList.Create;
begin
  inherited Create;
  fList := TList.Create;
end;

destructor TPropertyInfoList.Destroy;
var
  i: integer;
  aPropertyInfo: TPropertyInfo;
begin
  for i:= 0 to fList.Count - 1 do
  begin
    aPropertyInfo := TPropertyInfo(fList.Items[i]);
    FreeAndNil(aPropertyInfo);
  end;
  FreeAndNil(fList);
  inherited Destroy;
end;

function TPropertyInfoList.GetValue(Index: integer): TPropertyInfo;
begin
  Result := TPropertyInfo(fList.Items[Index]);
end;

procedure TPropertyInfoList.SetValue(Index: integer; Value: TPropertyInfo);
begin
  fList.Items[Index] := TObject(Value);
end;

function TPropertyInfoList.Add(Item: TPropertyInfo): Integer;
begin
  Result := fList.Add(Item);
end;

procedure TPropertyInfoList.Delete(Index: Integer);
var
  Item: TPropertyInfo;
begin
  Item := TPropertyInfo(fList[Index]);
  fList.Delete(Index);
  FreeAndNil(Item);
end;

function TPropertyInfoList.First: TPropertyInfo;
begin
  Result := TPropertyInfo(fList.First);
end;

procedure TPropertyInfoList.Insert(Index: Integer; Item: TPropertyInfo);
begin
  fList.Insert(Index,TObject(Item));
end;

function TPropertyInfoList.Remove(Item: TPropertyInfo): Integer;
begin
  Result := fList.Remove(TObject(Item));
end;

function TPropertyInfoList.Last: TPropertyInfo;
begin
  Result := TPropertyInfo(fList.Last);
end;

function TPropertyInfoList.GetCount: Integer;
begin
  Result := fList.Count;
end;

procedure TPropertyInfoList.Clear;
var
  index: integer;
begin
  for index:= 0 to Count - 1 do
    Remove(TPropertyInfo(fList[Index]));
end;

end.
