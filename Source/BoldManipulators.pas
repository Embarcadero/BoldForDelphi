
{ Global compiler directives }
{$include bold.inc}
unit BoldManipulators;

interface

uses
  BoldDefs,
  BoldElements,
  BoldHandles,
  Classes;

type
  { forward declarations }
  TBoldManipulator = class;
  TBoldManipulatorMapper = class;
  TBoldManipulatorMapperCollection = class;

  TBoldIdStringRepresentation = (isrVerbose, isrTerse);
  TBoldStringStringFunction = function (const s: string): string of object;
  TBoldManipulatorGetter = function(Element: TBoldElement): string of object;
  TBoldManipulatorSetter = procedure(Element: TBoldElement; const NewValue: string) of object;

  { TBoldManipulator }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldManipulator = class(TBoldSubscribableComponentViaBoldElem)
  private
    fIdStringRepresentation: TBoldIdStringRepresentation;
    fBoldSystemHandle: TBoldAbstractSystemhandle;
    fOnEncrypt: TBoldStringStringFunction;
    fOnDecrypt: TBoldStringStringFunction;
    FMappers: TBoldManipulatorMapperCollection;
    function RawIdStringForElement(Element: TBoldElement): string;
    function ElementForRawIdString(IdString: string): TBoldElement;
    function AddMapping(const RawIdString: string; const Mapping: string): string;
    function GetMapping(const IdString: string): string;
    function StripMapping(const IdString: string): string;
    procedure SetMappers(const Value: TBoldManipulatorMapperCollection);
    procedure SetBoldSystemHandle(const Value: TBoldAbstractSystemhandle);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetValueAndId(Element: TBoldElement; var IdString: string; const Mapping: string=''): string;
    function DefaultTagForElement(Element: TBoldElement): string;
    procedure SetValue(IdString: string; const NewValue: string);
    function ElementForIdString(IdString: string): TBoldElement;
    function IdStringForElement(Element: TBoldElement; const Mapping: string = ''): string;
    procedure DeleteObject(IdString: string);
    function CreateObject(Classname: string): string;
    procedure SetFromList(IdValuePairs: TStrings);     
  published
    property IdStringRepresentation:TBoldIdStringRepresentation read fIdStringRepresentation write fIdStringRepresentation;
    property BoldSystemHandle: TBoldAbstractSystemhandle read fBoldSystemHandle write SetBoldSystemHandle;
    property OnEncrypt: TBoldStringStringFunction read fOnEncrypt write fOnEncrypt;
    property OnDecrypt: TBoldStringStringFunction read fOnDecrypt write fOnDecrypt;
    property Mappers: TBoldManipulatorMapperCollection read FMappers write SetMappers;
  end;

  { TBoldManipulatorMapper }
  TBoldManipulatorMapper = class(TCollectionItem)
  private
    fOnGet: TBoldManipulatorGetter;
    fOnset: TBoldManipulatorSetter;
    fMappingName: string;
  protected
    function GetDisplayName: string; override;
  public
    function GetAsString(Element: TBoldElement): string;
    procedure SetFromString(Element: TBoldElement; NewValue: string);
  published
    property MappingName: string read fMappingName write fMappingName;
    property OnGet: TBoldManipulatorGetter read fOnGet write fOnGet;
    property OnSet: TBoldManipulatorSetter read fOnset write fOnSet;
  end;

  { TBoldManipulatorMapperCollection }
  TBoldManipulatorMapperCollection = class(TCollection)
  private
    fOwner: TComponent;
  private
    function GetItems(Index: integer): TBoldManipulatorMapper;
    function GetItemByname(Name: string): TBoldManipulatorMapper;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TComponent);
    property Items[Index: integer]: TBoldManipulatorMapper read GetItems; default;
    property ItemByname[Name: string]: TBoldManipulatorMapper read GetItemByname;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldId,
  BoldDefaultId,
  BoldSystem,
  BoldCoreConsts;

{ TBoldManipulator }

function TBoldManipulator.AddMapping(const RawIdString, Mapping: string): string;
begin
  if Mapping <> '' then
    Result := Format('%s[%s]', [RawIdString, Mapping])
  else
    Result := RawIdString;
end;

function TBoldManipulator.ElementForIdString(IdString: string): TBoldElement;
begin
  if Assigned(fOnDecrypt) and (IdString <> '') then
    IdString := OnDecrypt(IdString);
  IdString := StripMapping(IdString);
  result := ElementForRawIdString(IdString);
end;

function TBoldManipulator.ElementForRawIdString(IdString: string): TBoldElement;
var
  BoldObject: TBoldObject;
  ObjectIdStr: string;
  ColonPos: integer;
  ObjectId: TBoldObjectID;
begin
  if Trim(IdString) = '' then
  begin
    Result := nil;
    exit;
  end;
  if IdStringRepresentation = isrVerbose then
    IdString := Copy(IdString, Pos('.', IdString) + 1, MAXINT);
  ColonPos := Pos(':', IdString);
  if ColonPos = 0 then
    ObjectIdStr := IdString
  else
    ObjectIdStr := Copy(Idstring, 1, ColonPos - 1);
  ObjectId := nil;
  try
    if ObjectIdStr[1] = 'i' then
      ObjectId := TBoldInternalObjectId.CreateWithClassIDandInternalId(StrToInt(copy(ObjectIdStr, 2, maxint)), 0, false)
    else
    begin
      ObjectID := TBoldDefaultId.CreateWithClassID(0, false);
      (ObjectID as TBoldDefaultId).AsInteger := StrToInt(ObjectIdStr);
    end;
    BoldObject := BoldSystemHandle.System.EnsuredLocatorByID[ObjectID].EnsuredBoldObject;
    if ColonPos = 0 then
      result := BoldObject
    else if IdStringRepresentation = isrVerbose then
      Result := BoldObject.BoldMemberByExpressionName[Copy(IdString, Colonpos + 1, MAXINT)]
    else
      result := BoldObject.Boldmembers[StrToInt(Copy(IdString, Colonpos + 1, MAXINT))];
  finally
    FreeAndNil(ObjectId);
  end;
end;

function TBoldManipulator.GetMapping(
  const IdString: string): string;
begin
  if IdString[Length(IdString)] = ']' then
  begin
    Result := Copy(idstring, Pos('[', IdString) + 1, MAXINT);
    Result := Copy(Result, 1, Length(Result) - 1);
  end
end;

function TBoldManipulator.GetValueAndId(Element: TBoldElement;
  var IdString: string; const Mapping: string): string;
begin
  IdString := RawIdStringForElement(Element);
  if Mapping = '' then
  begin
    if Assigned(Element) then
      Result := Element.AsString
    else
      Result := '';
  end
  else
  begin
    IdString := AddMapping(IdString, Mapping);
    Result := Mappers.ItemByname[Mapping].GetAsString(Element);
  end;
  if Assigned(fOnEncrypt) and (IdString <> '') then
    IdString := OnEncrypt(IdString);
end;

function TBoldManipulator.RawIdStringForElement(Element: TBoldElement): string;
begin
  if Element is TBoldObject then
  begin
    if IdStringRepresentation = isrVerbose then
      Result := TBoldObject(Element).BoldClassTypeInfo.ExpressionName + '.';
    if not TBoldObject(Element).BoldObjectLocator.BoldObjectID.IsStorable then
      result := result + 'i';
    Result := Result + TBoldObject(Element).BoldObjectLocator.BoldObjectID.AsString;
  end
  else if (Element is TBoldMember) and Assigned(TBoldMember(Element).OwningObject) then
  begin
     Result := RawIdStringForElement(TBoldMember(Element).OwningObject) + ':';
     if IdStringRepresentation = isrVerbose then
       Result := Result +  TBoldMember(Element).BoldMemberRTInfo.ExpressionName
     else
       Result := Result + IntToStr(TBoldMember(Element).BoldMemberRTInfo.Index);
  end
end;

procedure TBoldManipulator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (aComponent = BoldSystemHandle) and (Operation = opRemove) then
    fBoldSystemHandle := nil;
end;

procedure TBoldManipulator.SetBoldSystemHandle(const Value: TBoldAbstractSystemhandle);
begin
  fBoldSystemHandle := Value;
  if Assigned(fBoldSystemHandle) then
    fBoldSystemHandle.FreeNotification(Self);
end;

procedure TBoldManipulator.SetMappers(const Value: TBoldManipulatorMapperCollection);
begin
  FMappers.Assign(Value);
end;

procedure TBoldManipulator.SetValue(IdString: string; const  NewValue: string);
  procedure SetObjectReference(r: TBoldObjectReference; NewId: string);
  var
    NewElement: TBoldELement;
  begin
    if Assigned(fOnDecrypt) and (NewId <> '') then
      NewId := OnDecrypt(NewId);
    NewElement := ElementForRawIdString(StripMapping(NewId));
    if (NewElement is TBoldObject) then
      r.BoldObject := TBoldObject(NewElement);  // FIXME error handling
  end;

var
  Element: TBoldElement;
  Mapping: string;
begin
  if Assigned(fOnDecrypt) and (IdString <> '') then
    IdString := OnDecrypt(IdString);
  Element := ElementForRawIdString(StripMapping(IdString));
  Mapping := GetMapping(IdString);
  if Mapping = '' then
  begin
    if Assigned(Element) then
    begin
      if (Element is TBoldAttribute) then
        Element.AsString := NewValue
      else if (Element is TBoldObjectReference) then
        SetObjectReference(TBoldObjectReference(Element), NewValue)
      else if (Element is TBoldObjectList) then
        raise EBold.Create(sCannotSetMultiLinkFromValue)
      else if (Element is TBoldObject) then
        raise EBold.Create(sCannotSetObjectFromValue);
    end;
  end
  else
    Mappers.ItemByname[Mapping].SetFromString(Element, NewValue);  // FIXME error handling
end;

function TBoldManipulator.StripMapping(const IdString: string): string;
begin
  if (IdString <> '') and (IdString[Length(IdString)] = ']') then
    Result := Copy(IdString, 1, Pos('[', IdString) - 1)
  else
    Result := IdString;
end;

function TBoldManipulator.IdStringForElement(Element: TBoldElement;
  const Mapping: string): string;
begin
  Result := RawIdStringForElement(Element);
  if Mapping <> '' then
    Result := AddMapping(Result, Mapping);
  if Assigned(fOnEncrypt) and (Result <> '') then
    Result := OnEncrypt(Result);
end;

constructor TBoldManipulator.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FMappers := TBoldManipulatorMapperCollection.Create(Self);
end;

destructor TBoldManipulator.Destroy;
begin
  FreeAndNil(fMappers);
  inherited;
end;

function TBoldManipulator.CreateObject(Classname: string): string;
begin
  if Assigned(BoldSystemhandle) then
    Result := IdStringForElement(BoldSystemHandle.System.CreateNewObjectByExpressionName(ClassName))
  else
    raise EBold.CreateFmt(sNoSystemHandle, [classname, 'CreateObject', Name]); // do not localize
end;

procedure TBoldManipulator.DeleteObject(IdString: string);
var
  Element: TBoldElement;
begin
  Element := ElementForIdString(IdString);
  if Assigned(Element) and (Element is TBoldObject) then
    TBoldObject(Element).Delete;
end;

procedure TBoldManipulator.SetFromList(IdValuePairs: TStrings);
var
  i: integer;
  CurrentRow, Id, NewValue: string;
  EqualPos: integer;
begin
  for i := 0 to IdValuePairs.Count - 1 do
  begin
    CurrentRow := IdValuePairs[i];
    EqualPos := Pos('=', CurrentRow);
    Id := Copy(Currentrow, 0, EqualPos - 1);
    NewValue := Copy(CurrentRow, EqualPos + 1, MAXINT);
    SetValue(Id, NewValue);
   end;
end;

function TBoldManipulator.DefaultTagForElement(Element: TBoldElement): string;
begin
  if Assigned(Element) and
     (Element is TBoldMember) and
     Assigned(TBoldMember(Element).BoldMemberRTInfo) then
    Result := TBoldMember(Element).BoldMemberRTInfo.ExpressionName
  else if Assigned(Element.BoldType) then
    Result := Element.BoldType.ExpressionName
  else
    raise EBold.CreateFmt(sElementLacksTypeInfo, [ClassName]);
end;

{ TBoldManipulatorMapper }

function TBoldManipulatorMapper.GetAsString(
  Element: TBoldElement): string;
begin
  if Assigned(fOnGet) then
    Result := OnGet(Element)
end;

function TBoldManipulatorMapper.GetDisplayName: string;
begin
  Result := MappingName;
end;

procedure TBoldManipulatorMapper.SetFromString(Element: TBoldElement; NewValue: string);
begin
  if Assigned(fOnSet) then
    OnSet(Element, NewValue)
end;

{ TBoldManipulatorMapperCollection }

constructor TBoldManipulatorMapperCollection.Create(Owner: TComponent);
begin
  inherited Create(TBoldManipulatorMapper);
  fOwner := Owner;
end;

function TBoldManipulatorMapperCollection.GetItemByname(
  Name: string): TBoldManipulatorMapper;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].MappingName = Name then
    begin
      result := Items[i];
      Exit;
    end;
  raise EBold.CreateFmt(sUnknownMapping, [Name]);
end;

function TBoldManipulatorMapperCollection.GetItems(Index: integer): TBoldManipulatorMapper;
begin
  Result := TBoldManipulatorMapper(inherited items[index]);
end;

function TBoldManipulatorMapperCollection.GetOwner: TPersistent;
begin
  result := fOwner;
end;

end.
