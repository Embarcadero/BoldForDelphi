{ Global compiler directives }
{$include bold.inc}
unit BoldNamedValueList;

interface

uses
  Classes,
  BoldBase,
  BoldIndexableList,
  BoldIndex,
  BoldHashIndexes;

type
  TBoldNamedValueListEntry = class(TBoldMemoryManagedObject)
  private
    fName: string;
    fValue: string;
    fObject: TObject;
  public
    constructor Create(Name, Value: string;  aObject: TObject);
    property Name: string read fName;
    property Value: string read fValue write fValue;
    property aObject: TObject read fObject write fObject;
  end;

  TBoldNamedValueList = class(TBoldIndexableList)
  private
    class var IX_Name: integer;
    function GetItem(index: Integer): TBoldNamedValueListEntry;
    function GetItemByName(const Name: string): TBoldNamedValueListEntry;
    procedure SetCommaText(const CommaText: string);
    function GetCommaText: string;
    function GetValueByname(const Name: string): string;
    procedure SetValueByname(const Name, NewValue: string);
    function GetObjectByName(const Name: string): TObject;
    procedure SetObjectByName(const Name: string; const NewObject: TObject);
  public
    constructor Create;
    procedure AddFromStrings(strings: TStrings);
    procedure AddToStrings(strings: TStrings);
    function AddEntry(const Name, Value: string; aObject: TObject = nil): TBoldNamedValueListEntry;
    procedure RemoveName(const Name: String);
    property Items[index: Integer]: TBoldNamedValueListEntry read GetItem; default;
    property ItemByName[const Name: string]: TBoldNamedValueListEntry read GetItemByName;
    property CommaText: string read GetCommaText write SetCommaText;

    property ValueByName[const Name: string]:string read GetValueByname write SetValueByName;
    property ObjectByName[const Name: string]:TObject read GetObjectByName write SetObjectByName;
  end;

implementation

type
  TNameIndex = class(TBoldCaseSensitiveStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

function TNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldNamedValueListEntry(Item).Name;
end;

{ TBoldStringDictionary }

function TBoldNamedValueList.AddEntry(const Name, Value: string; aObject: TObject): TBoldNamedValueListEntry;
begin
  result := TBoldNamedValueListEntry.Create(Name, Value, aObject);
  Add(result);
end;

procedure TBoldNamedValueList.AddFromStrings(strings: TStrings);
var
  i: integer;
begin
  for i := 0 to strings.count-1 do
    AddEntry(strings.Names[i], Copy(strings[i], Length(strings.Names[i])+2, maxint), strings.Objects[i]);
end;

procedure TBoldNamedValueList.AddToStrings(strings: TStrings);
var
  i: integer;
begin
  for i := 0 to count-1 do
    with Items[i] do
      strings.AddObject(Name + '=' + Value, aObject);
end;

constructor TBoldNamedValueList.Create;
begin
  inherited;
  SetIndexCapacity(2);
  SetIndexVariable(IX_Name, AddIndex(TNameIndex.Create));
  OwnsEntries := True;
end;

function TBoldNamedValueList.GetCommaText: string;
var
  StringList: TStringList;
begin
  Stringlist := TStringList.Create;
  try
    AddToStrings(StringList);
    Result := StringList.CommaText;
  finally  
    Stringlist.Free;
  end;
end;

function TBoldNamedValueList.GetItem(
  index: Integer): TBoldNamedValueListEntry;
begin
  Result := TBoldNamedValueListEntry(inherited Items[index]);
end;

function TBoldNamedValueList.GetItemByName(
  const Name: string): TBoldNamedValueListEntry;
begin
  Result := TBoldNamedValueListEntry(TBoldCaseSensitiveStringHashIndex(Indexes[IX_Name]).FindByString(Name));
end;


function TBoldNamedValueList.GetObjectByName(const Name: string): TObject;
var
  Entry: TBoldNamedValueListEntry;
begin
  Entry := ItemByName[Name];
  if Assigned(Entry) then
    result := Entry.aObject
  else
    Result := nil;
end;


function TBoldNamedValueList.GetValueByName(const Name: string): string;
var
  Entry: TBoldNamedValueListEntry;
begin
  Entry := ItemByName[Name];
  if Assigned(Entry) then
    result := Entry.Value
  else
    Result := '';
end;

procedure TBoldNamedValueList.RemoveName(const Name: String);
var
  item: TBoldNamedValueListEntry;
begin
  Item := ItemByName[Name];
  if assigned(item) then
    Remove(item);
end;

procedure TBoldNamedValueList.SetCommaText(const CommaText: string);
var
  StringList: TStringList;
begin
  Stringlist := TStringList.Create;
  try
    StringList.CommaText := CommaText;
    AddFromStrings(StringList);
  finally
    Stringlist.Free;
  end;
end;

{ TBoldNamedValueListEntry }

constructor TBoldNamedValueListEntry.Create(Name, Value: string;
  aObject: TObject);
begin
  fName := Name;
  fValue := Value;
  fObject := aObject;
end;

procedure TBoldNamedValueList.SetObjectByName(const Name: string;
  const NewObject: TObject);
var
  Entry: TBoldNamedValueListEntry;
begin
  Entry := ItemByName[Name];
  if Assigned(Entry) then
    Entry.aObject := NewObject
  else
    AddEntry(Name, '', NewObject);
end;

procedure TBoldNamedValueList.SetValueByname(const Name, NewValue: string);
var
  Entry: TBoldNamedValueListEntry;
begin
  Entry := ItemByName[Name];
  if Assigned(Entry) then
    Entry.Value := NewValue
  else
    AddEntry(Name, NewValue);
end;

initialization
  TBoldNamedValueList.IX_Name := -1;

end.