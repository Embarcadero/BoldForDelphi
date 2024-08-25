
{ Global compiler directives }
{$include bold.inc}
unit BoldStringList;

interface

uses
  Classes;

type
  { forward declarations }
  TBoldStringList = class;

  { TBoldStringList}
  TBoldStringList = class(TStringList)
  private
    fChangingValue: Boolean;
    function GetFastValue(const Name: String): String;
    procedure SetFastValue(const Name, Value: String);
    function GetValueByIndex(index: integer): string;
  protected
    procedure Put(Index: Integer; const S: string); override;
  public
    procedure Sort; override;
    procedure AddMultipleStrings(StringArr: array of string);
    procedure AssignMultipleStrings(StringArr: array of string);
    function Add(const S: string): Integer; override;
    function GetIndexOfName(const Name: String; var Index: integer): Boolean;
    function GetIndexOfprefix(const Name: String; var Index: integer): Boolean;
    procedure RemoveValue(const Name: String);
    property FastValues[const Name: String]: String read GetFastValue write SetFastValue;
    property ValueByIndex[index: integer]: string read GetValueByIndex;
  end;

implementation

uses
  BoldSharedStrings,
  SysUtils;

{ TBoldStringList }

function TBoldStringList.Add(const S: string): Integer;
begin
  result := inherited add(BoldSharedStringManager.GetSharedString(s));
end;

procedure TBoldStringList.AddMultipleStrings(StringArr: array of string);
var
  i: integer;
begin
  BeginUpdate;
  try
    for i := Low(StringArr) to High(StringArr) do
      Add(StringArr[i]);
  finally
    EndUpdate;
  end;
end;

procedure TBoldStringList.AssignMultipleStrings(StringArr: array of string);
begin
  BeginUpdate;
  try
    Clear;
    AddMultipleStrings(StringArr);
  finally
    EndUpdate;
  end;
end;

function TBoldStringList.GetFastValue(const Name: String): String;
var
  I: Integer;
begin
  if not Sorted then
    result := values[Name]
  else
  begin
    if GetIndexOfName(Name, i) then
      result := copy(get(i), length(name) + 2, maxint)
    else
      result := '';
  end;
end;

function TBoldStringList.GetIndexOfName(const Name: String; var Index: integer): Boolean;
begin
  result := GetIndexOfPrefix(name + '=', Index);
end;

function BoldAnsiLCompareShortest(Const s1, s2: string): integer;
var
  Len, Len1: integer;

begin
  Len := Length(s2);
  Len1 := Length(s1);
  if Len1 < Len then
    Len := Len1;
  Result := AnsiStrLIComp(PChar(s1), PChar(s2), Len);
end;

function TBoldStringList.GetIndexOfprefix(const Name: String; var Index: integer): Boolean;
var
  L, H, I, C: Integer;
begin
  result := false;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    c := BoldAnsiLCompareShortest(Get(i), Name);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

function TBoldStringList.GetValueByIndex(index: integer): string;
var
  s: String;
  p: integer;
begin
  s := Get(index);
  p := pos('=', s);
  if p <> 0 then
    result := copy(s, p + 1, maxint)
  else
    result := '';
end;

procedure TBoldStringList.Put(Index: Integer; const S: string);
begin
  inherited put(index, BoldSharedStringManager.GetSharedString(s));
end;

procedure TBoldStringList.RemoveValue(const Name: String);
var
  i: integer;
begin
  if GetIndexOfName(Name, i) then
    Delete(i);
end;

procedure TBoldStringList.SetFastValue(const Name, Value: String);
var
  i: integer;
begin
  if not Sorted then
    values[name] := value
  else
  begin
    if GetIndexOfname(Name, i) then
    begin
      fChangingValue := true;
      Sorted := false;
      put(i, name + '=' + value);
      sorted := true;
      fChangingvalue := false;
    end else
      Add(Name + '=' + value);
  end;
end;

procedure TBoldStringList.Sort;
begin
  if not fChangingValue then
    inherited;
end;

end.
