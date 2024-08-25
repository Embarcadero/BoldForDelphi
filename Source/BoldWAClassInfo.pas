
{ Global compiler directives }
{$include bold.inc}
unit BoldWAClassInfo;

interface

uses
  classes,
  BoldWAMethodInfo;

type
 TClassInfo = class
 private
  fDelphiName: string;
  Parent: string;
  fMethods: TMethodInfoList;
 public
  constructor Create; overload;
  constructor Create(id: string); overload;
  destructor Destroy; override;
  function getMethods(const Visibility: TVisibility; var Item: TMethodInfo; var I: Integer): Boolean;
  function addMethod(Method: TMethodInfo): Integer;
  property DelphiName: string read fDelphiName write fDelphiName;
  property methods: TMethodInfoList read fMethods write fMethods;
 end;

 TClassInfoList = class
 private
  fList: TList;
  function GetValue(Index: integer): TClassInfo;
  procedure SetValue(Index: integer; Value: TClassInfo);
  function GetCount: Integer;
 public
  constructor Create;
  destructor Destroy; override;
  function Add(Item: TClassInfo): Integer;
  procedure Delete(Index: Integer);
  function First: TClassInfo;
  procedure Insert(Index: Integer; Item: TClassInfo);
  function Remove(Item: TClassInfo): Integer;
  function Last: TClassInfo;
  procedure Clear;
  property Items[Index: integer]: TClassInfo read GetValue write SetValue ; default;
  property Count: Integer read getCount;
 end;

implementation

uses
  SysUtils,
  BoldUtils;

    { TClassInfo}
constructor TClassInfo.Create;
begin
  inherited Create;
  Methods := TMethodInfoList.Create;
end;

constructor TClassInfo.Create(id: string);
begin
  inherited Create;
  Create;
  fDelphiName := id;
  Parent := 'TObject';
end;

destructor TClassInfo.Destroy;
begin
  FreeAndNil(fmethods);
  inherited destroy;
end;

function TClassInfo.getMethods(const Visibility: TVisibility;var Item: TMethodInfo; var I: Integer): Boolean;
begin
  if (i < Methods.Count) and (I >= 0) then
  begin
    Result := true;
    if ((Methods[i].Visibility = Visibility) or (Visibility = stAll))then
      Item := Methods[i]
    else
      Item := nil;
    Inc(I);
  end
  else
    Result := false;
end;

function TClassInfo.addMethod(Method: TMethodInfo): Integer;
begin
  Result := Methods.Add(method);
end;

      { TClassInfoList }
constructor TClassInfoList.Create;
begin
  inherited Create;
  fList := TList.Create;
end;

destructor TClassInfoList.Destroy;
var
  i: integer;
  aClassInfo: TClassInfo;
begin
  for i:= 0 to fList.Count - 1 do
  begin
    aClassInfo := TClassInfo(fList.Items[i]);
    FreeAndNil(aClassInfo);
  end;
  FreeAndNil(fList);
  inherited Destroy;
end;

function TClassInfoList.GetValue(Index: integer): TClassInfo;
begin
  Result := TClassInfo(fList.Items[Index]);
end;

procedure TClassInfoList.SetValue(Index: integer; Value: TClassInfo);
begin
  fList.Items[Index] := TObject(Value);
end;

function TClassInfoList.Add(Item: TClassInfo): Integer;
begin
  Result := fList.Add(Item);
end;

procedure TClassInfoList.Delete(Index: Integer);
var
  Item: TClassInfo;
begin
  Item := TClassInfo(fList[Index]);
  fList.Delete(Index);
  FreeAndNil(Item);
end;

function TClassInfoList.First: TClassInfo;
begin
  Result := TClassInfo(fList.First);
end;

procedure TClassInfoList.Insert(Index: Integer; Item: TClassInfo);
begin
  fList.Insert(Index,TObject(Item));
end;

function TClassInfoList.Remove(Item: TClassInfo): Integer;
begin
  Result := fList.Remove(TObject(Item));
end;

function TClassInfoList.Last: TClassInfo;
begin
  Result := TClassInfo(fList.Last);
end;

function TClassInfoList.GetCount: Integer;
begin
  Result := fList.Count;
end;

procedure TClassInfoList.Clear;
var
  index: integer;
begin
  for index:= 0 to Count - 1 do
    Remove(TClassInfo(fList[Index]));
end;

end.
