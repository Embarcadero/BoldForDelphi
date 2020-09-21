unit BoldWAMethodInfo;

interface

uses
  classes;

type
 TVisibility = (stPrivate,stProtected,stPublic, stPublished, stAll);
 TListAction = (laAdd, laDelete, laInsert);
 TMethodType = (mtProcedure, mtFunction, mtConstructor, mtDestructor, mtNone);
 TMethodDirective = (mdVirtual, mdAbstract, mdDynamic, mdOverride, mdOverload, mdReintroduce);
 TMethodDirectiveSet = Set of TMethodDirective;
 TListChangeEvent = procedure(Action: TListAction; Index: Integer) of object;

 TMethodInfo = class
 public
  fClass: Pointer;
  methodType: TMethodType;
  Name,
  Params,
  ReturnType: string;
  Visibility: TVisibility;
  mDirectives: TMethodDirectiveSet;
  constructor Create(mType: TMethodType; id, mParams, mReturnType: string; vis: TVisibility; mClass: Pointer);overload;
  function IsFunction: Boolean;
  function IsOverriden: Boolean;
  procedure Assign(amethodType: TMethodType; amethodName, aParams, aReturnType: string;
              aVisibility: TVisibility; amDirectives: TMethodDirectiveSet);
  function MethodTypeAsString: string;
  function mDirectivesAsString: string;
  class function StrToMethodType(str: string): TMethodType;
  class function TVisibilityToStr(v: TVisibility): string;
  class function StrToVisibility(v: string): TVisibility;
 end;

  TMethodInfoList = class
  private
   fList: TList;
   FOnChange: TListChangeEvent;
   function GetValue(Index: integer): TMethodInfo;
   procedure SetValue(Index: integer; Value: TMethodInfo);
   function GetCount: Integer;
  public
   constructor Create;
   destructor Destroy; override;
   function Add(Item: TMethodInfo): Integer;
   procedure Delete(Index: Integer);
   function First: TMethodInfo;
   procedure Insert(Index: Integer; Item: TMethodInfo);
   function Remove(Item: TMethodInfo): Integer;
   function Last: TMethodInfo;
   procedure Clear;
   property Items[Index: integer]: TMethodInfo read GetValue write SetValue ; default;
   property Count: Integer read getCount;
   property OnChange: TListChangeEvent read FOnChange write FOnChange;
  end;

  function VirtualMethods: TStringList;
  function MethodIsOverriden(const MethodName: string): Boolean;


implementation

uses
  SysUtils,
  BoldUtils;

const
  C_VirtualMethods ='GetStringRepresentation' + #13+#10 +
                    'SetStringRepresentation' + #13+#10 +
                    'ValidateString' + #13+#10 +
                    'ValidateCharacter' + #13+#10 +
                    'GetAsVariant' + #13+#10 +
                    'SetAsVariant' + #13+#10 +
                    'Assign' + #13+#10 +
                    'AssignContentValue' + #13+#10 +
                    'AssignValue';

var
  G_VirtualMethods: TStringList;

function VirtualMethods: TStringList;
begin
  if not Assigned(G_VirtualMethods) then
  begin
    G_VirtualMethods := TStringList.Create;
    G_VirtualMethods.Text := C_VirtualMethods;
  end;
  Result := G_VirtualMethods;
end;

function MethodIsOverriden(const MethodName: string): Boolean;
begin
  Result := VirtualMethods.IndexOf(MethodName) <> -1 ;
end;

    { TMethodInfo}
constructor TMethodInfo.Create(mType: TMethodType; id, mParams, mReturnType: string; vis: TVisibility; mClass: Pointer);begin
  inherited Create;
  Name := id;
  methodType := mType;
  Params := mParams;
  ReturnType := mReturnType;
  Visibility := vis;
  fClass:= mClass;
end;

function TMethodInfo.IsFunction: Boolean;
begin
  Result := (self.methodType = mtFunction);
end;

procedure TMethodInfo.Assign(amethodType: TMethodType; amethodName, aParams, aReturnType: string;
              aVisibility: TVisibility; amDirectives: TMethodDirectiveSet);
begin
  methodType := aMethodType;
  Name := aMethodName;
  Params := Trim(aParams) ;
  if (length(Params) > 0) then
  begin
    if (Params[1] <> '(') then
      Params := Format('(%s',[Params]);
    if (Params[length(Params)] <> ')') then
      Params := Format('%s)',[Params]);
  end;
  ReturnType := aReturnType;
  Visibility := aVisibility;
  mDirectives := amDirectives;
end;

function TMethodInfo.MethodTypeAsString: string;
begin
  case methodType of
    mtProcedure: Result := 'procedure'; // do not localize
    mtFunction: Result := 'function'; // do not localize
    mtConstructor: Result := 'constructor'; // do not localize
    mtDestructor: Result := 'destructor'; // do not localize
    else Result := '';
  end;
end;

function TMethodInfo.mDirectivesAsString: string;
begin
  Result := '';
  if mdVirtual in mDirectives then
    Result := Result + 'virtual; '; // do not localize
  if mdAbstract in mDirectives then
    Result := Result + 'abstract; '; // do not localize
  if mdDynamic in mDirectives then
    Result := Result + 'dynamic; '; // do not localize
  if mdOverride in mDirectives then
    Result := Result + 'override; '; // do not localize
  if mdOverload in mDirectives then
    Result := Result + 'overload; '; // do not localize
  if mdReintroduce in mDirectives then
    Result := Result + 'reintroduce; '; // do not localize
end;

function TMethodInfo.IsOverriden: Boolean;
begin
  Result := MethodIsOverriden(Name);
end;

class function TMethodInfo.StrToMethodType(str: string): TMethodType;
begin
  if  (UpperCase(str) = UpperCase('procedure'))then // do not localize
    Result := mtProcedure
  else if  (UpperCase(str) = UpperCase('function')) then // do not localize
    Result := mtFunction
  else if  (UpperCase(str) = UpperCase('constructor')) then // do not localize
    Result := mtConstructor
  else if (UpperCase(str) = UpperCase('destructor')) then // do not localize
    Result := mtDestructor
  else
    Result := mtNone;
end;

class function TMethodInfo.TVisibilityToStr(v: TVisibility): string;
begin
  case v of
    stPublic: Result := 'public'; // do not localize
    stPrivate: Result := 'private'; // do not localize
    stPublished: Result := 'published'; // do not localize
    stProtected: Result := 'protected'; // do not localize
    else raise Exception.Create('Error: Visiblity of method not specified');
  end;
end;

class function TMethodInfo.StrToVisibility(v: string): TVisibility;
begin
  if (Trim(v) = 'private') then // do not localize
    Result := stprivate
  else if (Trim(v) = 'protected') then // do not localize
    Result := stprotected
  else if (Trim(v) = 'public') then // do not localize
    Result := stpublic
  else if (Trim(v) = 'published') then // do not localize
    Result := stpublished
  else raise Exception.Create('TVisibility: Error converting string');
end;

    { TMethodInfoList }
constructor TMethodInfoList.Create;
begin
  inherited Create;
  fList := TList.Create;
end;

destructor TMethodInfoList.Destroy;
var
  i: integer;
begin
  for i:= 0 to fList.Count - 1 do
  begin
    TMethodInfo(fList.Items[i]).Free;
  end;
  FreeAndNil(fList);
  inherited Destroy;
end;

function TMethodInfoList.GetValue(Index: integer): TMethodInfo;
begin
  Result := TMethodInfo(fList.Items[Index]);
end;

procedure TMethodInfoList.SetValue(Index: integer; Value: TMethodInfo);
begin
  fList.Items[Index] := TObject(Value);
end;

function TMethodInfoList.Add(Item: TMethodInfo): Integer;
begin
  Result := fList.Add(Item);
  if Assigned(FOnChange) then
    FOnChange(laAdd, Result);
end;

procedure TMethodInfoList.Delete(Index: Integer);
var
  Item: TMethodInfo;
begin
  Item := TMethodInfo(fList[Index]);
  fList.Delete(Index);
  FreeAndNil(Item);
  if Assigned(FOnChange) then
    FOnChange(laDelete, Index);
end;

function TMethodInfoList.First: TMethodInfo;
begin
  Result := TMethodInfo(fList.First);
end;

procedure TMethodInfoList.Insert(Index: Integer; Item: TMethodInfo);
begin
  fList.Insert(Index,TObject(Item));
  if Assigned(FOnChange) then
    FOnChange(laInsert, Index);
end;

function TMethodInfoList.Remove(Item: TMethodInfo): Integer;
begin
  Result := fList.Remove(TObject(Item));
  if Assigned(FOnChange) then
    FOnChange(laDelete, Result);
end;

function TMethodInfoList.Last: TMethodInfo;
begin
  Result := TMethodInfo(fList.Last);
end;

function TMethodInfoList.GetCount: Integer;
begin
  Result := fList.Count;
end;

procedure TMethodInfoList.Clear;
var
  index: integer;
begin
  for index:= 0 to Count - 1 do
    Remove(TMethodInfo(fList[Index]));
end;

initialization

finalization
  FreeAndNil(G_VirtualMethods);
end.
