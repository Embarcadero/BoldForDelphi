
{ Global compiler directives }
{$include bold.inc}
unit BoldComServerElementHandleFactory;

interface

uses
  Classes,
  BoldBase,
  BoldComServer;

type
  { forward declarations }
  TBoldComServerElementHandleFactory = class;

  {-- TBoldComServerElementHandleFactory --}
  TBoldComServerElementHandleFactory = class(TBoldNonRefCountedObject,IBoldComServerObjectProvider)
  private
    FCookie: Integer;
  protected
    { IBoldComServerObjectProvider }
    function CreateObject(const ClassId: TGUID; const ClassName: string): IUnknown;
    function GetObject(const ClassId: TGUID; const ObjectName: string): IUnknown;
    procedure GetObjectInfo(const ClassId: TGUID; ObjectNames, ClassNames: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  BoldComAdapter,
  BoldCursorHandle,
  BoldDerivedHandle,
  BoldExpressionHandle,
  BoldListHandle,
  BoldReferenceHandle,
  BoldSQlHandle,
  BoldVariableHandle;

var
  G_ServerElementHandleFactory: TBoldComServerElementHandleFactory = nil;

{-- TBoldComServerElementHandleFactory ----------------------------------------}

constructor TBoldComServerElementHandleFactory.Create;
begin
  inherited Create;
  FCookie := TBoldComServer.Instance.RegisterObjectProvider(Self);
end;

destructor TBoldComServerElementHandleFactory.Destroy;
begin
  TBoldComServer.Instance.RevokeObjectProvider(self, FCookie);
  inherited;
end;

function TBoldComServerElementHandleFactory.CreateObject(
  const ClassId: TGUID; const ClassName: string): IUnknown;
var
  Adapter: TBoldComAdapter;
  Adaptee: TBoldAdaptableObject;
begin
  Result := nil;
  if CompareText(ClassName, TBoldCursorHandle.ClassName) = 0 then
    Adaptee := TBoldCursorHandle.Create(nil)
  else if CompareText(ClassName, TBoldDerivedHandle.ClassName) = 0 then
    Adaptee := TBoldDerivedHandle.Create(nil)
  else if CompareText(ClassName, TBoldExpressionHandle.ClassName) = 0 then
    Adaptee := TBoldExpressionHandle.Create(nil)
  else if CompareText(ClassName, TBoldListHandle.ClassName) = 0 then
    Adaptee := TBoldListHandle.Create(nil)
  else if CompareText(ClassName, TBoldReferenceHandle.ClassName) = 0 then
    Adaptee := TBoldReferenceHandle.Create(nil)
  else if CompareText(ClassName, TBoldSQLHandle.ClassName) = 0 then
    Adaptee := TBoldSQLHandle.Create(nil)
  else if CompareText(ClassName, TBoldVariableHandle.ClassName) = 0 then
    Adaptee := TBoldVariableHandle.Create(nil)
  else
    Adaptee := nil;

  if Assigned(Adaptee) then
  begin
    Adapter := TBoldComAdapterFactory.Instance.CreateAdapterForObject(Adaptee, True);
    if not Assigned(Adapter) then
      FreeAndNil(Adaptee)
    else
      Result := Adapter;
  end;
end;

function TBoldComServerElementHandleFactory.GetObject(const ClassId: TGUID;
  const ObjectName: string): IUnknown;
begin
  Result := nil;
end;

procedure TBoldComServerElementHandleFactory.GetObjectInfo(
  const ClassId: TGUID; ObjectNames, ClassNames: TStrings);
begin
  ClassNames.Add(TBoldCursorHandle.ClassName);
  ClassNames.Add(TBoldDerivedHandle.ClassName);
  ClassNames.Add(TBoldExpressionHandle.ClassName);
  ClassNames.Add(TBoldListHandle.ClassName);
  ClassNames.Add(TBoldReferenceHandle.ClassName);
  ClassNames.Add(TBoldSQLHandle.ClassName);
  ClassNames.Add(TBoldVariableHandle.ClassName);
end;

initialization
  G_ServerElementHandleFactory := TBoldComServerElementHandleFactory.Create;

finalization
  FreeAndNil(G_ServerElementHandleFactory);  

end.
