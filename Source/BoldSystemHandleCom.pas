
{ Global compiler directives }
{$include bold.inc}
unit BoldSystemHandleCom;

interface

uses
  BoldHandlesCom;

type
  { forward declarations }
  TBoldSystemHandleCom = class;

  {-- TBoldSystemHandleCom --}
  TBoldSystemHandleCom = class(TBoldAbstractSystemHandleCom)
  private
    FPersistent: Boolean;
    function GetPersistent: Boolean;
  protected
    function ServerHandleClassName: string; override;

    procedure ClearAllValues; override;
    procedure ValuesFromServer; override;
    procedure ValuesToServer; override;
  public
    destructor Destroy; override;
    procedure UpdateDatabase;
    property Persistent: Boolean read GetPersistent;
  end;

implementation

uses
  SysUtils,
  BoldComObjectSpace_TLB,
  BoldComUtils,
  BoldDefs;

{-- TBoldSystemHandleCom ------------------------------------------------------}

destructor TBoldSystemHandleCom.Destroy;
begin
  ConnectionClosing;
  inherited;
end;

procedure TBoldSystemHandleCom.ClearAllValues;
begin
  FDynamicBoldType := nil;
  FStaticBoldType := nil;
  FStaticSystemTypeInfo := nil;
  FValue := nil;
  FHandleId := 0;
  FBoldSystem := nil;
  FSystemActive := False;
  FPersistent := False;
end;


function TBoldSystemHandleCom.GetPersistent: Boolean;
begin
  EnsureCurrent;
  Result := FPersistent;
end;

procedure TBoldSystemHandleCom.UpdateDatabase;
begin
  EnsureCurrent;
  if Assigned(System) then
    System.UpdateDatabase
  else
    raise EBold.Create('UpdateDatabase: Not connected');
end;

procedure TBoldSystemHandleCom.ValuesFromServer;
var
  NamedValues: OleVariant;
  DummyType: IBoldElementTypeInfo;
  DummyObject: IBoldObject;
  DummyList: IBoldList;
  DummyListType: IBoldElementTypeInfo;
begin
  ServerElementHandle.GetData(0,
    FValue,
    FDynamicBoldType,
    FStaticBoldType,
    FStaticSystemTypeInfo,
    FBoldSystem,
    DummyType,
    DummyObject,
    DummyList,
    DummyListType,
    NamedValues);
  FHandleId := BoldGetNamedValue(NamedValues,'HandleId');
  FSystemActive := BoldGetNamedValue(NamedValues,'Active');
  FPersistent := BoldGetNamedValue(NamedValues,'Persistent');
end;

procedure TBoldSystemHandleCom.ValuesToServer;
begin
end;


function TBoldSystemHandleCom.ServerHandleClassName: string;
begin
  result := 'TBoldServerHandle';
end;

end.
