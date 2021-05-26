
{ Global compiler directives }
{$include bold.inc}
unit BoldOLLEHandles;

interface

uses
  BoldOlleDistributableObjectHandlers,
  BoldHandle,
  classes,
  BoldAbstractPersistenceHandleDB,
  BoldOLLEController;

type
  TBoldOLLEHandle = class;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldOLLEHandle = class(TBoldHandle)
  private
    fOLLEController: TBoldOLLEController;
    fAppPHandle: TBoldAbstractPersistenceHandleDB;
    fOwnObjectHandler: TBoldOwnObjectHandler;
    fForeignObjectHandler: TBoldForeignObjectHandler;
    function GetPersistent: Boolean;
    procedure SetPersistent(const Value: Boolean);
    function GetOLLEController: TBoldOLLEController;
    function GetOwnObjectHandler: TBoldOwnObjectHandler;
    function GetForeignObjectHandler: TBoldForeignObjectHandler;
  protected
    function GetHandledObject: TObject; override;
  public
    destructor Destroy; override;
    procedure Loaded; override;
    property Persistent: Boolean read GetPersistent write SetPersistent;
    property OLLEController: TBoldOLLEController read GetOLLEController;
    property OwnObjectHandler: TBoldOwnObjectHandler read GetOwnObjectHandler;
    property ForeignObjectHandler: TBoldForeignObjectHandler read GetForeignObjectHandler;
  published
    property ApplicationPersistenceHandle: TBoldAbstractPersistenceHandleDB read fAppPHandle write fAppPHandle;
  end;



implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldOLLEHandle }

destructor TBoldOLLEHandle.Destroy;
begin
  inherited;
  fOLLEController.Free;
end;


function TBoldOLLEHandle.GetForeignObjectHandler: TBoldForeignObjectHandler;
begin
  if not assigned(fForeignObjectHandler) then
  begin
    fForeignObjectHandler := TBoldForeignObjectHandler.Create;
    fForeignObjectHandler.OllSystem := OLLEController.OLLESystem;
    fForeignObjectHandler.PController := ApplicationPersistenceHandle.PersistenceControllerDefault;
  end;
  result := fForeignObjectHandler;
end;

function TBoldOLLEHandle.GetHandledObject: TObject;
begin
  result := GetOLLEController;
end;

function TBoldOLLEHandle.GetOLLEController: TBoldOLLEController;
begin
  if not assigned(fOLLEController) then
    fOLLEController := TBoldOLLEController.Create(fAppPHandle);
  result := fOLLEController;
end;

function TBoldOLLEHandle.GetOwnObjectHandler: TBoldOwnObjectHandler;
begin
  if not assigned(fOwnObjectHandler) then
  begin
    fOwnObjectHandler := TBoldOwnObjectHandler.Create;
    fOwnObjectHandler.OllSystem := OLLEController.OLLESystem;
    fOwnObjectHandler.PController := ApplicationPersistenceHandle.PersistenceControllerDefault;
  end;
  result := fOwnObjectHandler;
end;

function TBoldOLLEHandle.GetPersistent: Boolean;
begin
  result := GetOLLEController.Persistent;
end;

procedure TBoldOLLEHandle.Loaded;
begin
end;

procedure TBoldOLLEHandle.SetPersistent(const Value: Boolean);
begin
  GetOLLEController.Persistent := Value;
end;


end.
