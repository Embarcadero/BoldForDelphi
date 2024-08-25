
{ Global compiler directives }
{$include bold.inc}
unit BoldThreadedComObjectFactory;

interface

uses
  ComObj,
  ActiveX,
  BoldThreadSafeLog,
  BoldApartmentThread,
  BoldDefs;

type

  TBoldThreadedComObjectFactory = class;
  TBoldComObject = class;

  TBoldThreadedComObjectFactory = class(TComObjectFactory, IClassFactory)
  private
    FApartmentType: TBoldApartmentType;
    FApartmentThread: TBoldApartmentThread;
  protected
    function DoCreateInstance(const UnkOuter: IUnknown; const IID: TGUID;
      out Obj): HResult; stdcall;
  public
    constructor Create(ComServer: TComServerObject; ComClass: TComClass;
      const ClassID: TGUID; const ClassName, Description: string;
      Instancing: TClassInstancing; ApartmentType: TBoldApartmentType);
    destructor Destroy; override;
    function CreateInstance(const UnkOuter: IUnknown; const IID: TGUID;
      out Obj): HResult; stdcall;
    property ApartmentType: TBoldApartmentType read FApartmentType write FApartmentType;
  end;

  TBoldComObject = class(TComObject)
  public
    procedure Disconnect; virtual;
  end;
  {$EXTERNALSYM TBoldThreadedComObjectFactory}

implementation

uses
  SysUtils,
  BoldUtils,
  Windows,
  BoldPropagatorConstants,
  Messages;

{ TBoldThreadedComObjectFactory }

constructor TBoldThreadedComObjectFactory.Create(
  ComServer: TComServerObject; ComClass: TComClass; const ClassID: TGUID;
  const ClassName, Description: string; Instancing: TClassInstancing;
  ApartmentType: TBoldApartmentType);
begin
  inherited Create(ComServer, ComClass, ClassID, ClassName, Description, Instancing, tmApartment);
  FApartmentType := ApartmentType;
end;

function TBoldThreadedComObjectFactory.CreateInstance(const UnkOuter: IUnknown;
  const IID: TGUID; out Obj): HResult;
var
  rciInfo: TCreateInstanceInfo;
begin
  if IsLibrary then
    Result := inherited CreateInstance(UnkOuter, IID, Obj)
  else
  begin
    if not Assigned(FApartmentThread) then
    begin
      FApartmentThread := TBoldApartmentThread.Create(ApartmentType, DoCreateInstance, UnkOuter, IID);
      FApartmentThread.Resume;
      if not FApartmentThread.WaitUntilReady(TIMEOUT*10) then
        raise EBold.Create('Appartment thread timed out');
    end
    else
      FApartmentThread.Init(UnkOuter, IID);
    Fillchar (rciInfo, sizeof (rciInfo), 0);
    rciInfo.ApartmentThread := FApartmentThread;
    with FApartmentThread do
    begin
      SendMessage(FApartmentThread.QueueWindow, BM_CREATEOBJECTINTHREAD, 0, wParam(@rciInfo));
      if WaitForSingleObject(ObjectCreatedEvent, INFINITE) = WAIT_OBJECT_0 then
      begin
        Result := CreateResult;
        if Result <> S_OK then Exit;
        Result := UnMarshalInterface(Obj);
      end else
      begin
        Result := E_NOINTERFACE;
      end;
    end;
  end;
end;

destructor TBoldThreadedComObjectFactory.Destroy;
begin
  inherited Destroy;
  if Assigned(FApartmentThread) then
    FApartmentThread.Quit(True);
  FreeAndNil(FApartmentThread);
end;

function TBoldThreadedComObjectFactory.DoCreateInstance(
  const UnkOuter: IUnknown; const IID: TGUID; out Obj): HResult;
begin
  Result := inherited CreateInstance(UnkOuter, IID, Obj);
end;

{ TBoldComObject }

procedure TBoldComObject.Disconnect;
begin
  CoDisconnectObject(self, 0);
end;

end.
