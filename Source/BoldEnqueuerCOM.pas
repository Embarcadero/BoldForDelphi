
{ Global compiler directives }
{$include bold.inc}
unit BoldEnqueuerCOM;

interface

uses
  comobj,
  BoldPropagatorInterfaces_TLB,
  BoldEnqueuer,
  BoldThreadSafeLog,
  BoldThreadedComObjectFactory
  ;
type
  {forward declarations}
  TBoldEnqueuerCOM = class;
  TBoldEnqueuerThreadedCOMFactory = class;

  {TBoldEnqueuerCOM}
  TBoldEnqueuerCOM = class(TBoldComObject, IBoldEventPropagator)
  private
    FEnqueuer: TBoldEnqueuer;
  public
    destructor Destroy; override;
    {IBoldEventReceiver}
    function  SendEvents(BoldClientID: Integer; Events: OleVariant): HResult; stdcall;
    function  AddSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
    function  CancelSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
    procedure Initialize; override;
    property Enqueuer: TBoldEnqueuer read FEnqueuer write FEnqueuer;
  end;

  {TBoldEnqueuerCOMFactory}
  TBoldEnqueuerCOMFactory = class(TComObjectFactory)
  private
    fEnqueuer: TBoldEnqueuer;
  public
    function CreateComObject(const Controller: IUnknown): TComObject; override;
    property Enqueuer: TBoldEnqueuer read fEnqueuer write fEnqueuer;
  end;

  {TBoldEnqueuerThreadedCOMFactory}
  TBoldEnqueuerThreadedCOMFactory = class(TBoldThreadedComObjectFactory)
  private
    fEnqueuer: TBoldEnqueuer;
  public
    function CreateComObject(const Controller: IUnknown): TComObject; override;
    property Enqueuer: TBoldEnqueuer read fEnqueuer write fEnqueuer;
  end;

var
  EnqueuerCOMFactory: TBoldEnqueuerThreadedCOMFactory;

implementation

uses
  BoldPropagatorServer;

{ TBoldEnqueuerCOM }
function TBoldEnqueuerCOM.AddSubscriptions(BoldClientID: Integer;
  Subscriptions: OleVariant): HResult;
begin
  BoldLogThread('ID=Enqueuer/AddS');
  Result := Enqueuer.AddSubscriptions(BoldClientID, Subscriptions);
end;

function TBoldEnqueuerCOM.CancelSubscriptions(BoldClientID: Integer;
  Subscriptions: OleVariant): HResult;
begin
  BoldLogThread('ID=Enqueuer/CancelS');
  Result := Enqueuer.CancelSubscriptions(BoldClientID, Subscriptions);
end;

destructor TBoldEnqueuerCOM.Destroy;
begin
  TBoldPropagatorServer.Instance.RemoveComObject(self);
  inherited Destroy;
end;

procedure TBoldEnqueuerCOM.Initialize;
begin
  inherited;
  TBoldPropagatorServer.Instance.AddComObject(self);
end;

function TBoldEnqueuerCOM.SendEvents(BoldClientID: Integer;
  Events: OleVariant): HResult;
begin
  BoldLogThread('ID=Enqueuer/SendEv');
  Result := Enqueuer.SendEvents(BoldClientId, Events);
end;

{ TBoldEnqueuerCOMFactory }
function TBoldEnqueuerCOMFactory.CreateComObject(
  const Controller: IUnknown): TComObject;
begin
  Result := inherited CreateComObject(Controller);
 (Result as TBoldEnqueuerCOM).Enqueuer := Enqueuer;
end;

{TBoldEnqueuerThreadedCOMFactory}
function TBoldEnqueuerThreadedCOMFactory.CreateComObject(
  const Controller: IUnknown): TComObject;
begin
  Result := inherited CreateComObject(Controller);
  (Result as TBoldEnqueuerCOM).Enqueuer := Enqueuer;
end;

end.
