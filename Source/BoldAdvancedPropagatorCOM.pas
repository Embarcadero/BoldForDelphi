
{ Global compiler directives }
{$include bold.inc}
unit BoldAdvancedPropagatorCOM;

interface
uses
  BoldThreadedComObjectFactory,
  ComObj,
  BoldPropagatorInterfaces_TLB,
  BoldAdvancedPropagator
  ;
type
  {forward declarations}
  TBoldAdvancedPropagatorCOM = class;
  TBoldPropagatorFactory = class;

  {TBoldAdvancedPropagatorCOM}
  TBoldAdvancedPropagatorCOM = class(TBoldComObject, IBoldClientHandler, IBoldEventPropagator)
  private
    FAdvancedPropagator: TBoldAdvancedPropagator;
    function getBoldClientHandler: IBoldClientHandler;
    function getEnqueuer: IBoldEventPropagator;
  protected
    function GetControllingUnknown: IUnknown;
    property AdvancedPropagator: TBoldAdvancedPropagator read FAdvancedPropagator write FAdvancedPropagator;
  public
    destructor Destroy; override;
    procedure Initialize; override;
    property BoldClientHandler: IBoldClientHandler read getBoldClientHandler implements IBoldClientHandler;
    property Enqueuer: IBoldEventPropagator read getEnqueuer implements IBoldEventPropagator;
  end;

  {TBoldPropagatorFactory}
  TBoldPropagatorFactory = class(TComObjectFactory)
  public
    constructor Create(ComServer: TComServerObject; const ClassID: TGUID; const ClassName, Description: string);
  end;

implementation

uses
  BoldClientHandlerCOM,
  BoldEnqueuerCOM,
  BoldApartmentThread,
  BoldPropagatorServer;

{TBoldAdvancedPropagatorCOM}
function TBoldAdvancedPropagatorCOM.GetBoldClientHandler: IBoldClientHandler;
begin
  Result := ClienthandlerCOMFactory.CreateComObject(nil) as IBoldClientHandler;
end;

function TBoldAdvancedPropagatorCOM.getEnqueuer: IBoldEventPropagator;
begin
  Result := EnqueuerCOMFactory.CreateComObject(nil) as IBoldEventPropagator;
end;

function TBoldAdvancedPropagatorCOM.GetControllingUnknown: IUnknown;
begin
  if Assigned(Controller) then
    Result := Controller
  else
    Result := Self as IUnknown;
end;

destructor TBoldAdvancedPropagatorCOM.Destroy;
begin
  TBoldPropagatorServer.Instance.RemoveComObject(self);
  inherited Destroy;
end;

procedure TBoldAdvancedPropagatorCOM.Initialize;
begin
  inherited;
  TBoldPropagatorServer.Instance.AddComObject(self);
end;

{ TBoldPropagatorFactory }

constructor TBoldPropagatorFactory.Create(ComServer: TComServerObject; const ClassID: TGUID;
          const ClassName, Description: string);
begin
  inherited Create(ComServer, TBoldAdvancedPropagatorCOM, ClassID, ClassName, Description, ciMultiInstance, tmFree);
end;

end.
