
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractComPersistenceControllerProxy;

interface

uses
  BoldId,
  BoldComConnection,
  BoldValueSpaceInterfaces,
  BoldPersistenceController,
  Boldsubscription;

type
  { forward declarations }
  TBoldAbstractComPersistenceControllerProxy = class;

  {-- TBoldAbstractComPersistenceControllerProxy --}
  TBoldAbstractComPersistenceControllerProxy = class(TBoldPersistenceController)
  protected
    procedure ExactifyIds(ObjectIdList: TBoldObjectidList; ValueSpace: IBoldValueSpace);
    function GetIsConnected: Boolean; virtual; abstract;
  public
    destructor Destroy; override;
    procedure Connect(const Provider: IBoldProvider; const ObjectName: string); virtual; abstract;
    procedure Disconnect; virtual;
    procedure SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet = []); override;
    property Connected: Boolean read GetIsConnected;
  end;


implementation


{ TBoldAbstractComPersistenceControllerProxy }

destructor TBoldAbstractComPersistenceControllerProxy.Destroy;
begin
  inherited;
  Disconnect;
end;

procedure TBoldAbstractComPersistenceControllerProxy.Disconnect;
begin
end;

procedure TBoldAbstractComPersistenceControllerProxy.ExactifyIds(
  ObjectIdList: TBoldObjectidList; ValueSpace: IBoldValueSpace);
var
  TranslationList: TBoldIDTranslationList;
begin
  TranslationList := TBoldIDTranslationList.Create;
  try
    PMExactifyIds(ObjectIdList, TranslationList, false);
    ValueSpace.ExactifyIds(TranslationList);
    ObjectIdList.ExactifyIds(TranslationList);
  finally
    TranslationList.Free;
  end;
end;

procedure TBoldAbstractComPersistenceControllerProxy.SubscribeToPersistenceEvents(
  Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet);
begin
  inherited;
end;

end.
