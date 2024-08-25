
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractOutputQueueHandler;

interface

uses
  BoldDefs;

type
  {forward declarations}
  TBoldAbstractOutputQueueHandler = class;
  
  TBoldAbstractOutputQueueHandler = class
  public
    procedure SendEvent(const ClientID: TBoldClientID; EventName: string); virtual; abstract;
    procedure ClearQueueForClient(const ClientID: TBoldClientID); virtual; abstract;
    function QueueCountForClient(const ClientID: TBoldClientID): integer; virtual; abstract;
    procedure SendEventAndFlushQueue(const ClientID: TBoldClientID; EventName: string); virtual; abstract;
  end;

implementation

end.
