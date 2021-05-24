
{ Global compiler directives }
{$include bold.inc}
unit BoldOSSMessage;

interface

uses
  Classes; // for TPersistent
//  BoldDefs; // for TBoldTimeStampType

const
  cOSSMessageSync = 'SYNC';
  cOSSMessageFail = 'FAIL';

type
  TSessionId = Int64;
  TBoldTimeStampType = integer; // copy from BoldDefs
  TBoldOSSMessageType = (mtSync, mtFail);
  TDateTimeMS = TDateTime;

  TBoldOSSMessage = class(TInterfacedPersistent)
  private
    fMessageType: TBoldOSSMessageType;
    fEvents: string;
    fBoldTimeStamp: TBoldTimeStampType;
    fTimeOfTimeStamp: TDateTimeMS;
    fClientSendTime: TDateTimeMS;
    fUser: string;
    fApplication: string;
    fComputer: string;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload;
    constructor Create(
      AMessageType: TBoldOSSMessageType;
      AEvents: string;
      ABoldTimeStamp: TBoldTimeStampType;
      ATimeOfTimeStamp: TDateTimeMS;
      AClientSendTime: TDateTimeMS;
      AUser: string = '';
      AComputer: string = '';
      AApplication: string = ''); overload;
    destructor Destroy; override;
    function Clone: TBoldOSSMessage;
    property AsString: string read GetAsString write SetAsString;
  published
    property MessageType: TBoldOSSMessageType read fMessageType;
    property Events: string read fEvents;
    property BoldTimeStamp: TBoldTimeStampType read fBoldTimeStamp;
    property TimeOfTimeStamp: TDateTimeMS read fTimeOfTimeStamp;
    property ClientSendTime: TDateTimeMS read fClientSendTime;
    // writeable properties, can be sent blank and filled in by service
    property Computer: string read fComputer write fComputer;
    property Application: string read fApplication write fApplication;
    property User: string read fUser write fUser;
  end;

implementation

uses
  System.SysUtils,
  BoldIsoDateTime,
  System.JSON,
  System.JSON.Writers,
  System.JSON.Readers,
  TypInfo;

{ TBoldOSSMessage }

constructor TBoldOSSMessage.Create;
begin
  inherited;
end;

destructor TBoldOSSMessage.Destroy;
begin
  inherited;
end;

function TBoldOSSMessage.GetAsString: string;
var
  JSONObject,t1: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('MessageType', String((GetEnumName(TypeInfo(TBoldOSSMessageType), Ord(MessageType)))));
    JSONObject.AddPair('BoldTimeStamp', IntToStr(BoldTimeStamp));
    JSONObject.AddPair('BoldTimeOfTimeStamp', AsISODateTimeMS(TimeOfTimeStamp));
    JSONObject.AddPair('SendTime', AsISODateTimeMS(ClientSendTime));
    JSONObject.AddPair('User', User);
    JSONObject.AddPair('Computer', Computer);
    JSONObject.AddPair('Application', Application);
    JSONObject.AddPair('Events', Events);
    result := JSONObject.ToString;
  finally
    JSONObject.free;
  end;
end;

procedure TBoldOSSMessage.SetAsString(const Value: string);
var
  JSONObject: TJSONObject;
  s: string;
begin
  JSONObject:= TJSONObject.ParseJSONValue(Value) as TJSONObject;
  try
    s := JSONObject.GetValue('MessageType').Value;
    if s = 'mtSync' then
      fMessageType := mtSync
    else
    if s = 'mtFail' then
      fMessageType := mtFail
    else
      raise Exception.Create('Unknown MessageType');
    fBoldTimeStamp := StrToInt(JSONObject.GetValue('BoldTimeStamp').Value);
    fTimeOfTimeStamp := ParseISODateTime(JSONObject.GetValue('BoldTimeOfTimeStamp').Value);
    fClientSendTime := ParseISODateTime(JSONObject.GetValue('SendTime').Value);
    fUser := JSONObject.GetValue('User').Value;
    fComputer := JSONObject.GetValue('Computer').Value;
    fApplication := JSONObject.GetValue('Application').Value;
    fEvents := JSONObject.GetValue('Events').Value;
  finally
    JSONObject.Free;
  end;
end;

constructor TBoldOSSMessage.Create(AMessageType: TBoldOSSMessageType; AEvents: string;
  ABoldTimeStamp: TBoldTimeStampType; ATimeOfTimeStamp,
  AClientSendTime: TDateTimeMS;
  AUser: string; AComputer: string; AApplication: string);
begin
  fMessageType := AMessageType;
  fEvents := AEvents;
  fBoldTimeStamp := ABoldTimeStamp;
  fTimeOfTimeStamp := ATimeOfTimeStamp;
  fClientSendTime := AClientSendTime;
  fUser := AUser;
  fComputer := AComputer;
  fApplication := AApplication;
end;

procedure TBoldOSSMessage.AssignTo(Dest: TPersistent);
begin
  if Dest is TBoldOSSMessage then
  with Dest as TBoldOSSMessage do
  begin
    fMessageType := self.fMessageType;
    fEvents := self.fEvents;
    fBoldTimeStamp := self.fBoldTimeStamp;
    fTimeOfTimeStamp := self.fTimeOfTimeStamp;
    fClientSendTime := self.fClientSendTime;
    fApplication := self.fApplication;
    fUser := self.fUser;
    fComputer := self.fComputer;
  end
  else
  inherited;
end;

function TBoldOSSMessage.Clone: TBoldOSSMessage;
begin
  result := TBoldOSSMessage.Create;
  self.AssignTo(result);
end;

end.

