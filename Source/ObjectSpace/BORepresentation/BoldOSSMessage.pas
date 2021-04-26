unit BoldOSSMessage;

interface

uses
  Classes; // for TPersistent
//  SynCommons; // for TDateTimeMS
//  BoldDefs; // for TBoldTimeStampType

const
  cOSSMessageSync = 'SYNC';
  cOSSMessageFail = 'FAIL';

type
  TSessionId = Int64;
  TBoldTimeStampType = integer; // copy from BoldDefs
  TOSSMessageType = (mtSync, mtFail);
  TDateTimeMS = TDateTime;

  TOSSMessage = class(TInterfacedPersistent)
  private
    fMessageType: TOSSMessageType;
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
      AMessageType: TOSSMessageType;
      AEvents: string;
      ABoldTimeStamp: TBoldTimeStampType;
      ATimeOfTimeStamp: TDateTimeMS;
      AClientSendTime: TDateTimeMS;
      AUser: string = '';
      AComputer: string = '';
      AApplication: string = ''); overload;
    destructor Destroy; override;
    function Clone: TOSSMessage;
    property AsString: string read GetAsString write SetAsString;
  published
    property MessageType: TOSSMessageType read fMessageType;
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
  TypInfo;

{ TOSSMessage }

constructor TOSSMessage.Create;
begin
  inherited;
end;

destructor TOSSMessage.Destroy;
begin
  inherited;
end;

function TOSSMessage.GetAsString: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.EnsureCapacity(Length(Events)+100);
    sb.AppendLine(String((GetEnumName(TypeInfo(TOSSMessageType), Ord(MessageType)))));
    sb.AppendLine(IntToStr(BoldTimeStamp));
    sb.AppendLine(AsISODateTimeMS(TimeOfTimeStamp));
    sb.AppendLine(AsISODateTimeMS(ClientSendTime));
    sb.AppendLine(User);
    sb.AppendLine(Computer);
    sb.AppendLine(Application);
    sb.AppendLine(Events);
    result := sb.ToString;
  finally
    sb.free;
  end;
end;

procedure TOSSMessage.SetAsString(const Value: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := Value;
    Assert(sl.Count = 8, 'Wrong count of lines in OSSMessage:' + IntToStr(sl.count));
    if sl[0] = 'mtSync' then
      fMessageType := mtSync
    else
    if sl[0] = 'mtFail' then
      fMessageType := mtFail
    else
      raise Exception.Create('Unknown MessageType');
    fBoldTimeStamp := StrToint(sl[1]);
    fTimeOfTimeStamp :=ParseISODateTime(sl[2]);
    fClientSendTime := ParseISODateTime(sl[3]);
    fUser := sl[4];
    fComputer := sl[5];
    fApplication := sl[6];
    fEvents := sl[7];
  finally
    sl.Free;
  end;
end;

constructor TOSSMessage.Create(AMessageType: TOSSMessageType; AEvents: string;
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

procedure TOSSMessage.AssignTo(Dest: TPersistent);
begin
  if Dest is TOSSMessage then
  with Dest as TOSSMessage do
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

function TOSSMessage.Clone: TOSSMessage;
begin
  result := TOSSMessage.Create;
  self.AssignTo(result);
end;

end.
