
{ Global compiler directives }
{$include bold.inc}
unit BoldOSSMessage;

interface
{.$DEFINE CRCOSSMESSAGES}

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
    fModelCRC: string;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
{$IFDEF CRCOSSMESSAGES}
    function GetCrc: Cardinal;
{$ENDIF}
    function GetDataLength: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload;
    constructor Create(
      AMessageType: TBoldOSSMessageType;
      AEvents: string;
      AModelCRC: string;
      ABoldTimeStamp: TBoldTimeStampType;
      ATimeOfTimeStamp: TDateTimeMS;
      AClientSendTime: TDateTimeMS;
      AUser: string = '';
      AComputer: string = '';
      AApplication: string = ''); overload;
    constructor Create(AJSON: string); overload;
    destructor Destroy; override;
    function Clone: TBoldOSSMessage;
    property AsString: string read GetAsString write SetAsString;
  published
    property MessageType: TBoldOSSMessageType read fMessageType;
    property Events: string read fEvents;
    property ModelCRC: string read fModelCRC;
    property BoldTimeStamp: TBoldTimeStampType read fBoldTimeStamp;
    property TimeOfTimeStamp: TDateTimeMS read fTimeOfTimeStamp;
    property ClientSendTime: TDateTimeMS read fClientSendTime;
    // writeable properties, can be sent blank and filled in by service
    property Computer: string read fComputer write fComputer;
    property Application: string read fApplication write fApplication;
    property User: string read fUser write fUser;
    property DataLength: Integer read GetDataLength;
{$IFDEF CRCOSSMESSAGES}
    property Crc: Cardinal read GetCrc;
{$ENDIF}
  end;

implementation

uses
  System.SysUtils,
  BoldIsoDateTime,
  System.JSON,
  System.JSON.Writers,
  System.JSON.Readers,
  TypInfo;

{$IFDEF CRCOSSMESSAGES}
function CRCHash(const S: string): CARDINAL;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    Result := ((Result shl 3) and 2147483647) or
              (Result shr (32-3)) xor ord(S[i]);
end;

function SumCRCs(CRC1, CRC2: Cardinal): Cardinal;
begin
  result := ((CRC1 shl 3) and 2147483647) or
               (CRC1 shr (32-3)) xor CRC2;
end;
{$ENDIF}

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
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('MessageType', String((GetEnumName(TypeInfo(TBoldOSSMessageType), Ord(MessageType)))));
    JSONObject.AddPair('ModelCRC', ModelCRC);
    JSONObject.AddPair('BoldTimeStamp', IntToStr(BoldTimeStamp));
    JSONObject.AddPair('BoldTimeOfTimeStamp', AsISODateTimeMS(TimeOfTimeStamp));
    JSONObject.AddPair('SendTime', AsISODateTimeMS(ClientSendTime));
    JSONObject.AddPair('User', User);
    JSONObject.AddPair('Computer', Computer);
    JSONObject.AddPair('Application', Application);
    JSONObject.AddPair('Events', Events);
    JSONObject.AddPair('DataLength', IntToStr(DataLength));
{$IFDEF CRCOSSMESSAGES}
    JSONObject.AddPair('Crc', UIntToStr(Crc));
{$ENDIF}
    result := JSONObject.ToString;
  finally
    JSONObject.free;
  end;
end;

{$IFDEF CRCOSSMESSAGES}
function TBoldOSSMessage.GetCrc: Cardinal;
begin
  result := CRCHash(Events);
end;
{$ENDIF}

function TBoldOSSMessage.GetDataLength: Integer;
begin
  result := Length(Events);
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
    fModelCRC := JSONObject.GetValue('ModelCRC').Value;
    fBoldTimeStamp := StrToInt(JSONObject.GetValue('BoldTimeStamp').Value);
    fTimeOfTimeStamp := ParseISODateTime(JSONObject.GetValue('BoldTimeOfTimeStamp').Value);
    fClientSendTime := ParseISODateTime(JSONObject.GetValue('SendTime').Value);
    fUser := JSONObject.GetValue('User').Value;
    fComputer := JSONObject.GetValue('Computer').Value;
    fApplication := JSONObject.GetValue('Application').Value;
    fEvents := JSONObject.GetValue('Events').Value;
    s := JSONObject.GetValue('DataLength').Value;
    if s.ToInteger <> DataLength then
      raise Exception.CreateFmt('Message length mismatch, expected %s received %d', [s, DataLength]);
{$IFDEF CRCOSSMESSAGES}
    s := JSONObject.GetValue('Crc').Value;
    if s.ToInteger <> Crc then
      raise Exception.CreateFmt('Message CRC mismatch, expected %s received %d', [s, Crc]);
{$ENDIF}
  finally
    JSONObject.Free;
  end;
end;

constructor TBoldOSSMessage.Create(AMessageType: TBoldOSSMessageType; AEvents: string;
  AModelCRC: string;
  ABoldTimeStamp: TBoldTimeStampType; ATimeOfTimeStamp,
  AClientSendTime: TDateTimeMS;
  AUser: string; AComputer: string; AApplication: string);
begin
  fMessageType := AMessageType;
  fEvents := AEvents;
  fModelCRC := AModelCRC;
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
    fModelCRC := self.fModelCRC;
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

constructor TBoldOSSMessage.Create(AJSON: string);
begin
  inherited Create;
  AsString := AJSon;
end;

end.

