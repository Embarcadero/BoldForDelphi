
{ Global compiler directives }
{$include bold.inc}
unit BoldIDAdder;

interface

uses
  BoldPersistenceControllerPassthrough,
  BoldID,
  BoldUpdatePrecondition, 
  BoldCondition,
  BoldValueSpaceInterfaces,
  BoldListenerThread,
  BoldDefs;

type
  { forward declarations }
  TBoldIDAdder = class;

  { TBoldIDAdder }
  TBoldIDAdder = class(TBoldPersistenceControllerPassthrough)
  private
    fListener: TBoldListenerThread;
    function GetClientID: TBoldClientID;
  public
    constructor Create;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID); override;
    property BoldClientID: TBoldClientID read GetClientID;
    property Listener: TBoldListenerThread read fListener write fListener;
  end;

implementation

{ TBoldIDAdder }

constructor TBoldIDAdder.Create;
begin
  inherited;
end;

function TBoldIDAdder.GetClientID: TBoldClientID;
begin
  Result := Listener.BoldClientID;
end;

procedure TBoldIDAdder.PMFetch(ObjectIdList: TBoldObjectIdList;
  ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList;
  FetchMode: Integer; 
  BoldClientID: TBoldClientID);
begin
  if Listener.Suspended then
    Listener.Resume;
  BoldClientID := Self.BoldClientID;
  inherited PMFetch(ObjectIdList, ValueSpace, MemberIdList, FetchMode, BoldClientID);
end;

procedure TBoldIDAdder.PMFetchIDListWithCondition(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  FetchMode: Integer; Condition: TBoldCondition;
  BoldClientID: TBoldClientID);
begin
  if Listener.Suspended then
    Listener.Resume;
  BoldClientID := Self.BoldClientID;
  inherited;
end;

procedure TBoldIDAdder.PMUpdate(ObjectIdList: TBoldObjectIdList;
  ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace;
  Precondition: TBoldUpdatePrecondition; 
  TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime;
  BoldClientID: TBoldClientID);
begin
  if Listener.Suspended then
    Listener.Resume;
  BoldClientID := Self.BoldClientID;
  inherited;
end;

end.
