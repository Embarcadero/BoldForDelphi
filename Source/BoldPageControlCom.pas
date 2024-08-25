
{ Global compiler directives }
{$include bold.inc}
unit BoldPageControlCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Classes,
  Controls,
  BoldEnvironmentVCL,
  BoldHandlesCom,
  BoldControlPackCom,
  BoldElementHandleFollowerCom,
  BoldStringControlPackCom,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  ComCtrls;

type
  TBoldPageControlCom = class(TPageControl)
  private
    { Private declarations }
    fHandleFollower: TBoldElementHandleFollowerCom;
    fBoldProperties: TBoldStringFollowerControllerCom;
    procedure _Display(Follower: TBoldFollowerCom);
    function _GetContextType: IBoldElementTypeInfo;
    function GetBoldHandle: TBoldElementHandleCom;
    procedure SetBoldHandle(const Value: TBoldElementHandleCom);
    procedure SetBoldProperties(const Value: TBoldStringFollowerControllerCom);
    function GetFollower: TBoldFollowerCom;
  protected
    { Protected declarations }
    property Follower: TBoldFollowerCom read GetFollower;
  public
    { Public declarations }
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property BoldHandle: TBoldElementHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerControllerCom read fBoldProperties write SetBoldProperties;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldPageControlCom }

constructor TBoldPageControlCom.Create(owner: TComponent);
begin
  inherited;
  fBoldProperties := TBoldStringFollowerControllerCom.Create(self);
  fHandleFollower := TBoldElementHandleFollowerCom.Create(owner, fBoldProperties);
  fBoldProperties.AfterMakeUptoDate := _Display;
  fBoldProperties.OnGetContextType := _GetContextType;
end;

destructor TBoldPageControlCom.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  inherited;
end;

function TBoldPageControlCom.GetBoldHandle: TBoldElementHandleCom;
begin
  result := fHandleFollower.BoldHandle;
end;

function TBoldPageControlCom.GetFollower: TBoldFollowerCom;
begin
  result := fHandleFollower.Follower;
end;

procedure TBoldPageControlCom.SetBoldHandle(const Value: TBoldElementHandleCom);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldPageControlCom.SetBoldProperties(
  const Value: TBoldStringFollowerControllerCom);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldPageControlCom._Display(Follower: TBoldFollowerCom);
var
  NewString: string;
  i: integer;
begin
  NewString := TBoldStringFollowerControllerCom(Follower.Controller).GetCurrentAsString(Follower);
  for i := 0 to PageCount-1 do
    if Pages[i].Name = NewString then
    begin
      ActivePage := Pages[i];
      exit;
    end;
end;

function TBoldPageControlCom._GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

end.
