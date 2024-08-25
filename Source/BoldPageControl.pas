{ Global compiler directives }
{$include bold.inc}
unit BoldPageControl;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  Controls,
  BoldEnvironmentVCL,
  Boldhandles,
  BoldControlPack,
  BoldElementHandleFollower,
  BoldStringControlPack,
  BoldElements,
  ComCtrls;

type
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldPageControl = class(TPageControl)
  private
    { Private declarations }
    fHandleFollower: TBoldElementHandleFollower;
    fBoldProperties: TBoldStringFollowerController;
    procedure _Display(Follower: TBoldFollower);
    function _GetContextType: TBoldElementTypeInfo;
    function GetBoldHandle: TBoldElementHandle;
    procedure SetBoldHandle(const Value: TBoldElementHandle);
    procedure SetBoldProperties(const Value: TBoldStringFollowerController);
    function GetFollower: TBoldFollower;
  protected
    { Protected declarations }
    property Follower: TBoldFollower read GetFollower;
  public
    { Public declarations }
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerController read fBoldProperties write SetBoldProperties;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldPageControl }

constructor TBoldPageControl.Create(owner: TComponent);
begin
  inherited;
  fBoldProperties := TBoldStringFollowerController.Create(self);
  fHandleFollower := TBoldElementHandleFollower.Create(owner, fBoldProperties);
  fBoldProperties.AfterMakeUptoDate := _Display;
  fBoldProperties.OnGetContextType := _GetContextType;
end;

destructor TBoldPageControl.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  inherited;
end;

function TBoldPageControl.GetBoldHandle: TBoldElementHandle;
begin
  result := fHandleFollower.BoldHandle;
end;

function TBoldPageControl.GetFollower: TBoldFollower;
begin
  result := fHandleFollower.Follower;
end;

procedure TBoldPageControl.SetBoldHandle(const Value: TBoldElementHandle);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldPageControl.SetBoldProperties(
  const Value: TBoldStringFollowerController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldPageControl._Display(Follower: TBoldFollower);
var
  NewString: string;
  i: integer;
begin
  NewString := TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower);
  for i := 0 to PageCount-1 do
    if Pages[i].Name = NewString then
    begin
      ActivePage := Pages[i];
      exit;
    end;
end;

function TBoldPageControl._GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

end.
