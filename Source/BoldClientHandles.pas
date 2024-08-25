
{ Global compiler directives }
{$include bold.inc}
unit BoldClientHandles;

interface

uses
  Classes,
  BoldHandle;

type
  { forward declarations }
  TBoldClientHandle = class;
  TBoldImportHandle = class;

  TBoldClientHandle = class(TBoldHandle)
  private
    FActive: Boolean;
    function GetIsDesigning: Boolean;
    function GetIsLoading: Boolean;
  protected
    procedure SetActive(Value: Boolean); virtual;
    property Active: Boolean read FActive write SetActive default True;
    property IsDesigning: Boolean read GetIsDesigning;
    property IsLoading: Boolean read GetIsLoading;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TBoldImportHandle = class(TBoldClientHandle)
  private
    FObjectName: string;
  protected
    procedure SetObjectName(const Value: string); virtual;
    property ObjectName: string read FObjectName write SetObjectName;
  end;

implementation


{-- TBoldClientHandle ---------------------------------------------------------}

constructor TBoldClientHandle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
end;

function TBoldClientHandle.GetIsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TBoldClientHandle.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

procedure TBoldClientHandle.SetActive(Value: Boolean);
begin
  if Value <> FActive then
    FActive := Value;
end;

{-- TBoldImportHandle ---------------------------------------------------------}

procedure TBoldImportHandle.SetObjectName(const Value: string);
begin
  if Value <> FObjectName then
  begin
    FObjectName := Value;
  end;
end;

end.
