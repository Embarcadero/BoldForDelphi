
{ Global compiler directives }
{$include bold.inc}
unit BoldServerHandles;

interface

uses
  Classes,
  BoldHandle;

type
  { forward declarations }
  TBoldServerHandle = class;
  TBoldExportHandle = class;

  TBoldServerHandle = class(TBoldHandle)
  private
    FActive: Boolean;
  protected
    procedure SetActive(Value: Boolean); virtual;
    property Active: Boolean read FActive write SetActive default True;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TBoldExportHandle = class(TBoldServerHandle)
  private
    FObjectName: string;
  protected
    function implicitName: String;
    procedure SetObjectName(const Value: string); virtual;
    function GetObjectName: string; virtual;
    property ObjectName: string read GetObjectName write SetObjectName;
  end;

implementation

uses
  SysUtils;

{-- TBoldServerHandle ---------------------------------------------------------}

constructor TBoldServerHandle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
end;

procedure TBoldServerHandle.SetActive(Value: Boolean);
begin
  if Value <> FActive then
    FActive := Value;
end;

{-- TBoldServerObjectHandle ---------------------------------------------------}

function TBoldExportHandle.GetObjectName: string;
begin
  if fObjectName <> '' then
    Result := fObjectName
  else
    result := ImplicitName;
end;

function TBoldExportHandle.implicitName: String;
begin
  if assigned(owner) and (owner.Name <> '') then
    result := format('%s.%s', [Owner.Name, Name])
  else
    result := name;
end;

procedure TBoldExportHandle.SetObjectName(const Value: string);
begin
  if Value <> FObjectName then
  begin
    if Value = Implicitname then
      fObjectName := ''
    else
      FObjectName := Value;
  end;
end;

end.
