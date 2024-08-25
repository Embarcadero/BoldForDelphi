
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractObjectUpgraderHandle;

interface

uses
  BoldHandle,
  BoldAbstractObjectUpgrader;

type
  TBoldAbstractObjectUpgraderHandle = class(TBoldHandle)
  private
    fConfig: TBoldObjectUpgraderConfiguration;
    fObjectUpgrader: TBoldAbstractObjectUpgrader;
    function GetConfig: TBoldObjectUpgraderConfiguration;
    procedure SetConfig(const Value: TBoldObjectUpgraderConfiguration);
    function GetObjectUpgrader: TBoldAbstractObjectUpgrader;
  protected
    function ConfigClass: TBoldObjectUpgraderConfigClass; virtual;
    function CreateObjectUpgrader: TBoldAbstractObjectUpgrader; virtual; abstract;
    function GetHandledObject: TObject; override;
  public
    destructor Destroy; override;
    property ObjectUpgrader: TBoldAbstractObjectUpgrader read GetObjectUpgrader;
  published
    property Config: TBoldObjectUpgraderConfiguration read GetConfig write SetConfig;
  end;

implementation

uses
  SysUtils,
  Classes;

{ TBoldAbstractObjectUpgraderHandle }

function TBoldAbstractObjectUpgraderHandle.ConfigClass: TBoldObjectUpgraderConfigClass;
begin
  result := TBoldObjectUpgraderConfiguration;
end;

destructor TBoldAbstractObjectUpgraderHandle.Destroy;
begin
  FreeAndNil(fObjectUpgrader);
  FreeAndNil(fConfig);
  inherited;
end;

function TBoldAbstractObjectUpgraderHandle.GetConfig: TBoldObjectUpgraderConfiguration;
begin
  if not assigned(fConfig) then
    fConfig := ConfigClass.Create(self);
  result := fConfig;
end;

function TBoldAbstractObjectUpgraderHandle.GetHandledObject: TObject;
begin
  result := ObjectUpgrader;
end;


function TBoldAbstractObjectUpgraderHandle.GetObjectUpgrader: TBoldAbstractObjectUpgrader;
begin
  if not assigned(fObjectUpgrader) then
    fObjectUpgrader := CreateObjectUpgrader;
  result := fObjectUpgrader;
end;

procedure TBoldAbstractObjectUpgraderHandle.SetConfig(const Value: TBoldObjectUpgraderConfiguration);
var
  i: integer;
begin
  fConfig.Clear;
  for i := 0 to Value.Count-1 do
    fConfig.add.Assign(Value[i]);
end;

end.
