{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelDataModule;

interface

uses
  Classes,
  BoldHandles,
  BoldSystemHandle,
  BoldModel,
  BoldTypeNameHandle,
  BoldSubscription,
  BoldAbstractModel;

type
  TdmModelEdit = class(TDataModule)
    BoldTypeNameHandle1: TBoldTypeNameHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    bmlUMLModel: TBoldModel;
    bshUMLModel: TBoldSystemHandle;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  dmModelEdit: TdmModelEdit;

procedure EnsureModelEditDataModule;

implementation

uses
  SysUtils;

{$R *.dfm}

var
  DataModuleEnsured: Boolean = false;

{ TdmModelEdit }

constructor TdmModelEdit.Create(AOwner: TComponent);
begin
  inherited;
  if (csDesigning in ComponentState) then
    InitInheritedComponent(Self, TDataModule);
end;

procedure EnsureModelEditDataModule;
begin
  if not DataModuleEnsured then
  begin
    DataModuleEnsured := true;
    dmModelEdit := TdmModelEdit.Create(nil);
  end;
end;    

initialization

finalization
  if DataModuleEnsured and assigned(dmModelEdit) then
    FreeAndNil(dmModelEdit);
end.
