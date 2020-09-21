unit BoldUMLModelDataModule;

interface

uses
  Classes,
  BoldHandles,
  BoldSystemHandle,
  BoldModel,
  BoldTypeNameHandle,
  BoldUMLRose98Link,
  BoldExpressionHandle, BoldHandle, BoldUMLModelLink, BoldRootedHandles,
  BoldSubscription, BoldAbstractModel;

type
  TdmModelEdit = class(TDataModule)
    bshUMLModel: TBoldSystemHandle;
    BoldTypeNameHandle1: TBoldTypeNameHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldExpressionHandle1: TBoldExpressionHandle;
    bmlUMLModel: TBoldModel;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmModelEdit: TdmModelEdit;

procedure EnsureModelEditDataModule;

implementation

uses
  SysUtils,
  BoldRev;

var
  DataModuleEnsured: Boolean = false;

procedure EnsureModelEditDataModule;
begin
  if not assigned(dmModelEdit) then
  begin
    dmModelEdit := TdmModelEdit.Create(nil);
    DataModuleEnsured := true;
  end;
end;

{$R *.dfm}

initialization
finalization
  if DataModuleEnsured and assigned(dmModelEdit) then
    FreeAndNil(dmModelEdit);
end.
