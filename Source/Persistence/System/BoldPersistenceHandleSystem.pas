unit BoldPersistenceHandleSystem;

interface

uses
  BoldSystemhandle,
  BoldPersistenceController,
  BoldPersistenceControllerSystem,
  BoldPersistenceHandle;

type
  TBoldPersistenceHandleSystem = class(TBoldPersistenceHandle)
  private
    fSystemHandle: TBoldSystemHandle;
    function GetPersistenceControllerSystem: TBoldPersistenceControllerSystem;
  protected
    function CreatePersistenceController: TBoldPersistenceController; override;
    procedure SetActive(Value: Boolean); override;
  public
    property PersistenceControllerSystem: TBoldPersistenceControllerSystem read GetPersistenceControllerSystem;
  published
    property SystemHandle: TBoldSystemHandle read fSystemHandle write fSystemHandle;
  end;

implementation

{ TBoldPersistenceHandleSystem }

function TBoldPersistenceHandleSystem.CreatePersistenceController: TBoldPersistenceController;
begin
  result := TBoldPersistenceControllerSystem.Create;
end;

function TBoldPersistenceHandleSystem.GetPersistenceControllerSystem: TBoldPersistenceControllerSystem;
begin
  result := PersistenceController as TBoldPersistenceControllerSystem;
end;

procedure TBoldPersistenceHandleSystem.SetActive(Value: Boolean);
begin
  if value <> Active then begin
    if value then
      PersistenceControllerSystem.BoldSystem := SystemHandle.System;
  end;
  inherited;
end;

end.
