library BoldMMPlugin;

uses
  ComServ,
  SysUtils,
  Classes,
  Forms,
  MMToolsAPI,
  BoldMMPlugin_TLB in 'BoldMMPlugin_TLB.pas',
  BoldMMImporter in '..\Support\BoldMMImporter.pas',
  BoldUMLModelDataModule in '..\..\..\Core\BoldUMLModelDataModule.pas' {dmModelEdit: TDataModule},
  BoldMMExpert in 'BoldMMExpert.pas',
  BoldMMTVEditor in 'BoldMMTVEditor.pas' {frmBoldMMTVEdit},
  BoldMMTVMemo in 'BoldMMTVMemo.pas' {frmMemoEdit};

{$R *.res}
{$R *.tlb}

procedure InitializeExpert(const Srv: IMMToolServices); stdcall;
begin
    MMToolServices := Srv;
    Srv.AddExpert(TBoldMMExpert.Create);
  Application.Handle := Srv.GetParentHandle;
end;

procedure FinalizeExpert; stdcall;
begin
end;

function ExpertVersion: LongInt; stdcall;
begin
  Result := MMToolsApiVersion;
end;

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  InitializeExpert name MMExpertEntryProcName,
  FinalizeExpert name MMExpertExitProcName,
  ExpertVersion name MMExpertVersionProcName;

begin
  ComServer.LoadTypeLib;
end.
