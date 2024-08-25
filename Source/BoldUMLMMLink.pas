{ Global compiler directives }
{$include bold.inc}
unit BoldUMLMMLink;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Variants,
  BoldUMLModelLink,
  BoldUMLModel;

type
  { forward declarations }
  TBoldUMLMMLink = class;

  { TBoldUMLMMLink }
  TBoldUMLMMLink = class(TBoldUMLModelLink)
  private
    fFilename: String;
    fValidateInput: Boolean;
  protected
    function GetCanExport: Boolean; override;
    function GetDisplayName: string; override;
    function GetFileName: string; override;
    procedure SetFileName(const Value: string); override;
  public
    function ExportModel(UMLModel: TUMLModel): Boolean; override;
    function ImportModel(UMLModel: TUMLModel): Boolean; override;
  published
    property FileName;
    property BoldModel;
    property ValidateInput: Boolean read fValidateInput write fValidateInput;
  end;

const
  MM_LINKEXTENSION: string = '.mpb';
  MM_LINKDESC: string = 'ModelMaker Project Bundle (*.mpb)';

implementation

uses
  SysUtils,
  Classes,
  BoldModelmaker_TLB,
  BoldMMPlugin_TLB,  
  BoldDefs,
  BoldUtils,
  BoldGuard,
  BoldQueue,
  BoldLogHandler,
  BoldMeta,
  BoldBld,
  BoldUMLModelConverter;

{ TBoldUMLMMLink }

function TBoldUMLMMLink.ExportModel(UMLModel: TUMLModel): Boolean;
var
  MoldModel: TMoldModel;
  G: IBoldGuard;
  App: IApp;
  Expert: IBoldExpertDisp;
  StringList: TStringList;
  ResultString: string;
begin
  G := TBoldGuard.Create(MoldModel, StringList);
  StringList := TStringList.Create;
  App := CoApp.Create;
  if not Assigned(App) then
     raise EBold.Create('Failed to Launch ModelMaker');
  Expert := App.GetExpert('BoldSoft.BoldExpert') as IBoldExpertDisp;
  if not Assigned(Expert) then
  begin
    Sleep(10);
    Expert := App.GetExpert('BoldSoft.BoldExpert') as IBoldExpertDisp;
  end;
  if not Assigned(Expert) then
     raise EBold.Create('Bold Expert not installed in ModelMaker. Make sure the file BoldMMPlugin.dll is in the ModelMaker\Experts directory, and register the DLL using RegSvr32.exe');
  if (Expert.ProjectFileName <> fFileName) then
    Expert.OpenProject(fFileName);
  if (Expert.ProjectFileName <> fFileName) then
    Expert.NewProject(fFileName);
  if (Expert.ProjectFileName <> fFileName) then
    raise EBoldImport.Createfmt('Failed to open ModelMaker project file %s, and failed to create new project', [fFileName]);

  Result := False;
  if not Assigned(UMLModel) then
    raise EBoldImport.CreateFmt('%s.ImportModel: Must have an UMLModel to export.', [className]);

  BoldLog.StartLog('Model Export');
  MoldModel :=TBoldModelConverter.UMLModelToMold(UMLModel);
  TMoldBLDRW.ModelToStrings(MoldModel, StringList);
  ResultString := Expert.SetModelAsString(StringList.Text);
  if ResultString <> '' then
    raise EBold.Createfmt(' Error during export: %s', [ResultString])
  else
    Expert.SaveProject(False);  
end;

function TBoldUMLMMLink.GetCanExport: Boolean;
begin
  Result := True;
end;

function TBoldUMLMMLink.GetDisplayName: string;
begin
  Result := fFilename;
end;

function TBoldUMLMMLink.GetFileName: string;
begin
  Result := fFilename;
end;

function TBoldUMLMMLink.ImportModel(UMLModel: TUMLModel): Boolean;
var
  MoldModel: TMoldModel;
  G: IBoldGuard;
  App: IApp;
  Expert: IBoldExpertDisp;
  StringList: TStringList;
begin
  G := TBoldGuard.Create(MoldModel, StringList);
  App := CoApp.Create;
  if not Assigned(App) then
     raise EBoldImport.Create('Failed to Launch ModelMaker');
  Expert := App.GetExpert('BoldSoft.BoldExpert') as IBoldExpertDisp;
  if not Assigned(Expert) then
  begin
    Sleep(10);         
    Expert := App.GetExpert('BoldSoft.BoldExpert') as IBoldExpertDisp;
  end;
  if not Assigned(Expert) then
     raise EBoldImport.Create('Bold Expert not installed in ModelMaker. Make sure the file BoldMMPlugin.dll is in the ModelMaker\Experts directory, and register the DLL using RegSvr32.exe');
  if (Expert.ProjectFileName <> fFileName) then
    Expert.OpenProject(fFileName);
  if (Expert.ProjectFileName <> fFileName) then
    raise EBoldImport.Createfmt('Failed to open ModelMaker project file %s', [fFileName]);


  Result := False;
  if not Assigned(UMLModel) then
    raise EBoldImport.CreateFmt('%s.ImportModel: Must have an UMLModel to import to.', [className]);
  TBoldQueueable.DeActivateDisplayQueue;
  try
    UMLModel.BoldSystem.StartTransaction;
    try
      Result := True;
      UMLModel.Clear;
      BoldLog.StartLog('Model import');
      StringList := TStringList.Create;
      StringList.Text := Expert.GetModelAsString;
      MoldModel := TMoldBLDRW.StringsToModel(StringList);
      TBoldModelConverter.MoldModelToUML(MoldModel, UMLModel);
    if BoldLog.ProcessInterruption then
      begin
        BoldLog.Log('Import was aborted, old model restored', ltError);
        UMLModel.BoldSystem.RollbackTransaction
      end
      else
        UMLModel.BoldSystem.CommitTransaction;
      BoldLog.EndLog;
    except
      on E: Exception do
      begin
        UMLModel.BoldSystem.RollbackTransaction;
        BoldLog.LogFmt('Error: %s', [E.Message], ltError);
      end;
    end;
  finally
    TBoldQueueable.ActivateDisplayQueue;
  end;
end;

procedure TBoldUMLMMLink.SetFileName(const Value: string);
begin
  fFilename := Value;
end;

initialization
  BoldUMLModelLinkList.AddLink(MM_LINKEXTENSION, MM_LINKDESC, TBoldUMLMMLink);

finalization
  if BoldUMLModelLinkListAssigned then
    BoldUMLModelLinkList.RemoveLink(TBoldUMLMMLink);

end.
