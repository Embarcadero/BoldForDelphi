unit BoldUMLBldLink;

interface

uses
  BoldUMLModelLink,
  BoldUMLModel;

type
  { forward declarations }
  TBoldUMLBldLink = class;

  { TBoldUMLBldLink }
  TBoldUMLBldLink = class(TBoldUMLModelLink)
  private
    fFilename: String;
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
  end;

const
  Bld_LINKEXTENSION: string = '.bld';
  Bld_LINKDESC: string = 'Bold Proprietary Format';

implementation
uses
  SysUtils,
  Classes,
  BoldDefs,
  BoldUtils,
  BoldGuard,
  BoldQueue,
  BoldLogHandler,
  BoldMeta,
  BoldBld,
  BoldUMLModelConverter;

{ TBoldUMLBldLink }

function TBoldUMLBldLink.ExportModel(UMLModel: TUMLModel): Boolean;
var
  ModelAsStrings: TStringList;
  MoldModel: TMoldModel;
  G: IBoldGuard;
begin
  Result := True;
  G := TBoldGuard.Create(MoldModel, ModelAsStrings);
  MoldModel :=  TBoldModelConverter.UMLModelToMold(UMLModel);
  ModelAsStrings := TStringList.Create;
  TMoldBLDrw.ModelToStrings(MoldModel, ModelAsStrings);
  ModelAsStrings.SaveToFile(FileName);
end;

function TBoldUMLBldLink.GetCanExport: Boolean;
begin
  Result := True;
end;

function TBoldUMLBldLink.GetDisplayName: string;
begin
  Result := fFilename;
end;

function TBoldUMLBldLink.GetFileName: string;
begin
  Result := fFilename;
end;

function TBoldUMLBldLink.ImportModel(UMLModel: TUMLModel): Boolean;
var
  MoldModel: TMoldModel;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(MoldModel);
  Result := False;
  if not Assigned(UMLModel) then
    raise EBoldImport.CreateFmt('%s.ImportModel: Must have an UMLModel to import to.', [className]);
  BoldInstalledQueue.DeActivateDisplayQueue;
  try
    UMLModel.BoldSystem.StartTransaction;
    try
      Result := True;
      UMLModel.Clear;
      BoldLog.LogFmtIndent('Start Import Bold Proprietary Model File: %s', [FileName]);
      MoldModel := TMoldBLDRW.ModelFromFile(FileName);
      TBoldModelConverter.MoldModelToUML(MoldModel, UMLModel);
    if BoldLog.ProcessInterruption then
      begin
        BoldLog.Log('Import was aborted, old model restored', ltError);
        UMLModel.BoldSystem.RollbackTransaction
      end
      else
      begin
        UMLModel.BoldSystem.CommitTransaction;
        BoldLog.LogDedent('Done importing model');
      end;
      BoldLog.EndLog;
    except
      on E: Exception do
      begin
        UMLModel.BoldSystem.RollbackTransaction;
        BoldLog.LogFmt('Error: %s', [E.Message], ltError);
      end;
    end;
  finally
    BoldInstalledQueue.ActivateDisplayQueue;
  end;
end;

procedure TBoldUMLBldLink.SetFileName(const Value: string);
begin
  fFilename := Value;
end;

initialization
  BoldUMLModelLinkList.AddLink(Bld_LINKEXTENSION, Bld_LINKDESC, TBoldUMLBldLink);

finalization
  if BoldUMLModelLinkListAssigned then
    BoldUMLModelLinkList.RemoveLink(TBoldUMLBldLink);

end.
