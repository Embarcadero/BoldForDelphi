
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLXMILink;

interface

uses
  Variants,
  BoldUMLModelLink,
  BoldUMLModel,
  Classes;

type
  { forward declarations }
  TBoldUMLXMILink = class;

  { TBoldUMLXMILink }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldUMLXMILink = class(TBoldUMLModelLink)
  private
    fFilename: String;
    fValidateInput: Boolean;
    fTranslateRoseTVs: Boolean;
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
    property TranslateRoseTaggedValues: Boolean read fTranslateRoseTVs write fTranslateRoseTVs default false;
  end;

const
  XMI_LINKEXTENSION: string = '.xmi';
  XMI_LINKDESC: string = 'XMI file';

implementation
uses
  SysUtils,
  BoldDefs,
  BoldUtils,
  BoldGuard,
  BoldQueue,
  BoldLogHandler,
  BoldUMLXMIImporter,
  BoldUMLModelMOFAdapters,
  BoldXMI10Exporter;

{ TBoldUMLXMILink }

function TBoldUMLXMILink.ExportModel(UMLModel: TUMLModel): Boolean;
var
  exporter: TBoldXMI10Exporter;
begin
  exporter := TBoldXMI10Exporter.Create(self);
  try
    BoldLog.StartLog('Model Export');
    exporter.RawExport(TBoldUMLMOFObjectAdapter.Create(UMLModel, self));
    BoldLog.EndLog;
  finally
    exporter.Free;
  end;    
  result := true;
end;

function TBoldUMLXMILink.GetCanExport: Boolean;
begin
  Result := true;
end;

function TBoldUMLXMILink.GetDisplayName: string;
begin
  Result := fFilename;
end;

function TBoldUMLXMILink.GetFileName: string;
begin
  Result := fFilename;
end;

function TBoldUMLXMILink.ImportModel(UMLModel: TUMLModel): Boolean;
var
  G: IBoldGuard;
  Importer: TBoldUMLXMIImporter;
begin
  G := TBoldGuard.Create(Importer);
  Result := False;
  if not Assigned(UMLModel) then
    raise EBoldImport.CreateFmt('%s.ImportModel: Must have an UMLModel to import to.', [className]);
  Importer := TBoldUMLXMIImporter.Create(Self, UMLModel);
  TBoldQueueable.DeActivateDisplayQueue;
  try
    UMLModel.BoldSystem.StartTransaction;
    try
      Result := True;
      UMLModel.Clear;
      BoldLog.StartLog('Model import');
      Importer.RawImport;
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

procedure TBoldUMLXMILink.SetFileName(const Value: string);
begin
  fFilename := Value;
end;

initialization
  BoldUMLModelLinkList.AddLink(XMI_LINKEXTENSION, XMI_LINKDESC, TBoldUMLXMILink);

finalization
  if BoldUMLModelLinkListAssigned then
    BoldUMLModelLinkList.RemoveLink(TBoldUMLXMILink);

end.
