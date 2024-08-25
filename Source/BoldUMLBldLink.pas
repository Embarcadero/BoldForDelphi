{ Global compiler directives }
{$include bold.inc}
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
    function SortCompare(Index1, Index2: integer): integer;
    procedure SortExchange(Index1, Index2: integer);
  public
    function ExportModel(UMLModel: TUMLModel): Boolean; override;
    function ImportModel(UMLModel: TUMLModel): Boolean; override;
    class function getSortStatus: Boolean;
    class procedure setSortStatus(sortStatus: Boolean);
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
  BoldUMLModelConverter,
  BoldSorter;

var
  msort: Boolean;
  MoldModel: TMoldModel;

{ TBoldUMLBldLink }

function TBoldUMLBldLink.ExportModel(UMLModel: TUMLModel): Boolean;
var
  ModelAsStrings: TStringList;
  G: IBoldGuard;
begin
  Result := True;
  G := TBoldGuard.Create(MoldModel, ModelAsStrings);
  MoldModel :=  TBoldModelConverter.UMLModelToMold(UMLModel);
  if getSortStatus then
    BoldSort(0, MoldModel.Classes.Count-1, SortCompare, SortExchange);
  ModelAsStrings := TStringList.Create;
  TMoldBLDrw.ModelToStrings(MoldModel, ModelAsStrings);
  ModelAsStrings.SaveToFile(FileName);
end;

{ 25-10-05 Bero
  Tried to implement BoldSort()
  It works but it don't sort the same way as bubblesort above
  Also experimenting with BoldElement.CompareTo() without success...
  }
function TBoldUMLBldLink.SortCompare(Index1, Index2: integer): integer;
var
  vToolId1, vToolId2: string;
begin
  Result := 0;
  vToolId1 := MoldModel.Classes[Index1].NonDefaultTaggedValuesCommaText;
  vToolId1:=copy(vToolId1,1,Ansipos(',',vToolId1)-1);
  if (Length(vToolId1)>0) and (Ansipos('persistent',vToolId1)=0)
    and (Ansipos('transient',vToolId1)=0) then
  begin
    vToolId1:=copy(vToolId1,AnsiPos('=',vToolId1)+1,Length(vToolId1));
    vToolId2 := MoldModel.Classes[Index2].NonDefaultTaggedValuesCommaText;
    vToolId2:=copy(vToolId2,1,Ansipos(',',vToolId2)-1);

    if (Length(vToolId2)>0) and (Ansipos('persistent',vToolId2)=0)
      and (Ansipos('transient',vToolId2)=0) then
    begin
      vToolId2:=copy(vToolId2,AnsiPos('=',vToolId2)+1,Length(vToolId2));
      if (Length(vToolId1) >= Length(vToolId2)) and (('$' + vToolId1)<('$' + vToolId2)) then
        Result := 1;
    end;
  end;
end;

procedure TBoldUMLBldLink.SortExchange(Index1, Index2: integer);
begin
  MoldModel.Classes.Exchange(Index1,Index2);
end;

{:Static method, no need for a classinstance. Get the status of the sort flag
@return true-sorted false-unsorted}
class function TBoldUMLBldLink.getSortStatus: Boolean;
begin
  Result := msort;
end;

{:Static method, no need for a classinstance. Set the status of the sort flag
@param sortstatus true-sorted false-unsorted}
class procedure TBoldUMLBldLink.setSortStatus(sortStatus: Boolean);
begin
  msort := sortStatus;
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
  UndoWasActive: boolean;
begin
  G := TBoldGuard.Create(MoldModel);
  Result := False;
  if not Assigned(UMLModel) then
    raise EBoldImport.CreateFmt('%s.ImportModel: Must have an UMLModel to import to.', [className]);
  UndoWasActive := UMLModel.BoldSystem.UndoHandlerInterface.Enabled;
  UMLModel.BoldSystem.UndoHandlerInterface.Enabled := false;
  TBoldQueueable.DeActivateDisplayQueue;
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
    UMLModel.BoldSystem.UndoHandlerInterface.Enabled := UndoWasActive;
    TBoldQueueable.ActivateDisplayQueue;
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
