
{ Global compiler directives }
{$include bold.inc}
unit BoldMMExpert;

interface

uses
  ComObj, ActiveX, BoldMMPlugin_TLB, Forms, Classes, Windows, MMToolsApi, Dialogs;

type
  TBoldMMExpert = class (TAutoObject, IBoldExpert, IMMExpert)
  private

    function GetModelAsString: WideString; safecall;
    function SetModelAsString(const ModelAsString: WideString): WideString; safecall;
    procedure OpenProject(const FileName: WideString); safecall;
    function GetModelAsDelphiString: string;
    function Get_ProjectFileName: WideString; safecall;
    procedure SaveModelToFile;
    procedure EditTaggedValues;
  public
    procedure Destroyed; safecall;
    procedure Execute(Index: Integer); safecall;
    function ExpertID: WideString; safecall;
    function GetMenuPositions(Index: Integer): TMMMenuPosition; safecall;
    function GetMenuShortCuts(Index: Integer): TShortCut; safecall;
    function GetVerbCount: Integer; safecall;
    function GetVerbs(Index: Integer): WideString; safecall;
    property MenuPositions[Index: Integer]: TMMMenuPosition read
            GetMenuPositions;
    property MenuShortCuts[Index: Integer]: TShortCut read GetMenuShortCuts;
    property VerbCount: Integer read GetVerbCount;
    property Verbs[Index: Integer]: WideString read GetVerbs;
    function GetInterface: IUnknown;
    procedure NewProject(const FileName: WideString); safecall;
    procedure SaveProject(SaveAs: WordBool); safecall;
  end;

implementation

uses
  ComServ,
  SysUtils,
  BoldDefs,
  BoldGuard,
  BoldMeta,
  BoldBld,
  BoldUMLModel,
  BoldUMLModelConverter,  
  BoldUMLModelSupport,
  BoldMMImporter,
  BoldUMLModelStreamer,
  BoldUMLModelDataModule,
  BoldMMTVEditor
  ;

{
********************************** TExpertAPI **********************************
}
procedure TBoldMMExpert.Destroyed;
begin
end;

procedure TBoldMMExpert.Execute(Index: Integer);
begin
  case Index of
    0:
      SaveModelToFile;
    1:
      EditTaggedValues;
    else
      MessageBox(0, PChar('ExecuteVerb = ' + IntToStr(Index)), nil, 0);
  end;
end;

function TBoldMMExpert.GetMenuPositions(Index: Integer): TMMMenuPosition;
begin
  case Index of
    0, 1: Result := mpToolsMenu;
  else
    Assert(False);
    Result := mpToolsMenu;
  end;
end;

function TBoldMMExpert.GetMenuShortCuts(Index: Integer): TShortCut;
begin
  Result := 0;
end;

function TBoldMMExpert.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TBoldMMExpert.GetVerbs(Index: Integer): WideString;
begin
  case Index of
    0: Result := 'Save in Bold format';
    1: Result := 'Bold tagged value editor';
  else
    Assert(False);
    Result := '';
  end;
end;


function TBoldMMExpert.ExpertID: WideString;
begin
  Result := 'BoldSoft.BoldExpert';
end;

function TBoldMMExpert.GetInterface: IUnknown;
begin
  GetInterface := MMToolServices;
end;

function TBoldMMExpert.GetModelAsDelphiString: string;
var
  G: IBoldGuard;
  Importer: TMMModelImporter;
  UMLModel: TUMLModel;
  Boldify: TBoldUMLBoldify;
  MoldModel: TMoldModel;
  ModelAsStrings: TStrings;
begin
  G := TBoldGuard.Create(Importer, Boldify, MoldModel);
  EnsureModelEditDataModule;
  dmModelEdit.bshUMLModel.Active := true;
  UMLModel := TUMLModel.Create(dmModelEdit.bshUMLModel.System);
  Boldify := TBoldUMLBoldify.Create;
  ModelAsStrings := TStringList.Create;
  Importer := TMMModelImporter.Create(UMLModel, MMToolServices);
  Importer.RawImport;
  TBoldUMLSupport.EnsureBoldTaggedValuesInModel(UMLModel);
  TBoldUMLSupport.Flatten(UMLModel);
  Boldify.Boldify(UMLModel);
  MoldModel :=  TBoldModelConverter.UMLModelToMold(UMLModel);
  TMoldBLDRW.ModelToStrings(MoldModel, ModelAsStrings);
  Result := ModelAsStrings.Text;
end;

procedure TBoldMMExpert.SaveModelToFile;
var
  Stream: TStream;
  ModelAsString: string;
  sdOpenModel: TSaveDialog;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(Stream, sdOpenModel);
  ModelAsString := GetModelAsDelphiString;
  sdOpenModel := TSaveDialog.Create(nil);
  sdOpenModel.Filter := 'Bold Proprietary Format (*.bld)|*.bld| All files (*.*)|*.*';
  if sdOpenModel.Execute then
  begin
    if ExtractFileExt(sdOpenModel.FileName) = '' then
      sdOpenModel.FileName := ChangeFileExt(sdOpenModel.FileName, '.bld');
    Stream := TFileStream.Create(sdOpenModel.FileName, fmCreate);
    Stream.WriteBuffer(Pointer(ModelAsString)^, Length(ModelAsString));
  end;
end;

function TBoldMMExpert.GetModelAsString: WideString;
begin
  Result := GetModelAsDelphiString;
end;

procedure TBoldMMExpert.OpenProject(const FileName: WideString);
begin
 MMToolServices.ProjectManager.OpenProject(FileName);
end;

function TBoldMMExpert.Get_ProjectFileName: WideString;
begin
  Result :=  MMToolServices.ProjectManager.ProjectName;
end;

procedure TBoldMMExpert.EditTaggedValues;
var
  form: TfrmBoldMMTVEdit;
begin
  form := TfrmBoldMMTVEdit.Create(nil, MMToolServices);
  form.ShowModal;
  form.Free;
end;

function TBoldMMExpert.SetModelAsString(
  const ModelAsString: WideString): WideString;
begin
  Result := 'Export not implemented in this version of expert';
end;

procedure TBoldMMExpert.NewProject(const FileName: WideString);
begin
 MMToolServices.ProjectManager.NewProject(FileName)
end;

procedure TBoldMMExpert.SaveProject(SaveAs: WordBool);
begin
 MMToolServices.ProjectManager.SaveProject(SaveAs);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TBoldMMExpert, CLASS_CBoldExpert,
    ciInternal, tmSingle);
end.
