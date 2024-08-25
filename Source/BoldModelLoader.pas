
{ Global compiler directives }
{$include bold.inc}
unit BoldModelLoader;

interface
uses
  Classes,
  Dialogs,
  BoldDefs,
  BoldModel,
  BoldUMLModelLink,
  BoldUMLModelDataModule,
  BoldUMLModelConverter,
  BoldUMLModel;

type
  TBoldModelLoader = class(TObject)
  protected
    procedure ModelToUML(LinkClass: TBoldUMLModelLinkClass; FileName: String; UMLModel: TUMLModel);
    procedure EnsureHasLinkClass(FileName: String);
  public
    function GetUMLLinkClass(FileName: string): TBoldUMLModelLinkClass;
    function ModelLoadDialog(InitialPath: String): String;
    procedure ModelFileToUML(ModelFileName: String; UMLModel: TUMLModel);
    procedure ModelFileToBoldModel(ModelFileName: String; BoldModel: TBoldModel);
  end;

function ModelLoader: TBoldModelLoader;

implementation

uses
  SysUtils,
  BoldUtils;

var
  G_ModelLoader: TBoldModelLoader = nil;

function ModelLoader: TBoldModelLoader;
begin
  if not assigned(G_ModelLoader) then
    G_ModelLoader := TBoldModelLoader.Create;
  result := G_ModelLoader;
end;

{ TBoldModelLoader }

procedure TBoldModelLoader.EnsureHasLinkClass(FileName: String);
begin
  if not assigned(GetUMLLinkClass(FileName)) then
    raise EBold.Create('Can not find any linkclass for .'+ExtractFileExt(FileName));
end;

function TBoldModelLoader.GetUMLLinkClass(FileName: string): TBoldUMLModelLinkClass;
begin
  result := BoldUMLModelLinkList.LinkClasses[-1, ExtractFileExt(FileName)];
end;

procedure TBoldModelLoader.ModelFileToBoldModel(ModelFileName: String;
  BoldModel: TBoldModel);
begin
  ModelFileToUML(ModelFileName, BoldModel.EnsuredUMLModel);
end;

procedure TBoldModelLoader.ModelFileToUML(ModelFileName: String; UMLModel: TUMLModel);
var
  UMLLinkClass: TBoldUMLModelLinkClass;
begin
  EnsureHasLinkClass(ModelFileName);
  UMLLinkClass := GetUMLLinkClass(ModelFileName);
  ModelToUML(UMLLinkClass, ModelFileName, UMLModel)
end;

function TBoldModelLoader.ModelLoadDialog(InitialPath: String): String;
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.InitialDir := InitialPath;
    OpenDialog.Filter :=  Copy(BoldUMLModelLinkList.FileFilter, 1, Length(BoldUMLModelLinkList.FileFilter) - Length(ALLFILESFILTER)) ;
    if OpenDialog.Execute then
      result := OpenDialog.FileName
    else
      result := '';
  finally
    OpenDialog.Free;
  end;
end;

procedure TBoldModelLoader.ModelToUML(LinkClass: TBoldUMLModelLinkClass; FileName: String; UMLModel: TUMLModel);
var
  UMLLink: TBoldUMLModelLink;
begin
  UMLLink := LinkClass.create(nil);
  try
    UMLLink.FileName := FileName;
    UMLLink.ImportModel(UMLModel);
  finally
    UMLLink.Free;
  end;
end;

initialization

finalization
  FreeAndNil(G_ModelLoader);
end.
