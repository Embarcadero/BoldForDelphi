
{ Global compiler directives }
{$include bold.inc}
unit BoldComEditors;

interface

uses
  Classes,
  DesignEditors,
  ToolsAPI,
  BoldPropertyEditors,
  BoldComServerHandles;

type
  { forward delcarations }
  TBoldComServerUnitFile = class;
  TBoldComServerUnitCreator = class;
  TBoldComServerHandleComponentEditor = class;
  TBoldComServerClassPropertyEditor = class;

  { TBoldComServerUnitFile }
  TBoldComServerUnitFile = class(TInterfacedObject, IOTAFile)
  private
    FCreator: TBoldComServerUnitCreator;
    FUnitIdent: string;
  protected
    function GetSource: string;
    function GetAge: TDateTime;
  public
    constructor Create(const UnitIdent: string; Creator: TBoldComServerUnitCreator);
    property Creator: TBoldComServerUnitCreator read FCreator;
  end;

  { TBoldComServerUnitCreator }
  TBoldComServerUnitCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FServerHandle: TBoldComServerHandle;
    FUnitName: string;
  protected
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  public
    constructor Create(const UnitName: string; ServerHandle: TBoldComServerHandle);
    property ServerHandle: TBoldComServerHandle read FServerHandle;
  end;

  { TBoldComServerHandleComponentEditor }
  TBoldComServerHandleComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TBoldComServerClassPropertyEditor }
  TBoldComServerClassPropertyEditor = class(TBoldStringSelectionProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

implementation

uses
  SysUtils,
  DesignIntf,
  Windows,
  Dialogs;

constructor TBoldComServerUnitFile.Create(const UnitIdent: string;
  Creator: TBoldComServerUnitCreator);
begin
  FCreator := Creator;
  FUnitIdent := UnitIdent;
end;

function TBoldComServerUnitFile.GetSource: string;
var
  I: Integer;
  Item: TBoldComClass;
begin
  with TStringList.Create do
  begin
    try
      Add(Format('unit %s;', [FUnitIdent]));
      Add('');
      Add('interface');
      Add('');
      Add('implementation');
      Add('');
      Add('uses');
      Add('  ComServ, BoldComServer;');
      Add('');
      Add('const');
      for I := 0 to Creator.ServerHandle.Classes.Count - 1 do
      begin
        Item := Creator.ServerHandle.Classes[I];
        Add(Format('  %s_CLSID: TGUID = ''%s'';', [Item.Name, Item.CLSID]));
      end;
      Add('');
      Add('initialization');
      for I := 0 to Creator.ServerHandle.Classes.Count - 1 do
      begin
        Item := Creator.ServerHandle.Classes[I];
        Add('  TBoldComServerConnectionFactory.Create(ComServer, ');
        Add(Format('    %s_CLSID, ''%s'', ''%s'');', [Item.Name, Item.Name, Item.Description]));
      end;
      Add('');
      Add('end.');
      Result := Text;
    finally
      Free;
    end;
  end;
end;

function TBoldComServerUnitFile.GetAge: TDateTime;
begin
  Result := -1;
end;

{------------------------------------------------------------------------------}

constructor TBoldComServerUnitCreator.Create(const UnitName: string;
  ServerHandle: TBoldComServerHandle);
begin
  FServerHandle := ServerHandle;
  FUnitName := UnitName;
end;

function TBoldComServerUnitCreator.GetCreatorType: string;
begin
  Result := sUnit;
end;

function TBoldComServerUnitCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TBoldComServerUnitCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TBoldComServerUnitCreator.GetOwner: IOTAModule;
var
  Module: IOTAModule;
begin
  Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if Assigned(Module) and (Module.OwnerCount > 0) then
    Result := Module.Owners[0]
  else
    Result := nil;
end;

function TBoldComServerUnitCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TBoldComServerUnitCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TBoldComServerUnitCreator.GetImplFileName: string;
begin
  Result := FUnitName;
end;

function TBoldComServerUnitCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TBoldComServerUnitCreator.GetFormName: string;
begin
  Result := '';
end;

function TBoldComServerUnitCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TBoldComServerUnitCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TBoldComServerUnitCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TBoldComServerUnitCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TBoldComServerUnitCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TBoldComServerUnitFile.Create(ChangeFileExt(ExtractFileName(FUnitName), ''), Self);
end;

function TBoldComServerUnitCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TBoldComServerUnitCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

{------------------------------------------------------------------------------}

procedure TBoldComServerHandleComponentEditor.ExecuteVerb(Index: Integer);
var
  Ms: IOTAModuleServices;
  UName, CName, FName: string;
  ServerHandle: TBoldComServerHandle;
begin
  if Index = 0 then
  begin
    ServerHandle := Component as TBoldComServerHandle;
    if ServerHandle.Classes.Count > 0 then
    begin
      if MessageDlg('This will generate server code, continue?',
        mtConfirmation, [mbYes, mbNo], 0) = idYes then
      begin
        BorlandIDEServices.QueryInterface(IOTAModuleServices, Ms);
        if Assigned(Ms) then
        begin
          Ms.GetNewModuleAndClassName('', UName, CName, FName);
          Ms.CreateModule(TBoldComServerUnitCreator.Create(FName, ServerHandle));
        end;
      end;
    end
    else
      MessageDlg('Can''t generate code, no class(es) defined.', mtWarning, [mbOK], 0);
  end;
end;

function TBoldComServerHandleComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
  begin
    Result := 'Generate server code...';
  end;
end;

function TBoldComServerHandleComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TBoldComServerClassPropertyEditor.GetValueList(List: TStrings);
var
  I: Integer;
  ExportHandle: TBoldComExportHandle;
  DesignerSelections: IDesignerSelections;
  ServerHandle: TBoldComServerHandle;
begin
  DesignerSelections := CreateSelectionList;
  Designer.GetSelections(DesignerSelections);
  ServerHandle := nil;
  for I := 0 to DesignerSelections.Count - 1 do
  begin
    ExportHandle := DesignerSelections.Items[I] as TBoldComExportHandle;
    if I = 0 then
      ServerHandle := ExportHandle.ServerHandle
    else
    begin
      if ExportHandle.ServerHandle <> ServerHandle then
      begin
        ServerHandle := nil;
        Break;
      end;
    end;
  end;
  if Assigned(ServerHandle) then
    ServerHandle.Classes.GetClassNames(List);
end;

end.
