
{ Global compiler directives }
{$include bold.inc}
unit BoldFileHandler;

interface

uses
  Classes,
  BoldLogHandler,
  BoldDefs,
  BoldContainers;

type
  TBoldFileHandler = class;
  TBoldDiskFileHandler = class;
  TBoldFileHandlerClass = class of TBoldFileHandler;

  TBoldInitializeFileContents = procedure(StringList: TStringList) of Object;

  {---TBoldFileHandler---}
  TBoldFileHandler = class
  private
    fFileFilter: String;
    FFileName: string;
    fIndentLevel: integer;
    fLastWasNewLine: Boolean;
    fModuleType: TBoldModuleType;
    FOnInitializeFileContents: TBoldInitializeFileContents;
    fSetFileName: string;
    fStringList: TStringList;
    fStringListModified: Boolean;
    procedure ContentsChanged(sender: TObject);
    function GetContent(i: integer): string;
    function GetCount: integer;
    function GetIsEmpty: boolean;
    function GetStringList: TStringList;
    function IndentSpace: string;
  protected
    function CheckWriteable(FName: string = ''): Boolean;
    function GetLineFromStringList(const S: string): Integer; {Zero based}
    procedure DoFlushFile; virtual; abstract;
    procedure CloseFile; virtual;
    procedure LoadStringList; virtual; abstract;
    property StringList: TStringList read GetStringList;
  public
    constructor Create(const FileName: string; ModuleType: TBoldModuleType; ShowFileInGuiIfPossible: Boolean; OnInitializeFileContents: TBoldInitializeFileContents); virtual;
    destructor Destroy; override;
    procedure AddString(const s: string);
    procedure AddStrings(s: TStrings);
    procedure Clear;
    procedure Dedent;
    procedure EndBlock(const AddNewLine: boolean);
    procedure FlushFile;
    procedure Format(const Fmt: string; Args: array of const);
    procedure FormatLn(const Fmt: string; Args: array of const);
    procedure Indent;
    procedure ModifyContents(const NewContents: string; Replace: Boolean);
    procedure NewLine;
    procedure StartBlock;
    procedure InitializeStringList;
    function TextFoundInFile(const S: string): Boolean;
    procedure WriteLn(const Str: string);
    property Content[i: integer]: string read GetContent;
    property Count: integer read GetCount;
    property FileFilter: String read fFileFilter write fFileFilter;
    property Filename: string read FFileName write fFileName;
    property IsEmpty: boolean read GetIsEmpty;
    property LastWasNewLine: Boolean read fLastWasNewLine;
    property ModuleType: TBoldModuleType read fModuleType;
    property OnInitializeFileContents: TBoldInitializeFileContents read FOnInitializeFileContents;
    property SetFileName: string read fSetFileName;
    property StringListModified: Boolean read fStringListModified;
  end;

  { TBoldDiskFileHandler }
  TBoldDiskFileHandler = class(TBoldFileHandler)
  protected
    procedure DoFlushFile; override;
    procedure LoadStringList; override;
  end;

procedure BoldCloseAllFilehandlers;
procedure BoldRemoveUnchangedFilesFromEditor;
function BoldFileHandlerList: TBoldObjectArray;
function BoldFileHandlerForFile(path, FileName: String; ModuleType: TBoldModuleType; ShowInEditor: Boolean; OnInitializeFileContents: TBoldInitializeFileContents): TBoldFileHandler;

var
  BOLD_INDENTSIZE: integer = 2;
  BoldFilehandlerLog: boolean = true;
  BoldPrefferedFileHandlerClass: TBoldFileHandlerClass = TBoldDiskFileHandler;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  Dialogs;

var
  G_FileHandlerList: TBoldObjectArray;

function BoldFileHandlerList: TBoldObjectArray;
begin
  if not assigned(G_FileHandlerList) then
    G_FileHandlerList := TBoldObjectArray.Create(0, []);
  result := G_FileHandlerList;
end;

function BoldFileHandlerForFile(path, FileName: String; ModuleType: TBoldModuleType; ShowInEditor: Boolean; OnInitializeFileContents: TBoldInitializeFileContents): TBoldFileHandler;
var
  i: integer;
begin
  try
    for i := 0 to BoldFileHandlerList.Count - 1 do
    begin
      if AnsiUpperCase((BoldFileHandlerList[i] as TBoldFileHandler).SetFileName) = AnsiUpperCase(Filename) then
      begin
        result := BoldFileHandlerList[i] as TBoldFileHandler;
        exit;
      end;
    end;

    result := BoldPrefferedFileHandlerClass.Create(path + FileName, ModuleType, ShowInEditor, OnInitializeFileContents);
    result.InitializeStringList;
  except
    on e: exception do
      raise EBold.CreateFmt(sUnableToCreateFileHandle, [FileName, e.message]);
  end;
end;

procedure BoldCloseAllFilehandlers;
var
  i: integer;
begin
  if assigned(G_FileHandlerlist) then
  begin
    for i := G_FileHandlerlist.Count - 1 Downto 0 do
      (G_FileHandlerlist[i] as TBoldFileHandler).Free;
    G_FileHandlerList.Clear;
  end;
end;

procedure BoldRemoveUnchangedFilesFromEditor;
var
  i: integer;
  FileHandler: TBoldFileHandler;
begin
  if assigned(G_FileHandlerlist) then
    for i := G_FileHandlerlist.Count - 1 Downto 0 do
    begin
      FileHandler := G_FileHandlerlist[i] as TBoldFileHandler;
      if not FileHandler.fStringListModified then
      begin
        (G_FileHandlerlist[i] as TBoldFileHandler).CloseFile;
        (G_FileHandlerlist[i] as TBoldFileHandler).Free;
      end;
    end;
end;

{---TBoldFileHandler---}

constructor TBoldFileHandler.Create(const FileName: string; ModuleType: TBoldModuleType; ShowFileInGuiIfPossible: Boolean; OnInitializeFileContents: TBoldInitializeFileContents);
begin
  FFileName := FileName;
  fSetFileName := FileName;
  fModuleType := ModuleType;
  fLastWasNewLIne := true;
  fIndentLevel := 0;
  FOnInitializeFileContents := OnInitializeFileContents;
  fFileFilter := sFileHandlerMask;
  BoldFileHandlerList.Add(self);
end;

destructor TBoldFileHandler.Destroy;
begin
  try
    FlushFile;
  except
    on e:Exception do
      ShowMessage(SysUtils.Format(sFileSaveProblem, [e.Message]));
  end;
  FreeAndNil(fStringList);
  BoldFileHandlerList.remove(self);
  inherited;
end;

function TBoldFileHandler.GetContent(i: integer): string;
begin
  result := StringList[i];
end;

function TBoldFileHandler.GetCount: integer;
begin
  result := StringList.Count;
end;

function TBoldfileHandler.IndentSpace: string;
begin
  Result := StringOfChar(' ', fIndentLevel * BOLD_INDENTSIZE);
end;

procedure TBoldfileHandler.AddString(const s: string);
begin
  if lastWasNewLine then
    StringList.Add(IndentSpace + s)
  else
    StringList[StringList.Count - 1] := StringList[StringList.Count - 1] + s;
  fLastWasNewLine := false;
end;

procedure TBoldfileHandler.StartBlock;
begin
  Writeln('begin');
  Indent;
end;

procedure TBoldfileHandler.EndBlock(const AddNewLine: boolean);
begin
  Dedent;
  Writeln('end;');
  if AddNewLine then
    NewLine;
end;

procedure TBoldfileHandler.NewLine;
begin
  if lastWasNewLine then
    StringList.Add('');
  fLastWasNewLine := true;
end;

procedure TBoldfileHandler.WriteLn(const Str: string);
begin
  AddString(Str);
  NewLine;
end;

procedure TBoldfileHandler.Indent;
begin
  Inc(fIndentLevel);
end;

procedure TBoldfileHandler.Dedent;
begin
  Dec(fIndentLevel);
  if fIndentLevel < 0 then
    fIndentLevel := 0;
end;

procedure TBoldfileHandler.FormatLn(const Fmt: string; Args: array of const);
begin
  WriteLn(SysUtils.Format(Fmt, Args))
end;

procedure TBoldfileHandler.format(const Fmt: string; Args: array of const);
begin
  AddString(SysUtils.Format(Fmt, Args))
end;

procedure TBoldFileHandler.Clear;
begin
  StringList.Clear;
end;

function TBoldFileHandler.GetIsEmpty: boolean;
begin
  result := StringList.Count = 0;
end;

procedure TBoldFileHandler.AddStrings(s: TStrings);
var
  i: integer;
begin
  for i := 0 to s.Count - 1 do
    Writeln(s[i]);
end;

procedure TBoldFileHandler.InitializeStringList;
begin
  if not assigned(fStringList) then
  begin
    fStringList := TStringList.Create;
    fStringList.OnChange := ContentsChanged;
    LoadStringList;
    if (StringList.Count = 0) and assigned(onInitializeFileContents) then
      onInitializeFileContents(StringList);
    fStringListModified := false;
  end;
end;

function TBoldFileHandler.GetStringList: TStringList;
begin
  InitializeStringList;
  Result := fStringList;
end;

function TBoldFileHandler.GetLineFromStringList(const S: string): Integer;
var
  I: Integer;
  Uc: string;
begin
  Result := -1;
  Uc := AnsiUppercase(S);
  for i := 0 to StringList.Count - 1 do
    if Pos(Uc, AnsiUppercase(StringList[i])) > 0 then
    begin
      Result := i;
      Break;
    end;
end;

function TBoldFileHandler.TextFoundInFile(const S: string): Boolean;
begin
  Result := GetLineFromStringList(S) <> -1;
end;

procedure TBoldFileHandler.ModifyContents(const NewContents: string; Replace: Boolean);
begin
  if Replace then
    StringList.Text := NewContents
  else
    StringList.Text := StringList.Text + BOLDCRLF + NewContents;
end;

procedure TBoldFileHandler.ContentsChanged(sender: tObject);
begin
  fStringListModified := true;
end;

procedure TBoldFileHandler.FlushFile;
begin
  if fStringListModified then
    DoFlushFile;
  fStringlistModified := false;
  FreeAndNil(fStringList);
end;

{ TBoldDiskFileHandler }

procedure TBoldDiskFileHandler.DoFlushFile;
begin
  if ExtractFilePath(FileName) = '' then
    fFileName := GetCurrentDir + PathDelim + FileName;
  if CheckWriteable then
  begin
    StringList.SaveToFile(FileName);
    BoldLog.LogFmt(sSaved, [FileName]);
  end
  else
  begin
    BoldLog.LogFmt(sModuleReadOnly, [FFileName], ltError);
    ShowMessage(SysUtils.Format(sModuleReadOnly, [fFileName]));
  end;
end;

procedure TBoldDiskFileHandler.LoadStringList;
begin
  if FileExists(FileName) then
    StringList.LoadFromFile(Filename)
end;

function TBoldFileHandler.CheckWriteable(FName: string = ''): Boolean;
begin
  if FName = '' then
    FName := FileName;
  Result := not (FileExists(FName) and FileIsReadOnly(FName));
end;

procedure TBoldFileHandler.CloseFile;
begin
end;

initialization

finalization
  FreeAndNil(G_FileHandlerList);

end.
