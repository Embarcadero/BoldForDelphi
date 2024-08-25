
{ Global compiler directives }
{$include bold.inc}
unit BoldOTAFileHandler;

interface

uses
  Classes,
  Windows,
  ToolsAPI,
  BoldBase,
  BoldFileHandler,
  BoldDefs,
  BoldOTASupport;

type
  { forward declarations }
  TBoldIOTANotifier = class;
  TBoldIOTAEditorNotifier = class;
  TBoldOTAFileHandler = class;

  TBoldIOTANotifierState = (nsModifying, nsNormal);

  { TBoldOTAFileHandler }
  TBoldOTAFileHandler = class(TBoldFileHandler)
  private
    fModuleCreator: TBoldModuleCreator;
    fOTAEditReader: IOTAEditReader;
    fOTAEditWriter: IOTAEditWriter;
    FOTAModule: IOTAModule;
    FOTASourceEditor: IOTASourceEditor;
    fShowInEditor: Boolean;
    fWasOpen: Boolean;
    function GetEditorSize: Integer;
    function GetModuleCreator: TBoldModuleCreator;
    function GetOTAEditReader: IOTAEditReader;
    function GetOTAEditWriter: IOTAEditWriter;
    function GetOTAModule: IOTAModule;
    function GetOTASourceEditor: IOTASourceEditor;
  protected
    procedure LoadStringList; override;
    procedure DoFlushFile; override;
    procedure CloseFile; override;
  public
    constructor Create(const FileName: string; ModuleType: TBoldModuleType; ShowFileInGuiIfPossible: Boolean; OnInitializeFileContents: TBoldInitializeFileContents); override;
    destructor Destroy; override;
    function FileInProject(const name: string): Boolean;
    function FilePathInProject(const name: string): string;
    function PositionToTextInEditor(const S: string): Boolean;
    procedure SetEditorLine(Line: Integer);    
    property ModuleCreator: TBoldModuleCreator read GetModuleCreator;
    property OTAEditReader: IOTAEditReader read GetOTAEditReader;
    property OTAEditWriter: IOTAEditWriter read GetOTAEditWriter;
    property OTAModule: IOTAModule read GetOTAModule;
    property OTASourceEditor: IOTASourceEditor read GetOTASourceEditor;
  end;

  { TBoldIOTANotifier }
  TBoldIOTANotifier = class(TBoldNonRefCountedObject, IOTANotifier)
  private
    fFileHandler: TBoldOTAFileHandler;
    fState: TBoldIOTANotifierState;
  public
    constructor create(fileHandler: TBoldOTAFileHandler);
    procedure AfterSave; virtual;
    procedure BeforeSave; virtual;
    procedure Destroyed; virtual;
    procedure Modified; virtual;
  end;

  { TBoldIOTAEditorNotifier }
  TBoldIOTAEditorNotifier = class(TBoldIOTANotifier, IOTAEditorNotifier)
    procedure ViewActivated(const View: IOTAEditView); virtual;
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation); virtual;
  end;

implementation

uses
  Dialogs,
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldLogHandler;

constructor TBoldOTAFileHandler.create(const FileName: string; ModuleType: TBoldModuleType; ShowFileInGuiIfPossible: Boolean; OnInitializeFileContents: TBoldInitializeFileContents);
begin
  if OTADEBUG then
    BoldLog.LogFmt(sLogCreatingOTAFileHandler, [FileName]);
  inherited Create(ExtractFileName(FileName), ModuleType, ShowFileInGuiIfPossible, OnInitializeFileContents);
  fModuleCreator := nil;
end;

function TBoldOTAFileHandler.FileInProject(const name: string): Boolean;
var
  Module: IOTAModule;
begin
  Module := FindFileModuleInProject(name, GetOTAProject);
  Result := assigned(Module);
end;

function TBoldOTAFileHandler.FilePathInProject(const name: string): string;
var
  module: IOTAModule;
begin
  Result := '';
  module := FindFileModuleInProject(Name, GetOTAProject);
  if assigned(Module) then
    result := Module.Getfilename;
end;

function TBoldOTAFileHandler.GetOTAModule: IOTAModule;
begin
  result := nil;
  if not assigned(fOTAModule) then
  begin
    if OTADEBUG then
      BoldLog.LogFmt('Getting a module for %s', [FileName]);

    fOtaModule := EnsuredModule(FileName, ModuleCreator, ModuleType = mttext, fWasOpen);
    if OTADEBUG then
    begin
      if fWasOpen then
        BoldLog.LogFmt(sLogModuleWasOpen, [FileName])
      else
        BoldLog.LogFmt(sLogHadToOpenModule, [FileName]);
    end;
  end;

  result := fOTAModule;
end;

function TBoldOTAFileHandler.GetOTASourceEditor: IOTASourceEditor;
var
  i: integer;
  Editor: IOTAEditor;
begin
  if not Assigned(fOTASourceEditor) then
  begin
    for i := 0 to OTAModule.GetModuleFileCount - 1 do
    begin
      Editor := OTAModule.GetModuleFileEditor(i);
      if SameFileName(ExtractFileName(Editor.GetFileName), ExtractFileName(FileName)) and
        (Editor.QueryInterFace(IOTASourceEditor, fOTASourceEditor) = S_OK) then
        break;
    end;

    if not Assigned(fOTASourceEditor) then
      raise EBoldDesignTime.CreateFmt(sUnableToOpenSourceEditor, [filename]);
  end;

  Result := fOTASourceEditor;
end;

procedure TBoldOTAFileHandler.SetEditorLine(Line: Integer);
var
  EditPos: TOTAEditPos;
begin
  try
    with OTASourceEditor.GetEditView(0) do
    begin
      EditPos.Col := 0;
      EditPos.Line := Line;
      SetCursorPos(EditPos);
      EditPos.Col := 1;
      SetTopPos(EditPos);
    end;
  except
    on E: Exception do
      Raise EBoldDesignTime.CreateFmt(sUnableToPositionCursor, [FileName, Line]);
  end;
end;

procedure TBoldOTAFileHandler.LoadStringList;
const
  ChunkSize = 30000;
var
  Buf: PAnsiChar;
  position: integer;
  s: String;
  Size,
  ReadChars: Integer;
begin
  Size := GetEditorSize;
  if Size > 0 then
  begin
    GetMem(Buf, Size + 1);
    try
      position := 0;
      while position < Size do
      begin
        ReadChars := OTAEditReader.GetText(position, buf + position, ChunkSize);
        position := position + ReadChars;
      end;
      Buf[Size] := BoldNULL;
      SetLength(S, Size);
      S := string(Buf);

      StringList.Text := S;
    finally
      FreeMem(Buf, Size + 1);
    end;
  end else
    Stringlist.Clear;
end;

function TBoldOTAFileHandler.PositionToTextInEditor(const S: string): Boolean;
var
  L: Integer;
begin
  Result := False;
  L := GetLineFromStringList(S);
  if L <> -1 then
  begin
    SetEditorLine(L + 1);
    Result := True;
  end;
end;

procedure TBoldOTAFileHandler.DoFlushFile;
begin
  if CheckWriteable(OTAModule.FileName) then
  begin
    OTAEditWriter.DeleteTo(GetEditorSize - 2);
    OTAEditWriter.Insert(PAnsiChar({$IFDEF BOLD_UNICODE}AnsiString{$ENDIF}(StringList.Text)));
    OTAModule.Save(False, True);
  end
  else
  begin
    BoldLog.LogFmt(sModuleReadOnly, [OTAModule.FileName], ltError);
    ShowMessage(SysUtils.Format(sModuleReadOnly, [OTAModule.FileName]));
  end;
end;

function TBoldOTAFileHandler.GetModuleCreator: TBoldModuleCreator;
begin
  if not assigned(fModuleCreator) then
  begin
    fModuleCreator := TBoldModuleCreator.Create(FileName, ModuleType, fShowInEditor);
{    if not FileInProject(Filename) then
      ShowMessage('File not in Project... ' + Filename);}
  end;

  result := fModuleCreator;
end;


function TBoldOTAFileHandler.GetEditorSize: Integer;
const
  ChunkSize = 30000;
var
  buf: array[0..ChunkSize] of AnsiChar;
  ReadChars: integer;
begin
  result := 0;
  repeat
    ReadChars := OTAEditReader.GetText(Result, buf, ChunkSize);
    Result := Result + ReadChars;
  until ReadChars < ChunkSize;
end;

function TBoldOTAFileHandler.GetOTAEditReader: IOTAEditReader;
begin
  if Assigned(fOTAEditWriter) then
    fOTAEditWriter := nil;

  if not Assigned(fOTAEditReader) then
  try
    fOTAEditReader := OTASourceEditor.CreateReader;
  except
    on e: exception do
    begin
      BoldLog.LogFmt(sUnableToCreateReader, [e.message], ltError);
      raise
    end;
  end;
  result := fOTAEditReader;
end;

function TBoldOTAFileHandler.GetOTAEditWriter: IOTAEditWriter;
begin
  if Assigned(fOTAEditReader) then
    fOTAEditReader := nil;

  if not Assigned(fOTAEditWriter) then
    fOTAEditWriter := OTASourceEditor.CreateWriter;

  result := fOTAEditWriter;
end;

destructor TBoldOTAFileHandler.destroy;
begin
  inherited;
  fOTAEditReader := nil;
  fOTAEditWriter := nil;
  fModuleCreator := nil;
end;

{ TBoldIOTANotifier }

procedure TBoldIOTANotifier.AfterSave;
begin
end;

procedure TBoldIOTANotifier.BeforeSave;
begin
end;

constructor TBoldIOTANotifier.create(fileHandler: TBoldOTAFileHandler);
begin
  fFilehandler := FileHandler;
  fState := nsNormal;
end;

procedure TBoldIOTANotifier.Destroyed;
begin
end;

procedure TBoldIOTANotifier.Modified;
begin
{  if (fstate <> nsModifying) and fFileHandler.StringListModified then
    ShowMessage('Oops, a filehandler had a dirty StringList!');
}
{  if fstate <> nsModifying then
    FreeAndNil(fFileHandler.fStringList);
}
end;

{ TBoldIOTAEditorNotifier }

procedure TBoldIOTAEditorNotifier.ViewActivated(const View: IOTAEditView);
begin
end;

procedure TBoldIOTAEditorNotifier.ViewNotification(
  const View: IOTAEditView; Operation: TOperation);
begin
end;

procedure TBoldOTAFileHandler.CloseFile;
begin
  if not OTASourceEditor.GetModified and not fWasOpen then
  begin
    fOTAEditReader := nil;
    fOTAEditWriter := nil;
    fModuleCreator := nil;

    if OTADEBUG then
      BoldLog.Log(SysUtils.format('%s has %d editors', [FileNAme, FOTASourceEditor.GetEditViewCount]));

    fOTASourceEditor := nil;
    if OTADEBUG then
      BoldLog.Log('Closing '+FileName);
    try
      if OTAModule.Close and OTADEBUG then
        BoldLog.Log('Closed '+FileName);
    except
      on e: exception do
        BoldLog.LogFmt(sFailedToCloseModule, [FileName, e.Message]); // do not localize
    end;
    if OTADEBUG then
      BoldLog.Log('Done Closing '+FileName);
    fOTAModule := nil;
  end;
end;

initialization
  BoldPrefferedFileHandlerClass := TBoldOTAFileHandler;

end.

