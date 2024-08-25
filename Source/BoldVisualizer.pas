unit BoldVisualizer;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  ToolsAPI,

  JSONTreeView,
  JSONDoc,
  // Bold
  BoldElements;

type
  TAvailableState = (asAvailable, asProcRunning, asOutOfScope, asNotAvailable);

  TBoldViewerFrame = class(TFrame, IOTADebuggerVisualizerExternalViewerUpdater,
    IOTAThreadNotifier, IOTAThreadNotifier160)
    procedure JSONTreeViewDblClick(Sender: TObject);
  private
    FOwningForm: TCustomForm;
    FClosedProc: TOTAVisualizerClosedProcedure;
    FJsonDocument: TJSONDocument;
    FJsonTreeView: TJSONTreeView;
    FExpression: string;
    FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
    FDeferredError: Boolean;
    FAvailableState: TAvailableState;
    function Evaluate(AExpression: string): string;
    procedure ShowJson(const AJsonString: string);
    function FromDbgStrToText(const AText: string): string;
    procedure LoadJsonFromFile(const AFileName: string);
{
    function GetDelimiter: string;
    function GetStrictDelimiter: Boolean;
    function GetDelimitedText: string;
    function GetText: string;
    function FromDbgStrToText(const AText: string): string;
}
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
    procedure SetForm(AForm: TCustomForm);
//    procedure AddStringListItems(const Expression, TypeName, EvalResult: string);
    procedure AddBoldElement(const Expression, TypeName, EvalResult: string);

    { IOTAThreadNotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer); overload;
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
    { IOTAThreadNotifier160 }
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress: TOTAAddress; ResultSize: LongWord; ReturnCode: Integer); overload;
  end;

procedure Register;

implementation

uses
  DesignIntf, Actnlist, ImgList, Menus, IniFiles, System.IOUtils;

{$R *.dfm}

resourcestring
  sBoldVisualizerName = 'Bold Visualizer for Delphi';
  sBoldVisualizerDescription = 'Displays details of TBoldElement instance';
  sMenuText = 'Show Element';
  sFormCaption = 'Bold Visualizer for %s';
  sProcessNotAccessible = 'process not accessible';
  sValueNotAccessible = 'value not accessible';
  sOutOfScope = 'out of scope';

var
  DebuggerBoldVisualizer: IOTADebuggerVisualizer;

type
  IFrameFormHelper = interface
    ['{0FD4A98F-CE6B-422A-BF13-14E59707D3B2}']
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Form: TCustomFrame);
  end;

  TBoldVisualizerForm = class(TInterfacedObject, INTACustomDockableForm, IFrameFormHelper)
  private
    fFrame: TBoldViewerFrame;
    fForm: TCustomForm;
    //fBoldElement: TBoldElement;
    FExpression: string;
  public
//    constructor Create(const ABoldElement: TBoldElement);
    constructor Create(const Expression: string);
    { INTACustomDockableForm }
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetIdentifier: string;
    function GetMenuActionList: TCustomActionList;
    function GetMenuImageList: TCustomImageList;
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu);
    function GetToolbarActionList: TCustomActionList;
    function GetToolbarImageList: TCustomImageList;
    procedure CustomizeToolBar(ToolBar: TToolBar);
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string);
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
    function GetEditState: TEditState;
    function EditAction(Action: TEditAction): Boolean;
    { IFrameFormHelper }
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Frame: TCustomFrame);
  end;

  TDebuggerBoldVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizerExternalViewer)
  public
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string; Suggestedleft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
  end;

procedure Register;
begin
  DebuggerBoldVisualizer := TDebuggerBoldVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(DebuggerBoldVisualizer);
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(DebuggerBoldVisualizer);
    DebuggerBoldVisualizer := nil;
  end;
end;

{ TBoldViewerFrame }

procedure TBoldViewerFrame.AddBoldElement(const Expression, TypeName,
  EvalResult: string);
var
  result: string;
  TempFile: string;
  EvalExpression: string;
  DebugAddress: string;
  DebuggerServices: IOTADebuggerServices;
  SrcMemStreamAddr: string;
begin
  Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices);
  FAvailableState := asAvailable;
  FExpression := Expression;
  EvalExpression := Format('%s.stringRepresentation[brJson]', [FExpression]);
  Result := Evaluate(EvalExpression);
  if Length(Result) < 4094 then
    ShowJson(FromDbgStrToText(Result))
  else
  begin
    TempFile := TPath.GetTempFileName;
    try
      // workaround for debugger 4k result length limit.
      // create StringStream save to temp file and load it here
      EvalExpression := Format('NativeInt(TStringStream.Create(%s))', [EvalExpression]);
      DebugAddress := Evaluate(EvalExpression);
      if DebugAddress <> '' then
      begin
        EvalExpression := Format('TStringStream(%s).SaveToFile(''%s'')', [DebugAddress, TempFile]);
        Evaluate(EvalExpression);
        EvalExpression := Format('TStringStream(%s).free', [DebugAddress]);
        Evaluate(EvalExpression);
        LoadJsonFromFile(TempFile);
      end;
    finally
      DeleteFile(TempFile);
    end;
  end;
end;

procedure TBoldViewerFrame.AfterSave;
begin

end;

procedure TBoldViewerFrame.BeforeSave;
begin

end;

procedure TBoldViewerFrame.CloseVisualizer;
begin
  if FOwningForm <> nil then
    FOwningForm.Close;
end;

constructor TBoldViewerFrame.Create(AOwner: TComponent);
begin
  inherited;
  fJsonDocument := TJSONDocument.Create(self);
  fJSONTreeView := TJSONTreeView.Create(self);
  fJSONTreeView.Parent := self;
  fJSONTreeView.Align := alClient;
  fJSONTreeView.JSONDocument := fJsonDocument;
  fJSONTreeView.OnDblCLick := JSONTreeViewDblClick;
end;

procedure TBoldViewerFrame.Destroyed;
begin

end;

function TBoldViewerFrame.Evaluate(AExpression: string): string;
var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..4095] of Char;
  CanModify: Boolean;
  Done: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
begin
  Result := '';
  begin
    if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
      CurProcess := DebugSvcs.CurrentProcess;
    if CurProcess <> nil then
    begin
      CurThread := CurProcess.CurrentThread;
      if CurThread <> nil then
      begin
        repeat
        begin
          Done := True;
          EvalRes := CurThread.Evaluate(AExpression, @ResultStr, Length(ResultStr),
            CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);
          case EvalRes of
            erOK: Result := ResultStr;
            erDeferred:
              begin
                FCompleted := False;
                FDeferredResult := '';
                FDeferredError := False;
                FNotifierIndex := CurThread.AddNotifier(Self);
                while not FCompleted do
                  DebugSvcs.ProcessDebugEvents;
                CurThread.RemoveNotifier(FNotifierIndex);
                FNotifierIndex := -1;
                if not FDeferredError then
                begin
                  if FDeferredResult <> '' then
                    Result := FDeferredResult
                  else
                    Result := ResultStr;
                end;
              end;
            erBusy:
              begin
                DebugSvcs.ProcessDebugEvents;
                Done := False;
              end;
          end;
        end
        until Done = True;
      end;
    end;
  end;
end;

procedure TBoldViewerFrame.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress: TOTAAddress;
  ResultSize: LongWord; ReturnCode: Integer);
begin
  FCompleted := True;
  FDeferredResult := ResultStr;
  FDeferredError := ReturnCode <> 0;
end;

function TBoldViewerFrame.FromDbgStrToText(const AText: string): string;
var
  LStream: TStream;
  LParser: TParser;
  LBytes: TBytes;
begin
  LStream := TMemoryStream.Create;
  try
    LBytes := TEncoding.UTF8.GetPreamble;
    LStream.Write(LBytes, Length(LBytes));

    LBytes := TEncoding.UTF8.GetBytes(AText);
    LStream.Write(LBytes, Length(LBytes));

    LStream.Position := 0;
    LParser := TParser.Create(LStream);
    try
      Result := LParser.TokenString;
    finally
      LParser.Free;
    end;
  finally
    LStream.Free;
  end;
end;

procedure TBoldViewerFrame.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord;
  ReturnCode: Integer);
begin
  FCompleted := True;
  FDeferredResult := ResultStr;
  FDeferredError := ReturnCode <> 0;
end;

procedure TBoldViewerFrame.JSONTreeViewDblClick(Sender: TObject);
begin
  (self as IOTADebuggerVisualizerExternalViewerUpdater).RefreshVisualizer(FExpression+'.'+FJsonTreeView.Selected.Text, '', '');
end;

procedure TBoldViewerFrame.LoadJsonFromFile(const AFileName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    ShowJson(sl.text);
  finally
    sl.free;
  end;
end;

procedure TBoldViewerFrame.MarkUnavailable(
  Reason: TOTAVisualizerUnavailableReason);
begin
  if Reason = ovurProcessRunning then
  begin
    FAvailableState := asProcRunning;
  end else if Reason = ovurOutOfScope then
    FAvailableState := asOutOfScope;
end;

procedure TBoldViewerFrame.Modified;
begin
end;

procedure TBoldViewerFrame.ModifyComplete(const ExprStr, ResultStr: string;
  ReturnCode: Integer);
begin
end;

procedure TBoldViewerFrame.RefreshVisualizer(const Expression, TypeName,
  EvalResult: string);
begin
  FAvailableState := asAvailable;
  AddBoldElement(Expression, TypeName, EvalResult);
end;

procedure TBoldViewerFrame.SetClosedCallback(
  ClosedProc: TOTAVisualizerClosedProcedure);
begin
  FClosedProc := ClosedProc;
end;

procedure TBoldViewerFrame.SetForm(AForm: TCustomForm);
begin
  FOwningForm := AForm;
end;

procedure TBoldViewerFrame.SetParent(AParent: TWinControl);
begin
  if AParent = nil then
  begin
    if Assigned(FClosedProc) then
      FClosedProc;
  end;
  inherited;
end;

procedure TBoldViewerFrame.ShowJson(const AJsonString: string);
begin
  fJSONDocument.JsonText := AJsonString;
  fJSONTreeView.Items.BeginUpdate;
  try
    fJSONTreeView.LoadJson;
    fJSONTreeView.FullCollapse;
  finally
    fJSONTreeView.Items.EndUpdate;
  end;
end;

procedure TBoldViewerFrame.ThreadNotify(Reason: TOTANotifyReason);
begin
//  Memo1.Lines.Add('ThreadNotify'+ Ord(Reason).ToString );
end;

{ TBoldVisualizerForm }

constructor TBoldVisualizerForm.Create(const Expression: string);
begin
  inherited Create;
  FExpression := Expression;
end;

procedure TBoldVisualizerForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
  // no toolbar
end;

procedure TBoldVisualizerForm.CustomizeToolBar(ToolBar: TToolBar);
begin
 // no toolbar
end;

function TBoldVisualizerForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TBoldVisualizerForm.FrameCreated(AFrame: TCustomFrame);
begin
  fFrame := TBoldViewerFrame(AFrame);
end;

function TBoldVisualizerForm.GetCaption: string;
begin
  Result := Format(sFormCaption, [FExpression]);
end;

function TBoldVisualizerForm.GetEditState: TEditState;
begin
  Result := [];
end;

function TBoldVisualizerForm.GetForm: TCustomForm;
begin
  Result := fForm;
end;

function TBoldVisualizerForm.GetFrame: TCustomFrame;
begin
  Result := fFrame;
end;

function TBoldVisualizerForm.GetFrameClass: TCustomFrameClass;
begin
  Result := TBoldViewerFrame;
end;

function TBoldVisualizerForm.GetIdentifier: string;
begin
  Result := 'BoldDebugVisualizer';
end;

function TBoldVisualizerForm.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TBoldVisualizerForm.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

function TBoldVisualizerForm.GetToolbarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TBoldVisualizerForm.GetToolbarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TBoldVisualizerForm.LoadWindowState(Desktop: TCustomIniFile;
  const Section: string);
begin
  //no desktop saving
end;

procedure TBoldVisualizerForm.SaveWindowState(Desktop: TCustomIniFile;
  const Section: string; IsProject: Boolean);
begin
  //no desktop saving
end;

procedure TBoldVisualizerForm.SetForm(Form: TCustomForm);
begin
  fForm := Form;
  if Assigned(fFrame) then
    fFrame.SetForm(fForm);
end;

procedure TBoldVisualizerForm.SetFrame(Frame: TCustomFrame);
begin
  fFrame := TBoldViewerFrame(Frame);
end;

{ TDebuggerBoldVisualizer }

function TDebuggerBoldVisualizer.GetMenuText: string;
begin
  Result := sMenuText;
end;

procedure TDebuggerBoldVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  TypeName := 'TBoldElement';
  AllDescendants := True;
end;

function TDebuggerBoldVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 1;
end;

function TDebuggerBoldVisualizer.GetVisualizerDescription: string;
begin
  Result := sBoldVisualizerDescription;
end;

function TDebuggerBoldVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TDebuggerBoldVisualizer.GetVisualizerName: string;
begin
  Result := sBoldVisualizerName;
end;

function TDebuggerBoldVisualizer.Show(const Expression, TypeName,
  EvalResult: string; Suggestedleft,
  SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var
  AForm: TCustomForm;
  AFrame: TBoldViewerFrame;
  VisDockForm: INTACustomDockableForm;
begin
  VisDockForm := TBoldVisualizerForm.Create(Expression) as INTACustomDockableForm;
  AForm := (BorlandIDEServices as INTAServices).CreateDockableForm(VisDockForm);
  AForm.Left := SuggestedLeft;
  AForm.Top := SuggestedTop;
  (VisDockForm as IFrameFormHelper).SetForm(AForm);
  AFrame := (VisDockForm as IFrameFormHelper).GetFrame as TBoldViewerFrame;
  AForm.Caption := Expression + ':' + TypeName;
  AFrame.AddBoldElement(Expression, TypeName, EvalResult);
  Result := AFrame as IOTADebuggerVisualizerExternalViewerUpdater;
end;

initialization

finalization
  RemoveVisualizer;
end.
