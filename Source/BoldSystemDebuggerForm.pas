{ Global compiler directives }
{$include bold.inc}
unit BoldSystemDebuggerForm;

interface

uses
  Classes, Controls, Forms, Dialogs,
  StdCtrls,
  BoldListBox,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldAFP,
  BoldAfpDefault,
  BoldSystem,
  BoldOcl,
  BoldLogHandler,
  BoldLogHandlerForm,
  BoldPMappers,
  BoldOCLGraphicRTDebug,
  BoldVariableHandle,
  BoldSharedStrings,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldDBInterfaces,
  BoldGui,
  BoldListHandle,
  BoldReferenceHandle,
  ExtCtrls;

type
  TBoldSystemDebuggerFrm = class(TForm)
    blhClasses: TBoldListHandle;
    bchDirtyObjects: TBoldCursorHandle;
    brhSystem: TBoldReferenceHandle;
    brhDirtyObjects: TBoldReferenceHandle;
    Panel3: TPanel;
    blbClasses: TBoldListBox;
    Label1: TLabel;
    Panel4: TPanel;
    Label3: TLabel;
    blbDirtyObjects: TBoldListBox;
    btnUpdateDirtyObjects: TButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Panel1: TPanel;
    lblMemoryInfo: TLabel;
    lblSharedStrings: TLabel;
    btnUpdateMemoryInfo: TButton;
    mmoMemoryInfo: TMemo;
    mmoSharedStrings: TMemo;
    Panel2: TPanel;
    Label2: TLabel;
    Button1: TButton;
    btnClearLog: TButton;
    cbOclDebugger: TCheckBox;
    cbLogSQL: TCheckBox;
    cbLogPMapper: TCheckBox;
    cbLogOcl: TCheckBox;
    Panel5: TPanel;
    btnUpdateDatabase: TButton;
    procedure btnUpdateMemoryInfoClick(Sender: TObject);
    procedure cbLogOclClick(Sender: TObject);
    procedure cbLogPMapperClick(Sender: TObject);
    procedure cbLogSQLClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbOclDebuggerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnUpdateDirtyObjectsClick(Sender: TObject);
    procedure btnUpdateDatabaseClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel2DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Panel2DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fSystem: TBoldSystem;
    fDirtyList: TBoldObjectList;
  public
    constructor CreateWithSystem(anOwner: TComponent; System: TBoldSystem);
    property System: TBoldSystem read fSystem;
  end;

implementation

{$R *.dfm}

uses
  SysUtils,
  BoldUtils;


procedure TBoldSystemDebuggerFrm.btnUpdateMemoryInfoClick(Sender: TObject);
begin
  mmoSharedStrings.Lines.text := BoldSharedStringManager.InfoString;
end;

procedure TBoldSystemDebuggerFrm.cbLogOclClick(Sender: TObject);
begin
  case cbLogOcl.Checked of
    true: BoldOCLLogHandler := BoldLog;
    false: BoldOCLLogHandler := nil;
  end;
end;

procedure TBoldSystemDebuggerFrm.cbLogPMapperClick(Sender: TObject);
begin
  case cbLogPMapper.Checked of
    true: BoldPMLogHandler := BoldLog;
    false: BoldPMLogHandler := nil;
  end;
end;

procedure TBoldSystemDebuggerFrm.cbLogSQLClick(Sender: TObject);
begin
  case cbLogSQL.Checked of
    true: BoldSQLLogHandler := BoldLog;
    false: BoldSQLLogHandler := nil;
  end;
end;

procedure TBoldSystemDebuggerFrm.Button1Click(Sender: TObject);
begin
  BoldLog.Show;
end;

procedure TBoldSystemDebuggerFrm.cbOclDebuggerClick(Sender: TObject);
begin
  case cbOclDebugger.Checked of
    true: BoldOCLRTDebugger := TBoldOclGraphicRTDebugger.Create;
    false: FreeAndNil(BoldOCLRTDebugger);
  end;
end;

procedure TBoldSystemDebuggerFrm.FormCreate(Sender: TObject);
begin
  if assigned(System) then
  begin
    fDirtyList := TBoldObjectList.CreateWithTypeInfo(system.BoldSystemTypeInfo.RootClassTypeInfo.ListTypeInfo);
    fDirtyList.SubscribeToObjectsInList := false;
    fDirtyList.SubscribeToLocatorsInList := true;

    brhDirtyObjects.Value := fDirtyList;
    btnUpdateDirtyObjects.Enabled := true;
    btnUpdateDatabase.Enabled := true;
  end;
  cbLogOcl.Checked := assigned(BoldOCLLogHandler);
  cbOclDebugger.Checked := assigned(BoldOCLRTDebugger);
  cbLogSQL.Checked := Assigned(BoldSQLLogHandler);
  cbLogPMapper.Checked := assigned(BoldPMLogHandler);
end;

procedure TBoldSystemDebuggerFrm.btnUpdateDirtyObjectsClick(Sender: TObject);
var
  i: integer;
begin
  fDirtyList.Clear;
  for i := 0 to System.DirtyObjects.Count-1 do
    fDirtyList.add(TBoldObject(System.DirtyObjects[i]));
end;

procedure TBoldSystemDebuggerFrm.btnUpdateDatabaseClick(Sender: TObject);
begin
  System.UpdateDatabase
end;

constructor TBoldSystemDebuggerFrm.CreateWithSystem(anOwner: TComponent;
  System: TBoldSystem);
begin
  inherited Create(anOwner);
  fSystem := System;
  brhSystem.Value := System;
end;

procedure TBoldSystemDebuggerFrm.btnClearLogClick(Sender: TObject);
begin
  BoldLog.Clear;
end;

procedure TBoldSystemDebuggerFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fDirtyList);
end;

procedure TBoldSystemDebuggerFrm.Panel2DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := BoldGUIHandler.DraggedObjects.Count = 1;
end;

procedure TBoldSystemDebuggerFrm.Panel2DragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  with TBoldDefaultObjectAutoFormProvider.Create(BoldGUIHandler.DraggedObjects[0]) do
  try
    GenerateAutoForm.Show;
  finally
    Free;
  end;
end;

procedure TBoldSystemDebuggerFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
