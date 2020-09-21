unit MainForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  Outline,
  BoldUtils,
  DirOutln,
  StdCtrls,
  DirectoryTraverser,
  ComCtrls,
  ExtCtrls,
  BoldSubscription,
  BoldLogHandler,
  Clientifier,
  BoldGen,
  BoldSystem,
  FileCtrl,
  BoldModel, BoldAbstractModel;

type
  TForm1 = class(TForm)
    edSourceDirectory: TEdit;
    RichEdit1: TRichEdit;
    btnStart: TButton;
    Label2: TLabel;
    cbDFMastext: TCheckBox;
    DirectoryOutline1: TDirectoryOutline;
    btnChooseSource: TButton;
    lblSourcepath: TLabel;
    edTargetDirectory: TEdit;
    btnChooseTarget: TButton;
    Label5: TLabel;
    edClientifierIni: TEdit;
    Button1: TButton;
    Button2: TButton;
    cbSkipReadOnly: TCheckBox;
    cbMerge: TCheckBox;
    Button3: TButton;
    cbForceWrite: TCheckBox;
    BoldModel1: TBoldModel;
    btnLoadModel: TButton;
    ProgressBar1: TProgressBar;
    Label3: TLabel;
    btnBrowseIniFile: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure UpdaterFile(CurrentDirectory: String; CurrentFile: TSearchRec);
    procedure btnChooseSourceClick(Sender: TObject);
    procedure ChangeDirectory(Sender: TObject);
    procedure btnChooseTargetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnLoadModelClick(Sender: TObject);
    procedure btnBrowseIniFileClick(Sender: TObject);
    procedure cbMergeClick(Sender: TObject);
    procedure edClientifierIniChange(Sender: TObject);
  private    { Private declarations }
    fCurrentEdit: TEdit;
    fabort : boolean;
    fClientifier: TBoldClientifier;
    fIniPath: String;
    function GetExpandedSourceDirectory: String;
    function GetExpandedTargetDirectory: String;
    procedure StartUpdate;
    function GetClientifier: TBoldClientifier;
  public
    { Public declarations }
    property ExpandedSourceDirectory: String read GetExpandedSourceDirectory;
    property ExpandedTargetDirectory: String read GetExpandedTargetDirectory;
    property CurrentEdit: TEdit read fCurrentEdit write fCurrentEdit;
    property Abort: boolean read fAbort write fAbort;
    property Clientifier: TBoldClientifier read GetClientifier;
  end;

var
  Form1: TForm1;

implementation

uses
  FileReplacer,
  BoldModelLoader;

{$R *.DFM}

Type
  TFileType = (ftpas, ftinc, ftdfm, ftdpk, ftrc);

var
  FileType: TFileType;

procedure TForm1.btnStartClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to BoldModel1.MoldModel.Components.Count-1 do
  begin
    Clientifier.UnitNames.Add( BoldModel1.MoldModel.Components[i].Name +'='+ BoldModel1.MoldModel.Components[i].Name+'_TLB');
    Clientifier.ExcludeFiles.Add(BoldModel1.MoldModel.Components[i].Name+'=1');
    Clientifier.ExcludeFiles.Add(BoldModel1.MoldModel.Components[i].Name+'_interface=1');
    Clientifier.ExcludeFiles.Add(BoldModel1.MoldModel.Components[i].Name+'_adapters=1');
    Clientifier.ExcludeFiles.Add(BoldModel1.MoldModel.Components[i].Name+'_TLB=1');
  end;
  StartUpdate;
end;

procedure TForm1.StartUpdate;
var
  Updater: TDirectoryTraverser;
begin
  DirectoryOutline1.Visible := false;

  Updater := TDirectoryTraverser.Create(nil);
  Updater.onFile := UpdaterFile;
  abort := false;
  BtnStart.Caption := 'Working';
  Replacements.Clear;
  replacementCount := 0;
  Updater.Path := ExpandedSourceDirectory;

  FileType := ftPas;
  Updater.Mask := '*.pas';
  updater.execute;

  FileType := ftinc;
  Updater.Mask := '*.inc';
  updater.execute;

  FileType := ftdfm;
  Updater.Mask := '*.dfm';
  Updater.Execute;

  FileType := ftdpk;
  Updater.Mask := '*.dpk';
  Updater.Execute;

  FileType := ftrc;
  Updater.Mask := '*.rc';
  updater.execute;

  btnStart.Caption := 'Done!';
  Beep;
  RichEdit1.Lines.Clear;
  RichEdit1.Lines.AddStrings( Replacements );
  Updater.Free;
end;       

procedure TForm1.UpdaterFile(CurrentDirectory: String; CurrentFile: TSearchRec);
var
  i: integer;
  FileContents: TStringList;
  Wastext: Boolean;
  TargetDirectory: String;
  TargetName: String;
begin
  Application.ProcessMessages;
  if abort or Clientifier.ExcludeFile( CurrentFile.Name ) then
    exit;

  try
    Caption := CurrentFile.Name + '/' + IntToStr( replacementCount );
    application.processMessages;
    i := replacements.Count;
    FileContents := TStringList.Create;
    TargetDirectory := ExpandedTargetDirectory + copy( CurrentDirectory, length( ExpandedSourceDirectory )+1, maxint );
    ForceDirectories( TargetDirectory );
    TargetName := Clientifier.Unitnames.values[ChangeFileExt( CurrentFile.Name, '')];
    if TargetName = '' then
      TargetName := TargetDirectory + CurrentFile.Name
    else
      TargetName := TargetDirectory + TargetName + ExtractFileExt( CurrentFile.name );

    // skip readonly files only if they already exists in the target-area.

    if cbSkipReadOnly.Checked and
      (CurrentFile.Attr and faReadOnly = faReadOnly) and
      FileExists( Targetname ) then
      exit;

    // clean the writeprotection if forced write
    if cbForceWrite.Checked and
       fileexists(TargetName) and
      (FileGetAttr(TargetName) and faReadOnly = faReadOnly) then
      FileSetAttr(TargetName, FileGetAttr(TargetName) and not faReadOnly );

    case FileType of
      ftDFM:
      begin
        WasText := dfmToStrList( CurrentDirectory+CurrentFile.Name, FileContents );

        Clientifier.UpdateDFM( FileContents );

        if (replacements.count <> i) or not fileExists( TargetName ) then
          StrListToDfm(TargetName, FileContents,
            (cbDFMastext.State = cbChecked) or
            ((cbDFMastext.state = cbGrayed) and wasText));
      end;
      ftrc, ftInc, ftPas:
      begin
        FileContents.loadfromFile(CurrentDirectory+CurrentFile.Name);

        Clientifier.UpdatePascal( FileContents );
        if BoldModel1.MoldModel.Classes.Count > 1 then
          Clientifier.UpdateModelStuff( FileContents, BoldModel1.MoldModel );

        if (replacements.count <> i) or not fileExists( TargetName ) then
          FileContents.SaveToFile( TargetName);
      end;
      ftdpk: begin
      end;
    end;

    FileSetAttr(TargetName, CurrentFile.Attr);

    FileContents.Free;

    Caption := CurrentFile.Name + '/' + IntToStr( replacementCount );
    application.processMessages;
    if replacements.count <> i then
      replacements.Insert(i, '******** ' + CurrentFile.Name);
  except
    on e : exception do begin
      if MessageDlg('Convert failed for '+CurrentDirectory+CurrentFile.Name+' reason: '+#13+#10+
        e.message + #13#10+#13#10 +'Do you want to abort?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      abort := true;
    end;
  end;
end;

procedure TForm1.btnChooseSourceClick(Sender: TObject);
begin
  CurrentEdit := edSourceDirectory;
  DirectoryOutLine1.visible := true;
end;

procedure TForm1.ChangeDirectory(Sender: TObject);
begin
  CurrentEdit.Text := DirectoryOutline1.Directory;
end;

function TForm1.GetExpandedSourceDirectory: String;
begin
  if cbMerge.Checked then
    result := ExpandFileName(edTargetDirectory.Text)
  else
    result := ExpandFileName(edSourceDirectory.Text);
end;

function TForm1.GetExpandedTargetDirectory: String;
begin
  result := ExpandFileName(edTargetDirectory.Text);
end;

procedure TForm1.btnChooseTargetClick(Sender: TObject);
begin
  CurrentEdit := edTargetDirectory;
  DirectoryOutLine1.visible := true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fIniPath := GetCurrentDir;
  edClientifierIni.text := fIniPath +'\'+ edClientifierIni.text;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Abort := true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  edTargetDirectory.Text := '..\..\BfD\Source\HandlesCom\Core';
  edSourceDirectory.Text := '..\..\BfD\Source\Handles\Core';
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  edSourceDirectory.Text := edSourceDirectory.Text + '\ControlPacks';
  edTargetDirectory.Text := edTargetDirectory.Text + '\ControlPacks';
end;

procedure TForm1.btnLoadModelClick(Sender: TObject);
var
  fileName: String;
begin
  Label3.Caption := '';
  FileName := ModelLoader.ModelLoadDialog( edSourceDirectory.Text );
  if fileName <> '' then
    ModelLoader.ModelFileToUML(FileName, BoldModel1.EnsuredUMLModel);
  Label3.Caption := format('Model Loaded: %s with %d classes', [BoldModel1.MoldModel.Name, BoldModel1.MoldModel.classes.Count] );
end;

procedure TForm1.btnBrowseIniFileClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.create(self);
  try
    OpenDialog.DefaultExt := 'ini';
    OpenDialog.Filter := 'Ini files (*.ini)|*.ini';
    OpenDialog.InitialDir := fIniPath;
    if OPenDialog.Execute then
    begin
      edClientifierIni.Text := OpenDialog.FileName;
      fIniPath := ExtractFilePath( OpenDialog.FileName );
    end;
  finally
    OPenDialog.Free;
  end;
end;

procedure TForm1.cbMergeClick(Sender: TObject);
begin
  edSourceDirectory.Visible := not cbMerge.Checked;
  lblSourcepath.Visible := not cbMerge.Checked;
  btnChooseSource.Visible := not cbMerge.Checked;
end;

function TForm1.GetClientifier: TBoldClientifier;
begin
  if not assigned( fClientifier ) then
    fClientifier := TBoldClientifier.Create( EdClientifierIni.Text );
  result := fClientifier;
end;

procedure TForm1.edClientifierIniChange(Sender: TObject);
begin
  freeAndNil( fClientifier );
end;

end.

