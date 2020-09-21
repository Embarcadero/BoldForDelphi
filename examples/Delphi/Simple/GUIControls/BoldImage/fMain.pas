unit fMain;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtDlgs,
  ExtCtrls,
  ActnList,
  BoldElements,
  BoldSystem,
  BoldModel,
  BoldHandle,
  Boldhandles,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldSystemHandle,
  BoldListHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldPlaceableSubscriber,
  BoldControlPack,
  BoldStringControlPack,
  BoldViewerControlPack,
  BoldNavigator,
  BoldMemo,
  BoldEdit,
  BoldImage,
  BoldImageBitmap,
  BoldImageJPEG,
  BoldHandleAction,
  BoldActions,
  BoldDBActions, BoldAbstractModel, BoldNavigatorDefs,
  BoldIBDatabaseAction, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB;


type
  TfrmMain = class(TForm)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldModel1: TBoldModel;
    blhImages: TBoldListHandle;
    BoldNavigator1: TBoldNavigator;
    btnUpdateDB: TButton;
    bmemDescription: TBoldMemo;
    gbxProperties: TGroupBox;
    lblDrawFocus: TLabel;
    cboDrawFocus: TComboBox;
    btxtImageAsString: TBoldEdit;
    gbxClipBoard: TGroupBox;
    btnPaste: TButton;
    btnCut: TButton;
    btnCopy: TButton;
    btnViewClipboardFmt: TButton;
    lblDescription: TLabel;
    lblBoldImage: TLabel;
    edtContentTypeOnPaste: TEdit;
    lblContentTypeOnPaste: TLabel;
    gbxFiles: TGroupBox;
    btnLoad: TButton;
    btnSave: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    btnView: TButton;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    chkEnabled: TCheckBox;
    chkReadOnly: TCheckBox;
    chkTabStop: TCheckBox;
    pnlImage: TPanel;
    BoldImage: TBoldImage;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button1: TButton;
    Button2: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure btnPasteClick(Sender: TObject);
    procedure btnUpdateDBClick(Sender: TObject);
    procedure btnViewClipboardFmtClick(Sender: TObject);
    procedure cboDrawFocusChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCutClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure edtContentTypeOnPasteChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure chkEnabledClick(Sender: TObject);
    procedure chkReadOnlyClick(Sender: TObject);
    procedure chkTabStopClick(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses
  ImageDemoClasses,
  fViewClipboardFmt,
  fViewAutoSize;

{$R *.DFM}

// Cut image to clipboard.
procedure TfrmMain.btnCutClick(Sender: TObject);
begin
  BoldImage.CutToClipboard;
end;

// Copy image to clipboard.
procedure TfrmMain.btnCopyClick(Sender: TObject);
begin
  BoldImage.CopyToClipboard;
end;

// Paste image from clipboard.
procedure TfrmMain.btnPasteClick(Sender: TObject);
begin
  BoldImage.PasteFromClipboard;
end;

// Save all changes to database.
procedure TfrmMain.btnUpdateDBClick(Sender: TObject);
begin
  BoldSystemHandle1.UpdateDatabase;
end;

// Show current clipboard format in separate window.
procedure TfrmMain.btnViewClipboardFmtClick(Sender: TObject);
begin
  if Assigned(frmViewClipboardFmt) then
  begin
    frmViewClipboardFmt.Show;
    frmViewClipboardFmt.BringToFront;
  end
  else
    frmViewClipboardFmt := TfrmViewClipboardFmt.Create(nil);
end;

// Initialize controls.
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cboDrawFocus.ItemIndex := 1;
  chkEnabled.Checked := True;
  chkReadOnly.Checked := False;
  chkTabStop.Checked := True;
  edtContentTypeOnPaste.Text := BoldImage.ContentTypeOnPaste;
end;

// Set DrawFocus property to true if user picks 'bsSingle'.
procedure TfrmMain.cboDrawFocusChange(Sender: TObject);
begin
  BoldImage.DrawFocus := TComboBox(Sender).ItemIndex = 1;
end;

// Set ContentTypeOnPaste property.
procedure TfrmMain.edtContentTypeOnPasteChange(Sender: TObject);
begin
  BoldImage.ContentTypeOnPaste := edtContentTypeOnPaste.Text;
end;

// Save current image to FileName.
procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  if assigned( BoldImage.Viewer ) then
    with SavePictureDialog1 do
    begin
      DefaultExt := BoldImage.Viewer.DefaultExtension;
      if Execute then
        BoldImage.Viewer.SaveToFile( FileName );
    end;
end;

// Open current image in a separate viewer window.
procedure TfrmMain.btnViewClick(Sender: TObject);
begin
  with TfrmImageViewer.Create( Self ) do
  begin
    behimage.Value := blhImages.CurrentBoldObject.BoldMemberByExpressionName['image'];
    Show;
  end;
end;

// Load image from FileName to current image.
procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
   with OpenPictureDialog1 do
  begin
    if Assigned(BoldImage.Viewer) then
      DefaultExt := BoldImage.Viewer.DefaultExtension;
    if Execute then
      BoldImage.LoadFromFile(FileName);
  end;
end;

// Give the user the opportunity to save changes before close.
procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if BoldSystemHandle1.Active then
  if BoldSystemHandle1.System.DirtyObjects.Count > 0 then
    case MessageDlg( 'There are dirty objects. save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: BoldSystemHandle1.System.UpdateDatabase;
      mrNo: BoldSystemHandle1.System.Discard;
      mrCancel: CanClose := False;
    end;
end;


// Set Enabled property.
procedure TfrmMain.chkEnabledClick(Sender: TObject);
begin
  BoldImage.Enabled := TCheckBox(Sender).Checked;
end;

// Set ReadOnly property.
procedure TfrmMain.chkReadOnlyClick(Sender: TObject);
begin
  BoldImage.ReadOnly := TCheckBox(Sender).Checked;
end;

// Set TabStop property.
procedure TfrmMain.chkTabStopClick(Sender: TObject);
begin
  BoldImage.TabStop := TCheckBox(Sender).Checked;
end;

end.

(*
IExplore GIF
49161 <DataObject>
15 <CF_HDROP>
8 <CF_DIB>
49320 <HTML Format>
49171 <Ole Private Data>
2 <CF_BITMAP>

IExplore JPEG
49161 <DataObject>
15 <CF_HDROP>
8 <CF_DIB>
49320 <HTML Format>
49171 <Ole Private Data>
2 <CF_BITMAP>

IExplore Shortcut
49161 <DataObject>
49255 <UniformResourceLocator>
1 <CF_TEXT>
49256 <FileGroupDescriptor>
49269 <FileGroupDescriptorW>
49257 <FileContents>
49171 <Ole Private Data>
16 <CF_LOCALE>
7 <CF_OEMTEXT>
13 <CF_UNICODETEXT>

IExplore HTML
49161 <DataObject>
1 <CF_TEXT>
13 <CF_UNICODETEXT>
49320 <HTML Format>
49221 <Rich Text Format>
49171 <Ole Private Data>
16 <CF_LOCALE>
7 <CF_OEMTEXT>

Word Picture
49161 <DataObject>
49166 <Object Descriptor>
49221 <Rich Text Format>
3 <CF_METAFILEPICT>
8 <CF_DIB>
2 <CF_BITMAP>
49163 <Embed Source>
49156 <Native>
49155 <OwnerLink>
49165 <Link Source>
49167 <Link Source Descriptor>
49154 <ObjectLink>
49342 <Hyperlink>
49171 <Ole Private Data>
14 <CF_ENHMETAFILE>

Word Extract from text with images
49161 <DataObject>
49166 <Object Descriptor>
49221 <Rich Text Format>
1 <CF_TEXT>
13 <CF_UNICODETEXT>
3 <CF_METAFILEPICT>
49163 <Embed Source>
49156 <Native>
49155 <OwnerLink>
49165 <Link Source>
49167 <Link Source Descriptor>
49154 <ObjectLink>
49342 <Hyperlink>
49171 <Ole Private Data>
16 <CF_LOCALE>
7 <CF_OEMTEXT>
14 <CF_ENHMETAFILE>

Explorer Directory and files
49161 <DataObject>
49234 <Shell IDList Array>
49274 <Preferred DropEffect>
49267 <Shell Object Offsets>
15 <CF_HDROP>
49158 <FileName>
49159 <FileNameW>
49171 <Ole Private Data>

Corel Draw 16 Color bitmap
49161 <DataObject>
49163 <Embed Source>
49156 <Native>
49155 <OwnerLink>
49166 <Object Descriptor>
3 <CF_METAFILEPICT>
49165 <Link Source>
49167 <Link Source Descriptor>
49344 <CorelPhotoPaint.Image.6>
8 <CF_DIB>
49171 <Ole Private Data>
14 <CF_ENHMETAFILE>
2 <CF_BITMAP>

Corel Draw Full Color bitmap
49161 <DataObject>
49163 <Embed Source>
49156 <Native>
49155 <OwnerLink>
49166 <Object Descriptor>
3 <CF_METAFILEPICT>
49344 <CorelPhotoPaint.Image.6>
8 <CF_DIB>
49171 <Ole Private Data>
14 <CF_ENHMETAFILE>
2 <CF_BITMAP>

Paint
49161 <DataObject>
49163 <Embed Source>
49156 <Native>
49155 <OwnerLink>
49166 <Object Descriptor>
3 <CF_METAFILEPICT>
8 <CF_DIB>
49171 <Ole Private Data>
14 <CF_ENHMETAFILE>
2 <CF_BITMAP>



*)
