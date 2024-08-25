
{ Global compiler directives }
{$include bold.inc}
unit BoldGridRTColEditorCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Classes,
  BoldDefs,
  BoldGridCom,
  {$IFNDEF BOLDCOMCLIENT}
  BoldOclPropEditor,
  {$ENDIF}
  {!! DO NOT REMOVE !! BoldSystemRT ,}
  BoldStringControlPackCom;

type
  TBoldExposedGridCom = class(TBoldGridCom);
  TfrmRTColEditorCom = class(TForm)
    lbxColumns: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    cmdOK: TButton;
    Panel3: TPanel;
    tbxCaption: TEdit;
    tbxExpression: TEdit;
    cmdApply: TButton;
    Label1: TLabel;
    Label2: TLabel;
    cmdAddColumn: TButton;
    cmdDeleteColumn: TButton;
    cmdOCLEditor: TButton;
    GroupBox1: TGroupBox;
    cbxRenderer: TCheckBox;
    cbxSetText: TCheckBox;
    cbxSetFont: TCheckBox;
    cbxSetColor: TCheckBox;
    Label3: TLabel;
    procedure lbxColumnsClick(Sender: TObject);
    procedure PropertyChanged(Sender: TObject);
    procedure cmdApplyClick(Sender: TObject);
    procedure cmdAddColumnClick(Sender: TObject);
    procedure cmdDeleteColumnClick(Sender: TObject);
    procedure PropertyKeyPress(Sender: TObject; var Key: Char);
    procedure cmdOCLEditorClick(Sender: TObject);
  private
    { Private declarations }
    fGrid: TBoldCustomGridCom;
    fCurrentGridColumn: TBoldGridColumnCom;
    fCurrentIndex: integer;
    procedure GetColumns;
    function GetEGrid: TBoldExposedGridCom;
    function GetGridColumn: TBoldGridColumnCom;
    procedure SetGridColumn(Value: TBoldGridColumnCom);
    property CurrentIndex: integer read fCurrentIndex write fCurrentIndex;
  public
    { Public declarations }
    procedure Execute(BoldCustomGrid: TBoldCustomGridCom);
    property Grid: TBoldCustomGridCom read fGrid write fGrid;
    property EGrid: TBoldExposedGridCom read GetEGrid;
    property CurrentGridColumn: TBoldGridColumnCom read GetGridColumn write SetGridColumn;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{$R *.dfm}

function TfrmRTColEditorCom.GetGridColumn: TBoldGridColumnCom;
begin
  Result := fCurrentGridColumn;
end;

procedure TfrmRTColEditorCom.SetGridColumn(Value: TBoldGridColumnCom);
begin
  fCurrentGridColumn := Value;
  if Assigned(CurrentGridColumn) then
    with CurrentGridColumn do
    begin
      tbxCaption.Text := Title.Caption;
      tbxExpression.Text := BoldProperties.Expression;
      cbxRenderer.Checked := Assigned(BoldProperties.Renderer);
      if cbxRenderer.Checked then
      begin
        if BoldProperties.Renderer is TBoldAsStringRendererCom then
          cbxSetText.Checked := Assigned(BoldProperties.Renderer.OnSetAsString) and
                                Assigned(BoldProperties.Renderer.OnGetAsString)
        else
          cbxSetText.State := cbGrayed;

        if BoldProperties.Renderer is TBoldAsStringRendererCom then
          cbxSetFont.Checked := Assigned(BoldProperties.Renderer.OnSetFont)
        else
          cbxSetFont.State := cbGrayed;

        if BoldProperties.Renderer is TBoldAsStringRendererCom then
          cbxSetColor.Checked := Assigned(BoldProperties.Renderer.OnSetColor)
        else
          cbxSetColor.State := cbGrayed;
      end
      else
      begin
        cbxSetText.Checked := False;
        cbxSetFont.Checked := False;
        cbxSetColor.Checked := False;
      end;

    end
    else
    begin
      tbxCaption.Text := '';
      tbxExpression.Text := '';
      cbxRenderer.State := cbGrayed;
      cbxSetText.State := cbGrayed;
      cbxSetFont.State := cbGrayed;
      cbxSetColor.State := cbGrayed;
    end;
  cmdApply.Enabled := False;
end;

function TfrmRTColEditorCom.GetEGrid: TBoldExposedGridCom;
begin
  Result := TBoldExposedGridCom(Grid);
end;

procedure TfrmRTColEditorCom.GetColumns;
var
  i: integer;
begin
  for i := 1 to Grid.ColCount-1 do
    lbxColumns.Items.AddObject(EGrid.Columns[i].Title.Caption, EGrid.Columns[i]);
end;

procedure TfrmRTColEditorCom.Execute(BoldCustomGrid: TBoldCustomGridCom);
begin
  if not Assigned(BoldCustomGrid) then
    raise EBold.CreateFmt('%s.Execute: BoldCustomGrid not assigned', [ClassName]);

  Grid := BoldCustomGrid;
  CurrentGridColumn := nil;
  GetColumns;
  ShowModal;
end;

procedure TfrmRTColEditorCom.lbxColumnsClick(Sender: TObject);
begin
  CurrentGridColumn := TBoldGridColumnCom(lbxColumns.Items.Objects[lbxColumns.ItemIndex]);
  CurrentIndex := lbxColumns.ItemIndex;
end;

procedure TfrmRTColEditorCom.PropertyChanged(Sender: TObject);
begin
  cmdApply.Enabled := True;
end;

procedure TfrmRTColEditorCom.cmdApplyClick(Sender: TObject);
begin
  if Assigned(CurrentGridColumn) then
    with CurrentGridColumn do
    begin
      Title.Caption := tbxCaption.Text;
      BoldProperties.Expression := tbxExpression.Text;
      lbxColumns.Items[CurrentIndex] := tbxCaption.Text;
    end;
end;

procedure TfrmRTColEditorCom.cmdAddColumnClick(Sender: TObject);
begin
  Grid.AddColumn;
  with EGrid do
  begin
    Columns[ColCount-1].Title.Caption := '<new column>';
    lbxColumns.Items.AddObject(Columns[ColCount-1].Title.Caption, Columns[ColCount-1]);
  end;
end;

procedure TfrmRTColEditorCom.cmdDeleteColumnClick(Sender: TObject);
begin
  CurrentGridColumn.Free;
  lbxColumns.Items.Delete(CurrentIndex);
  CurrentGridColumn := nil;
end;

procedure TfrmRTColEditorCom.PropertyKeyPress(Sender: TObject; var Key: Char);
begin
  if not Assigned(CurrentGridColumn) then
    Key := BOLDNULL; 
end;

procedure TfrmRTColEditorCom.cmdOCLEditorClick(Sender: TObject);
begin
  {$IFNDEF BOLDCOMCLIENT}
  with TBoldOCLPropEditForm.Create(nil) do
  try
    Context := EGrid.GetHandleListElementType;
    OclExpr := tbxExpression.text;
    if ShowModal = mrOK then
      tbxExpression.Text := OCLExpr;
  finally
    Free;
  end;
  {$ENDIF}
end;

end.
