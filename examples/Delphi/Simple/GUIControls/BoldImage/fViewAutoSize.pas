unit fViewAutoSize;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  BoldImage,
  ComCtrls,
  ToolWin,
  StdCtrls,
  Boldhandles,
  BoldControlsDefs,
  BoldSubscription,
  BoldRootedHandles,
  BoldExpressionHandle,
  BoldReferenceHandle;

type
  TfrmImageViewer = class(TForm)
    ToolBar1: TToolBar;
    cboScale: TComboBox;
    btnScale100: TToolButton;
    ToolButton2: TToolButton;
    ScrollBox1: TScrollBox;
    BoldImage: TBoldImage;
    ToolButton1: TToolButton;
    btnAuto: TToolButton;
    btnScale: TToolButton;
    behImage: TBoldReferenceHandle;
    procedure cboScaleChange(Sender: TObject);
    procedure btnScale100Click(Sender: TObject);
    procedure btnAutoClick(Sender: TObject);
    procedure btnScaleClick(Sender: TObject);
    procedure BoldImageResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TfrmImageViewer.cboScaleChange(Sender: TObject);
begin
  btnScale.Click;
  btnScale.Down := True;
  BoldImage.Scale := StrToInt(cboScale.Text);
end;

procedure TfrmImageViewer.btnScale100Click(Sender: TObject);
begin
  btnScale.Click;
  btnScale.Down := True;
  BoldImage.Scale := 100;
end;

procedure TfrmImageViewer.btnAutoClick(Sender: TObject);
begin
  BoldImage.StretchMode := bsmStretchProportional;
  BoldImage.AutoSize := False;
  BoldImage.Align := alClient;
end;

procedure TfrmImageViewer.btnScaleClick(Sender: TObject);
begin
  BoldImage.StretchMode := bsmStretchToScale;
  BoldImage.AutoSize := True;
  BoldImage.Align := alNone;
end;

procedure TfrmImageViewer.BoldImageResize(Sender: TObject);
begin
  BoldImage.Repaint;
  cboScale.Text := IntToStr(BoldImage.Scale);
end;

end.
