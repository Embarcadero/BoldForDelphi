unit fViewClipboardFmt;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  Clipbrd,
  StdCtrls;

type
  TfrmViewClipboardFmt = class(TForm)
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmViewClipboardFmt: TfrmViewClipboardFmt;

implementation

{$R *.DFM}

procedure TfrmViewClipboardFmt.FormShow(Sender: TObject);
var
  I: Integer;
  Buf: array [0..255] of char;
begin
  for I:=0 to Clipboard.FormatCount-1 do
  begin
    { Predefined Clipboard Formats }
    case Clipboard.Formats[I] of
      1: Buf := 'CF_TEXT';
      2: Buf := 'CF_BITMAP';
      3: Buf := 'CF_METAFILEPICT';
      4: Buf := 'CF_SYLK';
      5: Buf := 'CF_DIF';
      6: Buf := 'CF_TIFF';
      7: Buf := 'CF_OEMTEXT';
      8: Buf := 'CF_DIB';
      9: Buf := 'CF_PALETTE';
      10: Buf := 'CF_PENDATA';
      11: Buf := 'CF_RIFF';
      12: Buf := 'CF_WAVE';
      13: Buf := 'CF_UNICODETEXT';
      14: Buf := 'CF_ENHMETAFILE';
      15: Buf := 'CF_HDROP';
      16: Buf := 'CF_LOCALE';
      17: Buf := 'CF_MAX';
      128: Buf := 'CF_OWNERDISPLAY';
      129: Buf := 'CF_DSPTEXT';
      130: Buf := 'CF_DSPBITMAP';
      131: Buf := 'CF_DSPMETAFILEPICT';
      142: Buf := 'CF_DSPENHMETAFILE';
    else
      Buf := '';
      GetClipboardFormatName( Clipboard.Formats[I], @Buf, SizeOf( Buf ) );
    end;
    Memo1.Lines.Add( IntToStr( Clipboard.Formats[I] )+' <'+Buf+'>' );
  end;
end;

procedure TfrmViewClipboardFmt.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  frmViewClipboardFmt := nil;
end;

end.
