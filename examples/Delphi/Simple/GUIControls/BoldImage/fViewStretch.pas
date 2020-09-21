unit fViewStretch;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  Boldhandles,
  BoldImage;

type
  TfrmStretch = class(TForm)
    BoldImage: TBoldImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmStretch: TfrmStretch;

implementation

uses
  fMain;

{$R *.DFM}

end.
