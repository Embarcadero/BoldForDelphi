
{ Global compiler directives }
{$include bold.inc}
unit BoldMMTVDefGenGUI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  BOLDMMTVDefGen;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TBoldMMTVDefGen.Create.Generate('BoldMMTagDefs.xml');
end;

end.
