
{ Global compiler directives }
{$include bold.inc}
unit BoldMMTVMemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmMemoEdit = class(TForm)
    memoTheMemo: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMemoEdit: TfrmMemoEdit;

implementation

{$R *.dfm}

end.
