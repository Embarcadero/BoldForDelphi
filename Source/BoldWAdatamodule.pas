
{ Global compiler directives }
{$include bold.inc}
unit BoldWAdatamodule;

interface

uses
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  ImgList;

type
  TdmAttributeWizard = class(TDataModule)
    AttributeWizardMenu: TPopupMenu;
    TestWizard1: TMenuItem;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmAttributeWizard: TdmAttributeWizard;

implementation

uses
  SysUtils,
  BoldUtils;

{$R *.dfm}

end.
