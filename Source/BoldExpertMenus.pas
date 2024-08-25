
{ Global compiler directives }
{$include bold.inc}
unit BoldExpertMenus;

interface

uses
  Menus,
  Classes;

type
  TdmExpertMenus = class(TDataModule)
    GettingStartedMenu: TPopupMenu;
    GettingStarted1: TMenuItem;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

end.
