
{ Global compiler directives }
{$include bold.inc}
unit BoldIDEMenus;

interface

uses
  Windows,
  Classes,
  Menus,
  ToolsAPI,
  ShellAPI,
  BoldExpert;

type
  TBoldMenuExpert = class(TBoldExpert)
  private
    fOwner: TComponent;
    fBoldMenu: TMenuItem;
    procedure CreateBaseMenuItems;
    function IsInIDE: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    {Registration}
    procedure RegisterBoldExpert(aBoldExpertClass: TBoldExpertClass);
    function AddMenuItem(aName, aCaption: string; aClickEvent: TNotifyEvent; first: Boolean = false): TMenuItem;
    procedure RemoveAndDestroyMenuItem(var anItem: TMenuItem);
    {Menu Actions}
    procedure ActionAbout(Sender: TObject);
    procedure ActionHelp(Sender: TObject);
    procedure ActionURLBfD(Sender: TObject);
    {Properties}
    property BoldMenu: TMenuItem read fBoldMenu;
  end;

function BoldMenuExpert: TBoldMenuExpert;
function BoldMenuExpertAssigned: boolean;

implementation

uses
  SysUtils,
  Forms,
  BoldDefsDT;

var
  G_BoldMenuExpert: TBoldMenuExpert = nil;

function BoldMenuExpert: TBoldMenuExpert;
begin
  if not Assigned(G_BoldMenuExpert) then
  begin
    G_BoldMenuExpert := TBoldMenuExpert.Create;
    RegisterPackageWizard(G_BoldMenuExpert);
  end;
  Result := G_BoldMenuExpert;
end;

function BoldMenuExpertAssigned: boolean;
begin
  Result := Assigned(G_BoldMenuExpert);
end;

{ TBoldMenuExpert }

procedure TBoldMenuExpert.ActionAbout(Sender: TObject);
begin
end;

procedure TBoldMenuExpert.ActionHelp(Sender: TObject);
var
  HelpHandle: HWND;
begin
//FIXME PORT ###What happened to the helpfile property?
{ -- temproary removed during porting
  HelpHandle := Application.Handle;
  if Application.MainForm <> nil then
    HelpHandle := Application.MainForm.Handle;
  WinHelp(HelpHandle, HelpFile, HELP_FINDER, 0);
}
end;

procedure TBoldMenuExpert.ActionURLBfD(Sender: TObject);
begin
  ShellExecute(0, 'open', URLBoldForDelphi, '', '', SW_SHOWMAXIMIZED);
end;

function TBoldMenuExpert.AddMenuItem(aName, aCaption: string; aClickEvent: TNotifyEvent; first: Boolean = false): TMenuItem;
begin
  if not IsInIDE then
  begin
    Result := nil;
    Exit;
  end;
  Result := TMenuItem.Create(fOwner);
  Result.Name := aName;
  Result.Caption := aCaption;
  Result.OnClick := aClickEvent;
  if first then
    fBoldMenu.Insert(0, Result)
  else
    fBoldMenu.Add(Result);
end;

constructor TBoldMenuExpert.Create;
begin
  fOwner := TComponent.Create(nil);
  CreateBaseMenuItems;
end;

procedure TBoldMenuExpert.CreateBaseMenuItems;
var
  MainMenu: TMainMenu;
  I: Integer;
begin
  if not IsInIDE then Exit;
  MainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  {Main Bold Menu}
  fBoldMenu := TMenuItem.Create(fOwner);
  fBoldMenu.Enabled := true;
  fBoldMenu.Caption := '&Bold';
  fBoldMenu.Name := 'BoldMenu';
  I := 0;
  while (I<MainMenu.Items.Count) and (MainMenu.Items[I].Name<>'ToolsMenu') do
    Inc(I);

  AddMenuItem('BoldURLDelimiterMenu', '-', nil);
  AddMenuItem('BoldBfDHomePageMenu', 'Bold for &Delphi Home Page', ActionURLBfD);
  AddMenuItem('BoldHelpDelimiterMenu', '-', nil);
  AddMenuItem('BoldHelpMenu', '&Help', ActionHelp);
  AddMenuItem('BoldAboutDelimiterMenu', '-', nil);
//  AddMenuItem('BoldAboutMenu', '&About', ActionAbout);
  MainMenu.Items.Insert(I, fBoldMenu);
end;

destructor TBoldMenuExpert.Destroy;
begin
  FreeAndNil(fOwner);
  fBoldMenu := nil;
  G_BoldMenuExpert := nil;
  inherited Destroy;
end;

function TBoldMenuExpert.IsInIDE: boolean;
begin
  Result := Assigned(BorlandIDEServices);
end;

procedure TBoldMenuExpert.RegisterBoldExpert(aBoldExpertClass: TBoldExpertClass);
var
  Expert: TBoldExpert;
  MenuItem: TMenuItem;
begin
  if not IsInIDE then Exit;
  Expert := aBoldExpertClass.Create;
  RegisterPackageWizard(Expert);

  MenuItem := TMenuItem.Create(fOwner);
  with MenuItem do
  begin
    Name    := Expert.ClassName + 'Menu';
    Caption := Expert.GetMenuText;
    OnClick := Expert.ExecuteEvent;
  end;
  fBoldMenu.Insert(0, MenuItem);
end;

procedure TBoldMenuExpert.RemoveAndDestroyMenuItem(var anItem: TMenuItem);
begin
  if Assigned(fBoldMenu) then
    fBoldMenu.Remove(anItem);
  anItem := nil;
end;

initialization

end.
