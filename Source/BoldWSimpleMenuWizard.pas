
{ Global compiler directives }
{$include bold.inc}
unit BoldWSimpleMenuWizard;

interface

uses
  toolsapi,
  menus;


type

  TInsertAction = (iaBefore, iaAfter, iaChild);
              {TSimpleMenuWizard}
  TSimpleMenuWizard = class(TNotifierObject,IOTAWizard, IOTANotifier)
  private
    fWizardIndex: integer;
    fMenuItem: TMenuItem;
    fIDString: string;
    fName: string;
    fInstalled: Boolean;
    fState: TWizardState;
    fParentMenuName: string;
    fMenuItemIndex: integer;
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure InsertMenuItem(Action: TInsertAction; InsertPosition: Integer;
                    targetName: string; var PopupMenu: TPopUpMenu);
    function IsInIDE: boolean;
  protected
    procedure Initialize; virtual;
  public
    constructor Create(const vname, vIDString: string; const vState: TWizardState;
                  const InsertPosition: integer; const vParentMenuName: string);
    procedure AddMenuItem(var PopupMenu: TPopupMenu);
    procedure Execute; virtual;
    procedure AfterSave; virtual;
    procedure BeforeSave; virtual;
    procedure Modified; virtual;
    procedure Destroyed; virtual;
    procedure OnMenuClick(Sender: TObject); virtual;
    property  WizardIndex: integer read fWizardIndex write fWizardIndex;
    property IDString: string read fIDString write fIDString;
    property State: TWizardState read fState write fState;
    property Name: string read fName write fName;
    property ParentMenuName: string read fParentMenuName;
    property MenuItemIndex: integer read fMenuItemIndex;
    property Installed: Boolean read fInstalled write fInstalled default false;
    property MenuItem: TMenuItem read fMenuItem;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldIDEMenus;
                            {TSimpleMenuWizard}

constructor TSimpleMenuWizard.Create(const vname, vIDString: string; const vState: TWizardState;
                                const InsertPosition: integer; const vParentMenuName: string);
begin
 inherited Create;
 Name := vname;
 IDString := vIDString;
 State := vState;
 fParentMenuName := vParentMenuName;
 fMenuItemIndex := InsertPosition;
 Initialize;
end;

function TSimpleMenuWizard.GetIDString: string;
begin
  Result := IDString;
end;

function TSimpleMenuWizard.GetName: string;
begin
  Result := Name;
end;

function TSimpleMenuWizard.GetState: TWizardState;
begin
 Result := State;
end;

procedure TSimpleMenuWizard.Execute;
begin
end;

procedure TSimpleMenuWizard.OnMenuClick(Sender: TObject);
begin
  Execute;
end;

procedure TSimpleMenuWizard.AfterSave;
begin
end;

procedure TSimpleMenuWizard.BeforeSave;
begin
end;

procedure TSimpleMenuWizard.Destroyed;
begin
end;

procedure TSimpleMenuWizard.Modified;
begin
end;

procedure TSimpleMenuWizard.InsertMenuItem(Action: TInsertAction; InsertPosition: Integer;
                                      targetName: string; var PopupMenu: TPopupMenu);
var
  parentItem, targetItem, Item: TMenuItem;
  I: integer;
  ACaption: string;
begin
  if not IsInIDE then Exit;
  targetItem := nil;
  ACaption := StripHotkey(targetName);
  for I := 0 to (BorlandIDEServices as INTAServices).getMainMenu.Items.Count - 1 do
    if (AnsiCompareText(ACaption, StripHotkey((BorlandIDEServices as INTAServices).getMainMenu.Items[I].Caption))= 0) then
    begin
      targetItem := (BorlandIDEServices as INTAServices).getMainMenu.Items[I];
      System.Break;
    end;
  if Assigned(targetItem) then
  begin
    if (Action = iaChild) then
      begin
        parentItem := targetItem;
      end
    else
      begin
        parentItem := targetItem.Parent;
        if (Action = iaAfter) then
          Inc(InsertPosition);
      end;
   for I := PopupMenu.Items.Count - 1 downto 0 do
    begin
      Item := PopupMenu.Items[i];
      PopupMenu.Items.Delete(i);
      if Assigned(parentItem) then
        begin
          fMenuItem := Item;
          parentItem.Insert(InsertPosition, Item);
          fMenuItem.OnClick := OnMenuClick;
        end;
    end;
   end;
end;

procedure TSimpleMenuWizard.AddMenuItem(var PopupMenu: TPopupMenu);
begin
  InsertMenuItem(iaChild, MenuItemIndex, ParentMenuName,PopupMenu);
end;

function TSimpleMenuWizard.IsInIDE: boolean;
begin
  Result := Assigned(BorlandIDEServices);
end;

procedure TSimpleMenuWizard.Initialize;
begin
end;

end.
