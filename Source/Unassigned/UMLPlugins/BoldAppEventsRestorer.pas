unit BoldAppEventsRestorer;

interface

uses
  Graphics,
  Classes,
  ActnList,
  Forms,
  BoldUMLPlugins,
  BoldUMLModelEditPlugIn;

type
  { forward declaration of classes }
  TUMLAppEventsRestorer = class;

  { TUMLNameFixer }
  TUMLAppEventsRestorer = class(TUMLPlugInFunction)
  private
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnException: TExceptionEvent;
    FOnMessage: TMessageEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnIdle: TIdleEvent;
    FOnDeactivate: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShortCut: TShortCutEvent;
    FOnShowHint: TShowHintEvent;
    function GetComponentByClassName(const TheClassName: string): TComponent;
    procedure SaveAppEvents;
    procedure RestoreAppEvents;
  protected
    function GetMenuItemName: String; override;
    function GetPlugInType: TPlugInType; override;
    function GetImageResourceName: String; override;
    function GetImageMaskColor: TColor; override;
  public
    procedure Execute(Context: IUMLModelPlugInContext); override;
  end;

implementation

uses
  SysUtils,
  contnrs,
  AppEvnts;

type
  TMultiCasterShadow = class(TComponent)
  private
    FAppEvents: TComponentList;
  end;

  TComponentShadow = class(TPersistent)
  private
    FOwner: TComponent;
    FName: TComponentName;
    FTag: Longint;
    FComponents: TList;
  end;

var
  _UMLAppEventsRestorer: TUMLAppEventsRestorer = nil;


{ TUMLNameFixer }

// Main method to invoke plug in functionality
procedure TUMLAppEventsRestorer.Execute(context: IUMLModelPlugInContext);
var
  MultiCasterShadow: TMultiCasterShadow;
  ComponentList: TComponentList;
  ComponentShadow: TComponentShadow;
begin
  SaveAppEvents;

  // Get the MultiCaster and cast it into shape
  MultiCasterShadow := TMultiCasterShadow(GetComponentByClassName('TMultiCaster'));

  // Store the old list to application events
  ComponentList := MultiCasterShadow.FAppEvents;

  // Call constructor to rehook application events
  MultiCasterShadow.Create(Application);

  // Free the list created when Create was invoked.
  MultiCasterShadow.FAppEvents.Free;
  // Restore old list
  MultiCasterShadow.FAppEvents := ComponentList;

  // Now fix up the components list of Application
  ComponentShadow := TComponentShadow(Application);
  // ..by deleting the fake multicaster we just created...
  ComponentShadow.FComponents.Delete(ComponentShadow.FComponents.Count - 1);

  RestoreAppEvents;
end;

// Mask color for bitmap
function TUMLAppEventsRestorer.GetComponentByClassName(const TheClassName: string): TComponent;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Application.ComponentCount - 1 do
    if AnsiSameText(Application.Components[i].ClassName, TheClassName) then
    begin
      Result := Application.Components[i];
      Break;
    end;
end;

function TUMLAppEventsRestorer.GetImageMaskColor: TColor;
begin
  Result := clTeal;
end;

// Resource for menu and button icon
function TUMLAppEventsRestorer.GetImageResourceName: String;
begin
  result := 'AppEventsRestorer';
end;

// Caption for menu and hint for button
function TUMLAppEventsRestorer.GetMenuItemName: String;
begin
  Result := 'AppEventsRestorer';
end;

// Type of tool
function TUMLAppEventsRestorer.GetPlugInType: TPlugInType;
begin
  Result := ptTool;
end;

procedure TUMLAppEventsRestorer.RestoreAppEvents;
var
  NewAppEvent: TApplicationEvents;
function EnsuredNewAppEvent: TApplicationEvents;
begin
  if not Assigned(NewAppEvent) then
    NewAppEvent := TApplicationEvents.Create(Application);
  Result := NewAppEvent;
end;

begin
  NewAppEvent := nil;

  // if we changed onIdle then create a new application event
  if @Application.OnIdle <> @fOnIdle then
    EnsuredNewAppEvent.OnIdle := fOnIdle;
  if @Application.OnActionExecute <> @fOnActionExecute then
    EnsuredNewAppEvent.OnActionExecute := fOnActionExecute;
  if @Application.OnActionUpdate <> @fOnActionUpdate then
    EnsuredNewAppEvent.OnActionUpdate := fOnActionUpdate;
  if @Application.OnActivate <> @fOnActivate then
    EnsuredNewAppEvent.OnActivate := fOnActivate;
  if @Application.OnDeactivate <> @fOnDeactivate then
    EnsuredNewAppEvent.OnDeactivate := fOnDeactivate;
  if @Application.OnException <> @fOnException then
    EnsuredNewAppEvent.OnException := fOnException;
  if @Application.OnHelp <> @fOnHelp then
    EnsuredNewAppEvent.OnHelp := fOnHelp;
  if @Application.OnHint <> @fOnHint then
    EnsuredNewAppEvent.OnHint := fOnHint;
  if @Application.OnMessage <> @fOnMessage then
    EnsuredNewAppEvent.OnMessage := fOnMessage;
  if @Application.OnMinimize <> @fOnMinimize then
    EnsuredNewAppEvent.OnMinimize := fOnMinimize;
  if @Application.OnRestore <> @fOnRestore then
    EnsuredNewAppEvent.OnRestore := fOnRestore;
  if @Application.OnShowHint <> @fOnShowHint then
    EnsuredNewAppEvent.OnShowHint := fOnShowHint;
  if @Application.OnShortCut <> @fOnShortCut then
    EnsuredNewAppEvent.OnShortCut := fOnShortCut;
end;

procedure TUMLAppEventsRestorer.SaveAppEvents;
begin
  with Application do
  begin
    fOnActionExecute := OnActionExecute;
    fOnActionUpdate := OnActionUpdate;
    fOnActivate := OnActivate;
    fOnDeactivate := OnDeactivate;
    fOnException := OnException;
    fOnHelp := OnHelp;
    fOnHint := OnHint;
    fOnIdle := OnIdle;
    fOnMessage := OnMessage;
    fOnMinimize := OnMinimize;
    fOnRestore := OnRestore;
    fOnShowHint := OnShowHint;
    fOnShortCut := OnShortCut;
  end;
end;

initialization
  _UMLAppEventsRestorer := TUMLAppEventsRestorer.Create(true);

finalization
  FreeAndNil(_UMLAppEventsRestorer);

end.
