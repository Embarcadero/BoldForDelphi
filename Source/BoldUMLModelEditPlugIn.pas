
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelEditPlugIn;

interface

uses
  menus,
  windows,
  comctrls,
  Dialogs,
  graphics,
  ActnList,
  BoldModel,
  BoldUMLModel;

type
  IUMLModelPlugInContext = interface
    function GetIsExecutingPlugin: boolean;
    procedure SetIsExecutingPlugin(Value: boolean);
    function GetCurrentModelHandle: TBoldModel;
    function GetCurrentElement: TUMLModelElement;
    property IsExecutingPlugin: boolean read GetIsExecutingPlugin write SetIsExecutingPlugin;
  end;

  TUMLPluginExecuteEvent = procedure (context: IUMLModelPlugInContext) of object;

  TPlugInType = (ptTool, ptFile);

  TBoldUMLPluginOption = (poRequireBoldified, poRequireUnBoldified);
  TBoldUMLPluginOptions = set of TBoldUMLPluginOption;

  TUMLPlugIn = class
  private
    FMenuItemName: String;
    FOnExecute: TUMLPluginExecuteEvent;
    FPlugInType: TPlugInType;
    FInstance: THandle;
    FImageResourceName: String;
    FImageMaskColor: TColor;
    fPlugInClassName: string;
    fOptions: TBoldUMLPluginOptions;
  protected
    property OnExecute: TUMLPluginExecuteEvent read FOnExecute;
  public
    constructor Create(MenuItemName: String;
      OnExecute: TUMLPluginExecuteEvent;
      PlugInType: TPlugInType;
      Instance: THandle;
      ImageResourcename: String;
      ImageMaskColor: TColor;
      Options: TBoldUMLPluginOptions;
      PlugInClassName: String);
    property MenuItemName: String read FMenuItemName;
    property PlugInType: TPlugInType read FPlugInType;
    property Options: TBoldUMLPluginOptions read fOptions;
    property Instance: THandle read FInstance;
    property ImageResourceName: String read FImageResourceName;
    property ImageMaskColor: TColor read FImageMaskColor;
    property PluginClassName: string read fPlugInClassName;
    procedure GuardedExecute(Context: IUMLModelPlugInContext);
    procedure UnGuardedExecute(Context: IUMLModelPlugInContext);
  end;

  TUMLPlugInMenuItem = class(TMenuItem)
  private
    FPlugIn: TUMLPlugIn;
  public
    property PlugIn: TUMLPlugIn read FPlugIn write FPlugIn;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TUMLPlugIn }

constructor TUMLPlugIn.Create(MenuItemName: String;
  OnExecute: TUMLPluginExecuteEvent; PlugInType: TPlugInType;
  Instance: THandle; ImageResourcename: String; ImageMaskColor: TColor;
   Options: TBoldUMLPluginOptions; PlugInClassName: String);
begin
  inherited Create;
  fMenuItemName := MenuItemName;
  fOnExecute := OnExecute;
  fPlugInType := PlugInType;
  fInstance := Instance;
  fImageResourcename := ImageResourceName;
  fImageMaskColor := ImageMaskColor;
  fOptions := Options;
  fPlugInClassName := PlugInClassName;
end;

procedure TUMLPlugIn.GuardedExecute(Context: IUMLModelPlugInContext);
begin
  if Context.IsExecutingPlugin then
  begin
    ShowMessage('Already executing a plugin');
    exit;
  end;
  Context.IsExecutingPlugin := true;
  try
    UnguardedExecute(context);
  finally
    Context.IsExecutingPlugin := false;
  end;
end;


procedure TUMLPlugIn.UnGuardedExecute(Context: IUMLModelPlugInContext);
begin
  if Assigned(OnExecute) then
    OnExecute(Context);
end;

end.
