unit BoldExpert;

interface

uses
  Windows,
  ToolsAPI;

type
  { forward declarations }
  TBoldExpert = class;
  TBoldExpertClass = class of TBoldExpert;

  { TBoldExpert }
  TBoldExpert = class(TInterfacedObject, IOTANotifier, IOTAWizard)
  public
    constructor Create; virtual;
    procedure ExecuteEvent(Sender: TObject); //TNotifyEvent compatible. Calls Execute.
    {IOTANotifier}
    procedure AfterSave; virtual;
    procedure BeforeSave; virtual;
    procedure Destroyed; virtual;
    procedure Modified; virtual;
    {IOTAWizard}
    function GetIDString: string; virtual;
    function GetName: string; virtual;
    function GetState: TWizardState; virtual;
    procedure Execute; virtual;
    {IOTARepositoryWizard}
    function GetAuthor: string; virtual;
    function GetComment: string; virtual;
    function GetPage: string; virtual;
    function GetGlyph: HICON; virtual;
    {IOTAMenuWizard}
    function GetMenuText: string; virtual;
  end;

implementation

{.$R *.res}

{ TBoldExpert }

procedure TBoldExpert.AfterSave;
begin
  // Required for interface
end;

procedure TBoldExpert.BeforeSave;
begin
  // Required for interface
end;

constructor TBoldExpert.Create;
begin
  // To avoid abstract constructor.
end;

procedure TBoldExpert.Destroyed;
begin
  // Required for interface
end;

procedure TBoldExpert.Execute;
begin
  // Required for interface
end;

procedure TBoldExpert.ExecuteEvent(Sender: TObject);
begin
  Execute;
end;

function TBoldExpert.GetAuthor: string;
begin
  Result := 'BoldSoft'; // do not localize
end;

function TBoldExpert.GetComment: string;
begin
  Result := '';
end;

function TBoldExpert.GetGlyph: HICON;
begin
  Result := LoadIcon(FindClassHInstance(ClassType), 'BOLDEXPERT'); // do not localize
end;

function TBoldExpert.GetIDString: string;
begin
  Result := 'BoldSoft.' + GetName; // do not localize
end;

function TBoldExpert.GetMenuText: string;
begin
  Result := GetName;
end;

function TBoldExpert.GetName: string;
begin
  Result := Copy(ClassName, 2, Length(ClassName) - 1);
end;

function TBoldExpert.GetPage: string;
begin
  Result := 'Bold'; // do not localize
end;

function TBoldExpert.GetState: TWizardState;
begin
  Result := [];
end;

procedure TBoldExpert.Modified;
begin
  // Required for interface
end;

end.
