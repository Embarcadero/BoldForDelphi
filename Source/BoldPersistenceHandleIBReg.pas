
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleIBReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleIB.res}

uses
  SysUtils,
  BoldDefs,
  DesignIntf,
  Controls,
  ActnList,
  Actions,
  DesignEditors,
  Classes,
  BoldPropertyEditors,
  Dialogs,
  BoldPersistenceHandleIB,
  BoldIBDatabaseAction,
  BoldDatabaseAdapterIB,
  BoldIDEConsts;

type
  TBoldIBDataBaseProperty = class(TBoldFileNameProperty)
  protected
    function FileFilter: string; override;
  end;

  TBoldDatabaseAdapterIBEditor = class(TComponentEditor)
  protected
    procedure CreateIBDatabase;
    procedure EnsureIBDatabase;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


function TBoldIBDataBaseProperty.FileFilter: string;
begin
  Result := Format('%s (*%s)|*%1:s', ['Interbase Database file', '.gdb']);
end;

procedure Register;
begin
    RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterIB]);
    RegisterActions(BOLDACTIONGROUPNAME, [TBoldIBDatabaseAction], nil);
    RegisterComponentEditor(TBoldDatabaseAdapterIB, TBoldDatabaseAdapterIBEditor);
end;

{ TBoldDatabaseAdapterIBEditor }

procedure TBoldDatabaseAdapterIBEditor.CreateIBDatabase;
var
  Adapter: TBoldDatabaseAdapterIB;
begin
  if Component is TBoldDatabaseAdapterIB then
  begin
    Adapter := TBoldDatabaseAdapterIB(Component);
    if not assigned(Adapter.DataBase) then
      raise EBold.Create('Can''t ensure a database without a IBDatabase-component');
    if Adapter.Database.DatabaseName <> ExpandFileName(Adapter.Database.DatabaseName) then
      raise EBold.CreateFmt('Can''t perform this operation with a relative path for the database (%s)',
        [Adapter.Database.DatabaseName]);

    if MessageDlg('This will delete any existing database, are you sure?',
         mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    try
      Adapter.CreateInterbaseDatabase;
      showmessage('Database file created');
    except
      on e: Exception do
      begin
        Showmessage('Database creation failed: '+BOLDCRLF+BOLDCRLF+e.message);
      end;
    end;
  end;
end;

procedure TBoldDatabaseAdapterIBEditor.EnsureIBDatabase;
var
  Adapter: TBoldDatabaseAdapterIB;
begin
  if Component is TBoldDatabaseAdapterIB then
  begin
    Adapter := TBoldDatabaseAdapterIB(Component);
    if not assigned(Adapter.DataBase) then
      raise EBold.Create('Can''t ensure a database without a IBDatabase-component');
    if Adapter.Database.DatabaseName <> ExpandFileName(Adapter.Database.DatabaseName) then
      raise EBold.CreateFmt('Can''t perform this operation with a relative path for the database (%s)',
        [Adapter.Database.DatabaseName]);

    if fileexists(Adapter.Database.DatabaseName) then
    begin
      ShowMessage(format('Database file (%s) already exists.', [Adapter.Database.DatabaseName]));
      exit;
    end;

    try
      Adapter.EnsureInterbaseDatabase;
      showmessage(Format('Database file (%s) is now available', [Adapter.Database.DatabaseName]));
    except
      on e: Exception do
      begin
        Showmessage('Database creation failed: '+BOLDCRLF+BOLDCRLF+e.message);
      end;
    end;
  end;
end;

procedure TBoldDatabaseAdapterIBEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: EnsureIBDatabase;
    1: CreateIBDatabase;
  end;
end;


function TBoldDatabaseAdapterIBEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Ensure Interbase database file';
    1: Result := 'Create Interbase database file';
  end;
end;

function TBoldDatabaseAdapterIBEditor.GetVerbCount: Integer;
begin
  result := 2;
end;

end.
