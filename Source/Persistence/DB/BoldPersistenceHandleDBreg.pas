unit BoldPersistenceHandleDBreg;

interface

procedure Register;

implementation

{.$R BoldPersistenceHandleDB.res}

uses
  Classes,
  Dialogs,
  BoldDefs,
  BoldDbActions,
  DesignEditors,
  DesignIntf,
  actnlist,
  SysUtils,
  BoldPersistenceHandleDB,
  BoldIDEConsts,
  BoldPMConsts;

type
  { TBoldPersistenceHandleDBEditor }
  TBoldPersistenceHandleDBEditor = class(TComponentEditor)
  protected
    procedure GenerateSchema;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldPersistenceHandleDB]);
//  RegisterActions(BOLDACTIONGROUPNAME,
//                [TBoldGenerateSchemaAction], nil);
  RegisterComponentEditor(TBoldPersistenceHandleDB, TBoldPersistenceHandleDBEditor);
end;

{ TBoldPersistenceHandleDBEditor }

procedure TBoldPersistenceHandleDBEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: GenerateSchema;
  end;
end;

procedure TBoldPersistenceHandleDBEditor.GenerateSchema;
begin
  if Component is TBoldPersistenceHandleDB then
  begin
    try
      TBoldPersistenceHandleDB(Component).CreateDataBaseSchema;
      showmessage(sSchemaGenerated);
    except
      on e: Exception do
      begin
        Showmessage(Format(sSchemaGenerationFailed, [BOLDCRLF, BOLDCRLF, e.message]));
      end;
    end;
  end;
end;

function TBoldPersistenceHandleDBEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := sGenerateSchema;
  end;
end;

function TBoldPersistenceHandleDBEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

end.
