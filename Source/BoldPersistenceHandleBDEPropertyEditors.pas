
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleBDEPropertyEditors;

interface

uses
  Classes,
  BoldPropertyEditors;

type
  { forward declarations }         
  TBoldDatabaseNameProperty = class;
  TBoldSessionNameProperty = class;

  { TBoldDatabaseNameProperty }
  TBoldDatabaseNameProperty = class(TBoldStringSelectionProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  { TBoldSessionNameProperty }
  TBoldSessionNameProperty = class(TBoldStringSelectionProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

implementation      

uses
  DBTables;

{ TBoldDatabaseNameProperty }

procedure TBoldDatabaseNameProperty.GetValueList(List: TStrings);
begin
  DBTables.Session.GetDatabaseNames(List);
end;

{ TBoldSessionNameProperty }

{ TBoldSessionNameProperty }
procedure TBoldSessionNameProperty.GetValueList(List: TStrings);
begin
  Sessions.GetSessionNames(List);
end;

end.
