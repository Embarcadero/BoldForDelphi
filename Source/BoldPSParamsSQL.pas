
{ Global compiler directives }
{$include bold.inc}
unit BoldPSParamsSQL;

interface

uses
  BoldDefs,
  BoldDBInterfaces,
  BoldPSParams;

type
  TBoldPSSQLParams = class(TBoldPSParams)
  private
    fDatabase: IBoldDataBase;
    fIgnoreUnknownTables: Boolean;
  public
    property Database: IBoldDataBase read fDatabase write fDatabase;
    property IgnoreUnknownTables: Boolean read fIgnoreUnknownTables write fIgnoreUnknownTables;
  end;

implementation

end.
