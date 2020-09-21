unit BoldOracleBDEInterfaces;

interface

uses
  BoldDefs,
  BoldBDEInterfaces;

type
  TBoldOracleBDEQuery = class(TBoldBDEQuery)
  private
    procedure TrimSQL;
  protected
    procedure ExecSQL; override;
    procedure Open; override;
  public
    class procedure InstallQuery;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldOracleBDEQuery }

procedure TBoldOracleBDEQuery.ExecSQL;
begin
  TrimSQL;
  inherited;
end;

class procedure TBoldOracleBDEQuery.InstallQuery;
begin
  BoldBDEQueryClass := TBoldOracleBDEQuery;
end;

procedure TBoldOracleBDEQuery.Open;
begin
  TrimSQL;
  inherited;
end;

procedure TBoldOracleBDEQuery.TrimSQL;
var
  s, s2: string;
begin
  s := Query.SQL.Text;
  s2 := StringReplace(s, BOLDCR, ' ', [rfReplaceAll]);
  s2 := StringReplace(s2, BOLDLF, ' ', [rfReplaceAll]);
  s2 := StringReplace(s2, #9, ' ', [rfReplaceAll]);
  s2 := StringReplace(s2, '  ', ' ', [rfReplaceAll]);
  if s <> s2 then
  begin
    Query.SQL.BeginUpdate;
    Query.SQL.Text := s2;
    Query.SQL.EndUpdate;
  end;
end;

end.

