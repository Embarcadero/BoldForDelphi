
{ Global compiler directives }
{$include bold.inc}
unit BoldLoggableCriticalSection;

interface

uses
  SyncObjs,
  BoldThreadSafeLog;


type
  TBoldLoggableCriticalSection = class(TCriticalSection)
  private
    fName: String;
    lName: String;
    uName: String;
  public
    constructor Create(name: string);
    procedure Acquire; override;
    procedure Release; override;
  end;

implementation
uses
  SysUtils;

{ TBoldLoggableCriticalSection }

procedure TBoldLoggableCriticalSection.Acquire;
begin
  BoldLogThread(LName);
  inherited;
end;

constructor TBoldLoggableCriticalSection.Create(name: string);
begin
  inherited create;
  fName := Name;
  lName := 'L=' + fName;
  uName := 'U=' + fName;
end;

procedure TBoldLoggableCriticalSection.Release;
begin
  inherited;
  BoldLogThread(UName);
end;

end.
