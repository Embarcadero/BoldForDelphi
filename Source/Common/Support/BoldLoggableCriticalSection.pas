
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
  public
    constructor Create(name: string);
    procedure Acquire; override;
    procedure Release; override;
  end;

implementation
uses
  SysUtils,
  BoldRev;

{ TBoldLoggableCriticalSection }

procedure TBoldLoggableCriticalSection.Acquire;
begin
  BoldLogThread('L='+fName);
  inherited;
end;

constructor TBoldLoggableCriticalSection.Create(name: string);
begin
  inherited create;
  fName := Name;
end;

procedure TBoldLoggableCriticalSection.Release;
begin
  inherited;
  BoldLogThread('U=' + fName);
end;

initialization
end.
