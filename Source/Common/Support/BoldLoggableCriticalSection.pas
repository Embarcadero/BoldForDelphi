unit BoldLoggableCriticalSection;

interface

uses
  SyncObjs,
  BoldThreadSafeLog;

type
  { forward declarations }
  TBoldLoggableCriticalSection = class;

  { TBoldLoggableCriticalSection }
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
  SysUtils;

{ TBoldLoggableCriticalSection }

procedure TBoldLoggableCriticalSection.Acquire;
begin
  BoldLogThread('L='+fName); // do not localize
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
  BoldLogThread('U=' + fName); // do not localize
end;

end.
