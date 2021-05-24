
{ Global compiler directives }
{$include bold.inc}
unit BoldPMapper;

interface

uses
  BoldSubscription;

type
  TBoldPersistenceMapper = class;
  TBoldPersistenceMapperClass = class of TBoldPersistenceMapper;

  {---TBoldPersistenceMapper---}
  TBoldPersistenceMapper = class(TBoldSubscribableObject)
  protected
    procedure CreatePersistentStorage; virtual;
  end;

implementation

uses
  BoldRev;

{---TBoldPersistenceMapper---}
procedure TBoldPersistenceMapper.CreatePersistentStorage;
begin
end;

initialization

end.
