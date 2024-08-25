
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


{---TBoldPersistenceMapper---}
procedure TBoldPersistenceMapper.CreatePersistentStorage;
begin
end;

end.
