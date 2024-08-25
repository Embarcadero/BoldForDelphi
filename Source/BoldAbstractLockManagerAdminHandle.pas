
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractLockManagerAdminHandle;

interface

uses
  BoldLockingSupportInterfaces_TLB,
  BoldSubscription;

type

  {forward declarations}
  TBoldAbstractLockManagerAdminHandle = class;

  TBoldAbstractLockManagerAdminHandle = class(TBoldSubscribableComponent)
  protected
    function GetLockManagerAdmin: IBoldLockManagerAdmin; virtual; abstract;
  public
    property LockManagerAdmin: IBoldLockManagerAdmin read GetLockManagerAdmin;
  end;

implementation

end.
