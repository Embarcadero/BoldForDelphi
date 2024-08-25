
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractLockManagerHandle;

interface

uses
  BoldLockingSupportInterfaces_TLB,
  BoldSubscription;

type
  {forward declarations}
  TBoldAbstractLockManagerHandle = class;

  TBoldAbstractLockManagerHandle = class(TBoldSubscribableComponent)
  protected
    function GetLockManager: IBoldLockManager; virtual; abstract;
  public
    property LockManager: IBoldLockManager read GetLockManager;
  end;

implementation

end.
