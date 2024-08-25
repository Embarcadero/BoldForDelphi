{ Global compiler directives }
{$include bold.inc}
unit BoldCheckListBox;

interface

uses
  BoldCustomCheckListBox,
  Classes;

type
  {forward declarations}
  TBoldCheckListBox = class;

  { TBoldCheckListBox }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldCheckListBox = class(TBoldCustomCheckListBox)
  published
    property BoldListProperties;
    property BoldListHandle;
    property BoldRowStringProperties;
    property BoldRowCheckBoxProperties;
    property Alignment;
    property BoldHandleIndexLock;
  end;

implementation

end.