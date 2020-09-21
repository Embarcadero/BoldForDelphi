unit BoldCheckListBox;

interface

uses
  BoldCustomCheckListBox;

type
  {forward declarations}
  TBoldCheckListBox = class;

  { TBoldCheckListBox }
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
