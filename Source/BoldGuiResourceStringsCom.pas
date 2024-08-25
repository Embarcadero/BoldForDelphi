
{ Global compiler directives }
{$include bold.inc}
unit BoldGuiResourceStringsCom;

interface

resourcestring
  SCantGetIntegerValue = 'Can''t get integer value from element (%s)';
  SCantSetIntegerValue = 'Can''t set integer value om element (%s)';
  SBoldInvalidName =  '''%s'' is not a valid name';

  SBoldInactiveFollowerNoRenderData = 'Inactive Follower has no renderer data';

  SRepresentationDefault = 'Default';
  SRepresentationShort = 'Short';
  SRepresentationLong = 'Long';

  SDraggedObjectsNotCleared = 'TBoldRendererCom.DefaultStartDrag: BoldGUIHandler.DraggedObjects not cleared';
  SCannotDragOverMultipleObjects = 'Can''t DragOver multiple objects';
  SLinkAlreadyAssigned = 'Link already assigned';
  SCannotChangeStateWithModifiedValue = 'Can''t Change State with Modified Value';

  SValueReadOnly = 'Can''t change value. Value is read only';
  SNavHintFirst = 'First';
  SNavHintPrior = 'Prior';
  SNavHintNext = 'Next';
  SNavHintLast = 'Last';
  SNavHintNew = 'New';
  SNavHintDelete = 'Delete';
  SNavHintMoveUp = 'Move up';
  SNavHintMoveDown = 'Move down';

implementation

end.
