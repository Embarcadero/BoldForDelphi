unit BoldComHandlesConst;

interface

resourcestring
// BoldAbstractListHandleCom
  sNoNextElement = 'No next element';
  sNoPrevElement = 'No previous element';
  sNoCurrentElement = 'No current element';
  sAutoFirstIsReadOnly = 'AutoFirst is read-only';

// Generic
  sPropertyIsReadOnly = '% is Read Only';

//BoldReferenceHandle
  sSetValueNotAllowedWithoutConnectionHandle = '%s.Setvalue: Now allowed without a connectionHandle';
  sSetValueNotAllowedWithInactiveConnectionHandle = '%s.Setvalue: Now allowed with an inactive ConnectionHandle';

//BoldRootedHandlesCom
  sCircularReference = 'Circular reference in RootHandle';

//BoldSystemHandleCom
  sNotConnected = 'UpdateDatabase: Not connected';

// BoldVariableDefinitionCom
  sNameNotUnique = 'Can''t rename variable to "%s", name already exists';
  sInvalidCharsInName = 'Invalid variable name, only alphanum characters and underscore valid';

implementation

end.
