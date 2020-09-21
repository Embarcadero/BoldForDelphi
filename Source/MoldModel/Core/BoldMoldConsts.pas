unit BoldMoldConsts;

interface

resourcestring
  sRecursiveAssignment = 'Attempt to make recursive assignment';
  sClassIsRelation = 'Class "%s" is already relation class for another association';

//BoldBld
  sErrorOnPos = '%s Line: %d Position: %d';
  sUnexpectedEOF = 'BLD Reader: Unexpected EOF';
  sBadCharacter = 'BLD Reader: Bad character %s';
  sSyntaxError = 'BLD Reader: Syntax error';
  sAKeywordExpected = 'BLD Reader: ''%s'' expected';
  sQuotedStringExpected = 'BLD Reader: Quoted string expected';
  sKeyWordTokenExpected = 'BLD Reader: KEYWORD expected';
  sIntegerExpected = 'BLD Reader: Integer expected';
  sBooleanExpected = 'BLD Reader: Boolean expected';

//BoldGen
  sLogGeneratingInPath = 'Generating in path: %s';
  sLogGeneratingFile = 'Generating file: %s';
  sLogInitializingVars = 'Initializing variables';
  sLogExpandingTemplate = 'Expanding template';
  sLogConsiderNameChange1 = 'You should consider naming your model and base unit';
  sLogConsiderNameChange2 = 'to avoid filename conflicts with other projects';
  sLogNoDelphiMappingForType = 'No Delphimapping for type %s';
  sLogNoCOMMappingForType = 'No COM/IDL mapping for type %s, No attribute generated for %s.%s';
  sLogNoNativeMappingForType = 'No native mapping for type %s used in attribute %s.%s, only Bold attribute generated';
  sNoValueTypeMappingForType = 'No Valueinterface mapping for type %s used in persistent attribute %s.%s';
  sProcessingClassXFileY = 'Processing class %s, file %s';
  sMoveToComponent_NoSuperClass = 'MoveClassTreeToComponent: No SuperClass';
  sMoveToComponent_NoMoldModel = 'MoveClassTreeToComponent: No MoldModel';
  sCollidingFileName = 'WARNING! class %s has a file name that collides with another component (%s)!';
  sNoTemplateForPersistenceInterfaces = 'No template defined for PersistenceInterfaces';

//BoldMeta
  sUnknownRoleType = '%s.GetMulti: Unknown roletype for %s.%s';
  sCannotTrimAfterLinkRolesEnsured = 'Can not trim removed elements after the linkroles have been ensured...';
  sMemberHasNoDispID = 'Member has no Dispid';

//BoldTypeNameHandleReg
  sEditTypeNames = 'Edit type names';

//BoldNameExpander
  sNameHasInvalidChars = 'Name has invalid characters';
  sNameTooLong = 'Name is too long';
  sNameCannotBeEmpty = 'Name can not be empty';
  sInvalidFirstChar = 'Name must begin with an alpha-character or underscore';

implementation

end.
