unit BoldDefs;

interface

uses
  SysUtils;

const

  NO_CLASS        = -1;
  INTERNALNULLKEY = -1;     // used for 'databases' without NULL such as paradox  and DBase
  UNASSIGNEDID    = -1;     // indicating newly created DefaultID without DBValue

  NOTVALIDCLIENTID = -1;

  BOLDESC         = #27;
  BOLDNULL        = #0;
  BOLDCRLF        = #13+#10;
  BOLDCR          = #13;
  BOLDLF          = #10;

  BOLDDIRSEPARATOR = '\';  // Prepare for other directory separator

  ALLFILESFILTER: string = 'All files (*.*)|*.*';

  IDCOLUMN_NAME: string = 'BOLD_ID';
//  IDCOLUMN_SQLTYPE: string = 'INTEGER';
  TYPECOLUMN_NAME: string = 'BOLD_TYPE';
//  TYPECOLUMN_SQLTYPE: string = 'SMALLINT';
  TABLEPREFIXTAG = '<Prefix>';
  DEFAULTTABLEPREFIX: string = 'Bold';
  OBJECTTABLE_NAME: string = TABLEPREFIXTAG+'_OBJECT';
  TYPETABLE_NAME: string = TABLEPREFIXTAG+'_TYPE';
  IDTABLE_NAME: string = TABLEPREFIXTAG+'_ID';
  TABLETABLE_NAME: string = TABLEPREFIXTAG+'_TABLES';
  TIMESTAMPTABLE_NAME: string = TABLEPREFIXTAG+'_TIMESTAMP';
  CLOCKLOGTABLE_NAME: string = TABLEPREFIXTAG+'_CLOCKLOG';
  LASTCLOCKTABLE_NAME: string = TABLEPREFIXTAG+'_LASTCLOCK';

  MemberMappingTable_NAME: string = TABLEPREFIXTAG+'_MEMBERMAPPING';
    MMT_CLASSNAME_COLUMN: String = 'CLASSNAME';
    MMT_MEMBERNAME_COLUMN: String = 'MEMBERNAME';
    MMT_TABLENAME_COLUMN: String = 'TABLENAME';
    MMT_COLUMNS_COLUMN: String = 'COLUMNS';
    MMT_MAPPERNAME_COLUMN: String = 'MAPPERNAME';

  AllInstancesMappingTable_NAME: string = TABLEPREFIXTAG+'_R_CLSMAP';
    AID_CLASSNAME_COLUMN: String = 'CLASSNAME';
    AID_TABLENAME_COLUMN: String = 'TABLENAME';
    AID_CLASSIDREQUIRED_COLUMN: String = 'CLASSIDREQUIRED';

  ObjectStorageMappingTable_NAME: string = TABLEPREFIXTAG+'_W_CLSMAP';
    ST_CLASSNAME_COLUMN: String = 'CLASSNAME';
    ST_TABLENAME_COLUMN: String = 'TABLENAME';

  CLASSNAMECOLUMN_NAME: string = 'CLASSNAME';
  TABLENAMECOLUMN_NAME: string = 'TABLENAME';
  READONLYCOLUMN_NAME: string = 'READ_ONLY';
  TIMESTAMPCOLUMN_NAME: string = 'BOLD_TIME_STAMP';
  GLOBALIDCOLUMN_NAME: string = 'EXTERNAL_ID';
  TIMESTAMPSTARTCOLUMNNAME: String = 'TimeStampStart';
  TIMESTAMPSTOPCOLUMNNAME: String = 'TimeStampStop';
  LASTTIMESTAMPCOLUMN_NAME: string = 'LastTimestamp';
  THISTIMESTAMPCOLUMN_NAME: string = 'ThisTimestamp';
  LASTCLOCKCOLUMN_NAME: string = 'LastClockTime';
  THISCLOCKCOLUMN_NAME: string = 'ThisClockTime';
  MODELVERSIONCOLUMN_NAME: string = 'MODEL_VERSION';

  {values for TBoldRepresentation}
  brDefault       = 1;
  brShort         = 2;
  brLong          = 3;

  fmNormal        = 0;
  fmDistributable = 1;
  fmCompare       = 2;

  DEFAULTNAMELITERAL = '<Default>';
  DEFAULTNAMELITTERAL = DEFAULTNAMELITERAL {$IFNDEF T2H} deprecated {$ENDIF}; {r4.0}
  DEFAULTNAME: string = DEFAULTNAMELITERAL;

  BOLDTVPREFIX = 'Bold.';

  LanguageNameDelphi: String = 'Delphi';
  SQLDialectAnsi: String = 'AnsiSQL';

  PROPAGATOR_PARAMETER_DELIMITER_CHAR = '|';

type

  TBoldRoleType= (rtRole, rtLinkRole, rtInnerLinkRole);
  TBoldDataBaseGenerationMode = (dbgTable, dbgQuery);
  TBoldStorage = (bsInternal, bsPartiallyExternal, bsExternal, bsExternalKey);

  TBoldTimeStampType = integer;

  TBoldClientID = Integer;

  TBoldBooleanFunction = function: Boolean of object;
  TBoldGetTimeEvent = function: TDateTime of object;
  TBoldJustNotifyEvent = procedure of Object;
  TBoldNotifyEventWithErrorMessage = procedure (Sender: TObject; const ErrorMessage: string) of object;
  TBoldLockManagerProgressEvent = procedure(completed: integer; queued: integer) of object;
  TBoldByteSet = set of byte;

  TBoldDbType = integer;
  TBoldRepresentation = integer;
  TBoldExpression = string;
  TBoldCompareType = (ctDefault, ctAsString, ctAsText, ctAsAnsiString, ctAsAnsiText, ctAsDate, ctAsTime);
  TBoldOrientation = (orHorizontal, orVertical, orGrid);

  TBoldSQLStyle = (ssColumns, ssParameters, ssValues);
  TBoldAbstractionLevel = (alAbstract, alConcrete);


  // Exception types
  EBold = class(Exception); // General Bold Exception class

  EBoldDesignTime = class(EBold); // Raised for designtime errors
  EBoldImport = class(EBold); // Raised for errors during import
  EBoldBadRepresentation = class(EBold);

  EBoldInternal = class(EBold); // Raised for internal errors
  EBoldFeatureNotImplementedYet = class(EBoldInternal); // Not yet...
  EBoldBadColumnIndex = class(EBoldInternal); // Raised for column index "out of bound".

  EBoldAssertionFailed = class(EBold); // raised when a user defined constraint (ie May-operation) is violated.

  EBoldXMLLoadError = class(EBold);
  EBoldXMLIncorrectXPath = class(EBold);
  EBoldInvalidSOAP = class(EBold);

  EBoldEnsureDatabaseLockError = class(EBold);
  EBoldLockManagerError = class(EBold);
  EBoldLicenseError = class(EBold);

  TBoldValuePersistenceState = (bvpsCurrent, bvpsModified, bvpsInvalid, bvpsTransient);
  TBoldValuePersistenceStateSet = set of TBoldValuePersistenceState;


  // Log stuff
  TBoldLogType = (ltInfo, ltDetail, ltWarning, ltError, ltSeparator);
  TBoldModuleType = (mtUnit, mtText, mtIncFile);

const
  BOLDMAXTIMESTAMP = high(TBoldTimeStampType);
  BOLDINVALIDTIMESTAMP = -1;

  {$IFDEF BOLD_DELPHI}
  BOLD_HOST_IDE = 'Delphi';
  {$ENDIF}
  {$IFDEF BOLD_BCB}
  BOLD_HOST_IDE = 'C++Builder';
  {$ENDIF}

const
  ONE_SECOND = 1000;
  SIXTY_SECONDS = 60 * ONE_SECOND;
  THIRTY_MINUTES = SIXTY_SECONDS * 30;

const
  beConnected = 60;
  beDisconnected = 61;
  beDisconnecting = 62;
  beModified = 63;

implementation

end.


