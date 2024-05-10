
{ Global compiler directives }
{$include bold.inc}
unit BoldDefs;

interface

uses
  Classes,
  SysUtils;

const

  NO_CLASS        = -1;
  INTERNALNULLKEY = -1;
  UNASSIGNEDID    = -1;

  NOTVALIDCLIENTID = -1;

  BOLDESC         = #27;
  BOLDNULL        = #0;
  BOLDCRLF        = #13+#10;
  BOLDCR          = #13;
  BOLDLF          = #10;

  BOLDDIRSEPARATOR = '\';

  ALLFILESFILTER: string = 'All files (*.*)|*.*';

  IDCOLUMN_NAME: string = 'BOLD_ID';
  TYPECOLUMN_NAME: string = 'BOLD_TYPE';
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
    MMT_INDEX_COLUMN: string = 'COLUMNINDEX';

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
  TIMESTAMPSTARTCOLUMNNAMEUPPER: String = 'TIMESTAMPSTART';
  TIMESTAMPSTOPCOLUMNNAME: String = 'TimeStampStop';
  LASTTIMESTAMPCOLUMN_NAME: string = 'LastTimestamp';
  THISTIMESTAMPCOLUMN_NAME: string = 'ThisTimestamp';
  LASTCLOCKCOLUMN_NAME: string = 'LastClockTime';
  THISCLOCKCOLUMN_NAME: string = 'ThisClockTime';
  MODELVERSIONCOLUMN_NAME: string = 'MODEL_VERSION';
  ORDERCOLUMN_SUFFIX: string = '_O';
  ORDERCOLUMN_INDEX: integer = 1;

  {values for TBoldRepresentation}
  brDefault       = 1;
  brShort         = 2;
  brLong          = 3;
  brJson          = 4;
  brXml           = 5;
  brHtml          = 6;


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

  BOLD_DATABASE_ERROR_UNKNOWN = 'Unknown Error: %s';
  BOLD_DATABASE_ERROR_CONNECTION = 'Can not connect to Server %s ';
  BOLD_DATABASE_ERROR_LOGIN = 'User %s failed to logon to database %s on server %s';
  BOLD_DATABASE_ERROR_LOGIN_WINDOWS_AUTH = '(Windows Authentication error)';
  BOLD_DATABASE_ERROR_SQL = 'Syntax of SQL "%s" is not correct. (%s)';
  BOLD_DATABASE_ERROR_UPDATE = 'Failed to update database';
  BOLD_DATABASE_ERROR_DEADLOCK = 'Deadlock occured. (%s)';
type

  TBoldRoleType= (rtRole, rtLinkRole, rtInnerLinkRole);
  TBoldRoleSet = set of TBoldRoleType;
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
  TBoldCompareType = (ctDefault, ctAsString, ctCaseSensitive, ctCaseInsensitive, ctAsDate, ctAsTime);
  TBoldOrientation = (orHorizontal, orVertical, orGrid);

  TBoldSQLStyle = (ssColumns, ssParameters, ssValues);
  TBoldAbstractionLevel = (alAbstract, alConcrete);

  TBoldAnsiString = {$IFDEF BOLD_UNICODE}AnsiString{$ELSE}string{$ENDIF};
  TBoldUnicodeString = {$IFDEF BOLD_UNICODE}string{$ELSE}WideString{$ENDIF};  

  EBold = class(Exception);
  EBoldDesignTime = class(EBold);
  EBoldImport = class(EBold);
  EBoldBadRepresentation = class(EBold);

  EBoldInternal = class(EBold);
  EBoldFeatureNotImplementedYet = class(EBoldInternal);
  EBoldBadColumnIndex = class(EBoldInternal);

  EBoldAssertionFailed = class(EBold);

  EBoldXMLLoadError = class(EBold);
  EBoldXMLIncorrectXPath = class(EBold);
  EBoldInvalidSOAP = class(EBold);

  EBoldEnsureDatabaseLockError = class(EBold);
  EBoldLockManagerError = class(EBold);
  EBoldLicenseError = class(EBold);

  EBoldObjectIDError = class(EBold);

  EBoldMissingID = class(EBold);

  EBoldObjectNotInPs = class(EBold);
  EBoldDuplicateSingleLinkValueInDb = class(EBold);

  TBoldValuePersistenceState = (bvpsCurrent, bvpsModified, bvpsInvalid, bvpsTransient);
  TBoldValuePersistenceStateSet = set of TBoldValuePersistenceState;

  TBoldDatabaseErrorType = (bdetError, bdetConnection, bdetUpdate, bdetSQL,
      bdetDeadlock, bdetLogin);
  EBoldDatabaseError = class(EBold)
  private
    FOriginalExceptionClass: string;
    FOriginalExceptionMessage: string;
    procedure SetOriginalExceptionClass(const Value: string);
    procedure SetOriginalExceptionMessage(const Value: string);
  public
    property OriginalExceptionClass: string read FOriginalExceptionClass write SetOriginalExceptionClass;
    property OriginalExceptionMessage: string read FOriginalExceptionMessage write SetOriginalExceptionMessage;
  end;

  EBoldDatabaseConnectionError = class(EBoldDatabaseError);
  EBoldDatabaseUpdateError = class(EBoldDatabaseError);
  EBoldDatabaseSQLError = class(EBoldDatabaseError);
  EBoldDatabaseDeadlockError = class(EBoldDatabaseError);
  EBoldDatabaseLoginError = class(EBoldDatabaseError);

  // Log stuff
  TBoldLogType = (ltInfo, ltDetail, ltWarning, ltError, ltSeparator);
  TBoldModuleType = (mtUnit, mtText, mtIncFile);

  TBoldSortMode = (smQuickSort, smMergeSort, smMergeSortInplace);
const
  BOLDMAXTIMESTAMP = high(TBoldTimeStampType);
  BOLDINVALIDTIMESTAMP = -1;

  {$IFDEF BOLD_DELPHI16_OR_LATER}
  BOLDMAXLISTSIZE = MaxInt div 16;
  {$ELSE}
  BOLDMAXLISTSIZE = Classes.MaxListSize;
  {$ENDIF}

  {$IFDEF BOLD_DELPHI}
  BOLD_HOST_IDE = 'Delphi';
  {$ENDIF}
  {$IFDEF BOLD_BCB}
  BOLD_HOST_IDE = 'C++Builder';
  {$ENDIF}

  {$IFDEF BOLD_DELPHI6}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Borland\' + BOLD_HOST_IDE + '\6.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI7}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Borland\' + BOLD_HOST_IDE + '\7.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI8}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Borland\BDS\1.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI9}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Borland\BDS\2.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI10}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Borland\BDS\3.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI11}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Borland\BDS\4.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI12}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Borland\BDS\5.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI13}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\CodeGear\BDS\6.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI14}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\CodeGear\BDS\7.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI15}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\8.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI16}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\9.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI17}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\10.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI18}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\11.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI19}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\12.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI20}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\13.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI21}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\14.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI22}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\15.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI23}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\16.0\';  // check
  {$ENDIF}
  {$IFDEF BOLD_DELPHI24}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\18.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI25}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\19.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI26}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\20.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI27}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\21.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI28}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\22.0\';
  {$ENDIF}
  {$IFDEF BOLD_DELPHI29}
  BOLD_HOST_IDE_REGISTRYPATH = '\Software\Embarcadero\BDS\23.0\';
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

const
  BoldDefaultSortMode = smMergeSort; // smQuickSort;
implementation


{ EBoldDatabaseError }

procedure EBoldDatabaseError.SetOriginalExceptionClass(const Value: string);
begin
  FOriginalExceptionClass := Value;
end;

procedure EBoldDatabaseError.SetOriginalExceptionMessage(const Value: string);
begin
  FOriginalExceptionMessage := Value;
end;

end.
