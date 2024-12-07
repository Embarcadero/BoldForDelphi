
{ Global compiler directives }
{$include bold.inc}
unit BoldDefaultStreamNames;

interface

const
  BOLDVALUESPACENAME = 'BoldValueSpace';
  BOLDOBJECTCONTENTSNAME = 'BoldObjectContents';
  BOLDINTERNALIDNAME = 'BoldInternalObjectId';
  BOLDDEFAULTIDNAME = 'BoldDefaultObjectId';
  BOLDSTRINGIDNAME = 'BoldStringObjectId';
  BOLDTIMESTAMPEDDEFAULTIDNAME = 'BoldTimestampedDefaultObjectId';
  BOLDCLASSIDNAME = 'BoldClassId';
  BOLDMEMBERIDNAME = 'BoldMemberId';
  BOLDOBJECTIDLISTNAME = 'BoldObjectIdList';
  BOLDMEMBERIDLISTNAME = 'BoldMemberIdList';
  BOLDIDTRANSLATIONLISTNAME = 'BoldIDTranslationList';

var
  BoldContentName_String: string;
  BoldContentName_AnsiString: string;
  BoldContentName_UnicodeString: string;
  BoldContentName_Integer: string;
  BoldContentName_Float: string;
  BoldContentName_Currency: string;
  BoldContentName_Blob: string;
  BoldContentName_TypedBlob: string;
  BoldContentName_DateTime: string;
  BoldContentName_Date: string;
  BoldContentName_Time: string;
  BoldContentName_Boolean: string;

  BoldContentName_ObjectIdRef: string;
  BoldContentName_ObjectIdRefPair: string;
  BoldContentName_ObjectIdListRef: string;
  BoldContentName_ObjectIdListRefPair: string;

implementation

uses
  BoldSharedStrings;

initialization
  BoldContentName_ObjectIdRef := BoldSharedStringManager.GetSharedString('ObjectIdRef');
  BoldContentName_ObjectIdRefPair := BoldSharedStringManager.GetSharedString('ObjectIdRefPair');
  BoldContentName_ObjectIdListRef := BoldSharedStringManager.GetSharedString('ObjectIdListRef');
  BoldContentName_ObjectIdListRefPair := BoldSharedStringManager.GetSharedString('ObjectIdListRefPair');
  BoldContentName_String  := BoldSharedStringManager.GetSharedString('String');
  BoldContentName_AnsiString  := BoldSharedStringManager.GetSharedString('AnsiString');
  BoldContentName_UnicodeString  := BoldSharedStringManager.GetSharedString('UnicodeString');
  BoldContentName_Integer := BoldSharedStringManager.GetSharedString('Integer');
  BoldContentName_Float := BoldSharedStringManager.GetSharedString('Float');
  BoldContentName_Currency := BoldSharedStringManager.GetSharedString('Currency');
  BoldContentName_Blob := BoldSharedStringManager.GetSharedString('Blob');
  BoldContentName_TypedBlob := BoldSharedStringManager.GetSharedString('TypedBlob');
  BoldContentName_DateTime := BoldSharedStringManager.GetSharedString('DateTime');
  BoldContentName_Date := BoldSharedStringManager.GetSharedString('Date');
  BoldContentName_Time := BoldSharedStringManager.GetSharedString('Time');
  BoldContentName_Boolean := BoldSharedStringManager.GetSharedString('Boolean');

end.
