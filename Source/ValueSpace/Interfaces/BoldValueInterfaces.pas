unit BoldValueInterfaces;

interface

uses
  BoldId,
  BoldDefs;

type
  IBoldValue = interface;
  IBoldStringContent = interface;
  IBoldStringRepresentable = interface;
  IBoldIntegerContent = interface;
  IBoldFloatContent = interface;
  IBoldCurrencyContent = interface;
  IBoldBooleanContent = interface;
  IBoldDateContent = interface;
  IBoldTimeContent = interface;
  IBoldDateTimeContent = interface;
  IBoldBlobContent = interface;
  IBoldTypedBlob = interface;

  IBoldObjectIdRef = interface;
  IBoldObjectIdRefPair = interface;
  IBoldObjectIdListRef = interface;
  IBoldObjectIdListRefPair = interface;


  IBoldValue = interface
  ['{67C57AD9-621B-11D2-AFF7-006008F62CFF}']
    function GetContentName: String;
    procedure AssignContent(Source: IBoldValue);
    function GetBoldPersistenceState: TBoldValuePersistenceState;
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
    property BoldPersistenceState: TBoldValuePersistenceState read GetBoldPersistenceState write SetBoldPersistenceState;
    property ContentName: String read GetContentName;
   end;

  IBoldNullableValue = interface(IBoldValue)
  ['{4EE57D4A-0958-49AF-944A-A01AAEB099DF}']
    procedure SetContentToNull;
    function GetContentIsNull: Boolean;
    property IsNull: Boolean read GetContentIsNull;
   end;


  IBoldStringContent = interface(IBoldNullableValue)
  ['{67C57AC6-621B-11D2-AFF7-006008F62CFF}']
    procedure SetContentAsString(const NewValue: String);
    function GetContentAsString: String;
    property asString: String read GetContentAsString write SetContentAsString;
  end;

  IBoldStringRepresentable = interface(IBoldNullableValue)
  ['{67C57AC7-621B-11D2-AFF7-006008F62CFF}']
    function GetStringRepresentation(representation:integer): String;
    procedure SetStringRepresentation(Representation: integer; const NewValue: String);
    property StringRepresentation[representation: integer]: String read GetStringRepresentation write SetStringRepresentation;
  end;

  IBoldIntegerContent = interface(IBoldNullableValue)
  ['{67C57AC8-621B-11D2-AFF7-006008F62CFF}']
    function GetContentAsInteger: Integer;
    procedure SetContentAsInteger(NewValue: Integer);
    property asInteger: Integer read GetContentAsInteger write SetContentAsInteger;
  end;

  IBoldFloatContent = interface(IBoldNullableValue)
  ['{67C57AC9-621B-11D2-AFF7-006008F62CFF}']
    function GetContentAsFloat: Double;
    procedure SetContentAsFloat(NewValue: Double);
    property asFloat: Double read GetContentAsFloat write SetContentAsFloat;
  end;

  IBoldCurrencyContent = interface(IBoldNullableValue)
  ['{67C57ACA-621B-11D2-AFF7-006008F62CFF}']
    function GetContentAsCurrency: Currency;
    procedure SetContentAsCurrency(NewValue: Currency);
    property asCurrency: Currency read GetContentAsCurrency write SetContentAsCurrency;
  end;

  IBoldBooleanContent = interface(IBoldNullableValue)
  ['{67C57ACB-621B-11D2-AFF7-006008F62CFF}']
    function GetContentAsBoolean: Boolean;
    procedure SetContentAsBoolean(NewValue: Boolean);
    property asBoolean: Boolean read GetContentAsBoolean write SetContentAsBoolean;
  end;

  IBoldDateContent = interface(IBoldNullableValue)
  ['{67C57ACC-621B-11D2-AFF7-006008F62CFF}']
    function GetContentAsDate: TDateTime;
    procedure SetContentAsDate(NewValue: TDateTime);
    property asDate: TDateTime read GetContentAsDate write SetContentAsDate;
  end;

  IBoldTimeContent = interface(IBoldNullableValue)
  ['{67C57ACD-621B-11D2-AFF7-006008F62CFF}']
    function GetContentAsTime: TDateTime;
    procedure SetContentAsTime(NewValue: TDateTime);
    property asTime: TDateTime read GetContentAsTime write SetContentAsTime;
  end;

  IBoldDateTimeContent = interface(IBoldNullableValue)
  ['{67C57ACE-621B-11D2-AFF7-006008F62CFF}']
    function GetContentAsDateTime: TDateTime;
    procedure SetContentAsDateTime(NewValue: TDateTime);
    property asDateTime: TDateTime read GetContentAsDateTime write SetContentAsDateTime;
  end;

  IBoldBlobContent = interface(IBoldNullableValue)
  ['{F6CE03A0-6283-11D2-AFF7-006008F62CFF}']
    function GetContentAsBlob: String;
    procedure SetContentAsBlob(const NewValue: String);
    property asBlob: String read GetContentAsBlob write SetContentAsBlob;
  end;

  IBoldTypedBlob = interface(IBoldNullableValue)
  ['{6EFB7D60-65BF-11D2-AFF7-006008F62CFF}']
    function GetContentTypeContent: String;
    procedure SetContentTypeContent(const NewValue: String);
    property ContentTypeContent: String read GetContentTypeContent write SetContentTypeContent;
  end;

  IBoldObjectIdRef = Interface(IBoldValue)
  ['{E5AD30CD-544F-4941-998B-947DDDC4E698}']
    procedure SetFromId(Id: TBoldObjectId);
    function GetId: TBoldObjectID;
    function GetOrderNo: integer;
    procedure SetOrderNo(NewOrder: Integer);
    property Id: TBoldObjectID read GetId;
    property OrderNo: integer read getOrderNo write SetOrderNo;
  end;

  IBoldObjectIdListRef = Interface(IBoldValue)
  ['{2EFDD2F7-F998-4ADB-842B-9AEA65C9E602}']
    procedure SetFromIdList(IdLIst: TBoldObjectIdList);
    function GetIdList(Index: Integer): TBoldObjectID;
    property IdList[Index: integer]: TBoldObjectID read GetIdList;
    function GetCount: integer;
    property Count: integer read Getcount;
  end;

  IBoldObjectIdListRefPair = interface(IBoldValue)
  ['{13F940A4-3559-4CD8-84B5-9C87C1F4FCB4}']
    function GetIdList1(Index: Integer): TBoldObjectID;
    function GetIdList2(Index: Integer): TBoldObjectID;
    procedure SetFromIdLists(IdList1, IdList2: TBoldObjectIdList);
    function GetCount: integer;
    property Count: integer read Getcount;
    property IdList1[Index: integer]: TBoldObjectID read GetIdList1;
    property IdList2[Index: integer]: TBoldObjectID read GetIdList2;
  end;

  IBoldObjectIdRefPair = interface(IBoldValue)
  ['{C533AEE9-7B7B-496C-831C-4B05FDD27E4F}']
    procedure SetFromIds(Id1, Id2: TBoldObjectId);
    function GetId1: TBoldObjectID;
    property Id1: TBoldObjectID read GetId1;
    function GetId2: TBoldObjectID;
    property Id2: TBoldObjectID read GetId2;
    function GetOrderNo: integer;
    procedure SetOrderNo(NewOrder: Integer);
    property OrderNo: integer read getOrderNo write SetOrderNo;
  end;

implementation

end.
