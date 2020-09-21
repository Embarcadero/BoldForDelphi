unit BoldMetaSupport;

interface

uses
  BoldMeta,
  BoldDefaultTaggedValues,
  BoldDefs;

type
  { forward declarations }
  TBoldMetaSupport = class;

  { TBoldMetaSupport }
  TBoldMetaSupport = class
  public
    class function ParameterTypeToInterfaceType(ParameterType: String; MoldModel: TMoldModel): String;
    class function ParameterTypeToIDLType(ParameterType: String; MoldModel: TMoldModel; var IsPtr: Boolean): String;
    class function ParameterTypeToCOMType(ParameterType: String; MoldModel: TMoldModel; var IsConst: Boolean): String;
  end;

  { TBoldNamePairRecord }
  TBoldNamePairRecord = record
    InterfaceName: String;
    DelphiName: String;
  end;

const
{//}  BoldObjectSpaceInterfacenames: array[0..19] of TBoldNamePairRecord = (
{//}    (InterfaceName: 'IBoldElement'; DelphiName: 'TBoldElement'),
{//}    (InterfaceName: 'IBoldMetaElement'; DelphiName: 'TBoldMetaElement'),
{//}    (InterfaceName: 'IBoldElementTypeInfo'; DelphiName: 'TBoldElementTypeInfo'),
{//}    (InterfaceName: 'IBoldTypeTypeInfo'; DelphiName: 'TBoldTypeTypeInfo'),
{//}    (InterfaceName: 'IBoldClassTypeInfo'; DelphiName: 'TBoldClassTypeInfo'),
{//}    (InterfaceName: 'IBoldNilTypeInfo'; DelphiName: 'TBoldNilTypeInfo'),
{//}    (InterfaceName: 'IBoldListTypeInfo'; DelphiName: 'TBoldListTypeInfo'),
{//}    (InterfaceName: 'IBoldAttributeTypeInfo'; DelphiName: 'TBoldAttributeTypeInfo'),
{//}    (InterfaceName: 'IBoldSystemTypeInfo'; DelphiName: 'TBoldSystemTypeInfo'),
{//}    (InterfaceName: 'IBoldDomainElement'; DelphiName: 'TBoldDomainElement'),
{//}    (InterfaceName: 'IBoldObject'; DelphiName: 'TBoldObject'),
{//}    (InterfaceName: 'IBoldMember'; DelphiName: 'TBoldMember'),
{//}    (InterfaceName: 'IBoldAttribute'; DelphiName: 'TBoldAttribute'),
{//}    (InterfaceName: 'IBoldObjectReference'; DelphiName: 'TBoldObjectReference'),
{//}    (InterfaceName: 'IBoldList'; DelphiName: 'TBoldList'),
{//}    (InterfaceName: 'IBoldObjectList'; DelphiName: 'TBoldObjectList'),
{//}    (InterfaceName: 'IBoldMemberList'; DelphiName: 'TBoldMemberList'),
{//}    (InterfaceName: 'IBoldSystem'; DelphiName: 'TBoldSystem'),
{//}    (InterfaceName: 'IBoldBlob'; DelphiName: 'TBoldBlob'),
{//}    (InterfaceName: 'IBoldElementHandle'; DelphiName: 'TBoldElementHandle'));


  BoldWideStringTypeName = 'WideString';
  BoldWordBoolTypeName = 'WordBool';

implementation

uses
  SysUtils,
  BoldUtils;

type
  TBoldCOMIDLTypeNameMapping = record
    DelphiName: String;
    IDLName: String;
    ComName: String;
  end;

const
  BoldCOMIDLTypeNameMapping: array[0..21] of TBoldCOMIDLTypeNameMapping = (
    (DelphiName: 'String'; IDLName: 'BSTR'; ComName: BoldWideStringTypeName),        //Type Conversion, Automation safe
    (DelphiName: 'Integer'; IDLName: 'long'; ComName: 'Integer'),                    //Automation safe
    (DelphiName: 'Boolean'; IDLName: 'VARIANT_BOOL'; ComName: BoldWordBoolTypeName), //Type Conversion, Automation safe
    (DelphiName: 'Currency'; IDLName: 'CURRENCY'; ComName: 'Currency'),              //Automation safe
    (DelphiName: 'Double'; IDLName: 'double'; ComName: 'Double'),                    //Automation safe
    (DelphiName: 'TDateTime'; IDLName: 'DATE'; ComName: 'TDateTime'),                //Automation safe
    (DelphiName: 'Longint'; IDLName: 'long'; ComName: 'Longint'),                    //Automation safe
    (DelphiName: 'Smallint'; IDLName: 'short'; ComName: 'Smallint'),                 //Automation safe
    (DelphiName: 'Single'; IDLName: 'float'; ComName: 'Single'),                     //Automation safe
    (DelphiName: 'WideString'; IDLName: 'BSTR'; ComName: 'WideString'),              //Automation safe
    (DelphiName: 'WordBool'; IDLName: 'VARIANT_BOOL'; ComName: 'WordBool'),          //Automation safe
    (DelphiName: 'OleVariant'; IDLName: 'VARIANT'; ComName: 'OleVariant'),           //Automation safe
    (DelphiName: 'Variant'; IDLName: 'VARIANT'; ComName: 'OleVariant'),              //Type Conversion, Automation safe
    (DelphiName: 'Shortint'; IDLName: 'byte'; ComName: 'Shortint'),
    (DelphiName: 'Int64'; IDLName: '__int64'; ComName: 'Int64'),
    (DelphiName: 'Byte'; IDLName: 'unsigned char'; ComName: 'Byte'),
    (DelphiName: 'Word'; IDLName: 'unsigned short'; ComName: 'Word'),
    (DelphiName: 'Cardinal'; IDLName: 'unsigned long'; ComName: 'Cardinal'),
    (DelphiName: 'LongWord'; IDLName: 'unsigned long'; ComName: 'LongWord'),
    (DelphiName: 'Largeuint'; IDLName: 'unsigned __int64'; ComName: 'Largeuint'),

    (DelphiName: 'HResult'; IDLName: 'HRESULT'; ComName: 'HResult'),
    (DelphiName: 'SCODE'; IDLName: 'SCODE'; ComName: 'SCODE'));

class function TBoldMetaSupport.ParameterTypeToCOMType(ParameterType: String;
  MoldModel: TMoldModel; var IsConst: Boolean): String;
var
  i: integer;
begin
  result := '';
  IsConst := false;
  for i := 0 to high(BoldComIDLTypeNameMapping) do
    if CompareText(ParameterType, BoldCOMIDLTypeNameMapping[i].DelphiName) = 0 then
    begin
      Result := BoldCOMIDLTypeNameMapping[i].COMName;
      if Result = BoldWideStringTypeName then
        IsConst := true;
      exit;
    end;

  result := ParameterTypeToInterfacetype(ParameterType, MoldModel);
  if Result <> '' then
    IsConst := true;
end;

class function TBoldMetaSupport.ParameterTypeToIDLType(ParameterType: String; MoldModel: TMoldModel; var IsPtr: Boolean): String;
var
  i: integer;
begin
  result := '';
  IsPtr := false;
  for i := 0 to high(BoldComIDLTypeNameMapping) do
    if CompareText(ParameterType, BoldComIDLTypeNameMapping[i].DelphiName) = 0 then
    begin
      result := BoldCOMIDLTypeNameMapping[i].IDLName;
      exit;
    end;

  result := ParameterTypeToInterfacetype(ParameterType, MoldModel);
  if result <> '' then
    IsPtr := true;
end;

class function TBoldMetaSupport.ParameterTypeToInterfaceType(
  ParameterType: String; MoldModel: TMoldModel): String;
var
  i: integer;
  MoldClass: TMoldClass;
begin
  result := '';
  MoldClass := MoldModel.Classes.ItemsByName[ParameterType];
  if assigned(MoldClass) then
    result := MoldClass.ExpandedInterfaceName
  else
  begin
    MoldClass := MoldModel.Classes.ItemsByDelphiName[ParameterType];
    if assigned(MoldClass) then
      result := MoldClass.ExpandedInterfaceName
    else
    begin
      for i := 0 to high(BoldObjectSpaceInterfacenames) do
      begin
        if CompareText(ParameterType, BoldObjectSpaceInterfacenames[i].DelphiName)=0 then
        begin
          Result := BoldObjectSpaceInterfacenames[i].InterfaceName;
          exit;
        end;
      end;
    end;
  end;

  if SameText(ParameterType, 'IDispatch') then // do not localize
  begin
    result := 'IDispatch'; // do not localize
    exit;
  end;

  if SameText(ParameterType, 'IUnknown') then // do not localize
  begin
    result := 'IUnknown'; // do not localize
    exit;
  end;
end;

end.
