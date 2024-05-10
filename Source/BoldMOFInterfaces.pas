
{ Global compiler directives }
{$include bold.inc}
unit BoldMOFInterfaces;

interface

uses
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF};

type
  IBoldMOFObject = interface;
  IBoldMOFAttribute = interface;
  IBoldMOFReference = interface;
  IBoldMOFObjectList = interface;
  IBoldMOFAttributeList = interface;
  IBoldMOFReferenceList = interface;

  IBoldMOFObject = interface
  ['{692AE991-658D-47DF-A4A7-D2CC92522250}']
    function Attributes: IBoldMOFAttributeList;
    function References: IBoldMOFReferenceList;
    function QualifiedClassName: string;
    function LocalId: string;
  end;

  IBoldMOFAttribute = interface
  ['{09221DEC-0897-4142-B579-59D86E4617F2}']
    function IsBoolean: Boolean;
    function IsEnum: Boolean;
    function IsDerived: Boolean;
    function IsObject: Boolean;
    function AsObject: IBoldMOFObject;
    function AsString: string;
    function AsBoolean: Boolean;
    function QualifiedName: string;
  end;

  IBoldMOFReference = interface
  ['{28F3C7A5-C20E-4DA5-A866-A23ECF48DBE6}']
    function IsMulti: Boolean;
    function IsComposite: Boolean;
    function IsDerived: Boolean;
    function SingleObject: IBoldMOFObject;
    function Objects: IBoldMOFObjectList;
    function QualifiedName: string;
  end;

  IBoldMOFObjectList = interface
  ['{2E2672A3-CD07-44D2-9B57-83E755D12CFA}']
    function Count: Integer;
    function GetObject(index: Integer): IBoldMOFObject;
  end;

  IBoldMOFAttributeList = interface
  ['{62713CE1-3E6D-4B55-80FA-501356AAD074}']
    function Count: Integer;
    function Attribute(index: Integer): IBoldMOFAttribute;
  end;

  IBoldMOFReferenceList = interface
  ['{12D4B870-3944-4BEC-8B39-ED9D49B42F3A}']
    function Count: Integer;
    function Reference(index: Integer): IBoldMOFReference;
  end;

implementation

end.
