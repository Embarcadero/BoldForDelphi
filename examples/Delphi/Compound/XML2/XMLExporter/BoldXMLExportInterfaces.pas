unit BoldXMLExportInterfaces;

interface

uses
  BoldManipulators;

type
  IBoldXMLObject = interface;
  IBoldXMLAttribute = interface;
  IBoldXMLReference = interface;
  IBoldXMLObjectList = interface;
  IBoldXMLAttributeList = interface;
  IBoldXMLReferenceList = interface;

  IBoldXMLObject = interface
  ['{EE90BC4E-B9D1-4519-90F6-F103C4880BF7}']
    function Attributes: IBoldXMLAttributeList;
    function References: IBoldXMLReferenceList;
    function QualifiedClassName: string;
    function LocalId: string;
    function BoldId(BoldManipulator: TBoldManipulator; Mapping: string = ''): string;    
  end;

  IBoldXMLAttribute = interface
  ['{B7156AF2-04D6-4162-B62F-206F7B06D267}']
    function IsBoolean: Boolean;
    function IsEnum: Boolean;
    function IsDerived: Boolean;
    function IsObject: Boolean;
    function AsObject: IBoldXMLObject;
    function AsString: string;
    function AsBoolean: Boolean;
    function QualifiedName: string;
    function BoldId(BoldManipulator: TBoldManipulator; Mapping: string = ''): string;    
  end;

  IBoldXMLReference = interface
  ['{209C2297-9719-48A5-88BB-471B46AA82E4}']
    function IsMulti: Boolean;
    function IsComposite: Boolean;
    function IsDerived: Boolean;
    function SingleObject: IBoldXMLObject;
    function Objects: IBoldXMLObjectList;
    function QualifiedName: string;
  end;

  IBoldXMLObjectList = interface
  ['{20FF3961-1E68-4F52-9E4C-9B0D63EF162B}']
    function Count: Integer;
    function GetObject(index: Integer): IBoldXMLObject;
  end;

  IBoldXMLAttributeList = interface
  ['{D1C5E3E5-FA12-4DFD-AED8-CFC278299643}']
    function Count: Integer;
    function Attribute(index: Integer): IBoldXMLAttribute;
  end;

  IBoldXMLReferenceList = interface
  ['{87E10837-A1A0-4C93-8729-30C745B73F3E}']
    function Count: Integer;
    function Reference(index: Integer): IBoldXMLReference;
  end;

implementation

end.
