
{ Global compiler directives }
{$include bold.inc}
unit BoldFreeStandingValueFactories;

interface

uses
  BoldFreeStandingValues,
  BoldNamedValueList,
  BoldBase;

type
  { forward declarations }
  TBoldFreeStandingElementFactory = class;

  { TBoldFreeStandingElementFactory }
  TBoldFreeStandingElementFactory = class(TBoldNonRefCountedObject)
  private
    fClasses: TBoldNamedValueList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterFreeStandingClass(const ContentName: String; FreeStandingClass: TBoldFreeStandingElementClass);
    function CreateInstance(const ContentName: string): TBoldInterfacedObject;
  end;

var
  FreeStandingValueFactory: TBoldFreeStandingElementFactory = nil;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldDefaultStreamNames;
{--- TBoldFreeStandingObjectContentsFactory ---}

constructor TBoldFreeStandingElementFactory.create;
begin
  inherited;
  fClasses := TBoldNamedValueList.Create;
end;

destructor TBoldFreeStandingElementFactory.destroy;
begin
  FreeAndNil(fClasses);
  inherited;
end;

procedure TBoldFreeStandingElementFactory.RegisterFreeStandingClass(const ContentName: String; FreeStandingClass: TBoldFreeStandingElementClass);
begin
  fClasses.AddEntry(ContentName,  '', TObject(FreeStandingClass));
end;

function TBoldFreeStandingElementFactory.CreateInstance(const ContentName: string): TBoldInterfacedObject;
var
  ElementClass: TBoldFreeStandingElementClass;
begin
  ElementClass := TBoldFreeStandingElementClass(fClasses.ObjectByName[ContentName]);
  if Assigned(ElementClass) then
    result := ElementClass.Create
  else
    raise EBold.createFmt(sNoClassregisteredForName, [classname, ContentName]);
end;

initialization
  FreeStandingValueFactory := TBoldFreeStandingElementFactory.Create;
  With FreeStandingValueFactory do begin
    RegisterFreeStandingClass(BOLDOBJECTCONTENTSNAME, TBoldFreeStandingObjectContents);
    RegisterFreeStandingClass(BoldContentName_String, TBFSString);
    RegisterFreeStandingClass(BoldContentName_Currency, TBFSCurrency);
    RegisterFreeStandingClass(BoldContentName_Float, TBFSFloat);
    RegisterFreeStandingClass(BoldContentName_Integer, TBFSInteger);
    RegisterFreeStandingClass(BoldContentName_Boolean, TBFSBoolean);
    RegisterFreeStandingClass(BoldContentName_Date, TBFSDate);
    RegisterFreeStandingClass(BoldContentName_Time, TBFSTime);
    RegisterFreeStandingClass(BoldContentName_DateTime, TBFSDateTime);
    RegisterFreeStandingClass(BoldContentName_Blob, TBFSBlob);
    RegisterFreeStandingClass(BoldContentName_TypedBlob, TBFSTypedBlob);

    RegisterFreeStandingClass(BoldContentName_ObjectIdRef, TBFSObjectIdRef);
    RegisterFreeStandingClass(BoldContentName_ObjectIdRefPair, TBFSObjectIdRefPair);
    RegisterFreeStandingClass(BoldContentName_ObjectIdListRef, TBFSObjectIdListref);
    RegisterFreeStandingClass(BoldContentName_ObjectIdListRefPair, TBFSObjectIdListRefPair);
  end;

finalization
  FreeAndNil(FreeStandingValueFactory);

end.
