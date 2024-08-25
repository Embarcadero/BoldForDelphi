
{ Global compiler directives }
{$include bold.inc}
unit BoldComObj;

interface

uses
  ActiveX,
  BoldBase;

type
  { forward declarations }
  IBoldVariantCollection = interface;
  TBoldVariantEnumerator = class;
  TBoldAutoInterfacedObject = class;
  TBoldContainedAutoInterfacedObject = class;
  TBoldAggregatedAutoInterfacedObject = class;

  IBoldVariantCollection = interface
    ['{E2DF80E0-D720-11D3-89A9-444553540000}']
    function GetVariantCount: Cardinal;
    function GetVariantItems(Index: Cardinal): OleVariant;
    property VariantCount: Cardinal read GetVariantCount;
    property VariantItems[Index: Cardinal]: OleVariant read GetVariantItems;
  end;

  { Defined incorrectly in AxtiveX.pas, redefined here}
  PLongWord = ^LongWord;
  IEnumVariant = interface(IUnknown)
    ['{00020404-0000-0000-C000-000000000046}']
    function Next(celt: LongWord; var rgvar: OleVariant;
      pceltFetched: PLongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  end;

  {-- TBoldVariantEnumerator --}
  TBoldVariantEnumerator = class(TBoldRefCountedObject, IEnumVariant)
  private
    FCollection: IBoldVariantCollection;
    FIndex: Cardinal;
  protected
    { IEnumVariant }
    function Next(celt: LongWord; var rgvar: OleVariant;
      pceltFetched: PLongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  public
    constructor Create(Collection: IBoldVariantCollection);
    destructor Destroy; override;
  end;

  {-- TBoldAutoInterfacedObject --}
  TBoldAutoInterfacedObject = class(TBoldRefCountedObject, ISupportErrorInfo,
    IDispatch, IBoldVariantCollection)
  private
    FDispTypeInfo: ITypeInfo;
    FDispIntfEntry: PInterfaceEntry;
    FDispIID: TGUID;
  protected
    { ISupportErrorInfo }
    function InterfaceSupportsErrorInfo(const IID: TIID): HResult; stdcall;
    { IDispatch }
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function Invoke(DispatchID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    { IBoldVariantCollection }
    function GetVariantCount: Cardinal; virtual;
    function GetVariantItems(Index: Cardinal): OleVariant; virtual;
  public
    constructor Create(const TypeLib: ITypeLib; const DispIntf: TGUID);
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

  {-- TBoldContainedAutoInterfacedObject --}
  TBoldContainedAutoInterfacedObject = class(TBoldAutoInterfacedObject)
  protected
    FController: Pointer;
    function GetController: IUnknown;
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  public
    constructor Create(const Controller: IUnknown; const TypeLib: ITypeLib; const DispIntf: TGUID);
    property Controller: IUnknown read GetController;
  end;

  {-- TBoldAggregatedAutoInterfacedObject --}
  TBoldAggregatedAutoInterfacedObject = class(TBoldContainedAutoInterfacedObject)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  end;

implementation

uses
  Variants,
  SysUtils,
  Windows,
  ComObj;

const
  Easter_Name = 'BoldSoft';
  Easter_Text = '';

{-- TBoldVariantEnumerator -----------------------------------------------------}

constructor TBoldVariantEnumerator.Create(Collection: IBoldVariantCollection);
begin
  FCollection := Collection;
end;

destructor TBoldVariantEnumerator.Destroy;
begin
  inherited;
end;

function TBoldVariantEnumerator.Next(celt: LongWord; var rgvar: OleVariant;
  pceltFetched: PLongWord): HResult;
type
  TVariantArray = array [0..0] of OleVariant;
var
  I: LongWord;
begin
  I := 0;
  while (I < celt) and (FIndex < FCollection.VariantCount) do
  begin
    TVariantArray(rgvar)[I] := FCollection.VariantItems[FIndex];
    Inc(I);
    Inc(FIndex);
  end;
  if Assigned(pceltFetched) then
    pceltFetched^ := I;
  if (I = celt) then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TBoldVariantEnumerator.Skip(celt: LongWord): HResult;
begin
  if (FIndex + celt) <= FCollection.VariantCount then
  begin
    Inc(FIndex,celt);
    Result := S_OK;
  end
  else begin
    FIndex := FCollection.VariantCount;
    Result := S_FALSE;
  end;
end;

function TBoldVariantEnumerator.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TBoldVariantEnumerator.Clone(out Enum: IEnumVariant): HResult;
begin
  Enum := TBoldVariantEnumerator.Create(FCollection);
  Result := S_OK;
end;

{-- TBoldAutoInterfacedObject -------------------------------------------------}

constructor TBoldAutoInterfacedObject.Create(const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  TypeLib.GetTypeInfoOfGuid(DispIntf, FDispTypeInfo);
  FDispIID := DispIntf;
  FDispIntfEntry := GetInterfaceEntry(DispIntf);
end;

function TBoldAutoInterfacedObject.InterfaceSupportsErrorInfo(const IID: TIID): HResult;
begin
  if IsEqualGUID(FDispIID, iid) then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TBoldAutoInterfacedObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
var
  Name: string;
begin
  Result := DispGetIDsOfNames(FDispTypeInfo, Names, NameCount, DispIDs);
  if Failed(Result) and (NameCount = 1) then
  begin
    Name := WideCharToString(POleStrList(Names)[0]);
    if CompareText(Name,Easter_Name) = 0 then
    begin
      PDispIDList(DispIDs)[0] := MaxInt;
      Result := S_OK;
    end;
  end;
end;

function TBoldAutoInterfacedObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Pointer(TypeInfo) := nil;
  if Index <> 0 then
  begin
    Result := DISP_E_BADINDEX;
    Exit;
  end;
  ITypeInfo(TypeInfo) := FDispTypeInfo;
  Result := S_OK;
end;

function TBoldAutoInterfacedObject.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 1;
  Result := S_OK;
end;

function TBoldAutoInterfacedObject.Invoke(DispatchID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
const
  INVOKE_PROPERTYSET = INVOKE_PROPERTYPUT or INVOKE_PROPERTYPUTREF;
begin
  if Flags and INVOKE_PROPERTYSET <> 0 then Flags := INVOKE_PROPERTYSET;
  if (DispatchID = MaxInt) and (Flags and INVOKE_PROPERTYGET <> 0) then
  begin
    if Assigned(VarResult) then
      OleVariant(VarResult^) := Easter_Text;
    Result := S_OK;
    Exit;
  end;
  Result := FDispTypeInfo.Invoke(Pointer(Integer(Self) +
    FDispIntfEntry.IOffset), DispatchID, Flags, TDispParams(Params), VarResult,
    ExcepInfo, ArgErr);
end;

function TBoldAutoInterfacedObject.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, FDispIID, ParamStr(0), '');
end;

function TBoldAutoInterfacedObject.GetVariantCount: Cardinal;
begin
  Result := 0;
end;

function TBoldAutoInterfacedObject.GetVariantItems(Index: Cardinal): OleVariant;
begin
  Result := Unassigned;
end;

{-- TBoldContainedAutoInterfacedObject ----------------------------------------}

constructor TBoldContainedAutoInterfacedObject.Create(const Controller: IUnknown;
  const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  FController := Pointer(Controller);
  inherited Create(TypeLib,DispIntf);
end;

function TBoldContainedAutoInterfacedObject.GetController: IUnknown;
begin
  Result := IUnknown(FController);
end;

function TBoldContainedAutoInterfacedObject._AddRef: Integer;
begin
  Result := IUnknown(FController)._AddRef;
end;

function TBoldContainedAutoInterfacedObject._Release: Integer;
begin
  Result := IUnknown(FController)._Release;
end;

{-- TBoldAggregatedAutoInterfacedObject ---------------------------------------}

function TBoldAggregatedAutoInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := IUnknown(FController).QueryInterface(IID,Obj);
end;


end.
