{*******************************}
{   This unit was created by    }
{ the BoldSoft Attribute Wizard }
{      2000-04-27 14:58:47      }
{*******************************}

unit BACoordinatePMapper;

interface

uses
  DB,
  BoldMeta,
  BoldDefs,
  BoldMemberTypeDictionary,
  BoldId,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldDbInterfaces,
  BoldPMappers,
  BoldTypeNameDictionary,
  BoldPMapperLists,
  BoldPMappersDefault,
  BoldPMappersAttributeDefault,
  BACoordinateInterface;

type
  TBACoordinatePMapper = class(TBoldMemberDefaultMapper)
  private
    {Private declarations}
  protected
    {Protected declarations}
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function GetColumnCount: Integer; override;
    function GetColumnSize(ColumnIndex: Integer): Integer; override;
    function GetInitialColumnName(ColumnIndex: Integer): string; override;
  public
    {Public declarations}
    constructor CreateFromMold(Moldmember: TMoldmember; MoldClass : TMoldClass; Owner : TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary : TBoldTypeNameDictionary); override;
    class function CanStore(const StreamName : string): Boolean; override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure CreatePersistentStorage; override;
    procedure InitializeSystem(theDatabase: IBoldDataBase); override;
  end;

implementation

constructor TBACoordinatePMapper.CreateFromMold(Moldmember: TMoldmember; MoldClass : TMoldClass; Owner : TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary : TBoldTypeNameDictionary);
begin
  // if you need additional columns/tables to store your attribute,
  // this is where you should create the necessary PSDescriptions
  inherited;
end;

class function TBACoordinatePMapper.CanStore(const StreamName: string): Boolean;
begin
  Result := StreamName = StreamName;
end;

procedure TBACoordinatePMapper.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); 
var
  aCoordinate: ICoordinate;
begin
  aCoordinate := GetEnsuredValue(ObjectContent) as ICoordinate;
  case ColumnIndex of
    0: aCoordinate.x := Field.AsInteger; // edit "As<ColumnDataType>" to reflect the correct property of the TField
    1: aCoordinate.y := Field.AsInteger; // edit "As<ColumnDataType>" to reflect the correct property of the TField
    2: aCoordinate.z := Field.AsInteger; // edit "As<ColumnDataType>" to reflect the correct property of the TField
    else inherited;
  end;
end;

procedure TBACoordinatePMapper.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aCoordinate: ICoordinate;
begin
  aCoordinate := GetEnsuredValue(ObjectContent) as ICoordinate;
  case ColumnIndex of
    0: Param.AsInteger := aCoordinate.x; // edit "As<ColumnDataType>"  to reflect the correct property of the TParam
    1: Param.AsInteger := aCoordinate.y; // edit "As<ColumnDataType>"  to reflect the correct property of the TParam
    2: Param.AsInteger := aCoordinate.z; // edit "As<ColumnDataType>"  to reflect the correct property of the TParam
    else inherited;
  end;
end;

function TBACoordinatePMapper.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  case ColumnIndex of
    0: Result := 'integer';
    1: Result := 'integer';
    2: Result := 'integer';
    else Result := 'integer';
  end;
end;

function TBACoordinatePMapper.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  case ColumnIndex of
    0: Result := ftInteger;
    1: Result := ftInteger;
    2: Result := ftInteger;
    else Result := ftInteger;
  end;
end;

function TBACoordinatePMapper.GetColumnCount: Integer;
begin
  Result := 3;
end;

function TBACoordinatePMapper.GetColumnSize(ColumnIndex: Integer): Integer;
begin
  case ColumnIndex of
    0: Result := 0; //Change if this column stores a string!
    1: Result := 0; //Change if this column stores a string!
    2: Result := 0; //Change if this column stores a string!
    else Result := inherited GetColumnSize(ColumnIndex);
  end;
end;

function TBACoordinatePMapper.GetInitialColumnName(ColumnIndex: Integer): string;
begin
  // the column name should include the ColumnRootName
  // in case there are several attributes using the same table
  result := inherited GetInitialColumnName(ColumnIndex);
end;


procedure TBACoordinatePMapper.CreatePersistentStorage;
begin
  // this method is called du
  inherited;
end;

procedure TBACoordinatePMapper.InitializeSystem(theDatabase: IBoldDataBase);
begin
  // this function is called when the entire database has been created (all tables/columns are in place).
  // Here you may initialize counters for example
  inherited;
end;

initialization
  BoldMemberPersistenceMappers.AddDescriptor(TBACoordinatePMapper, alConcrete);

finalization
  if BoldMemberPersistenceMappersAssigned then
    BoldMemberPersistenceMappers.RemoveDescriptorByClass(TBACoordinatePMapper);
end.







 
 
 
 
 
 
 
 
 
 
 
 
 