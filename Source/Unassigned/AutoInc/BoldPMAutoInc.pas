unit BoldPMAutoInc;

interface

uses
  Classes,
  DB,
  BoldSystem,
  BoldId,
  BoldMeta,
  BoldDefs,
  BoldTaggedValueSupport,
  BoldDbInterfaces,
  BoldNameExpander,
  BoldPMappers,
  BoldPSDescriptionsSQL,
  BoldSQlDatabaseConfig,
  BoldTypeNameDictionary,
  BoldValueSpaceInterfaces,
  BoldValueInterfaces,
  BoldPMappersAttributeDefault;

type
  {TBoldPMInteger}
  TBoldPMAutoInc = class(TBoldPMInteger)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function DefaultDefaultDbValue: String; override;
    procedure CreatePersistentStorage; override;
    function GetGeneratorName: String;
  public
    procedure PMCreate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList); override;
  public
    constructor CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
  end;

implementation

uses
  SysUtils,
  BoldPMapperLists;

{ TBoldPMAutoInc }

function TBoldPMAutoInc.GetColumnBDEFieldType(
  ColumnIndex: Integer): TFieldType;
begin
  Result := ftAutoInc;
end;

function TBoldPMAutoInc.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger;
  if not (SystemPersistenceMapper.SQLDataBaseConfig.Engine in [dbeInterbaseSQLDialect1, dbeInterbaseSQLDialect3]) then
    Result := result + 'IDENTITY(1, 1)';
end;

function TBoldPMAutoInc.DefaultDefaultDbValue: String;
begin
  result := '';
end;

constructor TBoldPMAutoInc.CreateFromMold(Moldmember: TMoldMember;
  MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper;
  const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  FCustomCreateUpDate := True;
end;

procedure TBoldPMAutoInc.PMCreate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
begin
  // just sit back and relax
end;

procedure TBoldPMAutoInc.PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
begin
  // just sit back and relax
  // perhaps we should even raise an exception here since it is not allowed
  // to try to modify an autoinc-value
end;

procedure TBoldPMAutoInc.CreatePersistentStorage;
var
  Query: IBoldQuery;
begin
  inherited;
  if SystemPersistenceMapper.SQLDataBaseConfig.Engine in [dbeInterbaseSQLDialect1, dbeInterbaseSQLDialect3] then
  begin
    // Interbase does not have AutoInc, we must create a trigger and a generator.
    Query := SystemPersistenceMapper.Database.GetQuery;
    try
      Query.AssignSQLText(format('CREATE GENERATOR %s', [GetGeneratorName]));
      Query.ExecSQL;

      Query.AssignSQLText(format('SET GENERATOR %s TO 0', [GetGeneratorName]));
      Query.ExecSQL;

      Query.AssignSQLText(format(
        'CREATE TRIGGER %s FOR %s BEFORE INSERT AS BEGIN NEW.%s = GEN_ID(%s, 1); END',
        [
          BoldExpandName(copy(ExpressionName, 1, 8)+'_AutoInc', '', xtSQL, -1, nccDefault),
          (ColumnDescriptions[0].Owner as TBoldSQLTableDescription).SQLName,
          ColumnDescriptions[0].SQLName,
          GetGeneratorName
        ]));
      Query.ExecSQL;
    finally
      SystemPersistenceMapper.Database.ReleaseQuery(Query);
    end
  end;
end;

function TBoldPMAutoInc.GetGeneratorName: String;
begin
  result :=
    ColumnDescriptions[0].SQLName+'_'+
    (ColumnDescriptions[0].Owner as TBoldSQLTableDescription).SQLName+'_generator';
end;

initialization
  BoldMemberPersistenceMappers.AddDescriptor(TBoldPMAutoInc, alConcrete);
finalization
  if BoldMemberPersistenceMappersAssigned then
    BoldMemberPersistenceMappers.RemoveDescriptorByClass(TBoldPMAutoInc);
end.
