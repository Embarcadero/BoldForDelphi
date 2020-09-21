unit dmProduct;

{$INCLUDE Bold.inc}

interface

uses
{$IFDEF BOLD_DELPHI6_OR_LATER}
  variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldServerHandles, BoldComServerHandles, BoldHandle,
  BoldPersistenceHandle, BoldPersistenceHandleDB, 
  BoldHandles, BoldSystemHandle, BoldSubscription, BoldModel,
  BoldComServerElementHandles,
  AccountClasses_TLB,
  comobj,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link, BoldAbstractModel, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB
  ;

const
  CLSID_IAccountValidator: TGUID = '{B4E9AF33-541F-4F32-AFBE-D617289E4126}';

type
  TDataModule2 = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldComServerHandle1: TBoldComServerHandle;
    BoldComServerElementHandle1: TBoldComServerElementHandle;
    ServiceObject: TBoldComServerObjectHandle;
    BoldModel1: TBoldModel;
    blhAccounts: TBoldListHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    procedure ServiceObjectGetComObject(Sender: TObject;
      out Obj: IUnknown);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  {TProductionService}
  TAccountValidator = class(TCOMObject, IAccountValidator)
  public
    {IAccountValidator}
    function  Validate(const Number: WideString; out Value: WordBool): HResult; stdcall;
  end;


var
  DataModule2: TDataModule2;
  AccountValidatorFactory: TComObjectFactory;

implementation

uses
  comserv,
  boldSystem,
  AccountClasses
  ;

{$R *.DFM}

{ TProductionService }


function CreateProductList(Ids, Quantities: TList): OleVariant;
var
 IdList, QuantityList: oleVariant;
 i: integer;
begin
  if (Ids.Count > 0) and (Ids.Count = Quantities.Count) then
  begin
    IDList := VarArrayCreate([0,Ids.Count - 1],varInteger);
    QuantityList := VarArrayCreate([0,Ids.Count - 1],varInteger);
    for i := 0 to Ids.Count - 1 do
    begin
      IDList[i] := Integer(Ids[i]^);
      QuantityList[i] := Integer(Quantities[i]^);
    end;
    Result := VarArrayCreate([0,1],varVariant);
    Result[0] := IDList;
    Result[1] := QuantityList;
  end
  else
    Result := Null;
end;


procedure TDataModule2.ServiceObjectGetComObject(Sender: TObject;
  out Obj: IUnknown);
begin
  Obj := TAccountValidator.CreateFromFactory(AccountValidatorFactory, nil);
end;

{ TAccountValidator }

function TAccountValidator.Validate(const Number: WideString; out Value: WordBool): HResult;
var
  i: integer;
begin
  //validate account
  Result := S_OK;
  try
  Value := false;
  if not dataModule2.BoldSystemHandle1.Active then
    dataModule2.BoldSystemHandle1.Active := true;
  for i:= 0 to  DataModule2.blhAccounts.Count - 1 do
  begin
    if ((DataModule2.blhAccounts.List[i] as TAccount).Number = Number) then
    begin
      Value := true;
      Break;
    end;
  end;
  except
    Result := S_FALSE;
  end;
end;

initialization
   AccountValidatorFactory := TComObjectFactory.Create(ComServer, TAccountValidator, CLSID_IAccountValidator, 'TAccountValidator', '',
                            ciInternal, tmSingle);

end.

