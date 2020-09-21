
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//          This source code is distributed            //
//              as part of the examples.               //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

unit dMain;

{$Include Bold.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList,
  Db,
  DBTables,
  BoldId,
  BoldDefs,
  BoldMeta,
  BoldDefaultId,
  BoldUtils,
  BoldStringId,
  BoldActions,
  BoldHandleAction,
  BoldDBActions,
  BoldSubscription,
  BoldHandles,
  BoldHandle,
  BoldAbstractModel,
  BoldModel,
  BoldSystemHandle,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldPersistenceHandle,
  BoldPersistenceHandlePassthrough,
  BoldPersistenceHandlePTWithModel,
  BoldAbstractExternalPersistenceHandle,
  BoldAbstractPartiallyExternalPH,
  BoldExternalPersistenceSupport,
  BoldPersistenceHandleDB,
  BoldPersistenceHandleIB,
  BoldAFPPluggable,
  BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterBDE, IBDatabase, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB, BoldExternalPersistenceHandleSQL,
  BoldIBDatabaseAction;

type
  TdmMain = class(TDataModule)
    bstiMain: TBoldSystemTypeInfoHandle;
    bshMain: TBoldSystemHandle;
    bmMain: TBoldModel;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    bephEmployee: TBoldExternalPersistenceHandleSQL;
    bphBold: TBoldPersistenceHandleDB;
    dbaBold: TBoldDatabaseAdapterIB;
    IBBold: TIBDatabase;
    IBEmployee: TIBDatabase;
    dbaEmployee: TBoldDatabaseAdapterIB;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmMain: TdmMain;

implementation

uses
  {$IFDEF BOLD_DELPHI6_OR_LATER}
  Variants,
  {$ENDIF}
  BusinessClasses_PersistenceInterfaces;

{$R *.DFM}

{ TdMain }



end.
