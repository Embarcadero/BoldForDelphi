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
  BoldExternalPersistenceSupport,
  BoldValueSpaceInterfaces, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB,
  BoldPersistenceHandleDB, BoldIBDatabaseAction, BoldAFPPluggable,
  BoldPersistenceHandle, BoldPersistenceHandlePassthrough,
  BoldPersistenceHandlePTWithModel, BoldAbstractExternalPersistenceHandle,
  BoldAbstractPartiallyExternalPH, BoldExternalPersistenceHandleEventDriven;

type
  TdmMain = class(TDataModule)
    BoldExternalDataPersistenceHandle: TBoldExternalPersistenceHandleEventDriven;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldModel1: TBoldModel;
    Database1: TDatabase;
    CustomerTable: TTable;
    OrderTable: TTable;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    EmployeeTable: TTable;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    ItemTable: TTable;
    PartTable: TTable;
    VendorTable: TTable;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure CustomerGetKeyList(const ExternalKeys: TBoldObjectIdList);
    function CustomerExists(const ExternalKey: TBoldObjectId): Boolean;
    procedure CustomerUpdateObject(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace);
    procedure CustomerReadObject(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; FetchContext: TObject);
    procedure CustomerReadMember(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; Member: TMoldMember; FetchContext: TObject);
    procedure CustomerCreateObject(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace);
    procedure CustomerDeleteObject(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId);
    procedure OrderReadObject(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; FetchContext: TObject);
    procedure OrderUpdateObject(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace);
    procedure OrderCreateObject(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace);
    procedure OrderDeleteObject(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId);
    procedure EmployeeReadObject(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; FetchContext: TObject);
    procedure EmployeeReadMember(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; Member: TMoldMember; FetchContext: TObject);
    procedure BoldExternalDataPersistenceHandleActivate(Sender: TObject);
    procedure BoldExternalDataPersistenceHandleDeActivate(Sender: TObject);
    procedure BoldExternalDataPersistenceHandleAfterUpdates(
      Sender: TObject);
    procedure BoldExternalDataPersistenceHandleBeforeUpdates(
      Sender: TObject);
    procedure ItemReadObject(Obj: IPersistentBoldObject;
      ExternalKey: TBoldObjectId; FetchContext: TObject);
    procedure ItemAssignKeyToObject(const Obj: IPersistentBoldObject;
      const ExternalKey: TBoldObjectId);
    function ItemGetKeyFromObject(
      const Obj: IPersistentBoldObject): TBoldObjectId;
    procedure PartReadObject(Obj: IPersistentBoldObject;
      ExternalKey: TBoldObjectId; FetchContext: TObject);
    procedure PartReadMember(Obj: IPersistentBoldObject;
      ExternalKey: TBoldObjectId; Member: TMoldMember;
      FetchContext: TObject);
    procedure VendorReadObject(Obj: IPersistentBoldObject;
      ExternalKey: TBoldObjectId; FetchContext: TObject);
    procedure VendorReadMember(Obj: IPersistentBoldObject;
      ExternalKey: TBoldObjectId; Member: TMoldMember;
      FetchContext: TObject);
    procedure OrderReadMember(Obj: IPersistentBoldObject;
      ExternalKey: TBoldObjectId; Member: TMoldMember;
      FetchContext: TObject);
    function BoldExternalDataPersistenceHandleConfigItemGetInternalSQLForKeys(
      const ExternalKeys: TBoldObjectIdList): String;
  private
    { Private declarations }
    procedure GetKeysFromTable(Table: TTable; Column: String; ExternalKeys: TBoldObjectidList; Filter: String = '');
    procedure ReadMultiLink(Table: TTable; PrimaryKeyColumn, ForeignKeyColumn: String; ExternalKey: tBoldObjectId; IdList: IBoldObjectIdListRef; MoldClassOfOtherEnd: TMoldClass);
    procedure ReadSingleLink(Table: TTable; ForeignKeyColumn: String; SingleLink: IBoldObjectIdref; MoldClassOfOtherEnd: TMoldClass);
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

procedure TdmMain.BoldExternalDataPersistenceHandleActivate(Sender: TObject);
begin
  Database1.Open;
  CustomerTable.Open;
  OrderTable.Open;
  ItemTable.Open;
  PartTable.Open;
  VendorTable.Open;
  EmployeeTable.Open;
end;

procedure TdmMain.BoldExternalDataPersistenceHandleDeActivate(Sender: TObject);
begin
  CustomerTable.close;
  OrderTable.Close;
  EmployeeTable.Close;
  ItemTable.Close;
  PartTable.Close;
  VendorTable.Close;
  Database1.Close;
end;

procedure TdmMain.BoldExternalDataPersistenceHandleAfterUpdates(Sender: TObject);
begin
//  Database1.Commit;
end;

procedure TdmMain.BoldExternalDataPersistenceHandleBeforeUpdates(Sender: TObject);
begin
//  Database1.StartTransaction;
end;


// ===========================================================
// Generic helper functions
// ===========================================================


procedure TdmMain.GetKeysFromTable(Table: TTable; Column: String; ExternalKeys: TBoldObjectidList; Filter: String = '');
var
  tempKey: TBoldDefaultId;
  TempStrKey: TBoldStringId;
  KeyColumnList: TStringList;
  i: integer;
  Field: TField;
  s: string;
begin
  if Filter <> '' then
  begin
    Table.Filter := Filter;
    Table.Filtered := true;
  end;
  Table.First;
  if pos(',', Column) <> 0 then
  begin
    TempStrKey := TBoldStringId.Create;
    KeyColumnList := TStringList.create;
    KeyColumnList.CommaText := Column;
    while not Table.Eof do
    begin
      s := '';
      for i := 0 to KeyColumnList.Count-1 do
      begin
        if s <> '' then
          s := s + ',';
        Field := Table.FieldByName(KeyColumnList[i]);
        case Field.DataType of
          ftString: s := s + Field.AsString;
          ftFloat: s := s + IntToStr(Field.AsInteger);
          ftInteger, ftSmallInt, ftWord: s := s + IntToStr(Field.AsInteger);
          else
            raise Exception.CreateFmt('unsupported fieldtype: %s.%s', [Table.TableName, KeyColumnList[i]]);
        end;
      end;
      TempStrKey.AsString := s;
      ExternalKeys.Add(TempStrKey);
      Table.Next;
    end;
    KeyCOlumnList.Free;
    tempStrKey.free;
  end
  else
  begin
    TempKey := TBoldDefaultId.Create;
    while not Table.Eof do
    begin
      TempKey.AsInteger := Table[Column];
      ExternalKeys.Add(TempKey);
      Table.Next;
    end;
    TempKey.Free;
  end;
  Table.Filtered := false;
end;

procedure TdmMain.ReadMultiLink(Table: TTable; PrimaryKeyColumn,
  ForeignKeyColumn: String; ExternalKey: tBoldObjectId;
  IdList: IBoldObjectIdListRef; MoldClassOfOtherEnd: TMoldClass);
var
  ExternalKeys: TBoldObjectidList;
begin
  ExternalKeys := TBoldObjectidList.Create;
  GetKeysFromTable(Table, PrimaryKeyColumn, ExternalKeys, ForeignKeyColumn+' = '+ExternalKey.AsString);
  BoldExternalDataPersistenceHandle.SetMultiLink(IdList, ExternalKeys, MoldClassOfOtherEnd);
  ExternalKeys.Free;
end;

procedure TdmMain.ReadSingleLink(Table: TTable; ForeignKeyColumn: string; SingleLink: IBoldObjectIdref; MoldClassOfOtherEnd: TMoldClass);
var
  ExternalKey: TBoldDefaultId;
  Field: TField;
begin
  Field := Table.FieldByName(ForeignKeyColumn);
  if Field.IsNull then
    ExternalKey := nil
  else
  begin
    ExternalKey := tBoldDefaultId.Create;
    ExternalKey.AsInteger := Field.AsInteger;
  end;
  BoldExternalDataPersistenceHandle.SetSingleLink(SingleLink, ExternalKey, MoldClassOfOtherEnd);
  ExternalKey.Free;
end;

// ===========================================================
// Customer-events
// ===========================================================

procedure TdmMain.CustomerGetKeyList(const ExternalKeys: TBoldObjectIdList);
begin
  GetKeysFromTable(CustomerTable, 'CustNo', ExternalKeys);
end;

function TdmMain.CustomerExists(const ExternalKey: TBoldObjectId): Boolean;
begin
  result := CustomerTable.Locate('CustNo', StrToInt(ExternalKey.AsString), []);
end;

procedure TdmMain.CustomerUpdateObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace);
var
  Customer: IPersistentCustomer;
begin
  Obj.QueryInterface(IPersistentCustomer, customer);
  if CustomerTable.Locate('CustNo', StrToInt(ExternalKey.AsString), []) then
  begin
    Customertable.Edit;
    try
      CustomerTable['Company'] := Customer.Name;
      CustomerTable.Post;
    except
      Customertable.Cancel;
      raise;
    end;
  end;

end;

procedure TdmMain.CustomerReadObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; FetchContext: TObject);
var
  Customer: IPersistentCustomer;
begin
  Obj.QueryInterface(IPersistentCustomer, customer);
  if CustomerTable.Locate('CustNo', StrToInt(ExternalKey.AsString), []) then
  begin
    Customer.Name := CustomerTable['Company'];
  end
  else
  begin
    // object is not in external database...
  end;
end;

procedure TdmMain.CustomerReadMember(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; Member: TMoldMember; FetchContext: TObject);
var
  Customer: IPersistentCustomer;
begin
  Obj.QueryInterface(IPersistentCustomer, customer);
  if SameText(Member.Name, 'Orders') then
    ReadMultiLink(OrderTable, 'OrderNo', 'CustNo', ExternalKey, customer.Orders, customer.orders_Type)
  else
  begin
    // not supported single-fetch attribute
  end;
end;

procedure TdmMain.CustomerCreateObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace);
var
  Customer: IPersistentCustomer;
begin
  Obj.QueryInterface(IPersistentCustomer, customer);
  CustomerTable.Insert;
  try
    CustomerTable['CustNo'] := StrToInt(ExternalKey.AsString);
    CustomerTable['Company'] := Customer.Name;
    CustomerTable.Post;
  except
    CustomerTable.Cancel;
    raise;
  end;

end;

procedure TdmMain.CustomerDeleteObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId);
begin
  if CustomerTable.Locate('CustNo', StrToInt(ExternalKey.AsString), []) then
    CustomerTable.Delete;
end;


// ===========================================================
// Employee events
// ===========================================================

procedure TdmMain.EmployeeReadMember(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; Member: TMoldMember; FetchContext: TObject);
var
  Employee: IPersistentEmployee;
begin
  Obj.QueryInterface(IPersistentEmployee, Employee);
  if SameText(Member.Name, 'Orders') then
    ReadMultiLink(OrderTable, 'OrderNo', 'EmpNo', ExternalKey, Employee.orders, Employee.orders_Type)
  else
  begin
    // not supported single-fetch attribute
  end;
end;

procedure TdmMain.EmployeeReadObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; FetchContext: TObject);
var
  Employee: IPersistentEmployee;
begin
  Obj.QueryInterface(IPersistentEmployee, Employee);
  if EmployeeTable.Locate('EmpNo', Employee.EmployeeNo, []) then
  begin
    Employee.FirstName := EmployeeTable['FirstName'];
    Employee.LastName := EmployeeTable['LastName'];
  end
end;

// ===========================================================
// Order events
// ===========================================================

procedure TdmMain.OrderReadObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; FetchContext: TObject);
var
  Order: IPersistentOrder;
begin
  Obj.QueryInterface(IPersistentorder, Order);
  if OrderTable.Locate('OrderNo', StrToInt(ExternalKey.AsString), []) then
  begin
    Order.ShipDate := OrderTable['ShipDate'];
    Order.SaleDate := OrderTable['SaleDate'];
    Order.AmountPaid := OrderTable['AmountPaid'];
    ReadSingleLink(OrderTable, 'CustNo', Order.M_Customer, Order.Customer_Type);
    ReadSingleLink(OrderTable, 'EmpNo', Order.M_responsible, Order.responsible_Type);
  end
  else
  begin
    // object is not in external database...
  end;
end;

procedure TdmMain.OrderReadMember(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; Member: TMoldMember; FetchContext: TObject);
var
  Order: IPersistentOrder;
begin
  Obj.QueryInterface(IPersistentOrder, Order);
  if SameText(Member.Name, 'Items') then
    ReadMultiLink(ItemTable, 'OrderNo,ItemNo', 'OrderNo', ExternalKey, Order.items, Order.items_Type)
  else
  begin
    // not supported single-fetch attribute
  end;
end;

procedure TdmMain.OrderUpdateObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace);
var
  Order: IPersistentOrder;
begin
  Obj.QueryInterface(IPersistentorder, Order);
  if OrderTable.Locate('OrderNo', StrToInt(ExternalKey.AsString), []) then
  begin
    try
      OrderTable.Edit;
      OrderTable['CustNo'] := Order.Customer.CustomerID;
      OrderTable['EmpNo'] := Order.responsible.EmployeeNo;
      OrderTable['ShipDate'] := Order.ShipDate;
      OrderTable['SaleDate'] := Order.SaleDate;
      OrderTable['AmountPaid'] := Order.AmountPaid;
      OrderTable.Post;
    except
      OrderTable.Cancel;
      raise;
    end;
  end;
end;

procedure TdmMain.OrderCreateObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace);
var
  Order: IPersistentOrder;
begin
  Obj.QueryInterface(IPersistentorder, Order);
  OrderTable.Insert;
  try
    OrderTable['OrderNo'] := Order.OrderNo;
    OrderTable['CustNo'] := Order.Customer.CustomerID;
    OrderTable['EmpNo'] := Order.responsible.EmployeeNo;
    OrderTable['ShipDate'] := Order.ShipDate;
    OrderTable['SaleDate'] := Order.SaleDate;
    OrderTable['AmountPaid'] := Order.AmountPaid;
    OrderTable.Post;
  except
    OrderTable.Cancel;
    raise;
  end;

end;

procedure TdmMain.OrderDeleteObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId);
begin
  if OrderTable.Locate('OrderNo', StrToInt(ExternalKey.AsString), []) then
    OrderTable.Delete;
end;

// ===========================================================
// Item events
// ===========================================================

// The Item-objects have a more complex external key than the other as it is uniquely defined by
// the OrderNo and ItemNo, we use a StringId to represent its ExternalKey

procedure TdmMain.ItemReadObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; FetchContext: TObject);
var
  Item: IPersistentItem;
begin
  Obj.QueryInterface(IPersistentItem, Item);
  if ItemTable.Locate('OrderNo;ItemNo', VarArrayOf([Item.OrderNo, Item.ItemNo]), []) then
  begin
    Item.Qty := ItemTable['Qty'];
    Item.Discount := ItemTable['Discount'];
    ReadSingleLink(ItemTable, 'PartNo', Item.M_Part, item.Part_Type);
    ReadSingleLink(ItemTable, 'OrderNo', Item.M_Order, item.Order_Type);
  end
  else
  begin
    // object is not in external database...
  end;
end;

procedure TdmMain.ItemAssignKeyToObject(const Obj: IPersistentBoldObject;
  const ExternalKey: TBoldObjectId);
var
  Item: IPersistentItem;
  Key: String;
begin
  Obj.QueryInterface(IPersistentItem, Item);
  Key := ExternalKey.AsString;
  Item.OrderNo := StrToInt(copy(key, 1, pos(',', key)-1));
  Item.ItemNo := StrToInt(copy(key, pos(',', key)+1, maxint))
end;

function TdmMain.ItemGetKeyFromObject(const Obj: IPersistentBoldObject): TBoldObjectId;
var
  Item: IPersistentItem;
  Key: TBoldStringId;
begin
  Obj.QueryInterface(IPersistentItem, Item);
  Key := TBoldStringId.Create;
  Key.AsString := format('%d,%d', [Item.OrderNo, Item.ItemNo]);
  result := Key;
end;

function TdmMain.BoldExternalDataPersistenceHandleConfigItemGetInternalSQLForKeys(const ExternalKeys: TBoldObjectIdList): String;
var
  KeyList: TStringList;
  i: integer;
begin
  result := '';
  KeyList := TStringList.Create;
  for i := 0 to ExternalKeys.count-1 do
  begin
    KeyList.CommaText := ExternalKeys[i].AsString;
    if result <> '' then
      result := result + ' OR ';
    result := result + format('(OrderNo = %s AND ItemNo = %s)', [KeyList[0], KeyList[1]]);
  end;
  KeyList.Free;
end;


// ===========================================================
// Part events
// ===========================================================

procedure TdmMain.PartReadObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; FetchContext: TObject);
var
  Part: IPersistentPart;
begin
  Obj.QueryInterface(IPersistentPart, Part);
  if PartTable.Locate('PartNo', Part.PartNo, []) then
  begin
    Part.Cost := PartTable['Cost'];
    Part.Description := PartTable['Description'];
    Part.ListPrice := PartTable['ListPrice'];
    ReadSingleLink(PartTable, 'VendorNo', Part.M_Vendor, Part.Vendor_Type);
  end
  else
  begin
    // object is not in external database...
  end;
end;

procedure TdmMain.PartReadMember(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; Member: TMoldMember; FetchContext: TObject);
var
  Part: IPersistentPart;
begin
  Obj.QueryInterface(IPersistentPart, Part);
  if SameText(Member.Name, 'Items') then
    ReadMultiLink(ItemTable, 'OrderNo,ItemNo', 'PartNo', ExternalKey, Part.items, Part.items_Type)
  else
  begin
    // not supported single-fetch attribute
  end;
end;

// ===========================================================
// Vendor events
// ===========================================================

procedure TdmMain.VendorReadObject(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; FetchContext: TObject);
var
  Vendor: IPersistentVendor;
begin
  Obj.QueryInterface(IPersistentVendor, Vendor);
  if VendorTable.Locate('VendorNo', Vendor.VendorNo, []) then
  begin
    Vendor.VendorName := VendorTable['VendorName'];
    Vendor.Address1 := VendorTable['Address1'];
    if not VarIsNull(VendorTable['Address2']) then
      Vendor.Address2 := VendorTable['Address2']
    else
      Vendor.Address2 := '';

    if VendorTable['Preferred'] then
      Vendor.Preferred := 0
    else
      Vendor.Preferred := 1;
  end
  else
  begin
    // object is not in external database...
  end;
end;

procedure TdmMain.VendorReadMember(Obj: IPersistentBoldObject;
  ExternalKey: TBoldObjectId; Member: TMoldMember; FetchContext: TObject);
var
  Vendor: IPersistentVendor;
begin
  Obj.QueryInterface(IPersistentVendor, Vendor);
  if SameText(Member.Name, 'Parts') then
    ReadMultiLink(PartTable, 'PartNo', 'VendorNo', ExternalKey, Vendor.parts, Vendor.parts_Type)
  else
  begin
    // not supported single-fetch attribute
  end;
end;

end.

