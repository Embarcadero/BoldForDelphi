
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//          This source code is distributed            //
//              as part of the examples.               //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

unit fMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, BoldPersistenceNotifier, BoldSubscription, BoldHandle,
  BoldPersistenceHandle, BoldPersistenceHandlePassthrough,
  StdCtrls, BoldHandles,
  Grids, BoldGrid, BoldRootedHandles,
  BoldAbstractListHandle, BoldCursorHandle, BoldListHandle, ExtCtrls,
  BoldNavigatorDefs, BoldNavigator, BoldListBox, BoldLabel, ComCtrls,
  AppEvnts, BoldEdit;

type
  TMainForm = class(TForm)
    blhAllCustomers: TBoldListHandle;
    blhCustomerContacts: TBoldListHandle;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    BoldGrid1: TBoldGrid;
    BoldNavigator1: TBoldNavigator;
    BoldGrid2: TBoldGrid;
    BoldNavigator2: TBoldNavigator;
    Memo1: TMemo;
    BoldLabel1: TBoldLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnOpen: TButton;
    blhOrders: TBoldListHandle;
    BoldListBox1: TBoldListBox;
    Label2: TLabel;
    BoldNavigator3: TBoldNavigator;
    btnSave: TButton;
    btnCreateDB: TButton;
    ApplicationEvents1: TApplicationEvents;
    BoldEdit1: TBoldEdit;
    Label5: TLabel;
    BoldGrid3: TBoldGrid;
    blhItems: TBoldListHandle;
    Label6: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  BusinessClasses,
  dMain;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  dmMain.BoldUpdateDBAction1.Execute;
end;

procedure TMainForm.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  if dmMain.bephEmployee.HasNewObjects then
  begin
    Memo1.Lines.Add(Format('%d new objects added', [dmMain.bephEmployee.NewObjects.Count]));
    dmMain.bephEmployee.NewObjects.Clear;
  end;
  if dmMain.bephEmployee.HasDeletedObjects then
  begin
    Memo1.Lines.Add(Format('%d objects deleted in external database', [dmMain.bephEmployee.DeletedObjects.Count]));
    dmMain.bephEmployee.DeletedObjects.Clear;
  end;
end;

end.