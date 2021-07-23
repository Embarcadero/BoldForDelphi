unit BoldAFPCxGridProviderUnit;

interface

uses
  cxGrid,
  cxGridBoldSupportUnit,
  BoldAFPDefault,
  BoldAFP,
  BoldAbstractListHandle;

type
  TBoldCxGridFormProviderForList = class(TBoldDefaultFormProvider)
  private
    fGrid: TcxGrid;
    fListHandle: TBoldAbstractListHandle;
    function GetlistHandle: TBoldAbstractListHandle;
  protected
    procedure EnsureHandle; override;
    procedure PreEnsureComponents; override;
    property Grid: TcxGrid read fGrid;
    property ListHandle: TBoldAbstractListHandle read GetlistHandle;
  end;

  TBoldCxGridObjectAutoFormProvider = class(TBoldDefaultObjectAutoFormProvider)
  protected
    procedure EnsureMultiRoleMemberControls; override;
  end;

implementation

uses
  BoldElements,
  BoldReferenceHandle,
  BoldListHandle,
  BoldSystemRT,
//  BoldNavigator,
  BoldSystem,
  Controls,
  ComCtrls,
  SysUtils,
  cxGridCustomPopupMenu,
  cxGridPopupMenu;

const
  BOXMARGIN           = 8;
  BOXSPACING          = 2;
  CONTROLMARGIN       = 10;
  LISTBOXLABELHEIGHT  = 18;
  MAXFORMHEIGHT       = 450;
  MINFORMHEIGHT       = 150;
  EDITHEIGHT          = 20;
  EDITWIDTH           = 121;
  LISTBOXHEIGHT       = 97;
  LISTBOXWIDTH        = 121;
  PANELHEIGHT         = 35;

{ TBoldCxGridFormProviderForList }

procedure TBoldCxGridFormProviderForList.EnsureHandle;
var
  DesignTimeContext: TBoldElementTypeInfo;
begin
  BoldHandle := TBoldReferenceHandle.Create(Form);
  DesignTimeContext := Element.BoldType;
  if DesignTimeContext is TBoldListTypeInfo then
    DesignTimeContext := (DesignTimeContext as TBoldListTypeInfo).ListElementTypeInfo;
  BoldHandle.StaticValueTypeName := DesignTimeContext.ExpressionName;
  BoldHandle.OnObjectDeleted := DefaultReceiveObjectGone;
  BoldHandle.OnValueDestroyed := DefaultReceiveObjectGone;
  inherited;
end;

function TBoldCxGridFormProviderForList.GetlistHandle: TBoldAbstractListHandle;
begin
  if not assigned(fListHandle) then
  begin
    fListHandle := TBoldListHandle.Create(Form);
    fListHandle.RootHandle := BoldHandle;
  end;
  Result := fListHandle;
end;

procedure TBoldCxGridFormProviderForList.PreEnsureComponents;
var
  lcxGridBoldTableView: TcxGridBoldTableView;
  lcxGridPopupMenu: TcxGridPopupMenu;
begin
  inherited;
  fGrid := TcxGrid.Create(Form);
  with fGrid do
  begin
    Name := 'Grid'; // do not localize
    fGrid.Levels.Add;
    lcxGridBoldTableView := fGrid.CreateView(TcxGridBoldTableView) as TcxGridBoldTableView;
    lcxGridBoldTableView.OptionsBehavior.ImmediateEditor := false;
    lcxGridBoldTableView.OptionsBehavior.IncSearch := true;
    Levels[0].GridView := lcxGridBoldTableView;
    lcxGridBoldTableView.OptionsSelection.MultiSelect := true;
    lcxGridBoldTableView.DataController.BoldAutoColumns := true;
    lcxGridBoldTableView.DataController.BoldHandle := ListHandle;

    lcxGridPopupMenu := TcxGridPopupMenu.Create(Form);
    lcxGridPopupMenu.Grid := fGrid;

//    BoldShowConstraints := BoldShowConstraintsInAutoFormGrids;
//    BoldAutoColumns := True;

    Align       := alClient;
    Parent      := Target;
  end;

{  with TBoldNavigator.Create(Form) do
  begin
    Parent := Target;
    Align := alBottom;
    Boldhandle := ListHandle;
    name := 'BoldNavigator'; // do not localize
  end;
}
  Form.Caption := Element.BoldType.ModelName;
  ListHandle.Name := 'BoldListHandle'; // do not localize
end;

{ TBoldCxGridObjectAutoFormProvider }

procedure TBoldCxGridObjectAutoFormProvider.EnsureMultiRoleMemberControls;
var
  i: integer;
  Member: TBoldMemberRTInfo;
  ListHandle: TBoldListHandle;
  lGrid: TcxGrid;
  lView: TcxGridBoldTableView;
  TabSheet: TTabSheet;
  DesInfo: longint;
  ValueType: TBoldElementTypeInfo;
//  Navigator: TBoldNavigator;
begin
  for i := 0 to MemberRTInfoList.Count - 1 do
  begin
    Member := MemberRTInfoList[i];
    if Member.IsMultiRole then
    begin
      if MemberShouldBeDisplayed(Member) then
      begin
        TabSheet := CreateTabSheet(Member.ModelName, MakeComponentName('Tab', ClassTypeInfo, Member)); // do not localize

        listHandle := TBoldListHandle.Create(TabSheet); // must be tabsheet for "ActivateTabSheetHandle" to work
        ListHandle.Enabled := False;
        TabSheet.OnShow := ActivateTabSheetHandle;

        LongRec(desinfo).Lo := CONTROLMARGIN; //set Left
        LongRec(desinfo).Hi := CONTROLMARGIN; //Set Top;
        listHandle.DesignInfo := desInfo;
        listHandle.RootHandle := BoldHandle;
        listHandle.Expression := Member.ExpressionName;
        listHandle.MutableListExpression := Member.ExpressionName;
        listHandle.Name       := MakeComponentName('Handle', ClassTypeInfo, Member); // do not localize
        ValueType := Member.BoldType;
        if ValueType is TBoldListTypeInfo then
          ValueType := (ValueType as TBoldListTypeInfo).ListElementTypeInfo;
        listHandle.RootTypeName := ValueType.ExpressionName;
        listHandle.StaticSystemHandle := self.BoldHandle.StaticSystemHandle;

        lGrid := TcxGrid.Create(TabSheet);
        lGrid.Name := MakeComponentName('Grid', ClassTypeInfo, Member); // do not localize
        lGrid.Levels.Add;
        lView := lGrid.CreateView(TcxGridBoldTableView) as TcxGridBoldTableView;
        lView.OptionsBehavior.ImmediateEditor := false;
        lView.OptionsBehavior.IncSearch := true;
        lGrid.Levels[0].GridView := lView;
        lView.DataController.BoldHandle := ListHandle;
        lView.DataController.BoldAutoColumns := True;
        lView.OptionsSelection.MultiSelect := true;
        lGrid.Align   := alClient;
        lGrid.Parent  := TabSheet;

{        Navigator := TBoldNavigator.Create(Form);
        NAvigator.align := alBottom;
        Navigator.BoldHandle := ListHandle;
        Navigator.Parent := TabSheet;
}
      end;
    end;
  end;
end;

initialization
  AutoFormProviderRegistry.RegisterListProvider(bvtClass, TBoldObjectList, TBoldCxGridFormProviderForList);
  AutoFormProviderRegistry.RegisterProvider(bvtClass, TBoldObject, TBoldCxGridObjectAutoFormProvider);

finalization
  AutoFormProviderRegistry.UnregisterProvider(TBoldDefaultObjectListAutoFormProvider);
  AutoFormProviderRegistry.UnregisterProvider(TBoldCxGridObjectAutoFormProvider);

end.
