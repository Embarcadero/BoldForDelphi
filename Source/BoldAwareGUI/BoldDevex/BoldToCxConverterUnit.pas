unit BoldToCxConverterUnit;

interface

uses
  Classes,
  Controls,
  SysUtils,
  BoldEdit,
  BoldVariantControlPack,
  BoldStringControlPack,
  BoldComboBox,
  BoldGrid,
  BoldMemo,
  BoldLabel,
  Buttons,
  cxBoldEditors,
  cxGridBoldSupportUnit,
  cxGridPopupMenu,
  BoldToCxConverterForm,
  BoldCaptionController,
  BoldPropertiesController,
  StdCtrls,
  cxGridCustomTableView,
  cxTextEdit,
  cxLabel,
  cxGroupBox,
  cxButtons,
  cxMemo,
  TypInfo;

type
  TBoldToCxConverter = class(TComponent)
  private
    fBoldToCxConverterForm : TfrmBoldToCxConverter;
    fBoldComponents : TStringList;
    fLog: TStringList;

    function ConvertGrid(aBoldGrid : TBoldGrid): Boolean;
    procedure CopyBoldColumn(aBoldColumn : TBoldGridColumn; aCxGridBoldColumn : TCxGridBoldColumn);
    procedure CountNumberOfBoldComponents(var vBoldComponentCount: Integer; AOwner: TComponent);
    procedure FoundBoldComponent(aComponent: TComponent);
    procedure FoundTBoldGrid(aGrid: TBoldGrid);
    procedure FoundTBoldEdit(aEdit: TBoldEdit);
    procedure FoundTBoldMemo(aMemo: TBoldMemo);
    procedure FoundTBoldLabel(aLabel: TBoldLabel);
    procedure FoundTBoldComboBox(aComboBox : TBoldComboBox);
    procedure FoundTEdit(aEdit: TEdit);
    procedure FoundTLabel(aLabel: TLabel);
    procedure FoundTMemo(aMemo: TMemo);
    procedure FoundTGroupBox(aBox: TGroupBox);
    procedure FoundTSpeedButton(aButton: TSpeedButton);
    function ConvertTBoldEdit(aBoldEdit: TBoldEdit): Boolean;
    function ConvertTBoldEditToDateEdit(aBoldEdit: TBoldEdit): Boolean;
    function ConvertTBoldMemo(aBoldMemo: TBoldMemo): Boolean;
    function ConvertTBoldLabel(aBoldLabel: TBoldLabel): Boolean;
    function ConvertTBoldComboBox(aBoldComboBox: TBoldComboBox): Boolean;
    function ConvertTEdit(aEdit: TEdit): Boolean;
    function ConvertTMemo(aMemo: TMemo): Boolean;
    function ConvertTLabel(aLabel: TLabel): Boolean;
    function ConvertTGroupBox(aBox: TGroupBox): Boolean;
    function ConvertTSpeedButton(aButton: TSpeedButton): Boolean;
    procedure CopyPublishedProperties(FromControl, ToControl: TControl);
    procedure CopyFollowerProperties(FromFollower: TBoldStringFollowerController; ToFollower: TBoldVariantFollowerController);
    procedure CopyBoldComboListControllerProperties(FromComboListController: BoldComboBox.TBoldComboListController; ToComboListController: cxBoldEditors.TBoldComboListController);
    procedure CheckEvents(AEventList: array of string; AObject: TObject);
    function TryRename(aComponent: TComponent): Boolean;
  protected
    function GridController : TcxGridDataController;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy;override;
    { Public declarations }
  published
    { Published declarations }
  end;

const
  Ctrlf = #13#10;

//  cTControlEvents: array[0..9] of string = ('OnAlignPosition', 'OnDockDrop', 'OnDockOver', 'OnEnter', 'OnExit', 'OnGetSiteInfo', 'OnKeyDown', 'OnKeyPress', 'OnKeyUp', 'OnUnDock');
//  cTWinControlEvents: array[0..10] of string = ('OnAlignInsertBefore', 'OnAlignPosition', 'OnDockDrop', 'OnDockOver', 'OnEnter', 'OnExit', 'OnGetSiteInfo', 'OnKeyDown', 'OnKeyPress', 'OnKeyUp', 'OnUnDock');
{  cTEditEvents: array[0..20] of String = ('OnChange', 'OnClick', 'OnContextPopup', 'OnDblClick', 'OnDragDrop', 'OnDragOver', 'OnEndDock',
        'OnEndDrag', 'OnEnter', 'OnExit', 'OnKeyDown', 'OnKeyPress', 'OnKeyUp', 'OnMouseActivate', 'OnMouseDown',
        'OnMouseEnter', 'OnMouseLeave', 'OnMouseMove', 'OnMouseUp', 'OnStartDock', 'OnStartDrag');
  cTLabelEvents: array[0..14] of String = ('OnClick', 'OnContextPopup', 'OnDblClick', 'OnDragDrop', 'OnDragOver', 'OnEndDock', 'OnEndDrag',
        'OnMouseActivate', 'OnMouseDown', 'OnMouseMove', 'OnMouseUp', 'OnMouseEnter', 'OnMouseLeave', 'OnStartDock', 'OnStartDrag');
  cTMemoEvents: array[0..20] of String = ('OnChange', 'OnClick', 'OnContextPopup', 'OnDblClick', 'OnDragDrop', 'OnDragOver', 'OnEndDock', 'OnEndDrag',
        'OnEnter', 'OnExit', 'OnKeyDown', 'OnKeyPress', 'OnKeyUp', 'OnMouseActivate', 'OnMouseDown', 'OnMouseEnter',
        'OnMouseLeave', 'OnMouseMove', 'OnMouseUp', 'OnStartDock', 'OnStartDrag');
  cTGroupBoxEvents: array[0..22] of String = ('OnAlignInsertBefore', 'OnAlignPosition', 'OnClick', 'OnContextPopup', 'OnDblClick', 'OnDragDrop', 'OnDockDrop',
        'OnDockOver', 'OnDragOver', 'OnEndDock', 'OnEndDrag', 'OnEnter', 'OnExit', 'OnGetSiteInfo', 'OnMouseActivate',
        'OnMouseDown', 'OnMouseEnter', 'OnMouseLeave', 'OnMouseMove', 'OnMouseUp', 'OnStartDock', 'OnStartDrag', 'OnUnDock');
  cTSpeedButtonEvents: array[0..7] of String = ('OnClick', 'OnDblClick', 'OnMouseActivate', 'OnMouseDown', 'OnMouseEnter', 'OnMouseLeave', 'OnMouseMove', 'OnMouseUp');}

  cEvents: array[0..32] of String = ('OnAlignInsertBefore', 'OnAlignPosition', 'OnCanResize', 'OnChange', 'OnClick', 'OnConstrainedResize', 'OnContextPopup',
        'OnDblClick', 'OnDockDrop', 'OnDockOver', 'OnDragDrop', 'OnDragOver', 'OnEndDock', 'OnEndDrag', 'OnEnter', 'OnExit',
        'OnGetSiteInfo', 'OnKeyDown', 'OnKeyPress', 'OnKeyUp', 'OnMouseActivate', 'OnMouseDown', 'OnMouseEnter', 'OnMouseLeave',
        'OnMouseMove', 'OnMouseUp', 'OnMouseWheel', 'OnMouseWheelDown', 'OnMouseWheelUp', 'OnResize', 'OnStartDock', 'OnStartDrag', 'OnUnDock');

procedure Register;

implementation
uses Dialogs, cxGrid,Forms, BoldElements,  CxEdit, CxGridLevel;

procedure Register;
begin
  RegisterComponents('AT Core', [TBoldToCxConverter]);
end;

{ TBoldToCxGridConverter }

constructor TBoldToCxConverter.Create(AOwner: TComponent);
var
  vBoldComponentCount : Integer;
  I,j : Integer;
  vComponent: TComponent;
  res: Boolean;
begin
  inherited;
  fBoldComponents := TStringList.Create;
  fLog := TStringList.Create;
  Application.CreateForm(TfrmBoldToCxConverter,fBoldToCxConverterForm);
  CountNumberOfBoldComponents(vBoldComponentCount, AOwner);
  GridController.RecordCount := vBoldComponentCount;

  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    vComponent := AOwner.Components[I];
    // TODO
    if (vComponent is TBoldGrid) then FoundTBoldGrid(vComponent as TBoldGrid);
    if (vComponent is TBoldEdit) then FoundTBoldEdit(vComponent as TBoldEdit);
    if (vComponent is TBoldMemo) then FoundTBoldMemo(vComponent as TBoldMemo);
    if (vComponent is TBoldLabel) then FoundTBoldLabel(vComponent as TBoldLabel);
    if (vComponent is TBoldComboBox) then FoundTBoldComboBox(vComponent as TBoldComboBox);
    if (vComponent is TEdit) then FoundTEdit(vComponent as TEdit);
    if (vComponent is TLabel) then FoundTLabel(vComponent as TLabel);
    if (vComponent is TMemo) then FoundTMemo(vComponent as TMemo);
    if (vComponent is TGroupBox) then FoundTGroupBox(vComponent as TGroupBox);
    if (vComponent is TSpeedButton) then FoundTSpeedButton(vComponent as TSpeedButton);

    if (vComponent is TBoldCaptionController) then fLog.Add('Achtung: Form enthält einen TBoldCaptionController');
    if (vComponent is TBoldPropertiesController) then fLog.Add('Achtung: Form enthält einen TBoldPropertiesController');
  end;

  if fLog.Count > 0 then
    fBoldToCxConverterForm.cxMemoLog.Lines.AddStrings(fLog);

  if fBoldToCxConverterForm.ShowModal = mrOK then
  begin
    for j := GridController.RecordCount - 1 downto 0 do
    if not GridController.IsRowSelected(j) then
      fBoldComponents.Delete(fBoldComponents.IndexOf(GridController.GetValue(j,0)));
    for I := fBoldComponents.Count - 1 downto 0 do
    begin
        vComponent := fBoldComponents.Objects[I] as TComponent;
        res := false;
        // TODO
        if vComponent is TBoldGrid then    res := ConvertGrid(vComponent as TBoldGrid);
        if vComponent is TBoldMemo then    res := ConvertTBoldMemo(vComponent as TBoldMemo);
        if vComponent is TBoldLabel then   res := ConvertTBoldLabel(vComponent as TBoldLabel);
        if vComponent is TEdit then        res := ConvertTEdit(vComponent as TEdit);
        if vComponent is TMemo then        res := ConvertTMemo(vComponent as TMemo);
        if vComponent is TLabel then       res := ConvertTLabel(vComponent as TLabel);
        if vComponent is TGroupBox then    res := ConvertTGroupBox(vComponent as TGroupBox);
        if vComponent is TSpeedButton then res := ConvertTSpeedButton(vComponent as TSpeedButton);
        if vComponent is TBoldEdit then
        begin
          if GridController.Values[I,3] = 'TcxBoldDateEdit' then
            res := ConvertTBoldEditToDateEdit(vComponent as TBoldEdit)
          else
            res := ConvertTBoldEdit(vComponent as TBoldEdit)
        end;
        if vComponent is TBoldComboBox then res := ConvertTBoldComboBox(vComponent as TBoldComboBox);

        // Altes Component löschen
        if res and fBoldToCxConverterForm.cxRemoveAfterConvertionCheckbox.Checked then
          vComponent.Free;
    end;
  end;
end;

destructor TBoldToCxConverter.Destroy;
begin
  fBoldToCxConverterForm.Close;
  fBoldToCxConverterForm.Free;
  fBoldComponents.Free;
  fLog.Free;
  inherited;
end;

procedure TBoldToCxConverter.FoundBoldComponent(aComponent : TComponent);
begin
  fBoldComponents.AddObject(aComponent.Name,aComponent);
  GridController.Values[fBoldComponents.Count - 1, 0] := aComponent.Name;
  GridController.Values[fBoldComponents.Count - 1, 1] := aComponent.ClassName;
end;

procedure TBoldToCxConverter.FoundTBoldComboBox(aComboBox: TBoldComboBox);
begin
  FoundBoldComponent(aComboBox);
  GridController.Values[fBoldComponents.Count - 1, 3] := 'TcxBoldComboBox';
end;

procedure TBoldToCxConverter.FoundTBoldGrid(aGrid : TBoldGrid);
begin
  FoundBoldComponent(aGrid);
  GridController.Values[fBoldComponents.Count - 1, 3] := 'TcxGridBoldTableView';
end;

procedure TBoldToCxConverter.FoundTBoldLabel(aLabel: TBoldLabel);
begin
  CheckEvents(cEvents, aLabel);
  FoundBoldComponent(aLabel);
  GridController.Values[fBoldComponents.Count - 1, 3] := 'TcxBoldLabel';
end;

procedure TBoldToCxConverter.FoundTBoldMemo(aMemo: TBoldMemo);
begin
  CheckEvents(cEvents, aMemo);
  FoundBoldComponent(aMemo);
  GridController.Values[fBoldComponents.Count - 1, 3] := 'TcxBoldMemo';
end;

procedure TBoldToCxConverter.FoundTEdit(aEdit: TEdit);
begin
  CheckEvents(cEvents, aEdit);
  FoundBoldComponent(aEdit);
  GridController.Values[fBoldComponents.Count - 1, 3] := 'TcxTextEdit';
end;

procedure TBoldToCxConverter.FoundTGroupBox(aBox: TGroupBox);
begin
  CheckEvents(cEvents, aBox);
  FoundBoldComponent(aBox);
  GridController.Values[fBoldComponents.Count - 1, 3] := 'TcxGroupBox';
end;

procedure TBoldToCxConverter.FoundTLabel(aLabel: TLabel);
begin
  CheckEvents(cEvents, aLabel);
  FoundBoldComponent(aLabel);
  GridController.Values[fBoldComponents.Count - 1, 3] := 'TcxLabel';
end;

procedure TBoldToCxConverter.FoundTMemo(aMemo: TMemo);
begin
  CheckEvents(cEvents, aMemo);
  FoundBoldComponent(aMemo);
  GridController.Values[fBoldComponents.Count - 1, 3] := 'TcxMemo';
end;

procedure TBoldToCxConverter.FoundTSpeedButton(aButton: TSpeedButton);
begin
  CheckEvents(cEvents, aButton);
  FoundBoldComponent(aButton);
  GridController.Values[fBoldComponents.Count - 1, 3] := 'TcxButton';
end;

procedure TBoldToCxConverter.FoundTBoldEdit(aEdit : TBoldEdit);
var
  ExpressionType: TBoldElementTypeInfo;
  vProposedComponent: string;
begin
  CheckEvents(cEvents, aEdit);
  ExpressionType := nil;
  FoundBoldComponent(aEdit);
  vProposedComponent := 'Unknown';

  if Assigned(aEdit.BoldHandle) and Assigned(aEdit.BoldHandle.BoldType) and Assigned(aEdit.BoldHandle.BoldType.Evaluator) then
    ExpressionType := aEdit.BoldHandle.BoldType.Evaluator.ExpressionType(aEdit.BoldProperties.expression,aEdit.BoldHandle.BoldType,false);

  if Assigned(ExpressionType) then
  begin
    GridController.Values[fBoldComponents.Count - 1, 2] := ExpressionType.DelphiName;

    if ExpressionType.DelphiName = 'TBADateTime' then
      vProposedComponent := 'TcxBoldDateEdit'
    else if ExpressionType.DelphiName = 'TBABoolean' then
      vProposedComponent := 'TcxBoldCheckBox'
    else
      vProposedComponent := 'TcxBoldTextEdit';
  end;
  GridController.Values[fBoldComponents.Count - 1, 3] := vProposedComponent;
end;

function TBoldToCxConverter.GridController: TcxGridDataController;
begin
  Result := fBoldToCxConverterForm.tv.DataController;
end;

function TBoldToCxConverter.TryRename(aComponent: TComponent): Boolean;
begin
  try
    ValidateRename(aComponent, aComponent.Name, 'old' + aComponent.Name);
    result := true;
  except
    on e: Exception do begin
      ShowMessage(Format('%s (%s): %s', [aComponent.Name, aComponent.ClassName, e.Message]));
      result := false;
    end;
  end;
end;

procedure TBoldToCxConverter.CountNumberOfBoldComponents(var vBoldComponentCount: Integer; AOwner: TComponent);
var
  I: Integer;
  vComponent: TComponent;
begin
  vBoldComponentCount := 0;
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    vComponent := AOwner.Components[I];
    // TODO
    if (vComponent is TBoldGrid) or (vComponent is TBoldEdit) or (vComponent is TBoldComboBox) or (vComponent is TBoldMemo) or (vComponent is TBoldLabel)
      or (vComponent is TEdit) or (vComponent is TLabel) or (vComponent is TMemo) or (vComponent is TGroupBox) or (vComponent is TSpeedButton)
    then
      Inc(vBoldComponentCount);
  end;
end;

procedure TBoldToCxConverter.CopyBoldColumn(aBoldColumn: TBoldGridColumn;aCxGridBoldColumn: TCxGridBoldColumn);
begin
  aCxGridBoldColumn.DataBinding.BoldProperties.Expression := aBoldColumn.BoldProperties.Expression;
  aCxGridBoldColumn.Caption := aBoldColumn.Title.Caption;
  aCxGridBoldColumn.name := 'Column' + IntToStr(aCxGridBoldColumn.Index);
  if aBoldColumn.BoldProperties.Expression <> '' then
  try
    aCxGridBoldColumn.name := 'col' + AnsiUpperCase(aBoldColumn.BoldProperties.Expression[1]) + Copy(aBoldColumn.BoldProperties.Expression, 2, 1000);
  except
    // ignore rename attept
  end;
end;

procedure TBoldToCxConverter.CopyBoldComboListControllerProperties(FromComboListController : BoldComboBox.TBoldComboListController; ToComboListController: cxBoldEditors.TBoldComboListController);
begin
  with FromComboListController do
  begin
    ToComboListController.DragMode := DragMode;
    ToComboListController.DropMode := DropMode;
    ToComboListController.NilElementMode := NilElementMode;
  end;

end;

procedure TBoldToCxConverter.CheckEvents(AEventList: array of string; AObject: TObject);
var
  i: Integer;
  method: TMethod;
  str, name: String;
begin
  str := '';
  for i := Low(AEventList) to High(AEventList) do begin
    if TypInfo.IsPublishedProp(AObject, AEventList[i]) then begin
      method := TypInfo.GetMethodProp(AObject, AEventList[i]);
      if method.Data <> nil then begin
        name := TObject(Method.Data).MethodName(Method.Code);
        str := str + Format('%s=%s; ', [AEventList[i], name]);
      end;
    end;
  end;
  if str <> '' then
    fLog.Add(Format('Events von %s (%s):    %s', [(AObject as TComponent).Name, AObject.ClassName, str]));
end;

function TBoldToCxConverter.ConvertGrid(aBoldGrid : TBoldGrid): Boolean;
var
  vCxGrid : TCxGrid;
  vCxLevel: TCxGridLevel;
  vCxGridBoldTableView : TcxGridBoldTableView;
  vCol : Integer;
begin
  result := false;
  if not TryRename(aBoldGrid) then Exit;
  vCxGrid := TcxGrid.Create(aBoldGrid.Owner);
  CopyPublishedProperties	(aBoldGrid,vCxGrid);

  vCxGridBoldTableView := vCxGrid.CreateView(TcxGridBoldTableView) as TcxGridBoldTableView;
  vCxGridBoldTableView.Name := vCxGrid.Name + 'BoldTableView';

  vCxGridBoldTableView.DataController.BoldHandle := aBoldGrid.BoldHandle;
  if vCxGrid.Levels.Count = 0 then
    vCxLevel := vCxGrid.Levels.Add
  else
    vCxLevel := vCxGrid.Levels[0];
  vCxLevel.GridView := vCxGridBoldTableView;
  vCxGridBoldTableView.OptionsData.Editing := False;
  vCxGridBoldTableView.OptionsView.GroupByBox := False;
  vCxGridBoldTableView.OptionsBehavior.CellHints := True;
  vCxGridBoldTableView.OptionsSelection.CellSelect := False;

  for vCol := 1 to aBoldGrid.ColCount - 1 do
    CopyBoldColumn(aBoldGrid.Columns[vCol],vCxGridBoldTableView.CreateItem as TcxGridBoldColumn );
  result := true;
end;

function TBoldToCxConverter.ConvertTEdit(aEdit: TEdit): Boolean;
var
  vCxEdit : TcxTextEdit;
begin
  result := false;
  if not TryRename(aEdit) then Exit;
  vCxEdit := TcxTextEdit.Create(aEdit.Owner);
  vCxEdit.Autosize := aEdit.Autosize;
  CopyPublishedProperties(aEdit, vCxEdit);

  vCxEdit.Properties.AutoSelect := aEdit.AutoSelect;
  vCXEdit.Style.BorderStyle := ebsFlat; //ABoldEdit.BorderStyle;
  vCxEdit.Properties.CharCase := aEdit.CharCase;
  vCXEdit.Style.Color := aEdit.Color;
  vCxEdit.DragCursor := aEdit.DragCursor;
  vCxEdit.DragKind := aEdit.DragKind;
  vCxEdit.DragMode := aEdit.DragMode;
  vCxEdit.Style.Font := aEdit.Font;
  vCxEdit.ParentColor := aEdit.ParentColor;    //??
  vCxEdit.ParentFont := aEdit.ParentFont;      //??
  vCxEdit.Properties.PasswordChar := aEdit.PasswordChar;
  vCxEdit.PopupMenu := aEdit.PopupMenu;
  vCxEdit.TabOrder := aEdit.TabOrder;
  vCxEdit.TabStop := aEdit.TabStop;

  vCxEdit.Text := aEdit.Text;
  if aEdit.ReadOnly then
    vCxEdit.Properties.ReadOnly := aEdit.ReadOnly;
  if aEdit.MaxLength <> 0 then
    vCxEdit.Properties.MaxLength := aEdit.MaxLength;
  result := true;
end;

function TBoldToCxConverter.ConvertTGroupBox(aBox: TGroupBox): Boolean;
var
  vCxEdit : TcxGroupBox;
  i: Integer;
begin
  result := false;
  if not TryRename(aBox) then Exit;
  vCxEdit := TcxGroupBox.Create(aBox.Owner);
  CopyPublishedProperties(aBox, vCxEdit);

  vCXEdit.Style.BorderStyle := ebsFlat; //ABoldEdit.BorderStyle;
  vCXEdit.Style.Color := aBox.Color;
  vCxEdit.DragCursor := aBox.DragCursor;
  vCxEdit.DragKind := aBox.DragKind;
  vCxEdit.DragMode := aBox.DragMode;
  vCxEdit.Style.Font := aBox.Font;
  vCxEdit.ParentColor := aBox.ParentColor;    //??
  vCxEdit.ParentFont := aBox.ParentFont;      //??
  vCxEdit.PopupMenu := aBox.PopupMenu;
  vCxEdit.TabOrder := aBox.TabOrder;
  vCxEdit.TabStop := aBox.TabStop;

  vCxEdit.Caption := aBox.Caption;

  for i := aBox.ControlCount - 1 downto 0 do
    (aBox.Controls[i] as TControl).Parent := vCxEdit;
  result := true;
end;

function TBoldToCxConverter.ConvertTLabel(aLabel: TLabel): Boolean;
var
  vCxEdit : TcxLabel;
begin
  result := false;
  if not TryRename(aLabel) then Exit;
  vCxEdit := TcxLabel.Create(aLabel.Owner);
  vCxEdit.Properties.WordWrap := aLabel.WordWrap;
  vCxEdit.Autosize := aLabel.Autosize;
  CopyPublishedProperties(aLabel, vCxEdit);

  vCXEdit.Style.BorderStyle := ebsNone;
  vCXEdit.Style.Color := aLabel.Color;
  vCxEdit.DragCursor := aLabel.DragCursor;
  vCxEdit.DragKind := aLabel.DragKind;
  vCxEdit.DragMode := aLabel.DragMode;
  vCxEdit.Style.Font := aLabel.Font;
  vCxEdit.ParentColor := aLabel.ParentColor; //??
  vCxEdit.ParentFont := aLabel.ParentFont;   //??
  vCxEdit.PopupMenu := aLabel.PopupMenu;

  vCxEdit.Caption := aLabel.Caption;
  vCxEdit.Properties.Alignment.Horz := aLabel.Alignment;
  case aLabel.Layout of
    tlTop:    vCxEdit.Properties.Alignment.Vert := taTopJustify;
    tlCenter: vCxEdit.Properties.Alignment.Vert := taVCenter;
    tlBottom: vCxEdit.Properties.Alignment.Vert := taBottomJustify;
  end;

  vCxEdit.Transparent := True;
  result := true;
end;

function TBoldToCxConverter.ConvertTMemo(aMemo: TMemo): Boolean;
var
  vCxEdit : TcxMemo;
begin
  result := false;
  if not TryRename(aMemo) then Exit;
  vCxEdit := TcxMemo.Create(aMemo.Owner);
  CopyPublishedProperties(aMemo, vCxEdit);

  vCXEdit.Style.BorderStyle := ebsFlat;
  vCXEdit.Style.Color := aMemo.Color;
  vCxEdit.DragCursor := aMemo.DragCursor;
  vCxEdit.DragKind := aMemo.DragKind;
  vCxEdit.DragMode := aMemo.DragMode;
  vCxEdit.Style.Font := aMemo.Font;
  vCxEdit.ParentColor := aMemo.ParentColor;  //??
  vCxEdit.ParentFont := aMemo.ParentFont;    //??
  vCxEdit.PopupMenu := aMemo.PopupMenu;
  if aMemo.ReadOnly then
    vCxEdit.Properties.ReadOnly := aMemo.ReadOnly;
  vCxEdit.TabOrder := aMemo.TabOrder;
  vCxEdit.TabStop := aMemo.TabStop;

  vCxEdit.Lines := aMemo.Lines;
  vCxEdit.Properties.ScrollBars := aMemo.ScrollBars;
  vCxEdit.Properties.WantReturns := aMemo.WantReturns;
  vCxEdit.Properties.WantTabs := aMemo.WantTabs;
  vCxEdit.Properties.WordWrap := aMemo.WordWrap;
  result := true;
end;

function TBoldToCxConverter.ConvertTSpeedButton(aButton: TSpeedButton): Boolean;
var
  vCxEdit : TcxButton;
begin
  result := false;
  if not TryRename(aButton) then Exit;
  vCxEdit := TcxButton.Create(aButton.Owner);
  CopyPublishedProperties(aButton, vCxEdit);

  vCxEdit.ParentFont := aButton.ParentFont;      //??
  vCxEdit.PopupMenu := aButton.PopupMenu;
  vCxEdit.Caption := aButton.Caption;
  if aButton.Glyph <> nil then
    vCxEdit.Glyph.SetBitmap(aButton.Glyph);
  vCxEdit.TabStop := false;
  vCxEdit.SpeedButtonOptions.CanBeFocused := false;
  if aButton.Action <> nil then
    vCxEdit.Action := aButton.Action;
  result := true;
end;

procedure TBoldToCxConverter.CopyPublishedProperties(FromControl, ToControl : TControl);
var
  OldName : string;
begin

  with FromControl do
  begin
    ToControl.Align := Align;
    ToControl.AlignWithMargins := AlignWithMargins;
    ToControl.Anchors := Anchors;
    ToControl.Constraints := Constraints;
    ToControl.Cursor := Cursor;
    ToControl.Height := Height;
    ToControl.HelpContext := HelpContext;
    ToControl.HelpKeyword := HelpKeyword;
    ToControl.HelpType := HelpType;
    ToControl.Hint := Hint;
    ToControl.Left := Left;
    ToControl.Margins := Margins;
    ToControl.Parent := Parent;
    ToControl.Tag := Tag;
    ToControl.Top := Top;
    ToControl.Width := Width;
    ToControl.Enabled := Enabled;
    ToControl.ShowHint := ShowHint;
    ToControl.Visible := Visible;

    OldName := Name;
    Name := 'old' + Name;
  end;
  ToControl.Name := OldName;

end;

function TBoldToCxConverter.ConvertTBoldEdit(aBoldEdit : TBoldEdit): Boolean;
var
  vCxBoldEdit : TcxBoldTextEdit;
begin
  result := false;
  if not TryRename(aBoldEdit) then Exit;
  vCxBoldEdit := TcxBoldTextEdit.Create(aBoldEdit.Owner);
  CopyPublishedProperties(aBoldEdit, vCxBoldEdit);
  vCxBoldEdit.DataBinding.BoldHandle := aBoldEdit.BoldHandle;
  vCxBoldEdit.DataBinding.BoldProperties.Expression := aBoldEdit.BoldProperties.Expression;
  CopyFollowerProperties(aBoldEdit.BoldProperties,vCxBoldEdit.DataBinding.BoldProperties);

  vCxBoldEdit.Properties.Alignment.Horz := aBoldEdit.Alignment;
  vCxBoldEdit.Properties.AutoSelect := aBoldEdit.AutoSelect;
  vCxBoldEdit.Autosize := aBoldEdit.Autosize;
  vCXBoldEdit.Style.BorderStyle := ebsFlat; //ABoldEdit.BorderStyle;
  vCxBoldEdit.Properties.CharCase := aBoldEdit.CharCase;
  vCXBoldEdit.Style.Color := aBoldEdit.Color;
  vCxBoldEdit.DragCursor := aBoldEdit.DragCursor;
  vCxBoldEdit.DragKind := aBoldEdit.DragKind;
  vCxBoldEdit.DragMode := aBoldEdit.DragMode;
  vCxBoldEdit.Style.Font := aBoldEdit.Font;
  vCxBoldEdit.ParentColor := aBoldEdit.ParentColor;     //??
  vCxBoldEdit.ParentFont := aBoldEdit.ParentFont;       //??
  vCxBoldEdit.Properties.PasswordChar := aBoldEdit.PasswordChar;

  vCxBoldEdit.PopupMenu := aBoldEdit.PopupMenu;
  if aBoldEdit.ReadOnly then
    vCxBoldEdit.Properties.ReadOnly := aBoldEdit.ReadOnly;
  if aBoldEdit.MaxLength <> 0 then
    vCxBoldEdit.Properties.MaxLength := aBoldEdit.MaxLength;
  vCxBoldEdit.TabOrder := aBoldEdit.TabOrder;
  vCxBoldEdit.TabStop := aBoldEdit.TabStop;
  vCxBoldEdit.Text := '';
  result := true;
end;

function TBoldToCxConverter.ConvertTBoldEditToDateEdit(aBoldEdit : TBoldEdit): Boolean;
var
  vCxBoldDateEdit : TcxBoldDateEdit;
begin
  result := false;
  if not TryRename(aBoldEdit) then Exit;
  vCxBoldDateEdit := TcxBoldDateEdit.Create(aBoldEdit.Owner);
  CopyPublishedProperties(aBoldEdit, vCxBoldDateEdit);
  vCxBoldDateEdit.DataBinding.BoldHandle := aBoldEdit.BoldHandle;
  vCxBoldDateEdit.DataBinding.BoldProperties.Expression := aBoldEdit.BoldProperties.Expression;
  CopyFollowerProperties(aBoldEdit.BoldProperties,vCxBoldDateEdit.DataBinding.BoldProperties);

  vCxBoldDateEdit.Properties.Alignment.Horz := aBoldEdit.Alignment;
  vCxBoldDateEdit.Properties.AutoSelect := aBoldEdit.AutoSelect;
  vCxBoldDateEdit.Autosize := aBoldEdit.Autosize;
  vCxBoldDateEdit.Style.BorderStyle := ebsFlat; //ABoldEdit.BorderStyle;
  vCxBoldDateEdit.Properties.CharCase := aBoldEdit.CharCase;
  vCxBoldDateEdit.Style.Color := aBoldEdit.Color;
  vCxBoldDateEdit.DragCursor := aBoldEdit.DragCursor;
  vCxBoldDateEdit.DragKind := aBoldEdit.DragKind;
  vCxBoldDateEdit.DragMode := aBoldEdit.DragMode;
  vCxBoldDateEdit.Style.Font := aBoldEdit.Font;
  vCxBoldDateEdit.ParentColor := aBoldEdit.ParentColor;  //??
  vCxBoldDateEdit.ParentFont := aBoldEdit.ParentFont;    //??
  vCxBoldDateEdit.Properties.PasswordChar := aBoldEdit.PasswordChar;

  vCxBoldDateEdit.PopupMenu := aBoldEdit.PopupMenu;
  if aBoldEdit.ReadOnly then
    vCxBoldDateEdit.Properties.ReadOnly := aBoldEdit.ReadOnly;
  if aBoldEdit.MaxLength <> 0 then
    vCxBoldDateEdit.Properties.MaxLength := aBoldEdit.MaxLength;
  vCxBoldDateEdit.TabOrder := aBoldEdit.TabOrder;
  vCxBoldDateEdit.TabStop := aBoldEdit.TabStop;
  result := true;
end;


function TBoldToCxConverter.ConvertTBoldLabel(aBoldLabel: TBoldLabel): Boolean;
var
  vCxEdit : TcxBoldLabel;
begin
  result := false;
  if not TryRename(aBoldLabel) then Exit;
  vCxEdit := TcxBoldLabel.Create(aBoldLabel.Owner);
  vCxEdit.Properties.WordWrap := aBoldLabel.WordWrap;
  vCxEdit.Autosize := aBoldLabel.Autosize;
  CopyPublishedProperties(aBoldLabel, vCxEdit);

  vCxEdit.DataBinding.BoldHandle := aBoldLabel.BoldHandle;
  vCxEdit.DataBinding.BoldProperties.Expression := aBoldLabel.BoldProperties.Expression;
  CopyFollowerProperties(aBoldLabel.BoldProperties,vCxEdit.DataBinding.BoldProperties);

  vCXEdit.Style.BorderStyle := ebsNone;
  vCXEdit.Style.Color := aBoldLabel.Color;
  vCxEdit.DragCursor := aBoldLabel.DragCursor;
  vCxEdit.DragKind := aBoldLabel.DragKind;
  vCxEdit.DragMode := aBoldLabel.DragMode;
  vCxEdit.Style.Font := aBoldLabel.Font;
  vCxEdit.ParentColor := aBoldLabel.ParentColor; //??
  vCxEdit.ParentFont := aBoldLabel.ParentFont;   //??
  vCxEdit.PopupMenu := aBoldLabel.PopupMenu;

  vCxEdit.Caption := aBoldLabel.Caption;
  vCxEdit.Properties.Alignment.Horz := aBoldLabel.Alignment;
  case aBoldLabel.Layout of
    tlTop:    vCxEdit.Properties.Alignment.Vert := taTopJustify;
    tlCenter: vCxEdit.Properties.Alignment.Vert := taVCenter;
    tlBottom: vCxEdit.Properties.Alignment.Vert := taBottomJustify;
  end;

  vCxEdit.Transparent := True;
  result := true;
end;

function TBoldToCxConverter.ConvertTBoldMemo(aBoldMemo: TBoldMemo): Boolean;
var
  vCxEdit : TcxBoldMemo;
begin
  result := false;
  if not TryRename(aBoldMemo) then Exit;
  vCxEdit := TcxBoldMemo.Create(aBoldMemo.Owner);
  CopyPublishedProperties(aBoldMemo, vCxEdit);
  vCxEdit.DataBinding.BoldHandle := aBoldMemo.BoldHandle;
  vCxEdit.DataBinding.BoldProperties.Expression := aBoldMemo.BoldProperties.Expression;
  CopyFollowerProperties(aBoldMemo.BoldProperties,vCxEdit.DataBinding.BoldProperties);

  vCXEdit.Style.BorderStyle := ebsFlat;
  vCXEdit.Style.Color := aBoldMemo.Color;
  vCxEdit.DragCursor := aBoldMemo.DragCursor;
  vCxEdit.DragKind := aBoldMemo.DragKind;
  vCxEdit.DragMode := aBoldMemo.DragMode;
  vCxEdit.Style.Font := aBoldMemo.Font;
  vCxEdit.ParentColor := aBoldMemo.ParentColor;  //??
  vCxEdit.ParentFont := aBoldMemo.ParentFont;    //??
  vCxEdit.PopupMenu := aBoldMemo.PopupMenu;
  if aBoldMemo.ReadOnly then
    vCxEdit.Properties.ReadOnly := aBoldMemo.ReadOnly;
  vCxEdit.TabOrder := aBoldMemo.TabOrder;
  vCxEdit.TabStop := aBoldMemo.TabStop;

  vCxEdit.Lines := aBoldMemo.Lines;
  vCxEdit.Properties.ScrollBars := aBoldMemo.ScrollBars;
  vCxEdit.Properties.WantReturns := aBoldMemo.WantReturns;
  vCxEdit.Properties.WantTabs := aBoldMemo.WantTabs;
  vCxEdit.Properties.WordWrap := aBoldMemo.WordWrap;
  result := true;
end;

procedure TBoldToCxConverter.CopyFollowerProperties(FromFollower : TBoldStringFollowerController; ToFollower : TBoldVariantFollowerController);
begin
  with FromFollower do
  begin
    ToFollower.Expression := Expression;
    ToFollower.ApplyPolicy := ApplyPolicy;
    ToFollower.CleanOnEqual := CleanOnEqual;
    ToFollower.DragMode := DragMode;
    ToFollower.DropMode := DropMode;
    ToFollower.NilRepresentation := NilStringRepresentation;
    ToFollower.Representation := Representation;
    ToFollower.Variables := Variables;
    //Renderer  One is variant and the other stringrenderer
  end;
end;

function TBoldToCxConverter.ConvertTBoldComboBox(aBoldComboBox : TBoldComboBox): Boolean;
var
  vCxBoldComboBox : TcxBoldComboBox;
begin
  result := false;
  if not TryRename(aBoldComboBox) then Exit;
  vCxBoldComboBox := TcxBoldComboBox.Create(aBoldComboBox.Owner);
  CopyPublishedProperties	(aBoldComboBox, vCxBoldComboBox);
  CopyFollowerProperties(aBoldComboBox.BoldProperties,vCxBoldComboBox.DataBinding.BoldProperties);
  CopyBoldComboListControllerProperties(aBoldComboBox.BoldListProperties, vCxBoldComboBox.Properties.BoldLookupListProperties);

  vCxBoldComboBox.DataBinding.BoldHandle := aBoldComboBox.BoldHandle;
  CopyFollowerProperties(aBoldComboBox.BoldRowProperties, vCxBoldComboBox.Properties.BoldRowProperties);
  vCxBoldComboBox.Properties.BoldLookupListHandle := aBoldComboBox.BoldListHandle;

  vCxBoldComboBox.Properties.Alignment.Horz := aBoldComboBox.Alignment;
  vCxBoldComboBox.AlignWithMargins := ABoldComboBox.AlignWithMargins;
  //vCxBoldComboBox.Properties.AutoSelect := aBoldComboBox.AutoSelect;
  //vCxBoldComboBox.Autosize := aBoldComboBox.Autosize;
  vCXBoldComboBox.Style.BorderStyle := ebsFlat; //ABoldComboBox.BorderStyle;
  vCxBoldComboBox.Properties.CharCase := aBoldComboBox.CharCase;
  vCXBoldComboBox.Style.Color := aBoldComboBox.Color;
  vCxBoldComboBox.DragCursor := aBoldComboBox.DragCursor;
  vCxBoldComboBox.DragKind := aBoldComboBox.DragKind;
  vCxBoldComboBox.DragMode := aBoldComboBox.DragMode;
  vCxBoldComboBox.Style.Font := aBoldComboBox.Font;
  vCxBoldComboBox.ParentColor := aBoldComboBox.ParentColor;
  //  vCxBoldComboBox.ParentCtl3D := aBoldComboBox.ParentCtl3D;
  //MaxLength
  vCxBoldComboBox.ParentFont := aBoldComboBox.ParentFont;
  vCxBoldComboBox.ParentShowHint := aBoldComboBox.ParentShowHint;
  //vCxBoldComboBox.Properties.PasswordChar := aBoldComboBox.PasswordChar;

  vCxBoldComboBox.PopupMenu := aBoldComboBox.PopupMenu;
  if aBoldComboBox.ReadOnly then
    vCxBoldComboBox.Properties.ReadOnly := aBoldComboBox.ReadOnly;
  vCxBoldComboBox.TabOrder := aBoldComboBox.TabOrder;
  vCxBoldComboBox.TabStop := aBoldComboBox.TabStop;
  vCxBoldComboBox.Properties.BoldSelectChangeAction := aBoldComboBox.BoldSelectChangeAction;
  vCxBoldComboBox.Properties.BoldSetValueExpression := aBoldComboBox.BoldSetValueExpression;

  vCXBoldCombobox.Properties.OnChange := aBoldComboBox.OnChange;
  vCXBoldCombobox.OnClick := aBoldComboBox.OnClick;
  vCXBoldCombobox.OnContextPopup := aBoldComboBox.OnContextPopup;
  vCXBoldCombobox.OnDblClick := aBoldComboBox.OnDblClick;
  vCXBoldCombobox.OnDragDrop := aBoldComboBox.OnDragDrop;
  vCXBoldCombobox.OnDragOver := aBoldComboBox.OnDragOver;
  //vCXBoldCombobox.Properties.OnDrawItem := aBoldComboBox.OnDrawItem;
  //vCXBoldCombobox.OnDropDown := ????
  vCXBoldCombobox.OnEndDock := aBoldComboBox.OnEndDock;
  vCXBoldCombobox.OnEndDrag := aBoldComboBox.OnEndDrag;
  vCXBoldCombobox.OnEnter := aBoldComboBox.OnEnter;
  vCXBoldCombobox.OnExit := aBoldComboBox.OnExit;
  vCXBoldCombobox.OnKeyDown := aBoldComboBox.OnKeyDown;
  vCXBoldCombobox.OnKeyPress := aBoldComboBox.OnKeyPress;
  vCXBoldCombobox.OnKeyUp := aBoldComboBox.OnKeyUp;
  //vCXBoldCombobox.Properties.OnMeasureItem := aBoldComboBox.OnMeasureItem;
  //vCXBoldCombobox.Properties.OnSelectChanged := aBoldComboBox.OnSelectChanged;
  vCXBoldCombobox.OnStartDock := aBoldComboBox.OnStartDock;
  vCXBoldCombobox.OnStartDrag := aBoldComboBox.OnStartDrag;
  vCXBoldCombobox.PopupMenu := aBoldComboBox.PopupMenu;

  result := true;
end;

end.
