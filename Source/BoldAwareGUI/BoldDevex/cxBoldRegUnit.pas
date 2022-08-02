unit cxBoldRegUnit;

{$I cxVer.inc}

//  v2.03 - 25 Jan 2011  2007-2011 Daniel Mauric

{$ASSERTIONS ON}

interface

procedure Register;

implementation

uses
  Classes, SysUtils,{$IFDEF DELPHI6} DesignIntf, DesignEditors,{$ELSE} DsgnIntf,{$ENDIF}
  cxEditRepositoryEditor,
  cxEditRepositoryItems,
//  cxEditConsts,
  cxEdit,
  cxBoldEditors,
  dxBarBoldNav,
  cxBoldEditConsts,
  cxBoldEditRepositoryItems,
  dxBar,
  BoldPropertyEditors,
  BoldElements,
  BoldDefs,
  BoldHandles,
//  BoldDataBindingUnit,
//  BoldDataBindingTestUnit,
  BoldAbstractPropertyEditors,

  Dialogs,
  cxDropDownEdit,
  cxGridBoldSupportUnit,

  cxGridCustomView,
  cxGridCustomTableView,

  cxBoldExtLookupComboBox,
  cxBoldLookupComboBox,
  cxBoldLookupEdit,
  BoldToCxConverterUnit;

type
  TcxCustomEditAccess = class(TcxCustomEdit);
  TcxCustomBoldLookupEditAccess = class(TcxCustomBoldLookupEdit);

type
  TBoldOCLExpressionForCxBoldPropertiesSetValueExpression = class(TBoldOCLExpressionProperty)
  protected
    function GetContextType(Component: TPersistent): TBoldElementTypeInfo; override;
  end;

type
  TdxBoldItemEditor = class(TdxAddSubItemEditor)
  protected
    class function GetAddedItemClass(const AAddedItemName: string): TdxBarItemClass; override;
    class function GetPopupItemCaption: string; override;
  end;

{ TcxCustomGridTableItemProperty }

type
  TcxCustomGridTableItemProperty = class(TComponentProperty)
  protected
    function GetGridView: TcxCustomGridView;
    procedure GetGridViewItemNames(AGridView: TcxCustomGridView; Proc: TGetStrProc); virtual;
    function InternalGetGridView(APersistent: TPersistent): TcxCustomGridView; virtual; abstract;
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TcxCustomGridTableItemProperty.GetValues(Proc: TGetStrProc);
var
  AGridView: TcxCustomGridView;
begin
  AGridView := GetGridView;
  if AGridView <> nil then
    GetGridViewItemNames(AGridView, Proc);
end;

function TcxCustomGridTableItemProperty.GetGridView: TcxCustomGridView;
var
  I: Integer;
begin
  Result := InternalGetGridView(GetComponent(0));
  for I := 1 to PropCount - 1 do
    if InternalGetGridView(GetComponent(I)) <> Result then
    begin
      Result := nil;
      Break;
    end;
end;

procedure TcxCustomGridTableItemProperty.GetGridViewItemNames(AGridView: TcxCustomGridView;
  Proc: TGetStrProc);
var
  I: Integer;
begin
  if AGridView is TcxCustomGridTableView then
    with AGridView as TcxCustomGridTableView do
      for I := 0 to ItemCount - 1 do
        Proc(Designer.GetComponentName(Items[I]));
end;

type
  TcxExtLookupComboBoxPropertiesItemColumnProperty = class(TcxCustomGridTableItemProperty)
  protected
    function InternalGetGridView(APersistent: TPersistent): TcxCustomGridView; override;
  end;

  TcxExtLookupComboBoxPropertiesViewProperty = class(TComponentProperty)
  private
    FProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

(*
  TBoldOCLExpressionForSetValueExpression = class(TBoldOCLExpressionProperty)
  protected
    function GetContextType(Component: TPersistent): TBoldElementTypeInfo; override;
  end;

{ TBoldOCLExpressionForSetValueExpression }


function TBoldOCLExpressionForSetValueExpression.GetContextType(
  Component: TPersistent): TBoldElementTypeInfo;
var
  lBoldElementHandle: TBoldElementHandle;
begin
  if Component is TcxBarBoldEditItem then
    lBoldElementHandle := (Component as TcxBarBoldEditItem).BoldHandle
  else
  if component is TcxCustomEdit and (TcxCustomEditAccess(component).DataBinding is TcxBoldEditDataBinding) then
    lBoldElementHandle := (TcxCustomEditAccess(component).DataBinding as TcxBoldEditDataBinding).BoldHandle
  else
  if component is TcxBoldComboBoxProperties then
  begin
    raise EBold.CreateFmt('Unsuported component %s.', [(component as TcxBoldComboBoxProperties).Owner.Classname]);
  end
  else
    raise EBold.CreateFmt('Unsuported component %s.', [Component.ClassName]);

  if Assigned(lBoldElementHandle) then
    Result := lBoldElementHandle.StaticBoldType
  else
    Result := nil
end;
*)
{ TdxBoldItemEditor }

class function TdxBoldItemEditor.GetAddedItemClass(
  const AAddedItemName: string): TdxBarItemClass;
begin
  Result := TcxBarBoldEditItem;
end;

class function TdxBoldItemEditor.GetPopupItemCaption: string;
begin
  Result := 'Add BoldItem';//dxSBAR_CP_ADDBUTTON;
end;


procedure RegisterEditRepositoryItems;
begin
  RegisterEditRepositoryItem(TcxEditRepositoryBoldStringItem, scxSBoldEditRepositoryTextItem);
  RegisterEditRepositoryItem(TcxEditRepositoryBoldComboBoxItem, scxSBoldComboBoxRepositoryTextItem);
  RegisterEditRepositoryItem(TcxEditRepositoryBoldLookupComboBoxItem, scxSBoldLookupComboBoxRepositoryTextItem);
  RegisterEditRepositoryItem(TcxEditRepositoryBoldExtLookupComboBoxItem, scxSBoldExtLookupComboBoxRepositoryTextItem);
end;

{type
  TBoldOCLExpressionForDataBindingDefinitionProperty = class(TBoldOCLExpressionProperty)
  protected
    function GetContextType(Component: TPersistent): TBoldElementTypeInfo; override;
    function GetVariableList(Component: TPersistent): TBoldExternalVariableList; override;
  end;}
  
procedure Register;
begin
{$IFDEF DELPHI9}
  ForceDemandLoadState(dlDisable);
{$ENDIF}
  RegisterNoIcon([TcxBarBoldEditItem]);

  RegisterComponents('Express BoldEditors', [TdxBarBoldNavigator, TcxBoldTextEdit, TcxBoldDateEdit,
    TcxBoldTimeEdit, TcxBoldMemo, TcxBoldCurrencyEdit, TcxBoldMaskEdit, TcxBoldCheckBox, TcxBoldComboBox,
    TcxBoldSpinEdit, TcxBoldButtonEdit, TcxBoldHyperLinkEdit, TcxBoldProgressBar,
  {$IFDEF DevExScheduler}
    TcxBoldDateNavigator,
  {$ENDIF}
    TcxBoldLabel, TcxBoldImage, TcxBoldRichEdit,
    TcxBoldListBox, TcxBoldCheckListBox, TcxBoldSelectionCheckListBox, TcxBoldListView,
    TcxBoldExtLookupComboBox, TcxBoldLookupComboBox, TcxBoldNBLookupComboBox, TcxBoldNBExtLookupComboBox,
    TBoldToCxConverter]);

  RegisterNoIcon([TdxBarBoldNavButton]);

  RegisterPropertyEditor(TypeInfo(TdxBarBoldNavigator), TdxBarBoldNavButton, 'BarBoldNavigator', nil);
//  RegisterPropertyEditor(TypeInfo(String), TcxBoldComboBoxProperties, 'BoldSetValueExpression', TBoldOCLExpressionForSetValueExpression);

  BarDesignController.RegisterBarControlEditor(TdxBoldItemEditor);
  RegisterEditRepositoryItems;

  RegisterPropertyEditor(TypeInfo(TBoldElementHandle), TPersistent, 'BoldHandle', TBoldComponentPropertyIndicateMissing);
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TcxBoldComboBoxProperties, 'BoldSetValueExpression', TBoldOCLExpressionForCxBoldPropertiesSetValueExpression);
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TcxCustomBoldLookupEditProperties, 'BoldSetValueExpression', TBoldOCLExpressionForCxBoldPropertiesSetValueExpression);
  RegisterPropertyEditor(TypeInfo(TcxCustomGridTableItem), TcxBoldExtLookupComboBoxProperties, 'ListFieldItem', TcxExtLookupComboBoxPropertiesItemColumnProperty);
  RegisterPropertyEditor(TypeInfo(TcxCustomGridTableView), TcxBoldExtLookupComboBoxProperties, 'View', TcxExtLookupComboBoxPropertiesViewProperty);

  {GUI Component editors}
  RegisterComponentEditor(TcxBoldTextEdit, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldDateEdit, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldTimeEdit, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldMemo, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldCurrencyEdit, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldMaskEdit, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldCheckBox, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldComboBox, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldSpinEdit, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldButtonEdit, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldHyperLinkEdit, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldProgressBar, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldLabel, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldImage, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldRichEdit, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldExtLookupComboBox, TBoldOCLComponentEditor);
  RegisterComponentEditor(TcxBoldLookupComboBox, TBoldOCLComponentEditor);
end;


{ TBoldOCLExpressionForDataBindingDefinitionProperty }
{
function TBoldOCLExpressionForDataBindingDefinitionProperty.GetContextType(
  Component: TPersistent): TBoldElementTypeInfo;
begin
  EnsureComponentType(Component, TBoldDataBindingDefinition);
  Result := (Component as TBoldDataBindingDefinition).ContextType;
end;

function TBoldOCLExpressionForDataBindingDefinitionProperty.GetVariableList(
  Component: TPersistent): TBoldExternalVariableList;
begin
  EnsureComponentType(Component, TBoldDataBindingDefinition);
  Result := (Component as TBoldDataBindingDefinition).VariableList;
end;
}

{ TBoldOCLExpressionForCxBoldPropertiesSetValueExpression }

function TBoldOCLExpressionForCxBoldPropertiesSetValueExpression.GetContextType(
  Component: TPersistent): TBoldElementTypeInfo;
var
  lcxBoldComboBoxProperties: TcxBoldComboBoxProperties;
  lBoldAwareViewItem: IBoldAwareViewItem;
  lcxBoldComboBox: TcxBoldComboBox;
  lcxBoldDataController: TcxBoldDataController;
  lcxCustomBoldLookupEditProperties: TcxCustomBoldLookupEditProperties;
  lcxBoldEditDataBinding: TcxBoldEditDataBinding;
begin
  result := nil;
  if Component is TcxBoldComboBoxProperties then
  begin
//    ShowMessage(lcxBoldComboBoxProperties.GetContainerClass.ClassName);
//    ShowMessage(lcxBoldComboBoxProperties.Owner.ClassName);
    lcxBoldComboBoxProperties := Component as TcxBoldComboBoxProperties;
    if (lcxBoldComboBoxProperties.Owner is TcxCustomGridTableItem) and lcxBoldComboBoxProperties.Owner.GetInterface(IBoldAwareViewItem, lBoldAwareViewItem) then
    begin
      lcxBoldDataController := (TcxCustomGridTableItem(lcxBoldComboBoxProperties.Owner).DataBinding.DataController as TcxBoldDataController);
      if Assigned(lcxBoldDataController.BoldHandle) then
        result := lcxBoldDataController.BoldHandle.StaticBoldType;
    end
    else
    if lcxBoldComboBoxProperties.Owner is TcxBoldComboBox then
    begin
      lcxBoldComboBox := lcxBoldComboBoxProperties.Owner as TcxBoldComboBox;
      if Assigned(lcxBoldComboBox.DataBinding.BoldHandle) then
        result := lcxBoldComboBox.DataBinding.BoldHandle.StaticBoldType;
    end
    else
    if lcxBoldComboBoxProperties.Owner is TcxEditRepositoryItem then
    begin
      raise Exception.Create('OCL Editor is not available, repository items do not have a context.');
    end
    else
      raise Exception.Create('Unknown context: ' + lcxBoldComboBoxProperties.Owner.ClassName );
{      if Assigned(lcxBoldComboBoxProperties.BoldLookupListHandle) then
        result := lcxBoldComboBoxProperties.BoldLookupListHandle.StaticBoldType;
}
  end
  else
  if component is TcxCustomBoldLookupEditProperties then
  begin
    lcxCustomBoldLookupEditProperties := component as TcxCustomBoldLookupEditProperties;
    if (lcxCustomBoldLookupEditProperties.Owner is TcxCustomBoldLookupEdit) and (TcxCustomBoldLookupEditAccess(lcxCustomBoldLookupEditProperties.Owner).DataBinding is TcxBoldEditDataBinding) then
    begin
      lcxBoldEditDataBinding := TcxBoldEditDataBinding(TcxCustomEditAccess(lcxCustomBoldLookupEditProperties.Owner).DataBinding);
      if Assigned(lcxBoldEditDataBinding.BoldHandle) then
        result := lcxBoldEditDataBinding.BoldHandle.StaticBoldType;
    end
    else
      raise Exception.Create('Unknown context: ' + lcxCustomBoldLookupEditProperties.Owner.ClassName );
  end
  else
    Assert(false);
//    raise EBold.CreateFmt(sComponentNotComboBox, [ClassName]);
end;

{ TcxExtLookupComboBoxPropertiesItemColumnProperty }

function TcxExtLookupComboBoxPropertiesItemColumnProperty.InternalGetGridView(
  APersistent: TPersistent): TcxCustomGridView;
var           
  AProperties: TcxBoldExtLookupComboBoxProperties;
begin
  AProperties := APersistent as TcxBoldExtLookupComboBoxProperties;
  Result := AProperties.View;
end;

{ TcxExtLookupComboBoxPropertiesViewProperty }

procedure TcxExtLookupComboBoxPropertiesViewProperty.CheckComponent(
  const Value: string);
var
  AView: TcxCustomGridTableView;
begin
  AView := TcxCustomGridTableView(Designer.GetComponent(Value));
  if (AView <> nil) and TcxBoldExtLookupComboBoxProperties.IsViewSupported(AView) then
    FProc(Value);
end;

procedure TcxExtLookupComboBoxPropertiesViewProperty.GetValues(
  Proc: TGetStrProc);
begin
  FProc := Proc;
  inherited GetValues(CheckComponent);
end;

end.
