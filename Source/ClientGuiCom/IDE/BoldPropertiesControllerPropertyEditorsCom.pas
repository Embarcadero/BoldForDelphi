unit BoldPropertiesControllerPropertyEditorsCom;

interface

uses
  Classes,
  DesignEditors,
  DesignIntf,
  BoldAbstractPropertyEditors;

type
  // A Property editor for the PropertyName property.
  // It lists all meaningful properties of "Component" including properties of "sub-components"
  // It has currently some trouble with collections and will not show panels[0] for example
  TPropertyNamePropertyCom = class(TBoldStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // A Property editor for VCLComponent property
  // It is based on the TComponentProperty which lists all components of the form
  // In addition to those it will also list the form so we can control its properties !
  TVCLComponentPropertyCom = class(TBoldComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TBoldPropertiesControllerComponentEditorCom = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  SysUtils,
  BoldRev,
  TypInfo,
  BoldControlsDefs,
  BoldPropertiesControllerCom;

{-- TPropertyNamePropertyCom -----------------------------------------------------}

function TPropertyNamePropertyCom.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

procedure TPropertyNamePropertyCom.GetValues(Proc: TGetStrProc);
var
  SelectedComponent: TBoldDrivenPropertyCom;

  procedure DeclareProperties(LastObject: TObject; TypeInfo: PTypeInfo; Path: String);
  var
    I, J, Count: Integer;
    PropList   : TPropList;
    NewPath,
    SubPath    : String;
    NewObj     : TObject;
  begin
    Count := GetPropList(TypeInfo, BoldPropertiesController_SupportedPropertyTypes, @PropList);
    NewObj := nil;
    for I := 0 to Count - 1 do
    begin
      if Path = '' then
        NewPath := PropList[I]^.Name
      else
        NewPath := Path + '.' + PropList[I]^.Name;

      if (PropList[I]^.PropType^.Kind <> tkClass) then
      begin
        Proc(NewPath);
        NewObj := LastObject;
      end
      else
      begin
        if Assigned(LastObject) then
          NewObj := TObject(GetOrdProp(LastObject, PropList[I]^.Name));

        if NewObj is TCollection then
          for J := 0 to TCollection(NewObj).Count - 1 do
          begin
            SubPath := Format('%s[%d]',[NewPath,J]);
            Proc(SubPath);
            DeclareProperties(TCollection(NewObj).Items[J], TCollection(NewObj).Items[J].ClassInfo,SubPath)
          end
        else
          DeclareProperties(NewObj, PropList[I]^.PropType^,NewPath);
      end;
    end;
  end;

begin
  if PropCount < 1 then exit;

  SelectedComponent := GetComponent(0) as TBoldDrivenPropertyCom; //we don't allow multiselect
  if Assigned(SelectedComponent) and Assigned(SelectedComponent.VCLComponent) then
    DeclareProperties(SelectedComponent.VCLComponent,SelectedComponent.VCLComponent.ClassInfo,'');
end;

{-- TVCLComponentPropertyCom -----------------------------------------------------}

procedure TVCLComponentPropertyCom.GetValues(Proc: TGetStrProc);
begin
  Proc(TBoldDrivenPropertyCom(GetComponent(0)).PropertiesController.Owner.Name); // Add the Form's name to available components
  inherited;
end;

procedure TVCLComponentPropertyCom.SetValue(const Value: string);
var
  Component: TComponent;
begin
  // Special case for the form
  if Value = TBoldDrivenPropertyCom(GetComponent(0)).PropertiesController.Owner.Name then
  begin
    Component := TBoldDrivenPropertyCom(GetComponent(0)).PropertiesController.Owner;
    SetOrdValue(Longint(Component));
  end
  else
    inherited;
end;

{ TBoldPropertiesControllerComponentEditorCom }

procedure TBoldPropertiesControllerComponentEditorCom.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
begin
  if PropertyEditor.GetName = 'DrivenProperties' then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TBoldPropertiesControllerComponentEditorCom.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0: Edit;
  end;
end;

function TBoldPropertiesControllerComponentEditorCom.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit driven properties...';
  end;
end;

function TBoldPropertiesControllerComponentEditorCom.GetVerbCount: Integer;
begin
  Result := 1;
end;

initialization
end.
