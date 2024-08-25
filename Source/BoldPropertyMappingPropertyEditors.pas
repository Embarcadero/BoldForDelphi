{ Global compiler directives }
{$include bold.inc}
unit BoldPropertyMappingPropertyEditors;

interface

uses
  Classes,
  DesignEditors,
  DesignIntf,
  BoldAbstractPropertyEditors;

type


  TVCLPropertyProperty = class(TBoldStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;


  TVCLComponentProperty = class(TBoldComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TBoldPropertyMapperComponentEditor = class(TDefaultEditor)
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
  TypInfo,
  BoldControlsDefs,
  BoldPropertyMapper;

{-- TVCLPropertyProperty -----------------------------------------------------}

function TVCLPropertyProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable];
end;

procedure TVCLPropertyProperty.GetValues(Proc: TGetStrProc);
var
  SelectedComponent: TBoldPropertyMapping;
  ProcessedList: TList;

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
        NewPath := String(PropList[I]^.Name)
      else
        NewPath := Path + '.' + String(PropList[I]^.Name);

      if (PropList[I]^.PropType^.Kind <> tkClass) then
      begin
        Proc(NewPath);
        NewObj := LastObject;
      end
      else
      begin
        if Assigned(LastObject) then
          NewObj := TObject(GetOrdProp(LastObject, String(PropList[I]^.Name)));

        if NewObj is TCollection then
          for J := 0 to TCollection(NewObj).Count - 1 do
          begin
            SubPath := Format('%s[%d]',[NewPath,J]);
            Proc(SubPath);
            DeclareProperties(TCollection(NewObj).Items[J], TCollection(NewObj).Items[J].ClassInfo,SubPath)
          end
        else
          if ProcessedList.indexOf(NewObj) = -1 then
          begin
            ProcessedList.Add(NewObj);
            DeclareProperties(NewObj, PropList[I]^.PropType^,NewPath);
          end;
      end;
    end;
  end;

begin
  if PropCount < 1 then exit;
  ProcessedList := TList.Create;
  try
    SelectedComponent := GetComponent(0) as TBoldPropertyMapping;
    if Assigned(SelectedComponent) and Assigned(SelectedComponent.VCLComponent) then
      DeclareProperties(SelectedComponent.VCLComponent,SelectedComponent.VCLComponent.ClassInfo,'');
  finally
    ProcessedList.free;
  end;
end;

{-- TVCLComponentProperty -----------------------------------------------------}

procedure TVCLComponentProperty.GetValues(Proc: TGetStrProc);
begin
  Proc(TBoldPropertyMapping(GetComponent(0)).PropertyMapper.Owner.Name);
  inherited;
end;

procedure TVCLComponentProperty.SetValue(const Value: string);
var
  Component: TComponent;
begin
  if Value = TBoldPropertyMapping(GetComponent(0)).PropertyMapper.Owner.Name then
  begin
    Component := TBoldPropertyMapping(GetComponent(0)).PropertyMapper.Owner;
    SetOrdValue(Longint(Component));
  end
  else
    inherited;
end;

{ TBoldPropertyMapperComponentEditor }

procedure TBoldPropertyMapperComponentEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
begin
  if PropertyEditor.GetName = 'MappingCollection' then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TBoldPropertyMapperComponentEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0: Edit;
  end;
end;

function TBoldPropertyMapperComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit driven properties...';
  end;
end;

function TBoldPropertyMapperComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
