unit MultiUnitCodeGenPatch;

// This unit fixes a problem with generating code for implicit link classes.
// If the two association ends belong to a different unit than the
// default unit, the link class will still belong to the default unit
// causing bad typings of the associations.

// to generate code in runtime:
// uses
//   BoldGen,
//   BoldgeneratorTemplatesDelphi,
//   BoldgeneratorTemplates,
//   BoldFileHandler;
//
// var
//   Generator: TBoldGenerator;
// begin
//   BoldGeneratorTemplateManagerClass := TBoldDelphiTemplateManager;
//   Generator := TBoldGenerator.Create(BoldModel1.TypeNameDictionary);
//   Generator.BaseFilePath := '';
//   Generator.UseTypedLists := true;
//   Generator.MoldModel := BoldModel1.MoldModel;
//   Generator.PutInterfaceFile;
//   Generator.EnsureMethodImplementations;
//   generator.Free;
//   BoldCloseAllFilehandlers;
// end;


interface
uses
  BoldGen,
  BoldMeta;

type
  TMultiUnitCodeGenPatcher = class(TBoldCodeGenInitializer)
  protected
    procedure MoveImplicitLinkClasses;
  public
    procedure Execute; override;
  end;

implementation

{ TMultiUnitCodeGenPatcher }

procedure TMultiUnitCodeGenPatcher.Execute;
begin
  inherited;
  MoveImplicitLinkClasses;
end;

procedure TMultiUnitCodeGenPatcher.MoveImplicitLinkClasses;
  function GetAssociationEndComponent(Association: TMoldAssociation; Index: integer): TMoldComponent;
  begin
    if assigned(Association.Roles[index]) and
       assigned(Association.Roles[index].MoldClass) and
       assigned(Association.Roles[index].MoldClass.component) then
       result := Association.Roles[index].MoldClass.Component
    else
      result := nil;
  end;
var
  i: integer;
  LinkClass: TMoldClass;
  Component1, Component2: TMoldComponent;
begin
  for i := 0 to MoldModel.Classes.Count-1 do
  begin
    if assigned(MoldModel.Classes[i].Association) then
    begin
      LinkClass := MoldModel.Classes[i];
      // check if the link class is in the default component
      if LinkClass.Component = MoldModel.MainComponent then
      begin
        // if the linkclass inherits from a class in another component, then move it there.
        if (LinkClass.SuperClass.Component <> MoldModel.MainComponent) then
          LinkClass.Component := LinkClass.SuperClass.Component;

        Component1 := GetAssociationEndComponent(LinkClass.Association, 0);
        Component2 := GetAssociationEndComponent(LinkClass.Association, 1);
        // if the two ends belong to the same component, and it differs
        // from the link class, reassign the link class. if there is
        // already a dependency between the new and the old component
        if assigned(Component1) and (Component1 <> LinkClass.Component) and
          (Component1 = Component2) and
          Component1.DependentOf(LinkClass.Component) then
            LinkClass.Component := Component1;
      end;
    end;
  end;
end;

initialization
  BoldCodeGenInitializerClass := TMultiUnitCodeGenPatcher;
end.
