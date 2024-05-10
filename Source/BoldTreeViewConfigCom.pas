
{ Global compiler directives }
{$include bold.inc}
unit BoldTreeViewConfigCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Classes,
  BoldGenericListControlPackCom,
  BoldNodeControlPackCom,
  {!! DO NOT REMOVE !! BoldSystemRT ,}
  {$IFDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  {$ENDIF}
  BoldTreeViewCom;

procedure BoldGenericTreeView(SystemTypeInfo: IBoldSystemTypeInfo; TreeView: TBoldTreeViewCom);

implementation

uses
  SysUtils,
  BoldUtils;

{$IFDEF BOLDCOMCLIENT}
procedure BoldGenericTreeView(SystemTypeInfo: IBoldSystemTypeInfo; TreeView: TBoldTreeViewCom);
begin
end;
{$ELSE}

procedure FillListpartForAttr(RTAttr: TBoldAttributeRTInfo;
  Listpart: TBoldGenericListPartCom);
var
  s: string;
begin
  s := format('''%s: '' + %s.asString', [RTAttr.ExpressionName, RTAttr.ExpressionName]);
  s[2] := UpCase(s[2]);
  ListPart.ElementExpression := s;
  ListPart.InterpretAsList := False;
  ListPart.ControllerExpression := '''AttributeNode''';
end;


procedure FillListpartForRole(RTRole: TBoldRoleRTInfo;
  Listpart: TBoldGenericListPartCom;
  DefiningClass: IBoldClassTypeInfo;
  TreeView: TBoldTreeViewCom);
var
  RoleName: String;
  s: string;
begin
  ListPart.ElementExpression := RTRole.ExpressionName;
  RoleName := Format('%s.%s', [DefiningClass.ExpressionName, RTRole.ExpressionName]);
  ListPart.ControllerExpression := format('''%s''', [RoleName]);
  ListPart.InterpretAsList := False;
  if DefiningClass = RTRole.ClassTypeInfo then
    with TreeView.BoldProperties.NodeDescriptions.Add do begin
      Name := RoleName;
      s := format('''%s:''', [RTRole.ExpressionName]);
      s[2] := UpCase(s[2]);
      TextController.Expression := s;
      with ListController.Parts.Add do begin
        ControllerExpression := 'oclType';
        InterpretAsList := RTRole.IsMultiRole;
      end;
    end;
end;

procedure FillNodeDescriptorForClass(ClassTypeInfo: IBoldClassTypeInfo;
  NodeDescriptor: TBoldNodeDescriptionCom; TreeView: TBoldTreeViewCom);
var
  i: integer;
  DefiningClass: IBoldClassTypeInfo;
  Role: TBoldRoleRTInfo;
begin
  NodeDescriptor.Name := ClassTypeInfo.ExpressionName;
  NodeDescriptor.TextController.Expression := 'self.asString + '': '' + oclType.asString';
  for i := 0 to ClassTypeInfo.AllMembers.Count-1 do begin
    DefiningClass := ClassTypeInfo;
    while i < DefiningClass.FirstOwnMemberIndex do
      DefiningClass := DefiningClass.SuperClassTypeInfo;

    if ClassTypeInfo.AllMembers[i].IsRole then
    begin
      Role := ClassTypeInfo.AllMembers[i] as TBoldRoleRTInfo;
      if Role.IsNavigable then
        FillListPartForRole(
          Role,
          NodeDescriptor.ListController.Parts.Add,
          DefiningClass,
          TreeView);
    end else begin
      FillListPartForAttr(
        ClassTypeInfo.AllMembers[i] as TBoldAttributeRTInfo,
        NodeDescriptor.ListController.Parts.Add);
    end;
  end;
end;

procedure BoldGenericTreeView(SystemTypeInfo: IBoldSystemTypeInfo; TreeView: TBoldTreeViewCom);
var
  i: integer;
begin
  With TreeView.BoldProperties.NodeDescriptions.Add do begin
    Name := 'AttributeNode';
  end;

  With TreeView.BoldProperties.parts.add do begin
    InterpretAsList := true;
  end;

  for i := 0 to SystemTypeInfo.TopSortedClasses.count-1 do begin
    FillNodeDescriptorForClass(
      SystemTypeInfo.TopSortedClasses[i],
      TreeView.BoldProperties.NodeDescriptions.Add,
      TreeView);
  end;
end;

{$ENDIF}

end.
