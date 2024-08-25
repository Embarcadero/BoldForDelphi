{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelLinkSupport;

interface

uses
  Dialogs,
  Forms,
  Classes,
  BoldUMLModel,
  BoldUMLModelSupport;

type
  {---Forward declaration of classes---}
  TBoldUMLModelLinkSupport = class;

  TBoldUMLModelLinkSupport = class(TBoldUMLSupport)
  public
    class procedure StringToConstraints(Element: TUMLModelElement; ConstraintString: string);
    class function ConstraintsToString(Element: TUMLModelElement): string;
    class function ConstraintsAsString(Element: TUMLModelElement): string;
    class procedure LeadingSlashToDerived(Element: TUMLModelElement);
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldSystem
  ;

const
  CONSTRAINT_NAME_TERMINATOR = ':';
  CONSTRAINT_EXPRESSION_TERMINATOR = BOLDCRLF;

{ TBoldUMLModelLinkSupport }

class function TBoldUMLModelLinkSupport.ConstraintsAsString(Element: TUMLModelElement): string;
var
  i: integer;
begin
  for i := 0 to Element.constraint.count-1 do
    with Element.constraint[i] do
    begin
      if name <> '' then
        Result := Result + name + CONSTRAINT_NAME_TERMINATOR + body + CONSTRAINT_EXPRESSION_TERMINATOR
      else
        Result := Result + body + CONSTRAINT_EXPRESSION_TERMINATOR;
    end;

end;

class function TBoldUMLModelLinkSupport.ConstraintsToString(
  Element: TUMLModelElement): string;
begin
  Result := ConstraintsAsString(Element);
  while Element.constraint.count >0 do
     Element.constraint[Element.constraint.count-1].Delete;
end;

class procedure TBoldUMLModelLinkSupport.LeadingSlashToDerived(
  Element: TUMLModelElement);
begin

  with Element do
    if (Length(name) > 0) and (name[1] = '/') then
    begin
      if Element is TUMLAttribute then
      begin
        name := Copy(name, 2, MaxInt);
        derived := True;
      end
    end;
 end;

class procedure TBoldUMLModelLinkSupport.StringToConstraints(Element: TUMLModelElement; ConstraintString: string);
var
  AllConstraints: TStringList;
  aConstraint: string;
  i,p: integer;
begin
  AllConstraints := TStringList.Create;
  try
    AllConstraints.text := ConstraintString;
    for i := 0 to AllConstraints.Count-1 do
    begin
      aConstraint := AllConstraints[i];
      p := Pos(CONSTRAINT_NAME_TERMINATOR, aConstraint);
      with TUMLConstraint.Create(Element.BoldSystem) do
      begin
        constrainedElement.Add(element);
        if Element is TUMLNamespace then
          namespace_ := TUMLNamespace(Element)
        else
          namespace_ := Element.namespace_;
        name := Trim(Copy(aConstraint, 1, p - 1));;
        body := Trim(Copy(aConstraint, p + 1, MaxInt));
      end;
    end;
  finally
    FreeAndNil(AllConstraints);
  end;
end;

end.
