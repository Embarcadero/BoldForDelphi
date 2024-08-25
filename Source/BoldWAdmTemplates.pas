
{ Global compiler directives }
{$include bold.inc}
unit BoldWAdmTemplates;

interface

uses
  Forms,
  BoldTemplateExpander,
  Classes;

type
  Tattrdatamodule = class(TDataModule)
    ValueSetTemplate: TBoldTemplateHolder;
    SubClassedAttrTemplate: TBoldTemplateHolder;
    MapperTemplate: TBoldTemplateHolder;
    AttributeTemplate: TBoldTemplateHolder;
    InterfaceTemplate: TBoldTemplateHolder;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  attrdatamodule: TAttrdatamodule;

implementation

{$R *.dfm}

end.
