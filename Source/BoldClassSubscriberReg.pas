unit BoldClassSubscriberReg;

interface

uses
  Classes,
  DesignEditors,
  DesignIntf,
  BoldAbstractPropertyEditors,
  BoldPropertyEditors;

type
  TBoldClassSubscriberComponentEditor = class(TBoldComponentDblClickEditor)
  protected
    function GetDefaultMethodName: string; override;
  public
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  SysUtils,
  BoldIdeConsts,
  BoldClassSubscriber,
  BoldElements,
  BoldSystemRT,
  Vcl.Controls;

type
  { TBoldTypeNameSelectorForExpressionSubscriber }
  TBoldTypeNameSelectorForClassSubscriber = class(TBoldTypeNameSelectorForHandles)
  protected
    function GetApprovedTypes: TBoldValueTypes; override;
    function GetContextType(Component: TPersistent): TBoldSystemTypeInfo; override;
  end;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_MISC,
                     [
                      TBoldClassSubscriber
                     ]);
end;

procedure RegisterEditors;
begin
   RegisterComponentEditor(TBoldClassSubscriber, TBoldClassSubscriberComponentEditor);
   RegisterPropertyEditor(TypeInfo(String), TBoldClassEventMapping, 'ClassTypeName', TBoldTypeNameSelectorForClassSubscriber);
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
  RegisterEditors;
end;

{ TBoldClassSubscriberComponentEditor }

procedure TBoldClassSubscriberComponentEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
begin
  if PropertyEditor.GetName = 'EventMapping' then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TBoldClassSubscriberComponentEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0: Edit;
  end;
end;

function TBoldClassSubscriberComponentEditor.GetDefaultMethodName: string;
begin
  result := 'EventMapping';
end;

function TBoldClassSubscriberComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit events...';
  end;
end;

function TBoldClassSubscriberComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TBoldTypeNameSelectorForClassSubscriber }

function TBoldTypeNameSelectorForClassSubscriber.GetApprovedTypes: TBoldValueTypes;
begin
  Result := [bvtClass];
end;

function TBoldTypeNameSelectorForClassSubscriber.GetContextType(Component: TPersistent): TBoldSystemTypeInfo;
begin
  Assert(Component is TBoldClassEventMapping, Component.ClassName);
  result := TBoldClassEventMapping(Component).StaticSystemTypeInfo;
  Assert(Assigned(result));
end;

end.
