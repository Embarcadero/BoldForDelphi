unit cxBoldEditRepositoryItems;

interface

//  v2.03 - 25 Jan 2011  2007-2011 Daniel Mauric

uses
  cxEdit,
  cxBoldEditors,
  cxBoldExtLookupComboBox,
  cxBoldLookupComboBox;

type
  TcxEditRepositoryBoldStringItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxBoldTextEditProperties;
    procedure SetProperties(Value: TcxBoldTextEditProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxBoldTextEditProperties read GetProperties write SetProperties;
  end;

  TcxEditRepositoryBoldComboBoxItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxBoldComboBoxProperties;
    procedure SetProperties(Value: TcxBoldComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxBoldComboBoxProperties read GetProperties write SetProperties;
  end;

  TcxEditRepositoryBoldLookupComboBoxItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxBoldLookupComboBoxProperties;
    procedure SetProperties(Value: TcxBoldLookupComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxBoldLookupComboBoxProperties read GetProperties write SetProperties;
  end;

  TcxEditRepositoryBoldExtLookupComboBoxItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxBoldExtLookupComboBoxProperties;
    procedure SetProperties(Value: TcxBoldExtLookupComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxBoldExtLookupComboBoxProperties read GetProperties write SetProperties;
  end;

implementation

uses
  Classes;
  
{ TcxEditRepositoryBoldStringItem }

class function TcxEditRepositoryBoldStringItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxBoldTextEditProperties;
end;

function TcxEditRepositoryBoldStringItem.GetProperties: TcxBoldTextEditProperties;
begin
  Result := inherited Properties as TcxBoldTextEditProperties;
end;

procedure TcxEditRepositoryBoldStringItem.SetProperties(
  Value: TcxBoldTextEditProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryBoldComboBoxItem }

class function TcxEditRepositoryBoldComboBoxItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxBoldComboBoxProperties;
end;

function TcxEditRepositoryBoldComboBoxItem.GetProperties: TcxBoldComboBoxProperties;
begin
  Result := inherited Properties as TcxBoldComboBoxProperties;
end;

procedure TcxEditRepositoryBoldComboBoxItem.SetProperties(
  Value: TcxBoldComboBoxProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryBoldLookupComboBoxItem }

class function TcxEditRepositoryBoldLookupComboBoxItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  result := TcxBoldLookupComboBoxProperties;
end;

function TcxEditRepositoryBoldLookupComboBoxItem.GetProperties: TcxBoldLookupComboBoxProperties;
begin
  Result := inherited Properties as TcxBoldLookupComboBoxProperties;
end;

procedure TcxEditRepositoryBoldLookupComboBoxItem.SetProperties(
  Value: TcxBoldLookupComboBoxProperties);
begin
  inherited Properties := Value;
end;

{ TcxEditRepositoryBoldExtLookupComboBoxItem }

class function TcxEditRepositoryBoldExtLookupComboBoxItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  result := TcxBoldExtLookupComboBoxProperties;
end;

function TcxEditRepositoryBoldExtLookupComboBoxItem.GetProperties: TcxBoldExtLookupComboBoxProperties;
begin
  Result := inherited Properties as TcxBoldExtLookupComboBoxProperties;
end;

procedure TcxEditRepositoryBoldExtLookupComboBoxItem.SetProperties(
  Value: TcxBoldExtLookupComboBoxProperties);
begin
  inherited Properties := Value;
end;

initialization
  RegisterClasses([TcxEditRepositoryBoldStringItem, TcxEditRepositoryBoldComboBoxItem, TcxEditRepositoryBoldLookupComboBoxItem, TcxEditRepositoryBoldExtLookupComboBoxItem]);

end.
