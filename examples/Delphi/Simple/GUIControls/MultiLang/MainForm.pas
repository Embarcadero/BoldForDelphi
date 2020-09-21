unit MainForm;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Grids,
  ActnList,
  ComCtrls,
  ExtCtrls,
  BoldElements,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldTypeNameHandle,
  BoldReferenceHandle,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldControlPack,
  BoldStringControlPack,
  BoldMLRenderers,
  BoldModel,
  BoldSubscription,
  BoldEdit,
  BoldComboBox,
  BoldNavigator,
  BoldGrid,
  BoldLabel,
  BoldHandleAction,
  BoldActions,
  BoldDBActions, BoldAbstractModel, BoldNavigatorDefs,
  BoldIBDatabaseAction, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldModel1: TBoldModel;
    PageControl1: TPageControl;
    tsMain: TTabSheet;
    tsValueSets: TTabSheet;
    blhAllLanguageObjects: TBoldListHandle;
    blhAllHouseKindObjects: TBoldListHandle;
    grdHouseKind: TBoldGrid;
    grdPersonCategory: TBoldGrid;
    blhAllPCategoryObjects: TBoldListHandle;
    BoldNavigator2: TBoldNavigator;
    BoldNavigator3: TBoldNavigator;
    Label2: TLabel;
    Label3: TLabel;
    blhAllPersons: TBoldListHandle;
    blhAllBuildings: TBoldListHandle;
    grdPersons: TBoldGrid;
    grdBuildings: TBoldGrid;
    BoldNavigator4: TBoldNavigator;
    BoldNavigator5: TBoldNavigator;
    MotherToungeAsPrimaryLang: TButton;
    Panel1: TPanel;
    UpdateDatabase: TButton;
    EnglishRenderer: TBoldAsMLStringRenderer;
    tsLanguage: TTabSheet;
    Label1: TLabel;
    grdLanguages: TBoldGrid;
    BoldNavigator1: TBoldNavigator;
    btnSetPrimaryLang: TButton;
    brhPrimaryLanguage: TBoldReferenceHandle;
    BoldTypeNameHandle1: TBoldTypeNameHandle;
    blhAllPCategories: TBoldListHandle;
    bcbPersonCategory: TBoldComboBox;
    blhAllLanguageAttr: TBoldListHandle;
    bcbMotherTounge: TBoldComboBox;
    bcbHouseType: TBoldComboBox;
    blhAllHouseKinds: TBoldListHandle;
    btnSetSecondaryLang: TButton;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    lblPersonCategory: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    BoldLabel1: TBoldLabel;
    brhSecondaryLanguage: TBoldReferenceHandle;
    BoldLabel2: TBoldLabel;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button1: TButton;
    Button2: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure btnSetPrimaryLangClick(Sender: TObject);
    procedure MotherToungeAsPrimaryLangClick(Sender: TObject);
    procedure UpdateDatabaseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnSetSecondaryLangClick(Sender: TObject);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
    procedure BoldActivateSystemAction1SystemClosed(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  BoldMLAttributes,
  MultiLangClasses;

{$R *.DFM}

procedure TForm1.btnSetPrimaryLangClick(Sender: TObject);
// sets the primary language to the selected language in the language list
begin
  BoldSetPrimaryLanguageByName(BoldSystemHandle1.System, (blhAllLanguageObjects.CurrentBoldObject as TLanguageClass).LanguageName );
end;

procedure TForm1.MotherToungeAsPrimaryLangClick(Sender: TObject);
// sets the language of the current person as the primary language
begin
  BoldSetPrimaryLanguageByName(BoldSystemHandle1.System, (blhAllPersons.CurrentBoldObject as TPerson).M_MotherTounge.AsString)
end;

procedure TForm1.UpdateDatabaseClick(Sender: TObject);
begin
  BoldSystemHandle1.UpdateDatabase;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if BoldSystemHandle1.Active then
    BoldSystemHandle1.UpdateDatabase;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BoldSystemHandle1.IsDefault := true;
  width := 648;
  height := 377;
end;

procedure TForm1.btnSetSecondaryLangClick(Sender: TObject);
// sets the secondary language to the selected language in the language list
begin
  BoldSetSecondaryLanguageByName(BoldSystemHandle1.System, (blhAllLanguageObjects.CurrentBoldObject as TLanguageClass).LanguageName)
end;

procedure TForm1.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  if BoldSystemHandle1.Active then
  begin
    // initialize language-related classes
    // First create a number of languages.
    // the language with number 0 will be the default language when the
    // application is started.
    if BoldSystemHandle1.System.ClassByExpressionName[BoldMLLanguageClassName].Count = 0 then
    begin
      with TLanguageClass.Create(nil) do
      begin
        LanguageName := 'English';
        LanguageNumber := 0;
      end;

      with TLanguageClass.Create(nil) do
      begin
        LanguageName := 'Swedish';
        LanguageNumber := 1;
      end;

      with TLanguageClass.Create(nil) do
      begin
        LanguageName := 'Norweigan';
        LanguageNumber := 2;
      end;
    end;

    // then fill in the multilingual valueset classes with some default values

    if BoldSystemHandle1.System.ClassByExpressionName['PersonCategoryClass'].Count = 0 then
    begin
      with TPersonCategoryClass.create(nil) do
      begin
        Description := 'Ruler of the Kingdom';
        M_Description.AsStringByLanguage['Swedish'].AsString := 'Härskare av kungadömet';
        ShortDescription := 'King';
        M_ShortDescription.AsStringByLanguage['Swedish'].AsString := 'Kung';
        IntValue := 0;
      end;

      with TPersonCategoryClass.create(nil) do
      begin
        Description := 'Chief Executive Officer';
        M_Description.AsStringByLanguage['Swedish'].AsString := 'Verkställande direktör';
        ShortDescription := 'CEO';
        M_ShortDescription.AsStringByLanguage['Swedish'].AsString := 'VD';
        IntValue := 1;
      end;

      with TPersonCategoryClass.create(nil) do
      begin
        Description := 'Worker';
        M_Description.AsStringByLanguage['Swedish'].AsString := 'Arbetare';
        ShortDescription := 'Slave';
        M_ShortDescription.AsStringByLanguage['Swedish'].AsString := 'Slav';
        IntValue := 2;
      end;
    end;

    if BoldSystemHandle1.System.ClassByExpressionName['HouseKindClass'].Count = 0 then
    begin
      with THouseKindClass.create(nil) do
      begin
        Description := 'Castle with towers';
        M_Description.AsStringByLanguage['Swedish'].AsString := 'Slott med tinnar';
        ShortDescription := 'Castle';
        M_ShortDescription.AsStringByLanguage['Swedish'].AsString := 'Slott';
        IntValue := 0;
      end;

      with THouseKindClass.create(nil) do
      begin
        Description := 'House of Parliament';
        M_Description.AsStringByLanguage['Swedish'].AsString := 'Riksdagshuset';
        ShortDescription := 'HP';
        M_ShortDescription.AsStringByLanguage['Swedish'].AsString := 'RH';
        IntValue := 1;
      end;
    end;

    // finally create some domain objects

    if BoldSystemHandle1.System.ClassByExpressionName['Person'].Count = 0 then
    begin
      TPerson.Create(nil);
      TPerson.Create(nil);
    end;

    if BoldSystemHandle1.System.ClassByExpressionName['Building'].Count = 0 then
    begin
      TBuilding.Create(nil);
      TBuilding.Create(nil);
    end;

    // set up handles to point to the primary and secondary language
    brhPrimaryLanguage.Value := BoldPrimaryLanguage(BoldSystemHandle1.System);
    brhSecondaryLanguage.Value := BoldSecondaryLanguage(BoldSystemHandle1.System);
  end;
  UpdateDatabase.Enabled := BoldSystemHandle1.Active;
end;

procedure TForm1.BoldActivateSystemAction1SystemClosed(Sender: TObject);
begin
  UpdateDatabase.Enabled := BoldSystemHandle1.Active;
end;

initialization
  // Tell Bold how we have modelled the language configuration
  BoldMLLanguageClassName := 'LanguageClass';
  BoldMLLanguageNameAttributeName := 'LanguageName';
  BoldMLLanguageNumberAttributeName := 'LanguageNumber';

end.        
