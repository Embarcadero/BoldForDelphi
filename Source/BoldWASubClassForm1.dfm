object SubClassForm1: TSubClassForm1
  Left = 332
  Top = 238
  HelpContext = 20
  Caption = 'SubClassForm1'
  ClientHeight = 265
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnDestroy = FormDestroy
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 449
    Height = 265
    ActivePage = tsOverride
    Align = alClient
    Style = tsButtons
    TabOrder = 0
    object tsClassDef: TTabSheet
      HelpContext = 21
      Caption = 'Defifnition'
      OnShow = tsClassDefShow
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 441
        Height = 234
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object GroupBox1: TGroupBox
          Left = 0
          Top = 0
          Width = 441
          Height = 234
          Align = alClient
          Caption = 'Class Definition'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object Label1: TLabel
            Left = 66
            Top = 43
            Width = 31
            Height = 13
            Caption = 'Parent'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object Label3: TLabel
            Left = 14
            Top = 107
            Width = 82
            Height = 13
            Caption = 'Expression Name'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object Label6: TLabel
            Left = 104
            Top = 128
            Width = 79
            Height = 13
            Caption = '( Ex: ShortString)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object Label4: TLabel
            Left = 7
            Top = 179
            Width = 89
            Height = 13
            Caption = 'Delphi Class Name'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object Label7: TLabel
            Left = 104
            Top = 200
            Width = 86
            Height = 13
            Caption = '( Ex: TShortString)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object Label5: TLabel
            Left = 246
            Top = 43
            Width = 50
            Height = 13
            Caption = 'Unit Name'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object Label2: TLabel
            Left = 236
            Top = 107
            Width = 60
            Height = 13
            Caption = 'Model Name'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            Visible = False
          end
          object cbParent: TComboBox
            Left = 104
            Top = 40
            Width = 121
            Height = 21
            Style = csDropDownList
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnChange = cbParentChange
          end
          object edExpressionname: TEdit
            Left = 104
            Top = 104
            Width = 121
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnChange = edExpressionnameChange
          end
          object edDelphiName: TEdit
            Left = 104
            Top = 176
            Width = 121
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnChange = edDelphiNameChange
          end
          object edUnitname: TEdit
            Left = 304
            Top = 40
            Width = 121
            Height = 21
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            OnChange = edUnitnameChange
          end
          object edModelName: TEdit
            Left = 304
            Top = 104
            Width = 121
            Height = 21
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 4
            Visible = False
          end
        end
      end
    end
    object tsOverride: TTabSheet
      HelpContext = 22
      Caption = 'Override'
      ImageIndex = 1
      OnShow = tsOverrideShow
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 441
        Height = 234
        Align = alClient
        Caption = 'Select methods to override'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object ListViewOverride: TListView
          Left = 2
          Top = 15
          Width = 437
          Height = 217
          Align = alClient
          Checkboxes = True
          Columns = <>
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          HideSelection = False
          HotTrack = True
          HotTrackStyles = [htHandPoint]
          RowSelect = True
          ParentFont = False
          TabOrder = 0
          ViewStyle = vsList
        end
        object MemoBoldClasses: TMemo
          Left = 48
          Top = 40
          Width = 209
          Height = 129
          Enabled = False
          Lines.Strings = (
            'interface'
            ''
            ''
            '  {---TBAString---}'
            '  TBAString = class(TBoldAttribute,IBoldStringContent)'
            '  protected'
            
              '    function GetStringRepresentation(Representation:TBoldReprese' +
              'ntation): string; override;'
            
              '    procedure SetStringRepresentation(Representation:TBoldRepres' +
              'entation; Value:string); override;'
            '    { IBoldStreamable }'
            '    procedure SetAsVariant(const Value:Variant); override;'
            '    function GetAsVariant:Variant; override;'
            '    procedure AssignValue(Source: IBoldValue ); override;'
            '  public'
            '    procedure Assign(Source:TBoldElement); override;'
            
              '    function CompareToAs(CompType:TBoldCompareType; BoldElement:' +
              ' TBoldElement):Integer; override;'
            
              '    function ValidateString(Value: string; Representation:TBoldR' +
              'epresentation):Boolean; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBANumeric---}'
            '  TBANumeric = class(TBoldAttribute)'
            '  protected'
            '    function GetAsFloat:Double; virtual; abstract;'
            '    procedure SetAsInteger(Value: integer); virtual; abstract;'
            '  public'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBAInteger---}'
            '  TBAInteger = class(TBANumeric, IBoldIntegerContent)'
            '  protected'
            '    function CheckRange(Value: integer):Boolean; virtual;'
            '    function GetAsInteger:integer; virtual;'
            '    function GetAsFloat:Double; override;'
            
              '    function GetStringRepresentation(Representation:TBoldReprese' +
              'ntation): string; override;'
            '    procedure SetAsInteger(Value: integer); override;'
            
              '    procedure SetStringRepresentation(Representation:TBoldRepres' +
              'entation; Value:string); override;'
            '    { IBoldStreamable }'
            '    function GetStreamName:string; override;'
            '    procedure SetAsVariant(const Value:Variant); override;'
            '    function GetAsVariant:Variant; override;'
            
              '    functionMaySetValue(NewValue:integer; Subscriber:TBoldSubscr' +
              'iber): Boolean; virtual;'
            '    procedure AssignValue(Source: IBoldValue ); override;'
            '  public'
            
              '    function ValidateString(Value:string; Representation:TBoldRe' +
              'presentation):Boolean; override;'
            
              '    function ValidateCharacter(C:AnsiChar; Representation:TBoldR' +
              'epresentation):Boolean; override;'
            '    procedure Assign(Source:TBoldElement); override;'
            
              '    function CompareToAs(CompType:TBoldCompareType; BoldElement:' +
              ' TBoldElement):Integer; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '    end;'
            ''
            '  {---TBASmallInt---}'
            '  TBASmallInt = class(TBAInteger)'
            '  public'
            '    function CheckRange(Value: integer):Boolean; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBAShortInt---}'
            '  TBAShortInt = class(TBASmallInt)'
            '  public'
            '    function CheckRange(Value: integer):Boolean; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBAWord---}'
            '  TBAWord = class(TBAInteger)'
            '  public'
            '    function CheckRange(Value: integer):Boolean; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBAByte---}'
            '  TBAByte = class(TBAWord)'
            '  public'
            '    function CheckRange(Value: integer): Boolean; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBAFloat---}'
            '  TBAFloat = class(TBANumeric,IBoldFloatContent)'
            '  protected'
            '    function GetAsFloat:Double; override;'
            
              '    function GetStringRepresentation(Representation:TBoldReprese' +
              'ntation): string; override;'
            '    procedure SetAsFloat(Value:Double); virtual;'
            '    procedure SetAsInteger(Value : integer ); override;'
            
              '    procedure SetStringRepresentation(Representation:TBoldRepres' +
              'entation; Value:string); override;'
            '    { IBoldStreamable }'
            '    function GetStreamName: string; override;'
            '    procedure SetAsVariant(const Value:Variant); override;'
            '    function GetAsVariant:Variant; override;'
            
              '    function MaySetValue(NewValue:Double; Subscriber:TBoldSubscr' +
              'iber): Boolean; virtual;'
            '    procedure AssignValue(Source: IBoldValue ); override;'
            '  public'
            
              '    function ValidateString(Value: string; Representation:TBoldR' +
              'epresentation):Boolean; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation:TBold' +
              'Representation):Boolean; override;'
            '    procedure Assign(Source:TBoldElement); override;'
            
              '    function CompareToAs(CompType:TBoldCompareType; BoldElement:' +
              ' TBoldElement):Integer; override;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBACurrency---}'
            '  TBACurrency = class(TBANumeric,IBoldCurrencyContent)'
            '  protected'
            '    function GetAsFloat:Double; override;'
            
              '    function GetStringRepresentation(Representation:TBoldReprese' +
              'ntation): string; override;'
            '    procedure SetAsFloat(Value:Double); virtual;'
            '    procedure SetAsInteger(Value : integer ); override;'
            
              '    procedure SetStringRepresentation(Representation:TBoldRepres' +
              'entation; Value:string); override;'
            '    { IBoldStreamable }'
            '    function GetStreamName:string; override;'
            '    procedure SetAsVariant(const Value:Variant); override;'
            '    function GetAsVariant:Variant; override;'
            
              '    function MaySetValue(NewValue: Currency; Subscriber:TBoldSub' +
              'scriber): Boolean; virtual;'
            '    procedure AssignValue(Source: IBoldValue ); override;'
            '  public'
            
              '    function ValidateString(Value: string; Representation:TBoldR' +
              'epresentation):Boolean; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation:TBold' +
              'Representation):Boolean; override;'
            '    procedure Assign(Source:TBoldElement); override;'
            
              '    function CompareToAs(CompType:TBoldCompareType; BoldElement:' +
              ' TBoldElement):Integer; override;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBABlob---}'
            '  TBABlob = class(TBoldAttribute,IBoldBlobContent)'
            '  protected'
            
              '    function GetStringRepresentation(Representation:TBoldReprese' +
              'ntation): string;override;'
            
              '    procedure SetStringRepresentation(Representation:TBoldRepres' +
              'entation; Value:string); override;'
            '    function GetStreamName:string; override;'
            
              '    function MaySetValue(NewValue:String; Subscriber:TBoldSubscr' +
              'iber): Boolean;virtual;'
            '    procedure AssignValue(Source: IBoldValue ); override;'
            '  public'
            '    procedure SetToNull;override;'
            '    procedure Assign(Source:TBoldElement); override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {-- TBATypedBlob --}'
            '  TBATypedBlob = class(TBABlob,IBoldTypedBlob)'
            '  protected'
            
              '    function GetStringRepresentation(Representation:TBoldReprese' +
              'ntation): string;override;'
            
              '    procedure SetStringRepresentation(Representation:TBoldRepres' +
              'entation; Value:string); override;'
            '    function GetStreamName:string; override;'
            '    procedure AssignValue(Source: IBoldValue ); override;'
            '  public'
            '    procedure SetToNull;override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            ''
            '  {---tbaMoment---}'
            '  TBAMoment ='
            'class(TBoldAttribute)'
            '  protected'
            '    function GetAsVariant:Variant; override;'
            '    procedure SetAsVariant(const Value:Variant); override;'
            
              '    function MaySetValue(NewValue:TDateTime; Subscriber:TBoldSub' +
              'scriber): Boolean;virtual;'
            '  public'
            '    procedure Assign(Source:TBoldElement); override;'
            
              '    function CompareToAs(CompType:TBoldCompareType;BoldElement: ' +
              'TBoldElement):Integer; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBADateTime---}'
            
              '  TBADateTime = class(TBAMoment,IBoldDateTimeContent,IBoldDateCo' +
              'ntent,IBoldTimeContent)'
            '  protected'
            
              '    function GetStringRepresentation(Representation:TBoldReprese' +
              'ntation): string;override;'
            
              '    procedure SetStringRepresentation(Representation:TBoldRepres' +
              'entation; Value:string); override;'
            '    { IBoldStreamable }'
            '    function GetStreamName:string; override;'
            '    procedure AssignValue(Source: IBoldValue ); override;'
            '  public'
            
              '    function ValidateString(Value: string; Representation: TBold' +
              'Representation):Boolean; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation):Boolean; override;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBADate---}'
            '  TBADate = class(TBAMoment,IBoldDateContent)'
            '  protected'
            
              '    procedure SetStringRepresentation(Representation:TBoldRepres' +
              'entation; Value:string); override;'
            
              '    function GetStringRepresentation(Representation:TBoldReprese' +
              'ntation): string; override;'
            '    { IBoldStreamable }'
            '    function GetStreamName:string; override;'
            '    procedure AssignValue(Source: IBoldValue ); override;'
            '  public'
            
              '    function ValidateString(Value: string; Representation:TBoldR' +
              'epresentation):Boolean; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation:TBold' +
              'Representation):Boolean; override;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBATime---}'
            '  TBATime = class(TBAMoment,IBoldTimeContent)'
            '  protected'
            
              '    procedure SetStringRepresentation(Representation:TBoldRepres' +
              'entation; Value:string); override;'
            
              '    function GetStringRepresentation(Representation:TBoldReprese' +
              'ntation): string; override;'
            '    { IBoldStreamable }'
            '    function GetStreamName:string; override;'
            '    procedure AssignValue(Source: IBoldValue ); override;'
            '  public'
            
              '    function ValidateString(Value: string; Representation:TBoldR' +
              'epresentation):Boolean; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation:TBold' +
              'Representation):Boolean; override;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {TBAValueSet}'
            
              '  TBAValueSet = class(TBoldAttribute,IBoldIntegerContent,IBoldSt' +
              'ringContent)'
            '  protected'
            
              '    procedure InitializeMember(OwningElement: TBoldDomainElement' +
              '; ElementTypeInfo:TBoldElementTypeInfo);override;'
            '    function GetValues:TBAValueSetValueList; virtual; abstract;'
            '    { IBoldStreamable }'
            
              '    function GetStringRepresentation(Representation:TBoldReprese' +
              'ntation): string; override;'
            
              '    procedure SetStringRepresentation(Representation:TBoldRepres' +
              'entation; Value:string); override;'
            '    function GetStreamName:string; override;'
            '    procedure SetAsVariant(const Value:Variant); override;'
            '    function GetAsVariant:Variant; override;'
            
              '    function MaySetValue(NewValue:TBAValueSetValue; Subscriber: ' +
              'TBoldSubscriber):Boolean; virtual;'
            '    procedure AssignValue(Source: IBoldValue ); override;'
            '  public'
            '    procedure Assign(Source:TBoldElement); override;'
            
              '    function CompareToAs(CompType:TBoldCompareType; BoldElement:' +
              ' TBoldElement):Integer; override;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBABoolean---}'
            '  TBABoolean = class(TBAValueSet,IBoldBooleanContent)'
            '  protected'
            '    function GetValues:TBAValueSetValueList; override;'
            '    procedure SetAsVariant(const Value:Variant); override;'
            '    function GetAsVariant:Variant; override;'
            '  public'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  {---TBoldAttribute---}'
            '  TBoldAttribute = class(TBoldMember,IBoldNullableValue)'
            '  private'
            '    procedure MakeDbCurrent(Resubscribe:Boolean); override;'
            '  protected'
            '    function GetAsVariant:Variant; virtual;'
            
              '    function GetElementTypeInfoForType:TBoldElementTypeInfo;over' +
              'ride;'
            '    procedure SetAsVariant(const Value:Variant); virtual;'
            '  public'
            
              '    function ObserverMayModifyAsString(Representation: TBoldRepr' +
              'esentation; observer: TBoldSubscriber): Boolean; virtual;'
            
              '    function ValidateCharacter(C: AnsiChar; Representation: TBol' +
              'dRepresentation): Boolean; virtual;'
            
              '    function CompareToAs(CompType:TBoldCompareType;BoldElement: ' +
              'TBoldElement):Integer; override;'
            
              '    procedure DefaultSubscribe(Subscriber:TBoldSubscriber; Reque' +
              'stedEvent:TBoldEvent); override;'
            '    procedure SetToNull; virtual;'
            '  end;'
            ''
            '')
          TabOrder = 1
          Visible = False
          WordWrap = False
        end
      end
    end
  end
end
