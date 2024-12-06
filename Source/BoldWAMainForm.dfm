object MainForm: TMainForm
  Left = 266
  Top = 260
  HelpContext = 5
  BorderStyle = bsDialog
  Caption = 'Bold Wizard'
  ClientHeight = 303
  ClientWidth = 493
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000000020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF001111
    11111111111111111111111111111111C888FFFFFFFF88888FFFFFFFFF811111
    C888FFFFFFFF88888FFFFFFFFF811111C888FFFFFFFF88888FFFFFFFFF811111
    C8888888888888888888888888811111C8000000888888800008888888811111
    C8888888888888888888888888811111C8888888888888888888888888811111
    C8888888888888888888888888811111C88888FFFFFF8888888FFFFFFF811111
    C80008FFFFFF8880088FFFFFFF811111C8888888888888888888888888811111
    C8888888888888888888888888811111C88888FFFFFF8888888FFFFFFF811111
    C80008FFFFFF8880008FFFFFFF811111C88888888888888888888888888111FF
    C88888888888888888888888888111FFCCCCCCCCCCCCCCCCCCCCCCCCCCC111FF
    C88448888888888888444444444111FFC88444444444444444444444444111FF
    CCCCCCCCCCCCCCCCCCCCCCCCCCC111FF111FF1FF111FF1FF1FF1111FF11111FF
    FFFF11FF111FF1FF1FFF111FF11111FFFFF111FFF1FFF1FF11FF11FFF11111FF
    11FF111FFFFF11FF11FFFFFFF11111FF111FF111FFF111FF111FFF1FF11111FF
    111FF111111111FF1111111FF11111FF11FF1111111111FF1111111FF11111FF
    FFFF1111111111FF1111111FF11111FFFF111111111111FF1111111FF1111111
    1111111111111111111111111111111111111111111111111111111111110000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pnBottom: TPanel
    Left = 0
    Top = 254
    Width = 493
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnCancel: TButton
      Left = 408
      Top = 16
      Width = 81
      Height = 25
      Caption = 'Close'
      TabOrder = 3
      OnClick = btnCancelClick
    end
    object Button2: TButton
      Left = 240
      Top = 16
      Width = 81
      Height = 25
      Caption = '< &Back'
      Enabled = False
      TabOrder = 1
    end
    object BitBtn1: TBitBtn
      Left = 152
      Top = 16
      Width = 81
      Height = 25
      HelpContext = 5
      Caption = '&Help'
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnNext: TButton
      Left = 320
      Top = 16
      Width = 81
      Height = 25
      Caption = '&Next  >'
      Default = True
      TabOrder = 2
      OnClick = btnNextClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 493
    Height = 254
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Image1: TImage
      Left = 23
      Top = 8
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        055449636F6E0000010001002020100000000000E80200001600000028000000
        2000000040000000010004000000000000020000000000000000000000000000
        0000000000000000000080000080000000808000800000008000800080800000
        80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
        FFFFFF00111111111111111111111111111111111111C888FFFFFFFF88888FFF
        FFFFFF811111C888FFFFFFFF88888FFFFFFFFF811111C888FFFFFFFF88888FFF
        FFFFFF811111C8888888888888888888888888811111C8000000888888800008
        888888811111C8888888888888888888888888811111C8888888888888888888
        888888811111C8888888888888888888888888811111C88888FFFFFF8888888F
        FFFFFF811111C80008FFFFFF8880088FFFFFFF811111C8888888888888888888
        888888811111C8888888888888888888888888811111C88888FFFFFF8888888F
        FFFFFF811111C80008FFFFFF8880008FFFFFFF811111C8888888888888888888
        8888888111FFC88888888888888888888888888111FFCCCCCCCCCCCCCCCCCCCC
        CCCCCCC111FFC88448888888888888444444444111FFC8844444444444444444
        4444444111FFCCCCCCCCCCCCCCCCCCCCCCCCCCC111FF111FF1FF111FF1FF1FF1
        111FF11111FFFFFF11FF111FF1FF1FFF111FF11111FFFFF111FFF1FFF1FF11FF
        11FFF11111FF11FF111FFFFF11FF11FFFFFFF11111FF111FF111FFF111FF111F
        FF1FF11111FF111FF111111111FF1111111FF11111FF11FF1111111111FF1111
        111FF11111FFFFFF1111111111FF1111111FF11111FFFF111111111111FF1111
        111FF11111111111111111111111111111111111111111111111111111111111
        1111111100000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000}
    end
    object Bevel1: TBevel
      Left = 68
      Top = 37
      Width = 383
      Height = 17
      Shape = bsTopLine
    end
    object Label1: TLabel
      Left = 72
      Top = 13
      Width = 121
      Height = 13
      Caption = 'Bold Attribute Wizard'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 74
      Top = 81
      Width = 393
      Height = 13
      Caption = 
        '* Create a Bold ordinal type by subclassing  TBAValueSet and def' +
        'iniing new values.'
    end
    object Label3: TLabel
      Left = 74
      Top = 102
      Width = 382
      Height = 13
      Caption = 
        '* Subclass one of the registered Bold attributes (e.g TBAString,' +
        ' TBAInteger, etc.).'
    end
    object Label4: TLabel
      Left = 75
      Top = 122
      Width = 402
      Height = 26
      Caption = 
        '* Derive your custom attribute, this lets you define new propert' +
        'ies and methods. Code for the Interface and the Persistence Mapp' +
        'er are automatically generated.'
      WordWrap = True
    end
    object GroupBox1: TGroupBox
      Left = 98
      Top = 166
      Width = 297
      Height = 87
      Caption = 'Select attribute type'
      TabOrder = 0
      object rbValueSet: TRadioButton
        Left = 8
        Top = 20
        Width = 265
        Height = 17
        Caption = 'Derive attribute from TBAValueSet.'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbSubClass: TRadioButton
        Left = 8
        Top = 41
        Width = 265
        Height = 17
        Caption = 'Derive attribute from a registered Bold attribute class.'
        TabOrder = 1
      end
      object rbCustom: TRadioButton
        Left = 8
        Top = 62
        Width = 217
        Height = 17
        Caption = 'Create a custom attribute.'
        TabOrder = 2
      end
    end
    object StaticText1: TStaticText
      Left = 64
      Top = 48
      Width = 403
      Height = 25
      AutoSize = False
      Caption = 
        'This Wizard will assist you in the creation of new Bold attribut' +
        'es. It features 3 methods for creating your new attributes: '
      TabOrder = 1
    end
  end
end
