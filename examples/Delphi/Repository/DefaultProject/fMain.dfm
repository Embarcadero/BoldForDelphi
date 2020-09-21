object frmMain: TfrmMain
  Left = 191
  Top = 107
  Width = 472
  Height = 380
  Caption = 'frmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 7
    Top = 8
    Width = 449
    Height = 321
    Lines.Strings = (
      'Short instructions:'
      ''
      
        'This template project contains a brief template to get started b' +
        'uilding a Bold Application.'
      
        'It will use InterbaseExpress to connect to a database, and provi' +
        'des a mainform (this form) on '
      'which you can start to build a gui.'
      ''
      
        'You must provide a UML class model to power the object oriented ' +
        'framework of Bold. This '
      
        'model could be designed in ModelMaker, Rational Rose or some XMI' +
        ' compatible modelling '
      'tool.'
      ''
      
        '* Use either the provided BoldUMLRoseLink (on the dmMain datamod' +
        'ule), or add another '
      'suitable model link.'
      
        '* Open the Bold Model Editor (doubleclick the TBoldModel compone' +
        'nt on the dmMain '
      'Datamodule), import your model.'
      '* Generate code (from the Tools menu in the model editor).'
      ''
      
        'Now you should be able to compile and run the application. Press' +
        ' the "Create DB" button to '
      'initialize the database and its schema.'
      
        'Until you have designed a fancy gui for your application you can' +
        ' use the system debugger '
      
        '(look in the debug menu of this form) to inspect your classes (d' +
        'oubleclick a class to see all '
      'instances) and create new/modify objects.'
      ''
      
        'For more instructions on how to write Bold Applications, please ' +
        'read the tutorial and the online '
      'help.')
    TabOrder = 0
  end
  object MainMenu1: TMainMenu
    Left = 4
    Top = 4
    object File1: TMenuItem
      Caption = 'File'
      object Updatedatabase1: TMenuItem
        Action = dmMain.BoldUpdateDBAction1
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Debug1: TMenuItem
      Caption = 'Debug'
      object SystemDebugger1: TMenuItem
        Caption = 'SystemDebugger'
        OnClick = SystemDebugger1Click
      end
    end
    object N1: TMenuItem
      Caption = '?'
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
end
