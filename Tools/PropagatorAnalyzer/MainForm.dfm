object Form1: TForm1
  Left = 186
  Top = 303
  Width = 1088
  Height = 750
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 385
    Height = 21
    TabOrder = 0
    Text = 'D:\bold\BfD\Source\Propagator\Enterprise\BoldPropagator.thread'
  end
  object Button1: TButton
    Left = 424
    Top = 8
    Width = 75
    Height = 21
    Caption = 'Analyze'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 504
    Top = 9
    Width = 569
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Min = 0
    Max = 100
    TabOrder = 2
    Visible = False
  end
  object Memo1: TMemo
    Left = 0
    Top = 40
    Width = 1080
    Height = 683
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 3
  end
  object Button2: TButton
    Left = 392
    Top = 8
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 4
    OnClick = Button2Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'thread'
    Filter = 'ThreadLog|*.thread'
    FilterIndex = 0
    Left = 352
    Top = 8
  end
end
