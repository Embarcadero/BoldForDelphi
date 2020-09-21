object MainForm: TMainForm
  Left = 121
  Top = 137
  Width = 878
  Height = 570
  Caption = 'MainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 870
    Height = 543
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Bold'
      object Label1: TLabel
        Left = 16
        Top = 8
        Width = 59
        Height = 13
        Caption = 'Customers'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object BoldLabel1: TBoldLabel
        Left = 8
        Top = 328
        Width = 158
        Height = 13
        BoldHandle = blhAllCustomers
        BoldProperties.Expression = #39'Contacts for "'#39' + name + '#39'"'#39
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label3: TLabel
        Left = 216
        Top = 368
        Width = 22
        Height = 13
        Caption = 'Log'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label4: TLabel
        Left = 208
        Top = 8
        Width = 145
        Height = 33
        AutoSize = False
        Caption = 'Customers are fetched from external data source.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsItalic]
        ParentFont = False
        WordWrap = True
      end
      object Label2: TLabel
        Left = 280
        Top = 40
        Width = 31
        Height = 13
        Caption = 'Orders'
      end
      object Label5: TLabel
        Left = 456
        Top = 40
        Width = 58
        Height = 13
        Caption = 'Responsible'
      end
      object Label6: TLabel
        Left = 456
        Top = 96
        Width = 25
        Height = 13
        Caption = 'Items'
      end
      object BoldGrid1: TBoldGrid
        Left = 8
        Top = 64
        Width = 257
        Height = 241
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = blhAllCustomers
        BoldProperties.NilElementMode = neNone
        Columns = <
          item
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
          end
          item
            BoldProperties.Expression = 'customerID'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'CustomerID'
          end
          item
            BoldProperties.Expression = 'name'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Name'
          end
          item
            BoldProperties.Expression = 'isMarried'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'IsMarried'
            AllowCheckBox = True
          end>
        DefaultRowHeight = 17
        EnableColAdjust = False
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        ColWidths = (
          17
          64
          93
          47)
      end
      object BoldNavigator1: TBoldNavigator
        Left = 8
        Top = 32
        Width = 192
        Height = 25
        BoldHandle = blhAllCustomers
        TabOrder = 1
        ImageIndices.nbFirst = -1
        ImageIndices.nbPrior = -1
        ImageIndices.nbNext = -1
        ImageIndices.nbLast = -1
        ImageIndices.nbInsert = -1
        ImageIndices.nbDelete = -1
        ImageIndices.nbMoveUp = -1
        ImageIndices.nbMoveDown = -1
        DeleteQuestion = 'Delete object?'
        UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
        RemoveQuestion = 'Remove "%1:s" from the list?'
      end
      object BoldGrid2: TBoldGrid
        Left = 8
        Top = 384
        Width = 201
        Height = 113
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = blhCustomerContacts
        BoldProperties.NilElementMode = neNone
        Columns = <
          item
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
          end
          item
            BoldProperties.Expression = 'name'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Name'
          end>
        DefaultRowHeight = 17
        EnableColAdjust = False
        TabOrder = 2
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        ColWidths = (
          17
          156)
      end
      object BoldNavigator2: TBoldNavigator
        Left = 8
        Top = 352
        Width = 192
        Height = 25
        BoldHandle = blhCustomerContacts
        TabOrder = 3
        ImageIndices.nbFirst = -1
        ImageIndices.nbPrior = -1
        ImageIndices.nbNext = -1
        ImageIndices.nbLast = -1
        ImageIndices.nbInsert = -1
        ImageIndices.nbDelete = -1
        ImageIndices.nbMoveUp = -1
        ImageIndices.nbMoveDown = -1
        DeleteQuestion = 'Delete object?'
        UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
        RemoveQuestion = 'Remove "%1:s" from the list?'
      end
      object Memo1: TMemo
        Left = 216
        Top = 384
        Width = 377
        Height = 113
        Color = clInfoBk
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 4
      end
      object btnOpen: TButton
        Left = 712
        Top = 72
        Width = 131
        Height = 25
        Action = dmMain.BoldActivateSystemAction1
        TabOrder = 5
      end
      object BoldListBox1: TBoldListBox
        Left = 280
        Top = 64
        Width = 169
        Height = 241
        Alignment = taLeftJustify
        BoldHandle = blhOrders
        BoldProperties.NilElementMode = neNone
        DragMode = dmAutomatic
        ItemHeight = 16
        TabOrder = 6
      end
      object BoldNavigator3: TBoldNavigator
        Left = 344
        Top = 40
        Width = 56
        Height = 18
        BoldHandle = blhOrders
        TabOrder = 7
        VisibleButtons = [nbInsert, nbDelete]
        ImageIndices.nbFirst = -1
        ImageIndices.nbPrior = -1
        ImageIndices.nbNext = -1
        ImageIndices.nbLast = -1
        ImageIndices.nbInsert = -1
        ImageIndices.nbDelete = -1
        ImageIndices.nbMoveUp = -1
        ImageIndices.nbMoveDown = -1
        DeleteQuestion = 'Delete object?'
        UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
        RemoveQuestion = 'Remove "%1:s" from the list?'
      end
      object btnSave: TButton
        Left = 712
        Top = 312
        Width = 129
        Height = 25
        Action = dmMain.BoldUpdateDBAction1
        TabOrder = 8
      end
      object btnCreateDB: TButton
        Left = 712
        Top = 16
        Width = 129
        Height = 25
        Action = dmMain.BoldIBDatabaseAction1
        TabOrder = 9
      end
      object BoldEdit1: TBoldEdit
        Left = 456
        Top = 64
        Width = 121
        Height = 21
        BoldHandle = blhOrders
        BoldProperties.Expression = 'responsible'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 10
      end
      object BoldGrid3: TBoldGrid
        Left = 456
        Top = 112
        Width = 385
        Height = 193
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = blhItems
        BoldProperties.NilElementMode = neNone
        Columns = <
          item
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
          end
          item
            BoldProperties.Expression = 'part.description'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Product'
          end
          item
            BoldProperties.Expression = 'part.listPrice'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Price'
          end
          item
            BoldProperties.Expression = 'qty'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Qty'
          end
          item
            BoldProperties.Expression = 'discount'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Discount'
          end>
        DefaultRowHeight = 17
        EnableColAdjust = False
        TabOrder = 11
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        ColWidths = (
          17
          205
          33
          33
          49)
      end
    end
  end
  object blhAllCustomers: TBoldListHandle
    RootHandle = dmMain.bshMain
    Expression = 'Customer.allInstances'
    Left = 208
    Top = 152
  end
  object blhCustomerContacts: TBoldListHandle
    RootHandle = blhAllCustomers
    Expression = 'contacts'
    Left = 80
    Top = 448
  end
  object blhOrders: TBoldListHandle
    RootHandle = blhAllCustomers
    Expression = 'orders'
    Left = 324
    Top = 128
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 548
    Top = 488
  end
  object blhItems: TBoldListHandle
    RootHandle = blhOrders
    Expression = 'items'
    Left = 508
    Top = 232
  end
end
