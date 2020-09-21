object dmMain: TdmMain
  OldCreateOrder = False
  Left = 104
  Top = 145
  Height = 479
  Width = 885
  object BoldExternalDataPersistenceHandle: TBoldExternalPersistenceHandleEventDriven
    Config = <
      item
        ExpressionName = 'Customer'
        OnCreateObject = CustomerCreateObject
        OnReadObject = CustomerReadObject
        OnUpdateObject = CustomerUpdateObject
        OnReadMember = CustomerReadMember
        OnDeleteObject = CustomerDeleteObject
        OnExists = CustomerExists
        OnGetKeyList = CustomerGetKeyList
      end
      item
        ExpressionName = 'Order'
        OnCreateObject = OrderCreateObject
        OnReadObject = OrderReadObject
        OnUpdateObject = OrderUpdateObject
        OnReadMember = OrderReadMember
        OnDeleteObject = OrderDeleteObject
      end
      item
        ExpressionName = 'Employee'
        OnReadObject = EmployeeReadObject
        OnReadMember = EmployeeReadMember
      end
      item
        ExpressionName = 'Item'
        OnReadObject = ItemReadObject
        OnGetKeyFromObject = ItemGetKeyFromObject
        OnAssignKeyToObject = ItemAssignKeyToObject
        OnGetInternalSQLForKeys = BoldExternalDataPersistenceHandleConfigItemGetInternalSQLForKeys
      end
      item
        ExpressionName = 'Part'
        OnReadObject = PartReadObject
        OnReadMember = PartReadMember
      end
      item
        ExpressionName = 'Vendor'
        OnReadObject = VendorReadObject
        OnReadMember = VendorReadMember
      end>
    OnBeforeUpdates = BoldExternalDataPersistenceHandleBeforeUpdates
    OnAfterUpdates = BoldExternalDataPersistenceHandleAfterUpdates
    OnActivate = BoldExternalDataPersistenceHandleActivate
    OnDeActivate = BoldExternalDataPersistenceHandleDeActivate
    NextPersistenceHandle = BoldPersistenceHandleDB1
    BoldModel = BoldModel1
    Left = 344
    Top = 16
  end
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 64
    Top = 80
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = False
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldExternalDataPersistenceHandle
    Left = 64
    Top = 136
  end
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 64
    Top = 16
    Model = (
      'VERSION 19'
      '(Model'
      #9'"BusinessClasses"'
      #9'"BusinessClassesRoot"'
      #9'""'
      #9'""'
      
        #9'"_BoldInternal.toolId=3CA1F25F0024,_BoldInternal.flattened=True' +
        ',_Boldify.boldified=True,_BoldInternal.ModelErrors=,Bold.DelphiN' +
        'ame=<Name>,Bold.RootClass=BusinessClassesRoot"'
      #9'(Classes'
      #9#9'(Class'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'"<NONE>"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"_BoldInternal.toolId=3CA1FB0102A3,persistence=persistent"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Contact"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"_BoldInternal.toolId=3CA1FB0102CB,persistence=persistent"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CA1FB0102DF,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Customer"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"_BoldInternal.toolId=3CA1FB0102FD,persistence=persistent,Bol' +
        'd.DefaultStringRepresentation=name,Bold.Storage=PartiallyExterna' +
        'l"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CA1FB010311,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.ColumnName=Company,Bold.St' +
        'orage=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"CustomerID"'
      #9#9#9#9#9'"integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"EK"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CA1FB010325,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Length=16,Bold.ColumnName=' +
        'CustNo,Bold.Storage=ExternalKey"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"IsMarried"'
      #9#9#9#9#9'"Boolean"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"I"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CAC249702E6,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Order"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"_BoldInternal.toolId=3CADB7E50022,persistence=persistent,Bol' +
        'd.TableName=Orders,\"Bold.DefaultStringRepresentation='#39'Order #'#39' ' +
        '+ orderNo.AsString + '#39' $'#39'+totalcost.asstring\",Bold.Storage=Part' +
        'iallyExternal"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"OrderNo"'
      #9#9#9#9#9'"integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"EK"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB9140367,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=ExternalKey"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"ShipDate"'
      #9#9#9#9#9'"Date"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB81403D7,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.AllowNULL=True,Bold.Storag' +
        'e=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"SaleDate"'
      #9#9#9#9#9'"Date"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB81B00AC,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.AllowNULL=True,Bold.Storag' +
        'e=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"AmountPaid"'
      #9#9#9#9#9'"currency"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB8410237,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"totalCost"'
      #9#9#9#9#9'"currency"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CB8B20A01E2,BoldStdUML.Persistence=p' +
        'ersistent,persistence=transient,derived=True,Bold.DerivationOCL=' +
        'items.totalcost->sum"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"MayUpdate"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Boolean"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CB304B80344,Bold.OperationKind=Overr' +
        'ide"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Part"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"_BoldInternal.toolId=3CADB8580154,persistence=persistent,Bol' +
        'd.TableName=Parts,Bold.DefaultStringRepresentation=description,B' +
        'old.Storage=PartiallyExternal"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"PartNo"'
      #9#9#9#9#9'"integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"EK"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB942024B,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=ExternalKey"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Description"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB95A00BF,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Cost"'
      #9#9#9#9#9'"currency"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB962021F,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"ListPrice"'
      #9#9#9#9#9'"currency"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB968008D,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"MayUpdate"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CB8B3E101CB,Bold.OperationKind=Overr' +
        'ide"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Vendor"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"_BoldInternal.toolId=3CADB9740044,persistence=persistent,Bol' +
        'd.DefaultStringRepresentation=vendorname,Bold.Storage=PartiallyE' +
        'xternal"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"VendorNo"'
      #9#9#9#9#9'"integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"EK"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB9A00282,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=ExternalKey"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"VendorName"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB9A600A0,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Address1"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB9AA03D1,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Address2"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB9B0004A,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Preferred"'
      #9#9#9#9#9'"Boolean"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADB9C80275,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Item"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"_BoldInternal.toolId=3CADB9E80235,persistence=persistent,Bol' +
        'd.TableName=Items,\"Bold.DefaultStringRepresentation=qty.asstrin' +
        'g + '#39' pcs of '#39'+part.description\",Bold.Storage=PartiallyExternal' +
        '"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Qty"'
      #9#9#9#9#9'"integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADBA030021,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Discount"'
      #9#9#9#9#9'"integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADBA0802FA,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"OrderNo"'
      #9#9#9#9#9'"integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"EK"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADBEE40140,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=ExternalKey"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"ItemNo"'
      #9#9#9#9#9'"integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"EK"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADBEEB02A8,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=ExternalKey"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"totalCost"'
      #9#9#9#9#9'"currency"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CB8B2160211,BoldStdUML.Persistence=p' +
        'ersistent,persistence=transient,derived=True,Bold.DerivationOCL=' +
        'qty*part.listprice*((100-discount)/100)"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"PrepareUpdate"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CB8B39E0319,Bold.OperationKind=Overr' +
        'ide"'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"MayUpdate"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CB8B3BC0254,Bold.OperationKind=Overr' +
        'ide"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Employee"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"_BoldInternal.toolId=3CADC12801A8,persistence=persistent,\"B' +
        'old.DefaultStringRepresentation=firstname + '#39' '#39' + lastname\",Bol' +
        'd.Storage=PartiallyExternal"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"EmployeeNo"'
      #9#9#9#9#9'"integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"EK"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADC12F018A,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=ExternalKey"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"LastName"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADC16801D2,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"FirstName"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"E"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADC16B019A,BoldStdUML.Persistence=p' +
        'ersistent,persistence=persistent,Bold.Storage=External"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"bribesisBribedBy"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=persistent,_Boldify.autoCreated=True"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9')'
      #9'(Associations'
      #9#9'(Association'
      #9#9#9'"ContactToCustomer"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=persistent,_BoldInternal.toolId=3CA1FB010357,Bol' +
        'dStdUML.Persistence=persistent,Bold.DelphiName=<Name>"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Contacts"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Customer"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..*"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CA1FB010358,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Customer"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Contact"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CA1FB010361"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"Customerorders"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=persistent,_BoldInternal.toolId=3CADB80600B6,Bol' +
        'dStdUML.Persistence=persistent,_Boldify.noName=True,Bold.DelphiN' +
        'ame=<Name>,Bold.Storage=External"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Customer"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Order"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CADB806035F,_Boldify.noName=True"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"orders"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Customer"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..n"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CADB8060387,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"Vendorparts"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=persistent,_BoldInternal.toolId=3CADB979009B,Bol' +
        'dStdUML.Persistence=persistent,_Boldify.noName=True,Bold.DelphiN' +
        'ame=<Name>,Bold.Storage=External"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Vendor"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Part"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CADB9790312,_Boldify.noName=True"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"parts"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Vendor"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..n"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CADB979031C,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"Orderitems"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=persistent,_BoldInternal.toolId=3CADBA110343,Bol' +
        'dStdUML.Persistence=persistent,_Boldify.noName=True,Bold.DelphiN' +
        'ame=<Name>,Bold.Storage=External"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Order"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Item"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADBA1200AF,_Boldify.noName=True,Bol' +
        'd.ColumnName=OrderNo_"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"items"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Order"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..n"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CADBA1200B0,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"Partitems"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=persistent,_BoldInternal.toolId=3CADBA1A0115,Bol' +
        'dStdUML.Persistence=persistent,_Boldify.noName=True,Bold.DelphiN' +
        'ame=<Name>,Bold.Storage=External"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Part"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Item"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADBA1A0396,_Boldify.noName=True,Bol' +
        'd.ColumnName=PartNo"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"items"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Part"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..n"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CADBA1A0397,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"responsibleorders"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=persistent,_BoldInternal.toolId=3CADC1590072,Bol' +
        'dStdUML.Persistence=persistent,_Boldify.noName=True,Bold.DelphiN' +
        'ame=<Name>,Bold.Storage=External"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"responsible"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Order"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CADC15903BB"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"orders"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Employee"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..n"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CADC15903C5,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"parts_Part"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=transient,_BoldInternal.toolId=3CADC1E10367,Bold' +
        'StdUML.Persistence=persistent,derived=True,_Boldify.noName=True,' +
        'Bold.DelphiName=<Name>"'
      #9#9#9'TRUE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"parts"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Order"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..n"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADC1E3011B,Bold.Embed=False,Bold.De' +
        'rivationOCL=items.part"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"x_parts_Order"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Part"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..*"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      
        #9#9#9#9#9'"_BoldInternal.toolId=3CADC1E3011C,_Boldify.defaultMultipli' +
        'city=True,_Boldify.wasEmbeded=True,_Boldify.noName=True,Bold.Emb' +
        'ed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"bribesisBribedBy"'
      #9#9#9'"bribesisBribedBy"'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=persistent,_BoldInternal.toolId=3CADC25C00F6,Bol' +
        'dStdUML.Persistence=persistent,_Boldify.noName=True,Bold.DelphiN' +
        'ame=<Name>"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"bribes"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Vendor"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..n"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CADC25D004E,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"isBribedBy"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Employee"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..n"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=3CADC25D004F,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9')'
      ')')
  end
  object Database1: TDatabase
    DatabaseName = 'Employee'
    DriverName = 'STANDARD'
    Params.Strings = (
      'PATH=c:\program files\common files\borland shared\data'
      'DEFAULT DRIVER=PARADOX'
      'ENABLE BCD=FALSE')
    SessionName = 'Default'
    Left = 392
    Top = 32
  end
  object CustomerTable: TTable
    DatabaseName = 'Employee'
    TableName = 'customer.db'
    Left = 392
    Top = 96
  end
  object OrderTable: TTable
    DatabaseName = 'Employee'
    TableName = 'orders.db'
    Left = 400
    Top = 152
  end
  object BoldPlaceableAFP1: TBoldPlaceableAFP
    Left = 48
    Top = 344
  end
  object EmployeeTable: TTable
    DatabaseName = 'Employee'
    TableName = 'employee.db'
    Left = 400
    Top = 216
  end
  object ActionList1: TActionList
    Left = 136
    Top = 336
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = BoldSystemHandle1
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before closing system?'
      SaveOnClose = saAsk
    end
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      BoldSystemHandle = BoldSystemHandle1
    end
    object BoldIBDatabaseAction1: TBoldIBDatabaseAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = BoldSystemHandle1
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
  end
  object ItemTable: TTable
    DatabaseName = 'Employee'
    TableName = 'items.db'
    Left = 504
    Top = 200
  end
  object PartTable: TTable
    DatabaseName = 'Employee'
    TableName = 'parts.db'
    Left = 464
    Top = 248
  end
  object VendorTable: TTable
    DatabaseName = 'Employee'
    TableName = 'vendors.db'
    Left = 392
    Top = 280
  end
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterIB1
    Left = 168
    Top = 104
  end
  object BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB
    SQLDatabaseConfig.ColumnTypeForDate = 'TIMESTAMP'
    SQLDatabaseConfig.ColumnTypeForTime = 'TIMESTAMP'
    SQLDatabaseConfig.ColumnTypeForDateTime = 'TIMESTAMP'
    SQLDatabaseConfig.ColumnTypeForBlob = 'BLOB'
    SQLDatabaseConfig.ColumnTypeForFloat = 'DOUBLE PRECISION'
    SQLDatabaseConfig.ColumnTypeForCurrency = 'DOUBLE PRECISION'
    SQLDatabaseConfig.ColumnTypeForString = 'VARCHAR(%d)'
    SQLDatabaseConfig.ColumnTypeForInteger = 'INTEGER'
    SQLDatabaseConfig.ColumnTypeForSmallInt = 'SMALLINT'
    SQLDatabaseConfig.DropColumnTemplate = 'ALTER TABLE <TableName> DROP <ColumnName>'
    SQLDatabaseConfig.DropTableTemplate = 'DROP TABLE <TableName>'
    SQLDatabaseConfig.DropIndexTemplate = 'DROP INDEX <IndexName>'
    SQLDatabaseConfig.MaxDbIdentifierLength = 31
    SQLDatabaseConfig.MaxIndexNameLength = 31
    SQLDatabaseConfig.SQLforNotNull = 'NOT NULL'
    SQLDatabaseConfig.QuoteNonStringDefaultValues = False
    SQLDatabaseConfig.SupportsConstraintsInCreateTable = True
    SQLDatabaseConfig.SupportsStringDefaultValues = True
    SQLDatabaseConfig.DBGenerationMode = dbgQuery
    SQLDatabaseConfig.ReservedWords.Strings = (
      'ACTIVE, ADD, ALL, AFTER, ALTER'
      'AND, ANY, AS, ASC, ASCENDING,'
      'AT, AUTO, AUTOINC, AVG, BASE_NAME'
      'BEFORE, BEGIN, BETWEEN, BLOB, BOOLEAN,'
      'BOTH, BY, BYTES, CACHE, CAST, CHAR'
      'CHARACTER, CHECK, CHECK_POINT_LENGTH, COLLATE,'
      'COLUMN, COMMIT, COMMITTED, COMPUTED'
      'CONDITIONAL, CONSTRAINT, CONTAINING, COUNT, CREATE, CSTRING,'
      'CURRENT, CURSOR, DATABASE, DATE, DAY'
      'DEBUG, DEC, DECIMAL, DECLARE, DEFAULT,'
      'DELETE, DESC, DESCENDING, DISTINCT, DO'
      'DOMAIN, DOUBLE, DROP, ELSE, END,'
      'ENTRY_POINT, ESCAPE, EXCEPTION, EXECUTE'
      'EXISTS, EXIT, EXTERNAL, EXTRACT, FILE, FILTER,'
      'FLOAT, FOR, FOREIGN, FROM, FULL, FUNCTION'
      'GDSCODE, GENERATOR, GEN_ID, GRANT,'
      'GROUP, GROUP_COMMIT_WAIT_TIME, HAVING'
      'HOUR, IF, IN, INT, INACTIVE, INDEX, INNER,'
      'INPUT_TYPE, INSERT, INTEGER, INTO'
      'IS, ISOLATION, JOIN, KEY, LONG, LENGTH,'
      'LOGFILE, LOWER, LEADING, LEFT, LEVEL'
      'LIKE, LOG_BUFFER_SIZE, MANUAL, MAX, MAXIMUM_SEGMENT,'
      'MERGE, MESSAGE, MIN, MINUTE, MODULE_NAME'
      'MONEY, MONTH, NAMES, NATIONAL, NATURAL,'
      'NCHAR, NO, NOT, NULL, NUM_LOG_BUFFERS'
      'NUMERIC, OF, ON, ONLY, OPTION,'
      'OR, ORDER, OUTER, OUTPUT_TYPE, OVERFLOW'
      'PAGE_SIZE, PAGE, PAGES, PARAMETER, PASSWORD,'
      'PLAN, POSITION, POST_EVENT, PRECISION'
      
        'PROCEDURE, PROTECTED, PRIMARY, PRIVILEGES, RAW_PARTITIONS, RDB$D' +
        'B_KEY,'
      'READ, REAL, RECORD_VERSION, REFERENCES'
      'RESERV, RESERVING, RETAIN, RETURNING_VALUES, RETURNS, REVOKE,'
      'RIGHT, ROLE, ROLLBACK, SECOND, SEGMENT'
      'SELECT, SET, SHARED, SHADOW, SCHEMA, SINGULAR,'
      'SIZE, SMALLINT, SNAPSHOT, SOME, SORT'
      'SQLCODE, STABILITY, STARTING, STARTS, STATISTICS,'
      'SUB_TYPE, SUBSTRING, SUM, SUSPEND, TABLE'
      'THEN, TIME, TIMESTAMP, TIMEZONE_HOUR, TIMEZONE_MINUTE,'
      'TO, TRAILING, TRANSACTION, TRIGGER, TRIM'
      'UNCOMMITTED, UNION, UNIQUE, UPDATE, UPPER,'
      'USER, VALUE, VALUES, VARCHAR, VARIABLE'
      'VARYING, VIEW, WAIT, WHEN, WHERE,'
      'WHILE, WITH, WORK, WRITE, YEAR')
    SQLDatabaseConfig.StoreEmptyStringsAsNULL = False
    SQLDatabaseConfig.SystemTablePrefix = 'BOLD'
    DataBase = IBDatabase1
    DatabaseEngine = dbeInterbaseSQLDialect3
    Left = 152
    Top = 184
  end
  object IBDatabase1: TIBDatabase
    DatabaseName = 'ExPe.gdb'
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 144
    Top = 240
  end
end
