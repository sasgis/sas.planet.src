object DMMarksDb: TDMMarksDb
  OldCreateOrder = False
  Height = 320
  Width = 409
  object CDSKategory: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        Attributes = [faReadonly, faUnNamed]
        DataType = ftAutoInc
      end
      item
        Name = 'name'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 256
      end
      item
        Name = 'visible'
        Attributes = [faUnNamed]
        DataType = ftBoolean
      end
      item
        Name = 'AfterScale'
        Attributes = [faUnNamed]
        DataType = ftSmallint
      end
      item
        Name = 'BeforeScale'
        Attributes = [faUnNamed]
        DataType = ftSmallint
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 16
    Top = 10
    Data = {
      A00000009619E0BD010000001800000005000000000003000000A00002696404
      0001001200010007535542545950450200490008004175746F696E6300046E61
      6D6502004900100001000557494454480200020000010776697369626C650200
      0300100000000A41667465725363616C6502000100100000000B4265666F7265
      5363616C65020001001000000001000C4155544F494E4356414C554504000100
      01000000}
    object CDSKategoryid: TAutoIncField
      FieldName = 'id'
      ReadOnly = True
    end
    object CDSKategoryname: TStringField
      FieldName = 'name'
      Size = 256
    end
    object CDSKategoryvisible: TBooleanField
      FieldName = 'visible'
    end
    object CDSKategoryAfterScale: TSmallintField
      FieldName = 'AfterScale'
    end
    object CDSKategoryBeforeScale: TSmallintField
      FieldName = 'BeforeScale'
    end
  end
  object CDSmarks: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        Attributes = [faReadonly, faUnNamed]
        DataType = ftAutoInc
      end
      item
        Name = 'name'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 255
      end
      item
        Name = 'descr'
        Attributes = [faUnNamed]
        DataType = ftMemo
      end
      item
        Name = 'scale1'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'scale2'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'lonlatarr'
        Attributes = [faUnNamed]
        DataType = ftBlob
      end
      item
        Name = 'lonL'
        Attributes = [faUnNamed]
        DataType = ftFloat
      end
      item
        Name = 'latT'
        Attributes = [faUnNamed]
        DataType = ftFloat
      end
      item
        Name = 'LonR'
        Attributes = [faUnNamed]
        DataType = ftFloat
      end
      item
        Name = 'LatB'
        Attributes = [faUnNamed]
        DataType = ftFloat
      end
      item
        Name = 'color1'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'color2'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'visible'
        Attributes = [faUnNamed]
        DataType = ftBoolean
      end
      item
        Name = 'picname'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 20
      end
      item
        Name = 'categoryid'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'LonR;LonL;LatT;LatB;visible'
    Params = <>
    StoreDefs = True
    Left = 68
    Top = 10
    Data = {
      600100009619E0BD01000000180000000F000000000003000000600102696404
      0001001200010007535542545950450200490008004175746F696E6300046E61
      6D65020049001000010005574944544802000200FF0005646573637204004B00
      1000010007535542545950450200490005005465787400067363616C65310400
      010010000000067363616C65320400010010000000096C6F6E6C617461727204
      004B0010000100075355425459504502004900070042696E61727900046C6F6E
      4C0800040010000000046C6174540800040010000000044C6F6E520800040010
      000000044C617442080004001000000006636F6C6F7231040001001000000006
      636F6C6F723204000100100000000776697369626C6502000300100000000770
      69636E616D6501004900100001000557494454480200020014000A6361746567
      6F72796964040001001000000001000C4155544F494E4356414C554504000100
      01000000}
    object CDSmarksid: TAutoIncField
      FieldName = 'id'
      ReadOnly = True
    end
    object CDSmarksname: TStringField
      FieldName = 'name'
      Size = 255
    end
    object CDSmarksdescr: TMemoField
      FieldName = 'descr'
      BlobType = ftMemo
    end
    object CDSmarksscale1: TIntegerField
      FieldName = 'scale1'
    end
    object CDSmarksscale2: TIntegerField
      FieldName = 'scale2'
    end
    object CDSmarkslonlatarr: TBlobField
      FieldName = 'lonlatarr'
    end
    object CDSmarkslonL: TFloatField
      FieldName = 'lonL'
    end
    object CDSmarkslatT: TFloatField
      FieldName = 'latT'
    end
    object CDSmarksLonR: TFloatField
      FieldName = 'LonR'
    end
    object CDSmarksLatB: TFloatField
      FieldName = 'LatB'
    end
    object CDSmarkscolor1: TIntegerField
      FieldName = 'color1'
    end
    object CDSmarkscolor2: TIntegerField
      FieldName = 'color2'
    end
    object CDSmarksvisible: TBooleanField
      FieldName = 'visible'
    end
    object CDSmarkspicname: TStringField
      FieldName = 'picname'
    end
    object CDSmarkscategoryid: TIntegerField
      FieldName = 'categoryid'
    end
  end
end
