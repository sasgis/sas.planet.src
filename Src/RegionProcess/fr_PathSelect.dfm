object frPathSelect: TfrPathSelect
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object pnlmain: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 23
    Align = alTop
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 0
    object pnlPath: TPanel
      Left = 201
      Top = 0
      Width = 250
      Height = 23
      Align = alClient
      BevelEdges = []
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 169
      ExplicitWidth = 282
      object EPath: TEdit
        Left = 0
        Top = 0
        Width = 202
        Height = 23
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 234
      end
      object pnlButtnos: TPanel
        Left = 202
        Top = 0
        Width = 48
        Height = 23
        Align = alRight
        BevelEdges = []
        BevelKind = bkTile
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 234
        object BtnDef: TButton
          Tag = 2
          Left = 2
          Top = 2
          Width = 23
          Height = 19
          Align = alCustom
          Caption = '<>'
          TabOrder = 0
          OnClick = BtnDefClick
        end
        object BtnSelectPath: TButton
          Tag = 2
          Left = 25
          Top = 2
          Width = 23
          Height = 19
          Align = alCustom
          Caption = '...'
          TabOrder = 1
          OnClick = BtnSelectPathClick
        end
      end
    end
    object pnlCaption: TPanel
      Left = 0
      Top = 0
      Width = 201
      Height = 23
      Align = alLeft
      BevelEdges = []
      BevelOuter = bvNone
      TabOrder = 1
      object LCaption: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 195
        Height = 17
        Align = alClient
        Layout = tlCenter
        ExplicitWidth = 3
        ExplicitHeight = 13
      end
    end
  end
end
