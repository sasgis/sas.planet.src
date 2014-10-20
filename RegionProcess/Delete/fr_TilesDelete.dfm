object frTilesDelete: TfrTilesDelete
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 451
    Height = 304
    ActivePage = tsTiles
    Align = alClient
    TabOrder = 0
    object tsTiles: TTabSheet
      Caption = 'Tiles'
      object pnlBottom: TPanel
        Left = 0
        Top = 43
        Width = 443
        Height = 100
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object pnlCenter: TPanel
          Left = 0
          Top = 0
          Width = 443
          Height = 100
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          TabOrder = 0
          object flwpnlDelBySize: TFlowPanel
            AlignWithMargins = True
            Left = 6
            Top = 70
            Width = 431
            Height = 24
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            Padding.Top = 2
            TabOrder = 0
            object chkDelBySize: TCheckBox
              Left = 0
              Top = 2
              Width = 13
              Height = 21
              TabOrder = 0
            end
            object lblDelSize: TLabel
              AlignWithMargins = True
              Left = 16
              Top = 5
              Width = 148
              Height = 13
              Caption = 'Delete only tiles of size, bytes:'
            end
            object seDelSize: TSpinEdit
              Left = 167
              Top = 2
              Width = 69
              Height = 22
              MaxValue = 0
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
          end
          object rgTarget: TRadioGroup
            Left = 3
            Top = 3
            Width = 437
            Height = 64
            Align = alTop
            Caption = 'Delete target'
            ItemIndex = 0
            Items.Strings = (
              'Delete tiles'
              'Delete tne'
              'Delete both')
            TabOrder = 1
          end
        end
      end
      object pnlMapSelect: TPanel
        Left = 0
        Top = 0
        Width = 443
        Height = 43
        Align = alTop
        BevelEdges = [beBottom]
        BevelKind = bkTile
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          443
          41)
        object pnlZoom: TPanel
          Left = 384
          Top = 0
          Width = 59
          Height = 41
          Align = alRight
          Alignment = taLeftJustify
          BevelEdges = []
          BevelKind = bkTile
          BevelOuter = bvNone
          TabOrder = 0
          object Labelzoom: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 1
            Width = 30
            Height = 13
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Caption = 'Zoom:'
          end
          object cbbZoom: TComboBox
            Left = 3
            Top = 17
            Width = 53
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
          end
        end
        object pnlFrame: TPanel
          Left = 0
          Top = 5
          Width = 384
          Height = 44
          Alignment = taLeftJustify
          Anchors = [akLeft, akTop, akRight]
          BevelOuter = bvNone
          TabOrder = 1
          object lblMapCaption: TLabel
            Left = 0
            Top = 0
            Width = 384
            Height = 13
            Align = alTop
            Caption = 'Map:'
            ExplicitWidth = 24
          end
        end
      end
    end
    object tsMarks: TTabSheet
      Caption = 'Placemarks'
      ImageIndex = 1
      object chkDelHidden: TCheckBox
        Left = 0
        Top = 56
        Width = 443
        Height = 17
        Align = alTop
        Caption = 'Delete hidden placemarks'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object PnlTop: TPanel
        Left = 0
        Top = 0
        Width = 443
        Height = 56
        Align = alTop
        BevelKind = bkTile
        BevelOuter = bvNone
        TabOrder = 1
        object chkPlacemarks: TCheckBox
          Left = 0
          Top = 0
          Width = 439
          Height = 17
          Align = alTop
          Caption = 'Placemarks'
          TabOrder = 0
        end
        object chkPaths: TCheckBox
          Left = 0
          Top = 17
          Width = 439
          Height = 17
          Align = alTop
          Caption = 'Paths'
          TabOrder = 1
        end
        object chkPolygons: TCheckBox
          Left = 0
          Top = 34
          Width = 439
          Height = 17
          Align = alTop
          Caption = 'Polygons'
          TabOrder = 2
        end
      end
    end
  end
end
