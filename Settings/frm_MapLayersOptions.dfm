object frmMapLayersOptions: TfrmMapLayersOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Interface Options'
  ClientHeight = 482
  ClientWidth = 325
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pgcOptions: TPageControl
    Left = 0
    Top = 0
    Width = 376
    Height = 443
    ActivePage = tsStatBar
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsStatBar: TTabSheet
      Caption = 'Status Bar'
      DesignSize = (
        368
        415)
      object lblStatBarTextColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 216
        Width = 52
        Height = 13
        Caption = 'Text color:'
      end
      object lblStatBarTextOpacity: TLabel
        AlignWithMargins = True
        Left = 230
        Top = 216
        Width = 41
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Opacity:'
      end
      object lblStatBarBackgroundColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 263
        Width = 86
        Height = 13
        Caption = 'Background color:'
      end
      object lblStatBarBackgroundOpacity: TLabel
        AlignWithMargins = True
        Left = 230
        Top = 263
        Width = 41
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Opacity:'
      end
      object lblStatBarHeight: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 368
        Width = 35
        Height = 13
        Caption = 'Height:'
      end
      object lblStatBarRedrawTime: TLabel
        AlignWithMargins = True
        Left = 92
        Top = 368
        Width = 104
        Height = 13
        Caption = 'Redraw interval (ms):'
      end
      object chkStatBarHide: TCheckBox
        Left = 3
        Top = 9
        Width = 310
        Height = 17
        Caption = 'Hide Status Bar'
        TabOrder = 0
      end
      object clrbxStatBarTextColor: TColorBox
        Left = 3
        Top = 235
        Width = 221
        Height = 22
        Selected = clWhite
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 1
      end
      object seStatBarTextOpacity: TSpinEdit
        Left = 230
        Top = 235
        Width = 83
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 255
        MinValue = 0
        TabOrder = 2
        Value = 255
      end
      object clrbxStatBarBackgroundColor: TColorBox
        Left = 3
        Top = 282
        Width = 221
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 3
      end
      object seStatBarBackgroundOpacity: TSpinEdit
        Left = 230
        Top = 282
        Width = 83
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 255
        MinValue = 0
        TabOrder = 4
        Value = 80
      end
      object grpStatBarFont: TGroupBox
        Left = 3
        Top = 310
        Width = 310
        Height = 52
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Font'
        TabOrder = 5
        DesignSize = (
          310
          52)
        object btnStatBarFont: TSpeedButton
          Left = 283
          Top = 16
          Width = 17
          Height = 22
          Anchors = [akTop, akRight]
          Caption = '...'
          OnClick = btnStatBarFontClick
        end
        object seStatBarFontSize: TSpinEdit
          Left = 227
          Top = 16
          Width = 50
          Height = 22
          Anchors = [akTop, akRight]
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 10
        end
        object edtStatBarFontName: TEdit
          Left = 10
          Top = 16
          Width = 211
          Height = 21
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          Text = 'Arial'
        end
      end
      object seStatBarHeight: TSpinEdit
        Left = 3
        Top = 387
        Width = 82
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 6
        Value = 17
      end
      object seStatBarRedrawTime: TSpinEdit
        Left = 91
        Top = 387
        Width = 161
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 7
        Value = 200
      end
      object chkStatBarZoomInfo: TCheckBox
        Left = 3
        Top = 32
        Width = 310
        Height = 17
        Caption = 'Show Zoom Info'
        TabOrder = 8
      end
      object chkStatBarLonLatInfo: TCheckBox
        Left = 3
        Top = 55
        Width = 310
        Height = 17
        Caption = 'Show LonLat Info'
        TabOrder = 9
      end
      object chkStatBarMetrPerPixInfo: TCheckBox
        Left = 3
        Top = 78
        Width = 310
        Height = 17
        Caption = 'Show Meter Per Pixel Info'
        TabOrder = 10
      end
      object chkStatBarTimeZoneInfo: TCheckBox
        Left = 3
        Top = 124
        Width = 310
        Height = 17
        Caption = 'Show Time Zone Info'
        TabOrder = 11
      end
      object chkStatBarDownloadInfo: TCheckBox
        Left = 3
        Top = 147
        Width = 310
        Height = 17
        Caption = 'Show Download Info'
        TabOrder = 12
      end
      object chkStatBarQueueInfo: TCheckBox
        Left = 3
        Top = 170
        Width = 310
        Height = 17
        Caption = 'Show Queue Info'
        TabOrder = 13
      end
      object chkStatBarTilePathInfo: TCheckBox
        Left = 3
        Top = 193
        Width = 310
        Height = 17
        Caption = 'Show Tile Path Info'
        TabOrder = 14
      end
      object chkStatBarElevation: TCheckBox
        Left = 3
        Top = 101
        Width = 310
        Height = 17
        Caption = 'Show Elevation Info'
        TabOrder = 15
        OnClick = chkStatBarElevationClick
      end
    end
    object tsScaleLine: TTabSheet
      Caption = 'Scale Legend'
      ImageIndex = 1
      DesignSize = (
        368
        415)
      object lblScalelineColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 111
        Width = 29
        Height = 13
        Caption = 'Color:'
      end
      object lblScaleLineWidth: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 263
        Width = 32
        Height = 13
        Caption = 'Width:'
      end
      object lblScaleLineOutlineColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 158
        Width = 64
        Height = 13
        Caption = 'Outline color:'
      end
      object lblScaleLineColorOpacity: TLabel
        AlignWithMargins = True
        Left = 229
        Top = 111
        Width = 41
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Opacity:'
      end
      object lblScalelineOutlineOpacity: TLabel
        AlignWithMargins = True
        Left = 229
        Top = 158
        Width = 41
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Opacity:'
      end
      object chkScaleLineHide: TCheckBox
        Left = 3
        Top = 9
        Width = 299
        Height = 17
        Caption = 'Hide Scale Legend'
        TabOrder = 0
      end
      object chkShowVertScaleLine: TCheckBox
        Left = 3
        Top = 32
        Width = 309
        Height = 17
        Caption = 'Show Vertical Scale Legend'
        TabOrder = 1
      end
      object rgScaleLineNumbFormat: TRadioGroup
        Left = 3
        Top = 55
        Width = 309
        Height = 50
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Numbers Format'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Nice'
          'Round'
          'Science')
        TabOrder = 2
      end
      object seScaleLineWidth: TSpinEdit
        Left = 3
        Top = 282
        Width = 62
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 256
      end
      object clrbxScaleLineColor: TColorBox
        Left = 3
        Top = 130
        Width = 220
        Height = 22
        Selected = clWhite
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 4
      end
      object clrbxScaleLineOutlineColor: TColorBox
        Left = 3
        Top = 177
        Width = 220
        Height = 22
        Selected = clWhite
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 5
      end
      object seScaleLineColorOpacity: TSpinEdit
        Left = 229
        Top = 130
        Width = 83
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 255
        MinValue = 0
        TabOrder = 6
        Value = 255
      end
      object seScaleLineOutlineOpacity: TSpinEdit
        Left = 229
        Top = 177
        Width = 83
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 255
        MinValue = 0
        TabOrder = 7
        Value = 170
      end
      object grpScaleLineFont: TGroupBox
        Left = 3
        Top = 205
        Width = 309
        Height = 52
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Font'
        TabOrder = 8
        DesignSize = (
          309
          52)
        object btnScaleLineFont: TSpeedButton
          Left = 282
          Top = 16
          Width = 17
          Height = 22
          Anchors = [akTop, akRight]
          Caption = '...'
          OnClick = btnScaleLineFontClick
        end
        object seScaleLineFontSize: TSpinEdit
          Left = 226
          Top = 16
          Width = 50
          Height = 22
          Anchors = [akTop, akRight]
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 8
        end
        object edtScaleLineFont: TEdit
          Left = 10
          Top = 16
          Width = 210
          Height = 21
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          Text = 'Arial'
        end
      end
    end
    object tsElevation: TTabSheet
      Caption = 'Elevation Info'
      ImageIndex = 2
      object lblElevPrimaryProvider: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 55
        Width = 83
        Height = 13
        Caption = 'Primary provider:'
      end
      object chkElevShowInStatusBar: TCheckBox
        Left = 3
        Top = 9
        Width = 310
        Height = 17
        Caption = 'Show Elevation Info In Status Bar'
        TabOrder = 0
        OnClick = chkElevShowInStatusBarClick
      end
      object chkElevTrySecondaryProviders: TCheckBox
        Left = 3
        Top = 32
        Width = 310
        Height = 17
        Caption = 'Show Elevation Info From Any Available Source'
        TabOrder = 1
      end
      object cbbElevProviderList: TComboBox
        Left = 3
        Top = 71
        Width = 310
        Height = 21
        ItemHeight = 0
        TabOrder = 2
        Text = 'cbbElevProviderList'
      end
    end
  end
  object btnCancel: TButton
    Left = 244
    Top = 451
    Width = 73
    Height = 23
    Hint = 'Cancel'
    Align = alCustom
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnApply: TButton
    Left = 86
    Top = 451
    Width = 73
    Height = 23
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Default = True
    TabOrder = 2
    OnClick = btnApplyClick
  end
  object btnOk: TButton
    Left = 165
    Top = 451
    Width = 73
    Height = 23
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    TabOrder = 3
    OnClick = btnOkClick
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 8
    Top = 448
  end
end
