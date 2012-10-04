object frmMapLayersOptions: TfrmMapLayersOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Interface Options'
  ClientHeight = 459
  ClientWidth = 325
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
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
    Height = 420
    ActivePage = tsStatBar
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 325
    ExplicitHeight = 351
    object tsStatBar: TTabSheet
      Caption = 'Status Bar'
      ExplicitLeft = -36
      ExplicitTop = 0
      ExplicitWidth = 281
      ExplicitHeight = 165
      DesignSize = (
        368
        392)
      object lblStatBarTextColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 193
        Width = 52
        Height = 13
        Alignment = taRightJustify
        Caption = 'Text color:'
      end
      object lblStatBarTextOpacity: TLabel
        AlignWithMargins = True
        Left = 230
        Top = 193
        Width = 41
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Opacity:'
      end
      object lblStatBarBackgroundColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 240
        Width = 86
        Height = 13
        Alignment = taRightJustify
        Caption = 'Background color:'
      end
      object lblStatBarBackgroundOpacity: TLabel
        AlignWithMargins = True
        Left = 230
        Top = 240
        Width = 41
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Opacity:'
      end
      object lblStatBarHeight: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 345
        Width = 35
        Height = 13
        Alignment = taRightJustify
        Caption = 'Height:'
      end
      object lblStatBarRedrawTime: TLabel
        AlignWithMargins = True
        Left = 92
        Top = 345
        Width = 104
        Height = 13
        Alignment = taRightJustify
        Caption = 'Redraw interval (ms):'
      end
      object chkStatBarVisible: TCheckBox
        Left = 3
        Top = 9
        Width = 282
        Height = 17
        Caption = 'Visible'
        TabOrder = 0
      end
      object clrbxStatBarTextColor: TColorBox
        Left = 3
        Top = 212
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
        Top = 212
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
        Top = 259
        Width = 221
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 3
      end
      object seStatBarBackgroundOpacity: TSpinEdit
        Left = 230
        Top = 259
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
        Top = 287
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
          ExplicitLeft = 255
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
          ExplicitLeft = 199
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
          ExplicitWidth = 183
        end
      end
      object seStatBarHeight: TSpinEdit
        Left = 3
        Top = 364
        Width = 82
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 6
        Value = 17
      end
      object seStatBarRedrawTime: TSpinEdit
        Left = 91
        Top = 364
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
        Width = 282
        Height = 17
        Caption = 'Show Zoom Info'
        TabOrder = 8
      end
      object chkStatBarLonLatInfo: TCheckBox
        Left = 3
        Top = 55
        Width = 282
        Height = 17
        Caption = 'Show LonLat Info'
        TabOrder = 9
      end
      object chkStatBarMetrPerPixInfo: TCheckBox
        Left = 3
        Top = 78
        Width = 282
        Height = 17
        Caption = 'Show Meter Per Pixel Info'
        TabOrder = 10
      end
      object chkStatBarTimeZoneInfo: TCheckBox
        Left = 3
        Top = 101
        Width = 282
        Height = 17
        Caption = 'Show TimeZone Info'
        TabOrder = 11
      end
      object chkStatBarDownloadInfo: TCheckBox
        Left = 3
        Top = 124
        Width = 282
        Height = 17
        Caption = 'Show Download Info'
        TabOrder = 12
      end
      object chkStatBarQueueInfo: TCheckBox
        Left = 3
        Top = 147
        Width = 282
        Height = 17
        Caption = 'Show Queue Info'
        TabOrder = 13
      end
      object chkStatBarTilePathInfo: TCheckBox
        Left = 3
        Top = 170
        Width = 282
        Height = 17
        Caption = 'Show Tile Path Info'
        TabOrder = 14
      end
    end
    object tsScaleLine: TTabSheet
      Caption = 'Scale Legend'
      ImageIndex = 1
      ExplicitWidth = 317
      ExplicitHeight = 298
      DesignSize = (
        368
        392)
      object lblScalelineColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 111
        Width = 29
        Height = 13
        Alignment = taRightJustify
        Caption = 'Color:'
      end
      object lblScaleLineWidth: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 263
        Width = 32
        Height = 13
        Alignment = taRightJustify
        Caption = 'Width:'
      end
      object lblScaleLineOutlineColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 158
        Width = 64
        Height = 13
        Alignment = taRightJustify
        Caption = 'Outline color:'
      end
      object lblScaleLineColorOpacity: TLabel
        AlignWithMargins = True
        Left = 229
        Top = 111
        Width = 41
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Opacity:'
        ExplicitLeft = 256
      end
      object lblScalelineOutlineOpacity: TLabel
        AlignWithMargins = True
        Left = 229
        Top = 158
        Width = 41
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Opacity:'
        ExplicitLeft = 256
      end
      object chkScaleLineVisible: TCheckBox
        Left = 3
        Top = 9
        Width = 282
        Height = 17
        Caption = 'Visible'
        TabOrder = 0
      end
      object chkShowVertScaleLine: TCheckBox
        Left = 3
        Top = 32
        Width = 282
        Height = 17
        Caption = 'Show Vertical Legend'
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
        ExplicitWidth = 260
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
        ExplicitWidth = 247
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
        ExplicitWidth = 247
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
        ExplicitLeft = 256
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
        ExplicitLeft = 256
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
        ExplicitWidth = 336
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
          ExplicitLeft = 255
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
          ExplicitLeft = 199
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
          ExplicitWidth = 183
        end
      end
    end
  end
  object btnCancel: TButton
    Left = 244
    Top = 428
    Width = 73
    Height = 23
    Hint = 'Cancel'
    Align = alCustom
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitLeft = 193
    ExplicitTop = 357
  end
  object btnApply: TButton
    Left = 86
    Top = 428
    Width = 73
    Height = 23
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Default = True
    TabOrder = 2
    OnClick = btnApplyClick
    ExplicitLeft = 59
    ExplicitTop = 370
  end
  object btnOk: TButton
    Left = 165
    Top = 428
    Width = 73
    Height = 23
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    TabOrder = 3
    OnClick = btnOkClick
    ExplicitLeft = 138
    ExplicitTop = 370
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 152
    Top = 24
  end
end
