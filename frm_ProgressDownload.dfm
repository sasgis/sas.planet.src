object frmProgressDownload: TfrmProgressDownload
  Left = 226
  Top = 306
  BorderStyle = bsSizeToolWin
  Caption = 'Please wait...'
  ClientHeight = 248
  ClientWidth = 328
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  ScreenSnap = True
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 328
    Height = 248
    Align = alClient
    TabOrder = 0
    OnResize = Panel1Resize
    ExplicitWidth = 330
    ExplicitHeight = 250
    DesignSize = (
      328
      248)
    object RProgr: TRarProgress
      Left = 6
      Top = 204
      Width = 314
      Height = 17
      Min = 0
      Max = 100
      Progress1 = 50
      Progress2 = 30
      Double = True
      LightColor1 = 16770764
      DarkColor1 = 13395456
      LightColor2 = 16768959
      FrameColor1 = 16758122
      FrameColor2 = 16747546
      FillColor1 = 16757606
      FillColor2 = 16749867
      BackFrameColor1 = 16633762
      BackFrameColor2 = 16634540
      BackFillColor = 16635571
      ShadowColor = clGray
    end
    object LabelValue0: TLabel
      Left = 315
      Top = 119
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Anchors = [akRight, akBottom]
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelValue1: TLabel
      Left = 315
      Top = 138
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Anchors = [akRight, akBottom]
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelValue2: TLabel
      Left = 315
      Top = 150
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Anchors = [akRight, akBottom]
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelValue3: TLabel
      Left = 315
      Top = 169
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Anchors = [akRight, akBottom]
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName0: TLabel
      Left = 11
      Top = 121
      Width = 6
      Height = 13
      Anchors = [akLeft, akBottom]
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName1: TLabel
      Left = 11
      Top = 137
      Width = 3
      Height = 13
      Anchors = [akLeft, akBottom]
      BiDiMode = bdLeftToRight
      Caption = ' '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName2: TLabel
      Left = 11
      Top = 153
      Width = 6
      Height = 13
      Anchors = [akLeft, akBottom]
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName3: TLabel
      Left = 11
      Top = 169
      Width = 9
      Height = 13
      Anchors = [akLeft, akBottom]
      BiDiMode = bdLeftToRight
      Caption = '   '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName4: TLabel
      Left = 11
      Top = 185
      Width = 9
      Height = 13
      Anchors = [akLeft, akBottom]
      BiDiMode = bdLeftToRight
      Caption = '   '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelValue4: TLabel
      Left = 315
      Top = 185
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Anchors = [akRight, akBottom]
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object Memo1: TMemo
      Left = 8
      Top = 8
      Width = 313
      Height = 105
      Anchors = [akLeft, akTop, akRight, akBottom]
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object GridPanel1: TGridPanel
      Left = 8
      Top = 222
      Width = 313
      Height = 20
      Anchors = [akLeft, akRight, akBottom]
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 25.000000000000000000
        end
        item
          Value = 25.000000000000000000
        end
        item
          Value = 25.000000000000000000
        end
        item
          Value = 25.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = Button3
          Row = 0
        end
        item
          Column = 1
          Control = ButtonSave
          Row = 0
        end
        item
          Column = 2
          Control = Button1
          Row = 0
        end
        item
          Column = 3
          Control = Button2
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      DesignSize = (
        313
        20)
      object Button3: TButton
        Left = 0
        Top = 0
        Width = 75
        Height = 20
        Caption = 'Minimize'
        TabOrder = 0
        OnClick = Button3Click
      end
      object ButtonSave: TButton
        Left = 78
        Top = 0
        Width = 75
        Height = 20
        Hint = 'Save current download'
        Caption = 'Save'
        TabOrder = 1
        OnClick = ButtonSaveClick
      end
      object Button1: TButton
        Left = 159
        Top = 0
        Width = 75
        Height = 20
        Anchors = [akTop, akRight]
        Caption = 'Abort'
        TabOrder = 2
        OnClick = Button1Click
        ExplicitLeft = 175
      end
      object Button2: TButton
        Left = 238
        Top = 0
        Width = 75
        Height = 20
        Anchors = [akTop, akRight]
        Caption = 'Quit'
        TabOrder = 3
        OnClick = Button2Click
        ExplicitLeft = 244
      end
    end
  end
  object SaveSessionDialog: TSaveDialog
    DefaultExt = '*.sls'
    Filter = 'Download session (*.sls)|*.sls'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 280
    Top = 184
  end
  object UpdateTimer: TTimer
    OnTimer = UpdateTimerTimer
    Left = 240
    Top = 184
  end
end
