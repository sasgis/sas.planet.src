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
    DesignSize = (
      328
      248)
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
      Width = 112
      Height = 13
      Anchors = [akLeft, akBottom]
      BiDiMode = bdLeftToRight
      Caption = 'Process not more than:'
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName1: TLabel
      Left = 11
      Top = 137
      Width = 78
      Height = 13
      Anchors = [akLeft, akBottom]
      BiDiMode = bdLeftToRight
      Caption = 'Processed total:'
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName2: TLabel
      Left = 11
      Top = 153
      Width = 88
      Height = 13
      Anchors = [akLeft, akBottom]
      BiDiMode = bdLeftToRight
      Caption = 'Downloaded total:'
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName3: TLabel
      Left = 11
      Top = 169
      Width = 75
      Height = 13
      Anchors = [akLeft, akBottom]
      BiDiMode = bdLeftToRight
      Caption = 'Time remaining:'
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName4: TLabel
      Left = 11
      Top = 185
      Width = 105
      Height = 13
      Anchors = [akLeft, akBottom]
      BiDiMode = bdLeftToRight
      Caption = 'Approx. to download:'
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
        Hint = 'Save current session'
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
        Caption = 'Pause'
        TabOrder = 2
        OnClick = Button1Click
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
