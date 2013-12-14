object frmIntrnalBrowser: TfrmIntrnalBrowser
  Left = 444
  Top = 208
  BorderStyle = bsSizeToolWin
  ClientHeight = 306
  ClientWidth = 554
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 0
    Width = 554
    Height = 306
    Align = alClient
    TabOrder = 0
    Silent = False
    OnTitleChange = EmbeddedWB1TitleChange
    OnBeforeNavigate2 = EmbeddedWB1BeforeNavigate2
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    OnAuthenticate = EmbeddedWB1Authenticate
    About = ' EmbeddedWB http://bsalsa.com/'
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    OnKeyDown = EmbeddedWB1KeyDown
    ControlData = {
      4C000000D8380000DE2000000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object imgViewImage: TImgView32
    Left = 0
    Top = 0
    Width = 554
    Height = 306
    Cursor = crHandPoint
    Align = alClient
    AutoSize = True
    Bitmap.ResamplerClassName = 'TLinearResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smResize
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 16
    ScrollBars.Visibility = svHidden
    OverSize = 0
    TabOrder = 1
    Visible = False
    OnClick = imgViewImageClick
  end
end
