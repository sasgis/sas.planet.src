object frmSearchResults: TfrmSearchResults
  Left = 646
  Top = 333
  Width = 226
  Height = 489
  Caption = 'frmSearchResults'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object lvResults: TListView
    Left = 0
    Top = 0
    Width = 218
    Height = 455
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = #1053#1072#1079#1074#1072#1085#1080#1077
      end
      item
        Alignment = taCenter
        Caption = #1064#1080#1088#1086#1090#1072
      end
      item
        Alignment = taCenter
        Caption = #1044#1086#1083#1075#1086#1090#1072
      end>
    GridLines = True
    Items.Data = {
      210000000100000000000000FFFFFFFFFFFFFFFF000000000000000004CAE8E5
      E2}
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lvResultsDblClick
    OnKeyDown = lvResultsKeyDown
  end
end
