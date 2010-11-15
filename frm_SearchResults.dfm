object frmSearchResults: TfrmSearchResults
  Left = 646
  Top = 333
  Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090#1099' '#1087#1086#1080#1089#1082#1072
  ClientHeight = 462
  ClientWidth = 218
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object lvResults: TListView
    Left = 0
    Top = 0
    Width = 218
    Height = 462
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
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lvResultsDblClick
    OnKeyDown = lvResultsKeyDown
  end
end
