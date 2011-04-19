object frmSearchResults: TfrmSearchResults
  Left = 646
  Top = 333
  Caption = 'Search results'
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
        Caption = 'Название'
      end
      item
        Alignment = taCenter
        Caption = 'Широта'
      end
      item
        Alignment = taCenter
        Caption = 'Долгота'
      end>
    GridLines = True
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lvResultsDblClick
    OnKeyDown = lvResultsKeyDown
  end
end
