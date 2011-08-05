object frmSearchResults: TfrmSearchResults
  Left = 646
  Top = 333
  Caption = 'Search Results'
  ClientHeight = 462
  ClientWidth = 474
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
    Width = 474
    Height = 462
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        Caption = 'Distance to'
        Width = 90
      end
      item
        Caption = 'Coordinates'
        Width = 130
      end>
    GridLines = True
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
    OnCompare = lvResultsCompare
    OnDblClick = lvResultsDblClick
    OnKeyDown = lvResultsKeyDown
    ExplicitWidth = 501
  end
end
