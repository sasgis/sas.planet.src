object frSelectedPicture: TfrSelectedPicture
  Left = 0
  Top = 0
  Width = 53
  Height = 53
  Align = alClient
  TabOrder = 0
  object imgIcon: TImage32
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 47
    Height = 47
    Align = alClient
    Bitmap.CombineMode = cmMerge
    Bitmap.ResamplerClassName = 'TLinearResampler'
    BitmapAlign = baCenter
    Color = clBtnFace
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnMouseDown = imgIconMouseDown
    OnResize = imgIconResize
  end
end
