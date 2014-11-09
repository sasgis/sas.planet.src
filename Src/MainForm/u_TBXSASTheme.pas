unit u_TBXSASTheme;

interface

uses
 TBXDefaultTheme, windows, Messages, Graphics, TBXThemes, ImgList, CommCtrl;

type
  TTBXSASTheme = class(TTBXDefaultTheme)
  private
  protected
  public
    procedure PaintImage(DC: HDC; ARect: TRect; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer); override;
  end;

implementation

uses
  Classes, Controls, TBXUtils, TBXGraphics, TBXUxThemes, TB2Common, TB2Item, TBX, Forms;

procedure TTBXSASTheme.PaintImage(DC: HDC; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
var
  BgColor: TColor;
  HiContrast: Boolean;
  IsMenuItem: Boolean;
  C: TCanvas;
begin
  with ItemInfo do
  begin
    if ImageList is TTBCustomImageList then
    begin
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      SaveDC(DC);
      try
        C := TCanvas.Create;
        C.Handle := DC;
        TTBCustomImageList(ImageList).DrawState(C, ARect.Left, ARect.Top,
          ImageIndex, Enabled, (HoverKind <> hkNone), Selected);
        C.Handle := 0;
        C.Free;
      finally
        RestoreDC(DC, -1);
      end;
      Exit;
    end;

    IsMenuItem := (ViewType and VT_TYPEMASK = VT_POPUPMENU) and (ItemOptions and IO_TOOLBARSTYLE = 0);

    if (IsMenuItem and USE_FLATMENUS) or (not IsMenuItem and USE_THEMES) then
    begin
    { The icon painting here is not really made by the uxtheme.dll, this is
      just a simulation until I figure out how to work with DrawThemedIcon function }
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      BgColor := GetItemImageBackground(ItemInfo);
      HiContrast := not IsMenuItem and IsDarkColor(BGColor);
      if not Enabled then
        DrawTBXIconShadow(DC, ARect, ImageList, ImageIndex, 0)
      else if Selected or Pushed or (HoverKind <> hkNone) then
        DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast)
      else if HiContrast or TBXHiContrast or TBXLoColor then
        DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast)
      else
        HighlightTBXIcon(DC, ARect, ImageList, ImageIndex, clWindow, 255);
    end
    else
    begin
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      SaveDC(DC);
      try
        C := TCanvas.Create;
        C.Handle := DC;
        ImageList.Draw(C, ARect.Left, ARect.Top, ImageIndex, Enabled);
        C.Handle := 0;
        C.Free;
      finally
        RestoreDC(DC, -1);
      end;
    end;
  end;
end;

initialization
  RegisterTBXTheme('SAStbxTheme', TTBXSASTheme);
end.
