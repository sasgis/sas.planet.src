{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_TBXSASTheme;

interface

const
  CTTBSASThemeName = 'TBXSASTheme';

implementation

uses
  Windows,
  ImgList,
  Graphics,
  TBXUtils,
  TB2Item,
  TBXThemes,
  TBXDefaultTheme;

type
  TTBXSASTheme = class(TTBXDefaultTheme)
  public
    procedure PaintImage(
      DC: HDC;
      ARect: TRect;
      const ItemInfo: TTBXItemInfo;
      ImageList: TCustomImageList;
      ImageIndex: Integer
    ); override;
  end;

{ TTBXSASTheme }

procedure TTBXSASTheme.PaintImage(
  DC: HDC;
  ARect: TRect;
  const ItemInfo: TTBXItemInfo;
  ImageList: TCustomImageList;
  ImageIndex: Integer
);
var
  BgColor: TColor;
  HiContrast: Boolean;
  IsMenuItem: Boolean;
  C: TCanvas;
begin
  with ItemInfo do begin
    if ImageList is TTBCustomImageList then begin
      if Pushed or Selected then begin
        OffsetRect(ARect, 1, 1);
      end;
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

    if (IsMenuItem and USE_FLATMENUS) or (not IsMenuItem and USE_THEMES) then begin
    { The icon painting here is not really made by the uxtheme.dll, this is
      just a simulation until I figure out how to work with DrawThemedIcon function }
      if Pushed or Selected then begin
        OffsetRect(ARect, 1, 1);
      end;
      BgColor := GetItemImageBackground(ItemInfo);
      HiContrast := not IsMenuItem and IsDarkColor(BGColor);
      if not Enabled then begin
        DrawTBXIconShadow(DC, ARect, ImageList, ImageIndex, 0);
      end else if Selected or Pushed or (HoverKind <> hkNone) then begin
        DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast);
      end else if HiContrast or TBXHiContrast or TBXLoColor then begin
        DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast);
      end else begin
        // 2019-11-23, zed:
        // The only difference this function from original is the next line of code,
        // which provide a fix to draw full-color icons instead of semi-transparent
        // (original value 178 replaced with 255)
        HighlightTBXIcon(DC, ARect, ImageList, ImageIndex, clWindow, 255);
      end;
    end else begin
      if Pushed or Selected then begin
        OffsetRect(ARect, 1, 1);
      end;
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
  RegisterTBXTheme(CTTBSASThemeName, TTBXSASTheme);

end.
