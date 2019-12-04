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
      Canvas: TCanvas;
      ARect: TRect;
      const ItemInfo: TTBXItemInfo;
      ImageList: TCustomImageList;
      ImageIndex: Integer
    ); override;
  end;

{ TTBXSASTheme }

procedure TTBXSASTheme.PaintImage(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
var
  BgColor: TColor;
  HiContrast: Boolean;
  IsMenuItem: Boolean;
begin
  with ItemInfo do
  begin
    if ImageList is TTBCustomImageList then
    begin
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      TTBCustomImageList(ImageList).DrawState(Canvas, ARect.Left, ARect.Top,
        ImageIndex, Enabled, (HoverKind <> hkNone), Selected);
      Exit;
    end;

    IsMenuItem := ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) and
      ((ItemOptions and IO_TOOLBARSTYLE) = 0);

    if (IsMenuItem and USE_FLATMENUS) or (not IsMenuItem and USE_THEMES) then
    begin
    { The icon painting here is not really made by the uxtheme.dll, this is
      just a simulation until I figure out how to work with DrawThemedIcon function }
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      BgColor := GetItemImageBackground(ItemInfo);
      HiContrast := not IsMenuItem and IsDarkColor(BGColor);
      if not Enabled then
        DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 0)
      else if Selected or Pushed or (HoverKind <> hkNone) then
        DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
      else if HiContrast or TBXHiContrast or TBXLoColor then
        DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
      else begin
        // 2019-11-23, zed:
        // The only difference this function from original is the next line of code,
        // which provide a fix to draw full-color icons instead of semi-transparent
        // (original value 178 replaced with 255)
        HighlightTBXIcon(Canvas, ARect, ImageList, ImageIndex, clWindow, 255);
      end;
    end
    else
    begin
      if Pushed or Selected then OffsetRect(ARect, 1, 1);
      ImageList.Draw(Canvas, ARect.Left, ARect.Top, ImageIndex, Enabled);
    end;
  end;
end;

initialization
  RegisterTBXTheme(CTTBSASThemeName, TTBXSASTheme);

end.
