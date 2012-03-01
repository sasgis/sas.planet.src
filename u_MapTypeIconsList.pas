{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MapTypeIconsList;

interface

uses
  Windows,
  ActiveX,
  Graphics,
  ImgList,
  TBXGraphics,
  GR32,
  i_Bitmap32Static,
  i_GUIDSet,
  i_MapTypeIconsList;

type
  TMapTypeIconsList = class(TInterfacedObject, IMapTypeIconsList)
  private
    FList: IGUIDObjectSet;
    FImageList: TTBXImageList;
    function GetImageList: TCustomImageList;
    function GetIconIndexByGUID(AGUID: TGUID): Integer;
    function GetIterator: IEnumGUID;
  public
    procedure Add(AGUID: TGUID; ABmp: IBitmap32Static);
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_GUIDObjectSet;

{ TMapTypeIconsList }

procedure TMapTypeIconsList.Add(AGUID: TGUID; ABmp: IBitmap32Static);
var
  VIndex: Integer;
  VDib32: TDIB32;
  VPixelData32: TPixelData32;
begin
  VDib32 := TDIB32.Create;
  try
    VDib32.SetSize(ABmp.Bitmap.Width, ABmp.Bitmap.Height);
    VPixelData32.Bits := PRGBQuad(ABmp.Bitmap.Bits);
    VPixelData32.ContentRect := ABmp.Bitmap.BoundsRect;
    VPixelData32.RowStride := ABmp.Bitmap.Width;
    VDib32.CopyFrom(VPixelData32, 0, 0, ABmp.Bitmap.BoundsRect);
    VIndex := GetIconIndexByGUID(AGUID);
    if VIndex < 0 then begin
      VIndex := FImageList.Add(VDib32);
      FList.Add(AGUID, Pointer(VIndex + 1));
    end;
  finally
    VDib32.Free;
  end;
end;

constructor TMapTypeIconsList.Create(AWidth, AHeight: Integer);
begin
  FImageList := TTBXImageList.Create(nil);
  FImageList.Width := AWidth;
  FImageList.Height := AHeight;
  FList := TGUIDObjectSet.Create(True);
end;

destructor TMapTypeIconsList.Destroy;
begin
  FreeAndNil(FImageList);
  FList := nil;
  inherited;
end;

function TMapTypeIconsList.GetImageList: TCustomImageList;
begin
  Result := FImageList;
end;

function TMapTypeIconsList.GetIterator: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

function TMapTypeIconsList.GetIconIndexByGUID(AGUID: TGUID): Integer;
begin
  Result := Integer(Flist.GetByGUID(AGUID)) - 1;
end;

end.
