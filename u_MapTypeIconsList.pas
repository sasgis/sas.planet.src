{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_MapTypeIconsList;

interface

uses
  ActiveX,
  ImgList,
  TBXGraphics,
  GR32,
  i_Bitmap32Static,
  i_GUIDSet,
  i_MapTypeIconsList,
  u_BaseInterfacedObject;

type
  TMapTypeIconsList = class(TBaseInterfacedObject, IMapTypeIconsList)
  private
    FList: IGUIDObjectSet;
    FImageList: TTBXImageList;
    function GetImageList: TCustomImageList;
    function GetIconIndexByGUID(const AGUID: TGUID): Integer;
    function GetIterator: IEnumGUID;
  public
    procedure Add(
      const AGUID: TGUID;
      const ABmp: IBitmap32Static
    );
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Resamplers,
  u_BitmapFunc,
  u_GUIDObjectSet;

{ TMapTypeIconsList }

procedure TMapTypeIconsList.Add(
  const AGUID: TGUID;
  const ABmp: IBitmap32Static
);
var
  VIndex: Integer;
  VDib32: TDIB32;
  VPixelData32: TPixelData32;
  VValidBitmap: TCustomBitmap32;
  VResampler: TCustomResampler;
begin
  VDib32 := TDIB32.Create;
  try
    VValidBitmap := TCustomBitmap32.Create;
    try
      if (ABmp.Size.X = FImageList.Width) and (ABmp.Size.Y = FImageList.Height) then begin
        AssignStaticToBitmap32(VValidBitmap, ABmp);
      end else begin
        VResampler := TLinearResampler.Create;
        try
          VValidBitmap.SetSize(FImageList.Width, FImageList.Height);
          StretchTransferFull(
            VValidBitmap,
            VValidBitmap.BoundsRect,
            ABmp,
            VResampler,
            dmOpaque
          );
        finally
          VResampler.Free;
        end;
      end;
      VDib32.SetSize(VValidBitmap.Width, VValidBitmap.Height);
      VPixelData32.Bits := PRGBQuad(VValidBitmap.Bits);
      VPixelData32.ContentRect := VValidBitmap.BoundsRect;
      VPixelData32.RowStride := VValidBitmap.Width;
      VDib32.CopyFrom(VPixelData32, 0, 0, VValidBitmap.BoundsRect);
    finally
      VValidBitmap.Free;
    end;
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
  inherited Create;
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

function TMapTypeIconsList.GetIconIndexByGUID(const AGUID: TGUID): Integer;
begin
  Result := Integer(FList.GetByGUID(AGUID)) - 1;
end;

end.
