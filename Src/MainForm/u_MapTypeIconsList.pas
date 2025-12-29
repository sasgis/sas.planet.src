{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MapTypeIconsList;

interface

{$DEFINE USE_FREE_IMAGE_RESAMPLER}
{.$DEFINE ENABLE_RESAMPLING_DIAGNOSTIC}

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
    class procedure ResampleBitmap(
      const ASrc: IBitmap32Static;
      const ADest: TCustomBitmap32;
      const ADestWidth, ADestHeight: Integer
    ); static;
  private
    { IMapTypeIconsList }
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
  {$IFDEF ENABLE_RESAMPLING_DIAGNOSTIC}
  Diagnostics,
  u_DebugLogger,
  {$ENDIF}
  SysUtils,
  {$IFDEF USE_FREE_IMAGE_RESAMPLER}
  FreeImage,
  u_BitmapFuncFreeImage,
  u_GlobalDllName,
  {$ELSE}
  GR32_Resamplers,
  {$ENDIF}
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
begin
  VDib32 := TDIB32.Create;
  try
    VValidBitmap := TCustomBitmap32.Create;
    try
      if (ABmp.Size.X = FImageList.Width) and (ABmp.Size.Y = FImageList.Height) then begin
        AssignStaticToBitmap32(VValidBitmap, ABmp);
      end else begin
        {$IFDEF ENABLE_RESAMPLING_DIAGNOSTIC}
        var VTimer := TStopwatch.StartNew;
        {$ENDIF}
        ResampleBitmap(ABmp, VValidBitmap, FImageList.Width, FImageList.Height);
        {$IFDEF ENABLE_RESAMPLING_DIAGNOSTIC}
        VTimer.Stop;
        GLog.Write(Self, 'ResampleBitmap: %d ticks', [VTimer.ElapsedTicks]);
        {$ENDIF}
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
  {$IFDEF USE_FREE_IMAGE_RESAMPLER}
  InitFreeImageLib(GDllName.FreeImage);
  {$ENDIF}
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

{$IFDEF USE_FREE_IMAGE_RESAMPLER}
class procedure TMapTypeIconsList.ResampleBitmap(
  const ASrc: IBitmap32Static;
  const ADest: TCustomBitmap32;
  const ADestWidth, ADestHeight: Integer
);
const
  CFilter: FREE_IMAGE_FILTER = FILTER_LANCZOS3;
var
  VSrc, VDest: PFIBITMAP;
begin
  VSrc := Bitmap32StaticToFiBitmap(ASrc);
  if not Assigned(VSrc) then begin
    ADest.SetSize(0, 0);
    Assert(False);
    Exit;
  end;
  try
    VDest := FreeImage_Rescale(VSrc, ADestWidth, ADestHeight, CFilter);
    if not Assigned(VDest) then begin
      ADest.SetSize(0, 0);
      Assert(False);
      Exit;
    end;
    try
      FiBitmapToBitmap32(VDest, ADest);
    finally
      FreeImage_Unload(VDest);
    end;
  finally
    FreeImage_Unload(VSrc);
  end;
end;
{$ELSE}
class procedure TMapTypeIconsList.ResampleBitmap(
  const ASrc: IBitmap32Static;
  const ADest: TCustomBitmap32;
  const ADestWidth, ADestHeight: Integer
);
var
  VResampler: TCustomResampler;
begin
  ADest.SetSize(ADestWidth, ADestHeight);

  //VResampler := TKernelResampler.Create;
  //TKernelResampler(VResampler).Kernel := TLanczosKernel.Create;

  VResampler := TLinearResampler.Create;
  try
    StretchTransferFull(
      ADest,
      ADest.BoundsRect,
      ASrc,
      VResampler,
      dmOpaque
    );
  finally
    VResampler.Free;
  end;
end;
{$ENDIF}

end.
