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

unit u_ThreadMapCombineBMP;

interface

uses
  SysUtils,
  Classes,
  Types,
  t_Bitmap32,
  i_NotifierOperation,
  i_BitmapTileProvider,
  i_RegionProcessProgressInfo,
  i_ProjectionInfo,
  i_GeometryLonLat,
  i_MapCalibration,
  u_ThreadMapCombineBase,
  LibBMP;

type
  TThreadMapCombineBMP = class(TThreadMapCombineBase)
  protected
    procedure SaveRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AImageProvider: IBitmapTileProvider;
      const AMapRect: TRect
    ); override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolygon: IGeometryLonLatPolygon;
      const AMapRect: TRect;
      const AImageProvider: IBitmapTileProvider;
      const AMapCalibrationList: IMapCalibrationList;
      const AFileName: string;
      const ASplitCount: TPoint
    );
  end;

implementation

uses
  gnugettext,
  i_ImageLineProvider,
  u_ImageLineProvider,
  u_GeoFunc,
  u_ResStrings;

constructor TThreadMapCombineBMP.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: IGeometryLonLatPolygon;
  const AMapRect: TRect;
  const AImageProvider: IBitmapTileProvider;
  const AMapCalibrationList: IMapCalibrationList;
  const AFileName: string;
  const ASplitCount: TPoint
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AMapRect,
    AImageProvider,
    AMapCalibrationList,
    AFileName,
    ASplitCount,
    Self.ClassName
  );
end;

procedure TThreadMapCombineBMP.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
const
  BMP_MAX_WIDTH = 32768;
  BMP_MAX_HEIGHT = 32768;
var
  i: Integer;
  VBMP: TBitmapFile;
  VLineBGR: Pointer;
  VSize: TPoint;
  VLineProvider: IImageLineProvider;
begin
  VSize := RectSize(AMapRect);

  if (VSize.X >= BMP_MAX_WIDTH) or (VSize.Y >= BMP_MAX_HEIGHT) then begin
    raise Exception.CreateFmt(SAS_ERR_ImageIsTooBig, ['BMP', VSize.X, BMP_MAX_WIDTH, VSize.Y, BMP_MAX_HEIGHT, 'BMP']);
  end;

  VBMP := TBitmapFile.Create(AFileName, VSize.X, VSize.Y);
  try
    VLineProvider :=
      TImageLineProviderBGR.Create(
        AImageProvider,
        AMapRect
      );
    for i := 0 to VSize.Y - 1 do begin
      VLineBGR := VLineProvider.GetLine(AOperationID, ACancelNotifier, i);
      if VLineBGR <> nil then begin
        if not VBMP.WriteLine(i, VLineBGR) then begin
          raise Exception.Create(_('BMP: Line write failure!'));
        end;
      end else begin
        raise Exception.Create(_('BMP: Fill line failure!'));
      end;

      if CancelNotifier.IsOperationCanceled(OperationID) then begin
        Break;
      end;
      if i mod 256 = 0 then begin
        ProgressFormUpdateOnProgress(i / VSize.Y);
      end;
    end;
  finally
    VBMP.Free;
  end;
end;

end.
