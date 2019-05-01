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

unit u_BitmapMapCombinerKMZ;

interface

uses
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoadFactory,
  i_GeometryLonLat,
  i_ArchiveReadWriteFactory,
  i_BitmapMapCombiner,
  i_RegionProcessParamsFrame,
  u_BitmapMapCombinerFactoryBase;

type
  TBitmapMapCombinerFactoryKMZ = class(TBitmapMapCombinerFactoryBase)
  private
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
  protected
    function PrepareMapCombiner(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const AProgressInfo: IBitmapCombineProgressUpdate
    ): IBitmapMapCombiner; override;
    function Validate(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const APolygon: IGeometryLonLatPolygon
    ): Boolean; override;
  public
    constructor Create(
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const ABitmapFactory: IBitmap32StaticFactory
    );
  end;

implementation

uses
  SysUtils,
  Classes,
  Types,
  Dialogs,
  Math,
  GR32,
  gnugettext,
  ALString,
  t_GeoTypes,
  t_CommonTypes,
  t_MapCombineOptions,
  i_BinaryData,
  i_BitmapTileSaveLoad,
  i_ArchiveReadWrite,
  i_Projection,
  i_NotifierOperation,
  i_BitmapTileProvider,
  i_Bitmap32Static,
  u_BaseInterfacedObject,
  u_TileIteratorByRect,
  u_BinaryDataByMemStream,
  u_Bitmap32ByStaticBitmap,
  u_BitmapFunc,
  u_ResStrings,
  u_GeoFunc,
  u_GeoToStrFunc;

type
  TBitmapMapCombinerKMZ = class(TBaseInterfacedObject, IBitmapMapCombiner)
  private
    FProgressUpdate: IBitmapCombineProgressUpdate;
    FQuality: Integer;
    FBitmapFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AImageProvider: IBitmapTileProvider;
      const AMapRect: TRect
    ): IBitmap32Static;
  private
    procedure SaveRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AImageProvider: IBitmapTileProvider;
      const AMapRect: TRect
    );
  public
    constructor Create(
      const AProgressUpdate: IBitmapCombineProgressUpdate;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      AQuality: Integer
    );
  end;

constructor TBitmapMapCombinerKMZ.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  AQuality: Integer
);
begin
  inherited Create;
  FProgressUpdate := AProgressUpdate;
  FQuality := AQuality;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FBitmapFactory := ABitmapFactory;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

function TBitmapMapCombinerKMZ.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
): IBitmap32Static;
var
  VTileRect: TRect;
  VProjection: IProjection;
  VIterator: TTileIteratorByRectRecord;
  VTile: TPoint;
  VBitmap: TBitmap32ByStaticBitmap;
  VMapSize: TPoint;
  VTileBitmap: IBitmap32Static;
  VTileMapPixelRect: TRect;
  VCopyRect: TRect;
  VCopyPos: TPoint;
begin
  Result := nil;
  if not Types.IsRectEmpty(AMapRect) then begin
    VProjection := AImageProvider.Projection;
    VMapSize := RectSize(AMapRect);
    VTileRect := VProjection.PixelRect2TileRect(AMapRect);

    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
    try
      VIterator.Init(VTileRect);
      while VIterator.Next(VTile) do begin
        VTileBitmap :=
          AImageProvider.GetTile(
            AOperationID,
            ACancelNotifier,
            VTile
          );
        if Assigned(VTileBitmap) then begin
          if VBitmap.Empty then begin
            VBitmap.SetSize(VMapSize.X, VMapSize.Y);
            VBitmap.Clear(0);
          end;
          VTileMapPixelRect := VProjection.TilePos2PixelRect(VTile);
          Types.IntersectRect(VCopyRect, AMapRect, VTileMapPixelRect);
          VCopyPos := PointMove(VCopyRect.TopLeft, AMapRect.TopLeft);
          BlockTransfer(
            VBitmap,
            VCopyPos.X,
            VCopyPos.Y,
            VTileBitmap,
            RectMove(VCopyRect, VTileMapPixelRect.TopLeft),
            dmOpaque
          );
        end;
      end;
      Result := VBitmap.MakeAndClear;
    finally
      VBitmap.Free;
    end;
  end;
end;

procedure TBitmapMapCombinerKMZ.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
var
  iWidth, iHeight: integer;
  i, j: integer;
  VFileName: AnsiString;
  kmlm: TMemoryStream;
  VLLRect: TDoubleRect;
  VStr: UTF8String;
  VNameInKmz: AnsiString;
  nim: TPoint;
  VZip: IArchiveWriter;
  VPixelRect: TRect;
  JPGSaver: IBitmapTileSaver;
  VKmzFileNameOnly: AnsiString;
  VCurrentPieceRect: TRect;
  VProjection: IProjection;
  VMapPieceSize: TPoint;
  VTilesProcessed: Integer;
  VTilesToProcess: Integer;
  VBitmapTile: IBitmap32Static;
  VData: IBinaryData;
begin
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;

  VProjection := AImageProvider.Projection;
  VCurrentPieceRect := AMapRect;
  VMapPieceSize := RectSize(VCurrentPieceRect);
  nim.X := ((VMapPieceSize.X - 1) div 1024) + 1;
  nim.Y := ((VMapPieceSize.Y - 1) div 1024) + 1;
  VTilesProcessed := 0;
  VTilesToProcess := nim.X * nim.Y;
  iWidth := VMapPieceSize.X div (nim.X);
  iHeight := VMapPieceSize.y div (nim.Y);

  JPGSaver := FBitmapTileSaveLoadFactory.CreateJpegSaver(FQuality);
  VZip := FArchiveReadWriteFactory.Zip.WriterFactory.BuildByFileName(AFileName);
  VKmzFileNameOnly := 'part_';
  kmlm := TMemoryStream.Create;
  try
    VStr := ansiToUTF8('<?xml version="1.0" encoding="UTF-8"?>' + #13#10 + '<kml xmlns="http://earth.google.com/kml/2.2">' + #13#10 + '<Folder>' + #13#10 + '<name>' + VKmzFileNameOnly + '</name>' + #13#10);
    for i := 1 to nim.X do begin
      for j := 1 to nim.Y do begin
        if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
          break;
        end;
        VPixelRect.Left := VCurrentPieceRect.Left + iWidth * (i - 1);
        VPixelRect.Right := VCurrentPieceRect.Left + iWidth * i;
        VPixelRect.Top := VCurrentPieceRect.Top + iHeight * (j - 1);
        VPixelRect.Bottom := VCurrentPieceRect.Top + iHeight * j;
        VBitmapTile :=
          GetBitmapRect(
            AOperationID,
            ACancelNotifier,
            AImageProvider,
            VPixelRect
          );
        if VBitmapTile <> nil then begin
          if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
            break;
          end;
          VData := JPGSaver.Save(VBitmapTile);

          if VData <> nil then begin
            VFileName := VKmzFileNameOnly + ALIntToStr(i) + '_' + ALIntToStr(j) + '.jpg';
            VNameInKmz := 'files/' + VFileName;
            VStr := VStr + ansiToUTF8('<GroundOverlay>' + #13#10 + '<name>' + VFileName + '</name>' + #13#10 + '<drawOrder>75</drawOrder>' + #13#10);
            VStr := VStr + ansiToUTF8('<Icon><href>' + VNameInKmz + '</href>' + '<viewBoundScale>0.75</viewBoundScale></Icon>' + #13#10);
            VLLRect := VProjection.PixelRect2LonLatRect(VPixelRect);
            VStr := VStr + ansiToUTF8('<LatLonBox>' + #13#10);
            VStr := VStr + ansiToUTF8('<north>' + R2StrPoint(VLLRect.Top) + '</north>' + #13#10);
            VStr := VStr + ansiToUTF8('<south>' + R2StrPoint(VLLRect.Bottom) + '</south>' + #13#10);
            VStr := VStr + ansiToUTF8('<east>' + R2StrPoint(VLLRect.Right) + '</east>' + #13#10);
            VStr := VStr + ansiToUTF8('<west>' + R2StrPoint(VLLRect.Left) + '</west>' + #13#10);
            VStr := VStr + ansiToUTF8('</LatLonBox>' + #13#10 + '</GroundOverlay>' + #13#10);

            VZip.AddFile(VData, VNameInKmz, Now);
          end;
        end;
        Inc(VTilesProcessed);
        FProgressUpdate.Update(VTilesProcessed / VTilesToProcess);
      end;
    end;
    VStr := VStr + ansiToUTF8('</Folder>' + #13#10 + '</kml>');
    kmlm.Write(VStr[1], length(VStr));
    kmlm.Position := 0;

    VData := TBinaryDataByMemStream.CreateFromStream(kmlm);
    VZip.AddFile(VData, 'doc.kml', Now);
  finally
    kmlm.Free;
  end;
end;

{ TBitmapMapCombinerFactoryKMZ }

constructor TBitmapMapCombinerFactoryKMZ.Create(
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const ABitmapFactory: IBitmap32StaticFactory
);
begin
  inherited Create(
    Types.Point(0, 0),
    Types.Point(10240, 10240),
    stsUnicode,
    'kmz',
    gettext_NoExtract('KMZ for Garmin (JPEG Overlays)'),
    [mcQuality]
  );
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FBitmapFactory := ABitmapFactory;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

function TBitmapMapCombinerFactoryKMZ.PrepareMapCombiner(
  const AParams: IRegionProcessParamsFrameMapCombine;
  const AProgressInfo: IBitmapCombineProgressUpdate
): IBitmapMapCombiner;
begin
  Result :=
    TBitmapMapCombinerKMZ.Create(
      AProgressInfo,
      FBitmapFactory,
      FBitmapTileSaveLoadFactory,
      FArchiveReadWriteFactory,
      AParams.CustomOptions.Quality
    );
end;

function TBitmapMapCombinerFactoryKMZ.Validate(
  const AParams: IRegionProcessParamsFrameMapCombine;
  const APolygon: IGeometryLonLatPolygon
): Boolean;
var
  VSplitCount: TPoint;
  VProjection: IProjection;
  VLonLatRect: TDoubleRect;
  VPixelRect: TRect;
  VPixelSize: TPoint;
  VKmzImgesCount: TPoint;
begin
  Result := inherited Validate(AParams, APolygon);
  if not Result then begin
    Exit;
  end;

  if not Assigned(APolygon) then begin
    Assert(False, _('Polygon isn''t selected'));
    Result := False;
    Exit;
  end;
  VSplitCount := AParams.SplitCount;
  VProjection := (AParams as IRegionProcessParamsFrameTargetProjection).Projection;
  if not Assigned(VProjection) then begin
    Assert(False, _('Projection isn''t selected'));
    Result := False;
    Exit;
  end;
  VLonLatRect := APolygon.Bounds.Rect;
  VProjection.ProjectionType.ValidateLonLatRect(VLonLatRect);
  VPixelRect :=
    RectFromDoubleRect(
      VProjection.LonLatRect2PixelRectFloat(VLonLatRect),
      rrOutside
    );
  VPixelSize := RectSize(VPixelRect);
  VPixelSize.X := Trunc(VPixelSize.X / VSplitCount.X);
  VPixelSize.Y := Trunc(VPixelSize.Y / VSplitCount.Y);

  VKmzImgesCount.X := ((VPixelSize.X - 1) div 1024) + 1;
  VKmzImgesCount.Y := ((VPixelSize.Y - 1) div 1024) + 1;
  if ((VKmzImgesCount.X * VKmzImgesCount.Y) > 100) then begin
    ShowMessage(SAS_MSG_GarminMax1Mp);
  end;

  Result := True;
end;

end.
