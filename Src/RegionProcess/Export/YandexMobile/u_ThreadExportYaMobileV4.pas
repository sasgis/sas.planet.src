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

unit u_ThreadExportYaMobileV4;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  YaMobileCache,
  i_BinaryData,
  i_Bitmap32BufferFactory,
  i_GeometryLonLat,
  i_NotifierOperation,
  i_BitmapTileSaveLoad,
  i_RegionProcessProgressInfo,
  i_BitmapLayerProvider,
  i_CoordConverterFactory,
  i_GeometryProjectedFactory,
  u_ThreadExportAbstract;

type
  TYaMobileV4TileSize = (yats128 = 0, yats256 = 1);

  TExportTaskYaMobileV4 = record
    FMapId: Integer;
    FMapName: string;
    FSaver: IBitmapTileSaver;
    FImageProvider: IBitmapTileUniProvider;
  end;
  TExportTaskYaMobileV4Array = array of TExportTaskYaMobileV4;

  TThreadExportYaMobileV4 = class(TThreadExportAbstract)
  private
    FTasks: TExportTaskYaMobileV4Array;
    FIsReplace: Boolean;
    FExportPath: string;
    FProjectionSetFactory: IProjectionSetFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FCacheFile: array [0..7] of TYaMobileCacheFile;
    FCacheCount: Byte;
    FTileSize: TYaMobileV4TileSize;
    procedure GenUserXml(const AMapID, AMapName: string);
    function OpenCacheFile(
      const ACachePath: string;
      out ACacheFile: TYaMobileCacheFile
    ): Boolean;
    procedure CloseCacheFiles;
    procedure AddTileToCache(
      const AData: IBinaryData;
      const ATilePoint: TPoint;
      AZoom: Byte;
      AMapID: Integer
    );
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const APath: string;
      const APolygon: IGeometryLonLatPolygon;
      const ATasks: TExportTaskYaMobileV4Array;
      const Azoomarr: TByteDynArray;
      const AReplace: boolean;
      const ATileSize: TYaMobileV4TileSize
    );
    destructor Destroy; override;
  end;

implementation

uses
  GR32,
  c_CoordConverter,
  i_ProjectionSet,
  i_Bitmap32Static,
  i_ProjectionInfo,
  i_GeometryProjected,
  i_TileIterator,
  u_BitmapFunc,
  u_ResStrings,
  u_TileIteratorByPolygon;

{ TThreadExportYaMobileV4 }

constructor TThreadExportYaMobileV4.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const APath: string;
  const APolygon: IGeometryLonLatPolygon;
  const ATasks: TExportTaskYaMobileV4Array;
  const Azoomarr: TByteDynArray;
  const AReplace: boolean;
  const ATileSize: TYaMobileV4TileSize
);
var
  i: integer;
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    Azoomarr,
    Self.ClassName
  );
  FTileSize := ATileSize;
  FProjectionSetFactory := AProjectionSetFactory;
  FProjectionFactory := AProjectionFactory;
  FBitmapFactory := ABitmapFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FTasks := ATasks;
  FExportPath := APath;
  FIsReplace := AReplace;

  for i := 0 to Length(FCacheFile) - 1 do begin
    FCacheFile[i] := nil;
  end;
  FCacheCount := 0;
end;

destructor TThreadExportYaMobileV4.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FTasks) - 1 do begin
    FTasks[i].FMapName := '';
    FTasks[i].FSaver := nil;
    FTasks[i].FImageProvider := nil;
  end;
  inherited;
end;

function TThreadExportYaMobileV4.OpenCacheFile(
  const ACachePath: string;
  out ACacheFile: TYaMobileCacheFile
): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(FCacheFile) - 1 do begin
    if Assigned(FCacheFile[I]) then begin
      if ACachePath = FCacheFile[I].FilePath then begin
        ACacheFile := FCacheFile[I];
        Result := True;
        Exit;
      end;
    end;
  end;
  if Assigned(FCacheFile[FCacheCount]) then begin
    FreeAndNil(FCacheFile[FCacheCount]);
  end;
  FCacheFile[FCacheCount] := TYaMobileCacheFile.Create(ACachePath, FIsReplace);
  FCacheCount := FCacheCount + 1;
  if FCacheCount > Length(FCacheFile) - 1 then begin
    FCacheCount := 0;
  end;
  Result := Assigned(ACacheFile);
end;

procedure TThreadExportYaMobileV4.CloseCacheFiles;
var
  I: Integer;
begin
  for I := 0 to Length(FCacheFile) - 1 do begin
    if Assigned(FCacheFile[I]) then begin
      FreeAndNil(FCacheFile[I]);
    end;
  end;
end;

procedure TThreadExportYaMobileV4.AddTileToCache(
  const AData: IBinaryData;
  const ATilePoint: TPoint;
  AZoom: Byte;
  AMapID: Integer
);
var
  VCacheFilePath: string;
  VCacheFile: TYaMobileCacheFile;
  VTileData: TTileStream;
begin
  if Assigned(AData) then begin
    VCacheFile := nil;
    VCacheFilePath := GetFilePath(FExportPath, ATilePoint, AZoom, AMapID);
    if OpenCacheFile(VCacheFilePath, VCacheFile) then begin
      VTileData.Data := AData;
      VTileData.Point := Types.Point(ATilePoint.X mod 128, ATilePoint.Y mod 128);
      VTileData.Zoom := AZoom;
      VTileData.MapVersion := 1;
      VCacheFile.AddTile(VTileData);
      VTileData.Data := nil;
    end;
  end;
end;

procedure TThreadExportYaMobileV4.GenUserXml(const AMapID, AMapName: string);
var
  VUserXml: string;
  VUserXmlAnsi: AnsiString;
  VUserXmlPath: string;
  VStream: TMemoryStream;
  VAddStr: string;
  BOM: array[0..2] of Byte;
  VSize: Integer;
  VSizeInPixelsStr: string;
begin
  VStream := TMemoryStream.Create;
  try
    VUserXmlPath := FExportPath + 'config' + PathDelim + 'user.xml';
    if ForceDirectories(ExtractFilePath(VUserXmlPath)) then begin
      case FTileSize of
        yats128: VSizeInPixelsStr := '128';
        yats256: VSizeInPixelsStr := '256';
      end;
      VAddStr := '    <l id="' + AMapID + '" request="" name="' + AMapName + '" service="0" size_in_pixels="' + VSizeInPixelsStr + '" ver="1" />' + #10 + '</map_layers>' + #10;
      if not FileExists(VUserXmlPath) then begin
        VUserXml := '<?xml version="1.0" encoding="utf-8" ?>' + #10 + '<map_layers>' + #10 + VAddStr;
      end else begin
        VStream.LoadFromFile(VUserXmlPath);
        VStream.Position := 0;
        VStream.Read(BOM[0], 3);
        if (BOM[0] = $EF) and (BOM[1] = $BB) and (BOM[2] = $BF) then begin
          VSize := VStream.Size - 3;
          VStream.Position := 3;
        end else begin
          VSize := VStream.Size;
          VStream.Position := 0;
        end;
        SetLength(VUserXmlAnsi, VSize);
        VStream.ReadBuffer(VUserXmlAnsi[1], Length(VUserXmlAnsi));
        VUserXml := Utf8ToAnsi(VUserXmlAnsi);
        VUserXml := StringReplace(VUserXml, '</map_layers>'#10, '', [rfIgnoreCase, rfReplaceAll]);
        if VUserXml <> '' then begin
          VUserXml := VUserXml + VAddStr;
        end;
      end;
      if VUserXml <> '' then begin
        VUserXmlAnsi := #239#187#191 + AnsiToUtf8(VUserXml);
        VStream.Clear;
        VStream.Position := 0;
        VStream.WriteBuffer(VUserXmlAnsi[1], Length(VUserXmlAnsi));
        VStream.SaveToFile(VUserXmlPath);
      end;
    end;
  finally
    FreeAndNil(VStream);
  end;
end;

procedure TThreadExportYaMobileV4.ProcessRegion;
var
  i, j, xi, yi, hxyi, sizeim: integer;
  VZoom: Byte;
  VBitmapTile: IBitmap32Static;
  bmp32crop: TCustomBitmap32;
  tc: cardinal;
  VProjectionSet: IProjectionSet;
  VTile: TPoint;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VProjection: IProjectionInfo;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VStaticBitmapCrop: IBitmap32Static;
  VDataToSave: IBinaryData;
begin
  inherited;
  hxyi := 1;
  sizeim := 128;
  bmp32crop := TCustomBitmap32.Create;
  try
    bmp32crop.Width := sizeim;
    bmp32crop.Height := sizeim;
    VProjectionSet := FProjectionSetFactory.GetProjectionSetByCode(CYandexProjectionEPSG, CTileSplitQuadrate256x256);
    VTilesToProcess := 0;
    SetLength(VTileIterators, Length(FZooms));

    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      VProjection := VProjectionSet.Zooms[VZoom];
      VProjectedPolygon :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
          VProjection,
          PolygLL
        );
      VTileIterators[i] := TTileIteratorByPolygon.Create(VProjection, VProjectedPolygon);
      VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal * Length(FTasks);
    end;
    try
      VTilesProcessed := 0;

      ProgressInfo.SetCaption(SAS_STR_ExportTiles);
      ProgressInfo.SetFirstLine(
        SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
      );
      ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);

      tc := GetTickCount;
      try
        for j := 0 to length(FTasks) - 1 do begin
          GenUserXml(IntToStr(FTasks[j].FMapId), FTasks[j].FMapName);
          for i := 0 to Length(FZooms) - 1 do begin
            VTileIterator := VTileIterators[i];
            VProjection := VTileIterator.TilesRect.ProjectionInfo;
            VZoom := VProjection.Zoom;
            VTileIterator.Reset;
            while VTileIterator.Next(VTile) do begin
              if CancelNotifier.IsOperationCanceled(OperationID) then begin
                exit;
              end;
              VBitmapTile :=
                FTasks[j].FImageProvider.GetTile(
                  OperationID,
                  CancelNotifier,
                  VProjection,
                  VTile
                );
              if VBitmapTile <> nil then begin
                case FTileSize of
                  yats256:
                    begin
                      VDataToSave := FTasks[j].FSaver.Save(VBitmapTile);
                      AddTileToCache(
                        VDataToSave,
                        VTile,
                        VZoom,
                        FTasks[j].FMapId
                      );
                    end;
                  yats128:
                    begin
                      for xi := 0 to hxyi do begin
                        for yi := 0 to hxyi do begin
                          bmp32crop.Clear;
                          BlockTransfer(
                            bmp32crop,
                            0,
                            0,
                            VBitmapTile,
                            bounds(sizeim * xi, sizeim * yi, sizeim, sizeim),
                            dmOpaque
                          );
                          VStaticBitmapCrop :=
                            FBitmapFactory.Build(
                              Point(sizeim, sizeim),
                              bmp32crop.Bits
                            );
                          VDataToSave := FTasks[j].FSaver.Save(VStaticBitmapCrop);
                          AddTileToCache(
                            VDataToSave,
                            Types.Point(2 * VTile.X + Xi, 2 * VTile.Y + Yi),
                            VZoom,
                            FTasks[j].FMapId
                          );
                        end;
                      end;
                    end;
                end;
              end;
              inc(VTilesProcessed);
              if (GetTickCount - tc > 1000) then begin
                tc := GetTickCount;
                ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
              end;
            end;
          end;
        end;
      finally
        CloseCacheFiles;
      end;
    finally
      for i := 0 to Length(FZooms) - 1 do begin
        VTileIterators[i] := nil;
      end;
    end;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
  finally
    bmp32crop.Free;
  end;
end;

end.
