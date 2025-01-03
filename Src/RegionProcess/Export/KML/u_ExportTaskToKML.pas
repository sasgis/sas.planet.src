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

unit u_ExportTaskToKML;

interface

uses
  Types,
  SysUtils,
  Classes,
  t_GeoTypes,
  i_NotifierOperation,
  i_ContentTypeInfo,
  i_RegionProcessProgressInfo,
  i_TileIterator,
  i_TileIteratorFactory,
  i_GeometryLonLat,
  i_MapVersionInfo,
  i_BitmapTileProvider,
  i_BitmapTileSaveLoad,
  i_TileStorage,
  i_TileFileNameGenerator,
  i_Projection,
  i_GeometryProjected,
  i_GeometryProjectedFactory,
  u_ExportTaskAbstract;

type
  TExportTaskToKML = class(TExportTaskAbstract)
  private
    FTileStorage: ITileStorage;
    FVersion: IMapVersionInfo;
    FSaveExistsOnly: Boolean;
    FKmlFileName: string;
    FTilesPath: string;
    FTryUseRelativePath: Boolean;
    FExtractTilesFromStorage: Boolean;
    FTileFileNameGenerator: ITileFileNameGenerator;
    FBitmapTileSaver: IBitmapTileSaver;
    FBitmapTileProviderArr: TBitmapTileProviderDynArray;
    FContentTypeInfo: IContentTypeInfoBasic;
    FProviderIndexByZoom: array[0..23] of Integer;
    FTilesToProcess: Int64;
    FTilesProcessed: Int64;
    FKmlStream: TFileStream;
    FUseTileProviderAndSaver: Boolean;
    FIsFirstVisibleLayerWritten: Boolean;
    FGeometryProjectedFactory: IGeometryProjectedFactory;
    FArr: array of record
      Proj: IProjection;
      Iter: ITileIterator;
      Poly: IGeometryProjectedPolygon;
    end;
    procedure KmlFileWrite(
      const ATile: TPoint;
      const AZoom: Byte;
      const ALevel: Integer;
      const AIsParentInPoly: Boolean
    );
    procedure WriteTextToKmlStream(
      const AText: string
    );
    function CopyTileToFileSystem(
      const ATile: TPoint;
      const AZoom: Byte
    ): string;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AOutputFileName: string;
      const AGeometryProjectedFactory: IGeometryProjectedFactory;
      const ATileIteratorFactory: ITileIteratorFactory;
      const APolygon: IGeometryLonLatPolygon;
      const AZoomArr: TByteDynArray;
      const ATileStorage: ITileStorage;
      const AVersion: IMapVersionInfo;
      const ANotSaveNotExists: Boolean;
      const ARelativePath: Boolean;
      const AExtractTilesFromStorage: Boolean;
      const ATileFileNameGenerator: ITileFileNameGenerator;
      const ABitmapTileSaver: IBitmapTileSaver;
      const AContentTypeInfo: IContentTypeInfoBasic;
      const ABitmapTileProviderArr: TBitmapTileProviderDynArray
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  StrUtils,
  i_BinaryData,
  i_Bitmap32Static,
  i_TileInfoBasic,
  u_TileIteratorByRect,
  u_GeoToStrFunc,
  u_GeoFunc,
  u_ResStrings;

constructor TExportTaskToKML.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AOutputFileName: string;
  const AGeometryProjectedFactory: IGeometryProjectedFactory;
  const ATileIteratorFactory: ITileIteratorFactory;
  const APolygon: IGeometryLonLatPolygon;
  const AZoomArr: TByteDynArray;
  const ATileStorage: ITileStorage;
  const AVersion: IMapVersionInfo;
  const ANotSaveNotExists: Boolean;
  const ARelativePath: Boolean;
  const AExtractTilesFromStorage: Boolean;
  const ATileFileNameGenerator: ITileFileNameGenerator;
  const ABitmapTileSaver: IBitmapTileSaver;
  const AContentTypeInfo: IContentTypeInfoBasic;
  const ABitmapTileProviderArr: TBitmapTileProviderDynArray
);
var
  I, J: Integer;
begin
  if AExtractTilesFromStorage then begin
    Assert(ATileFileNameGenerator <> nil);
  end;

  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    ATileIteratorFactory
  );

  FGeometryProjectedFactory := AGeometryProjectedFactory;

  FKmlFileName := AOutputFileName;
  FTilesPath := FKmlFileName + '.files' + PathDelim;
  FTryUseRelativePath := ARelativePath;
  FTileFileNameGenerator := ATileFileNameGenerator;
  FTileStorage := ATileStorage;
  FExtractTilesFromStorage := AExtractTilesFromStorage;
  FSaveExistsOnly := ANotSaveNotExists;
  FVersion := AVersion;
  FBitmapTileSaver := ABitmapTileSaver;
  FContentTypeInfo := AContentTypeInfo;
  FBitmapTileProviderArr := ABitmapTileProviderArr;

  FUseTileProviderAndSaver :=
    FExtractTilesFromStorage and
    (FBitmapTileSaver <> nil) and
    (FContentTypeInfo <> nil) and
    (Length(FBitmapTileProviderArr) = Length(AZoomArr));

  if FUseTileProviderAndSaver then begin
    for I := 0 to Length(FProviderIndexByZoom) - 1 do begin
      FProviderIndexByZoom[I] := -1;
      for J := 0 to Length(AZoomArr) - 1 do begin
        if I = AZoomArr[J] then begin
          FProviderIndexByZoom[I] := J;
          Break;
        end;
      end;
    end;
  end;
end;

destructor TExportTaskToKML.Destroy;
begin
  FreeAndNil(FKmlStream);
  inherited Destroy;
end;

procedure TExportTaskToKML.KmlFileWrite(
  const ATile: TPoint;
  const AZoom: Byte;
  const ALevel: Integer;
  const AIsParentInPoly: Boolean
);

  function _IsTileInPoly: Boolean;
  var
    VIntersection: TRectWithPolygonIntersection;
  begin
    with FArr[ALevel] do begin
      VIntersection := Poly.CheckRectIntersection( Proj.TilePos2PixelRectFloat(ATile) );
      // Inore rwpPolygonInRect intersection to skip insufficient zooms
      Result := VIntersection in [rwpIntersectPartial, rwpRectInPolygon];
    end;
  end;

  function _WriteTile: Boolean;
  var
    VSavePath: string;
    VMinLodPix: string;
    VNorth, VSouth, VEast, VWest: string;
    VLonLatRect: TDoubleRect;
    VTileInfo: ITileInfoBasic;
  begin
    Result := False;

    if FSaveExistsOnly then begin
      VTileInfo := FTileStorage.GetTileInfo(ATile, AZoom, FVersion, gtimAsIs);
      if not Assigned(VTileInfo) or not VTileInfo.GetIsExists then begin
        Exit;
      end;
    end;

    if FExtractTilesFromStorage then begin
      VSavePath := CopyTileToFileSystem(ATile, AZoom);
      if VSavePath = '' then begin
        Exit;
      end;
    end else begin
      VSavePath := FTileStorage.GetTileFileName(ATile, AZoom, FVersion);
    end;

    if FTryUseRelativePath then begin
      VSavePath := ExtractRelativePath(ExtractFilePath(FKmlFileName), VSavePath);
    end;

    VLonLatRect := FArr[ALevel].Proj.TilePos2LonLatRect(ATile);
    VNorth := R2StrPoint(VLonLatRect.Top);
    VSouth := R2StrPoint(VLonLatRect.Bottom);
    VEast := R2StrPoint(VLonLatRect.Right);
    VWest := R2StrPoint(VLonLatRect.Left);

    if not FIsFirstVisibleLayerWritten then begin
      VMinLodPix := '16';
      FIsFirstVisibleLayerWritten := True;
    end else begin
      VMinLodPix := '128';
    end;

    WriteTextToKmlStream(
      #13#10 +
      '<Folder>' + #13#10 +
      '  <Region>' + #13#10 +
      '    <LatLonAltBox>' + #13#10 +
      '      <north>' + VNorth + '</north>' + #13#10 +
      '      <south>' + VSouth + '</south>' + #13#10 +
      '      <east>' + VEast + '</east>' + #13#10 +
      '      <west>' + VWest + '</west>' + #13#10 +
      '    </LatLonAltBox>' + #13#10 +
      '    <Lod>' + #13#10 +
      '      <minLodPixels>' + VMinLodPix +'</minLodPixels>' + #13#10 +
      '      <maxLodPixels>-1</maxLodPixels>' + #13#10 +
      '    </Lod>' + #13#10 +
      '  </Region>' + #13#10 +
      '  <GroundOverlay>' + #13#10 +
      '    <drawOrder>' + IntToStr(ALevel) + '</drawOrder>' + #13#10 +
      '    <Icon>' + #13#10 +
      '      <href>' + VSavePath + '</href>' + #13#10 +
      '    </Icon>' + #13#10 +
      '    <LatLonBox>' + #13#10 +
      '      <north>' + VNorth + '</north>' + #13#10 +
      '      <south>' + VSouth + '</south>' + #13#10 +
      '      <east>' + VEast + '</east>' + #13#10 +
      '      <west>' + VWest + '</west>' + #13#10 +
      '    </LatLonBox>' + #13#10 +
      '  </GroundOverlay>'
    );

    Result := True;
  end;

  procedure _ProcessNextLevel(const AIsCurrentTileInPoly: Boolean);
  var
    VNextLevel: Integer;
    VTile: TPoint;
    VTileRect: TRect;
    VIterator: TTileIteratorByRectRecord;
  begin
    VNextLevel := ALevel + 1;
    if VNextLevel < Length(FZooms) then begin
      VTileRect :=
        RectFromDoubleRect(
          FArr[VNextLevel].Proj.RelativeRect2TileRectFloat( FArr[ALevel].Proj.TilePos2RelativeRect(ATile) ),
          rrClosest
        );
      if FArr[VNextLevel].Iter.TilesRect.IsIntersecWithRect(VTileRect) then begin
        VIterator.Init(VTileRect);
        while VIterator.Next(VTile) do begin
          KmlFileWrite(VTile, FZooms[VNextLevel], VNextLevel, AIsCurrentTileInPoly);
        end;
      end;
    end;
  end;

var
  VIsTileInPoly: Boolean;
  VWriteKmlFooter: Boolean;
begin
  if CancelNotifier.IsOperationCanceled(OperationID) then begin
    Exit;
  end;

  Inc(FTilesProcessed);
  if FTilesProcessed mod 100 = 0 then begin
    ProgressFormUpdateOnProgress(FTilesProcessed, FTilesToProcess);
  end;

  if FUseTileProviderAndSaver then begin
    VIsTileInPoly := _IsTileInPoly;
  end else begin
    VIsTileInPoly := AIsParentInPoly or _IsTileInPoly;
  end;

  if VIsTileInPoly then begin
    VWriteKmlFooter := _WriteTile;
  end else begin
    VWriteKmlFooter := False;
  end;

  _ProcessNextLevel(VIsTileInPoly);

  if VWriteKmlFooter then begin
    WriteTextToKmlStream(#13#10 + '</Folder>');
  end;
end;

procedure TExportTaskToKML.ProcessRegion;
var
  I: Integer;
  VTile: TPoint;
begin
  inherited;

  FTilesProcessed := 0;
  FTilesToProcess := 0;

  SetLength(FArr, Length(FZooms));
  for I := 0 to Length(FZooms) - 1 do begin
    with FArr[I] do begin
      Proj := FTileStorage.ProjectionSet.Zooms[FZooms[I]];
      Iter := Self.MakeTileIterator(Proj);
      Poly := FGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(Proj, Self.PolygLL);

      Inc(FTilesToProcess, Iter.TilesTotal);
    end;
  end;

  ProgressInfo.SetCaption(SAS_STR_ExportTiles);
  ProgressInfo.SetFirstLine(
    SAS_STR_AllSaves + ' ' + IntToStr(FTilesToProcess) + ' ' + SAS_STR_Files
  );
  ProgressFormUpdateOnProgress(FTilesProcessed, FTilesToProcess);

  try
    WriteTextToKmlStream(
      '<?xml version="1.0" encoding="UTF-8"?>' + #13#10 +
      '<kml xmlns="http://earth.google.com/kml/2.1">' + #13#10 +
      '<Document>' + #13#10 +
      '<name>' + ExtractFileName(FKmlFileName) + '</name>'
    );

    while FArr[0].Iter.Next(VTile) do begin
      KmlFileWrite(VTile, FZooms[0], 0, False);
    end;

    WriteTextToKmlStream(#13#10 + '</Document>' + #13#10 + '</kml>' + #13#10);
  finally
    ProgressFormUpdateOnProgress(FTilesProcessed, FTilesToProcess);
  end;
end;

procedure TExportTaskToKML.WriteTextToKmlStream(const AText: string);
var
  VUtf8Text: UTF8String;
begin
  if AText = '' then begin
    Exit;
  end;
  if FKmlStream = nil then begin
    FKmlStream := TFileStream.Create(FKmlFileName, fmCreate);
  end;
  VUtf8Text := UTF8Encode(AText);
  FKmlStream.WriteBuffer(VUtf8Text[1], Length(VUtf8Text));
end;

function TExportTaskToKML.CopyTileToFileSystem(
  const ATile: TPoint;
  const AZoom: Byte
): string;

  function _GetTileFileName(const AContentTypeInfo: IContentTypeInfoBasic): string;
  begin
    Result := FTilesPath +
      ChangeFileExt(
        FTileFileNameGenerator.GetTileFileName(ATile, AZoom),
        AContentTypeInfo.GetDefaultExt
      );
  end;

  function _GetTile(out AFileName: string; out AData: IBinaryData): Boolean;
  var
    I: Integer;
    VBitmap: IBitmap32Static;
    VTileInfo: ITileInfoBasic;
    VTileInfoWithData: ITileInfoWithData;
  begin
    if FUseTileProviderAndSaver then begin
      I := FProviderIndexByZoom[AZoom];
      Assert((I >= 0) and (I < Length(FBitmapTileProviderArr)));
      VBitmap := FBitmapTileProviderArr[I].GetTile(Self.OperationID, Self.CancelNotifier, ATile);
      AData := FBitmapTileSaver.Save(VBitmap);
      AFileName := _GetTileFileName(FContentTypeInfo);
    end else begin
      VTileInfo := FTileStorage.GetTileInfo(ATile, AZoom, FVersion, gtimWithData);
      if Supports(VTileInfo, ITileInfoWithData, VTileInfoWithData) then begin
        AData := VTileInfoWithData.TileData;
        AFileName := _GetTileFileName(VTileInfo.ContentType);
      end;
    end;
    Result := AData <> nil;
  end;

var
  VData: IBinaryData;
begin
  if not _GetTile(Result, VData) then begin
    Result := '';
    Exit;
  end;

  if not ForceDirectories(ExtractFileDir(Result)) then begin
    RaiseLastOSError;
  end;

  with TFileStream.Create(Result, fmCreate) do
  try
    WriteBuffer(VData.Buffer^, VData.Size);
  finally
    Free;
  end;
end;

end.
