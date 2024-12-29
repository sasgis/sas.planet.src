{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_ContentTypeManagerSimple;

interface

uses
  i_GeometryLonLatFactory,
  i_ProjConverter,
  i_ContentTypeInfo,
  i_ContentConverter,
  i_VectorDataFactory,
  i_AppearanceOfMarkFactory,
  i_MarkPicture,
  i_VectorItemSubsetBuilder,
  i_InternalPerformanceCounter,
  i_BitmapTileSaveLoad,
  i_BitmapTileSaveLoadFactory,
  i_ArchiveReadWriteFactory,
  i_ContentTypeManager,
  u_ContentTypeListByKey,
  u_ContentConverterMatrix,
  u_ContentTypeManagerBase,
  u_BaseInterfacedObject;

type
  IContentTypeManagerBitmapInternal = interface(IContentTypeManagerBitmap)
    function GetLoadPerfCounterList: IInternalPerformanceCounterList;
    property LoadPerfCounterList: IInternalPerformanceCounterList read GetLoadPerfCounterList;

    function GetSavePerfCounterList: IInternalPerformanceCounterList;
    property SavePerfCounterList: IInternalPerformanceCounterList read GetSavePerfCounterList;

    function GetBitmapExtList: TContentTypeListByKey;
    function GetBitmapTypeList: TContentTypeListByKey;

    function GetIsBitmapType(const AType: AnsiString): Boolean;
    function GetBitmapLoaderByFileName(const AFileName: string): IBitmapTileLoader;
    function GetIsBitmapExt(const AExt: AnsiString): Boolean;
  end;

type
  TContentTypeManagerBitmap = class(TBaseInterfacedObject, IContentTypeManagerBitmap, IContentTypeManagerBitmapInternal)
  private
    FBitmapExtList: TContentTypeListByKey;
    FBitmapTypeList: TContentTypeListByKey;

    FLoadPerfCounterList: IInternalPerformanceCounterList;
    FSavePerfCounterList: IInternalPerformanceCounterList;
    procedure AddByType(
      const AInfo: IContentTypeInfoBitmap;
      const AType: AnsiString
    );
    procedure AddByExt(
      const AInfo: IContentTypeInfoBitmap;
      const AExt: AnsiString
    );
    procedure InitLists(
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory
    );

  private
    function GetLoadPerfCounterList: IInternalPerformanceCounterList;
    function GetSavePerfCounterList: IInternalPerformanceCounterList;

    function GetBitmapExtList: TContentTypeListByKey;
    function GetBitmapTypeList: TContentTypeListByKey;

    function GetIsBitmapType(const AType: AnsiString): Boolean;
    function GetIsBitmapExt(const AExt: AnsiString): Boolean;
    function GetBitmapLoaderByFileName(const AFileName: string): IBitmapTileLoader;
  public
    constructor Create(
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

type
  TContentTypeManagerSimple = class(TContentTypeManagerBase)
  private
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    procedure ConverterMatrixUpdateFixed;
    procedure ConverterMatrixUpdateSynonyms;
    procedure ConverterMatrixUpdateFixedWithSynonyms;
    procedure ConverterMatrixUpdateBitmaps;
    function FindConverterWithSynonyms(const ASourceType, ATargetType: AnsiString): IContentConverter;
    procedure UpdateConverterMatrix;
    procedure AddFromContentTypeManagerBitmapInternal(
      const AContentTypeManagerBitmapInternal: IContentTypeManagerBitmapInternal
    );
    procedure InitLists(
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkPictureList: IMarkPictureList;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AProjConverterFactory: IProjConverterFactory
    );
  public
    constructor Create(
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkPictureList: IMarkPictureList;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AContentTypeManagerBitmapInternal: IContentTypeManagerBitmapInternal;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AProjConverterFactory: IProjConverterFactory
    );
  end;

implementation

uses
  SysUtils,
  Alcinoe.StringList,
  i_VectorDataLoader,
  u_ContentTypeInfo,
  u_ContentConverterKmz2Kml,
  u_ContentConverterKml2Kmz,
  u_WikimapiaKmlSimpleParser,
  u_KmzInfoSimpleParser,
  u_XmlInfoSimpleParser,
  u_GeoJsonParser,
  u_AnsiStr,
  u_ContentConverterBase,
  u_ContentConverterBitmap;

{ TContentTypeManagerSimple }

constructor TContentTypeManagerSimple.Create(
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkPictureList: IMarkPictureList;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AContentTypeManagerBitmapInternal: IContentTypeManagerBitmapInternal;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AProjConverterFactory: IProjConverterFactory
);
begin
  inherited Create;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;

  AddFromContentTypeManagerBitmapInternal(AContentTypeManagerBitmapInternal);

  InitLists(
    AVectorGeometryLonLatFactory,
    AVectorDataFactory,
    AAppearanceOfMarkFactory,
    AMarkPictureList,
    AVectorItemSubsetBuilderFactory,
    AProjConverterFactory
  );
end;

procedure TContentTypeManagerSimple.InitLists(
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkPictureList: IMarkPictureList;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AProjConverterFactory: IProjConverterFactory
);
var
  VContentType: IContentTypeInfoBasic;
  VParser: IVectorDataLoader;
begin
  // KML/KMZ simple (wikimapia)
  VParser :=
    TWikimapiaKmlSimpleParser.Create(
      AVectorGeometryLonLatFactory,
      AVectorDataFactory,
      AVectorItemSubsetBuilderFactory
    );

  VContentType := TContentTypeInfoVector.Create(
    'application/vnd.sas.wikimapia.kml+xml',
    '.kml',
    VParser
  );
  AddByType(VContentType, VContentType.GetContentType);

  VContentType := TContentTypeInfoVector.Create(
    'application/vnd.sas.wikimapia.kmz',
    '.kmz',
    TKmzInfoSimpleParser.Create(
      VParser,
      FArchiveReadWriteFactory.Zip.ReaderFactory
    ) as IVectorDataLoader
  );
  AddByType(VContentType, VContentType.GetContentType);

  // KML/KMZ full
  VParser :=
    TXmlInfoSimpleParser.Create(
      AMarkPictureList,
      AAppearanceOfMarkFactory,
      AVectorGeometryLonLatFactory,
      AVectorDataFactory,
      AVectorItemSubsetBuilderFactory
    );

  VContentType := TContentTypeInfoVector.Create(
    'application/vnd.google-earth.kml+xml',
    '.kml',
    VParser
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoVector.Create(
    'application/vnd.google-earth.kmz',
    '.kmz',
    TKmzInfoSimpleParser.Create(
      VParser,
      FArchiveReadWriteFactory.Zip.ReaderFactory
    ) as IVectorDataLoader
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);

  // GPX
  VContentType := TContentTypeInfoVector.Create(
    'application/gpx+xml',
    '.gpx',
    VParser
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);

  // GeoJSON
  VParser := TGeoJsonParser.Create(
    AVectorItemSubsetBuilderFactory,
    AVectorDataFactory,
    AVectorGeometryLonLatFactory,
    AProjConverterFactory
  );

  VContentType := TContentTypeInfoVector.Create(
    'application/geo+json',
    '.json',
    VParser
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoVector.Create(
    'application/geo+json',
    '.geojson',
    VParser
  );
  AddByExt(VContentType, VContentType.GetDefaultExt);

  UpdateConverterMatrix;
end;

procedure TContentTypeManagerSimple.UpdateConverterMatrix;
begin
  ConverterMatrixUpdateFixed;
  ConverterMatrixUpdateSynonyms;
  ConverterMatrixUpdateFixedWithSynonyms;
  ConverterMatrixUpdateBitmaps;
end;

procedure TContentTypeManagerSimple.ConverterMatrixUpdateFixed;
var
  VSoruceName: AnsiString;
  VSourceContent: IContentTypeInfoBasic;
  VTargetName: AnsiString;
  VTargetContent: IContentTypeInfoBasic;
  VConverter: IContentConverter;
begin
  VSoruceName := 'application/vnd.google-earth.kml+xml';
  VSourceContent := TypeList.Get(VSoruceName);

  VTargetName := 'application/vnd.sas.wikimapia.kml+xml';
  VTargetContent := TypeList.Get(VTargetName);

  VConverter := TContentConverterSimpleCopy.Create(
    VSourceContent,
    VTargetContent
  );
  ConverterMatrix.Add(VSoruceName, VTargetName, VConverter);

  VSoruceName := 'application/vnd.google-earth.kml+xml';
  VSourceContent := TypeList.Get(VSoruceName);

  VTargetName := 'application/vnd.sas.wikimapia.kmz';
  VTargetContent := TypeList.Get(VTargetName);

  VConverter := TContentConverterKml2Kmz.Create(
    VSourceContent,
    VTargetContent,
    FArchiveReadWriteFactory
  );
  ConverterMatrix.Add(VSoruceName, VTargetName, VConverter);

  VSoruceName := 'application/vnd.google-earth.kmz';
  VSourceContent := TypeList.Get(VSoruceName);

  VTargetName := 'application/vnd.google-earth.kml+xml';
  VTargetContent := TypeList.Get(VTargetName);

  VConverter := TContentConverterKmz2Kml.Create(
    VSourceContent,
    VTargetContent,
    FArchiveReadWriteFactory
  );
  ConverterMatrix.Add(VSoruceName, VTargetName, VConverter);

  VSoruceName := 'application/vnd.google-earth.kml+xml';
  VSourceContent := TypeList.Get(VSoruceName);

  VTargetName := 'application/vnd.google-earth.kmz';
  VTargetContent := TypeList.Get(VTargetName);

  VConverter := TContentConverterKml2Kmz.Create(
    VSourceContent,
    VTargetContent,
    FArchiveReadWriteFactory
  );
  ConverterMatrix.Add(VSoruceName, VTargetName, VConverter);
end;

procedure TContentTypeManagerSimple.ConverterMatrixUpdateSynonyms;
var
  VSourceEnumerator: TStringsEnumeratorA;
  VSoruceName: AnsiString;
  VSourceContent: IContentTypeInfoBasic;
  VTargetEnumerator: TStringsEnumeratorA;
  VTargetName: AnsiString;
  VTargetContent: IContentTypeInfoBasic;
  VConverter: IContentConverter;
begin
  VSourceEnumerator := TypeList.GetEnumerator;
  try
    while VSourceEnumerator.MoveNext do begin
      VSoruceName := VSourceEnumerator.Current;
      VSourceContent := TypeList.Get(VSoruceName);
      VTargetEnumerator := TypeList.GetEnumerator;
      try
        while VTargetEnumerator.MoveNext do begin
          VTargetName := VTargetEnumerator.Current;
          VTargetContent := TypeList.Get(VTargetName);
          if ConverterMatrix.Get(VSoruceName, VTargetName) = nil then begin
            if VSourceContent.GetContentType = VTargetContent.GetContentType then begin
              VConverter := TContentConverterSimpleCopy.Create(VSourceContent, VTargetContent);
              ConverterMatrix.Add(VSoruceName, VTargetName, VConverter);
            end;
          end;
        end;
      finally
        VTargetEnumerator.Free;
      end;
    end;
  finally
    VSourceEnumerator.Free;
  end;
end;

function TContentTypeManagerSimple.FindConverterWithSynonyms(
  const ASourceType, ATargetType: AnsiString
): IContentConverter;
var
  VSourceEnumerator: TStringsEnumeratorA;
  VSoruceName: AnsiString;
  VTargetEnumerator: TStringsEnumeratorA;
  VTargetName: AnsiString;
  VConverter: IContentConverter;
begin
  Result := nil;
  VSourceEnumerator := TypeList.GetEnumerator;
  try
    while VSourceEnumerator.MoveNext do begin
      VSoruceName := VSourceEnumerator.Current;
      VConverter := ConverterMatrix.Get(ASourceType, VSoruceName);
      if VConverter <> nil then begin
        if VConverter.GetIsSimpleCopy then begin
          VTargetEnumerator := TypeList.GetEnumerator;
          try
            while VTargetEnumerator.MoveNext do begin
              VTargetName := VTargetEnumerator.Current;
              VConverter := ConverterMatrix.Get(VTargetName, ATargetType);
              if VConverter <> nil then begin
                if VConverter.GetIsSimpleCopy then begin
                  VConverter := ConverterMatrix.Get(VSoruceName, VTargetName);
                  if VConverter <> nil then begin
                    Result := VConverter;
                  end;
                end;
              end;
              if Result <> nil then begin
                Break;
              end;
            end;
          finally
            VTargetEnumerator.Free;
          end;
        end;
      end;
      if Result <> nil then begin
        Break;
      end;
    end;
  finally
    VSourceEnumerator.Free;
  end;
end;

procedure TContentTypeManagerSimple.ConverterMatrixUpdateFixedWithSynonyms;
var
  VSourceEnumerator: TStringsEnumeratorA;
  VSoruceName: AnsiString;
  VSourceContent: IContentTypeInfoBasic;
  VTargetEnumerator: TStringsEnumeratorA;
  VTargetName: AnsiString;
  VTargetContent: IContentTypeInfoBasic;
  VConverter: IContentConverter;
begin
  VSourceEnumerator := TypeList.GetEnumerator;
  try
    while VSourceEnumerator.MoveNext do begin
      VSoruceName := VSourceEnumerator.Current;
      VSourceContent := TypeList.Get(VSoruceName);
      VTargetEnumerator := TypeList.GetEnumerator;
      try
        while VTargetEnumerator.MoveNext do begin
          VTargetName := VTargetEnumerator.Current;
          VTargetContent := TypeList.Get(VTargetName);
          if ConverterMatrix.Get(VSoruceName, VTargetName) = nil then begin
            VConverter := FindConverterWithSynonyms(VSoruceName, VTargetName);
            if VConverter <> nil then begin
              ConverterMatrix.Add(VSoruceName, VTargetName, VConverter);
            end;
          end;
        end;
      finally
        VTargetEnumerator.Free;
      end;
    end;
  finally
    VSourceEnumerator.Free;
  end;
end;

procedure TContentTypeManagerSimple.AddFromContentTypeManagerBitmapInternal(
  const AContentTypeManagerBitmapInternal: IContentTypeManagerBitmapInternal
);
var
  VList: TContentTypeListByKey;
  VEnumerator: TStringsEnumeratorA;
begin
  VList := AContentTypeManagerBitmapInternal.GetBitmapExtList;
  VEnumerator := VList.GetEnumerator;
  try
    while VEnumerator.MoveNext do begin
      AddByExt(VList.Get(VEnumerator.Current), VEnumerator.Current);
    end;
  finally
    VEnumerator.Free;
  end;

  VList := AContentTypeManagerBitmapInternal.GetBitmapTypeList;
  VEnumerator := VList.GetEnumerator;
  try
    while VEnumerator.MoveNext do begin
      AddByType(VList.Get(VEnumerator.Current), VEnumerator.Current);
    end;
  finally
    VEnumerator.Free;
  end;
end;

procedure TContentTypeManagerSimple.ConverterMatrixUpdateBitmaps;
var
  VSourceEnumerator: TStringsEnumeratorA;
  VSoruceName: AnsiString;
  VSourceContent: IContentTypeInfoBitmap;
  VTargetEnumerator: TStringsEnumeratorA;
  VTargetName: AnsiString;
  VTargetContent: IContentTypeInfoBitmap;
  VConverter: IContentConverter;
begin
  VSourceEnumerator := BitmapTypeList.GetEnumerator;
  try
    while VSourceEnumerator.MoveNext do begin
      VSoruceName := VSourceEnumerator.Current;
      VSourceContent := BitmapTypeList.Get(VSoruceName) as IContentTypeInfoBitmap;
      VTargetEnumerator := BitmapTypeList.GetEnumerator;
      try
        while VTargetEnumerator.MoveNext do begin
          VTargetName := VTargetEnumerator.Current;
          VTargetContent := BitmapTypeList.Get(VTargetName) as IContentTypeInfoBitmap;
          if ConverterMatrix.Get(VSoruceName, VTargetName) = nil then begin
            if VSourceContent.GetLoader <> nil then begin
              if VTargetContent.GetSaver <> nil then begin
                VConverter := TContentConverterBitmap.Create(VSourceContent, VTargetContent);
                ConverterMatrix.Add(VSoruceName, VTargetName, VConverter);
              end;
            end;
          end;
        end;
      finally
        VTargetEnumerator.Free;
      end;
    end;
  finally
    VSourceEnumerator.Free;
  end;
end;

{ TContentTypeManagerBitmap }

procedure TContentTypeManagerBitmap.AddByExt(
  const AInfo: IContentTypeInfoBitmap;
  const AExt: AnsiString
);
begin
  Assert(IsAscii(AExt));
  FBitmapExtList.Add(AExt, AInfo);
end;

procedure TContentTypeManagerBitmap.AddByType(
  const AInfo: IContentTypeInfoBitmap;
  const AType: AnsiString
);
begin
  Assert(IsAscii(AType));
  FBitmapTypeList.Add(AType, AInfo);
end;

constructor TContentTypeManagerBitmap.Create(
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FLoadPerfCounterList := APerfCounterList.CreateAndAddNewSubList('TileLoad');
  FSavePerfCounterList := APerfCounterList.CreateAndAddNewSubList('TileSave');

  FBitmapExtList := TContentTypeListByKey.Create;
  FBitmapTypeList := TContentTypeListByKey.Create;

  InitLists(ABitmapTileSaveLoadFactory);
end;

destructor TContentTypeManagerBitmap.Destroy;
begin
  FreeAndNil(FBitmapExtList);
  FreeAndNil(FBitmapTypeList);
  inherited;
end;

function TContentTypeManagerBitmap.GetBitmapLoaderByFileName(
  const AFileName: string
): IBitmapTileLoader;
var
  VExt: string;
  VContentType: IContentTypeInfoBasic;
  VContentTypeBitmap: IContentTypeInfoBitmap;
begin
  Result := nil;
  VExt := LowerCase(ExtractFileExt(AFileName));
  if IsAscii(VExt) then begin
    VContentType := FBitmapExtList.Get(AnsiString(VExt));
    if Assigned(VContentType) then begin
      if Supports(VContentType, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
        Result := VContentTypeBitmap.GetLoader;
      end;
    end;
  end;
end;

function TContentTypeManagerBitmap.GetBitmapExtList: TContentTypeListByKey;
begin
  Result := FBitmapExtList;
end;

function TContentTypeManagerBitmap.GetBitmapTypeList: TContentTypeListByKey;
begin
  Result := FBitmapTypeList;
end;

function TContentTypeManagerBitmap.GetIsBitmapExt(
  const AExt: AnsiString
): Boolean;
begin
  Assert(IsAscii(AExt));
  Result := FBitmapExtList.Get(AExt) <> nil;
end;

function TContentTypeManagerBitmap.GetIsBitmapType(
  const AType: AnsiString
): Boolean;
begin
  Assert(IsAscii(AType));
  Result := FBitmapTypeList.Get(AType) <> nil;
end;

function TContentTypeManagerBitmap.GetLoadPerfCounterList: IInternalPerformanceCounterList;
begin
  Result := FLoadPerfCounterList;
end;

function TContentTypeManagerBitmap.GetSavePerfCounterList: IInternalPerformanceCounterList;
begin
  Result := FSavePerfCounterList;
end;

procedure TContentTypeManagerBitmap.InitLists(
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory
);
var
  VContentType: IContentTypeInfoBitmap;
  VLoader: IBitmapTileLoader;
  VSaver: IBitmapTileSaver;
  VExt: AnsiString;
  VType: AnsiString;
begin
  VSaver := ABitmapTileSaveLoadFactory.CreateJpegSaver(85, FSavePerfCounterList);
  VLoader := ABitmapTileSaveLoadFactory.CreateJpegLoader(FLoadPerfCounterList);
  VType := 'image/jpg';
  VExt := '.jpg';
  VContentType := TContentTypeInfoBitmap.Create(VType, VExt, VLoader, VSaver);
  AddByType(VContentType, VType);
  AddByType(VContentType, 'image/jpeg');
  AddByType(VContentType, 'image/pjpeg');
  AddByExt(VContentType, VExt);

  VExt := '.jpeg';
  VContentType := TContentTypeInfoBitmap.Create(VType, VExt, VLoader, VSaver);
  AddByExt(VContentType, VExt);

  VSaver := ABitmapTileSaveLoadFactory.CreatePngSaver(i32bpp, 2, FSavePerfCounterList);
  VLoader := ABitmapTileSaveLoadFactory.CreatePngLoader(FLoadPerfCounterList);
  VType := 'image/png';
  VExt := '.png';
  VContentType := TContentTypeInfoBitmap.Create(VType, VExt, VLoader, VSaver);
  AddByType(VContentType, VType);
  AddByType(VContentType, 'image/x-png');
  AddByType(VContentType, 'image/png; mode=24bit');
  AddByExt(VContentType, VExt);

  VSaver := ABitmapTileSaveLoadFactory.CreateGifSaver(FSavePerfCounterList);
  VLoader := ABitmapTileSaveLoadFactory.CreateGifLoader(FLoadPerfCounterList);
  VType := 'image/gif';
  VExt := '.gif';
  VContentType := TContentTypeInfoBitmap.Create(VType, VExt, VLoader, VSaver);
  AddByType(VContentType, VType);
  AddByExt(VContentType, VExt);

  VSaver := ABitmapTileSaveLoadFactory.CreateBmpSaver(FSavePerfCounterList);
  VLoader := ABitmapTileSaveLoadFactory.CreateBmpLoader(FLoadPerfCounterList);
  VType := 'image/bmp';
  VExt := '.bmp';
  VContentType := TContentTypeInfoBitmap.Create(VType, VExt, VLoader, VSaver);
  AddByType(VContentType, VType);
  AddByType(VContentType, 'image/x-ms-bmp');
  AddByType(VContentType, 'image/x-windows-bmp');
  AddByExt(VContentType, VExt);

  VSaver := ABitmapTileSaveLoadFactory.CreateWebpSaver(75, FSavePerfCounterList);
  VLoader := ABitmapTileSaveLoadFactory.CreateWebpLoader(FLoadPerfCounterList);
  VType := 'image/webp';
  VExt := '.webp';
  VContentType := TContentTypeInfoBitmap.Create(VType, VExt, VLoader, VSaver);
  AddByType(VContentType, VType);
  AddByExt(VContentType, VExt);
end;

end.
