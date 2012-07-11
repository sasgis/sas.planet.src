{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_ContentTypeManagerSimple;

interface

uses
  i_VectorItmesFactory,
  i_ContentTypeInfo,
  i_ContentConverter,
  i_InternalPerformanceCounter,
  u_ContentTypeListByKey,
  u_ContentConverterMatrix,
  u_ContentTypeManagerBase;

type
  TContentTypeManagerSimple = class(TContentTypeManagerBase)
  private
    procedure ConverterMatrixUpdateFixed;
    procedure ConverterMatrixUpdateSynonyms;
    procedure ConverterMatrixUpdateFixedWithSynonyms;
    procedure ConverterMatrixUpdateBitmaps;
    function FindConverterWithSynonyms(const ASourceType, ATargetType: string): IContentConverter;
    procedure UpdateConverterMatrix;
    procedure InitLists(
      const AFactory: IVectorItmesFactory;
      const ALoadPerfCounterList: IInternalPerformanceCounterList;
      const ASavePerfCounterList: IInternalPerformanceCounterList
    );
  public
    constructor Create(
      const AFactory: IVectorItmesFactory;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  Classes,
  i_BitmapTileSaveLoad,
  u_ContentTypeInfo,
  u_ContentConverterKmz2Kml,
  u_ContentConverterKml2Kmz,
  u_BitmapTileVampyreLoader,
  u_BitmapTileVampyreSaver,
  u_BitmapTileJpegLoadSave,
  u_KmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_ContentConverterBase,
  u_ContentConverterBitmap;

{ TContentTypeManagerSimple }

constructor TContentTypeManagerSimple.Create(
  const AFactory: IVectorItmesFactory;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  InitLists(
    AFactory,
    APerfCounterList.CreateAndAddNewSubList('TileLoad'),
    APerfCounterList.CreateAndAddNewSubList('TileSave')
  );
end;

procedure TContentTypeManagerSimple.InitLists(
  const AFactory: IVectorItmesFactory;
  const ALoadPerfCounterList: IInternalPerformanceCounterList;
  const ASavePerfCounterList: IInternalPerformanceCounterList
);
var
  VContentType: IContentTypeInfoBasic;
  VLoader: IBitmapTileLoader;
  VSaver: IBitmapTileSaver;
begin
  VLoader := TBitmapTileJpegLoadSave.Create(ALoadPerfCounterList);
  VSaver := TBitmapTileJpegLoadSave.Create(85, ASavePerfCounterList);

  VContentType := TContentTypeInfoBitmap.Create(
    'image/jpg',
    '.jpg',
    VLoader,
    VSaver
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByType(VContentType, 'image/jpeg');
  AddByType(VContentType, 'image/pjpeg');
  AddByExt(VContentType, VContentType.GetDefaultExt);
  AddByExt(VContentType, '.jpeg');

  VContentType := TContentTypeInfoBitmap.Create(
    'image/png',
    '.png',
    TVampyreBasicBitmapTileLoaderPNG.Create(ALoadPerfCounterList),
    TVampyreBasicBitmapTileSaverPNG.Create(ASavePerfCounterList)
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByType(VContentType, 'image/x-png');
  AddByType(VContentType, 'image/png; mode=24bit');
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoBitmap.Create(
    'image/gif',
    '.gif',
    TVampyreBasicBitmapTileLoaderGIF.Create(ALoadPerfCounterList),
    TVampyreBasicBitmapTileSaverGIF.Create(ASavePerfCounterList)
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoBitmap.Create(
    'image/bmp',
    '.bmp',
    TVampyreBasicBitmapTileLoaderBMP.Create(ALoadPerfCounterList),
    TVampyreBasicBitmapTileSaverBMP.Create(ASavePerfCounterList)
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByType(VContentType, 'image/x-ms-bmp');
  AddByType(VContentType, 'image/x-windows-bmp');
  AddByExt(VContentType, VContentType.GetDefaultExt);

(*
  VContentType := TContentTypeInfoBitmap.Create(
    'application/vnd.google-earth.tile-image',
    '.ge_image',
    TBitmapTileGELoader.Create(APerfCounterList),
    nil
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);
*)

  VContentType := TContentTypeInfoKml.Create(
    'application/vnd.google-earth.kml+xml',
    '.kml',
    TKmlInfoSimpleParser.Create(
      AFactory,
      ALoadPerfCounterList
    )
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoKml.Create(
    'application/vnd.google-earth.kmz',
    '.kmz',
    TKmzInfoSimpleParser.Create(
      AFactory,
      ALoadPerfCounterList
    )
  );
  AddByType(VContentType, VContentType.GetContentType);
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
  VSoruceName: string;
  VSourceContent: IContentTypeInfoBasic;
  VTargetName: string;
  VTargetContent: IContentTypeInfoBasic;
  VConverter: IContentConverter;
begin
  VSoruceName := 'application/vnd.google-earth.kmz';
  VSourceContent := TypeList.Get(VSoruceName);

  VTargetName := 'application/vnd.google-earth.kml+xml';
  VTargetContent := TypeList.Get(VTargetName);

  VConverter := TContentConverterKmz2Kml.Create(VSourceContent, VTargetContent);
  ConverterMatrix.Add(VSoruceName, VTargetName, VConverter);

  VSoruceName := 'application/vnd.google-earth.kml+xml';
  VSourceContent := TypeList.Get(VSoruceName);

  VTargetName := 'application/vnd.google-earth.kmz';
  VTargetContent := TypeList.Get(VTargetName);

  VConverter := TContentConverterKml2Kmz.Create(VSourceContent, VTargetContent);
  ConverterMatrix.Add(VSoruceName, VTargetName, VConverter);
end;

procedure TContentTypeManagerSimple.ConverterMatrixUpdateSynonyms;
var
  VSourceEnumerator: TStringsEnumerator;
  VSoruceName: string;
  VSourceContent: IContentTypeInfoBasic;
  VTargetEnumerator: TStringsEnumerator;
  VTargetName: string;
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
  const ASourceType, ATargetType: string
): IContentConverter;
var
  VSourceEnumerator: TStringsEnumerator;
  VSoruceName: string;
  VTargetEnumerator: TStringsEnumerator;
  VTargetName: string;
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
  VSourceEnumerator: TStringsEnumerator;
  VSoruceName: string;
  VSourceContent: IContentTypeInfoBasic;
  VTargetEnumerator: TStringsEnumerator;
  VTargetName: string;
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

procedure TContentTypeManagerSimple.ConverterMatrixUpdateBitmaps;
var
  VSourceEnumerator: TStringsEnumerator;
  VSoruceName: string;
  VSourceContent: IContentTypeInfoBitmap;
  VTargetEnumerator: TStringsEnumerator;
  VTargetName: string;
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

end.
