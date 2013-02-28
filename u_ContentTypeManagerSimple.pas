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
  i_VectorItemsFactory,
  i_ContentTypeInfo,
  i_ContentConverter,
  i_InternalPerformanceCounter,
  i_BitmapTileSaveLoadFactory,
  i_ArchiveReadWriteFactory,
  u_ContentTypeListByKey,
  u_ContentConverterMatrix,
  u_ContentTypeManagerBase;

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
    procedure InitLists(
      const AVectorItemsFactory: IVectorItemsFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ALoadPerfCounterList: IInternalPerformanceCounterList;
      const ASavePerfCounterList: IInternalPerformanceCounterList
    );
  public
    constructor Create(
      const AVectorItemsFactory: IVectorItemsFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  ALStringList,
  i_BitmapTileSaveLoad,
  u_ContentTypeInfo,
  u_ContentConverterKmz2Kml,
  u_ContentConverterKml2Kmz,
  u_KmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_XmlInfoSimpleParser,
  u_ContentConverterBase,
  u_ContentConverterBitmap;

{ TContentTypeManagerSimple }

constructor TContentTypeManagerSimple.Create(
  const AVectorItemsFactory: IVectorItemsFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
  InitLists(
    AVectorItemsFactory,
    ABitmapTileSaveLoadFactory,
    APerfCounterList.CreateAndAddNewSubList('TileLoad'),
    APerfCounterList.CreateAndAddNewSubList('TileSave')
  );
end;

procedure TContentTypeManagerSimple.InitLists(
  const AVectorItemsFactory: IVectorItemsFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ALoadPerfCounterList: IInternalPerformanceCounterList;
  const ASavePerfCounterList: IInternalPerformanceCounterList
);
var
  VContentType: IContentTypeInfoBasic;
  VLoader: IBitmapTileLoader;
  VSaver: IBitmapTileSaver;
  VExt: string;
  VType: string;
begin
  VSaver := ABitmapTileSaveLoadFactory.CreateJpegSaver(85, ASavePerfCounterList);
  VLoader := ABitmapTileSaveLoadFactory.CreateJpegLoader(ALoadPerfCounterList);
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

  VSaver := ABitmapTileSaveLoadFactory.CreatePngSaver(i32bpp, 2, ASavePerfCounterList);
  VLoader := ABitmapTileSaveLoadFactory.CreatePngLoader(ALoadPerfCounterList);
  VType := 'image/png';
  VExt := '.png';
  VContentType := TContentTypeInfoBitmap.Create(VType, VExt, VLoader, VSaver);
  AddByType(VContentType, VType);
  AddByType(VContentType, 'image/x-png');
  AddByType(VContentType, 'image/png; mode=24bit');
  AddByExt(VContentType, VExt);

  VSaver := ABitmapTileSaveLoadFactory.CreateGifSaver(ASavePerfCounterList);
  VLoader := ABitmapTileSaveLoadFactory.CreateGifLoader(ALoadPerfCounterList);
  VType := 'image/gif';
  VExt := '.gif';
  VContentType := TContentTypeInfoBitmap.Create(VType, VExt, VLoader, VSaver);
  AddByType(VContentType, VType);
  AddByExt(VContentType, VExt);

  VSaver := ABitmapTileSaveLoadFactory.CreateBmpSaver(ASavePerfCounterList);
  VLoader := ABitmapTileSaveLoadFactory.CreateBmpLoader(ALoadPerfCounterList);
  VType := 'image/bmp';
  VExt := '.bmp';
  VContentType := TContentTypeInfoBitmap.Create(VType, VExt, VLoader, VSaver);
  AddByType(VContentType, VType);
  AddByType(VContentType, 'image/x-ms-bmp');
  AddByType(VContentType, 'image/x-windows-bmp');
  AddByExt(VContentType, VExt);

  VContentType := TContentTypeInfoVector.Create(
    'application/vnd.google-earth.kml+xml',
    '.kml',
    TKmlInfoSimpleParser.Create(
      AVectorItemsFactory,
      ALoadPerfCounterList
    )
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoVector.Create(
    'application/vnd.google-earth.kmz',
    '.kmz',
    TKmzInfoSimpleParser.Create(
      TKmlInfoSimpleParser.Create(AVectorItemsFactory, nil),
      FArchiveReadWriteFactory,
      ALoadPerfCounterList
    )
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoVector.Create(
    'application/gpx+xml',
    '.gpx',
    TXmlInfoSimpleParser.Create(
      AVectorItemsFactory,
      False,
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
  VSoruceName: AnsiString;
  VSourceContent: IContentTypeInfoBasic;
  VTargetName: AnsiString;
  VTargetContent: IContentTypeInfoBasic;
  VConverter: IContentConverter;
begin
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
  VSourceEnumerator: TALStringsEnumerator;
  VSoruceName: AnsiString;
  VSourceContent: IContentTypeInfoBasic;
  VTargetEnumerator: TALStringsEnumerator;
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
  VSourceEnumerator: TALStringsEnumerator;
  VSoruceName: AnsiString;
  VTargetEnumerator: TALStringsEnumerator;
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
  VSourceEnumerator: TALStringsEnumerator;
  VSoruceName: AnsiString;
  VSourceContent: IContentTypeInfoBasic;
  VTargetEnumerator: TALStringsEnumerator;
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

procedure TContentTypeManagerSimple.ConverterMatrixUpdateBitmaps;
var
  VSourceEnumerator: TALStringsEnumerator;
  VSoruceName: AnsiString;
  VSourceContent: IContentTypeInfoBitmap;
  VTargetEnumerator: TALStringsEnumerator;
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

end.
