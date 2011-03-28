unit u_ContentTypeManagerSimple;

interface

uses
  i_ContentTypeInfo,
  i_ContentConverter,
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
    function FindConverterWithSynonyms(ASourceType, ATargetType: string): IContentConverter;
    procedure UpdateConverterMatrix;
    procedure InitLists;
  public
    constructor Create();
  end;

implementation

uses
  Classes,
  SysUtils,
  u_ContentTypeInfo,
  u_ContentConverterKmz2Kml,
  u_ContentConverterKml2Kmz,
  u_BitmapTileVampyreLoader,
  u_BitmapTileVampyreSaver,
  u_BitmapTileGELoader,
  u_KmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_ContentConverterBase,
  u_ContentConverterBitmap;

{ TContentTypeManagerSimple }

constructor TContentTypeManagerSimple.Create;
begin
  inherited;
  InitLists;
end;

procedure TContentTypeManagerSimple.InitLists;
var
  VContentType: IContentTypeInfoBasic;
begin
  VContentType := TContentTypeInfoBitmap.Create(
    'image/jpg',
    '.jpg',
    TVampyreBasicBitmapTileLoaderJPEG.Create,
    TVampyreBasicBitmapTileSaverJPG.Create(85)
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByType(VContentType, 'image/jpeg');
  AddByType(VContentType, 'image/pjpeg');
  AddByExt(VContentType, VContentType.GetDefaultExt);
  AddByExt(VContentType, '.jpeg');
  
  VContentType := TContentTypeInfoBitmap.Create(
    'image/png',
    '.png',
    TVampyreBasicBitmapTileLoaderPNG.Create,
    TVampyreBasicBitmapTileSaverPNG.Create
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByType(VContentType, 'image/x-png');
  AddByType(VContentType, 'image/png; mode=24bit');
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoBitmap.Create(
    'image/gif',
    '.gif',
    TVampyreBasicBitmapTileLoaderGIF.Create,
    TVampyreBasicBitmapTileSaverGIF.Create
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoBitmap.Create(
    'image/bmp',
    '.bmp',
    TVampyreBasicBitmapTileLoaderBMP.Create,
    TVampyreBasicBitmapTileSaverBMP.Create
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByType(VContentType, 'image/x-ms-bmp');
  AddByType(VContentType, 'image/x-windows-bmp');
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoBitmap.Create(
    'application/vnd.google-earth.tile-image',
    '.ge_image',
    TBitmapTileGELoader.Create,
    nil
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);


  VContentType := TContentTypeInfoKml.Create(
    'application/vnd.google-earth.kml+xml',
    '.kml',
    TKmlInfoSimpleParser.Create
  );
  AddByType(VContentType, VContentType.GetContentType);
  AddByExt(VContentType, VContentType.GetDefaultExt);

  VContentType := TContentTypeInfoKml.Create(
    'application/vnd.google-earth.kmz',
    '.kmz',
    TKmzInfoSimpleParser.Create
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

function TContentTypeManagerSimple.FindConverterWithSynonyms(ASourceType,
  ATargetType: string): IContentConverter;
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
