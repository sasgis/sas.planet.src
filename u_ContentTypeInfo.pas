unit u_ContentTypeInfo;

interface

uses
  i_BitmapTileSaveLoad,
  i_KmlInfoSimpleLoader,
  i_ContentTypeInfo;

type
  TContentTypeInfoBase = class(TInterfacedObject, IContentTypeInfoBasic)
  private
    FContentType: WideString;
    FDefaultExt: WideString;
  protected
    function GetContentType: WideString;
    function GetDefaultExt: WideString;
  public
    constructor Create(
      AContentType: WideString;
      ADefaultExt: WideString
    );
  end;

  TContentTypeInfoBitmap = class(TContentTypeInfoBase, IContentTypeInfoBitmap)
  private
    FLoader: IBitmapTileLoader;
    FSaver: IBitmapTileSaver;
  protected
    function GetLoader: IBitmapTileLoader;
    function GetSaver: IBitmapTileSaver;
  public
    constructor Create(
      AContentType: WideString;
      ADefaultExt: WideString;
      ALoader: IBitmapTileLoader;
      ASaver: IBitmapTileSaver
    );
    destructor Destroy; override;
  end;

  TContentTypeInfoKml = class(TContentTypeInfoBase, IContentTypeInfoVectorData)
  private
    FLoader: IVectorDataLoader;
  protected
    function GetLoader: IVectorDataLoader;
  public
    constructor Create(
      AContentType: WideString;
      ADefaultExt: WideString;
      ALoader: IVectorDataLoader
    );
    destructor Destroy; override;
  end;

implementation

{ TContentTypeInfoBase }

constructor TContentTypeInfoBase.Create(AContentType, ADefaultExt: WideString);
begin
  FContentType := AContentType;
  FDefaultExt := ADefaultExt;
end;

function TContentTypeInfoBase.GetContentType: WideString;
begin
  Result := FContentType;
end;

function TContentTypeInfoBase.GetDefaultExt: WideString;
begin
  Result := FDefaultExt;
end;

{ TContentTypeInfoBitmap }

constructor TContentTypeInfoBitmap.Create(AContentType, ADefaultExt: WideString;
  ALoader: IBitmapTileLoader; ASaver: IBitmapTileSaver);
begin
  inherited Create(AContentType, ADefaultExt);
  FLoader := ALoader;
  FSaver := ASaver;
end;

destructor TContentTypeInfoBitmap.Destroy;
begin
  FLoader := nil;
  FSaver := nil;
  inherited;
end;

function TContentTypeInfoBitmap.GetLoader: IBitmapTileLoader;
begin
  Result := FLoader;
end;

function TContentTypeInfoBitmap.GetSaver: IBitmapTileSaver;
begin
  Result := FSaver;
end;

{ TContentTypeInfoKml }

constructor TContentTypeInfoKml.Create(AContentType, ADefaultExt: WideString;
  ALoader: IVectorDataLoader);
begin
  inherited Create(AContentType, ADefaultExt);
  FLoader := ALoader;
end;

destructor TContentTypeInfoKml.Destroy;
begin
  FLoader := nil;
  inherited;
end;

function TContentTypeInfoKml.GetLoader: IVectorDataLoader;
begin
  Result := FLoader;
end;

end.
