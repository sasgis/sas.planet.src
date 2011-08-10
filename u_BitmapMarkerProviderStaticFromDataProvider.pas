unit u_BitmapMarkerProviderStaticFromDataProvider;

interface

uses
  Types,
  GR32,
  i_JclNotify,
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ContentTypeManager,
  i_BitmapMarker,
  i_BitmapMarkerProviderSimpleConfig;

type
  TBitmapMarkerProviderStaticFromDataProvider = class(TInterfacedObject, IBitmapMarkerProvider)
  private
    FMarker: IBitmapMarker;
    FChangeNotifier: IJclNotifier;
  protected
    function GetMarker: IBitmapMarker;
    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(
      AResourceDataProvider: IConfigDataProvider;
      AContentTypeManager: IContentTypeManager;
      AResourceName: string;
      AAnchorPoint: TDoublePoint;
      AUseDirection: Boolean;
      ADefaultDirection: Double
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  u_JclNotify,
  i_ContentTypeInfo,
  u_BitmapMarker;

{ TBitmapMarkerProviderStaticFromDataProvider }

constructor TBitmapMarkerProviderStaticFromDataProvider.Create(
  AResourceDataProvider: IConfigDataProvider;
  AContentTypeManager: IContentTypeManager;
  AResourceName: string;
  AAnchorPoint: TDoublePoint;
  AUseDirection: Boolean;
  ADefaultDirection: Double
);
var
  VFileName: string;
  VFileExt: string;
  VInfoBasic: IContentTypeInfoBasic;
  VBitmapContntType: IContentTypeInfoBitmap;
  VBitmap: TCustomBitmap32;
  VStream: TMemoryStream;
begin
  FChangeNotifier := TJclBaseNotifier.Create;

  VFileName := ExtractFileName(AResourceName);
  VFileExt := ExtractFileExt(VFileName);
  VBitmap := TCustomBitmap32.Create;
  try
    VInfoBasic := AContentTypeManager.GetInfoByExt(VFileExt);
    if VInfoBasic <> nil then begin
      if Supports(VInfoBasic, IContentTypeInfoBitmap, VBitmapContntType) then begin
        VStream := TMemoryStream.Create;
        try
          if AResourceDataProvider.ReadBinaryStream(VFileName, VStream) > 0 then begin
            VStream.Position := 0;
            try
              VBitmapContntType.GetLoader.LoadFromStream(VStream, VBitmap);
            except
              Assert(False, 'Ошибка при загрузке картинки ' + AResourceName);
            end;
          end;
        finally
          VStream.Free;
        end;
      end;
    end;

    FMarker :=
      TBitmapMarker.Create(
        VBitmap,
        AAnchorPoint,
        AUseDirection,
        ADefaultDirection
      );
  finally
    VBitmap.Free;
  end;
end;

function TBitmapMarkerProviderStaticFromDataProvider.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapMarkerProviderStaticFromDataProvider.GetMarker: IBitmapMarker;
begin
  Result := FMarker;
end;

end.
