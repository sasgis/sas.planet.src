unit u_ContentConverterBitmap;

interface

uses
  Classes,
  i_ContentTypeInfo,
  i_BitmapTileSaveLoad,
  u_ContentConverterBase;

type
  TContentConverterBitmap = class(TContentConverterBase)
  private
    FSourceLoader: IBitmapTileLoader;
    FTargetSaver: IBitmapTileSaver;
  protected
    procedure ConvertStream(ASource, ATarget: TStream); override;
  public
    constructor Create(
      ASource: IContentTypeInfoBasic;
      ATarget: IContentTypeInfoBasic
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32;

{ TContentConverterBitmap }

constructor TContentConverterBitmap.Create(ASource,
  ATarget: IContentTypeInfoBasic);
var
  VSource: IContentTypeInfoBitmap;
  VTarget: IContentTypeInfoBitmap;
begin
  inherited;
  VSource := GetSource as IContentTypeInfoBitmap;
  FSourceLoader := VSource.GetLoader;
  if FSourceLoader = nil then begin
    raise Exception.Create('Из исходного типа нельзя загрузить растр');
  end;

  VTarget := GetTarget as IContentTypeInfoBitmap;
  FTargetSaver := VTarget.GetSaver;
  if FTargetSaver  = nil then begin
    raise Exception.Create('В результирующий тип нельзя сохранить растр');
  end;
end;

destructor TContentConverterBitmap.Destroy;
begin
  FSourceLoader := nil;
  FTargetSaver := nil;
  inherited;
end;

procedure TContentConverterBitmap.ConvertStream(ASource, ATarget: TStream);
var
  VBitmap: TCustomBitmap32;
begin
  inherited;
  VBitmap := TCustomBitmap32.Create;
  try
    FSourceLoader.LoadFromStream(ASource, VBitmap);
    FTargetSaver.SaveToStream(VBitmap, ATarget);
  finally
    VBitmap.Free;
  end;
end;

end.
