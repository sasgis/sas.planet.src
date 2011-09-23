unit u_ProviderTilesGenPrev;

interface

uses
  Windows,
  Forms,
  t_GeoTypes,
  u_ExportProviderAbstract,
  fr_TilesGenPrev;

type
  TProviderTilesGenPrev = class(TExportProviderAbstract)
  private
    FFrame: TfrTilesGenPrev;
  public
    destructor Destroy; override;
    function GetCaption: string; override;
    procedure InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint); override;
    procedure Show; override;
    procedure Hide; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(APolygon: TArrayOfDoublePoint); override;
  end;


implementation

uses
  SysUtils,
  GR32,
  i_ImageResamplerFactory,
  u_ThreadGenPrevZoom,
  u_GlobalState,
  u_ResStrings,
  u_MapType;

{ TProviderTilesGenPrev }

destructor TProviderTilesGenPrev.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TProviderTilesGenPrev.GetCaption: string;
begin
  Result := SAS_STR_OperationGenPrevCaption;
end;

procedure TProviderTilesGenPrev.InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint);
begin
  if FFrame = nil then begin
    FFrame := TfrTilesGenPrev.Create(
      nil,
      FMainMapsConfig,
      FFullMapsSet,
      FGUIConfigList
    );
    FFrame.Visible := False;
    FFrame.Parent := FParent;
  end;
  FFrame.Init(Azoom);
end;

procedure TProviderTilesGenPrev.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TProviderTilesGenPrev.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TProviderTilesGenPrev.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TProviderTilesGenPrev.StartProcess(APolygon: TArrayOfDoublePoint);
var
  i:integer;
  VInZooms: TArrayOfByte;
  VMapType: TMapType;
  VZoomsCount: Integer;
  VFromZoom: Byte;
  VResampler: IImageResamplerFactory;
begin
  inherited;
  VMapType:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  VFromZoom := FFrame.cbbFromZoom.ItemIndex + 1;
  VZoomsCount := 0;
  for i:=0 to FFrame.cbbFromZoom.ItemIndex do begin
    if FFrame.chklstZooms.ItemEnabled[i] then begin
      if FFrame.chklstZooms.Checked[i] then begin
        SetLength(VInZooms, VZoomsCount + 1);
        VInZooms[VZoomsCount] := FFrame.cbbFromZoom.ItemIndex - i;
        Inc(VZoomsCount);
      end;
    end;
  end;
  try
    VResampler := GState.ImageResamplerConfig.GetList.Items[FFrame.cbbResampler.ItemIndex];
  except
    VResampler := GState.ImageResamplerConfig.GetActiveFactory;
  end;

  TThreadGenPrevZoom.Create(
    VFromZoom,
    VInZooms,
    APolygon,
    VMapType,
    FFrame.chkReplace.Checked,
    FFrame.chkSaveFullOnly.Checked,
    FFrame.chkFromPrevZoom.Checked,
    Color32(GState.ViewConfig.BackGroundColor),
    VResampler
  );
end;

end.

