unit u_ExportProviderAUX;

interface

uses
  Forms,
  t_GeoTypes,
  u_ExportProviderAbstract,
  fr_ExportAUX;

type
  TExportProviderAUX = class(TExportProviderAbstract)
  private
    FFrame: TfrExportAUX;
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
  u_ThreadExportToAUX,
  u_ResStrings,
  u_MapType;

{ TExportProviderKml }

destructor TExportProviderAUX.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TExportProviderAUX.GetCaption: string;
begin
  Result := SAS_STR_ExportAUXGeoServerCaption;
end;

procedure TExportProviderAUX.InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint);
begin
  if FFrame = nil then begin
    FFrame := TfrExportAUX.Create(nil);
    FFrame.Visible := False;
    FFrame.Parent := FParent;
  end;
  FFrame.Init(Azoom);
end;

procedure TExportProviderAUX.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TExportProviderAUX.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TExportProviderAUX.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TExportProviderAUX.StartProcess(APolygon: TArrayOfDoublePoint);
var
  path:string;
  VMapType: TMapType;
begin
  inherited;
  VMapType:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  path:=FFrame.edtTargetFile.Text;
  if FFrame.cbbZoom.ItemIndex < 0 then begin
    FFrame.cbbZoom.ItemIndex := 0;
  end;
  TThreadExportToAUX.Create(APolygon,FFrame.cbbZoom.ItemIndex,VMapType,path)
end;

end.

