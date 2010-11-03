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
    procedure InitFrame(Azoom: byte; APolygon: TDoublePointArray); override;
    procedure Show; override;
    procedure Hide; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(APolygon: TDoublePointArray); override;
  end;


implementation

uses
  SysUtils,
  gnugettext,
  u_ThreadExportToAUX,
  UMapType;

{ TExportProviderKml }

destructor TExportProviderAUX.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TExportProviderAUX.GetCaption: string;
begin
  Result := _('AUX для GeoExpress Server');
end;

procedure TExportProviderAUX.InitFrame(Azoom: byte; APolygon: TDoublePointArray);
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

procedure TExportProviderAUX.StartProcess(APolygon: TDoublePointArray);
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

