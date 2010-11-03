unit u_ExportProviderZip;

interface

uses
  Forms,
  t_GeoTypes,
  u_ExportProviderAbstract,
  fr_ExportToFileCont;

type
  TExportProviderZip = class(TExportProviderAbstract)
  private
    FFrame: TfrExportToFileCont;
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
  i_ITileFileNameGenerator,
  u_GlobalState,
  u_ThreadExportToZip,
  UMapType;

{ TExportProviderKml }

destructor TExportProviderZip.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TExportProviderZip.GetCaption: string;
begin
  Result := _('”паковка в Zip');
end;

procedure TExportProviderZip.InitFrame(Azoom: byte; APolygon: TDoublePointArray);
begin
  if FFrame = nil then begin
    FFrame := TfrExportToFileCont.CreateForFileType(nil, 'Zip |*.zip', 'zip');
    FFrame.Visible := False;
    FFrame.Parent := FParent;
  end;
  FFrame.Init;
end;

procedure TExportProviderZip.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TExportProviderZip.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TExportProviderZip.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TExportProviderZip.StartProcess(APolygon: TDoublePointArray);
var
  i:integer;
  path:string;
  Zoomarr:array [0..23] of boolean;
  VMapType: TMapType;
  VNameGenerator: ITileFileNameGenerator;
begin
  inherited;
  for i:=0 to 23 do begin
    ZoomArr[i]:= FFrame.chklstZooms.Checked[i];
  end;
  VMapType:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  path:=FFrame.edtTargetFile.Text;
  VNameGenerator := GState.TileNameGenerator.GetGenerator(FFrame.cbbNamesType.ItemIndex + 1);
  TThreadExportToZip.Create(path, APolygon, Zoomarr, VMapType, VNameGenerator);
end;

end.

