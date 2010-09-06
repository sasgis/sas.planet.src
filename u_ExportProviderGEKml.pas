unit u_ExportProviderGEKml;

interface

uses
  Forms,
  t_GeoTypes,
  u_ExportProviderAbstract,
  fr_ExportGEKml;

type
  TExportProviderGEKml = class(TExportProviderAbstract)
  private
    FFrame: TfrExportGEKml;
  public
    destructor Destroy; override;
    function GetCaption: string; override;
    procedure InitFrame(Azoom: byte); override;
    procedure Show; override;
    procedure Hide; override;
    procedure StartProcess(APolygon: TExtendedPointArray); override;
  end;


implementation

uses
  SysUtils,
  u_ThreadExportKML,
  UMapType;

{ TExportProviderKml }

destructor TExportProviderGEKml.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TExportProviderGEKml.GetCaption: string;
begin
  Result := 'KML (Для просмотра в GE)';
end;

procedure TExportProviderGEKml.InitFrame(Azoom: byte);
begin
  if FFrame = nil then begin
    FFrame := TfrExportGEKml.Create(nil);
    FFrame.Visible := False;
    FFrame.Parent := FParent;
  end;
  FFrame.Init;
end;

procedure TExportProviderGEKml.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TExportProviderGEKml.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TExportProviderGEKml.StartProcess(APolygon: TExtendedPointArray);
var
  i:integer;
  path:string;
  Zoomarr:array [0..23] of boolean;
  VMapType: TMapType;
  comprSat,comprMap:byte;
  NotSaveNotExists: boolean;
  RelativePath: Boolean;
begin
  inherited;
  for i:=0 to 23 do ZoomArr[i]:=FFrame.chklstZooms.Checked[i];
  VMapType:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  path:=FFrame.edtTargetFile.Text;
  RelativePath:=FFrame.chkUseRelativePath.Checked;
  NotSaveNotExists:=FFrame.chkNotSaveNotExists.Checked;
  TThreadExportKML.Create(path,APolygon,ZoomArr,VMapType,NotSaveNotExists,RelativePath)
end;

end.

