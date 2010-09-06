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
    function GetCaption: string; override;
    function GetDialogFrame(Azoom: byte): TFrame; override;
    procedure StartProcess(APolygon: TExtendedPointArray); override;
  end;


implementation

uses
  SysUtils,
  u_ThreadExportKML,
  UMapType;

{ TExportProviderKml }

function TExportProviderGEKml.GetCaption: string;
begin
  Result := 'KML (Для просмотра в GE)';
end;

function TExportProviderGEKml.GetDialogFrame(Azoom: byte): TFrame;
begin
  if FFrame = nil then begin
    FFrame := TfrExportGEKml.Create(FOwner);
    FFrame.Parent := FParent;
  end;
  FFrame.Init;
  Result := FFrame;
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

