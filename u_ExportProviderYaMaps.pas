unit u_ExportProviderYaMaps;

interface

uses
  Forms,
  t_GeoTypes,
  u_ExportProviderAbstract,
  fr_ExportYaMaps;

type
  TExportProviderYaMaps = class(TExportProviderAbstract)
  private
    FFrame: TfrExportYaMaps;
  public
    destructor Destroy; override;
    function GetCaption: string; override;
    procedure InitFrame(Azoom: byte; APolygon: TExtendedPointArray); override;
    procedure Show; override;
    procedure Hide; override;
    procedure StartProcess(APolygon: TExtendedPointArray); override;
  end;


implementation

uses
  SysUtils,
  u_ThreadExportYaMaps,
  UMapType;

{ TExportProviderYaMaps }

destructor TExportProviderYaMaps.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TExportProviderYaMaps.GetCaption: string;
begin
  Result := 'Мобильные Яндекс.Карты (версия 3)';
end;

procedure TExportProviderYaMaps.InitFrame(Azoom: byte; APolygon: TExtendedPointArray);
begin
  if FFrame = nil then begin
    FFrame := TfrExportYaMaps.Create(nil);
    FFrame.Visible := False;
    FFrame.Parent := FParent;
  end;
  FFrame.Init;
end;

procedure TExportProviderYaMaps.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TExportProviderYaMaps.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TExportProviderYaMaps.StartProcess(APolygon: TExtendedPointArray);
var
  i:integer;
  path:string;
  Zoomarr:array [0..23] of boolean;
  typemaparr:array of TMapType;
  comprSat,comprMap:byte;
  Replace:boolean;
begin
  inherited;
  for i:=0 to 23 do ZoomArr[i]:= FFrame.chklstZooms.Checked[i];
  setlength(typemaparr,3);
  typemaparr[0]:=TMapType(FFrame.cbbSat.Items.Objects[FFrame.cbbSat.ItemIndex]);
  typemaparr[1]:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  typemaparr[2]:=TMapType(FFrame.cbbHybr.Items.Objects[FFrame.cbbHybr.ItemIndex]);
  comprSat:=FFrame.seSatCompress.Value;
  comprMap:=FFrame.seMapCompress.Value;
  path:=IncludeTrailingPathDelimiter(FFrame.edtTargetPath.Text);
  Replace:=FFrame.chkReplaseTiles.Checked;
  TThreadExportYaMaps.Create(path,APolygon,ZoomArr,typemaparr,Replace,comprSat,comprMap);
end;

end.
