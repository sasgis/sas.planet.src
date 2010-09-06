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
    function GetCaption: string; override;
    function GetDialogFrame(Azoom: byte): TFrame; override;
    procedure StartProcess(APolygon: TExtendedPointArray); override;
  end;


implementation

uses
  SysUtils,
  u_ThreadExportYaMaps,
  UMapType;

{ TExportProviderYaMaps }

function TExportProviderYaMaps.GetCaption: string;
begin
  Result := 'Мобильные Яндекс.Карты (версия 3)';
end;

function TExportProviderYaMaps.GetDialogFrame(Azoom: byte): TFrame;
begin
  if FFrame = nil then begin
    FFrame := TfrExportYaMaps.Create(FOwner);
    FFrame.Parent := FParent;
  end;
  FFrame.Init;
  Result := FFrame;
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
  comprSat:=FFrame.seSatCompr.Value;
  comprMap:=FFrame.seMapCompr.Value;
  path:=IncludeTrailingPathDelimiter(FFrame.edtTargetPath.Text);
  Replace:=FFrame.chkReplaseTiles.Checked;
  TThreadExportYaMaps.Create(path,APolygon,ZoomArr,typemaparr,Replace,comprSat,comprMap);
end;

end.
