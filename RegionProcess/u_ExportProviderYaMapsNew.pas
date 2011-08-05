unit u_ExportProviderYaMapsNew;

interface

uses
  Forms,
  t_GeoTypes,
  u_ExportProviderAbstract,
  fr_ExportYaMapsNew;

type
  TExportProviderYaMapsNew = class(TExportProviderAbstract)
  private
    FFrame: TfrExportYaMapsNew;
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
  u_ThreadExportYaMapsNew,
  u_ResStrings,
  u_MapType;

{ TExportProviderYaMaps }

destructor TExportProviderYaMapsNew.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TExportProviderYaMapsNew.GetCaption: string;
begin
  Result := SAS_STR_ExportYaMapsNewCaption;
end;

procedure TExportProviderYaMapsNew.InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint);
begin
  if FFrame = nil then begin
    FFrame := TfrExportYaMapsNew.Create(nil);
    FFrame.Visible := False;
    FFrame.Parent := FParent;
  end;
  FFrame.Init;
end;

procedure TExportProviderYaMapsNew.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TExportProviderYaMapsNew.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TExportProviderYaMapsNew.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TExportProviderYaMapsNew.StartProcess(APolygon: TArrayOfDoublePoint);
var
  i:integer;
  path:string;
  Zoomarr:array [0..23] of boolean;
  typemaparr:array of TMapType;
  comprSat,comprMap:byte;
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
  TThreadExportYaMapsNew.Create(path,APolygon,ZoomArr,typemaparr,true,comprSat,comprMap);
end;

end.
