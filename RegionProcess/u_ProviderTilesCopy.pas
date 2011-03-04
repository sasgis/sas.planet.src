unit u_ProviderTilesCopy;

interface

uses
  Windows,
  Forms,
  t_GeoTypes,
  u_ExportProviderAbstract,
  fr_TilesCopy;

type
  TProviderTilesCopy = class(TExportProviderAbstract)
  private
    FFrame: TfrTilesCopy;
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
  u_GlobalState,
  u_ThreadExportToFileSystem,
  UResStrings,
  UMapType;

{ TProviderTilesDelete }

destructor TProviderTilesCopy.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TProviderTilesCopy.GetCaption: string;
begin
  Result := SAS_STR_OperationTilesCopyCaption;
end;

procedure TProviderTilesCopy.InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint);
begin
  if FFrame = nil then begin
    FFrame := TfrTilesCopy.Create(nil);
    FFrame.Visible := False;
    FFrame.Parent := FParent;
  end;
  FFrame.Init;
end;

procedure TProviderTilesCopy.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TProviderTilesCopy.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TProviderTilesCopy.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TProviderTilesCopy.StartProcess(APolygon: TArrayOfDoublePoint);
var
  i:integer;
  path:string;
  Zoomarr:array [0..23] of boolean;
  typemaparr:array of TMapType;
  Replace:boolean;
begin
  for i:=0 to 23 do begin
    ZoomArr[i]:=FFrame.chklstZooms.Checked[i];
  end;
  for i:=0 to FFrame.chklstMaps.Items.Count-1 do begin
    if FFrame.chklstMaps.Checked[i] then begin
    setlength(typemaparr,length(typemaparr)+1);
    typemaparr[length(typemaparr)-1]:=TMapType(FFrame.chklstMaps.Items.Objects[i]);
    end;
  end;
  path:=IncludeTrailingPathDelimiter(FFrame.edtTargetPath.Text);
  Replace:=FFrame.chkReplaseTarget.Checked;

  TThreadExportToFileSystem.Create(
    path,
    APolygon,
    ZoomArr,
    typemaparr,
    FFrame.chkDeleteSource.Checked,
    Replace,
    GState.TileNameGenerator.GetGenerator(FFrame.cbbNamesType.ItemIndex + 1)
  )
end;

end.

