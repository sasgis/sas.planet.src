unit u_ExportProviderIPhone;

interface

uses
  Classes,
  Controls,
  Forms,
  t_GeoTypes,
  u_ExportProviderAbstract,
  fr_ExportIPhone;

type
  TExportProviderIPhone = class(TExportProviderAbstract)
  private
    FFrame: TfrExportIPhone;
    FNewFormat: Boolean;
  public
    constructor Create(AParent: TWinControl; ANewFormat: Boolean);
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
  u_ThreadExportIPhone,
  u_ResStrings,
  UMapType;

{ TExportProviderIPhone }

constructor TExportProviderIPhone.Create(
  AParent: TWinControl; ANewFormat: Boolean);
begin
  inherited Create(AParent);
  FNewFormat := ANewFormat;
end;

destructor TExportProviderIPhone.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TExportProviderIPhone.GetCaption: string;
begin
  if FNewFormat then begin
    Result := SAS_STR_ExportIPhone128Caption;
  end else begin
    Result := SAS_STR_ExportIPhone64Caption;
  end;
end;

procedure TExportProviderIPhone.InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint);
begin
  if FFrame = nil then begin
    FFrame := TfrExportIPhone.Create(nil);
    FFrame.Visible := False;
    FFrame.Parent := FParent;
  end;
  FFrame.Init;
end;

procedure TExportProviderIPhone.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TExportProviderIPhone.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TExportProviderIPhone.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TExportProviderIPhone.StartProcess(APolygon: TArrayOfDoublePoint);
var
  i:integer;
  path:string;
  Zoomarr:array [0..23] of boolean;
  typemaparr:array of TMapType;
  comprSat,comprMap,comprHyb:byte;
  Replace:boolean;
  VActiveMapIndex: Integer;
begin
  inherited;
  for i:=0 to 23 do ZoomArr[i]:= FFrame.chklstZooms.Checked[i];
  setlength(typemaparr,3);
  VActiveMapIndex := 0;
  typemaparr[0]:=TMapType(FFrame.cbbSat.Items.Objects[FFrame.cbbSat.ItemIndex]);
  if typemaparr[0]<>nil then begin
    if FFrame.rbSat.Checked then begin
      VActiveMapIndex := 0;
    end;
  end;
  typemaparr[1]:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  if typemaparr[1]<>nil then begin
    if FFrame.rbMap.Checked then begin
      VActiveMapIndex := 1;
    end;
  end;
  typemaparr[2]:=TMapType(FFrame.cbbHybr.Items.Objects[FFrame.cbbHybr.ItemIndex]);
  if typemaparr[2]<>nil then begin
    if FFrame.rbHybr.Checked then begin
      VActiveMapIndex := 2;
    end;
  end;
  comprSat:=FFrame.seSatCompress.Value;
  comprMap:=FFrame.seMapCompress.Value;
  comprHyb:=FFrame.seHybrCompress.Value;
  path:=IncludeTrailingPathDelimiter(FFrame.edtTargetPath.Text);
  Replace:=FFrame.chkAppendTilse.Checked;
  TThreadExportIPhone.Create(path,APolygon,ZoomArr,typemaparr,VActiveMapIndex,Replace,FNewFormat,comprSat,comprMap,comprHyb)
end;

end.

