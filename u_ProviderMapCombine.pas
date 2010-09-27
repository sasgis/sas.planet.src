unit u_ProviderMapCombine;

interface

uses
  Windows,
  Forms,
  t_GeoTypes,
  u_ExportProviderAbstract,
  fr_MapCombine;

type
  TProviderMapCombine = class(TExportProviderAbstract)
  private
    FFrame: TfrMapCombine;
  public
    destructor Destroy; override;
    function GetCaption: string; override;
    procedure InitFrame(Azoom: byte; APolygon: TExtendedPointArray); override;
    procedure Show; override;
    procedure Hide; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(APolygon: TExtendedPointArray); override;
  end;


implementation

uses
  Classes,
  SysUtils,
  gnugettext,
  u_GlobalState,
  u_ThreadMapCombineBMP,
  u_ThreadMapCombineECW,
  u_ThreadMapCombineJPG,
  u_ThreadMapCombineKMZ,
  UResStrings,
  UMapType;

{ TProviderTilesDelete }

destructor TProviderMapCombine.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TProviderMapCombine.GetCaption: string;
begin
  Result := _('Склейка карты');
end;

procedure TProviderMapCombine.InitFrame(Azoom: byte; APolygon: TExtendedPointArray);
begin
  if FFrame = nil then begin
    FFrame := TfrMapCombine.Create(nil);
    FFrame.Visible := False;
    FFrame.Parent := FParent;
  end;
  FFrame.Init(Azoom, APolygon);
end;

procedure TProviderMapCombine.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TProviderMapCombine.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TProviderMapCombine.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TProviderMapCombine.StartProcess(APolygon: TExtendedPointArray);
var
  Amt,Hmt:TMapType;
  i:integer;
  VPrTypes: IInterfaceList;
  VFileName: string;
  VSplitCount: TPoint;
  VFileExt: string;
begin
  Amt:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  Hmt:=TMapType(FFrame.cbbHybr.Items.Objects[FFrame.cbbHybr.ItemIndex]);
  VFileName := FFrame.edtTargetFile.Text;
  VPrTypes := TInterfaceList.Create;
  for i:=0 to FFrame.chklstPrTypes.Items.Count-1 do begin
    if FFrame.chklstPrTypes.Checked[i] then begin
      VPrTypes.Add(IInterface(Pointer(FFrame.chklstPrTypes.Items.Objects[i])));
    end;
  end;
  VSplitCount.X := FFrame.seSplitHor.Value;
  VSplitCount.Y := FFrame.seSplitVert.Value;
  VFileExt := UpperCase(ExtractFileExt(VFileName));
  if (VFileExt='.ECW')or(VFileExt='.JP2') then begin
    TThreadMapCombineECW.Create(
      VPrTypes,
      VFileName,
      APolygon,
      VSplitCount,
      FFrame.cbbZoom.ItemIndex+1,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      FFrame.chkUseMapMarks.Checked,
      FFrame.seJpgQuality.Value
    );
  end else if (VFileExt='.BMP') then begin
    TThreadMapCombineBMP.Create(
      VPrTypes,
      VFileName,
      APolygon,
      VSplitCount,
      FFrame.cbbZoom.ItemIndex+1,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      FFrame.chkUseMapMarks.Checked
    );
  end else if (VFileExt='.KMZ') then begin
    TThreadMapCombineKMZ.Create(
      VPrTypes,
      VFileName,
      APolygon,
      VSplitCount,
      FFrame.cbbZoom.ItemIndex+1,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      FFrame.chkUseMapMarks.Checked,
      FFrame.seJpgQuality.Value
    );
  end else begin
    TThreadMapCombineJPG.Create(
      VPrTypes,
      VFileName,
      APolygon,
      VSplitCount,
      FFrame.cbbZoom.ItemIndex+1,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      FFrame.chkUseMapMarks.Checked,
      FFrame.seJpgQuality.Value
    );
  end;
end;

end.

