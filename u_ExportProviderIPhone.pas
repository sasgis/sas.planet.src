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
    constructor Create(AOwner: TComponent; AParent: TWinControl; ANewFormat: Boolean);
    function GetCaption: string; override;
    function GetDialogFrame(Azoom: byte): TFrame; override;
    procedure StartProcess(APolygon: TExtendedPointArray); override;
  end;


implementation

uses
  SysUtils,
  u_ThreadExportIPhone,
  UMapType;

{ TExportProviderIPhone }

constructor TExportProviderIPhone.Create(AOwner: TComponent;
  AParent: TWinControl; ANewFormat: Boolean);
begin
  inherited Create(AOwner, AParent);
  FNewFormat := ANewFormat;
end;

function TExportProviderIPhone.GetCaption: string;
begin
  if FNewFormat then begin
    Result := 'iPhone (2.2 и выше 128х128)';
  end else begin
    Result := 'iPhone (Ниже v2.2 64х64)';
  end;
end;

function TExportProviderIPhone.GetDialogFrame(Azoom: byte): TFrame;
begin
  if FFrame = nil then begin
    FFrame := TfrExportIPhone.Create(FOwner);
    FFrame.Parent := FParent;
  end;
  FFrame.Init;
  Result := FFrame;
end;

procedure TExportProviderIPhone.StartProcess(APolygon: TExtendedPointArray);
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

