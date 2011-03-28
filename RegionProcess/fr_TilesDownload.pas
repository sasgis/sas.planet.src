unit fr_TilesDownload;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  t_GeoTypes,
  u_CommonFormAndFrameParents;

type
  TfrTilesDownload = class(TFrame)
    lblZoom: TLabel;
    lblStat: TLabel;
    chkReplace: TCheckBox;
    chkReplaceIfDifSize: TCheckBox;
    chkReplaceOlder: TCheckBox;
    dtpReplaceOlderDate: TDateTimePicker;
    cbbMap: TComboBox;
    cbbZoom: TComboBox;
    chkTryLoadIfTNE: TCheckBox;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlRight: TPanel;
    pnlMain: TPanel;
    pnlTileReplaceCondition: TPanel;
    pnlReplaceOlder: TPanel;
    lblReplaceOlder: TLabel;
    lblMap: TLabel;
    Bevel1: TBevel;
    procedure chkReplaceClick(Sender: TObject);
    procedure chkReplaceOlderClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
  private
    FPolygLL: TArrayOfDoublePoint;
  public
    procedure Init(AZoom: Byte; APolygLL: TArrayOfDoublePoint);
  end;

implementation

uses
  u_GlobalState,
  u_GeoFun,
  u_ResStrings,
  u_MapType;

{$R *.dfm}

procedure TfrTilesDownload.cbbZoomChange(Sender: TObject);
var
  polyg:TArrayOfPoint;
  min,max:TPoint;
  numd:int64 ;
  Vmt: TMapType;
  VZoom: byte;
begin
  if cbbMap.ItemIndex >= 0 then begin
    Vmt := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
    VZoom := cbbZoom.ItemIndex;
    polyg := Vmt.GeoConvert.LonLatArray2PixelArray(FPolygLL, VZoom);
    numd:=GetDwnlNum(min,max,polyg,true);
    lblStat.Caption:=SAS_STR_filesnum+': '+inttostr((max.x-min.x)div 256+1)+'x'
                    +inttostr((max.y-min.y)div 256+1)+'('+inttostr(numd)+')';
    GetMinMax(min,max,polyg,false);
    lblStat.Caption:=lblStat.Caption+', '+SAS_STR_Resolution+' '+inttostr(max.x-min.x)+'x'
                  +inttostr(max.y-min.y);
  end;
end;

procedure TfrTilesDownload.chkReplaceClick(Sender: TObject);
var
  VEnabled: Boolean;
begin
  VEnabled := chkReplace.Checked;
  chkReplaceIfDifSize.Enabled := VEnabled;
  chkReplaceOlder.Enabled := VEnabled;
  chkReplaceOlderClick(chkReplaceOlder);
end;

procedure TfrTilesDownload.chkReplaceOlderClick(Sender: TObject);
begin
  dtpReplaceOlderDate.Enabled := chkReplaceOlder.Enabled and chkReplaceOlder.Checked;
end;

procedure TfrTilesDownload.Init(AZoom: Byte; APolygLL: TArrayOfDoublePoint);
var
  i: integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
begin
  FPolygLL := APolygLL;
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;

  VActiveMapGUID := GState.MainFormConfig.MainMapsConfig.GetActiveMap.GetSelectedGUID;
  cbbMap.items.Clear;
  For i:=0 to GState.MapType.Count - 1 do begin
    VMapType := GState.MapType[i];
    if (VMapType.UseDwn)and(VMapType.Enabled) then begin
      VAddedIndex := cbbMap.Items.AddObject(VMapType.name,VMapType);
      if IsEqualGUID(VMapType.GUID, VActiveMapGUID) then begin
        cbbMap.ItemIndex:=VAddedIndex;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
  dtpReplaceOlderDate.Date:=now;
  cbbZoomChange(nil);
end;

end.
