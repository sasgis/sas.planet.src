unit fr_LonLat;

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
  Mask,
  rxToolEdit,
  rxCurrEdit,
  u_CommonFormAndFrameParents,
  t_GeoTypes;

type
  TfrLonLat = class(TFrame)
    lblLat: TLabel;
    cbbLatNS: TComboBox;
    edtLatDeg: TCurrencyEdit;
    edtLatMin: TCurrencyEdit;
    edtLatSec: TCurrencyEdit;
    cbbLonWE: TComboBox;
    edtLonDeg: TCurrencyEdit;
    edtLonMin: TCurrencyEdit;
    edtLonSec: TCurrencyEdit;
    lblLon: TLabel;
    flwpnlLat: TFlowPanel;
    flwpnlLon: TFlowPanel;
    grdpnlMain: TGridPanel;
  private
    function GetLonLat: TDoublePoint;
    procedure SetLonLat(const Value: TDoublePoint);
  public
    property LonLat: TDoublePoint read GetLonLat write SetLonLat;
  end;

implementation

uses
  Ugeofun;

{$R *.dfm}

{ TfrLonLat }

function TfrLonLat.GetLonLat: TDoublePoint;
begin
  Result.X:=DMS2G(edtLonDeg.Value,edtLonMin.Value,edtLonSec.Value,cbbLonWE.Itemindex=1);
  Result.Y:=DMS2G(edtLatDeg.Value,edtLatMin.Value,edtLatSec.Value,cbbLatNS.Itemindex=1);
end;

procedure TfrLonLat.SetLonLat(const Value: TDoublePoint);
var
  DMS:TDMS;
begin
  DMS:=D2DMS(Value.y);
  edtLatDeg.Value:=DMS.D;
  edtLatMin.Value:=DMS.M;
  edtLatSec.Value:=DMS.S;
  if DMS.N then begin
    cbbLatNS.ItemIndex:=1
  end else begin
    cbbLatNS.ItemIndex:=0;
  end;
  DMS:=D2DMS(Value.x);
  edtLonDeg.Value:=DMS.D;
  edtLonMin.Value:=DMS.M;
  edtLonSec.Value:=DMS.S;
  if DMS.N then begin
    cbbLonWE.ItemIndex:=1
  end else begin
    cbbLonWE.ItemIndex:=0;
  end;
end;

end.
