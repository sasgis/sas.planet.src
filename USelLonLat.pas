unit USelLonLat;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  Mask,
  StdCtrls,
  rxCurrEdit,
  rxToolEdit,
  Ugeofun,
  UResStrings;

type
  TFSelLonLat = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label21: TLabel;
    Label22: TLabel;
    _lat_ns: TComboBox;
    _Lon_we: TComboBox;
    _lat2: TCurrencyEdit;
    _lat3: TCurrencyEdit;
    _lon1: TCurrencyEdit;
    _lon2: TCurrencyEdit;
    _lon3: TCurrencyEdit;
    _Lat1: TCurrencyEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lat_ns: TComboBox;
    Lon_we: TComboBox;
    lat2: TCurrencyEdit;
    lat3: TCurrencyEdit;
    lon1: TCurrencyEdit;
    lon2: TCurrencyEdit;
    lon3: TCurrencyEdit;
    Lat1: TCurrencyEdit;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
    lon_k,lat_k,_lon_k,_lat_k:extended;
    function Execute: Boolean;
  end;

var
  FSelLonLat: TFSelLonLat;

implementation



{$R *.dfm}

function TFSelLonLat.Execute: Boolean;
begin
 result:=false;
 ShowModal;
 if ModalResult = mrOK then
  begin
   lon_k:=DMS2G(lon1.Value,lon2.Value,lon3.Value,lon_we.Itemindex=1);
   lat_k:=DMS2G(lat1.Value,lat2.Value,lat3.Value,lat_ns.Itemindex=1);
   _lon_k:=DMS2G(_lon1.Value,_lon2.Value,_lon3.Value,_lon_we.Itemindex=1);
   _lat_k:=DMS2G(_lat1.Value,_lat2.Value,_lat3.Value,_lat_ns.Itemindex=1);
   if (_lon_k>lon_k)then begin
                         ShowMessage(SAS_ERR_LonLat1);
                         result:=false;
                         exit;
                        end;
   if (-_lat_k>-lat_k)then begin
                         ShowMessage(SAS_ERR_LonLat2);
                         result:=false;
                         exit;
                        end;
   FSelLonLat.Visible:=false;
   result:=true;
  end;
end;

procedure TFSelLonLat.Button2Click(Sender: TObject);
begin
 ModalResult := mrCancel;
end;

procedure TFSelLonLat.Button1Click(Sender: TObject);
begin
 ModalResult := mrOK;
end;

end.
