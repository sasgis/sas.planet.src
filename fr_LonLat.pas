{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit fr_LonLat;

interface

uses
  Windows,
  SysUtils,
  StrUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  u_CommonFormAndFrameParents,
  i_LocalCoordConverter,
  t_GeoTypes,
  i_ValueToStringConverter,
  i_ViewPortState;

type
  TTileSelectStyle = (tssCenter, tssTopLeft, tssBottomRight);

  TfrLonLat = class(TFrame)
    pnlTop: TPanel;
    cbbCoordType: TComboBox;
    pnlXY: TPanel;
    grdpnlLonLat: TGridPanel;
    lblLat: TLabel;
    lblLon: TLabel;
    edtLat: TEdit;
    edtLon: TEdit;
    lblZoom: TLabel;
    cbbZoom: TComboBox;
    grdpnlXY: TGridPanel;
    lblY: TLabel;
    edtX: TEdit;
    edtY: TEdit;
    lblX: TLabel;
    grdpnlZoom: TGridPanel;
    procedure cbbCoordTypeSelect(Sender: TObject);
  private
    FCoordinates: TDoublePoint;
    FViewPortState: IViewPortState;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FTileSelectStyle: TTileSelectStyle;
    function GetLonLat: TDoublePoint;
    procedure SetLonLat(const Value: TDoublePoint);
    function Edit2Digit(Atext:string; lat:boolean; var res:Double): boolean;
  public
    constructor Create(
      AOwner: TComponent;
      AViewPortState: IViewPortState;
      AValueToStringConverterConfig: IValueToStringConverterConfig;
      ATileSelectStyle: TTileSelectStyle
    ); reintroduce;
    property LonLat: TDoublePoint read GetLonLat write SetLonLat;
  end;

implementation

uses
  u_GeoFun,
  u_GeoToStr,
  u_ResStrings;

{$R *.dfm}

{ TfrLonLat }
procedure TfrLonLat.cbbCoordTypeSelect(Sender: TObject);
begin
  SetLonLat(FCoordinates);
  case cbbCoordType.ItemIndex of
   0:   begin
          pnlXY.Visible := False;
          grdpnlLonLat.Visible := True;
          grdpnlLonLat.Realign;
        end;
   1,2: begin
          pnlXY.Visible := True;
          grdpnlLonLat.Visible := False;
          grdpnlXY.Realign;
        end;
  end;
end;

constructor TfrLonLat.Create(AOwner: TComponent; AViewPortState: IViewPortState;
  AValueToStringConverterConfig: IValueToStringConverterConfig; ATileSelectStyle: TTileSelectStyle);
begin
  inherited Create(AOwner);
  FViewPortState := AViewPortState;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FTileSelectStyle:=ATileSelectStyle;
end;

function TfrLonLat.Edit2Digit(Atext:string; lat:boolean; var res:Double): boolean;
var i,delitel:integer;
    gms:double;
    text:string;
    minus: boolean;
begin
  result:=true;
  res:=0;
  text:=Atext;

  text:=StringReplace(text,'S','-',[rfReplaceAll]);
  text:=StringReplace(text,'W','-',[rfReplaceAll]);
  text:=StringReplace(text,'N','+',[rfReplaceAll]);
  text:=StringReplace(text,'E','+',[rfReplaceAll]);
  text:=StringReplace(text,'Ю','-',[rfReplaceAll]);
  text:=StringReplace(text,'З','-',[rfReplaceAll]);
  text:=StringReplace(text,'В','+',[rfReplaceAll]);
  text:=StringReplace(text,'С','+',[rfReplaceAll]);
  minus:= false;
  if posEx('-',text,1)>0 then minus := true;

  i:=1;
  while i<=length(text) do begin
    if (not(text[i] in ['0'..'9','-','+','.',',',' '])) then begin
      text[i]:=' ';
      dec(i);
    end;

    if ((i=1)and(text[i]=' '))or
       ((i=length(text))and(text[i]=' '))or
       ((i<length(text)-1)and(text[i]=' ')and(text[i+1]=' '))or
       ((i>1) and (text[i]=' ') and (not(text[i-1] in ['0'..'9'])))or
       ((i<length(text)-1)and(text[i]=',')and(text[i+1]=' ')) then begin
      Delete(text,i,1);
      dec(i);
    end;
    inc(i);
  end;

  try
    res:=0;
    delitel:=1;
    repeat
     i:=posEx(' ',text,1);
     if i=0 then begin
       gms:=str2r(text);
     end else begin
       gms:=str2r(copy(text,1,i-1));
       Delete(text,1,i);
     end;
     if ((delitel>1)and(abs(gms)>60))or
        ((delitel=1)and(lat)and(abs(gms)>90))or
        ((delitel=1)and(not lat)and(abs(gms)>180)) then begin
       Result:=false;
     end;
     if res<0 then begin
       res:=res-gms/delitel;
     end else begin
       res:=res+gms/delitel;
     end;
     if minus and (res>0) then res:=-res;
     delitel:=delitel*60;
    until (i=0)or(delitel>3600)or(result=false);
  except
    result:=false;
  end;
end;

function TfrLonLat.GetLonLat: TDoublePoint;
var
  VLocalConverter: ILocalCoordConverter;
  VTile: TPoint;
  XYPoint: TDoublePoint;
  VZoom: Byte;
begin
  case cbbCoordType.ItemIndex of
   0: begin
        if not(Edit2Digit(edtLat.Text,true,Result.y))or
           not(Edit2Digit(edtLon.Text,false,Result.x)) then begin
          ShowMessage(SAS_ERR_CoordinatesInput);
        end;
      end;
   1: begin
        try
          XYPoint.X := strtoint(edtX.Text);
          XYPoint.Y := strtoint(edtY.Text);
        except
          ShowMessage(SAS_ERR_CoordinatesInput);
        end;
        VLocalConverter :=  FViewPortState.GetVisualCoordConverter;
        VZoom := cbbZoom.ItemIndex;
        VLocalConverter.GeoConverter.CheckPixelPosFloat(XYPoint, VZoom, False);
        Result := VLocalConverter.GetGeoConverter.PixelPosFloat2LonLat(XYPoint, VZoom);
      end;
   2: begin
        try
          VTile.X := strtoint(edtX.Text);
          VTile.Y := strtoint(edtY.Text);
        except
          ShowMessage(SAS_ERR_CoordinatesInput);
        end;
        VLocalConverter :=  FViewPortState.GetVisualCoordConverter;
        VZoom := cbbZoom.ItemIndex;

        case FTileSelectStyle of
          tssCenter: XYPoint := DoublePoint(VTile.X + 0.5, VTile.Y + 0.5);
          tssTopLeft: XYPoint := DoublePoint(VTile);
          tssBottomRight: XYPoint := DoublePoint(VTile.X + 1, VTile.Y + 1);
        end;

        VLocalConverter.GeoConverter.CheckTilePos(VTile, VZoom, False);
        Result := VLocalConverter.GeoConverter.TilePosFloat2LonLat(XYPoint, VZoom);
      end;
  end;
end;

procedure TfrLonLat.SetLonLat(const Value: TDoublePoint);
var
  VValueConverter: IValueToStringConverter;
  XYPoint:TPoint;
  CurrZoom:integer;
  VLocalConverter: ILocalCoordConverter;
begin
  FCoordinates:=Value;
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  CurrZoom:=FViewPortState.GetCurrentZoom;
  cbbZoom.ItemIndex:=CurrZoom;
  if cbbCoordType.ItemIndex=-1 then begin
    cbbCoordType.ItemIndex:=0;
  end;

  case cbbCoordType.ItemIndex of
   0: begin
        edtLon.Text:=VValueConverter.LonConvert(Value.x);
        edtLat.Text:=VValueConverter.LatConvert(Value.y);
      end;
   1: begin
        VLocalConverter :=  FViewPortState.GetVisualCoordConverter;
        XYPoint:=VLocalConverter.GetGeoConverter.LonLat2PixelPos(Value,CurrZoom);
        edtX.Text:=inttostr(XYPoint.x);
        edtY.Text:=inttostr(XYPoint.y);
      end;
   2: begin
        VLocalConverter :=  FViewPortState.GetVisualCoordConverter;
        XYPoint:=VLocalConverter.GetGeoConverter.LonLat2TilePos(Value,CurrZoom);
        edtX.Text:=inttostr(XYPoint.x);
        edtY.Text:=inttostr(XYPoint.y);
      end;
  end;
end;

end.
