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
  t_GeoTypes,
  i_LanguageManager,
  i_ValueToStringConverter,
  i_LocalCoordConverterChangeable,
  u_CommonFormAndFrameParents;

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
    FViewPortState: ILocalCoordConverterChangeable;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FTileSelectStyle: TTileSelectStyle;
    function GetLonLat: TDoublePoint;
    procedure SetLonLat(const Value: TDoublePoint);
    function Edit2Digit(const Atext:string; lat:boolean; out res: Double): boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AViewPortState: ILocalCoordConverterChangeable;
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      ATileSelectStyle: TTileSelectStyle
    ); reintroduce;
    property LonLat: TDoublePoint read GetLonLat write SetLonLat;
    function Validate: Boolean;
  end;

implementation

uses
  i_LocalCoordConverter,
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

constructor TfrLonLat.Create(
  const ALanguageManager: ILanguageManager;
  const AViewPortState: ILocalCoordConverterChangeable;
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  ATileSelectStyle: TTileSelectStyle
);
begin
  inherited Create(ALanguageManager);
  FViewPortState := AViewPortState;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FTileSelectStyle:=ATileSelectStyle;
end;

function TfrLonLat.Edit2Digit(const Atext: string; lat: boolean; out res: Double): boolean;
var i,delitel:integer;
    gms:double;
    VText:string;
    minus: boolean;
begin
  result:=true;
  res:=0;
  VText:=UpperCase(Atext);

  VText:=StringReplace(VText,'S','-',[rfReplaceAll]);
  VText:=StringReplace(VText,'W','-',[rfReplaceAll]);
  VText:=StringReplace(VText,'N','+',[rfReplaceAll]);
  VText:=StringReplace(VText,'E','+',[rfReplaceAll]);
  VText:=StringReplace(VText,'Ю','-',[rfReplaceAll]);
  VText:=StringReplace(VText,'З','-',[rfReplaceAll]);
  VText:=StringReplace(VText,'В','+',[rfReplaceAll]);
  VText:=StringReplace(VText,'С','+',[rfReplaceAll]);
  minus:= false;
  if posEx('-',VText,1)>0 then minus := true;

  i:=1;
  while i<=length(VText) do begin
    if (not(VText[i] in ['0'..'9','-','+','.',',',' '])) then begin
      VText[i]:=' ';
      dec(i);
    end;

    if ((i=1)and(VText[i]=' '))or
       ((i=length(VText))and(VText[i]=' '))or
       ((i<length(VText)-1)and(VText[i]=' ')and(VText[i+1]=' '))or
       ((i>1) and (VText[i]=' ') and (not(VText[i-1] in ['0'..'9'])))or
       ((i<length(VText)-1)and(VText[i]=',')and(VText[i+1]=' ')) then begin
      Delete(VText,i,1);
      dec(i);
    end;
    inc(i);
  end;

  try
    res:=0;
    delitel:=1;
    repeat
     i:=posEx(' ',VText,1);
     if i=0 then begin
       gms:=str2r(VText);
     end else begin
       gms:=str2r(copy(VText,1,i-1));
       Delete(VText,1,i);
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
    until (i=0)or(delitel>3600)or(not result);
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
  Result := FCoordinates;
end;

procedure TfrLonLat.SetLonLat(const Value: TDoublePoint);
var
  VValueConverter: IValueToStringConverter;
  XYPoint:TPoint;
  CurrZoom:integer;
  VLocalConverter: ILocalCoordConverter;
begin
  FCoordinates := Value;
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  VLocalConverter :=  FViewPortState.GetStatic;
  CurrZoom := VLocalConverter.Zoom;
  cbbZoom.ItemIndex:=CurrZoom;
  if cbbCoordType.ItemIndex=-1 then begin
    cbbCoordType.ItemIndex:=0;
  end;

  case cbbCoordType.ItemIndex of
   0: begin
        edtLon.Text:=VValueConverter.LonConvert(Value.x, false);
        edtLat.Text:=VValueConverter.LatConvert(Value.y, false);
      end;
   1: begin
        XYPoint:=
          PointFromDoublePoint(
            VLocalConverter.GetGeoConverter.LonLat2PixelPosFloat(Value,CurrZoom),
            prToTopLeft
          );
        edtX.Text:=inttostr(XYPoint.x);
        edtY.Text:=inttostr(XYPoint.y);
      end;
   2: begin
        XYPoint:=
          PointFromDoublePoint(
            VLocalConverter.GetGeoConverter.LonLat2TilePosFloat(Value,CurrZoom),
            prToTopLeft
          );
        edtX.Text:=inttostr(XYPoint.x);
        edtY.Text:=inttostr(XYPoint.y);
      end;
  end;
end;

function TfrLonLat.Validate: Boolean;
var
  VLocalConverter: ILocalCoordConverter;
  VTile: TPoint;
  XYPoint: TDoublePoint;
  VZoom: Byte;
  VLonLat: TDoublePoint;
begin
  VLonLat := CEmptyDoublePoint;
  Result := True;
  case cbbCoordType.ItemIndex of
   0: begin
        if not(Edit2Digit(edtLat.Text, true, VLonLat.y))or
           not(Edit2Digit(edtLon.Text, false, VLonLat.x)) then begin
          ShowMessage(SAS_ERR_CoordinatesInput);
          Result := False;
        end;
        if Result then begin
          VLocalConverter :=  FViewPortState.GetStatic;
          VLocalConverter.GeoConverter.CheckLonLatPos(VLonLat);
        end;
      end;
   1: begin
        try
          XYPoint.X := strtoint(edtX.Text);
          XYPoint.Y := strtoint(edtY.Text);
        except
          ShowMessage(SAS_ERR_CoordinatesInput);
          Result := False;
        end;
        if Result then  begin
          VLocalConverter :=  FViewPortState.GetStatic;
          VZoom := cbbZoom.ItemIndex;
          VLocalConverter.GeoConverter.CheckPixelPosFloat(XYPoint, VZoom, False);
          VLonLat := VLocalConverter.GetGeoConverter.PixelPosFloat2LonLat(XYPoint, VZoom);
        end;
      end;
   2: begin
        try
          VTile.X := strtoint(edtX.Text);
          VTile.Y := strtoint(edtY.Text);
        except
          ShowMessage(SAS_ERR_CoordinatesInput);
          Result := False;
        end;
        if Result then begin
          VLocalConverter :=  FViewPortState.GetStatic;
          VZoom := cbbZoom.ItemIndex;

          case FTileSelectStyle of
            tssCenter: XYPoint := DoublePoint(VTile.X + 0.5, VTile.Y + 0.5);
            tssTopLeft: XYPoint := DoublePoint(VTile);
            tssBottomRight: XYPoint := DoublePoint(VTile.X + 1, VTile.Y + 1);
          end;

          VLocalConverter.GeoConverter.CheckTilePos(VTile, VZoom, False);
          VLonLat := VLocalConverter.GeoConverter.TilePosFloat2LonLat(XYPoint, VZoom);
        end;
      end;
  end;
  if Result then begin
    FCoordinates := VLonLat;
  end;
end;

end.
