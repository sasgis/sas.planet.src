{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
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
  i_ProjectionSetChangeable,
  i_CoordFromStringParser,
  i_CoordToStringConverter,
  i_CoordRepresentationConfig,
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
    pnlProjected: TPanel;
    grdpnlProjected: TGridPanel;
    lbl1: TLabel;
    lbl2: TLabel;
    edtProjectedX: TEdit;
    edtProjectedY: TEdit;
    grdpnlZone: TGridPanel;
    lblZone: TLabel;
    cbbZone: TComboBox;
    chkNorth: TCheckBox;
    procedure cbbCoordTypeSelect(Sender: TObject);
  private
    FCoordinates: TDoublePoint;
    FProjectionSet: IProjectionSetChangeable;
    FViewPortState: ILocalCoordConverterChangeable;
    FCoordFromStringParser: ICoordFromStringParser;
    FCoordToStringConverter: ICoordToStringConverterChangeable;
    FCoordRepresentationConfig: ICoordRepresentationConfig;
    FTileSelectStyle: TTileSelectStyle;
    function GetLonLat: TDoublePoint;
    procedure SetLonLat(const Value: TDoublePoint);
    function IsProjected: Boolean; inline;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProjectionSet: IProjectionSetChangeable;
      const AViewPortState: ILocalCoordConverterChangeable;
      const ACoordRepresentationConfig: ICoordRepresentationConfig;
      const ACoordFromStringParser: ICoordFromStringParser;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      ATileSelectStyle: TTileSelectStyle
    ); reintroduce;
    property LonLat: TDoublePoint read GetLonLat write SetLonLat;
    function Validate: Boolean;
    procedure Init;
  end;

implementation

uses
  Types,
  Math,
  gnugettext,
  Proj4,
  t_CoordRepresentation,
  i_Projection,
  i_ProjectionSet,
  i_LocalCoordConverter,
  u_GeoFunc,
  u_GeoToStrFunc,
  u_ResStrings;

{$R *.dfm}

{ TfrLonLat }

procedure TfrLonLat.cbbCoordTypeSelect(Sender: TObject);
begin
  SetLonLat(FCoordinates);
end;

constructor TfrLonLat.Create(
  const ALanguageManager: ILanguageManager;
  const AProjectionSet: IProjectionSetChangeable;
  const AViewPortState: ILocalCoordConverterChangeable;
  const ACoordRepresentationConfig: ICoordRepresentationConfig;
  const ACoordFromStringParser: ICoordFromStringParser;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  ATileSelectStyle: TTileSelectStyle
);
var
  I: Integer;
begin
  inherited Create(ALanguageManager);
  FProjectionSet := AProjectionSet;
  FViewPortState := AViewPortState;
  FCoordFromStringParser := ACoordFromStringParser;
  FCoordToStringConverter := ACoordToStringConverter;
  FCoordRepresentationConfig := ACoordRepresentationConfig;
  FTileSelectStyle := ATileSelectStyle;
  cbbZone.Clear;
  for I := 1 to 60 do begin
    cbbZone.AddItem(IntToStr(I), nil);
  end;
end;

function TfrLonLat.IsProjected: Boolean;
begin
  Result := FCoordRepresentationConfig.CoordSysType in [cstSK42GK];
end;

procedure TfrLonLat.Init;
var
  VIndex: Integer;
  VIsProjected: Boolean;
begin
  VIsProjected := IsProjected;
  VIndex := cbbCoordType.ItemIndex;

  if VIsProjected then begin
    cbbCoordType.Items.Strings[0] := _('Projected Coordinates');
  end else begin
    cbbCoordType.Items.Strings[0] := _('Geographic Coordinates');
  end;
  cbbCoordType.ItemIndex := VIndex;

  if cbbCoordType.ItemIndex = -1 then begin
    cbbCoordType.ItemIndex := 0;
  end;

  case cbbCoordType.ItemIndex of
    0: begin
      pnlXY.Visible := False;
      if VIsProjected then begin
        pnlProjected.Visible := True;
        grdpnlLonLat.Visible := False;
      end else begin
        grdpnlLonLat.Visible := True;
        pnlProjected.Visible := False;
        grdpnlLonLat.Realign;
      end;
    end;
    1, 2: begin
      pnlXY.Visible := True;
      grdpnlLonLat.Visible := False;
      grdpnlXY.Realign;
    end;
  end;
end;

function TfrLonLat.GetLonLat: TDoublePoint;
begin
  Result := FCoordinates;
end;

procedure TfrLonLat.SetLonLat(const Value: TDoublePoint);
var
  VLon, VLat: string;
  XYPoint: TPoint;
  CurrZoom: integer;
  VZone: Integer;
  VLocalConverter: ILocalCoordConverter;
begin
  Init;

  FCoordinates := Value;
  VLocalConverter := FViewPortState.GetStatic;

  CurrZoom := VLocalConverter.Projection.Zoom;
  cbbZoom.ItemIndex := CurrZoom;

  VZone := long_to_gauss_kruger_zone(Value.X);
  Assert( (VZone >= 1) and (VZone <= 60) );
  cbbZone.ItemIndex := VZone - 1;

  chkNorth.Checked := (Value.Y > 0);

  case cbbCoordType.ItemIndex of
    0: begin
      FCoordToStringConverter.GetStatic.LonLatConvert(
        Value.X, Value.Y, False, VLon, VLat
      );
      if IsProjected then begin
        edtProjectedX.Text := VLon;
        edtProjectedY.Text := VLat;
      end else begin
        edtLon.Text := VLon;
        edtLat.Text := VLat;
      end;
    end;
    1: begin
      XYPoint :=
        PointFromDoublePoint(
          VLocalConverter.Projection.LonLat2PixelPosFloat(Value),
          prToTopLeft
        );
      edtX.Text := inttostr(XYPoint.x);
      edtY.Text := inttostr(XYPoint.y);
    end;
    2: begin
      XYPoint :=
        PointFromDoublePoint(
          VLocalConverter.Projection.LonLat2TilePosFloat(Value),
          prToTopLeft
        );
      edtX.Text := inttostr(XYPoint.x);
      edtY.Text := inttostr(XYPoint.y);
    end;
  end;
end;

function TfrLonLat.Validate: Boolean;
var
  X, Y: string;
  VProjectionSet: IProjectionSet;
  VTile: TPoint;
  XYPoint: TDoublePoint;
  VZoom: Byte;
  VLonLat: TDoublePoint;
  VProjection: IProjection;
  VLocalConverter: ILocalCoordConverter;
begin
  VLonLat := CEmptyDoublePoint;
  Result := True;
  case cbbCoordType.ItemIndex of
    0: begin
      if IsProjected then begin
        X := edtProjectedY.Text; // !
        Y := edtProjectedX.Text; // !
        Result :=
          FCoordFromStringParser.TryStrToCoord(
            X,
            Y,
            cbbZone.ItemIndex + 1,
            chkNorth.Checked,
            VLonLat
          );
      end else begin
        Result :=
          FCoordFromStringParser.TryStrToCoord(
            edtLon.Text,
            edtLat.Text,
            VLonLat
          );
      end;
      if Result then begin
        VLocalConverter := FViewPortState.GetStatic;
        VProjection := VLocalConverter.Projection;
        VProjection.ProjectionType.ValidateLonLatPos(VLonLat);
      end else begin
        ShowMessage(SAS_ERR_CoordinatesInput);
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
      if Result then begin
        VZoom := cbbZoom.ItemIndex;
        VProjectionSet := FProjectionSet.GetStatic;
        VProjectionSet.ValidateZoom(VZoom);
        VProjection := VProjectionSet.Zooms[VZoom];
        VProjection.ValidatePixelPosFloat(XYPoint, False);
        VLonLat := VProjection.PixelPosFloat2LonLat(XYPoint);
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
        VZoom := cbbZoom.ItemIndex;

        case FTileSelectStyle of
          tssCenter: begin
            XYPoint := DoublePoint(VTile.X + 0.5, VTile.Y + 0.5);
          end;
          tssTopLeft: begin
            XYPoint := DoublePoint(VTile);
          end;
          tssBottomRight: begin
            XYPoint := DoublePoint(VTile.X + 1, VTile.Y + 1);
          end;
        end;
        VProjectionSet := FProjectionSet.GetStatic;
        VProjectionSet.ValidateZoom(VZoom);
        VProjection := VProjectionSet.Zooms[VZoom];
        VProjection.ValidateTilePos(VTile, False);
        VLonLat := VProjection.TilePosFloat2LonLat(XYPoint);
      end;
    end;
  end;
  if Result then begin
    FCoordinates := VLonLat;
  end;
end;

end.
