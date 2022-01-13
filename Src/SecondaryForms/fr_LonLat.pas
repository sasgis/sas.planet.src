{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
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
  UITypes,
  Menus,
  TBX,
  TBXDkPanels,
  TB2Item,
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
    btnCopy: TTBXButton;
    grdpnlTop: TGridPanel;
    btnPaste: TTBXButton;
    pnlButtons: TPanel;
    btnCoordFormat: TTBXButton;
    tbxpmnCoordFormat: TTBXPopupMenu;
    procedure cbbCoordTypeSelect(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnCoordFormatClick(Sender: TObject);
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
    procedure SetEnabled(const Value: Boolean); reintroduce;
    function IsProjected: Boolean; inline;
    procedure BuildCoordFormatMenu;
    procedure OnCoordFormatClick(Sender: TObject);
  protected
    procedure RefreshTranslation; override;
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
    property Enabled: Boolean write SetEnabled;
    function Validate: Boolean;
    procedure Init;
  end;

implementation

uses
  Types,
  Math,
  Clipbrd,
  gnugettext,
  Proj4SK42,
  t_CoordRepresentation,
  i_Projection,
  i_ProjectionSet,
  i_LocalCoordConverter,
  u_ClipboardFunc,
  u_CoordRepresentation,
  u_GeoFunc,
  u_GeoToStrFunc,
  u_ResStrings;

{$R *.dfm}

{ TfrLonLat }

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
  BuildCoordFormatMenu;
end;

procedure TfrLonLat.cbbCoordTypeSelect(Sender: TObject);
begin
  SetLonLat(FCoordinates);
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

procedure TfrLonLat.SetEnabled(const Value: Boolean);

  procedure SetControlEnabled(const AControl: TControl; const AEnabled: Boolean);
  var
    I: Integer;
  begin
    if AControl = nil then begin
      Exit;
    end;
    if AControl is TWinControl then begin
      for I := 0 to TWinControl(AControl).ControlCount - 1 do begin
        SetControlEnabled(TWinControl(AControl).Controls[I], AEnabled);
      end;
    end;
    AControl.Enabled := AEnabled;
  end;

begin
  SetControlEnabled(Self, Value);
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
  else
    Assert(False);
  end;
  if Result then begin
    FCoordinates := VLonLat;
  end;
end;

procedure TfrLonLat.btnCopyClick(Sender: TObject);
const
  CSep = ' ';
var
  VStr: string;
begin
  if not Validate then begin
    Exit;
  end;

  VStr := '';

  case cbbCoordType.ItemIndex of
    0: begin
      if IsProjected then begin
        VStr := edtProjectedX.Text + CSep + edtProjectedY.Text;
      end else begin
        VStr := edtLat.Text + CSep + edtLon.Text;
      end;
    end;
    1, 2: begin
      VStr := edtX.Text + CSep + edtY.Text;
    end;
  else
    Assert(False);
  end;

  CopyStringToClipboard(Self.Handle, VStr);
end;

procedure TfrLonLat.btnPasteClick(Sender: TObject);
var
  I: Integer;
  S1, S2: string;
  VText: string;
begin
  VText := Clipboard.AsText;

  if VText = '' then begin
    MessageDlg(_('Clipboard is empty!'), mtError, [mbOK], 0);
    Exit;
  end;

  I := Pos(' ', VText); // "Lat Lon" or "X Y"
  if I > 0 then begin
    S1 := Trim(Copy(VText, 1, I-1));
    S2 := Trim(Copy(VText, I+1));
  end else begin
    S1 := '';
    S2 := '';
  end;

  if (S1 = '') or (S2 = '') then begin
    MessageDlg(
      Format(_('Can''t parse coordinates from clipboard: %s'), [VText]),
      mtError, [mbOK], 0
    );
    Exit;
  end;

  case cbbCoordType.ItemIndex of
    0: begin
      if IsProjected then begin
        edtProjectedX.Text := S1;
        edtProjectedY.Text := S2;
      end else begin
        edtLon.Text := S2;
        edtLat.Text := S1;
      end;
    end;
    1, 2: begin
      edtX.Text := S1;
      edtY.Text := S2;
    end;
  else
    Assert(False);
  end;
end;

procedure TfrLonLat.BuildCoordFormatMenu;
var
  I: TDegrShowFormat;
  VItem: TTBXCustomItem;
  VCaption: TDegrShowFormatCaption;
begin
  VCaption := GetDegrShowFormatCaption;
  tbxpmnCoordFormat.Items.Clear;
  for I := Low(TDegrShowFormat) to High(TDegrShowFormat) do begin
    VItem := TTBXCustomItem.Create(tbxpmnCoordFormat);
    VItem.Caption := VCaption[I];
    VItem.Tag := 100 + Integer(I);
    VItem.OnClick := Self.OnCoordFormatClick;
    tbxpmnCoordFormat.Items.Add(VItem);
  end;
end;

procedure TfrLonLat.OnCoordFormatClick(Sender: TObject);
var
  VItem: TTBXCustomItem;
begin
  VItem := Sender as TTBXCustomItem;
  FCoordRepresentationConfig.DegrShowFormat := TDegrShowFormat(VItem.Tag - 100);
  SetLonLat(FCoordinates);
end;

procedure TfrLonLat.btnCoordFormatClick(Sender: TObject);
begin
  with btnCoordFormat.ClientToScreen(Point(0, btnCoordFormat.Height)) do begin
    tbxpmnCoordFormat.Popup(X, Y);
  end;
end;

procedure TfrLonLat.RefreshTranslation;
begin
  inherited;
  BuildCoordFormatMenu;
end;

end.
