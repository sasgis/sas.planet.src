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
  t_CoordRepresentation,
  i_Listener,
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
    pnlMgrs: TPanel;
    edtMgrs: TEdit;
    procedure cbbCoordTypeSelect(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnCoordFormatClick(Sender: TObject);
  private
    FCoordinates: TDoublePoint;
    FCoordTypeItems: array of TCoordSysType;
    FCoordTypeItemsCount: Integer;
    FProjectionSet: IProjectionSetChangeable;
    FViewPortState: ILocalCoordConverterChangeable;
    FCoordFromStringParser: ICoordFromStringParser;
    FCoordToStringConverter: ICoordToStringConverterChangeable;
    FCoordRepresentationConfig: ICoordRepresentationConfig;
    FCoordRepresentationConfigListener: IListener;
    FTileSelectStyle: TTileSelectStyle;

    function GetLonLat: TDoublePoint;
    procedure SetLonLat(const AValue: TDoublePoint);
    procedure SetEnabled(const Value: Boolean); reintroduce;

    procedure BuildCoordFormatMenu;
    procedure OnCoordFormatClick(Sender: TObject);
    procedure OnCoordRepresentationConfigChange;

    procedure InitZooms(const AMin, AMax: Integer; const AHint: string);
    procedure InitCoordTypes;

    function CoordTypeToItemIndex(const AType: TCoordSysType): Integer;
    function ItemIndexToCoordType(const AIndex: Integer; out AType: TCoordSysType): Boolean;
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
      const ATileSelectStyle: TTileSelectStyle
    ); reintroduce;
    destructor Destroy; override;

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
  Proj4.UTM,
  Proj4.GaussKruger,
  i_Projection,
  i_ProjectionSet,
  i_LocalCoordConverter,
  u_ListenerByEvent,
  u_ClipboardFunc,
  u_CoordRepresentation,
  u_StrFunc,
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
  const ATileSelectStyle: TTileSelectStyle
);
begin
  Assert(ACoordRepresentationConfig <> nil);

  inherited Create(ALanguageManager);
  FProjectionSet := AProjectionSet;
  FViewPortState := AViewPortState;
  FCoordFromStringParser := ACoordFromStringParser;
  FCoordToStringConverter := ACoordToStringConverter;
  FCoordRepresentationConfig := ACoordRepresentationConfig;
  FTileSelectStyle := ATileSelectStyle;

  InitCoordTypes;
  InitZooms(1, 60, '');
  BuildCoordFormatMenu;

  FCoordRepresentationConfigListener :=
    TNotifyNoMmgEventListener.Create(
      Self.OnCoordRepresentationConfigChange
    );
  FCoordRepresentationConfig.ChangeNotifier.Add(
    FCoordRepresentationConfigListener
  );

  OnCoordRepresentationConfigChange;
end;

destructor TfrLonLat.Destroy;
begin
  if FCoordRepresentationConfigListener <> nil then begin
    FCoordRepresentationConfig.ChangeNotifier.Remove(FCoordRepresentationConfigListener);
  end;
  inherited Destroy;
end;

procedure TfrLonLat.InitZooms(const AMin, AMax: Integer; const AHint: string);
var
  I: Integer;
begin
  cbbZone.Clear;
  for I := AMin to AMax do begin
    cbbZone.Items.Add( IntToStr(I) );
  end;
  cbbZone.Hint := AHint;
end;

procedure TfrLonLat.InitCoordTypes;
var
  I: Integer;
  VCoordType: TCoordSysType;
  VCoordTypeCaptions: TCoordSysTypeCaption;
begin
  cbbCoordType.Clear;
  VCoordTypeCaptions := GetCoordSysTypeCaption;

  I := 0;
  FCoordTypeItemsCount := Length(VCoordTypeCaptions);
  SetLength(FCoordTypeItems, FCoordTypeItemsCount);

  for VCoordType := Low(VCoordTypeCaptions) to High(VCoordTypeCaptions) do begin
    cbbCoordType.Items.Add(VCoordTypeCaptions[VCoordType]);
    FCoordTypeItems[I] := VCoordType;
    Inc(I);
  end;

  cbbCoordType.Items.Add( _('Pixel Coordinates') );
  cbbCoordType.Items.Add( _('Tile Coordinates') );

  VCoordType := FCoordRepresentationConfig.CoordSysType;
  cbbCoordType.ItemIndex := CoordTypeToItemIndex(VCoordType);
end;

function TfrLonLat.ItemIndexToCoordType(const AIndex: Integer; out AType: TCoordSysType): Boolean;
begin
  Assert(AIndex >= 0);
  if AIndex < Length(FCoordTypeItems) then begin
    AType := FCoordTypeItems[AIndex];
    Result := True;
  end else begin
    Result := False;
  end;
end;

function TfrLonLat.CoordTypeToItemIndex(const AType: TCoordSysType): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FCoordTypeItems) - 1 do begin
    if FCoordTypeItems[I] = AType then begin
      Result := I;
      Break;
    end;
  end;
  Assert(Result >= 0);
end;

procedure TfrLonLat.cbbCoordTypeSelect(Sender: TObject);
var
  VCoordType: TCoordSysType;
begin
  if ItemIndexToCoordType(cbbCoordType.ItemIndex, VCoordType) then begin
    FCoordRepresentationConfig.CoordSysType := VCoordType;
  end;
  SetLonLat(FCoordinates);
end;

procedure TfrLonLat.Init;
var
  VCoordType: TCoordSysType;
begin
  grdpnlLonLat.Visible := False;
  pnlProjected.Visible := False;
  pnlXY.Visible := False;
  pnlMgrs.Visible := False;

  if ItemIndexToCoordType(cbbCoordType.ItemIndex, VCoordType) then begin
    case VCoordType of
      cstWGS84, cstSK42: begin
        grdpnlLonLat.Visible := True;
        grdpnlLonLat.Realign;
      end;
      cstUTM, cstSK42GK: begin
        pnlProjected.Visible := True;
      end;
      cstMGRS: begin
        pnlMgrs.Visible := True;
      end
    else
      Assert(False);
    end;
  end else begin
    pnlXY.Visible := True;
    grdpnlXY.Realign;
  end;
end;

function TfrLonLat.GetLonLat: TDoublePoint;
begin
  Result := FCoordinates;
end;

procedure TfrLonLat.SetLonLat(const AValue: TDoublePoint);
var
  VLon, VLat, VZoneStr: string;
  VLatBand: Char;
  VTilePoint: TPoint;
  VPixelPoint: TPoint;
  VZone: Integer;
  VLocalConverter: ILocalCoordConverter;
  VCoordType: TCoordSysType;
begin
  Init;

  FCoordinates := AValue;

  VLocalConverter := FViewPortState.GetStatic;
  cbbZoom.ItemIndex := VLocalConverter.Projection.Zoom;

  if ItemIndexToCoordType(cbbCoordType.ItemIndex, VCoordType) then begin

    if VCoordType = cstSK42GK then begin
      if cbbZone.Items.Count <> 60 then begin
        InitZooms(1, 60, '');
      end;
      VZone := sk42_long_to_gauss_kruger_zone(AValue.X);
    end else
    if VCoordType = cstUTM then begin
      if cbbZone.Items.Count <> 61 then begin
        InitZooms(0, 60, _('0 - for UPS') );
      end;
      VZone := 0;
      wgs84_longlat_to_utm_zone(AValue.X, AValue.Y, VZone, VLatBand);
      Inc(VZone);
    end;

    FCoordToStringConverter.GetStatic.LonLatConvert(
      AValue.X, AValue.Y, False, False, VLon, VLat, VZoneStr
    );

    case VCoordType of
      cstWGS84, cstSK42: begin
        edtLon.Text := VLon;
        edtLat.Text := VLat;
      end;

      cstUTM, cstSK42GK: begin
        edtProjectedX.Text := VLon;
        edtProjectedY.Text := VLat;

        cbbZone.ItemIndex := VZone - 1;
        chkNorth.Checked := (AValue.Y >= 0);
      end;

      cstMGRS: begin
        edtMgrs.Text := VZoneStr + ' ' + VLon + ' ' + VLat;
      end
    else
      Assert(False);
    end;
  end else begin
    if cbbCoordType.ItemIndex = FCoordTypeItemsCount + 0 then begin
      VPixelPoint :=
        PointFromDoublePoint(
          VLocalConverter.Projection.LonLat2PixelPosFloat(AValue),
          prToTopLeft
        );
      edtX.Text := IntToStr(VPixelPoint.X);
      edtY.Text := IntToStr(VPixelPoint.Y);
    end else
    if cbbCoordType.ItemIndex = FCoordTypeItemsCount + 1 then begin
      VTilePoint :=
        PointFromDoublePoint(
          VLocalConverter.Projection.LonLat2TilePosFloat(AValue),
          prToTopLeft
        );
      edtX.Text := IntToStr(VTilePoint.X);
      edtY.Text := IntToStr(VTilePoint.Y);
    end else begin
      Assert(False);
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
  VPoint: TDoublePoint;
  VZoom: Byte;
  VZone: Integer;
  VLonLat: TDoublePoint;
  VProjection: IProjection;
  VLocalConverter: ILocalCoordConverter;
  VCoordType: TCoordSysType;
begin
  Result := True;
  VLonLat := CEmptyDoublePoint;

  VCoordType := FCoordRepresentationConfig.CoordSysType;

  if ItemIndexToCoordType(cbbCoordType.ItemIndex, VCoordType) then begin
    case VCoordType of
      cstWGS84, cstSK42: begin
        Result := FCoordFromStringParser.TryStrToCoord(
          edtLon.Text, edtLat.Text, VLonLat
        );
      end;

      cstUTM, cstSK42GK: begin
        X := edtProjectedX.Text;
        Y := edtProjectedY.Text;

        VZone := StrToInt(cbbZone.Items[cbbZone.ItemIndex]);

        if (VCoordType = cstSK42GK) or
           ((VCoordType = cstUTM) and (VZone = 0)) then
        begin
          SwapStr(X, Y);
        end;

        Result := FCoordFromStringParser.TryStrToCoord(
          X, Y, VZone, chkNorth.Checked, VLonLat
        );
      end;

      cstMGRS: begin
        Result := FCoordFromStringParser.TryStrToCoord(
          edtMgrs.Text, '', 0, False, VLonLat
        );
      end
    else
      Assert(False);
    end;

    if Result then begin
      VLocalConverter := FViewPortState.GetStatic;
      VProjection := VLocalConverter.Projection;
      VProjection.ProjectionType.ValidateLonLatPos(VLonLat);
    end else begin
      ShowMessage(SAS_ERR_CoordinatesInput);
    end;
  end else begin
    if cbbCoordType.ItemIndex = FCoordTypeItemsCount + 0 then begin
      try
        VPoint.X := StrToInt(edtX.Text);
        VPoint.Y := StrToInt(edtY.Text);
      except
        ShowMessage(SAS_ERR_CoordinatesInput);
        Result := False;
      end;

      if Result then begin
        VZoom := cbbZoom.ItemIndex;
        VProjectionSet := FProjectionSet.GetStatic;
        VProjectionSet.ValidateZoom(VZoom);
        VProjection := VProjectionSet.Zooms[VZoom];
        VProjection.ValidatePixelPosFloat(VPoint, False);
        VLonLat := VProjection.PixelPosFloat2LonLat(VPoint);
      end;
    end else
    if cbbCoordType.ItemIndex = FCoordTypeItemsCount + 1 then begin
      try
        VTile.X := StrToInt(edtX.Text);
        VTile.Y := StrToInt(edtY.Text);
      except
        ShowMessage(SAS_ERR_CoordinatesInput);
        Result := False;
      end;

      if Result then begin
        case FTileSelectStyle of
          tssCenter: begin
            VPoint := DoublePoint(VTile.X + 0.5, VTile.Y + 0.5);
          end;
          tssTopLeft: begin
            VPoint := DoublePoint(VTile);
          end;
          tssBottomRight: begin
            VPoint := DoublePoint(VTile.X + 1, VTile.Y + 1);
          end;
        else
          Assert(False);
        end;
        VZoom := cbbZoom.ItemIndex;
        VProjectionSet := FProjectionSet.GetStatic;
        VProjectionSet.ValidateZoom(VZoom);
        VProjection := VProjectionSet.Zooms[VZoom];
        VProjection.ValidateTilePos(VTile, False);
        VLonLat := VProjection.TilePosFloat2LonLat(VPoint);
      end;
    end else begin
      Assert(False);
    end;
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
  VCoordType: TCoordSysType;
begin
  if not Validate then begin
    Exit;
  end;

  VStr := '';

  if ItemIndexToCoordType(cbbCoordType.ItemIndex, VCoordType) then begin
    case VCoordType of
      cstWGS84, cstSK42: begin
        VStr := edtLat.Text + CSep + edtLon.Text;
      end;

      cstUTM, cstSK42GK: begin
        VStr := edtProjectedX.Text + CSep + edtProjectedY.Text;
      end;

      cstMGRS: begin
        VStr := edtMgrs.Text;
      end
    else
      Assert(False);
    end;
  end else begin
    VStr := edtX.Text + CSep + edtY.Text;
  end;

  CopyStringToClipboard(Self.Handle, VStr);
end;

procedure TfrLonLat.btnPasteClick(Sender: TObject);

  function GetCoordFromClipboardText(const AText: string; out S1, S2: string): Boolean;
  var
    I: Integer;
  begin
    I := Pos(' ', AText); // "Lat Lon" or "X Y"
    if I > 0 then begin
      S1 := Trim(Copy(AText, 1, I-1));
      S2 := Trim(Copy(AText, I+1));
    end else begin
      S1 := '';
      S2 := '';
    end;

    Result := (S1 <> '') and (S2 <> '');

    if not Result then begin
      MessageDlg(
        Format(_('Can''t parse coordinates from clipboard: %s'), [AText]),
        mtError, [mbOK], 0
      );
      Exit;
    end;
  end;

var
  S1, S2: string;
  VText: string;
  VCoordType: TCoordSysType;
begin
  VText := Trim(Clipboard.AsText);

  if VText = '' then begin
    MessageDlg(_('Clipboard is empty!'), mtError, [mbOK], 0);
    Exit;
  end;

  if ItemIndexToCoordType(cbbCoordType.ItemIndex, VCoordType) then begin
    case VCoordType of
      cstWGS84, cstSK42: begin
        if GetCoordFromClipboardText(VText, S1, S2) then begin
          edtLat.Text := S1;
          edtLon.Text := S2;
        end;
      end;

      cstUTM, cstSK42GK: begin
        if GetCoordFromClipboardText(VText, S1, S2) then begin
          edtProjectedX.Text := S1;
          edtProjectedY.Text := S2;
        end;
      end;

      cstMGRS: begin
        edtMgrs.Text := VText;
      end
    else
      Assert(False);
    end;
  end else begin
    if GetCoordFromClipboardText(VText, S1, S2) then begin
      edtX.Text := S1;
      edtY.Text := S2;
    end;
  end;
end;

function TagToGeogCoordShowFormat(const ATag: Integer): TGeogCoordShowFormat; inline;
begin
  Result := TGeogCoordShowFormat(ATag - 100);
end;

function GeogCoordShowFormatToTag(const AFormat: TGeogCoordShowFormat): Integer; inline;
begin
  Result := 100 + Integer(AFormat);
end;

function TagToProjCoordShowFormat(const ATag: Integer): TProjCoordShowFormat; inline;
begin
  Result := TProjCoordShowFormat(ATag - 500);
end;

function ProjCoordShowFormatToTag(const AFormat: TProjCoordShowFormat): Integer; inline;
begin
  Result := 500 + Integer(AFormat);
end;

procedure TfrLonLat.BuildCoordFormatMenu;
var
  I: TGeogCoordShowFormat;
  J: TProjCoordShowFormat;
  VItem: TTBXCustomItem;
  VGeogCaption: TGeogCoordShowFormatCaption;
  VProjCaption: TProjCoordShowFormatCaption;
  VGeogFormat: TGeogCoordShowFormat;
  VProjFormat: TProjCoordShowFormat;
begin
  tbxpmnCoordFormat.Items.Clear;

  case FCoordRepresentationConfig.CoordSysType of
    cstWGS84, cstSK42: begin
      VGeogCaption := GetGeogCoordShowFormatCaption;
      VGeogFormat := FCoordRepresentationConfig.GeogCoordShowFormat;
      for I := Low(TGeogCoordShowFormat) to High(TGeogCoordShowFormat) do begin
        VItem := TTBXCustomItem.Create(tbxpmnCoordFormat);
        VItem.Caption := VGeogCaption[I];
        VItem.Checked := (I = VGeogFormat);
        VItem.GroupIndex := 1;
        VItem.Tag := GeogCoordShowFormatToTag(I);
        VItem.OnClick := Self.OnCoordFormatClick;
        tbxpmnCoordFormat.Items.Add(VItem);
      end;
    end;

    cstUTM, cstSK42GK: begin
      VProjCaption := GetProjCoordShowFormatCaption;
      VProjFormat := FCoordRepresentationConfig.ProjCoordShowFormat;
      for J := Low(TProjCoordShowFormat) to High(TProjCoordShowFormat) do begin
        VItem := TTBXCustomItem.Create(tbxpmnCoordFormat);
        VItem.Caption := VProjCaption[J];
        VItem.Checked := (J = VProjFormat);
        VItem.GroupIndex := 1;
        VItem.Tag := ProjCoordShowFormatToTag(J);
        VItem.OnClick := Self.OnCoordFormatClick;
        tbxpmnCoordFormat.Items.Add(VItem);
      end;
    end;

    cstMGRS: begin
      // do nothing
    end;
  else
    Assert(False);
  end;

  btnCoordFormat.Enabled := tbxpmnCoordFormat.Items.Count > 0;
end;

procedure TfrLonLat.OnCoordRepresentationConfigChange;
begin
  cbbCoordType.ItemIndex := CoordTypeToItemIndex(
    FCoordRepresentationConfig.CoordSysType
  );
  BuildCoordFormatMenu;
end;

procedure TfrLonLat.OnCoordFormatClick(Sender: TObject);
var
  VItem: TTBXCustomItem;
begin
  VItem := Sender as TTBXCustomItem;

  case FCoordRepresentationConfig.CoordSysType of
    cstWGS84, cstSK42: begin
      FCoordRepresentationConfig.GeogCoordShowFormat := TagToGeogCoordShowFormat(VItem.Tag);
    end;

    cstUTM, cstSK42GK: begin
      FCoordRepresentationConfig.ProjCoordShowFormat := TagToProjCoordShowFormat(VItem.Tag);
    end;
  else
    Assert(False);
  end;

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
  InitCoordTypes;
  BuildCoordFormatMenu;
end;

end.
