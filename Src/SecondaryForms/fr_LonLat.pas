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
    edtZone: TEdit;
    btnCopy: TTBXButton;
    grdpnlTop: TGridPanel;
    btnPaste: TTBXButton;
    pnlButtons: TPanel;
    btnCoordFormat: TTBXButton;
    tbxpmnCoordFormat: TTBXPopupMenu;
    pnlCustom: TPanel;
    edtCustom: TEdit;
    procedure cbbCoordTypeSelect(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnCoordFormatClick(Sender: TObject);
  private
    type
      TCoordSysGroup = (
        csgGeog,          // WGS 84, SK-42
        csgProjWithZone,  // Gauss-Kruger, UTM
        csgCustom,        // MGRS
        csgPixelXYZ,
        csgTileXYZ
      );
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

    procedure InitCoordTypes;
    procedure BuildCoordFormatMenu;
    procedure OnCoordFormatClick(Sender: TObject);
    procedure OnCoordRepresentationConfigChange;

    function CoordTypeToItemIndex(const AType: TCoordSysType): Integer;
    function ItemIndexToCoordType(const AIndex: Integer; out AType: TCoordSysType): Boolean;
    function ItemIndexToCoordSysGroup(const AIndex: Integer): TCoordSysGroup;
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

  cbbCoordType.DropDownCount := cbbCoordType.Items.Count;

  VCoordType := FCoordRepresentationConfig.CoordSysType;
  cbbCoordType.ItemIndex := CoordTypeToItemIndex(VCoordType);
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

procedure TfrLonLat.cbbCoordTypeSelect(Sender: TObject);
var
  VCoordType: TCoordSysType;
begin
  if ItemIndexToCoordType(cbbCoordType.ItemIndex, VCoordType) then begin
    FCoordRepresentationConfig.CoordSysType := VCoordType;
  end;
  SetLonLat(FCoordinates);
end;

function TfrLonLat.ItemIndexToCoordSysGroup(const AIndex: Integer): TCoordSysGroup;
var
  VCoordSysType: TCoordSysType;
begin
  if ItemIndexToCoordType(AIndex, VCoordSysType) then begin
    case VCoordSysType of
      cstWGS84, cstSK42, cstGSK2011: begin
        Result := csgGeog;
      end;
      cstUTM, cstSK42GK, cstGSK2011GK: begin
        Result := csgProjWithZone;
      end;
      cstMGRS: begin
        Result := csgCustom;
      end
    else
      raise Exception.CreateFmt('Unexpected TCoordSysType value: %d', [Integer(VCoordSysType)]);
    end;
  end else
  if AIndex = FCoordTypeItemsCount + 0 then begin
    Result := csgPixelXYZ;
  end else
  if AIndex = FCoordTypeItemsCount + 1 then begin
    Result := csgTileXYZ;
  end else begin
    raise Exception.CreateFmt('Index out of bouds: %d', [AIndex]);
  end;
end;

procedure TfrLonLat.Init;
begin
  grdpnlLonLat.Visible := False;
  pnlProjected.Visible := False;
  pnlXY.Visible := False;
  pnlCustom.Visible := False;

  case ItemIndexToCoordSysGroup(cbbCoordType.ItemIndex) of
    csgGeog: begin
      grdpnlLonLat.Visible := True;
      grdpnlLonLat.Realign;
    end;
    csgProjWithZone: begin
      pnlProjected.Visible := True;
    end;
    csgCustom: begin
      pnlCustom.Visible := True;
    end;
    csgPixelXYZ, csgTileXYZ: begin
      pnlXY.Visible := True;
      grdpnlXY.Realign;
    end;
  else
    Assert(False);
  end;
end;

function TfrLonLat.GetLonLat: TDoublePoint;
begin
  Result := FCoordinates;
end;

procedure TfrLonLat.SetLonLat(const AValue: TDoublePoint);
var
  VCoord: TCoordPartArray;
  VTilePoint: TPoint;
  VPixelPoint: TPoint;
  VLocalConverter: ILocalCoordConverter;
begin
  Init;

  FCoordinates := AValue;

  VLocalConverter := FViewPortState.GetStatic;
  cbbZoom.ItemIndex := VLocalConverter.Projection.Zoom;

  case ItemIndexToCoordSysGroup(cbbCoordType.ItemIndex) of
    csgGeog: begin
      VCoord := FCoordToStringConverter.GetStatic.LonLatConvertExt(AValue);

      edtLon.Text := VCoord[cpiLon];
      edtLat.Text := VCoord[cpiLat];
    end;

    csgProjWithZone: begin
      VCoord := FCoordToStringConverter.GetStatic.LonLatConvertExt(AValue, [coIncludeZone]);

      edtProjectedX.Text := VCoord[cpiLon];
      edtProjectedY.Text := VCoord[cpiLat];
      edtZone.Text := VCoord[cpiZone]
    end;

    csgCustom: begin
      edtCustom.Text := FCoordToStringConverter.GetStatic.LonLatConvert(AValue, [coIncludeZone]);
    end;

    csgPixelXYZ: begin
      VPixelPoint :=
        PointFromDoublePoint(
          VLocalConverter.Projection.LonLat2PixelPosFloat(AValue),
          prToTopLeft
        );

      edtX.Text := IntToStr(VPixelPoint.X);
      edtY.Text := IntToStr(VPixelPoint.Y);
    end;

    csgTileXYZ: begin
      VTilePoint :=
        PointFromDoublePoint(
          VLocalConverter.Projection.LonLat2TilePosFloat(AValue),
          prToTopLeft
        );
      edtX.Text := IntToStr(VTilePoint.X);
      edtY.Text := IntToStr(VTilePoint.Y);
    end;
  else
    Assert(False);
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
  VProjectionSet: IProjectionSet;
  VTile: TPoint;
  VPoint: TDoublePoint;
  VZoom: Byte;
  VLonLat: TDoublePoint;
  VProjection: IProjection;
  VLocalConverter: ILocalCoordConverter;
begin
  Result := True;
  VLonLat := CEmptyDoublePoint;

  try
    case ItemIndexToCoordSysGroup(cbbCoordType.ItemIndex) of
      csgGeog: begin
        Result := FCoordFromStringParser.TryStrToCoord(
          edtLon.Text, edtLat.Text, VLonLat
        );
      end;

      csgProjWithZone: begin
        Result := FCoordFromStringParser.TryStrToCoord(
          edtProjectedX.Text, edtProjectedY.Text, edtZone.Text, VLonLat
        );
      end;

      csgCustom: begin
        Result := FCoordFromStringParser.TryStrToCoord(
          edtCustom.Text, VLonLat
        );
      end;

      csgPixelXYZ: begin
        VPoint.X := StrToInt(edtX.Text);
        VPoint.Y := StrToInt(edtY.Text);

        VZoom := cbbZoom.ItemIndex;
        VProjectionSet := FProjectionSet.GetStatic;
        VProjectionSet.ValidateZoom(VZoom);
        VProjection := VProjectionSet.Zooms[VZoom];
        VProjection.ValidatePixelPosFloat(VPoint, False);
        VLonLat := VProjection.PixelPosFloat2LonLat(VPoint);
      end;

      csgTileXYZ: begin
        VTile.X := StrToInt(edtX.Text);
        VTile.Y := StrToInt(edtY.Text);

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
          raise Exception.CreateFmt('Unexpected TTileSelectStyle: %d', [Integer(FTileSelectStyle)]);
        end;

        VZoom := cbbZoom.ItemIndex;
        VProjectionSet := FProjectionSet.GetStatic;
        VProjectionSet.ValidateZoom(VZoom);
        VProjection := VProjectionSet.Zooms[VZoom];
        VProjection.ValidateTilePos(VTile, False);
        VLonLat := VProjection.TilePosFloat2LonLat(VPoint);
      end;
    else
      Result := False;
      Assert(False);
    end;

    if Result then begin
      VLocalConverter := FViewPortState.GetStatic;
      VProjection := VLocalConverter.Projection;
      VProjection.ProjectionType.ValidateLonLatPos(VLonLat);
    end;
  except
    Result := False;
  end;

  if Result then begin
    FCoordinates := VLonLat;
  end else begin
    ShowMessage(SAS_ERR_CoordinatesInput);
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

  case ItemIndexToCoordSysGroup(cbbCoordType.ItemIndex) of
    csgGeog: begin
      VStr := edtLat.Text + CSep + edtLon.Text;
    end;
    csgProjWithZone: begin
      VStr := edtProjectedX.Text + CSep + edtProjectedY.Text;
    end;
    csgCustom: begin
      VStr := edtCustom.Text;
    end;
    csgPixelXYZ, csgTileXYZ: begin
      VStr := edtX.Text + CSep + edtY.Text;
    end;
  else
    Assert(False);
    Exit;
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
  VGroup: TCoordSysGroup;
begin
  VText := Trim(Clipboard.AsText);

  if VText = '' then begin
    MessageDlg(_('Clipboard is empty!'), mtError, [mbOK], 0);
    Exit;
  end;

  VGroup := ItemIndexToCoordSysGroup(cbbCoordType.ItemIndex);

  if (VGroup <> csgCustom) and not GetCoordFromClipboardText(VText, S1, S2) then begin
    Exit;
  end;

  case VGroup of
    csgGeog: begin
      edtLat.Text := S1;
      edtLon.Text := S2;
    end;
    csgProjWithZone: begin
      edtProjectedX.Text := S1;
      edtProjectedY.Text := S2;
    end;
    csgCustom: begin
      edtCustom.Text := VText;
    end;
    csgPixelXYZ, csgTileXYZ: begin
      edtX.Text := S1;
      edtY.Text := S2;
    end;
  else
    Assert(False);
  end;
end;

procedure TfrLonLat.BuildCoordFormatMenu;
var
  I: Integer;
  VIndex: Integer;
  VItems: TStringDynArray;
  VItem: TTBXCustomItem;
begin
  tbxpmnCoordFormat.Items.Clear;

  if GetCoordShowFormatCaptions(FCoordRepresentationConfig.GetStatic, VItems, VIndex) then begin
    for I := 0 to Length(VItems) - 1 do begin
      VItem := TTBXCustomItem.Create(tbxpmnCoordFormat);
      VItem.Caption := VItems[I];
      VItem.Checked := (I = VIndex);
      VItem.GroupIndex := 1;
      VItem.Tag := I;
      VItem.OnClick := Self.OnCoordFormatClick;

      tbxpmnCoordFormat.Items.Add(VItem);
    end;
  end;

  btnCoordFormat.Enabled := tbxpmnCoordFormat.Items.Count > 0;
end;

procedure TfrLonLat.OnCoordFormatClick(Sender: TObject);
begin
  SetCoordShowFormat(FCoordRepresentationConfig, TTBXCustomItem(Sender).Tag);
  SetLonLat(FCoordinates);
end;

procedure TfrLonLat.OnCoordRepresentationConfigChange;
begin
  cbbCoordType.ItemIndex := CoordTypeToItemIndex(FCoordRepresentationConfig.CoordSysType);
  BuildCoordFormatMenu;
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
