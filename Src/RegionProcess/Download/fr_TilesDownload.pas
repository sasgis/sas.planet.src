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

unit fr_TilesDownload;

interface

uses
  Types,
  Classes,
  Controls,
  ComCtrls,
  ExtCtrls,
  Forms,
  SysUtils,
  StdCtrls,
  Windows,
  Spin,
  i_MapType,
  i_LanguageManager,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_RegionProcessParamsFrame,
  fr_MapSelect,
  fr_ZoomsSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameTilesDownload = interface(IRegionProcessParamsFrameBase)
    ['{70B48431-5383-4CD2-A1EF-AF9291F6ABB0}']
    function GetIsStartPaused: Boolean;
    property IsStartPaused: Boolean read GetIsStartPaused;

    function GetIsIgnoreTne: Boolean;
    property IsIgnoreTne: Boolean read GetIsIgnoreTne;

    function GetLoadTneOlderDate: TDateTime;
    property LoadTneOlderDate: TDateTime read GetLoadTneOlderDate;

    function GetIsReplace: Boolean;
    property IsReplace: Boolean read GetIsReplace;

    function GetIsReplaceIfDifSize: Boolean;
    property IsReplaceIfDifSize: Boolean read GetIsReplaceIfDifSize;

    function GetIsReplaceIfOlder: Boolean;
    property IsReplaceIfOlder: Boolean read GetIsReplaceIfOlder;

    function GetReplaceDate: TDateTime;
    property ReplaceDate: TDateTime read GetReplaceDate;

    function GetSplitCount: Integer;
    property SplitCount: Integer read GetSplitCount;
  end;

type
  TfrTilesDownload = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTilesDownload
    )
    lblStat: TLabel;
    chkReplace: TCheckBox;
    chkReplaceIfDifSize: TCheckBox;
    chkReplaceOlder: TCheckBox;
    dtpReplaceOlderDate: TDateTimePicker;
    chkTryLoadIfTNE: TCheckBox;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlMain: TPanel;
    pnlTileReplaceCondition: TPanel;
    pnlReplaceOlder: TPanel;
    lblReplaceOlder: TLabel;
    chkStartPaused: TCheckBox;
    pnlMapSelect: TPanel;
    pnlZoom: TPanel;
    lblMapCaption: TLabel;
    pnlFrame: TPanel;
    pnlLoadIfTneParams: TPanel;
    pnlLoadIfTneOld: TPanel;
    lblLoadIfTneOld: TLabel;
    chkLoadIfTneOld: TCheckBox;
    dtpLoadIfTneOld: TDateTimePicker;
    chkSplitRegion: TCheckBox;
    pnlSplitRegionParams: TPanel;
    lblSplitRegion: TLabel;
    sePartsCount: TSpinEdit;
    procedure chkReplaceClick(Sender: TObject);
    procedure chkReplaceOlderClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
    procedure chkLoadIfTneOldClick(Sender: TObject);
    procedure chkTryLoadIfTNEClick(Sender: TObject);
    procedure chkSplitRegionClick(Sender: TObject);
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FPolygLL: IGeometryLonLatPolygon;
    FfrMapSelect: TfrMapSelect;
    FfrZoomsSelect: TfrZoomsSelect;
    function GetZoomInfo(
      const AZoom: Byte;
      const AMapType: IMapType;
      out ATileRect: TRect;
      out APixelRect: TRect;
      out ATilesCount: Int64
    ): Boolean;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  private
    function GetMapType: IMapType;
    function GetZoomArray: TByteDynArray;
  private
    function GetIsStartPaused: Boolean;
    function GetIsIgnoreTne: Boolean;
    function GetLoadTneOlderDate: TDateTime;
    function GetIsReplace: Boolean;
    function GetIsReplaceIfDifSize: Boolean;
    function GetIsReplaceIfOlder: Boolean;
    function GetReplaceDate: TDateTime;
    function GetAllowDownload(const AMapType: IMapType): boolean; // чисто для проверки
    function GetSplitCount: Integer;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  Dialogs,
  t_GeoTypes,
  i_Projection,
  i_GeometryProjected,
  u_GeoFunc,
  u_ResStrings;

{$R *.dfm}

function TfrTilesDownload.GetZoomInfo(
  const AZoom: Byte;
  const AMapType: IMapType;
  out ATileRect: TRect;
  out APixelRect: TRect;
  out ATilesCount: Int64
): Boolean;
var
  VZoom: Byte;
  VProjection: IProjection;
  VBounds: TDoubleRect;
  VPolyLL: IGeometryLonLatPolygon;
  VProjected: IGeometryProjectedPolygon;
begin
  Result := False;
  if AMapType <> nil then begin
    VZoom := AZoom;
    AMapType.ProjectionSet.ValidateZoom(VZoom);
    VProjection := AMapType.ProjectionSet[VZoom];
    VPolyLL := FPolygLL;
    if VPolyLL <> nil then begin
      VProjected :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
          VProjection,
          VPolyLL
        );
      if Assigned(VProjected) then begin
        VBounds := VProjected.Bounds;
        APixelRect := RectFromDoubleRect(VBounds, rrOutside);
        ATileRect := VProjection.PixelRect2TileRect(APixelRect);
        ATilesCount :=
          Int64(ATileRect.Right - ATileRect.Left) * Int64(ATileRect.Bottom - ATileRect.Top);
        Result := True;
      end;
    end;
  end;
end;

procedure TfrTilesDownload.cbbZoomChange(Sender: TObject);
var
  I: Integer;
  VZoomArr: TByteDynArray;
  VMapType: IMapType;
  VTileRect: TRect;
  VPixelRect: TRect;
  VTilesCount: Int64;
  VTilesTotal: Int64;
begin
  VMapType := FfrMapSelect.GetSelectedMapType;
  VZoomArr := FfrZoomsSelect.GetZoomList;
  if Length(VZoomArr) = 1 then begin
    if GetZoomInfo(VZoomArr[0], VMapType, VTileRect, VPixelRect, VTilesCount) then begin
      lblStat.Caption :=
        SAS_STR_filesnum + ': ' +
        IntToStr(VTileRect.Right - VTileRect.Left) + 'x' +
        IntToStr(VTileRect.Bottom - VTileRect.Top) +
        '(' + IntToStr(VTilesCount) + ')' +
        ', ' + SAS_STR_Resolution + ' ' +
        IntToStr(VPixelRect.Right - VPixelRect.Left) + 'x' +
        IntToStr(VPixelRect.Bottom - VPixelRect.Top) + ' pix';
    end;
  end else begin
    VTilesTotal := 0;
    for I := 0 to Length(VZoomArr) - 1 do begin
      if GetZoomInfo(VZoomArr[I], VMapType, VTileRect, VPixelRect, VTilesCount) then begin
        Inc(VTilesTotal, VTilesCount);
      end;
    end;
    lblStat.Caption := SAS_STR_filesnum + ': ' + IntToStr(VTilesTotal);
  end;
end;

destructor TfrTilesDownload.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrZoomsSelect);
  inherited;
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

procedure TfrTilesDownload.chkSplitRegionClick(Sender: TObject);
begin
  sePartsCount.Enabled := chkSplitRegion.Checked;
end;

constructor TfrTilesDownload.Create(
  const ALanguageManager: ILanguageManager;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder
);
begin
  inherited Create(ALanguageManager);
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FfrMapSelect :=
    AMapSelectFrameBuilder.Build(
      mfAll, // show maps and layers
      false,  // add -NO- to combobox
      false,  // show disabled map
      GetAllowDownload
    );
  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager,
      Self.cbbZoomChange
    );
  FfrZoomsSelect.Init(0, 23);
end;

procedure TfrTilesDownload.chkLoadIfTneOldClick(Sender: TObject);
begin
  dtpLoadIfTneOld.Enabled := chkLoadIfTneOld.Enabled and chkLoadIfTneOld.Checked;
end;

procedure TfrTilesDownload.chkTryLoadIfTNEClick(Sender: TObject);
var
  VEnabled: Boolean;
begin
  VEnabled := chkTryLoadIfTNE.Checked;
  chkLoadIfTneOld.Enabled := VEnabled;
  chkLoadIfTneOldClick(chkLoadIfTneOld);
end;

function TfrTilesDownload.GetAllowDownload(const AMapType: IMapType): boolean; // чисто для проверки
begin
  Result := (AMapType.StorageConfig.GetAllowAdd) and (AMapType.TileDownloadSubsystem.State.GetStatic.Enabled);
end;

function TfrTilesDownload.GetIsIgnoreTne: Boolean;
begin
  Result := chkTryLoadIfTNE.Checked;
end;

function TfrTilesDownload.GetIsReplace: Boolean;
begin
  Result := chkReplace.Checked;
end;

function TfrTilesDownload.GetIsReplaceIfDifSize: Boolean;
begin
  Result := chkReplaceIfDifSize.Checked;
end;

function TfrTilesDownload.GetIsReplaceIfOlder: Boolean;
begin
  Result := chkReplaceOlder.Checked;
end;

function TfrTilesDownload.GetIsStartPaused: Boolean;
begin
  Result := chkStartPaused.Checked;
end;

function TfrTilesDownload.GetLoadTneOlderDate: TDateTime;
begin
  if chkLoadIfTneOld.Checked then begin
    Result := dtpLoadIfTneOld.DateTime;
  end else begin
    Result := NaN;
  end;
end;

function TfrTilesDownload.GetMapType: IMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrTilesDownload.GetReplaceDate: TDateTime;
begin
  Result := dtpReplaceOlderDate.DateTime;
end;

function TfrTilesDownload.GetSplitCount: Integer;
begin
  if chkSplitRegion.Checked then begin
    Result := sePartsCount.Value;
  end else begin
    Result := 1;
  end;
end;

function TfrTilesDownload.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

procedure TfrTilesDownload.Init(
  const AZoom: Byte;
  const APolygon: IGeometryLonLatPolygon
);
var
  I: Integer;
  VMapType: IMapType;
begin
  FPolygLL := APolygon;
  FfrZoomsSelect.Show(pnlZoom);
  dtpReplaceOlderDate.Date := now;
  dtpLoadIfTneOld.Date := now;
  FfrMapSelect.Show(pnlFrame);
  cbbZoomChange(Self);

  sePartsCount.Enabled := chkSplitRegion.Checked;
  VMapType := FfrMapSelect.GetSelectedMapType;
  I := VMapType.TileDownloaderConfig.MaxConnectToServerCount;
  if (sePartsCount.MinValue <= I) and (sePartsCount.MaxValue >= I) then begin
    sePartsCount.Value := I;
  end;
end;

function TfrTilesDownload.Validate: Boolean;
begin
  Result := FfrZoomsSelect.Validate;
  if not Result then begin
    MessageDlg(SAS_MSG_NeedZoom, mtError, [mbOk], 0);
  end;
end;

end.
