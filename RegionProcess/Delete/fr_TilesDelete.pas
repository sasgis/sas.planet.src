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

unit fr_TilesDelete;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  Spin,
  t_CommonTypes,
  i_LanguageManager,
  i_MapTypes,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_PredicateByTileInfo,
  i_GeometryLonLat,
  i_RegionProcessParamsFrame,
  fr_MapSelect,
  u_CommonFormAndFrameParents;

type
  TfrTilesDelete = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameOneZoom,
      IRegionProcessParamsFrameProcessPredicate
    )
    seDelSize: TSpinEdit;
    chkDelBySize: TCheckBox;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlCenter: TPanel;
    lblStat: TLabel;
    flwpnlDelBySize: TFlowPanel;
    lblDelSize: TLabel;
    rgTarget: TRadioGroup;
    pnlMapSelect: TPanel;
    pnlZoom: TPanel;
    Labelzoom: TLabel;
    cbbZoom: TComboBox;
    pnlFrame: TPanel;
    lblMapCaption: TLabel;
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FfrMapSelect: TfrMapSelect;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  private
    function GetMapType: IMapType;
    function GetZoom: Byte;
    function CheckIsDeleteable(const AMapType: IMapType): boolean;
  private
    function GetPredicate: IPredicateByTileInfo;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  u_PredicateByTileInfoBase;

{$R *.dfm}

{ TFrame3 }

constructor TfrTilesDelete.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList
);
begin
  inherited Create(ALanguageManager);
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;

  FfrMapSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfAll, // show maps and layers
      False,  // add -NO- to combobox
      true,  // show disabled map
      CheckIsDeleteable
    );
end;

destructor TfrTilesDelete.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  inherited;
end;

function TfrTilesDelete.GetMapType: IMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrTilesDelete.GetPredicate: IPredicateByTileInfo;
begin
  Result := nil;
  if rgTarget.ItemIndex < 0 then begin
    rgTarget.ItemIndex := 0;
  end;
  if rgTarget.ItemIndex = 0 then begin
    if chkDelBySize.Checked and (seDelSize.Value >= 0) then begin
      Result := TPredicateByTileInfoEqualSize.Create(False, seDelSize.Value);
    end else begin
      Result := TPredicateByTileInfoExistsTile.Create;
    end;
  end else begin
    if rgTarget.ItemIndex = 1 then begin
      Result := TPredicateByTileInfoExistsTNE.Create;
    end else begin
      if rgTarget.ItemIndex = 2 then begin
        if chkDelBySize.Checked and (seDelSize.Value >= 0) then begin
          Result := TPredicateByTileInfoEqualSize.Create(True, seDelSize.Value);
        end else begin
          Result := TPredicateByTileInfoExistsTileOrTNE.Create;
        end;
      end;
    end;
  end;
end;

function TfrTilesDelete.GetZoom: Byte;
begin
  if cbbZoom.ItemIndex < 0 then begin
    cbbZoom.ItemIndex := 0;
  end;
  Result := cbbZoom.ItemIndex;
end;

function TfrTilesDelete.CheckIsDeleteable(const AMapType: IMapType): boolean;
begin
  Result := (AMapType.StorageConfig.AllowDelete) and (AMapType.TileStorage.State.GetStatic.WriteAccess <> asDisabled);
end;

procedure TfrTilesDelete.Init(
  const AZoom: byte;
  const APolygon: IGeometryLonLatPolygon
);
var
  i: integer;
begin
  cbbZoom.Items.Clear;
  for i:= 1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;
  FfrMapSelect.Show(pnlFrame);
end;
function TfrTilesDelete.Validate: Boolean;
begin
  Result := True;
end;

end.
