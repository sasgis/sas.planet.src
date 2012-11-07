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
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_PredicateByTileInfo,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
  u_MapType,
  u_CommonFormAndFrameParents;

type
  TfrTilesDelete = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameOneZoom,
      IRegionProcessParamsFrameProcessPredicate
    )
    cbbMap: TComboBox;
    seDelSize: TSpinEdit;
    chkDelBySize: TCheckBox;
    lblMap: TLabel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlRight: TPanel;
    lblZoom: TLabel;
    cbbZoom: TComboBox;
    pnlCenter: TPanel;
    lblStat: TLabel;
    flwpnlDelBySize: TFlowPanel;
    lblDelSize: TLabel;
    rgTarget: TRadioGroup;
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
  private
    function GetMapType: TMapType;
    function GetZoom: Byte;
  private
    function GetPredicate: IPredicateByTileInfo;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList
    ); reintroduce;
  end;

implementation

uses
  i_GUIDListStatic,
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
end;

function TfrTilesDelete.GetMapType: TMapType;
begin
  Result := nil;
  if cbbMap.ItemIndex >= 0 then begin
    Result := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  end;
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
  end else if rgTarget.ItemIndex = 1 then begin
    Result := TPredicateByTileInfoExistsTNE.Create;
  end else if rgTarget.ItemIndex = 2 then begin
    if chkDelBySize.Checked and (seDelSize.Value >= 0) then begin
      Result := TPredicateByTileInfoEqualSize.Create(True, seDelSize.Value);
    end else begin
      Result := TPredicateByTileInfoExistsTileOrTNE.Create;
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

procedure TfrTilesDelete.Init(
  const AZoom: byte;
  const APolygon: ILonLatPolygon
);
var
  i: integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;

  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetStatic.GUID;
  cbbMap.items.Clear;
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  For i := 0 to VGUIDList.Count-1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    if (VMapType.StorageConfig.AllowDelete)and(VMapType.GUIConfig.Enabled) then begin
      VAddedIndex := cbbMap.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);

      // select current map by default
      if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
        cbbMap.ItemIndex:=VAddedIndex;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
end;

end.
