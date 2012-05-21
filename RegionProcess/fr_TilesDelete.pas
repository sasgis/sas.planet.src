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
  Windows, // for inline AnsiSameText
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
  i_MapAttachmentsInfo,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
  u_CommonFormAndFrameParents;

type
  TfrTilesDelete = class(TFrame, IRegionProcessParamsFrameBase)
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
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
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
  u_MapType;

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
  VMapAttachmentsInfo: IMapAttachmentsInfo;
  VMapAttachmentsName: String;
begin
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;

  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetSelectedGUID;
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

      // check attachments for map (with another name!)
      VMapAttachmentsInfo:=VMapType.Zmp.MapAttachmentsInfo;
      if Assigned(VMapAttachmentsInfo) then
      if VMapAttachmentsInfo.GetUseDel then begin // no direct deleting by default
        VMapAttachmentsName := VMapAttachmentsInfo.GetString(VMapType.GetLanguageManager.CurrentLanguageIndex);
        if (not AnsiSameText(VMapType.GUIConfig.Name.Value, VMapAttachmentsName)) then
          cbbMap.Items.AddObject(VMapAttachmentsName, VMapType);
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
end;

end.
