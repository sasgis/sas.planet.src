{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_FavoriteMapSetConfig;

interface

uses
  Classes,
  i_GUIDListStatic,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_FavoriteMapSetConfig,
  i_FavoriteMapSetItemStatic,
  u_ConfigDataElementBase;

type
  TFavoriteMapSetConfig = class(TConfigDataElementBase, IFavoriteMapSetConfig)
  private
    FItems: IInterfaceListSimple;
    procedure _Sort;
    function _FindItemIndex(const AID: TGUID): Integer;
  private
    { IFavoriteMapSetConfig }
    function GetCount: Integer;
    function GetByID(const AID: TGUID): IFavoriteMapSetItemStatic;
    procedure Delete(const AID: TGUID);
    procedure Add(
      const AID: TGUID;
      const ABaseMap: TGUID;
      const ALayers: IGUIDSetStatic;
      const AMergeLayers: Boolean;
      const AZoom: Integer;
      const AName: string;
      const AHotKey: TShortCut
    );
    procedure Update(
      const AID: TGUID;
      const ABaseMap: TGUID;
      const ALayers: IGUIDSetStatic;
      const AMergeLayers: Boolean;
      const AZoom: Integer;
      const AName: string;
      const AHotKey: TShortCut
    );
    function GetStatic: IInterfaceListStatic;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  i_StringListStatic,
  u_SortFunc,
  u_GUIDListStatic,
  u_InterfaceListSimple,
  u_ConfigProviderHelpers,
  u_FavoriteMapSetItemStatic;

{ TFavoriteMapSetConfig }

constructor TFavoriteMapSetConfig.Create;
begin
  inherited Create;
  FItems := TInterfaceListSimple.Create;
end;

function TFavoriteMapSetConfig._FindItemIndex(const AID: TGUID): Integer;
var
  I: Integer;
  VItem: IFavoriteMapSetItemStatic;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do begin
    VItem := IFavoriteMapSetItemStatic(FItems[I]);
    if IsEqualGUID(VItem.ID, AID) then begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TFavoriteMapSetConfig._Sort;
var
  I: Integer;
  VCount: Integer;
  VIndexList: array of Integer;
begin
  Exit;
  VCount := FItems.Count;
  if VCount > 1 then begin
    SetLength(VIndexList, VCount);
    for I := 0 to VCount - 1 do begin
      VIndexList[I] := IFavoriteMapSetItemStatic(FItems[I]).SortIndex;
    end;
    SortInterfaceListByIntegerMeasure(FItems, VIndexList);
  end;
end;

function TFavoriteMapSetConfig.GetCount: Integer;
begin
  LockRead;
  try
    Result := FItems.Count;
  finally
    UnlockRead;
  end;
end;

function TFavoriteMapSetConfig.GetStatic: IInterfaceListStatic;
begin
  LockRead;
  try
    Result := FItems.MakeStaticCopy;
  finally
    UnlockRead;
  end;
end;

function TFavoriteMapSetConfig.GetByID(const AID: TGUID): IFavoriteMapSetItemStatic;
var
  I: Integer;
begin
  LockRead;
  try
    I := _FindItemIndex(AID);
    if I >= 0 then begin
      Result := IFavoriteMapSetItemStatic(FItems[I]);
    end else begin
      Result := nil;
    end;
  finally
    UnlockRead;
  end;
end;

procedure TFavoriteMapSetConfig.Delete(const AID: TGUID);
var
  I: Integer;
begin
  LockWrite;
  try
    I := _FindItemIndex(AID);
    if I >= 0 then begin
      FItems.Delete(I);
      SetChanged;
    end else begin
      Assert(False);
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFavoriteMapSetConfig.Add(
  const AID: TGUID;
  const ABaseMap: TGUID;
  const ALayers: IGUIDSetStatic;
  const AMergeLayers: Boolean;
  const AZoom: Integer;
  const AName: string;
  const AHotKey: TShortCut
);
var
  VItem: IFavoriteMapSetItemStatic;
begin
  VItem :=
    TFavoriteMapSetItemStatic.Create(
      AID,
      ABaseMap,
      ALayers,
      AMergeLayers,
      AZoom,
      AName,
      AHotKey,
      0
    );
  LockWrite;
  try
    FItems.Add(VItem);
    _Sort;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TFavoriteMapSetConfig.Update(
  const AID: TGUID;
  const ABaseMap: TGUID;
  const ALayers: IGUIDSetStatic;
  const AMergeLayers: Boolean;
  const AZoom: Integer;
  const AName: string;
  const AHotKey: TShortCut
);
var
  I: Integer;
  VNewItem, VOldItem: IFavoriteMapSetItemStatic;
begin
  VNewItem :=
    TFavoriteMapSetItemStatic.Create(
      AID,
      ABaseMap,
      ALayers,
      AMergeLayers,
      AZoom,
      AName,
      AHotKey,
      0
    );
  LockWrite;
  try
    I := _FindItemIndex(AID);
    if I >= 0 then begin
      VOldItem := IFavoriteMapSetItemStatic(FItems[I]);
      FItems[I] := VNewItem;
      if VOldItem.SortIndex <> VNewItem.SortIndex then begin
        _Sort;
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFavoriteMapSetConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  I, J, K: Integer;
  VCount: Integer;
  VConfig: IConfigDataProvider;
  VItemID: TGUID;
  VItemBaseMap: TGUID;
  VItemLayers: IGUIDSetStatic;
  VItemLayersCount: Integer;
  VItemLayersGUID: array of TGUID;
  VItemMergeLayers: Boolean;
  VItemZoom: Integer;
  VItemName: string;
  VItemHotKey: TShortCut;
  VItemSortIndex: Integer;
  VItem: IFavoriteMapSetItemStatic;
begin
  inherited;
  if AConfigData <> nil then begin
    VConfig := AConfigData.GetSubItem('Main');
    if VConfig <> nil then begin
      VCount := VConfig.ReadInteger('Count', 0);
      for I := 0 to VCount - 1 do begin
        VConfig := AConfigData.GetSubItem('FavoriteMapSet_' + IntToStr(I + 1));
        if VConfig <> nil then begin

          VItemID := ReadGUID(VConfig, 'ID', CGUID_Zero);
          if IsEqualGUID(VItemID, CGUID_Zero) then begin
            Assert(False);
            Continue;
          end;

          VItemBaseMap := ReadGUID(VConfig, 'BaseMap', CGUID_Zero);

          VItemMergeLayers := VConfig.ReadBool('MergeLayers', False);

          VItemLayers := nil;
          VItemLayersCount := VConfig.ReadInteger('LayersCount', 0);
          if VItemLayersCount > 0 then begin
            K := 0;
            SetLength(VItemLayersGUID, VItemLayersCount);
            for J := 0 to VItemLayersCount - 1 do begin
              VItemLayersGUID[K] := ReadGUID(VConfig, 'Layer_' + IntToStr(J + 1), CGUID_Zero);
              if not IsEqualGUID(VItemLayersGUID[K], CGUID_Zero) then begin
                Inc(K);
              end else begin
                Assert(False);
              end;
            end;
            if K > 0 then begin
              SetLength(VItemLayersGUID, K);
              VItemLayers := TGUIDSetStatic.CreateAndSort(VItemLayersGUID, K);
            end;
          end;

          VItemZoom := VConfig.ReadInteger('Zoom', -1);
          VItemName := VConfig.ReadString('Name', '<Unnamed>');
          VItemHotKey := TShortCut(VConfig.ReadInteger('HotKey', 0));
          VItemSortIndex := VConfig.ReadInteger('SortIndex', 0);

          VItem :=
            TFavoriteMapSetItemStatic.Create(
              VItemID,
              VItemBaseMap,
              VItemLayers,
              VItemMergeLayers,
              VItemZoom,
              VItemName,
              VItemHotKey,
              VItemSortIndex
            );

          FItems.Add(VItem);
        end;
      end;
      _Sort;
    end;
  end;
end;

procedure TFavoriteMapSetConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
var
  I, J: Integer;
  VCount: Integer;
  VLayers: IGUIDSetStatic;
  VLayersCount: Integer;
  VSubItemsList: IStringListStatic;
  VConfig: IConfigDataWriteProvider;
  VItem: IFavoriteMapSetItemStatic;
begin
  inherited;

  VSubItemsList := AConfigData.ReadSubItemsList;
  for I := 0 to VSubItemsList.Count - 1 do begin
    AConfigData.DeleteSubItem(VSubItemsList.Items[I]);
  end;

  VConfig := AConfigData.GetOrCreateSubItem('Main');

  VCount := FItems.Count;
  VConfig.WriteInteger('Count', VCount);

  for I := 0 to VCount - 1 do begin
    VConfig := AConfigData.GetOrCreateSubItem('FavoriteMapSet_' + IntToStr(I + 1));

    VItem := IFavoriteMapSetItemStatic(FItems[I]);

    WriteGUID(VConfig, 'ID', VItem.ID);
    WriteGUID(VConfig, 'BaseMap', VItem.BaseMap);

    VConfig.WriteBool('MergeLayers', VItem.MergeLayers);

    VLayers := VItem.Layers;
    if Assigned(VLayers) then begin
      VLayersCount := VLayers.Count;
      VConfig.WriteInteger('LayersCount', VLayersCount);
      for J := 0 to VLayersCount - 1 do begin
        WriteGUID(VConfig, 'Layer_' + IntToStr(J + 1), VLayers.Items[J]);
      end;
    end else begin
      VConfig.WriteInteger('LayersCount', 0);
    end;

    VConfig.WriteInteger('Zoom', VItem.Zoom);
    VConfig.WriteString('Name', VItem.Name);
    VConfig.WriteInteger('HotKey', Integer(VItem.HotKey));
    VConfig.WriteInteger('SortIndex', VItem.SortIndex);
  end;
end;

end.
