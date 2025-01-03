{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit fr_CacheTypeList;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  i_LanguageManager,
  i_TileStorageAbilities,
  i_TileStorageTypeList,
  i_TileStorageTypeListItem,
  u_CommonFormAndFrameParents;

type
  TTileStorageTypeClassSet = set of TTileStorageTypeClass;

const
  CTileStorageTypeClassAll = [tstcInMemory, tstcOneFile, tstcFolder, tstcInSeparateFiles, tstcOther];

type
  TTileStorageAbilitiesClass = (tsacRead, tsacScan, tsacAdd, tsacDelete, tsacReplace);
  TTileStorageAbilitiesClassSet = set of TTileStorageAbilitiesClass;

type
  TfrCacheTypeList = class(TFrame)
    cbbCacheType: TComboBox;
    procedure cbbCacheTypeChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FOptions: TTileStorageTypeClassSet;
    FRequaredAbilities: TTileStorageAbilitiesClassSet;
    FTileStorageTypeList: ITileStorageTypeListStatic;
    FWithDefaultItem: Boolean;
    function IsItemAllowed(
      const AItem: ITileStorageTypeListItem
    ): Boolean; inline;
    procedure FillItems(
      const ATypesList: ITileStorageTypeListStatic;
      const AWithDefaultItem: Boolean
    );
    procedure SetFilterOptions(
      const AOptions: TTileStorageTypeClassSet
    );
  public
    procedure Show(AParent: TWinControl);
    function TrySetIntCode(const AValue: Integer): Boolean;
    function GetIntCode: Integer;
    procedure SetIntCode(const AValue: Integer);
    property IntCode: Integer read GetIntCode write SetIntCode;
    property FilterOptions: TTileStorageTypeClassSet read FOptions write SetFilterOptions;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const AWithDefaultItem: Boolean = False;
      const AFilterOptions: TTileStorageTypeClassSet = CTileStorageTypeClassAll;
      const ARequaredAbilities: TTileStorageAbilitiesClassSet = [];
      const AOnChange: TNotifyEvent = nil
    );
  end;

implementation

{$R *.dfm}

uses
  gnugettext,
  c_CacheTypeCodes;

{ TfrCacheTypeList }

constructor TfrCacheTypeList.Create(
  const ALanguageManager: ILanguageManager;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const AWithDefaultItem: Boolean;
  const AFilterOptions: TTileStorageTypeClassSet;
  const ARequaredAbilities: TTileStorageAbilitiesClassSet;
  const AOnChange: TNotifyEvent
);
begin
  inherited Create(ALanguageManager);

  FTileStorageTypeList := ATileStorageTypeList;
  FWithDefaultItem := AWithDefaultItem;
  FRequaredAbilities := ARequaredAbilities;
  FOnChange := AOnChange;

  SetFilterOptions(AFilterOptions);

  FPropertyState := CreateComponentPropertyState(
    Self, [], [], True, False, True, True
  );
end;

procedure TfrCacheTypeList.Show(AParent: TWinControl);
begin
  Parent := AParent;
end;

function TfrCacheTypeList.IsItemAllowed(const AItem: ITileStorageTypeListItem): Boolean;
var
  VItemAbilities: ITileStorageTypeAbilities;
begin
  Result := False;

  if not AItem.CanUseAsDefault then begin
    Exit;
  end;

  VItemAbilities := AItem.StorageType.Abilities;
  Result := VItemAbilities.StorageClass in FOptions;
  if Result then begin
    if tsacRead in FRequaredAbilities then begin
      if not VItemAbilities.BaseStorageAbilities.AllowRead then begin
        Result := False;
        Exit;
      end;
    end;
    if tsacScan in FRequaredAbilities then begin
      if not VItemAbilities.BaseStorageAbilities.AllowScan then begin
        Result := False;
        Exit;
      end;
    end;
    if tsacAdd in FRequaredAbilities then begin
      if not VItemAbilities.BaseStorageAbilities.AllowAdd then begin
        Result := False;
        Exit;
      end;
    end;
    if tsacDelete in FRequaredAbilities then begin
      if not VItemAbilities.BaseStorageAbilities.AllowDelete then begin
        Result := False;
        Exit;
      end;
    end;
    if tsacReplace in FRequaredAbilities then begin
      if not VItemAbilities.BaseStorageAbilities.AllowReplace then begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

procedure TfrCacheTypeList.FillItems(
  const ATypesList: ITileStorageTypeListStatic;
  const AWithDefaultItem: Boolean
);
var
  I: Integer;
  VItem: ITileStorageTypeListItem;
begin
  Assert(Assigned(ATypesList));

  cbbCacheType.Clear;

  if AWithDefaultItem then begin
    cbbCacheType.Items.AddObject(_('By default'), TObject(c_File_Cache_Id_DEFAULT));
  end;

  for I := 0 to ATypesList.Count - 1 do begin
    VItem := ATypesList.Items[I];
    if IsItemAllowed(VItem) then begin
      cbbCacheType.Items.AddObject(VItem.Caption, TObject(VItem.IntCode));
    end;
  end;

  if cbbCacheType.Items.Count > 0 then begin
    cbbCacheType.ItemIndex := 0;
    cbbCacheType.DropDownCount := cbbCacheType.Items.Count;
  end;
end;

function TfrCacheTypeList.GetIntCode: Integer;
var
  I: Integer;
begin
  I := cbbCacheType.ItemIndex;
  if I >= 0 then begin
    Result := Integer(cbbCacheType.Items.Objects[I]);
  end else begin
    Result := c_File_Cache_Id_INVALID;
  end;
end;

procedure TfrCacheTypeList.SetFilterOptions(
  const AOptions: TTileStorageTypeClassSet
);
begin
  Assert(AOptions <> []);
  FOptions := AOptions;
  FillItems(FTileStorageTypeList, FWithDefaultItem);
end;

function TfrCacheTypeList.TrySetIntCode(const AValue: Integer): Boolean;
var
  I: Integer;
  VCode: Integer;
begin
  Result := False;
  if AValue = c_File_Cache_Id_INVALID then begin
    Exit;
  end;
  for I := 0 to cbbCacheType.Items.Count - 1 do begin
    VCode := Integer(cbbCacheType.Items.Objects[I]);
    if VCode = AValue then begin
      cbbCacheType.ItemIndex := I;
      Result := True;
      Break;
    end;
  end;
  if Result and Assigned(FOnChange) then begin
    FOnChange(Self);
  end;
end;

procedure TfrCacheTypeList.SetIntCode(const AValue: Integer);
begin
  if not TrySetIntCode(AValue) then begin
    Assert(False);
  end;
end;

procedure TfrCacheTypeList.cbbCacheTypeChange(Sender: TObject);
begin
  if Assigned(FOnChange) then begin
    FOnChange(Self);
  end;
end;

end.
