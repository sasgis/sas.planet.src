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

unit u_MapTypeSetChangeableBySourceSetWithFilter;

interface

uses
  i_Listener,
  i_MapType,
  i_MapTypeSet,
  i_MapTypeSetBuilder,
  i_MapTypeSetChangeable,
  u_ChangeableBase;

type
  TMapTypeSetChangeableBySourceSetWithFilter = class(TChangeableWithSimpleLockBase, IMapTypeSetChangeable)
  private
    FMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
    FSourceSet: IMapTypeSetChangeable;
    FSourceSetListener: IListener;

    FPrevSourceSetStatic: IMapTypeSet;
    FStatic: IMapTypeSet;
    procedure OnActiveMapsSetChange;
    function CreateStatic: IMapTypeSet;
  private
    function GetStatic: IMapTypeSet;
  protected
    function IsValidMapType(const AMapType: IMapType): Boolean; virtual;
  public
    constructor Create(
      const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
      const ASourceSet: IMapTypeSetChangeable
    );
    destructor Destroy; override;
  end;

type
  TMapTypeSetChangeableBySourceSetWithFilterBitmap = class(TMapTypeSetChangeableBySourceSetWithFilter)
  protected
    function IsValidMapType(const AMapType: IMapType): Boolean; override;
  end;

type
  TMapTypeSetChangeableBySourceSetWithFilterVector = class(TMapTypeSetChangeableBySourceSetWithFilter)
  protected
    function IsValidMapType(const AMapType: IMapType): Boolean; override;
  end;

type
  TMapTypeSetChangeableBySourceSetWithFilterLicenseNotEmpty = class(TMapTypeSetChangeableBySourceSetWithFilter)
  protected
    function IsValidMapType(const AMapType: IMapType): Boolean; override;
  end;

implementation

uses
  u_ListenerByEvent;

{ TMapTypeSetChangeableBySourceSetWithFilter }

constructor TMapTypeSetChangeableBySourceSetWithFilter.Create(
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const ASourceSet: IMapTypeSetChangeable
);
begin
  inherited Create;
  FMapTypeSetBuilderFactory := AMapTypeSetBuilderFactory;
  FSourceSet := ASourceSet;

  FSourceSetListener := TNotifyNoMmgEventListener.Create(Self.OnActiveMapsSetChange);
  FSourceSet.ChangeNotifier.Add(FSourceSetListener);

  FPrevSourceSetStatic := FSourceSet.GetStatic;
  FStatic := CreateStatic;
end;

destructor TMapTypeSetChangeableBySourceSetWithFilter.Destroy;
begin
  if Assigned(FSourceSet) and Assigned(FSourceSetListener) then begin
    FSourceSet.ChangeNotifier.Remove(FSourceSetListener);
    FSourceSetListener := nil;
    FSourceSet := nil;
  end;
  inherited;
end;

function TMapTypeSetChangeableBySourceSetWithFilter.CreateStatic: IMapTypeSet;
var
  VResult: IMapTypeSetBuilder;
  i: Integer;
  VMapType: IMapType;
begin
  VResult := FMapTypeSetBuilderFactory.Build(False);
  if FPrevSourceSetStatic <> nil then begin
    for i := 0 to FPrevSourceSetStatic.Count - 1 do begin
      VMapType := FPrevSourceSetStatic.Items[i];
      if IsValidMapType(VMapType) then begin
        VResult.Add(VMapType);
      end;
    end;
  end;
  Result := VResult.MakeAndClear;
end;

function TMapTypeSetChangeableBySourceSetWithFilter.GetStatic: IMapTypeSet;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

function TMapTypeSetChangeableBySourceSetWithFilter.IsValidMapType(
  const AMapType: IMapType): Boolean;
begin
  Result := True;
end;

procedure TMapTypeSetChangeableBySourceSetWithFilter.OnActiveMapsSetChange;
var
  VNewSet: IMapTypeSet;
  i: Integer;
  VMapType: IMapType;
  VChanged: Boolean;
begin
  VChanged := False;
  CS.BeginWrite;
  try
    VNewSet := FSourceSet.GetStatic;
    if (FPrevSourceSetStatic <> nil) and FPrevSourceSetStatic.IsEqual(VNewSet) then begin
      Exit;
    end;
    if FPrevSourceSetStatic <> nil then begin
      for i := 0 to FPrevSourceSetStatic.Count - 1 do begin
        VMapType := FPrevSourceSetStatic.Items[i];
        if (VNewSet = nil) or (VNewSet.GetMapTypeByGUID(VMapType.GUID) = nil) then begin
          if IsValidMapType(VMapType) then begin
            VChanged := True;
          end;
        end;
      end;
    end;
    if VNewSet <> nil then begin
      for i := 0 to VNewSet.Count - 1 do begin
        VMapType := VNewSet.Items[i];
        if (FPrevSourceSetStatic = nil) or (FPrevSourceSetStatic.GetMapTypeByGUID(VMapType.GUID) = nil) then begin
          if IsValidMapType(VMapType) then begin
            VChanged := True;
          end;
        end;
      end;
    end;
    FPrevSourceSetStatic := VNewSet;
    if VChanged then begin
      FStatic := CreateStatic;
    end;
  finally
    CS.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

{ TMapTypeSetChangeableBySourceSetWithFilterBitmap }

function TMapTypeSetChangeableBySourceSetWithFilterBitmap.IsValidMapType(
  const AMapType: IMapType
): Boolean;
begin
  Result := (AMapType <> nil) and (AMapType.IsBitmapTiles);
end;

{ TMapTypeSetChangeableBySourceSetWithFilterVector }

function TMapTypeSetChangeableBySourceSetWithFilterVector.IsValidMapType(
  const AMapType: IMapType
): Boolean;
begin
  Result := (AMapType <> nil) and (AMapType.IsKmlTiles);
end;

{ TMapTypeSetChangeableBySourceSetWithFilterLicenseNotEmpty }

function TMapTypeSetChangeableBySourceSetWithFilterLicenseNotEmpty.IsValidMapType(
  const AMapType: IMapType): Boolean;
begin
  Result := False;
  if AMapType <> nil then begin
    Result := AMapType.Zmp.License.GetDefault <> '';
  end;
end;

end.
