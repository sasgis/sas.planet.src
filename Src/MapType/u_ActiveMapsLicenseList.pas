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

unit u_ActiveMapsLicenseList;

interface

uses
  i_Listener,
  i_StringListStatic,
  i_StringListChangeable,
  i_MapTypeSetChangeable,
  i_LanguageManager,
  u_ChangeableBase;

type
  TActiveMapsLicenseList = class(TChangeableWithSimpleLockBase, IStringListChangeable)
  private
    FMapsSet: IMapTypeSetChangeable;
    FLanguageManager: ILanguageManager;

    FMapsListListener: IListener;
    FLanguageManagerListener: IListener;
    FStatic: IStringListStatic;
    procedure OnMapsListChange;
    procedure OnLangChange;
    function CreateStatic: IStringListStatic;
  private
    function GetStatic: IStringListStatic;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapsSet: IMapTypeSetChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  i_MapType,
  i_MapTypeSet,
  u_ListenerByEvent,
  u_StringListStatic;

{ TActiveMapsLicenseList }

constructor TActiveMapsLicenseList.Create(
  const ALanguageManager: ILanguageManager;
  const AMapsSet: IMapTypeSetChangeable
);
begin
  Assert(ALanguageManager <> nil);
  Assert(AMapsSet <> nil);
  inherited Create;
  FMapsSet := AMapsSet;
  FLanguageManager := ALanguageManager;

  FMapsListListener := TNotifyNoMmgEventListener.Create(Self.OnMapsListChange);
  FMapsSet.ChangeNotifier.Add(FMapsListListener);

  FLanguageManagerListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLanguageManager.ChangeNotifier.Add(FLanguageManagerListener);
  FStatic := CreateStatic;
end;

destructor TActiveMapsLicenseList.Destroy;
begin
  if Assigned(FMapsSet) and Assigned(FMapsListListener) then begin
    FMapsSet.ChangeNotifier.Remove(FMapsListListener);
    FMapsSet := nil;
    FMapsListListener := nil;
  end;
  if Assigned(FLanguageManager) and Assigned(FLanguageManagerListener) then begin
    FLanguageManager.ChangeNotifier.Remove(FLanguageManagerListener);
    FLanguageManager := nil;
    FLanguageManagerListener := nil;
  end;
  inherited;
end;

function TActiveMapsLicenseList.CreateStatic: IStringListStatic;
var
  VStatic: IStringListStatic;
  VStringList: TStringList;
  VMapsSet: IMapTypeSet;
  VMapType: IMapType;
  VLangIndex: Integer;
  VLicense: string;
  i: Integer;
begin
  Result := nil;
  VMapsSet := FMapsSet.GetStatic;
  if Assigned(VMapsSet) then begin
    VStringList := TStringList.Create;
    try
      VLangIndex := FLanguageManager.CurrentLanguageIndex;
      for i := 0 to VMapsSet.Count - 1 do begin
        VMapType := VMapsSet.Items[i];
        Assert(Assigned(VMapType));
        VLicense := VMapType.Zmp.License.GetString(VLangIndex);
        if VLicense <> '' then begin
          VStringList.Add(VLicense);
        end;
      end;
      VStatic := TStringListStatic.CreateWithOwn(VStringList);
      VStringList := nil;
    finally
      VStringList.Free;
    end;
    Result := VStatic;
  end;
end;

function TActiveMapsLicenseList.GetStatic: IStringListStatic;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TActiveMapsLicenseList.OnLangChange;
begin
  CS.BeginWrite;
  try
    FStatic := CreateStatic;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TActiveMapsLicenseList.OnMapsListChange;
begin
  CS.BeginWrite;
  try
    FStatic := CreateStatic;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

end.
