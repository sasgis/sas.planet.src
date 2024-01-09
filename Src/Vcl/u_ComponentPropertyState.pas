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

unit u_ComponentPropertyState;

interface

uses
  Classes,
  t_ComponentProperty,
  i_ComponentPropertyState,
  i_ComponentPropertyStorage,
  u_BaseInterfacedObject;

type
  TComponentPropertyState = class(TBaseInterfacedObject, IComponentPropertyState)
  private
    FComponent: TComponent;
    FOptions: TComponentPropertyStateOptions;
    FIgnore: TComponentDynArray;
    FTemporary: TComponentDynArray;
    FStorageCache: TComponentPropertyStorageCache;
    FComponentPropertyStorage: IComponentPropertyStorage;
    FWasRestored: Boolean;
    class procedure AddAssigned(
      const ASrc: TComponentDynArray;
      var ADest: TComponentDynArray
    ); static;
  private
    { IComponentPropertyState }
    procedure Save;
    procedure Restore;
    function GetOptions: TComponentPropertyStateOptions;
  public
    constructor Create(
      const AComponent: TComponent;
      const AIgnore: TComponentDynArray;
      const ATemporary: TComponentDynArray;
      const AOptions: TComponentPropertyStateOptions
    );
  end;

implementation

uses
  u_GlobalState;

{ TComponentPropertyState }

constructor TComponentPropertyState.Create(
  const AComponent: TComponent;
  const AIgnore, ATemporary: TComponentDynArray;
  const AOptions: TComponentPropertyStateOptions
);
begin
  inherited Create;

  FComponent := AComponent;

  AddAssigned(AIgnore, FIgnore);
  AddAssigned(ATemporary, FTemporary);

  FOptions := AOptions;

  FStorageCache.FNamePath := '';
  FStorageCache.FIgnoreNamePath := nil;
  FStorageCache.FTemporaryNamePath := nil;

  FComponentPropertyStorage := GState.ComponentPropertyStorage;
end;

procedure TComponentPropertyState.Save;
begin
  FComponentPropertyStorage.Save(FComponent, FIgnore, FTemporary, FStorageCache);
end;

procedure TComponentPropertyState.Restore;
begin
  if FWasRestored and (cpsoIgnoreSecondaryRestoreCalls in FOptions) then begin
    Exit;
  end;
  FWasRestored := True;
  FComponentPropertyStorage.Restore(FComponent, FIgnore, FTemporary, FStorageCache);
end;

function TComponentPropertyState.GetOptions: TComponentPropertyStateOptions;
begin
  Result := FOptions;
end;

class procedure TComponentPropertyState.AddAssigned(const ASrc: TComponentDynArray; var ADest: TComponentDynArray);
var
  I, J: Integer;
begin
  J := 0;
  SetLength(ADest, Length(ASrc));
  for I := 0 to Length(ASrc) - 1 do begin
    if ASrc[I] <> nil then begin
      ADest[J] := ASrc[I];
      Inc(J);
    end;
  end;
  if J <> Length(ADest) then begin
    SetLength(ADest, J);
  end;
end;

end.
