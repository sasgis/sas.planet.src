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

unit u_MarkOnMapEditProvider;

interface

uses
  i_VectorDataItemSimple,
  i_MarkOnMapEditProvider,
  u_BaseInterfacedObject;

type
  TOnMapEditCallBack = procedure(const AItem: IVectorDataItem) of object;

  TMarkOnMapEditProvider = class(
    TBaseInterfacedObject,
    IMarkOnMapEditProvider,
    IMarkOnMapEditProviderInternal
  )
  private
    FEnabled: Boolean;
    FCallBack: TOnMapEditCallBack;
  private
    { IMarkOnMapEditProvider }
    procedure ProcessOnMapEdit(const AItem: IVectorDataItem);
  private
    { IMarkOnMapEditProviderInternal }
    procedure SetEnabled(const AValue: Boolean);
  public
    constructor Create(
      const AOnMapEditCallBack: TOnMapEditCallBack
    );
  end;

implementation

{ TMarkOnMapEditProvider }

constructor TMarkOnMapEditProvider.Create(
  const AOnMapEditCallBack: TOnMapEditCallBack
);
begin
  inherited Create;
  FCallBack := AOnMapEditCallBack;
  SetEnabled(True);
end;

procedure TMarkOnMapEditProvider.ProcessOnMapEdit(const AItem: IVectorDataItem);
begin
  if FEnabled and (AItem <> nil) then begin
    FCallBack(AItem);
  end;
end;

procedure TMarkOnMapEditProvider.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue and Assigned(FCallBack);
end;

end.
