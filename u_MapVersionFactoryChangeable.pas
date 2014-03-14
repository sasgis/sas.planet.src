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

unit u_MapVersionFactoryChangeable;

interface

uses
  i_MapVersionFactory,
  u_ConfigDataElementBase;

type
  TMapVersionFactoryChangeable = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IMapVersionFactoryChangeable, IMapVersionFactoryChangeableInternal)
  private
    FFactory: IMapVersionFactory;
  protected
    function CreateStatic: IInterface; override;
  private
    procedure SetFactory(const AValue: IMapVersionFactory);
  private
    function GetStatic: IMapVersionFactory;
  public
    constructor Create(
      const ADefFactory: IMapVersionFactory
    );
  end;

implementation

{ TMapVersionFactoryChangeable }

constructor TMapVersionFactoryChangeable.Create(
  const ADefFactory: IMapVersionFactory);
begin
  inherited Create;
  FFactory := ADefFactory;
end;

function TMapVersionFactoryChangeable.CreateStatic: IInterface;
begin
  Result := FFactory;
end;

function TMapVersionFactoryChangeable.GetStatic: IMapVersionFactory;
begin
  Result := IMapVersionFactory(GetStaticInternal);
end;

procedure TMapVersionFactoryChangeable.SetFactory(
  const AValue: IMapVersionFactory
);
begin
  LockWrite;
  try
    if not FFactory.IsSameFactoryClass(AValue) then begin
      FFactory := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
