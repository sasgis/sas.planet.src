{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_SensorBase;

interface

uses
  i_JclNotify,
  i_LanguageManager,
  i_Sensor,
  i_SensorList,
  u_UserInterfaceItemBase,
  u_ConfigDataElementBase;

type
  TSensorBase = class(TUserInterfaceItemBase, ISensor, ISensorListEntity)
  private
    FCanReset: Boolean;
    FSensorTypeIID: TGUID;

    FDataUpdateNotifier: IJclNotifier;
  protected
    procedure NotifyDataUpdate;
  protected
    function CanReset: Boolean;
    procedure Reset; virtual;
    function GetSensorTypeIID: TGUID;
    function GetDataUpdateNotifier: IJclNotifier;
    function GetSensor: ISensor;
  public
    constructor Create(
      const AGUID: TGUID;
      ACanReset: Boolean;
      const ASensorTypeIID: TGUID;
      const ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  u_JclNotify;

{ TSensorBase }

constructor TSensorBase.Create(
  const AGUID: TGUID;
  ACanReset: Boolean;
  const ASensorTypeIID: TGUID;
  const ALanguageManager: ILanguageManager
);
begin
  inherited Create(AGUID, ALanguageManager);
  FCanReset := ACanReset;
  FSensorTypeIID := ASensorTypeIID;

  FDataUpdateNotifier := TJclBaseNotifier.Create;
end;

function TSensorBase.CanReset: Boolean;
begin
  Result := FCanReset;
end;

function TSensorBase.GetDataUpdateNotifier: IJclNotifier;
begin
  Result := FDataUpdateNotifier;
end;

function TSensorBase.GetSensor: ISensor;
begin
  Result := Self;
end;

function TSensorBase.GetSensorTypeIID: TGUID;
begin
  Result := FSensorTypeIID;
end;

procedure TSensorBase.NotifyDataUpdate;
begin
  FDataUpdateNotifier.Notify(nil);
end;

procedure TSensorBase.Reset;
begin
end;

end.
