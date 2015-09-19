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

unit u_SensorListEntity;

interface

uses
  i_StringConfigDataElement,
  i_Sensor,
  i_SensorList,
  i_ListenerNotifierLinksList,
  u_ChangeableBase;

type
  TSensorListEntity = class(TChangeableWithSimpleLockBase, ISensorListEntity)
  private
    FGUID: TGUID;
    FCaption: IStringConfigDataElement;
    FDescription: IStringConfigDataElement;
    FMenuItemName: IStringConfigDataElement;

    FSensor: ISensor;
    FSensorTypeIID: TGUID;

    FLinksList: IListenerNotifierLinksList;

    procedure OnTextChange;
  private
    function GetGUID: TGUID;
    function GetCaption: string;
    function GetDescription: string;
    function GetMenuItemName: string;

    function GetSensor: ISensor;
    function GetSensorTypeIID: TGUID;
  public
    constructor Create(
      const AGUID: TGUID;
      const ACaption: IStringConfigDataElement;
      const ADescription: IStringConfigDataElement;
      const AMenuItemName: IStringConfigDataElement;
      const ASensor: ISensor;
      const ASensorTypeIID: TGUID
    );
  end;

implementation

uses
  i_Listener,
  u_ListenerNotifierLinksList,
  u_ListenerByEvent;

{ TSensorListEntity }

constructor TSensorListEntity.Create(
  const AGUID: TGUID;
  const ACaption, ADescription, AMenuItemName: IStringConfigDataElement;
  const ASensor: ISensor;
  const ASensorTypeIID: TGUID
);
var
  VListener: IListener;
begin
  inherited Create;
  FGUID := AGUID;
  FCaption := ACaption;
  FDescription := ADescription;
  FMenuItemName := AMenuItemName;
  FSensor := ASensor;
  FSensorTypeIID := ASensorTypeIID;
  FLinksList := TListenerNotifierLinksList.Create;
  VListener := TNotifyNoMmgEventListener.Create(Self.OnTextChange);
  FLinksList.Add(
    VListener,
    FCaption.ChangeNotifier
  );
  FLinksList.Add(
    VListener,
    FDescription.ChangeNotifier
  );
  FLinksList.Add(
    VListener,
    FMenuItemName.ChangeNotifier
  );

  FLinksList.ActivateLinks;
end;

function TSensorListEntity.GetSensor: ISensor;
begin
  Result := FSensor;
end;

function TSensorListEntity.GetSensorTypeIID: TGUID;
begin
  Result := FSensorTypeIID;
end;

function TSensorListEntity.GetCaption: string;
begin
  Result := FCaption.Value;
end;

function TSensorListEntity.GetDescription: string;
begin
  Result := FDescription.Value;
end;

function TSensorListEntity.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TSensorListEntity.GetMenuItemName: string;
begin
  Result := FMenuItemName.Value;
end;

procedure TSensorListEntity.OnTextChange;
begin
  DoChangeNotify;
end;

end.
