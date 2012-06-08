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

unit u_SensorBase;

interface

uses
  SysUtils,
  i_JclNotify,
  i_LanguageManager,
  i_StringConfigDataElement,
  i_Sensor,
  i_SensorList,
  i_JclListenerNotifierLinksList,
  u_UserInterfaceItemBase,
  u_ConfigDataElementBase;

type
  TSensorListEntity = class(TUserInterfaceItemBase, ISensorListEntity)
  private
    FSensor: ISensor;
    FSensorTypeIID: TGUID;
  private
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

  TSensorBase = class(TInterfacedObject, ISensor)
  private
    FCanReset: Boolean;

    FLock: IReadWriteSync;
    FDataUpdateNotifier: IJclNotifier;
    FLinksList: IJclListenerNotifierLinksList;
  protected
    property LinksList: IJclListenerNotifierLinksList read FLinksList;
    procedure NotifyDataUpdate;
    procedure LockRead;
    procedure LockWrite;
    procedure UnlockRead;
    procedure UnlockWrite; 
  protected
    function CanReset: Boolean;
    procedure Reset; virtual;
    function GetDataUpdateNotifier: IJclNotifier;
  public
    constructor Create(ACanReset: Boolean);
  end;

  TSensorDoubeleValue = class(TSensorBase)
  private
    FLastValue: Double;
    procedure OnDataChanged;
    function ValueChanged(const AOld, ANew: Double): Boolean;
  protected
    function GetCurrentValue: Double; virtual; abstract;
  protected
    function GetValue: Double;
  public
    constructor Create(
      ACanReset: Boolean;
      const ADataChangeNotifier: IJclNotifier
    );
  end;

implementation

uses
  Math,
  u_JclNotify,
  u_NotifyEventListener,
  u_JclListenerNotifierLinksList,
  u_Synchronizer;

{ TSensorBase }

constructor TSensorBase.Create(
  ACanReset: Boolean
);
begin
  inherited Create;
  FCanReset := ACanReset;

  FLock := MakeSyncRW_Var(Self);
  FDataUpdateNotifier := TJclBaseNotifier.Create;
  FLinksList := TJclListenerNotifierLinksList.Create;
  FLinksList.ActivateLinks;
end;

function TSensorBase.CanReset: Boolean;
begin
  Result := FCanReset;
end;

function TSensorBase.GetDataUpdateNotifier: IJclNotifier;
begin
  Result := FDataUpdateNotifier;
end;

procedure TSensorBase.LockRead;
begin
  FLock.BeginRead;
end;

procedure TSensorBase.LockWrite;
begin
  FLock.BeginWrite;
end;

procedure TSensorBase.NotifyDataUpdate;
begin
  FDataUpdateNotifier.Notify(nil);
end;

procedure TSensorBase.Reset;
begin
end;

procedure TSensorBase.UnlockRead;
begin
  FLock.EndRead;
end;

procedure TSensorBase.UnlockWrite;
begin
  FLock.EndWrite
end;

{ TSensorListEntity }

constructor TSensorListEntity.Create(
  const AGUID: TGUID;
  const ACaption, ADescription, AMenuItemName: IStringConfigDataElement;
  const ASensor: ISensor;
  const ASensorTypeIID: TGUID
);
begin
  inherited Create(AGUID, ACaption, ADescription, AMenuItemName);
  FSensor := ASensor;
  FSensorTypeIID := ASensorTypeIID;
end;

function TSensorListEntity.GetSensor: ISensor;
begin
  Result := FSensor;
end;

function TSensorListEntity.GetSensorTypeIID: TGUID;
begin
  Result := FSensorTypeIID;
end;

{ TSensorDoubeleValue }

constructor TSensorDoubeleValue.Create(ACanReset: Boolean;
  const ADataChangeNotifier: IJclNotifier);
begin
  inherited Create(ACanReset);
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnDataChanged),
    ADataChangeNotifier
  );
end;

function TSensorDoubeleValue.GetValue: Double;
begin
  LockRead;
  try
    Result := FLastValue;
  finally
    UnlockRead;
  end;
end;

procedure TSensorDoubeleValue.OnDataChanged;
var
  VValue: Double;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := GetValue;
  LockWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      VNeedNotify := True;
    end;
  finally
    UnlockWrite;
  end;
  if VNeedNotify then begin
    NotifyDataUpdate;
  end;
end;

function TSensorDoubeleValue.ValueChanged(const AOld, ANew: Double): Boolean;
var
  VOldIsNan: Boolean;
  VNewIsNan: Boolean;
begin
  VOldIsNan := IsNan(AOld);
  VNewIsNan := IsNan(ANew);
  if VOldIsNan and VNewIsNan then begin
    Result := False;
  end else if (not VOldIsNan) and (not VNewIsNan) then begin
    Result := (Abs(AOld - ANew) > 0.001);
  end else begin
    Result := True;
  end;
end;

end.
