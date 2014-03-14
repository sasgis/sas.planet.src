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

unit u_SensorBase;

interface

uses
  t_GeoTypes,
  i_Notifier,
  i_GPS,
  i_StringConfigDataElement,
  i_Sensor,
  i_SensorList,
  i_ListenerNotifierLinksList,
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

  TSensorBase = class(TConfigDataElementBaseEmptySaveLoad, ISensor)
  private
    FLinksList: IListenerNotifierLinksList;
  protected
    property LinksList: IListenerNotifierLinksList read FLinksList;
  protected
    function GetSensorTypeIID: TGUID; virtual; abstract;
  public
    constructor Create;
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
    procedure AfterConstruction; override;
  public
    constructor Create(
      const ADataChangeNotifier: INotifier
    );
  end;

  TSensorByteValue = class(TSensorBase)
  private
    FLastValue: Byte;
    procedure OnDataChanged;
    function ValueChanged(const AOld, ANew: Byte): Boolean;
  protected
    function GetCurrentValue: Byte; virtual; abstract;
  protected
    function GetValue: Byte;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const ADataChangeNotifier: INotifier
    );
  end;

  TSensorDateTimeValue = class(TSensorBase)
  private
    FLastValue: TDateTime;
    procedure OnDataChanged;
    function ValueChanged(const AOld, ANew: TDateTime): Boolean;
  protected
    function GetCurrentValue: TDateTime; virtual; abstract;
  protected
    function GetValue: TDateTime;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const ADataChangeNotifier: INotifier
    );
  end;

  TSensorPosititonValue = class(TSensorBase)
  private
    FLastValue: TDoublePoint;
    procedure OnDataChanged;
    function ValueChanged(const AOld, ANew: TDoublePoint): Boolean;
  protected
    function GetCurrentValue: TDoublePoint; virtual; abstract;
  protected
    function GetValue: TDoublePoint;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const ADataChangeNotifier: INotifier
    );
  end;

  TSensorTextValue = class(TSensorBase)
  private
    FLastValue: string;
    procedure OnDataChanged;
    function ValueChanged(const AOld, ANew: string): Boolean;
  protected
    function GetCurrentValue: string; virtual; abstract;
  protected
    function GetText: string;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const ADataChangeNotifier: INotifier
    );
  end;

  TSensorGPSSatellitesValue = class(TSensorBase)
  private
    FLastValue: IGPSSatellitesInView;
    procedure OnDataChanged;
    function ValueChanged(const AOld, ANew: IGPSSatellitesInView): Boolean;
  protected
    function GetCurrentValue: IGPSSatellitesInView; virtual; abstract;
  protected
    function GetInfo: IGPSSatellitesInView;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const ADataChangeNotifier: INotifier
    );
  end;

implementation

uses
  Math,
  u_ListenerByEvent,
  u_ListenerNotifierLinksList,
  u_GeoFunc;

{ TSensorBase }

constructor TSensorBase.Create;
begin
  inherited Create;

  FLinksList := TListenerNotifierLinksList.Create;
  FLinksList.ActivateLinks;
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

procedure TSensorDoubeleValue.AfterConstruction;
begin
  inherited;
  OnDataChanged;
end;

constructor TSensorDoubeleValue.Create(
  const ADataChangeNotifier: INotifier);
begin
  inherited Create;
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
begin
  VValue := GetCurrentValue;
  LockWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
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

{ TSensorDateTimeValue }

procedure TSensorDateTimeValue.AfterConstruction;
begin
  inherited;
  OnDataChanged;
end;

constructor TSensorDateTimeValue.Create(
  const ADataChangeNotifier: INotifier
);
begin
  inherited Create;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnDataChanged),
    ADataChangeNotifier
  );
end;

function TSensorDateTimeValue.GetValue: TDateTime;
begin
  LockRead;
  try
    Result := FLastValue;
  finally
    UnlockRead;
  end;
end;

procedure TSensorDateTimeValue.OnDataChanged;
var
  VValue: TDateTime;
begin
  VValue := GetCurrentValue;
  LockWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TSensorDateTimeValue.ValueChanged(const AOld,
  ANew: TDateTime): Boolean;
var
  VOldIsNan: Boolean;
  VNewIsNan: Boolean;
begin
  VOldIsNan := IsNan(AOld);
  VNewIsNan := IsNan(ANew);
  if VOldIsNan and VNewIsNan then begin
    Result := False;
  end else if (not VOldIsNan) and (not VNewIsNan) then begin
    Result := (Abs(AOld - ANew) > 0.000001);
  end else begin
    Result := True;
  end;
end;

{ TSensorPosititonValue }

procedure TSensorPosititonValue.AfterConstruction;
begin
  inherited;
  OnDataChanged;
end;

constructor TSensorPosititonValue.Create(
  const ADataChangeNotifier: INotifier
);
begin
  inherited Create;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnDataChanged),
    ADataChangeNotifier
  );
end;

function TSensorPosititonValue.GetValue: TDoublePoint;
begin
  LockRead;
  try
    Result := FLastValue;
  finally
    UnlockRead;
  end;
end;

procedure TSensorPosititonValue.OnDataChanged;
var
  VValue: TDoublePoint;
begin
  VValue := GetCurrentValue;
  LockWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TSensorPosititonValue.ValueChanged(const AOld,
  ANew: TDoublePoint): Boolean;
begin
  Result := not DoublePointsEqual(AOld, ANew);
end;

{ TSensorTextValue }

procedure TSensorTextValue.AfterConstruction;
begin
  inherited;
  OnDataChanged;
end;

constructor TSensorTextValue.Create(
  const ADataChangeNotifier: INotifier
);
begin
  inherited Create;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnDataChanged),
    ADataChangeNotifier
  );
end;

function TSensorTextValue.GetText: string;
begin
  LockRead;
  try
    Result := FLastValue;
  finally
    UnlockRead;
  end;
end;

procedure TSensorTextValue.OnDataChanged;
var
  VValue: string;
begin
  VValue := GetCurrentValue;
  LockWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TSensorTextValue.ValueChanged(const AOld, ANew: string): Boolean;
begin
  Result := AOld <> ANew;
end;

{ TSensorByteValue }

procedure TSensorByteValue.AfterConstruction;
begin
  inherited;
  OnDataChanged;
end;

constructor TSensorByteValue.Create(
  const ADataChangeNotifier: INotifier);
begin
  inherited Create;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnDataChanged),
    ADataChangeNotifier
  );
end;

function TSensorByteValue.GetValue: Byte;
begin
  LockRead;
  try
    Result := FLastValue;
  finally
    UnlockRead;
  end;
end;

procedure TSensorByteValue.OnDataChanged;
var
  VValue: Byte;
begin
  VValue := GetCurrentValue;
  LockWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TSensorByteValue.ValueChanged(const AOld, ANew: Byte): Boolean;
begin
  Result := AOld <> ANew;
end;

{ TSensorGPSSatellitesValue }

procedure TSensorGPSSatellitesValue.AfterConstruction;
begin
  inherited;
  OnDataChanged;
end;

constructor TSensorGPSSatellitesValue.Create(
  const ADataChangeNotifier: INotifier);
begin
  inherited Create;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnDataChanged),
    ADataChangeNotifier
  );
end;

function TSensorGPSSatellitesValue.GetInfo: IGPSSatellitesInView;
begin
  LockRead;
  try
    Result := FLastValue;
  finally
    UnlockRead;
  end;
end;

procedure TSensorGPSSatellitesValue.OnDataChanged;
var
  VValue: IGPSSatellitesInView;
begin
  VValue := GetCurrentValue;
  LockWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TSensorGPSSatellitesValue.ValueChanged(
  const AOld, ANew: IGPSSatellitesInView
): Boolean;
begin
  Result := AOld <> ANew;
end;

end.
