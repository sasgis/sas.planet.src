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
  i_Sensor,
  i_ListenerNotifierLinksList,
  u_ChangeableBase;

type
  TSensorBase = class(TChangeableWithSimpleLockBase, ISensor)
  private
    FLinksList: IListenerNotifierLinksList;
  protected
    property LinksList: IListenerNotifierLinksList read FLinksList;
  protected
    procedure OnDataChanged; virtual; abstract;
    function GetSensorTypeIID: TGUID; virtual; abstract;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const ADataChangeNotifier: INotifier
    );
  end;

  TSensorDoubeleValue = class(TSensorBase)
  private
    FLastValue: Double;
    function ValueChanged(const AOld, ANew: Double): Boolean;
  protected
    procedure OnDataChanged; override;
    function GetCurrentValue: Double; virtual; abstract;
  protected
    function GetValue: Double;
  end;

  TSensorByteValue = class(TSensorBase)
  private
    FLastValue: Byte;
    function ValueChanged(const AOld, ANew: Byte): Boolean;
  protected
    procedure OnDataChanged; override;
    function GetCurrentValue: Byte; virtual; abstract;
  protected
    function GetValue: Byte;
  end;

  TSensorDateTimeValue = class(TSensorBase)
  private
    FLastValue: TDateTime;
    function ValueChanged(const AOld, ANew: TDateTime): Boolean;
  protected
    procedure OnDataChanged; override;
    function GetCurrentValue: TDateTime; virtual; abstract;
  protected
    function GetValue: TDateTime;
  end;

  TSensorPosititonValue = class(TSensorBase)
  private
    FLastValue: TDoublePoint;
    function ValueChanged(const AOld, ANew: TDoublePoint): Boolean;
  protected
    procedure OnDataChanged; override;
    function GetCurrentValue: TDoublePoint; virtual; abstract;
  protected
    function GetValue: TDoublePoint;
  end;

  TSensorTextValue = class(TSensorBase)
  private
    FLastValue: string;
    function ValueChanged(const AOld, ANew: string): Boolean;
  protected
    procedure OnDataChanged; override;
    function GetCurrentValue: string; virtual; abstract;
  protected
    function GetText: string;
  end;

  TSensorGPSSatellitesValue = class(TSensorBase)
  private
    FLastValue: IGPSSatellitesInView;
    function ValueChanged(const AOld, ANew: IGPSSatellitesInView): Boolean;
  protected
    procedure OnDataChanged; override;
    function GetCurrentValue: IGPSSatellitesInView; virtual; abstract;
  protected
    function GetInfo: IGPSSatellitesInView;
  end;

implementation

uses
  Math,
  u_ListenerByEvent,
  u_ListenerNotifierLinksList,
  u_GeoFunc;

{ TSensorBase }

constructor TSensorBase.Create(
  const ADataChangeNotifier: INotifier
);
begin
  inherited Create;

  FLinksList := TListenerNotifierLinksList.Create;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnDataChanged),
    ADataChangeNotifier
  );
end;

procedure TSensorBase.AfterConstruction;
begin
  inherited;
  FLinksList.ActivateLinks;
  OnDataChanged;
end;

{ TSensorDoubeleValue }

function TSensorDoubeleValue.GetValue: Double;
begin
  CS.BeginRead;
  try
    Result := FLastValue;
  finally
    CS.EndRead;
  end;
end;

procedure TSensorDoubeleValue.OnDataChanged;
var
  VValue: Double;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := GetCurrentValue;
  CS.BeginWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
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

function TSensorDateTimeValue.GetValue: TDateTime;
begin
  CS.BeginRead;
  try
    Result := FLastValue;
  finally
    CS.EndRead;
  end;
end;

procedure TSensorDateTimeValue.OnDataChanged;
var
  VValue: TDateTime;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := GetCurrentValue;
  CS.BeginWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
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

function TSensorPosititonValue.GetValue: TDoublePoint;
begin
  CS.BeginRead;
  try
    Result := FLastValue;
  finally
    CS.EndRead;
  end;
end;

procedure TSensorPosititonValue.OnDataChanged;
var
  VValue: TDoublePoint;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := GetCurrentValue;
  CS.BeginWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

function TSensorPosititonValue.ValueChanged(const AOld,
  ANew: TDoublePoint): Boolean;
begin
  Result := not DoublePointsEqual(AOld, ANew);
end;

{ TSensorTextValue }

function TSensorTextValue.GetText: string;
begin
  CS.BeginRead;
  try
    Result := FLastValue;
  finally
    CS.EndRead;
  end;
end;

procedure TSensorTextValue.OnDataChanged;
var
  VValue: string;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := GetCurrentValue;
  CS.BeginWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

function TSensorTextValue.ValueChanged(const AOld, ANew: string): Boolean;
begin
  Result := AOld <> ANew;
end;

{ TSensorByteValue }

function TSensorByteValue.GetValue: Byte;
begin
  CS.BeginRead;
  try
    Result := FLastValue;
  finally
    CS.EndRead;
  end;
end;

procedure TSensorByteValue.OnDataChanged;
var
  VValue: Byte;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := GetCurrentValue;
  CS.BeginWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

function TSensorByteValue.ValueChanged(const AOld, ANew: Byte): Boolean;
begin
  Result := AOld <> ANew;
end;

{ TSensorGPSSatellitesValue }

function TSensorGPSSatellitesValue.GetInfo: IGPSSatellitesInView;
begin
  CS.BeginRead;
  try
    Result := FLastValue;
  finally
    CS.EndRead;
  end;
end;

procedure TSensorGPSSatellitesValue.OnDataChanged;
var
  VValue: IGPSSatellitesInView;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := GetCurrentValue;
  CS.BeginWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

function TSensorGPSSatellitesValue.ValueChanged(
  const AOld, ANew: IGPSSatellitesInView
): Boolean;
begin
  Result := AOld <> ANew;
end;

end.
