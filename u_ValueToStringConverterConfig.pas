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

unit u_ValueToStringConverterConfig;

interface

uses
  i_Notifier,
  i_Listener,
  t_CommonTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ConfigDataElement,
  i_ValueToStringConverter,
  u_ConfigDataElementBase;

type
  TValueToStringConverterConfig = class(TConfigDataElementWithStaticBase, IValueToStringConverterConfig)
  private
    FDependentOnElement: IConfigDataElement;
    FDependentOnElementListener: IListener;

    FDistStrFormat: TDistStrFormat;
    FIsLatitudeFirst: Boolean;
    FDegrShowFormat: TDegrShowFormat;
    FAreaShowFormat: TAreaStrFormat;
    procedure OnDependentOnElementChange;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetDistStrFormat: TDistStrFormat;
    procedure SetDistStrFormat(AValue: TDistStrFormat);

    function GetIsLatitudeFirst: Boolean;
    procedure SetIsLatitudeFirst(AValue: Boolean);

    function GetDegrShowFormat: TDegrShowFormat;
    procedure SetDegrShowFormat(AValue: TDegrShowFormat);

    function GetAreaShowFormat: TAreaStrFormat;
    procedure SetAreaShowFormat(AValue: TAreaStrFormat);

    function GetStatic: IValueToStringConverter;
  public
    constructor Create(const ADependentOnElement: IConfigDataElement);
    destructor Destroy; override;
  end;


implementation

uses
  u_ListenerByEvent,
  u_ValueToStringConverter;

{ TValueToStringConverterConfig }

constructor TValueToStringConverterConfig.Create(const ADependentOnElement: IConfigDataElement);
begin
  inherited Create;
  FIsLatitudeFirst := True;
  FDistStrFormat := dsfKmAndM;
  FDegrShowFormat := dshCharDegrMinSec;
  FAreaShowFormat := asfAuto;
  FDependentOnElement := ADependentOnElement;
  FDependentOnElementListener := TNotifyNoMmgEventListener.Create(Self.OnDependentOnElementChange);
  FDependentOnElement.GetChangeNotifier.Add(FDependentOnElementListener);
end;

destructor TValueToStringConverterConfig.Destroy;
begin
  if Assigned(FDependentOnElement) and Assigned(FDependentOnElementListener) then begin
    FDependentOnElement.GetChangeNotifier.Remove(FDependentOnElementListener);
    FDependentOnElementListener := nil;
    FDependentOnElement := nil;
  end;
  inherited;
end;

function TValueToStringConverterConfig.CreateStatic: IInterface;
var
  VStatic: IValueToStringConverter;
begin
  VStatic :=
    TValueToStringConverter.Create(
      FDistStrFormat,
      FIsLatitudeFirst,
      FDegrShowFormat,
      FAreaShowFormat
    );
  Result := VStatic;
end;

procedure TValueToStringConverterConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsLatitudeFirst := AConfigData.ReadBool('FirstLat', FIsLatitudeFirst);
    FDistStrFormat := TDistStrFormat(AConfigData.ReadInteger('DistFormat', Integer(FDistStrFormat)));
    FDegrShowFormat := TDegrShowFormat(AConfigData.ReadInteger('DegrisShowFormat', Integer(FDegrShowFormat)));
    FAreaShowFormat := TAreaStrFormat(AConfigData.ReadInteger('AreaShowFormat', Integer(FAreaShowFormat)));
    SetChanged;
  end;
end;

procedure TValueToStringConverterConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('FirstLat', FIsLatitudeFirst);
  AConfigData.WriteInteger('DistFormat', Integer(FDistStrFormat));
  AConfigData.WriteInteger('DegrisShowFormat', Integer(FDegrShowFormat));
  AConfigData.WriteInteger('AreaShowFormat', Integer(FAreaShowFormat));
end;

function TValueToStringConverterConfig.GetAreaShowFormat: TAreaStrFormat;
begin
  LockRead;
  try
    Result := FAreaShowFormat;
  finally
    UnlockRead;
  end;
end;

function TValueToStringConverterConfig.GetDegrShowFormat: TDegrShowFormat;
begin
  LockRead;
  try
    Result := FDegrShowFormat;
  finally
    UnlockRead;
  end;
end;

function TValueToStringConverterConfig.GetDistStrFormat: TDistStrFormat;
begin
  LockRead;
  try
    Result := FDistStrFormat;
  finally
    UnlockRead;
  end;
end;

function TValueToStringConverterConfig.GetIsLatitudeFirst: Boolean;
begin
  LockRead;
  try
    Result := FIsLatitudeFirst;
  finally
    UnlockRead;
  end;
end;

function TValueToStringConverterConfig.GetStatic: IValueToStringConverter;
begin
  Result := IValueToStringConverter(GetStaticInternal);
end;

procedure TValueToStringConverterConfig.OnDependentOnElementChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TValueToStringConverterConfig.SetAreaShowFormat(
  AValue: TAreaStrFormat);
begin
  LockWrite;
  try
    if FAreaShowFormat <> AValue then begin
      FAreaShowFormat := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TValueToStringConverterConfig.SetDegrShowFormat(
  AValue: TDegrShowFormat);
begin
  LockWrite;
  try
    if FDegrShowFormat <> AValue then begin
      FDegrShowFormat := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TValueToStringConverterConfig.SetDistStrFormat(
  AValue: TDistStrFormat);
begin
  LockWrite;
  try
    if FDistStrFormat <> AValue then begin
      FDistStrFormat := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TValueToStringConverterConfig.SetIsLatitudeFirst(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsLatitudeFirst <> AValue then begin
      FIsLatitudeFirst := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
