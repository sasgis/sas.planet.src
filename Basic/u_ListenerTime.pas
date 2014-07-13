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

unit u_ListenerTime;

interface

uses
  t_Listener,
  i_SimpleFlag,
  i_ListenerTime,
  u_BaseInterfacedObject;

type
  TListenerTTLCheck = class(TBaseInterfacedObject, IListenerTimeWithUsedFlag, IListenerTime)
  private
    FOnTrimByTTL: TNotifyListenerNoMmgEvent;
    FUseFlag: ISimpleFlag;
    FUpdateFlag: ISimpleFlag;
    FLastUseTime: Cardinal;
    FTTL: Cardinal;
  private
    procedure Notification(const ANow: Cardinal);
    procedure UpdateUseTime;
    procedure CheckUseTimeUpdated;
  public
    constructor Create(
      AOnTrimByTTL: TNotifyListenerNoMmgEvent;
      ATTL: Cardinal
    );
  end;

  TListenerTimeCheck = class(TBaseInterfacedObject, IListenerTime)
  private
    FOnTime: TNotifyListenerNoMmgEvent;
    FCheckInterval: Cardinal;

    FNextTime: Cardinal;
  private
    procedure Notification(const ANow: Cardinal);
  public
    constructor Create(
      AOnTime: TNotifyListenerNoMmgEvent;
      ACheckInterval: Cardinal
    );
  end;

implementation

uses
  u_SimpleFlagWithInterlock;

{ TListenerTTLCheck }

constructor TListenerTTLCheck.Create(
  AOnTrimByTTL: TNotifyListenerNoMmgEvent;
  ATTL: Cardinal
);
begin
  inherited Create;
  FOnTrimByTTL := AOnTrimByTTL;
  FTTL := ATTL;
  FUpdateFlag := TSimpleFlagWithInterlock.Create;
  FUseFlag := TSimpleFlagWithInterlock.Create;
  FLastUseTime := 0;
end;

procedure TListenerTTLCheck.Notification(const ANow: Cardinal);
var
  VCleanTime: Cardinal;
  VLastUseTime: Cardinal;
begin
  if FUseFlag.CheckFlagAndReset then begin
    FLastUseTime := ANow;
  end else begin
    VLastUseTime := FLastUseTime;
    if VLastUseTime <> 0 then begin
      VCleanTime := VLastUseTime + FTTL;
      if (VCleanTime <= ANow) or ((ANow < 1 shl 29) and (VCleanTime > 1 shl 30)) then begin
        FUpdateFlag.CheckFlagAndReset;
        FOnTrimByTTL;
        FLastUseTime := 0;
      end;
    end;
  end;
end;

procedure TListenerTTLCheck.UpdateUseTime;
begin
  FUseFlag.SetFlag;
end;

procedure TListenerTTLCheck.CheckUseTimeUpdated;
begin
  if not FUpdateFlag.CheckFlag then begin
    FUpdateFlag.SetFlag;
    UpdateUseTime;
  end;
end;

{ TListenerTimeCheck }

constructor TListenerTimeCheck.Create(
  AOnTime: TNotifyListenerNoMmgEvent;
  ACheckInterval: Cardinal
);
begin
  Assert(Assigned(AOnTime));
  Assert(ACheckInterval <= 3600000);
  inherited Create;
  FOnTime := AOnTime;
  FCheckInterval := ACheckInterval;
  if FCheckInterval > 3600000  then begin
    FCheckInterval := 3600000;
  end;

  FNextTime := 0;
end;

procedure TListenerTimeCheck.Notification(const ANow: Cardinal);
var
  VNextTime: Cardinal;
begin
  VNextTime := FNextTime;
  if (VNextTime <= ANow) or ((ANow < 1 shl 29) and (VNextTime > 1 shl 30)) then begin
    if Assigned(FOnTime) then begin
      FOnTime;
    end;
    FNextTime := ANow + FCheckInterval;
  end;
end;

end.
