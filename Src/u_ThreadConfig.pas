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

unit u_ThreadConfig;

interface

uses
  Classes,
  i_ThreadConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase;

type
  TThreadConfig = class(TConfigDataElementBase, IThreadConfig)
  private
    FPriority: TThreadPriority;
    FPriorityDef: TThreadPriority;
  private
    function GetPriority: TThreadPriority;
    procedure SetPriority(AValue: TThreadPriority);
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create(
      ADefPriority: TThreadPriority
    );

  end;

implementation

{ TThreadPriorityConfig }

constructor TThreadConfig.Create(ADefPriority: TThreadPriority);
begin
  inherited Create;
  FPriorityDef := ADefPriority;
  if (FPriorityDef < tpIdle) or (FPriorityDef > tpHigher) then begin
    FPriorityDef := tpLower;
  end;
  FPriority := FPriorityDef;
end;

procedure TThreadConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetPriority(TThreadPriority(AConfigData.ReadInteger('ThreadPriority', Ord(FPriority))));
  end;
end;

procedure TThreadConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger('ThreadPriority', Ord(FPriority));
end;

function TThreadConfig.GetPriority: TThreadPriority;
begin
  LockRead;
  try
    Result := FPriority;
  finally
    UnlockRead;
  end;
end;

procedure TThreadConfig.SetPriority(AValue: TThreadPriority);
var
  VValue: TThreadPriority;
begin
  VValue := AValue;
  if (VValue < tpIdle) or (VValue > tpHigher) then begin
    VValue := FPriorityDef;
  end;

  LockWrite;
  try
    if FPriority <> VValue then begin
      FPriority := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
