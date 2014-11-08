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

unit u_MarkNameGenerator;

interface


uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_StringConfigDataElement,
  i_MarkNameGenerator,
  u_ConfigDataElementComplexBase;

type
  TMarkNameGenerator = class(TConfigDataElementComplexBase, IMarkNameGenerator)
  private
    FFormatString: IStringConfigDataElement;
    FCounter: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetFormatString: IStringConfigDataElement;

    function GetCounter: Integer;
    procedure SetCounter(AValue: Integer);

    function GetNewName: string;
  public
    constructor Create(const AFormatString: IStringConfigDataElement);
  end;

implementation

uses
  SysUtils,
  u_ConfigSaveLoadStrategyBasicUseProvider;

{ TMarkNameGenerator }

constructor TMarkNameGenerator.Create(const AFormatString: IStringConfigDataElement);
begin
  inherited Create;
  FFormatString := AFormatString;
  Add(FFormatString, TConfigSaveLoadStrategyBasicUseProvider.Create);
  FCounter := 0;
end;

procedure TMarkNameGenerator.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FCounter := AConfigData.ReadInteger('Counter', FCounter);
    SetChanged;
  end;
end;

procedure TMarkNameGenerator.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger('Counter', FCounter);
end;

function TMarkNameGenerator.GetCounter: Integer;
begin
  LockRead;
  try
    Result := FCounter;
  finally
    UnlockRead;
  end;
end;

function TMarkNameGenerator.GetFormatString: IStringConfigDataElement;
begin
  Result := FFormatString;
end;

function TMarkNameGenerator.GetNewName: string;
begin
  LockWrite;
  try
    Result := Format(FFormatString.Value, [FCounter]);
    Inc(FCounter);
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TMarkNameGenerator.SetCounter(AValue: Integer);
begin
  LockWrite;
  try
    if FCounter <> AValue then begin
      FCounter := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
