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

unit u_SensorTextFromGPSRecorder;

interface

uses
  i_GPSRecorder,
  u_SensorBase;

type
  TSensorDoubeleValueFromGPSRecorder = class(TSensorDoubeleValue)
  private
    FGPSRecorder: IGPSRecorder;
  protected
    property GPSRecorder: IGPSRecorder read FGPSRecorder;
  public
    constructor Create(
      ACanReset: Boolean;
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorDateTimeValueFromGPSRecorder = class(TSensorDateTimeValue)
  private
    FGPSRecorder: IGPSRecorder;
  protected
    property GPSRecorder: IGPSRecorder read FGPSRecorder;
  public
    constructor Create(
      ACanReset: Boolean;
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorTextValueFromGPSRecorder = class(TSensorTextValue)
  private
    FGPSRecorder: IGPSRecorder;
  protected
    property GPSRecorder: IGPSRecorder read FGPSRecorder;
  public
    constructor Create(
      ACanReset: Boolean;
      const AGPSRecorder: IGPSRecorder
    );
  end;

implementation

uses
  SysUtils;

{ TSensorDoubeleValueFromGPSRecorder }

constructor TSensorDoubeleValueFromGPSRecorder.Create(
  ACanReset: Boolean;
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(
    ACanReset,
    AGPSRecorder.ChangeNotifier
  );
  FGPSRecorder := AGPSRecorder;
end;

{ TSensorDateTimeValueFromGPSRecorder }

constructor TSensorDateTimeValueFromGPSRecorder.Create(
  ACanReset: Boolean;
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(
    ACanReset,
    AGPSRecorder.ChangeNotifier
  );
  FGPSRecorder := AGPSRecorder;
end;

{ TSensorTextValueFromGPSRecorder }

constructor TSensorTextValueFromGPSRecorder.Create(ACanReset: Boolean;
  const AGPSRecorder: IGPSRecorder);
begin
  inherited Create(
    ACanReset,
    AGPSRecorder.ChangeNotifier
  );
  FGPSRecorder := AGPSRecorder;
end;

end.
