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

unit u_ZmpConfig;

interface

uses
//  Windows,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ZmpConfig,
  u_ConfigDataElementComplexBase;

type
  TZmpConfig = class(TConfigDataElementComplexBase, IZmpConfig)
  private
//    FIgnoreMIMEType: Boolean;
//    FDefaultMIMEType: string;
//    FExpectedMIMETypes: string;
//    FWaitInterval: Cardinal;
    FMaxConnectToServerCount: Cardinal;
//    FIteratorSubRectSize: TPoint;
//    FUrlBase: string;
//    FRequestHead: string;
//    FVersion: Variant;

   protected
//    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
//    function GetIgnoreMIMEType: Boolean;
//    procedure SetIgnoreMIMEType(AValue: Boolean);
//
//    function GetDefaultMIMEType: string;
//    procedure SetDefaultMIMEType(AValue: string);
//
//    function GetExpectedMIMETypes: string;
//    procedure SetExpectedMIMETypes(AValue: string);
//
//    function GetWaitInterval: Cardinal;
//    procedure SetWaitInterval(AValue: Cardinal);

    function GetMaxConnectToServerCount: Cardinal;
    procedure SetMaxConnectToServerCount(AValue: Cardinal);

//    function GetIteratorSubRectSize: TPoint;
//    procedure SetIteratorSubRectSize(AValue: TPoint);
//
//    function GetUrlBase: string;
//    procedure SetUrlBase(AValue: string);
//
//    function GetRequestHead: string;
//    procedure SetRequestHead(AValue: string);
//
//    function GetVersion: Variant;
//    procedure SetVersion(AValue: Variant);
  public
    constructor Create;
  end;

implementation

{ TZmpConfig }

constructor TZmpConfig.Create;
begin
  inherited;
  FMaxConnectToServerCount := 4;
end;

procedure TZmpConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FMaxConnectToServerCount := AConfigData.ReadInteger('MaxConnectToServerCount', FMaxConnectToServerCount);
    SetChanged;
  end;
end;

procedure TZmpConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('MaxConnectToServerCount', FMaxConnectToServerCount);
end;

function TZmpConfig.GetMaxConnectToServerCount: Cardinal;
begin
  LockRead;
  try
    Result := FMaxConnectToServerCount;
  finally
    UnlockRead;
  end;
end;

procedure TZmpConfig.SetMaxConnectToServerCount(AValue: Cardinal);
begin
  LockWrite;
  try
    if FMaxConnectToServerCount <> AValue then begin
      FMaxConnectToServerCount := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
