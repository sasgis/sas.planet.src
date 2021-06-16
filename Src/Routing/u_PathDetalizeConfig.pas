{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_PathDetalizeConfig;

interface

uses
  i_PathDetalizeConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase;

type
  TPathDetalizeConfig = class(TConfigDataElementBase, IPathDetalizeConfig)
  private
    FEnableZlzk: Boolean;
    FEnableProjectOSRM: Boolean;
    FArrayOfProjectOSRM: TArrayOfProjectOSRM;
    FEnableAutomaticRouting: Boolean;
    FDefaultProvider: TGUID;
  private
    { IPathDetalizeConfig }
    function GetEnableProjectOSRM: Boolean;
    function GetEnableZlzk: Boolean;

    function GetArrayOfProjectOSRM: TArrayOfProjectOSRM;

    function GetEnableAutomaticRouting: Boolean;
    procedure SetEnableAutomaticRouting(const AValue: Boolean);

    function GetDefaultProvider: TGUID;
    procedure SetDefaultProvider(const AValue: TGUID);
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create;
  end;

implementation

uses
  Classes,
  SysUtils,
  libcrc32,
  c_ZeroGUID,
  u_ConfigProviderHelpers;

{ TPathDetalizeConfig }

constructor TPathDetalizeConfig.Create;
begin
  inherited Create;
  FEnableProjectOSRM := True;
  FEnableZlzk := True;
  FArrayOfProjectOSRM := nil;
  FEnableAutomaticRouting := True;
  FDefaultProvider := CGUID_Zero; // first available
end;

procedure TPathDetalizeConfig.DoReadConfig(const AConfigData: IConfigDataProvider);

  procedure ReadArrayOfProjectOSRM(const AStr: string);
  var
    I, J, K: Integer;
    VGuid: TGUID;
    VAddress: AnsiString;
    VList: TStringList;
  begin
    VList := TStringList.Create;
    try
      VList.Delimiter := ';';
      VList.StrictDelimiter := True;
      VList.DelimitedText := AStr;
      K := 0;
      SetLength(FArrayOfProjectOSRM, VList.Count);
      for I := 0 to Length(FArrayOfProjectOSRM) - 1 do begin
        FArrayOfProjectOSRM[K].Address := Trim(VList.Strings[I]);
        if FArrayOfProjectOSRM[K].Address = '' then begin
          Continue;
        end;
        if Pos('zlzk.biz', LowerCase(FArrayOfProjectOSRM[K].Address)) > 0 then begin
          // this server is built in now
          Continue;
        end;
        VAddress := AnsiString(FArrayOfProjectOSRM[K].Address);
        VGuid := CGUID_Zero;
        VGuid.D1 := libcrc32.crc32(0, @VAddress[1], Length(VAddress));
        for J := 0 to Length(FArrayOfProjectOSRM[K].Guid) - 1 do begin
          VGuid.D2 := J+1;
          FArrayOfProjectOSRM[K].Guid[J] := VGuid;
        end;
        Inc(K);
      end;
      SetLength(FArrayOfProjectOSRM, K);
    finally
      VList.Free;
    end;
  end;

begin
  inherited;
  if AConfigData <> nil then begin
    FEnableZlzk := AConfigData.ReadBool('EnableZlzk', FEnableZlzk);
    FEnableProjectOSRM := AConfigData.ReadBool('EnableProjectOSRM', FEnableProjectOSRM);
    ReadArrayOfProjectOSRM( AConfigData.ReadString('CustomOSRM', '') );
    FEnableAutomaticRouting := AConfigData.ReadBool('EnableAutomaticRouting', FEnableAutomaticRouting);
    FDefaultProvider := ReadGUID(AConfigData, 'DefaultProvider', FDefaultProvider);
    SetChanged;
  end;
end;

procedure TPathDetalizeConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);

  function _GetCustomOSRM: string;
  const
    CDelim: array [Boolean] of string = ('', ';');
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to Length(FArrayOfProjectOSRM) - 1 do begin
      Result := Result + CDelim[I>0] + FArrayOfProjectOSRM[I].Address;
    end;
  end;

begin
  inherited;
  AConfigData.WriteBool('EnableZlzk', FEnableZlzk);
  AConfigData.WriteBool('EnableProjectOSRM', FEnableProjectOSRM);
  AConfigData.WriteString('CustomOSRM', _GetCustomOSRM);
  AConfigData.WriteBool('EnableAutomaticRouting', FEnableAutomaticRouting);
  AConfigData.WriteString('DefaultProvider', GUIDToString(FDefaultProvider));
end;

function TPathDetalizeConfig.GetArrayOfProjectOSRM: TArrayOfProjectOSRM;
begin
  LockRead;
  try
    Result := Copy(FArrayOfProjectOSRM);
  finally
    UnlockRead;
  end;
end;

function TPathDetalizeConfig.GetDefaultProvider: TGUID;
begin
  LockRead;
  try
    Result := FDefaultProvider;
  finally
    UnlockRead;
  end;
end;

function TPathDetalizeConfig.GetEnableAutomaticRouting: Boolean;
begin
  LockRead;
  try
    Result := FEnableAutomaticRouting;
  finally
    UnlockRead;
  end;
end;

function TPathDetalizeConfig.GetEnableZlzk: Boolean;
begin
  LockRead;
  try
    Result := FEnableZlzk;
  finally
    UnlockRead;
  end;
end;

function TPathDetalizeConfig.GetEnableProjectOSRM: Boolean;
begin
  LockRead;
  try
    Result := FEnableProjectOSRM;
  finally
    UnlockRead;
  end;
end;

procedure TPathDetalizeConfig.SetDefaultProvider(const AValue: TGUID);
begin
  LockWrite;
  try
    if not IsEqualGUID(FDefaultProvider, AValue) then begin
      FDefaultProvider := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPathDetalizeConfig.SetEnableAutomaticRouting(const AValue: Boolean);
begin
  LockWrite;
  try
    if FEnableAutomaticRouting <> AValue then begin
      FEnableAutomaticRouting := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
