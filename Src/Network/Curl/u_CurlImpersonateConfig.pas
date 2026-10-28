{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_CurlImpersonateConfig;

interface

uses
  Classes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_CurlImpersonateConfig,
  i_StringListStatic,
  u_ConfigDataElementBase,
  u_StringListStatic,
  u_BaseInterfacedObject;

type
  TCurlImpersonateConfigStatic = class(TBaseInterfacedObject, ICurlImpersonateConfigStatic)
  private
    FTarget: AnsiString;
  private
    function GetTarget: AnsiString;
  public
    constructor Create(
      const ATarget: AnsiString
    );
  end;

  TCurlImpersonateConfig = class(TConfigDataElementWithStaticBase, ICurlImpersonateConfig)
  private
    FTargets: IStringListStatic;
    FTarget: AnsiString;
  private
    function GetDefaultTarget: AnsiString;
    function CheckTarget(const ATarget: AnsiString): AnsiString;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetTargets: IStringListStatic;

    function GetTarget: AnsiString;
    procedure SetTarget(const AValue: AnsiString);

    function GetStatic: ICurlImpersonateConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils;

const
  cImpersonateTargetsFileName = 'curl-impersonate.txt';

procedure LoadTargetsFromFile(var AResult: TStringList);
var
  I: Integer;
  VPos: Integer;
  VFileName: string;
  VLine: string;
  VSource: TStringList;
begin
  AResult.Clear;

  VFileName := ExtractFilePath(ParamStr(0)) + cImpersonateTargetsFileName;
  if not FileExists(VFileName) then begin
    Exit;
  end;

  VSource := TStringList.Create;
  try
    try
      VSource.LoadFromFile(VFileName);
    except
      VSource.Clear;
      Exit;
    end;
    for I := 0 to VSource.Count - 1 do begin
      VLine := VSource.Strings[I];
      // cut off a comment: either a whole-line one or one at the end of a line
      VPos := Pos('#', VLine);
      if VPos > 0 then begin
        SetLength(VLine, VPos - 1);
      end;
      VLine := Trim(VLine);
      if VLine <> '' then begin
        AResult.Add(VLine);
      end;
    end;
  finally
    VSource.Free;
  end;
end;

{ TCurlImpersonateConfig }

constructor TCurlImpersonateConfig.Create;
var
  VList: TStringList;
begin
  inherited Create;

  VList := TStringList.Create;
  try
    LoadTargetsFromFile(VList);
    FTargets := TStringListStatic.CreateByStrings(VList);
  finally
    VList.Free;
  end;

  FTarget := GetDefaultTarget;
end;

function TCurlImpersonateConfig.GetDefaultTarget: AnsiString;
begin
  // the library has no default impersonation target, so the first
  // line of the available targets is used as the default value
  if FTargets.Count > 0 then begin
    Result := AnsiString(FTargets.Items[0]);
  end else begin
    Result := '';
  end;
end;

function TCurlImpersonateConfig.CheckTarget(const ATarget: AnsiString): AnsiString;
var
  I: Integer;
begin
  // if the target is not in the list of available targets, return the default one
  for I := 0 to FTargets.Count - 1 do begin
    if AnsiString(FTargets.Items[I]) = ATarget then begin
      Result := ATarget;
      Exit;
    end;
  end;
  Result := GetDefaultTarget;
end;

function TCurlImpersonateConfig.CreateStatic: IInterface;
var
  VStatic: ICurlImpersonateConfigStatic;
begin
  VStatic := TCurlImpersonateConfigStatic.Create(FTarget);
  Result := VStatic;
end;

procedure TCurlImpersonateConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FTarget := CheckTarget(AConfigData.ReadAnsiString('Target', FTarget));
    SetChanged;
  end;
end;

procedure TCurlImpersonateConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteAnsiString('Target', FTarget);
end;

function TCurlImpersonateConfig.GetTargets: IStringListStatic;
begin
  Result := FTargets;
end;

function TCurlImpersonateConfig.GetTarget: AnsiString;
begin
  LockRead;
  try
    Result := FTarget;
  finally
    UnlockRead;
  end;
end;

procedure TCurlImpersonateConfig.SetTarget(const AValue: AnsiString);
begin
  LockWrite;
  try
    if FTarget <> AValue then begin
      FTarget := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TCurlImpersonateConfig.GetStatic: ICurlImpersonateConfigStatic;
begin
  Result := ICurlImpersonateConfigStatic(GetStaticInternal);
end;

{ TCurlImpersonateConfigStatic }

constructor TCurlImpersonateConfigStatic.Create(
  const ATarget: AnsiString
);
begin
  inherited Create;
  FTarget := ATarget;
end;

function TCurlImpersonateConfigStatic.GetTarget: AnsiString;
begin
  Result := FTarget;
end;

end.
