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

unit u_BuildInfo;

interface

uses
  Windows,
  i_BuildInfo;

type
  TBuildInfo = class(TInterfacedObject, IBuildInfo)
  private
    FVersion: string;
    FBuildDate: TDateTime;
    FBuildType: string;
    FSrcRev: Integer;
    FSrcNode: string;
    FReqRev: Integer;
    FReqNode: string;
  private
    procedure TryLoadInfoFromRes;
  private
    { IBuildInfo }
    function GetVersion: string;
    function GetVersionDetaled: string;
    function GetBuildDate: TDateTime;
    function GetBuildType: string;
    function GetBuildSrcInfo(out ARev: Integer; out ANode: string): Boolean;
    function GetBuildReqInfo(out ARev: Integer; out ANode: string): Boolean;
    function GetCompilerInfo: string;
    function GetDescription: string;
  public
    constructor Create;
  end;

implementation

uses
  Classes,
  SysUtils,
  ExeInfo;

{ TBuildInfo }

constructor TBuildInfo.Create;
begin
  inherited Create;

  FBuildDate := GetBuildDateTime;
  FVersion := FormatDateTime('yymmdd', FBuildDate);

  FBuildType := 'Custom';
  FSrcRev := 0;
  FSrcNode := '';
  FReqRev := 0;
  FReqNode := '';

  TryLoadInfoFromRes;
end;

procedure TBuildInfo.TryLoadInfoFromRes;
var
  VResInfoVer: Integer;
  VResource: TResourceStream;
  VList: TStringList;
begin
  VResource := TResourceStream.Create(hInstance, 'BUILD_INFO_CSV', RT_RCDATA);
  try
    VList := TStringList.Create;
    try
      VList.LoadFromStream(VResource);
      VList.CommaText := VList.Text;
      if VList.Count > 0 then begin
        VResInfoVer := StrToIntDef(VList.Strings[0], 0);
        if (VResInfoVer = 1) and (VList.Count >= 7) then begin
          if VList.Strings[1] <> '' then begin
            FVersion := VList.Strings[1];
          end;
          if VList.Strings[2] <> '' then begin
            FBuildType := VList.Strings[2];
          end;
          FSrcRev := StrToIntDef(VList.Strings[3], 0);
          FSrcNode := VList.Strings[4];
          FReqRev := StrToIntDef(VList.Strings[5], 0);
          FReqNode := VList.Strings[6];
        end;
      end;
    finally
      VList.Free;
    end;
  finally
    VResource.Free;
  end;
end;

function TBuildInfo.GetVersion: string;
begin
  Result := FVersion;
  if FSrcRev > 0 then begin
    Result := Result + '.' + IntToStr(FSrcRev);
  end;
end;

function TBuildInfo.GetVersionDetaled: string;
begin
  Result := GetVersion + ' ' + FBuildType {$IFDEF DEBUG} + ' -= Debug =-' {$ENDIF};
end;

function TBuildInfo.GetBuildDate: TDateTime;
begin
  Result := FBuildDate;
end;

function TBuildInfo.GetBuildType: string;
begin
  Result := FBuildType;
end;

function TBuildInfo.GetBuildSrcInfo(out ARev: Integer; out ANode: string): Boolean;
begin
  ARev := FSrcRev;
  ANode := FSrcNode;
  Result := (ARev > 0) and (ANode <> '');
end;

function TBuildInfo.GetBuildReqInfo(out ARev: Integer; out ANode: string): Boolean;
begin
  ARev := FReqRev;
  ANode := FReqNode;
  Result := (ARev > 0) and (ANode <> '');
end;

function TBuildInfo.GetCompilerInfo: string;
begin
  {$IFDEF VER185} Result := 'CodeGear' + #153 +' Delphi' + #174 + ' 2007'; {$ENDIF}
  {$IFDEF VER230} Result := 'Embarcadero' + #153 +' Delphi' + #174 + ' XE2'; {$ENDIF}
end;

function TBuildInfo.GetDescription: string;
begin
  Result := 'Windows' + ', ' +
    {$IFDEF WIN32} '32-bit' {$ELSE} '64-bit' {$ENDIF} + ', ' +
    {$IFDEF UNICODE} 'Unicode' {$ELSE} 'Non-Unicode' {$ENDIF}
    {$IFDEF DEBUG} + ', ' + 'Debug'{$ENDIF}
  ;
end;

end.
