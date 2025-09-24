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
  Result :=
    GetVersion + ' ' + FBuildType
    {$IFDEF DEBUG} + ' -= Debug =-' {$ENDIF}
    {$IFDEF WIN64} + ' (x64)'{$ENDIF};
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
  Result := 'Delphi'
    {$if     defined(VER185)} + ' 2007'
    {$elseif defined(VER200)} + ' 2009'
    {$elseif defined(VER210)} + ' 2010'
    {$elseif defined(VER220)} + ' XE'
    {$elseif defined(VER230)} + ' XE2'
    {$elseif defined(VER240)} + ' XE3'
    {$elseif defined(VER250)} + ' XE4'
    {$elseif defined(VER260)} + ' XE5'
    {$elseif defined(VER270)} + ' XE6'
    {$elseif defined(VER280)} + ' XE7'
    {$elseif defined(VER290)} + ' XE8'
    {$elseif defined(VER300)} + ' 10 Seattle'
    {$elseif defined(VER310)} + ' 10.1 Berlin'
    {$elseif defined(VER320)} + ' 10.2 Tokyo'
    {$elseif defined(VER330)} + ' 10.3 Rio'
    {$elseif defined(VER340)} + ' 10.4'
      {$if declared(RTLVersion1042)} + '.2' {$else}
      {$if declared(RTLVersion1041)} + '.1' {$ifend} {$ifend}
                                    + ' Sydney'
    {$elseif defined(VER350)} + ' 11'
      {$if declared(RTLVersion113)} + '.3' {$else}
      {$if declared(RTLVersion112)} + '.2' {$else}
      {$if declared(RTLVersion111)} + '.1' {$ifend} {$ifend} {$ifend}
                                    + ' Alexandria'
    {$elseif defined(VER360)} + ' 12'
      {$if declared(RTLVersion123)} + '.3' {$else}
      {$if declared(RTLVersion122)} + '.2' {$else}
      {$if declared(RTLVersion121)} + '.1' {$ifend} {$ifend} {$ifend}
                                    + ' Athens'
    {$elseif defined(VER370)} + ' 13'
      {$if declared(RTLVersion131)} + '.1' {$ifend}
                                    + ' Florence'
    {$else} {$message hint 'Define your compiler version above!'}
    {$ifend};
end;

function TBuildInfo.GetDescription: string;
begin
  Result := 'Windows'
    {$IFDEF WIN32}     + ', 32-bit'  {$ELSE} + ', 64-bit'      {$ENDIF}
    {$IFDEF UNICODE}   + ', Unicode' {$ELSE} + ', Non-Unicode' {$ENDIF}
    {$IFDEF DEBUG}     + ', Debug'{$ENDIF}
    {$IFDEF EUREKALOG} + ', EL' {$ENDIF}
    {$IFDEF MADEXCEPT} + ', ME' {$ENDIF}
  ;
end;

end.
