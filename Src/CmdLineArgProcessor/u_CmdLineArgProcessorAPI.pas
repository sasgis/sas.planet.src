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

unit u_CmdLineArgProcessorAPI;

interface

uses
  Messages;

const
  WM_FRIEND_OR_FOE = WM_USER + $FF; // 0x04FF;

const
  cCmdLineArgProcessorOk                      = $00000000;
  cCmdLineArgProcessorLonLatParserError       = $00000001;
  cCmdLineArgProcessorLonLatOutOfBounds       = $00000002;
  cCmdLineArgProcessorZoomParserError         = $00000004;
  cCmdLineArgProcessorZoomOutOfBounds         = $00000008;
  cCmdLineArgProcessorGUIDParserError         = $00000010;
  cCmdLineArgProcessorUnknownGUID             = $00000020;
  cCmdLineArgProcessorShowMarksParserError    = $00000040;
  cCmdLineArgProcessorSASExceptionRaised      = -1;

resourcestring
  rsCmdLineArgProcessorLonLatParserError      = 'LonLatParserError';
  rsCmdLineArgProcessorLonLatOutOfBounds      = 'LonLatOutOfBounds';
  rsCmdLineArgProcessorZoomParserError        = 'ZoomParserError';
  rsCmdLineArgProcessorZoomOutOfBounds        = 'ZoomOutOfBounds';
  rsCmdLineArgProcessorGUIDParserError        = 'GUIDParserError';
  rsCmdLineArgProcessorUnknownGUID            = 'UnknownGUID';
  rsCmdLineArgProcessorShowMarksParserError   = 'ShowMarksParserError';
  rsCmdLineArgProcessorUnknownError           = 'UnknownError: %s';
  rsCmdLineArgProcessorSASExceptionRaised     = 'SASExceptionRaised';

function GetHelp(const AppName: string): string;
function GetErrorFromCode(const ACode: Integer): string;

implementation

uses
  SysUtils;

function GetHelp(const AppName: string): string;
const
  CR = #13#10;
begin
  Result :=
    'Usage:                                                             ' + CR +
    '    ' + AppName + ' [Options] [Arguments]                          ' + CR +
                                                                            CR +
    'Options:                                                           ' + CR +
    '    -h, --help              Print this help message and exit       ' + CR +
    '    --map=<GUID>            Activate the map assigned to <GUID>,   ' + CR +
    '                            or switch ON the layer''s visibility   ' + CR +
    '    --zoom=<z>              Set Zoom to the level <z>              ' + CR +
    '    --move=(<lon>,<lat>)    Center the map with <lon>/<lat>        ' + CR +
    '    --show-placemarks=<v>   Set the placemarks visibility:         ' + CR +
    '                                <v>=0 - visibility is OFF          ' + CR +
    '                                <v>=1 - visibility is ON           ' + CR +
                                                                            CR +
    '    --navigate=(<lon>,<lat>) Start navigating to the location with ' + CR +
    '                             given <lon>/<lat>                     ' + CR +
                                                                            CR +
    '    --insert-placemark="<name>";(<lon>,<lat>);"<desc>"             ' + CR +
    '                            Insert the given placemark into the    ' + CR +
    '                            temporary database                     ' + CR +
    'Arguments:                                                         ' + CR +
    '    filename                One (or more, space-separated) files   ' + CR +
    '                            to be imported into the temporary      ' + CR +
    '                            database                               ' + CR +
    'Examples:                                                          ' + CR +
    '    ' + AppName + ' MyPoints.kml MyTracks.gpx                      ' + CR +
    '    ' + AppName + ' --navigate=(24.56,-32.11)                      ' + CR +
    '    ' + AppName + ' --insert-placemark="My Point";(-44,1.2);"Home" ' + CR +
    '    ' + AppName + ' --show-placemarks=1 --zoom=10 --move=(5,-5)    ' + CR +
    '    ' + AppName + ' --map={F6574B06-E632-4D5F-BC75-C8FA658B57DF}   ' + CR;
end;

function GetErrorFromCode(const ACode: Integer): string;
const
  cSep = ' && ';
  cPrefix = 'CmdLineArgProcessorAPI' + ': ';
var
  VSep: string;
begin
  Result := '';
  VSep := '';

  if ACode = cCmdLineArgProcessorOk then begin
    Exit;
  end;

  if ACode = cCmdLineArgProcessorSASExceptionRaised then begin
    Result := cPrefix + rsCmdLineArgProcessorSASExceptionRaised;
    Exit;
  end;

  if (ACode and cCmdLineArgProcessorLonLatParserError) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorLonLatParserError;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorLonLatOutOfBounds) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorLonLatOutOfBounds;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorZoomParserError) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorZoomParserError;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorZoomOutOfBounds) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorZoomOutOfBounds;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorGUIDParserError) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorGUIDParserError;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorUnknownGUID) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorUnknownGUID;
    VSep := cSep;
  end;

  if (ACode and cCmdLineArgProcessorShowMarksParserError) > 0 then begin
    Result := Result + VSep + rsCmdLineArgProcessorShowMarksParserError;
    VSep := cSep;
  end;

  if Result = '' then begin
    Result := Format(rsCmdLineArgProcessorUnknownError, [IntToHex(ACode, 8)]);
  end;

  Result := cPrefix + Result;
end;

end.
