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

unit t_PlayerPlugin;

interface

const
  // mode
  PPM_SET_APP_HANDLE     = $01; // set main Application.Handle
  PPM_CREATE_PLUGIN      = $02;
  PPM_DESTROY_PLUGIN     = $03;

  PPM_SET_SUBFOLDER_NAME = $10; // set SubFolder name

  PPM_PLAY_START         = $20; // start to play file
  PPM_PLAY_STOP          = $21; // stop playing file

  // call options
  PPCO_ANSI = $00000001; // if set - AFile is PAnsiChar, else - PWideChar

  // play options
  PPPO_WAIT_ON_START = $00000001; // wait for playing file (at start)
  PPPO_FREE_ON_STOP  = $00000002; // kill player (otherwise just 'forget' the player and play until finished)


  // results
  PPR_OK = 0;
  // unhandled exception
  PPR_UNHANDLED_EXCEPTION = 1;
  // unknown mode
  PPR_UNKNOWN_MODE = 2;
  // no plugin handle
  PPR_NO_HANDLE = 3;
  // no task handle
  PPR_NO_TASK = 4;
  // no data
  PPR_NO_DATA = 5;
  // no file
  PPR_NO_FILE = 6;
  // no file extension
  PPR_NO_EXT = 7;
  // unknown extension
  PPR_UNKNOWN_EXT = 8;
  // unknown codec
  PPR_UNKNOWN_CODEC = 9;
  // failed to load codec
  PPR_FAILED_CODEC = 10;
  // failed to execute operation
  PPR_FAILED_TO_PLAY = 11;
  // requested file not found
  PPR_FILE_NOT_FOUND = 12;
  // unsupported DLL version
  PPR_UNSUPPORTED_VERSION = 13;


type
  TPlayerPluginHandle = Pointer;
  PPlayerPluginHandle = ^TPlayerPluginHandle;

  TTaskPluginHandle = Pointer;
  PTaskPluginHandle = ^TTaskPluginHandle;

  PPlayerPluginData = ^TPlayerPluginData;
  TPlayerPluginData = packed record
    wSize: SmallInt;
    wCallOptions: SmallInt;
    iPlayOptions: LongWord;
  end;

  // name 'PlayerFunc'
  TPlayerPluginFunc = function(
    const AHandlePtr: PPlayerPluginHandle; // mandatory
    const ATaskPtr: PTaskPluginHandle;
    const AMode: Byte; // mandatory
    const AFile: Pointer; // optional (PAnsiChar or PWideChar or Pointer)
    const AData: PPlayerPluginData // optional (if NIL - AFile is PWideChar)
  ): Byte; stdcall;

const
  // name of the exported plugin function
  c_PlayerFunc_Name = 'PlayerFunc';

  // fields to handle audiolinks
  c_PlayerField_VOX = 'VOX'; // do not modify!
  c_PlayerField_IMPORTED_FROM = 'IMPORTED FROM';

implementation

end.