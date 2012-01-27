{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit t_ETS_Result;

interface

const
  // result codes

  // get error code from GetLastError
  // TSR_GETLASTERROR   = LongInt(-1); // use GetLastError to obtain error information

  // Success
  ETSR_OK             = $00000000;

  // Unsupported operation
  ETSR_NOT_SUPPORTED  = $00000001;

  // invalid_tile_id format
  ETSR_INVALID_TILEID_FORMAT = $00000002;

  // storage doesn't support any known tile_id formats
  ETSR_NO_TILEID_FORMATS = $00000003;

  // not all mandatory functions are defined
  ETSR_NO_MANDATORY_FUNCTIONS = $00000004;

  // no root storage function
  ETSR_NO_ROOT_FUNCTION = $00000005;

  // rised an exception (in storage - to EXE)
  ETSR_STORAGE_EXCEPTION      = $00000006;

  // rised an exception (in EXE - to storage)
  ETSR_HOST_EXCEPTION      = $00000007;

  // not all mandatory parameters are defined
  ETSR_NO_MANDATORY_PARAMETER = $00000008;

  // operation aborted by user (manually or terminating application)
  ETSR_ABORTED_BY_USER = $00000009;

  // already connected (if no reinitialization)
  ETSR_ALREADY_CONNECTED = $0000000A;

  // not connected (for tile operations and others)
  ETSR_NOT_CONNECTED = $0000000B;

  // size of buffer less then required
  ETSR_SIZE_MISMATCH  = $0000000C;

  // invalid pointer to object
  ETSR_INVALID_OBJECT_POINTER  = $0000000D;

  // one or more transaction routines is NULL
  ETSR_NULL_TRANSACTION_ROUTINES = $0000000E;

  // not initialized (for connect)
  ETSR_NOT_INITIALIZED = $0000000F;

  // cannot connect to underlaying storage because it's unavailable (no such address etc)
  ETSR_UNDERLAYING_UNAVAILABLE = $00000010;
  
  // cannot connect to underlaying storage because of "access denied" errors
  ETSR_UNDERLAYING_UNATHORIZED = $00000011;

  // cannot connect to underlaying storage because it is restricted by any other reasons
  ETSR_UNDERLAYING_RESTRICTED = $00000012;

  // cannot connect to underlaying storage because it has no structure (empty database or no database)
  ETSR_UNDERLAYING_WITHOUT_STRUCTURE = $00000013;

  // cannot connect to underlaying storage because it has invalid structure (incorrect database of database from another service)
  ETSR_UNDERLAYING_INVALID_STRUCTURE = $00000014;

  // cannot connect to underlaying storage because of any other critical errors
  ETSR_UNDERLAYING_CRITICAL_ERROR = $00000015;

  // error creating environment object
  ETSR_ERROR_CREATING_ENVIRONMENT = $00000016;
  
  // error creating connection object
  ETSR_ERROR_CREATING_CONNECTION = $00000017;


  
  // to be implemented
  ETSR_NOT_IMPLEMENTED  = $10000000;
  
(*
  ETSR_PENDING
  ETSR_ACCESS_DENIED
  ETSR_AUTH_REQUIRED
  ETSR_AUTH_INVALID
*)

implementation

end.