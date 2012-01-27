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

unit u_DBMS_provider;

interface

uses
  t_ETS_Provider;

function r_DBMS_Provider_Query_Info(const AProvHandle: TETS_Provider_Handle;
                                    const AProvQueryInfoClass: LongWord;
                                    const AProvQueryInfoSize: LongWord;
                                    const AProvQueryInfoData: Pointer): LongInt; stdcall;

implementation

uses
  t_ETS_Result,
  t_ETS_Tiles,
  t_ETS_AuthKind,
  u_DBMS_Basic,
  u_DBMS_ODBC;

function r_Provider_Open(const AHostProvPtr: Pointer;
                         const APtrProvHandle: PETS_Provider_Handle;
                         const AProvOptions: LongWord): LongInt; stdcall;
begin
  if (nil=APtrProvHandle) or (nil=AHostProvPtr) then begin
    // no pointer(s)
    Result:=ETSR_NO_MANDATORY_PARAMETER;
  end else begin
    //APtrProvHandle^:=nil;
    try
      APtrProvHandle^:=TDBMS_Provider_Basic.Create(AHostProvPtr, AProvOptions);
      with TDBMS_Provider_Basic(APtrProvHandle^) do begin
        DBMS_Environment_Basic_Class:=TDBMS_Environment_ODBC;
        DBMS_Connection_Basic_Class:=TDBMS_Connection_ODBC;
        DBMS_Link_Basic_Class:=TDBMS_Link_ODBC;
        InternalProviderName:='DBMS via ODBC';
      end;
      Result:=ETSR_OK;
    except
      Result:=ETSR_STORAGE_EXCEPTION;
    end;
  end;
end;

function r_Provider_Close(const AProvHandle: TETS_Provider_Handle): LongInt; stdcall;
begin
  try
    if (nil=AProvHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just destroy provider object
      TDBMS_Provider_Basic(AProvHandle).Free;
      Result:=ETSR_OK;
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Provider_Set_Info(const AProvHandle: TETS_Provider_Handle;
                             const AProvSetInfoClass: LongWord;
                             const AProvSetInfoSize: LongWord;
                             const AProvSetInfoData: Pointer): LongInt; stdcall;
begin
  try
    if (nil=AProvHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call provider object
      TDBMS_Provider_Basic(AProvHandle).Set_Info(AProvSetInfoClass, AProvSetInfoSize, AProvSetInfoData);
      Result:=ETSR_OK;
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Link_Open(const AHostLinkPtr: Pointer;
                     const AProvHandle: TETS_Provider_Handle;
                     const APtrLinkHandle: PETS_Link_Handle): LongInt; stdcall;
begin
  try
    if (nil=AProvHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else if (nil=APtrLinkHandle) then begin
      // no pointer
      Result:=ETSR_NO_MANDATORY_PARAMETER
    end else begin
      // just call provider object
      APtrLinkHandle^:=Pointer(TDBMS_Provider_Basic(AProvHandle).Create_Link(AHostLinkPtr));
      Result:=ETSR_OK;
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Link_Close(const ALinkHandle: TETS_Link_Handle): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just destroy link object
      TDBMS_Link_Basic(ALinkHandle).Free;
      Result:=ETSR_OK;
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Link_Flush(const ALinkHandle: TETS_Link_Handle): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Flush_Obj;
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Link_Sync(const ALinkHandle: TETS_Link_Handle;
                     const ASyncPointer: Pointer): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Sync_Obj(ASyncPointer);
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Link_BeginTran(const ALinkHandle: TETS_Link_Handle): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Begin_Tran;
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Link_Commit(const ALinkHandle: TETS_Link_Handle): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Commit_Tran;
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Link_Rollback(const ALinkHandle: TETS_Link_Handle): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Rollback_Tran;
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Link_TranCount(const ALinkHandle: TETS_Link_Handle;
                          const APtrValue: PLongWord): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Tran_Count(APtrValue);
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Link_Set_Info(const ALinkHandle: TETS_Link_Handle;
                         const ALinkSetInfoClass: LongWord;
                         const ALinkSetInfoSize: LongWord;
                         const ALinkSetInfoData: Pointer): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Set_Info(ALinkSetInfoClass,ALinkSetInfoSize,ALinkSetInfoData);
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_Link_Query_Info(const ALinkHandle: TETS_Link_Handle;
                           const ALinkQueryInfoClass: LongWord;
                           const ALinkQueryInfoSize: LongWord;
                           const ALinkQueryInfoData: Pointer): LongInt; stdcall;
begin
  try
    if (nil=ALinkHandle) then begin
      // no pointer
      Result:=ETSR_INVALID_OBJECT_POINTER
    end else begin
      // just call object
      Result:=TDBMS_Link_Basic(ALinkHandle).Query_Info(ALinkQueryInfoClass,ALinkQueryInfoSize,ALinkQueryInfoData);
    end;
  except
    Result:=ETSR_STORAGE_EXCEPTION;
  end;
end;

function r_DBMS_Provider_Query_Info(const AProvHandle: TETS_Provider_Handle;
                                    const AProvQueryInfoClass: LongWord;
                                    const AProvQueryInfoSize: LongWord;
                                    const AProvQueryInfoData: Pointer): LongInt; stdcall;

  procedure _FillZero;
  begin
    FillChar(AProvQueryInfoData^, AProvQueryInfoSize, 0);
  end;
  
begin
  if (ETS_PQIC_PROV_FUNC=AProvQueryInfoClass) then begin
    // return provider functions (not need AProvHandle)
    if (AProvQueryInfoSize>=sizeof(TETS_PQI_PROV_FUNC)) then begin
      // fill struct
      _FillZero;
      with PETS_PQI_PROV_FUNC(AProvQueryInfoData)^ do begin
        p_Provider_Open:=r_Provider_Open;
        p_Provider_Close:=r_Provider_Close;
        p_Provider_Set_Info:=r_Provider_Set_Info;
        p_Link_Open:=r_Link_Open;
        p_Link_Close:=r_Link_Close;
      end;
      Result:=ETSR_OK;
    end else begin
      // size mismatch
      Result:=ETSR_SIZE_MISMATCH;
    end;
  end else begin
    // all other calls requires AProvHandle
    // switch by AProvQueryInfoClass
    if (nil=AProvHandle) then begin
      Result:=ETSR_INVALID_OBJECT_POINTER;
      Exit;
    end else if (ETS_PQIC_PROV_CAPS=AProvQueryInfoClass) then begin
      // return provider capabilities
      if (AProvQueryInfoSize>=sizeof(TETS_PQI_PROV_CAPS)) then begin
        // fill struct
        _FillZero;
        with PETS_PQI_PROV_CAPS(AProvQueryInfoData)^ do begin
          dwSupport:=ETS_SUPPORT_FULL;
          dwAuths:=ETS_MASK_AK_ALL_DBMS;
          dwTileIdFormats:=TILE_ID_FORMAT_XYZ;
        end;
        Result:=ETSR_OK;
      end else begin
        // size mismatch
        Result:=ETSR_SIZE_MISMATCH;
      end;
    end else if (ETS_PQIC_PROTO_VER=AProvQueryInfoClass) then begin
      // get this protocol version (treat buffer as PLongWord only if size=0)
      if (0=AProvQueryInfoSize) then begin
        PLongWord(AProvQueryInfoData)^:=1;
        Result:=ETSR_OK;
      end else begin
        // structure usage not supported in this version
        Result:=ETSR_NOT_SUPPORTED;
      end;
    end else if (ETS_PQIC_LINK_FUNC=AProvQueryInfoClass) then begin
      // return link functions
      if (AProvQueryInfoSize>=sizeof(TETS_PQI_LINK_FUNC)) then begin
        // fill struct
        _FillZero;
        with PETS_PQI_LINK_FUNC(AProvQueryInfoData)^ do begin
          // runtime routines
          p_Link_Flush:=r_Link_Flush;
          p_Link_Sync:=r_Link_Sync;
          // transaction routines
          p_Link_BeginTran:=r_Link_BeginTran;
          p_Link_Commit:=r_Link_Commit;
          p_Link_Rollback:=r_Link_Rollback;
          p_Link_TranCount:=r_Link_TranCount;
          // set and query info
          p_Link_Set_Info:=r_Link_Set_Info;
          p_Link_Query_Info:=r_Link_Query_Info;
        end;
        Result:=ETSR_OK;
      end else begin
        // size mismatch
        Result:=ETSR_SIZE_MISMATCH;
      end;
    end else begin
      // ETS_PQIC_NAME_W/A and others
      Result:=TDBMS_Provider_Basic(AProvHandle).Query_Info(AProvQueryInfoClass,AProvQueryInfoSize,AProvQueryInfoData);
    end;
  end;
end;

end.