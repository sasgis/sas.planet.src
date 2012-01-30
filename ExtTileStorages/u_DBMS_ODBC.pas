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

unit u_DBMS_ODBC;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  t_ODBC,
  t_ETS_Result,
  t_ETS_AuthKind,
  u_DBMS_Basic;

const
  ODBC32DLL='ODBC32.DLL';

  // constrants for names in database
  c_TABLE_CFG_NAME = '_sas_config_';
  c_TABLE_SVC_ZOOM_DELIMITER = '_'; // between servicename and zoom ("yasat_16")
  c_TABLE_SVC_ZOOM_COLLECTOR = '00'; // use instead of zoom (if zoom<10)

type
  TODBC_Functions = packed record
    // ODBC32DLL
    fSQLAllocHandle: TSQLAllocHandle; // ODBC 3.0
    fSQLSetEnvAttr: TSQLSetEnvAttr; // ODBC 3.0
    fSQLConnectA: TSQLConnectA; // ODBC 1.0
    fSQLConnectW: TSQLConnectW; // ODBC 1.0
    fSQLBindParameter: TSQLBindParameter; // ODBC 2.0
    fSQLExecDirectA: TSQLExecDirectA; // ODBC 1.0
    fSQLExecDirectW: TSQLExecDirectW; // ODBC 1.0
    fSQLPrepareA: TSQLPrepareA; // ODBC 1.0
    fSQLPrepareW: TSQLPrepareW; // ODBC 1.0
    fSQLExecute: TSQLExecute; // ODBC 1.0
    fSQLBindCol: TSQLBindCol; // ODBC 1.0
    fSQLFetch: TSQLFetch; // ODBC 1.0
    fSQLCloseCursor: TSQLCloseCursor; // ODBC 3.0
    fSQLFreeHandle: TSQLFreeHandle; // ODBC 3.0
    fSQLDisconnect: TSQLDisconnect; // ODBC 1.0
    fSQLSetConnectAttrA: TSQLSetConnectAttrA; // ODBC 3.0
    fSQLSetConnectAttrW: TSQLSetConnectAttrW; // ODBC 3.0
  end;
  PODBC_Functions = ^TODBC_Functions;

  TDBMS_Environment_ODBC = class(TDBMS_Environment_Basic)
  private
    henv: SqlHEnv;
    hODBC32DLL: THandle;
    // functions
    FODBC_Functions: TODBC_Functions;
  protected
    function Internal_Initialize: LongInt; override;
    procedure Internal_Uninitialize; override;
    procedure Internal_Zero; override;
  end;

  TDBMS_Connection_ODBC = class(TDBMS_Connection_Basic)
  private
    hdbc: SqlHDbc;
    pFunctions: PODBC_Functions;
  protected
    function Internal_Initialize: LongInt; override;
    procedure Internal_Uninitialize; override;
    procedure Internal_Zero; override;
    function Internal_Connect: LongInt; override;
    procedure Internal_Disconnect; override;
  end;

  TDBMS_Link_ODBC = class(TDBMS_Link_Basic)

  end;

implementation

{ TDBMS_Environment_ODBC }

function TDBMS_Environment_ODBC.Internal_Initialize: LongInt;
begin
  Result:=ETSR_NO_LIB_FUNCTION;
  
  // load main library
  hODBC32DLL:=LoadLibrary(ODBC32DLL);
  if (0=hODBC32DLL) then
    Exit;

  // get function for 3.0 version
  FODBC_Functions.fSQLAllocHandle:=GetProcAddress(hODBC32DLL,'SQLAllocHandle');
  if (not Assigned(FODBC_Functions.fSQLAllocHandle)) then
    Exit;

  FODBC_Functions.fSQLSetEnvAttr:=GetProcAddress(hODBC32DLL,'SQLSetEnvAttr');
  if (not Assigned(FODBC_Functions.fSQLSetEnvAttr)) then
    Exit;

  // ansi and unicode
  FODBC_Functions.fSQLConnectA:=GetProcAddress(hODBC32DLL,'SQLConnect');
  FODBC_Functions.fSQLConnectW:=GetProcAddress(hODBC32DLL,'SQLConnectW');
  if (not Assigned(FODBC_Functions.fSQLConnectA)) and (not Assigned(FODBC_Functions.fSQLConnectW)) then
    Exit;

  FODBC_Functions.fSQLBindParameter:=GetProcAddress(hODBC32DLL,'SQLBindParameter');
  if (not Assigned(FODBC_Functions.fSQLBindParameter)) then
    Exit;

  // ansi and unicode
  FODBC_Functions.fSQLExecDirectA:=GetProcAddress(hODBC32DLL,'SQLExecDirect');
  FODBC_Functions.fSQLExecDirectW:=GetProcAddress(hODBC32DLL,'SQLExecDirectW');
  if (not Assigned(FODBC_Functions.fSQLExecDirectA)) and (not Assigned(FODBC_Functions.fSQLExecDirectW)) then
    Exit;

  // ansi and unicode
  FODBC_Functions.fSQLPrepareA:=GetProcAddress(hODBC32DLL,'SQLPrepare');
  FODBC_Functions.fSQLPrepareW:=GetProcAddress(hODBC32DLL,'SQLPrepareW');
  if (not Assigned(FODBC_Functions.fSQLPrepareA)) and (not Assigned(FODBC_Functions.fSQLPrepareW)) then
    Exit;

  FODBC_Functions.fSQLExecute:=GetProcAddress(hODBC32DLL,'SQLExecute');
  if (not Assigned(FODBC_Functions.fSQLExecute)) then
    Exit;

  FODBC_Functions.fSQLBindCol:=GetProcAddress(hODBC32DLL,'SQLBindCol');
  if (not Assigned(FODBC_Functions.fSQLBindCol)) then
    Exit;

  FODBC_Functions.fSQLFetch:=GetProcAddress(hODBC32DLL,'SQLFetch');
  if (not Assigned(FODBC_Functions.fSQLFetch)) then
    Exit;

  FODBC_Functions.fSQLCloseCursor:=GetProcAddress(hODBC32DLL,'SQLCloseCursor');
  if (not Assigned(FODBC_Functions.fSQLCloseCursor)) then
    Exit;

  FODBC_Functions.fSQLFreeHandle:=GetProcAddress(hODBC32DLL,'SQLFreeHandle');
  if (not Assigned(FODBC_Functions.fSQLFreeHandle)) then
    Exit;

  FODBC_Functions.fSQLDisconnect:=GetProcAddress(hODBC32DLL,'SQLDisconnect');
  if (not Assigned(FODBC_Functions.fSQLDisconnect)) then
    Exit;

  // ansi and unicode
  FODBC_Functions.fSQLSetConnectAttrA:=GetProcAddress(hODBC32DLL,'SQLSetConnectAttr');
  FODBC_Functions.fSQLSetConnectAttrW:=GetProcAddress(hODBC32DLL,'SQLSetConnectAttrW');
  if (not Assigned(FODBC_Functions.fSQLSetConnectAttrA)) and (not Assigned(FODBC_Functions.fSQLSetConnectAttrW)) then
    Exit;

  // allocate environment
  if (SQL_ERROR=FODBC_Functions.fSQLAllocHandle(SQL_HANDLE_ENV, nil, @henv)) then begin
    Result:=ETSR_ERROR_INITIALIZING_LIB;
    Exit;
  end;

  // set version
  if (SQL_ERROR=FODBC_Functions.fSQLSetEnvAttr(henv, SQL_ATTR_ODBC_VERSION, Pointer(SQL_OV_ODBC3), SQL_IS_INTEGER)) then begin
    Result:=ETSR_ERROR_LIB_VERSION;
    Exit;
  end;

  Result:=ETSR_OK;
end;

procedure TDBMS_Environment_ODBC.Internal_Uninitialize;
begin
  if (0<>hODBC32DLL) then begin
    FillChar(FODBC_Functions, sizeof(FODBC_Functions), 0);
    FreeLibrary(hODBC32DLL);
    hODBC32DLL:=0;
  end;
end;

procedure TDBMS_Environment_ODBC.Internal_Zero;
begin
  henv:=nil;
  hODBC32DLL:=0;
  FillChar(FODBC_Functions, sizeof(FODBC_Functions), 0);
end;

{ TDBMS_Connection_ODBC }

function TDBMS_Connection_ODBC.Internal_Connect: LongInt;
var
  pFODBCUserName: Pointer;
  pFODBCAuthentication: Pointer;
  VNameLen2, VNameLen3: SqlSmallint;
  VODBCUserName, VODBCAuthentication, VODBCServerName: AnsiString;
begin
  // connect
  if Assigned(pFunctions^.fSQLConnectW) then begin
    // use unicode version
    if (ETS_AT_INTEGRATED=(FAuthType and ETS_AT_INTEGRATED)) then begin
      // use current user credentials
      pFODBCUserName:=nil;
      pFODBCAuthentication:=nil;
      VNameLen2:=0;
      VNameLen3:=0;
    end else begin
      // use given login and password
      pFODBCUserName:=PWideChar(FAuthLogin);
      pFODBCAuthentication:=PWideChar(FAuthPassword);
      VNameLen2:=SQL_NTS;
      VNameLen3:=SQL_NTS;
    end;

    if (SQL_ERROR=pFunctions^.fSQLConnectW(hdbc,
                                           PWideChar(FServerNameToConnect),
                                           SQL_NTS,
                                           pFODBCUserName,
                                           VNameLen2,
                                           pFODBCAuthentication,
                                           VNameLen3)) then begin
      Result:=ETSR_ERROR_CONNECTING_LIB;
      Exit;
    end;
  end else if Assigned(pFunctions^.fSQLConnectA) then begin
    // use ansi version
    VODBCServerName:=FServerNameToConnect;
    
    if (ETS_AT_INTEGRATED=(FAuthType and ETS_AT_INTEGRATED)) then begin
      // use current user credentials
      pFODBCUserName:=nil;
      pFODBCAuthentication:=nil;
      VNameLen2:=0;
      VNameLen3:=0;
    end else begin
      // use given login and password
      VODBCUserName:=FAuthLogin;
      pFODBCUserName:=PAnsiChar(VODBCUserName);
      VODBCAuthentication:=FAuthPassword;
      pFODBCAuthentication:=PAnsiChar(VODBCAuthentication);
      VNameLen2:=SQL_NTS;
      VNameLen3:=SQL_NTS;
    end;

    if (SQL_ERROR=pFunctions^.fSQLConnectA(hdbc,
                                           PAnsiChar(VODBCServerName),
                                           SQL_NTS,
                                           pFODBCUserName,
                                           VNameLen2,
                                           pFODBCAuthentication,
                                           VNameLen3)) then begin
      Result:=ETSR_ERROR_CONNECTING_LIB;
      Exit;
    end;
  end else begin
    // no functions
    Result:=ETSR_NO_LIB_FUNCTION;
    Exit;
  end;

  Result:=ETSR_OK;
end;

procedure TDBMS_Connection_ODBC.Internal_Disconnect;
begin
  // disconnect from server
  pFunctions^.fSQLDisconnect(hdbc);
end;

function TDBMS_Connection_ODBC.Internal_Initialize: LongInt;
begin
  // allocate connection
  if (SQL_ERROR=pFunctions^.fSQLAllocHandle(SQL_HANDLE_DBC, TDBMS_Environment_ODBC(DBMS_Environment).henv, @hdbc)) then begin
    Result:=ETSR_ERROR_INITIALIZING_LIB;
    Exit;
  end;

  // set connection options
  if Assigned(pFunctions^.fSQLSetConnectAttrW) then begin
    // unicode version

    // SQL_LOGIN_TIMEOUT: set login timeout to 30 seconds
    pFunctions^.fSQLSetConnectAttrW(hdbc, SQL_LOGIN_TIMEOUT, SQLPOINTER(30), 0);

    // SQL_ATTR_CONNECTION_TIMEOUT: set connection timeout to 30 seconds
    pFunctions^.fSQLSetConnectAttrW(hdbc, SQL_ATTR_CONNECTION_TIMEOUT, SQLPOINTER(30), 0);

  end else if Assigned(pFunctions^.fSQLSetConnectAttrA) then begin
    // ansi version
    pFunctions^.fSQLSetConnectAttrA(hdbc, SQL_LOGIN_TIMEOUT, SQLPOINTER(30), 0);
    pFunctions^.fSQLSetConnectAttrA(hdbc, SQL_ATTR_CONNECTION_TIMEOUT, SQLPOINTER(30), 0);
  end;

  // SQL_ATTR_CURRENT_CATALOG

  // SQL_ATTR_PACKET_SIZE

  // SQL_ATTR_QUIET_MODE

  // done
  Result:=ETSR_OK;
end;

procedure TDBMS_Connection_ODBC.Internal_Uninitialize;
begin
  // SQLDisconnect(hdbc);

  if (nil<>hdbc) then begin
    pFunctions^.fSQLFreeHandle(SQL_HANDLE_DBC, hdbc);
    hdbc:=nil;
  end;
end;

procedure TDBMS_Connection_ODBC.Internal_Zero;
begin
  hdbc:=nil;
  pFunctions:=@(TDBMS_Environment_ODBC(DBMS_Environment).FODBC_Functions);
end;

end.