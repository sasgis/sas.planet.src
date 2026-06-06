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

unit u_MarkSystemImplORMClientProvider;

interface

{$I MarkSystemORM.inc}

uses
  Windows,
  mormot.core.base,
  mormot.core.unicode,
  mormot.core.os,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.server,
  mormot.orm.sql,
  mormot.orm.mongodb,
  mormot.db.nosql.mongodb,
  {$IFDEF ENABLE_DBMS}
  mormot.db.core,
  mormot.db.sql,
  {$ENDIF ENABLE_DBMS}
  {$IFDEF ENABLE_ZEOS_DBMS}
  mormot.db.sql.zeos,
  {$ENDIF ENABLE_ZEOS_DBMS}
  {$IFDEF ENABLE_ODBC_DBMS}
  mormot.db.sql.odbc,
  {$ENDIF ENABLE_ODBC_DBMS}
  {$IFDEF USE_STATIC_SQLITE3}
  mormot.db.raw.sqlite3.static,
  {$ELSE}
  SyncObjs,
  {$ENDIF USE_STATIC_SQLITE3}
  mormot.db.raw.sqlite3,
  mormot.rest.core,
  mormot.rest.client,
  mormot.rest.server,
  mormot.rest.sqlite3,
  i_MarkSystemImplConfigORM,
  i_MarkSystemImplORMClientProvider,
  t_MarkSystemORM,
  u_MarkSystemORMModel,
  u_BaseInterfacedObject;

type
  TMarkSystemImplORMClientProvider = class(TBaseInterfacedObject, IMarkSystemImplORMClientProvider)
  private type
    TCreateMissingIndexesProc = procedure(const AServer: TRestServerDB);
  private
    FUserName: string;
    FPassword: string;
    FBasePath: string;
    FImplConfig: IMarkSystemImplConfigORM;
    FUserID: TID;
    FModel: TOrmModel;
    FClientDB: TRestClientDB;
    FClientType: TMarkSystemImplORMClientType;
    {$IFDEF ENABLE_DBMS}
    FExternalDB: TSqlDBConnectionProperties;
    {$ENDIF}
    FMongoClient: TMongoClient;
    FInitializeTableOptions: TOrmInitializeTableOptions;
  private
    procedure CreateAllMissing(
      const AOptions: TOrmInitializeTableOptions;
      const ACreateMissingIndexes: TCreateMissingIndexesProc
    );
    procedure Build;
    procedure BuildSQLite3Client;
    {$IFDEF ENABLE_DBMS}
    procedure BuildDBMSClient;
    {$ENDIF}
    procedure BuildMongoDBClient;
    procedure InitUserID;
  private
    { IMarkSystemImplORMClientProvider }
    function GetUserID: TID;
    function GetRestClientType: TMarkSystemImplORMClientType;
    function GetRestClient: TRestClientDB;
  public
    constructor Create(
      const ABasePath: string;
      const AImplConfig: IMarkSystemImplConfigORM;
      const AClientType: TMarkSystemImplORMClientType
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  gnugettext,
  u_GlobalDllName,
  u_FileSystemFunc,
  u_MarkSystemORMTools;

const
  cDefUserName = 'sasgis';
  cDefSQLite3DbFileName = 'Marks.db3';

{$IFNDEF USE_STATIC_SQLITE3}
var
  GSQLite3Lock: TCriticalSection;

procedure _SQLite3Init;
begin
  if not Assigned(mormot.db.raw.sqlite3.sqlite3) then begin
    GSQLite3Lock.Acquire;
    try
      if not Assigned(mormot.db.raw.sqlite3.sqlite3) then begin
        mormot.db.raw.sqlite3.sqlite3 := TSQLite3LibraryDynamic.Create(GDllName.Sqlite3);
      end;
    finally
      GSQLite3Lock.Release;
    end;
  end;
end;
{$ENDIF}

{ TMarkSystemImplORMClientProvider }

constructor TMarkSystemImplORMClientProvider.Create(
  const ABasePath: string;
  const AImplConfig: IMarkSystemImplConfigORM;
  const AClientType: TMarkSystemImplORMClientType
);
begin
  inherited Create;

  FBasePath := ABasePath;
  FClientType := AClientType;
  FImplConfig := AImplConfig;
  FUserName := FImplConfig.UserName;
  FPassword := FImplConfig.PasswordPlain;

  FUserID := 0;
  FInitializeTableOptions := [itoNoIndex4TID];

  {$IFNDEF USE_STATIC_SQLITE3}
  _SQLite3Init;
  {$ENDIF}

  Build;
end;

destructor TMarkSystemImplORMClientProvider.Destroy;
begin
  FreeAndNil(FMongoClient);
  FreeAndNil(FExternalDB);
  FreeAndNil(FClientDB);
  FreeAndNil(FModel);
  inherited Destroy;
end;

procedure TMarkSystemImplORMClientProvider.CreateAllMissing(
  const AOptions: TOrmInitializeTableOptions;
  const ACreateMissingIndexes: TCreateMissingIndexesProc
);
var
  VServer: TRestServerDB;
  VRestOrmServer: TRestOrmServer;
begin
  VServer := FClientDB.Server;
  VRestOrmServer := VServer.OrmInstance as TRestOrmServer;
  VRestOrmServer.CreateMissingTables(0, AOptions);
  ACreateMissingIndexes(VServer);
end;

procedure TMarkSystemImplORMClientProvider.BuildSQLite3Client;

  function GetSQLite3DatabaseFileName(const ABasePath, AFileName: string): string;
  var
    VName, VPath: string;
  begin
    if AFileName <> '' then begin
      VName := ExtractFileName(AFileName);
      if VName = '' then begin
        VName := cDefSQLite3DbFileName;
      end;
      VPath := ExtractFilePath(AFileName);
      if VPath = '' then begin
        VPath := ABasePath;
      end else begin
        if IsRelativePath(VPath) then begin
          VPath := RelativeToAbsolutePath(ABasePath, VPath);
        end;
      end;
    end else begin
      VName := cDefSQLite3DbFileName;
      VPath := ABasePath;
    end;
    Result := IncludeTrailingPathDelimiter(VPath) + VName;
  end;

var
  VFileName: string;
begin
  VFileName := GetSQLite3DatabaseFileName(FBasePath, FImplConfig.FileName);
  if not DirectoryExists(ExtractFileDir(VFileName)) then begin
    raise EMarkSystemORMError.CreateFmt(_('Directory does not exists: %s'), [ExtractFileDir(VFileName)]);
  end;
  if FImplConfig.IsReadOnly and not FileExists(VFileName) then begin
    raise EMarkSystemORMError.CreateFmt(_('File does not exists: %s'), [VFileName]);
  end;
  FModel := CreateModelSQLite3;

  FClientDB := TRestClientDB.Create(FModel, nil, VFileName, TRestServerDB);
  FClientDB.DB.WALMode := True; // for multi-user access
  if not FImplConfig.IsReadOnly then begin
    CreateAllMissing(FInitializeTableOptions, @CreateMissingIndexesSQLite3);
  end;
end;

procedure TMarkSystemImplORMClientProvider.BuildMongoDBClient;
const
  cPrefix = 'mongodb';
  cDefPort = 27017;
var
  I: Integer;
  VDB: RawUtf8;
  VHost: RawUtf8;
  VPort: Integer;
  VText, VTmp: string;
  VUser, VPass: string;
  VServer: TRestOrmServer;
  VTable: TOrmClass;
  VTableName: RawUtf8;
  VDatabase: TMongoDatabase;
  VStorage: TRestStorageMongoDB;
begin
  // 'mongodb://server:port/db'
  // 'mongodb://<user>:<pass>@server:port/db'

  VHost := '';
  VPort := cDefPort;
  VDB := '';
  VUser := FUserName;
  VPass := FPassword;

  VText := FImplConfig.FileName;
  if StartsText(cPrefix+'://', VText) then begin
    VText := StringReplace(VText, cPrefix+'://', '', [rfIgnoreCase]);

    // user/pass
    I := Pos('@', VText);
    if I > 0 then begin
      VTmp := Copy(VText, 1, I-1);
      Delete(VText, 1, I);
      I := Pos(':', VTmp);
      if I > 0 then begin
        VUser := Copy(VTmp, 1, I-1);
        Delete(VTmp, 1, I);
        VPass := VTmp;
        if VPass = '' then begin
          raise EMarkSystemORMError.Create('MongoDB URI: "Pass" param is missing');
        end;
      end else begin
        VUser := VTmp;
      end;
      if VUser = '' then begin
        raise EMarkSystemORMError.Create('MongoDB URI: "User" param is missing');
      end else if FUserName = '' then begin
        FUserName := VUser;
      end;
    end;

    // server/port
    I := Pos('/', VText);
    if I > 0 then begin
      VTmp := Copy(VText, 1, I-1);
      Delete(VText, 1, I);
      I := Pos(':', VTmp);
      if I > 0 then begin
        VHost := StringToUtf8(Copy(VTmp, 1, I-1));
        Delete(VTmp, 1, I);
        VPort := StrToInt(VTmp);
      end else begin
        VHost := StringToUtf8(VTmp);
        VPort := cDefPort;
      end;
    end;

    // db
    VDB := StringToUtf8(VText);
  end else begin
    raise EMarkSystemORMError.Create('MongoDB URI: Prefix is missing');
  end;

  if VHost = '' then begin
    raise EMarkSystemORMError.Create('MongoDB URI: "Server" param is missing');
  end;
  if VDB = '' then begin
    raise EMarkSystemORMError.Create('MongoDB URI: "DB Name" param is missing');
  end;

  FMongoClient := TMongoClient.Create(VHost, VPort);
  if VUser <> '' then begin
    VDatabase := FMongoClient.OpenAuth(VDB, StringToUtf8(VUser), StringToUtf8(VPass));
  end else begin
    VDatabase := FMongoClient.Open(VDB);
  end;

  FModel := CreateModelMongoDB;
  FClientDB := TRestClientDB.Create(FModel, nil, ':memory:', TRestServerDB);
  VServer := FClientDB.Server.OrmInstance as TRestOrmServer;

  for I := 0 to High(FModel.Tables) do begin
    VTable := FModel.Tables[I];
    if VTable.InheritsFrom(TOrmMark) then begin
      VTableName := 'Mark';
    end else begin
      VTableName := '';
    end;
    if OrmMapMongoDB(VTable, VServer, VDatabase, VTableName) = nil then begin
      raise EMarkSystemORMError.Create('OrmMapMongoDB failed');
    end;
  end;

  VServer.InitializeTables(FInitializeTableOptions); // initialize void tables

  if not FImplConfig.IsReadOnly then begin
    CreateAllMissing(FInitializeTableOptions, @CreateMissingIndexesMongoDB);
  end;

  for I := 0 to High(FModel.Tables) do begin
    VStorage := VServer.GetStaticStorage(FModel.Tables[I]) as TRestStorageMongoDB;
    if Assigned(VStorage) then begin
      VStorage.EngineAddCompute := eacLastIDEachTime;
    end;
  end;
end;

{$IFDEF ENABLE_DBMS}
procedure TMarkSystemImplORMClientProvider.BuildDBMSClient;
var
  I, J: Integer;
  VText, VUser: string;
  VConnectionStr: RawUtf8;
  VTable: TOrmClass;
  VTableName: RawUtf8;
  VServer: TRestOrmServer;
  VStorage: TRestStorageExternal;
begin
  FModel := CreateModelDBMS;
  VText := FImplConfig.FileName;
  VConnectionStr := StringToUtf8(VText);
  case FClientType of
    ctODBC: begin
      {$IFDEF ENABLE_ODBC_DBMS}
      // 'Driver=PostgreSQL Unicode;Database=sasgis_marks;Server=localhost;Port=5432;UID=postgres;Pwd=1'
      if StartsText('Driver=', VText) then begin
        I := Pos('uid=', AnsiLowerCase(VText));
        if I > 0 then begin
          if FUserName = '' then begin
            J := I + 4;
            VUser := Copy(VText, J, Length(VText) - (J-1));
            if VUser <> '' then begin
              I := Pos(';', VUser);
              if I > 0 then begin
                VUser := Copy(VUser, 1, I-1);
              end;
              if VUser <> '' then begin
                FUserName := VUser;
              end;
            end;
            Assert((VUser <> ''), 'UID is empty!');
          end;
        end else if FUserName <> '' then begin
          VText := VText + ';UID=' + FUserName;
        end;
        if (FPassword <> '') and not (Pos('pwd=', AnsiLowerCase(VText)) > 0) then begin
          VText := VText + ';Pwd=' + FPassword;
        end;
        VConnectionStr := StringToUtf8(VText);
        FExternalDB := TSqlDbOdbcConnectionProperties.Create('', VConnectionStr, '', '');
      end else begin
        FExternalDB := TSqlDbOdbcConnectionProperties.Create(VConnectionStr, '', '', '');
      end;
      if FImplConfig.ForcedSchemaName <> '' then begin
        FExternalDB.ForcedSchemaName := StringToUtf8(FImplConfig.ForcedSchemaName);
      end;
      {$ELSE}
      raise EMarkSystemORMError.Create('MarkSystemORM: ODBC driver is disabled');
      {$ENDIF}
    end;

    ctZDBC: begin
      {$IFDEF ENABLE_ZEOS_DBMS}
      // [zdbc:]PROTOCOL://HOST:PORT[/DATABASE][?paramname=value]
      // zdbc:postgresql://127.0.0.1:5439/sasgis_marks?username=postgres;password=1
      FExternalDB := TSqlDbZeosConnectionProperties.Create(VConnectionStr, '', '', '');
      {$ELSE}
      raise EMarkSystemORMError.Create('MarkSystemORM: ZDBC driver is disabled');
      {$ENDIF}
    end;
  else
    Assert(False);
  end;

  FExternalDB.UseCache := False;

  for I := 0 to High(FModel.Tables) do begin
    VTable := FModel.Tables[I];
    if VTable.InheritsFrom(TOrmMark) then begin
      VTableName := 'Mark'
    end else begin
      VTableName := '';
    end;

    // http://www.sasgis.org/mantis/view.php?id=2854
    if FExternalDB.Dbms = dMSSQL then begin
      if VTable.SQLTableName = 'User' then begin
        VTableName := 'UserInfo';
      end;
    end;

    if OrmMapExternal(FModel, VTable, FExternalDB, VTableName) = nil then begin
      raise EMarkSystemORMError.Create('OrmMapExternal failed');
    end;
  end;

  FClientDB := TRestClientDB.Create(FModel, nil, ':memory:', TRestServerDB);

  if FExternalDB.Dbms = dMSSQL then begin
    // Some database client libraries may not allow transactions to be shared
    // among several threads - for instance MS SQL
    // http://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html#TITLE_196
    FClientDB.Server.AcquireExecutionMode[execORMWrite] := amBackgroundThread;
    FClientDB.Server.AcquireExecutionMode[execORMGet] := amBackgroundThread;
  end;

  CreateAllMissing(FInitializeTableOptions, @CreateMissingIndexesDBMS);

  VServer := FClientDB.Server.OrmInstance as TRestOrmServer;
  for I := 0 to High(FModel.Tables) do begin
    VTable := FModel.Tables[I];
    VStorage := VServer.GetStaticStorage(VTable) as TRestStorageExternal;
    if Assigned(VStorage) then begin
      VStorage.EngineAddUseSelectMaxID := True;
    end;
  end;
end;
{$ENDIF}

procedure TMarkSystemImplORMClientProvider.InitUserID;
var
  VRestOrm: TRestOrm;
  VOrmUser: TOrmUser;
  VUserName: RawUtf8;
  VTransaction: TTransactionRec;
begin
  if FUserName = '' then begin
    FUserName := cDefUserName;
  end;
  VUserName := StringToUtf8(FUserName);
  VRestOrm := FClientDB.OrmInstance;
  VOrmUser := TOrmUser.Create(VRestOrm, 'uName=?', [VUserName]);
  try
    if VOrmUser.ID = 0 then begin
      if FImplConfig.IsReadOnly then begin
        raise EMarkSystemORMError.Create('MarkSystemORM: Can''t init User in read-only mode!');
      end else begin
        VOrmUser.FName := VUserName;
        StartTransaction(VRestOrm, VTransaction, TOrmUser, FImplConfig.IsReadOnly);
        try
          CheckID( VRestOrm.Add(VOrmUser, True) );
          CommitTransaction(VRestOrm, VTransaction);
        except
          RollBackTransaction(VRestOrm, VTransaction);
          raise;
        end;
      end;
    end;
    FUserID := VOrmUser.ID;
  finally
    VOrmUser.Free;
  end;
end;

procedure TMarkSystemImplORMClientProvider.Build;
begin
  case FClientType of
    ctSQLite3: BuildSQLite3Client;
    ctMongoDB: BuildMongoDBClient;
    {$IFDEF ENABLE_DBMS}
    ctZDBC, ctODBC: BuildDBMSClient;
    {$ENDIF}
  else
    raise EMarkSystemORMError.Create('MarkSystemORM: Unknown Client type!');
  end;
  InitUserID;
end;

function TMarkSystemImplORMClientProvider.GetUserID: TID;
begin
  Result := FUserID;
end;

function TMarkSystemImplORMClientProvider.GetRestClient: TRestClientDB;
begin
  Result := FClientDB;
end;

function TMarkSystemImplORMClientProvider.GetRestClientType: TMarkSystemImplORMClientType;
begin
  Result := FClientType;
end;

{$IFNDEF USE_STATIC_SQLITE3}
initialization
  GSQLite3Lock := TCriticalSection.Create;

finalization
  FreeAndNil(GSQLite3Lock);
{$ENDIF}

end.
