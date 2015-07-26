{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_MarkSystemImplORMClientProvider;

interface

{.$DEFINE ENABLE_ZEOS_DBMS}
{.$DEFINE ENABLE_ODBC_DBMS}

{$IF DEFINED(ENABLE_ZEOS_DBMS) OR DEFINED(ENABLE_ODBC_DBMS)}
  {$DEFINE ENABLE_DBMS}
{$IFEND}

uses
  Windows,
  mORMot,
  mORMotSQLite3,
  mORMotMongoDB,
  {$IFDEF ENABLE_DBMS}
  mORMotDB,
  SynDB,
  {$ENDIF}
  {$IFDEF ENABLE_ZEOS_DBMS}
  SynDBZEOS,
  {$ENDIF}
  {$IFDEF ENABLE_ODBC_DBMS}
  SynDBODBC,
  {$ENDIF}
  SynMongoDB,
  SynSQLite3Static,
  SynCommons,
  i_MarkSystemImplConfig,
  i_MarkSystemImplORMClientProvider,
  t_MarkSystemORM,
  u_MarkSystemORMModel,
  u_BaseInterfacedObject;

type
  TMarkSystemImplORMClientProvider = class(TBaseInterfacedObject, IMarkSystemImplORMClientProvider)
  private
    FBasePath: string;
    FImplConfig: IMarkSystemImplConfigStatic;
    FUserID: TID;
    FModel: TSQLModel;
    FClientDB: TSQLRestClientDB;
    FClientType: TMarkSystemImplORMClientType;
    {$IFDEF ENABLE_DBMS}
    FDBMSProps: TSQLDBConnectionProperties;
    {$ENDIF}
    FMongoClient: TMongoClient;
  private
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
    function GetRestClient: TSQLRestClientDB;
  public
    constructor Create(
      const ABasePath: string;
      const AImplConfig: IMarkSystemImplConfigStatic;
      const AClientType: TMarkSystemImplORMClientType
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  i_MarkSystemImplConfigORM,
  u_FileSystemFunc,
  u_MarkSystemORMTools;

const
  cDefUserName: RawUTF8 = 'sasgis';
  cDefSQLite3DbFileName = 'Marks.db3';

{ TMarkSystemImplORMClientProvider }

constructor TMarkSystemImplORMClientProvider.Create(
  const ABasePath: string;
  const AImplConfig: IMarkSystemImplConfigStatic;
  const AClientType: TMarkSystemImplORMClientType
);
begin
  inherited Create;
  FBasePath := ABasePath;
  FImplConfig := AImplConfig;
  FClientType := AClientType;

  FUserID := 0;
  FModel := nil;
  FClientDB := nil;
  {$IFDEF ENABLE_DBMS}
  FDBMSProps := nil;
  {$ENDIF}
  FMongoClient := nil;

  Build;
end;

destructor TMarkSystemImplORMClientProvider.Destroy;
begin
  if Assigned(FMongoClient) then begin
    FreeAndNil(FMongoClient);
  end;
  {$IFDEF ENABLE_DBMS}
  if Assigned(FDBMSProps) then begin
    FreeAndNil(FDBMSProps);
  end;
  {$ENDIF}
  if Assigned(FClientDB) then begin
    FreeAndNil(FClientDB);
  end;
  if Assigned(FModel) then begin
    FreeAndNil(FModel);
  end;
  inherited Destroy;
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
        VPath := IncludeTrailingPathDelimiter(ABasePath);
      end else begin
        if IsRelativePath(VPath) then begin
          VPath := GetFullPath(ABasePath, VPath);
        end;
      end;
    end else begin
      VName := cDefSQLite3DbFileName;
      VPath := IncludeTrailingPathDelimiter(ABasePath);
    end;
    Result := VPath + VName;
  end;

var
  VFileName: string;
begin
  VFileName := GetSQLite3DatabaseFileName(FBasePath, FImplConfig.FileName);
  FClientDB := TSQLRestClientDB.Create(FModel, nil, VFileName, TSQLRestServerDB);
  FClientDB.DB.WALMode := True; // for multi-user access
  if not FImplConfig.IsReadOnly then begin
    FClientDB.Server.CreateMissingTables;
  end;
end;

procedure TMarkSystemImplORMClientProvider.BuildMongoDBClient;
const
  cPrefix = 'mongodb';
  cDefPort = 27017;
var
  I: Integer;
  VHost: RawUTF8;
  VPort: Integer;
  VDB, VUser, VPass: RawUTF8;
  VDatabase: TMongoDatabase;
  VCollection: TMongoCollection;
  VText, VTmp: string;
begin
  // 'mongodb://server:port/db'
  // 'mongodb://<user>:<pass>@server:port/db'

  VHost := '';
  VPort := cDefPort;
  VDB := '';
  VUser := '';
  VPass := '';

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
        VUser := StringToUTF8(Copy(VTmp, 1, I-1));
        Delete(VTmp, 1, I);
        VPass := StringToUTF8(VTmp);
        if VPass = '' then begin
          raise EMarkSystemORMError.Create('MongoDB URI: "Pass" param is missing');
        end;
      end else begin
        VUser := StringToUTF8(VTmp);
      end;
      if VUser = '' then begin
        raise EMarkSystemORMError.Create('MongoDB URI: "User" param is missing');
      end;
    end;

    // server/port
    I := Pos('/', VText);
    if I > 0 then begin
      VTmp := Copy(VText, 1, I-1);
      Delete(VText, 1, I);
      I := Pos(':', VTmp);
      if I > 0 then begin
        VHost := StringToUTF8(Copy(VTmp, 1, I-1));
        Delete(VTmp, 1, I);
        VPort := StrToInt(VTmp);
      end else begin
        VHost := StringToUTF8(VTmp);
        VPort := cDefPort;
      end;
    end;

    // db
    VDB := VText;
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
    VDatabase := FMongoClient.OpenAuth(VDB, VUser, VPass);
  end else begin
    VDatabase := FMongoClient.Open(VDB);
  end;

  FClientDB := TSQLRestClientDB.Create(FModel, nil, ':memory:', TSQLRestServerDB);
  if not StaticMongoDBRegisterAll(FClientDB.Server, VDatabase) then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: StaticMongoDBRegisterAll failed');
  end;

  if not FImplConfig.IsReadOnly then begin
    FClientDB.Server.CreateMissingTables;

    VCollection :=
      (FClientDB.Server.StaticDataServer[TSQLMark] as TSQLRestStorageMongoDB).Collection;

    VCollection.EnsureIndex(
      _ObjFast(['GeoJsonIdx','2dsphere']),
      _ObjFast(['name','GeoJsonIdx_','2dsphereIndexVersion',2])
    );
  end;
end;

{$IFDEF ENABLE_DBMS}
procedure TMarkSystemImplORMClientProvider.BuildDBMSClient;
var
  VConnectionStr: RawUTF8;
begin
  VConnectionStr := StringToUTF8(FImplConfig.FileName);
  case FClientType of
    ctODBC: begin
      {$IFDEF ENABLE_ODBC_DBMS}
      // 'Driver=PostgreSQL Unicode;Database=sasgis_marks;Server=localhost;Port=5439;UID=postgres;Pwd=1'
      if StartsText('Driver=', FImplConfig.FileName) then begin
        FDBMSProps := TODBCConnectionProperties.Create('', VConnectionStr, '', '');
      end else begin
        FDBMSProps := TODBCConnectionProperties.Create(VConnectionStr, '', '', '');
      end;
      {$ELSE}
      raise EMarkSystemORMError.Create('MarkSystemORM: ODBC driver is disabled');
      {$ENDIF}
    end;

    ctZDBC: begin
      {$IFDEF ENABLE_ZEOS_DBMS}
      // [zdbc:]PROTOCOL://HOST:PORT[/DATABASE][?paramname=value]
      // zdbc:postgresql://127.0.0.1:5439/sasgis_marks?username=postgres;password=1
      FDBMSProps := TSQLDBZEOSConnectionProperties.Create(VConnectionStr, '', '', '');
      {$ELSE}
      raise EMarkSystemORMError.Create('MarkSystemORM: ZDBC driver is disabled');
      {$ENDIF}
    end;
  else
    Assert(False);
  end;

  VirtualTableExternalRegisterAll(FModel, FDBMSProps);

  // map conflict field names
  FModel.Props[TSQLCategoryView].ExternalDB.MapField('User', 'User_').MapAutoKeywordFields;
  FModel.Props[TSQLMarkView].ExternalDB.MapField('User', 'User_').MapAutoKeywordFields;
  FModel.Props[TSQLMark].ExternalDB.MapField('Desc', 'Desc_').MapAutoKeywordFields;
  FModel.Props[TSQLMarkFTS].ExternalDB.MapField('Desc', 'Desc_').MapAutoKeywordFields;
  FModel.Props[TSQLMarkRTree].ExternalDB.MapField('Left', 'Left_').MapAutoKeywordFields;

  FClientDB := TSQLRestClientDB.Create(FModel, nil, ':memory:', TSQLRestServerDB);

  if not FImplConfig.IsReadOnly then begin
    FClientDB.Server.CreateMissingTables;
  end;

  //FClientDB.Server.AcquireExecutionMode[execORMWrite] := amBackgroundThread;
  //FClientDB.Server.AcquireExecutionMode[execORMGet] := amBackgroundThread;
end;
{$ENDIF}

procedure TMarkSystemImplORMClientProvider.InitUserID;
var
  VSQLUser: TSQLUser;
  VUserName: RawUTF8;
  VTransaction: TTransactionRec;
  VImplConfig: IMarkSystemImplConfigORM;
begin
  if not Supports(FImplConfig, IMarkSystemImplConfigORM, VImplConfig) then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: Unknown Impl config interface!');
  end;

  if VImplConfig.UserName = '' then begin
    VUserName := cDefUserName;
  end else begin
    VUserName := StringToUTF8(VImplConfig.UserName);
  end;

  VSQLUser := TSQLUser.Create(FClientDB, 'Name=?', [VUserName]);
  try
    if VSQLUser.ID = 0 then begin
      if VImplConfig.IsReadOnly then begin
        raise EMarkSystemORMError.Create('MarkSystemORM: Can''t init User in read-only mode!');
      end else begin
        VSQLUser.Name := VUserName;
        StartTransaction(FClientDB, VTransaction, TSQLUser);
        try
          CheckID( FClientDB.Add(VSQLUser, True) );
          CommitTransaction(FClientDB, VTransaction);
        except
          RollBackTransaction(FClientDB, VTransaction);
          raise;
        end;
      end;
    end;
    FUserID := VSQLUser.ID;
  finally
    VSQLUser.Free;
  end;
end;

procedure TMarkSystemImplORMClientProvider.Build;
begin
  FModel := CreateModel;
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

function TMarkSystemImplORMClientProvider.GetRestClient: TSQLRestClientDB;
begin
  Result := FClientDB;
end;

function TMarkSystemImplORMClientProvider.GetRestClientType: TMarkSystemImplORMClientType;
begin
  Result := FClientType;
end;

end.
