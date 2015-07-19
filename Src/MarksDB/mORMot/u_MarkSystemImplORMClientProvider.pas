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

uses
  Windows,
  mORMot,
  mORMotDB,
  mORMotSQLite3,
  mORMotMongoDB,
  SynDB,
  {$IFDEF ENABLE_ZEOS_DBMS}
  SynDBZEOS,
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
    FDBMSProps: TSQLDBConnectionPropertiesThreadSafe;
    FMongoClient: TMongoClient;
    FMongoDatabase: TMongoDatabase;
  private
    procedure Build;
    procedure BuildSQLite3Client;
    procedure BuildZeosDBMSClient;
    procedure BuildMongoDBClient;
    procedure InitUserID;
  private
    { IMarkSystemImplORMClientProvider }
    function GetUserID: TID;
    function GetRestClientType: TMarkSystemImplORMClientType;
    function GetRestClient: TSQLRestClientDB;
    function GetMongoDatabase: TMongoDatabase;
    function GetDBMSProps: TSQLDBConnectionPropertiesThreadSafe;
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
  FDBMSProps := nil;
  FMongoClient := nil;
  FMongoDatabase := nil;

  Build;
end;

destructor TMarkSystemImplORMClientProvider.Destroy;
begin
  if Assigned(FMongoDatabase) then begin
    FreeAndNil(FMongoDatabase);
  end;
  if Assigned(FMongoClient) then begin
    FreeAndNil(FMongoClient);
  end;
  if Assigned(FDBMSProps) then begin
    FreeAndNil(FDBMSProps);
  end;
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
end;

procedure TMarkSystemImplORMClientProvider.BuildMongoDBClient;
var
  VHost: RawUTF8;
  VPort: Integer;
  VDB, VUser, VPass: RawUTF8;
begin
  //ToDo: parse params from FImplConfig.FileName
  VHost := 'localhost';
  VPort := 2707;
  VDB := 'sasgis_marks';
  VUser := '';
  VPass := '';

  FMongoClient := TMongoClient.Create(VHost, VPort);
  if VUser <> '' then begin
    FMongoDatabase := FMongoClient.OpenAuth(VDB, VUser, VPass);
  end else begin
    FMongoDatabase := FMongoClient.Open(VDB);
  end;

  FClientDB := TSQLRestClientDB.Create(FModel, nil, ':memory:', TSQLRestServerDB);
  if not StaticMongoDBRegisterAll(FClientDB.Server, FMongoDatabase) then begin
    raise EMarkSystemORMError.Create('MarkSystemORM: StaticMongoDBRegisterAll failed');
  end;
end;

procedure TMarkSystemImplORMClientProvider.BuildZeosDBMSClient;
begin
  {$IFDEF ENABLE_ZEOS_DBMS}
  // zdbc:postgresql://127.0.0.1:5439/sasgis_marks?username=postgres
  FDBMSProps := TSQLDBZEOSConnectionProperties.Create(FImplConfig.FileName, '', '', '');
  VirtualTableExternalRegisterAll(FModel, FDBMSProps);
  // map conflict field names
  FModel.Props[TSQLCategoryView].ExternalDB.MapField('User', 'User_').MapAutoKeywordFields;
  FModel.Props[TSQLMarkView].ExternalDB.MapField('User', 'User_').MapAutoKeywordFields;
  FModel.Props[TSQLMark].ExternalDB.MapField('Desc', 'Desc_').MapAutoKeywordFields;
  FModel.Props[TSQLMarkFTS].ExternalDB.MapField('Desc', 'Desc_').MapAutoKeywordFields;
  FModel.Props[TSQLMarkRTree].ExternalDB.MapField('Left', 'Left_').MapAutoKeywordFields;
  FClientDB := TSQLRestClientDB.Create(FModel, nil, ':memory:', TSQLRestServerDB);
  {$ELSE}
  raise EMarkSystemORMError.Create('MarkSystemORM: ZEOS disabled');
  {$ENDIF}
end;

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
    ctZeosDBMS: BuildZeosDBMSClient;
  else
    raise EMarkSystemORMError.Create('MarkSystemORM: Unknown Client type!');
  end;
  if not FImplConfig.IsReadOnly then begin
    FClientDB.Server.CreateMissingTables;
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

function TMarkSystemImplORMClientProvider.GetMongoDatabase: TMongoDatabase;
begin
  Result := FMongoDatabase;
end;

function TMarkSystemImplORMClientProvider.GetDBMSProps: TSQLDBConnectionPropertiesThreadSafe;
begin
  Result := FDBMSProps;
end;

end.
