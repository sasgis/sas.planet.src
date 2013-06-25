{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_GlobalBerkeleyDBHelper;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  libdb51,
  i_Listener,
  i_PathConfig,
  i_GlobalBerkeleyDBHelper,
  i_BerkeleyDBEnv,
  u_BaseInterfacedObject;

type
  TGlobalBerkeleyDBHelper = class(TBaseInterfacedObject, IGlobalBerkeleyDBHelper)
  private
    FSaveErrorsToLog: Boolean;
    FLogFileStream: TFileStream;
    FDebugLogPath: string;
    FCacheConfigChangeListener: IListener;
    FPathConfig: IPathConfig;
    FEnvList: IInterfaceList;
    FEnvCS: TCriticalSection;
    FLogCS: TCriticalSection;
    function GetFullPathName(const ARelativePathName: string): string;
    procedure SaveErrorToLog(const AMsg: AnsiString);
    procedure OnCacheConfigChange;
  private
    { IGlobalBerkeleyDBHelper }
    function AllocateEnvironment(const AEnvRootPath: string): IBerkeleyDBEnvironment;
    procedure FreeEnvironment(const AEnv: IBerkeleyDBEnvironment);
    procedure RaiseException(const EMsg: AnsiString);
    procedure LogException(const EMsg: AnsiString);
  public
    constructor Create(const APathConfig: IPathConfig);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  ShLwApi,
  u_ListenerByEvent,
  u_BerkeleyDBEnv;

{ TGlobalBerkeleyDBHelper }

constructor TGlobalBerkeleyDBHelper.Create(const APathConfig: IPathConfig);
begin
  inherited Create;
  FPathConfig := APathConfig;
  FEnvList := TInterfaceList.Create;
  FEnvCS := TCriticalSection.Create;
  FLogCS := TCriticalSection.Create;
  FLogFileStream := nil;
  FSaveErrorsToLog := {$IFDEF DEBUG} True {$ELSE} False {$ENDIF};
  FDebugLogPath := FPathConfig.FullPath;
  FCacheConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnCacheConfigChange);
  FPathConfig.ChangeNotifier.Add(FCacheConfigChangeListener);
end;

destructor TGlobalBerkeleyDBHelper.Destroy;
begin
  if (FPathConfig <> nil) and (FCacheConfigChangeListener <> nil) then begin
    FPathConfig.ChangeNotifier.Remove(FCacheConfigChangeListener);
    FPathConfig := nil;
    FCacheConfigChangeListener := nil;
  end;
  FEnvList := nil;
  FreeAndNil(FEnvCS);
  FreeAndNil(FLogFileStream);
  FreeAndNil(FLogCS);
  inherited;
end;

procedure TGlobalBerkeleyDBHelper.OnCacheConfigChange;
begin
  FLogCS.Acquire;
  try
    FDebugLogPath := FPathConfig.FullPath;
  finally
    FLogCS.Release;
  end;
end;

function TGlobalBerkeleyDBHelper.GetFullPathName(const ARelativePathName: string): string;
begin
  SetLength(Result, MAX_PATH);
  PathCombine(@Result[1], PChar(ExtractFilePath(ParamStr(0))), PChar(ARelativePathName));
  SetLength(Result, StrLen(PChar(Result)));
  Result := LowerCase(IncludeTrailingPathDelimiter(Result));
end;

function TGlobalBerkeleyDBHelper.AllocateEnvironment(
  const AEnvRootPath: string
): IBerkeleyDBEnvironment;
var
  I: Integer;
  VPath: string;
  VEnv: IBerkeleyDBEnvironment;
begin
  Result := nil;
  FEnvCS.Acquire;
  try
    VPath := GetFullPathName(AEnvRootPath);
    for I := 0 to FEnvList.Count - 1 do begin
      VEnv := FEnvList.Items[I] as IBerkeleyDBEnvironment;
      if Assigned(VEnv) then begin
        if VEnv.RootPath = VPath then begin
          VEnv.ClientsCount := VEnv.ClientsCount + 1;
          Result := VEnv;
          Break;
        end;
      end;
    end;
    if not Assigned(Result) then begin
      VEnv := TBerkeleyDBEnv.Create(Self, VPath);
      FEnvList.Add(VEnv);
      Result := VEnv;
    end;
  finally
    FEnvCS.Release;
  end;
end;

procedure TGlobalBerkeleyDBHelper.FreeEnvironment(const AEnv: IBerkeleyDBEnvironment);
var
  I: Integer;
  VEnv: IBerkeleyDBEnvironment;
begin
  FEnvCS.Acquire;
  try
    if Assigned(AEnv) then begin
      for I := 0 to FEnvList.Count - 1 do begin
        VEnv := FEnvList.Items[I] as IBerkeleyDBEnvironment;
        if Assigned(VEnv) then begin
          if (VEnv as IInterface) = (AEnv as IInterface) then begin
            VEnv.ClientsCount := VEnv.ClientsCount - 1;
            if VEnv.ClientsCount <= 0 then begin
              FEnvList.Remove(VEnv);
            end;
            Break;
          end;
        end;
      end;
    end;
  finally
    FEnvCS.Release;
  end;
end;

procedure TGlobalBerkeleyDBHelper.SaveErrorToLog(const AMsg: AnsiString);
var
  VLogMsg: AnsiString;
  VLogFileName: string;
  VDateTimeStr: string;
begin
  FLogCS.Acquire;
  try
    if not Assigned(FLogFileStream) then begin
      VLogFileName := IncludeTrailingPathDelimiter(FDebugLogPath) + 'sdb.log';
      if not FileExists(VLogFileName) then begin
        FLogFileStream := TFileStream.Create(VLogFileName, fmCreate);
        FLogFileStream.Free;
      end;
      FLogFileStream := TFileStream.Create(VLogFileName, fmOpenReadWrite or fmShareDenyNone);
    end;

    DateTimeToString(VDateTimeStr, 'dd-mm-yyyy hh:nn:ss.zzzz', Now);
    VLogMsg := AnsiString(VDateTimeStr) + #09 + AMsg + #13#10;

    FLogFileStream.Position := FLogFileStream.Size;
    FLogFileStream.Write(VLogMsg[1], Length(VLogMsg));
  finally
    FLogCS.Release;
  end;
end;

procedure TGlobalBerkeleyDBHelper.RaiseException(const EMsg: AnsiString);
begin
  LogException(EMsg);
  raise EBerkeleyDBExeption.Create(string(EMsg));
end;

procedure TGlobalBerkeleyDBHelper.LogException(const EMsg: AnsiString);
begin
  if FSaveErrorsToLog then
  try
    SaveErrorToLog(EMsg);
  except
    // ignore
  end;
end;

end.
