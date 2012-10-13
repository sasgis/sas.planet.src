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

unit u_PlayerPlugin;

interface

uses
  SysUtils,
  t_PlayerPlugin,
  i_PlayerPlugin;

type
  TPlayerPlugin = class(TInterfacedObject, IPlayerPlugin)
  private
    //FSync: IReadWriteSync;
    FPlayerDLL: IPlayerDLL;
    FAvailable: Boolean;
  private
    procedure InternalFindPluginDLL(const AFolderNameFromRoot: WideString);
  private
    function Available: Boolean;
    function PlayByDefault(const AFilename: String): IPlayerTask;
    function PlayByDescription(const ADescription: String): IPlayerTask;
    function PlayWithOptions(const AFilename: String; const APlayOptions: LongWord): IPlayerTask;
  public
    constructor Create;
  end;

  TPlayerPluginIni = class(TObject)
  private
    // preloaded DLLs
    FPreloadedDLLs: array of THandle;
  private
    procedure UnloadPreloadedDLLs;
    procedure InternalAddToPreload(const APreloadHandle: THandle);
  public
    constructor Create(
      const AFolderNameFromRoot: WideString;
      const AIniFilename: WideString
    );
    destructor Destroy; override;
  end;

  TPlayerDLL = class(TInterfacedObject, IPlayerDLL)
  private
    FDLLHandle: THandle;
    FPlayerFunc: Pointer;
    FPluginHandle: TPlayerPluginHandle;
    FPlayerPluginIni: TPlayerPluginIni;
  private
    function DLLAvailable: Boolean;
    function DLLFunc: Pointer;
    function DLLHandle: THandle;
    function DLLInitPlugin: Byte;
    function DLLPluginHandlePtr: Pointer;
  public
    constructor Create(
      const ADLLFilename: WideString;
      var APlayerPluginIni: TPlayerPluginIni
    );
    destructor Destroy; override;
  end;

  TPlayerTask = class(TInterfacedObject, IPlayerTask)
  private
    FPlayerDLL: IPlayerDLL;
    FFilename: String;
    FPlayOptions: LongWord;
    FPluginTaskHandle: TTaskPluginHandle;
  private
    FPlayerData: TPlayerPluginData;
    procedure InternalClosePlayer;
  public
    constructor Create(
      const APlayerDLL: IPlayerDLL;
      const AFilename: String;
      const APlayOptions: LongWord
    );
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  Classes,
  //Forms,
  IniFiles,
  //u_Synchronizer,
  i_FileNameIterator,
  u_FileNameIteratorInFolderByMask;

const
  c_PluginSubFolder = 'PlayerPlugin';
  c_PluginFileMask = '*player*.dll';

{ TPlayerPlugin }

function TPlayerPlugin.Available: Boolean;
begin
  Result := FAvailable;
end;

constructor TPlayerPlugin.Create;
begin
  inherited Create;
  //FSync := MakeSyncRW_Std(Self, FALSE);
  FPlayerDLL := nil;
  // try to load plugin from current folder
  //InternalFindPluginDLL('');
  // if failed - try to load plugin from special folder
  //if (nil=FPlayerDLL) then
    InternalFindPluginDLL(c_PluginSubFolder);
  FAvailable := (FPlayerDLL<>nil) and (FPlayerDLL.DLLAvailable);
end;

procedure TPlayerPlugin.InternalFindPluginDLL(const AFolderNameFromRoot: WideString);
var
  VIterator: IFileNameIterator;
  VFileName: WideString;
  VPath: WideString;
  VPluginIni: TPlayerPluginIni;
begin
  VPath := AFolderNameFromRoot;
  if (VPath<>'') then
    VPath := VPath + '\';
  VIterator := TFileNameIteratorInFolderByMask.Create('', AFolderNameFromRoot, c_PluginFileMask, TRUE);
  if (nil<>VIterator) then
  while VIterator.Next(VFileName) do begin
    // check preload options before loading plugin
    if FileExists(VFileName+'.ini') then begin
      VPluginIni := TPlayerPluginIni.Create(AFolderNameFromRoot, VFileName+'.ini');
    end else begin
      VPluginIni := nil;
    end;
    // check VFileName has plugin interface
    try
      FPlayerDLL := TPlayerDLL.Create(VFileName, VPluginIni);
      if (FPlayerDLL<>nil) and (FPlayerDLL.DLLAvailable) then begin
        // allow to use this DLL as plugin
        FPlayerDLL.DLLInitPlugin;
        // exit
        break;
      end;
      //FPlayerDLL := nil;
    finally
      VPluginIni.Free;
    end;
  end;
end;

function TPlayerPlugin.PlayByDefault(const AFilename: String): IPlayerTask;
begin
  Result := PlayWithOptions(AFilename, 0);
end;

function TPlayerPlugin.PlayByDescription(const ADescription: String): IPlayerTask;
var
  VParams: TStringList;
  VVox, VPath: String;
begin
  Result := nil;

  if (not FAvailable) then
    Exit;

  VParams := TStringList.Create;
  try
    // load and parse description
    VVox := StringReplace(ADescription, '<br>', #13#10, [rfReplaceAll,rfIgnoreCase]);
    VParams.NameValueSeparator := ':';
    VParams.QuoteChar := '"';
    VParams.Text := VVox;
    if (0=VParams.Count) then
      Exit;

    // find VOX param
    VVox := Trim(VParams.Values[c_PlayerField_VOX]);
    if FileExists(VVox) then begin
      // done
      Result := PlayByDefault(VVox);
      Exit;
    end;

    // find "IMPORTED FROM" param
    VPath := Trim(VParams.Values[c_PlayerField_IMPORTED_FROM]);
    if (0=Length(VPath)) then
      VPath := Trim(VParams.Values[VParams.QuoteChar+c_PlayerField_IMPORTED_FROM+VParams.QuoteChar]);
    if (0=Length(VPath)) then
      Exit;

    // remove quotes
    if (VPath[1]=VParams.QuoteChar) and (VPath[Length(VPath)]=VParams.QuoteChar) then begin
      VPath := System.Copy(VPath, 2, Length(VPath)-2);
    end;

    // make full path
    VVox := VPath + VVox;

    Result := PlayByDefault(VVox);
  finally
    VParams.Free;
  end;
end;

function TPlayerPlugin.PlayWithOptions(const AFilename: String; const APlayOptions: LongWord): IPlayerTask;
begin
  if (not FAvailable) then begin
    Result := nil;
    Exit;
  end;
  Result := TPlayerTask.Create(FPlayerDLL, AFilename, APlayOptions);
end;

{ TPlayerDLL }

constructor TPlayerDLL.Create(
  const ADLLFilename: WideString;
  var APlayerPluginIni: TPlayerPluginIni
);
begin
  inherited Create;
  FPlayerPluginIni := nil;
  FPluginHandle := nil;
  // try to load library
  FDLLHandle := LoadLibraryW(PWideChar(ADLLFilename));
  // check and init
  if (FDLLHandle<>0) then begin
    // ok
    FPlayerFunc:=GetProcAddress(FDLLHandle, c_PlayerFunc_Name);
    if DLLAvailable then begin
      FPlayerPluginIni := APlayerPluginIni;
      APlayerPluginIni := nil;
    end;
  end else begin
    // failed
    FPlayerFunc:=nil;
  end;
end;

destructor TPlayerDLL.Destroy;
begin
  // destroy plugin
  TPlayerPluginFunc(FPlayerFunc)(DLLPluginHandlePtr, nil, PPM_DESTROY_PLUGIN, nil, nil);

  // unload dll
  if (FDLLHandle <> 0) then begin
    FreeLibrary(FDLLHandle);
    FDLLHandle := 0;
  end;

  FreeAndNil(FPlayerPluginIni);

  inherited;
end;

function TPlayerDLL.DLLAvailable: Boolean;
begin
  Result := (FDLLHandle<>0) and (FPlayerFunc<>nil);
end;

function TPlayerDLL.DLLFunc: Pointer;
begin
  Result := FPlayerFunc;
end;

function TPlayerDLL.DLLHandle: THandle;
begin
  Result := FDLLHandle;
end;

function TPlayerDLL.DLLInitPlugin: Byte;
var
  VSubFolderName: WideString;
begin
  // set main Application.Handle
  Result := TPlayerPluginFunc(FPlayerFunc)(nil, nil, PPM_SET_APP_HANDLE, nil{Pointer(Application.Handle)}, nil);
  if (Result<>PPR_OK) then
    Exit;

  // create handle
  Result := TPlayerPluginFunc(FPlayerFunc)(DLLPluginHandlePtr, nil, PPM_CREATE_PLUGIN, nil, nil);
  if (Result<>PPR_OK) then
    Exit;

  // set SubFolder name
  VSubFolderName := c_PluginSubFolder;
  Result := TPlayerPluginFunc(FPlayerFunc)(DLLPluginHandlePtr, nil, PPM_SET_SUBFOLDER_NAME, PWideChar(VSubFolderName), nil);
end;

function TPlayerDLL.DLLPluginHandlePtr: Pointer;
begin
  Result := @FPluginHandle;
end;

{ TPlayerTask }

constructor TPlayerTask.Create(
  const APlayerDLL: IPlayerDLL;
  const AFilename: String;
  const APlayOptions: LongWord
);
begin
  inherited Create;
  FPluginTaskHandle := nil;
  FPlayerDLL := APlayerDLL;
  FFilename := AFilename;
  FPlayOptions := APlayOptions;
  // prepare data
  FillChar(FPlayerData, sizeof(FPlayerData), 0);
  FPlayerData.wSize := SizeOf(FPlayerData);
  if (SizeOf(Char)=SizeOf(AnsiChar)) then
    FPlayerData.wCallOptions := PPCO_ANSI;
  FPlayerData.iPlayOptions := APlayOptions;
  // call func
  if TPlayerPluginFunc(APlayerDLL.DLLFunc)(APlayerDLL.DLLPluginHandlePtr, @FPluginTaskHandle, PPM_PLAY_START, PChar(AFilename), @FPlayerData)<>PPR_OK then begin
    InternalClosePlayer;
  end;
end;

destructor TPlayerTask.Destroy;
begin
  InternalClosePlayer;
  inherited;
end;

procedure TPlayerTask.InternalClosePlayer;
begin
  if (FPluginTaskHandle<>nil) then begin
    TPlayerPluginFunc(FPlayerDLL.DLLFunc)(FPlayerDLL.DLLPluginHandlePtr, @FPluginTaskHandle, PPM_PLAY_STOP, nil, @FPlayerData);
  end;
end;

{ TPlayerPluginIni }

constructor TPlayerPluginIni.Create(
  const AFolderNameFromRoot: WideString;
  const AIniFilename: WideString
);
var
  VIni: TIniFile;
  VSL: TStrings;
  i: Integer;
  VExists: Boolean;
  VPreloadFile{, VPath}: WideString;
  VPreloadHandle: THandle;
begin
  inherited Create;
  SetLength(FPreloadedDLLs, 0);
  try
    VSL := nil;
    VIni := TIniFile.Create(AIniFilename);
    try
      {
      VPath := AFolderNameFromRoot;
      if (VPath<>'') then
        VPath := VPath + '\';
      }
      VSL := TStringList.Create;
      VIni.ReadSectionValues('PRELOAD', VSL);
      // check if has DLLs to preload
      if VSL.Count>0 then
      for i := 0 to VSL.Count-1 do begin
        VPreloadFile := VSL.ValueFromIndex[i];
        // IF NOT current directory OR fully-qualfied path
        // THEN try to load with path
        VExists := FileExists(VPreloadFile);

        if (not VExists) and (0<Length(AFolderNameFromRoot)) then begin
          VPreloadFile := AFolderNameFromRoot + '\' + VPreloadFile;
          VExists := FileExists(VPreloadFile);
        end;

        if VExists then
        try
          VPreloadHandle := LoadLibraryW(PWideChar(VPreloadFile));
          InternalAddToPreload(VPreloadHandle);
        except
        end;
      end;
    finally
      VIni.Free;
      VSL.Free;
    end;
  except
  end;
end;

destructor TPlayerPluginIni.Destroy;
begin
  UnloadPreloadedDLLs;
  inherited;
end;

procedure TPlayerPluginIni.InternalAddToPreload(const APreloadHandle: THandle);
var k: Integer;
begin
  if (APreloadHandle<>0) then begin
    k := Length(FPreloadedDLLs);
    SetLength(FPreloadedDLLs, (k+1));
    FPreloadedDLLs[k] := APreloadHandle;
  end;
end;

procedure TPlayerPluginIni.UnloadPreloadedDLLs;
var i,k: Integer;
begin
  k := Length(FPreloadedDLLs);
  if (k>0) then
  for i := k-1 downto 0 do
  if (FPreloadedDLLs[i]<>0) then
  try
    FreeLibrary(FPreloadedDLLs[i]);
  finally
    FPreloadedDLLs[i] := 0;
    SetLength(FPreloadedDLLs, i);
  end;
end;

end.