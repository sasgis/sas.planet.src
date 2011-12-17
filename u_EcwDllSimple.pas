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

unit u_EcwDllSimple;

interface

uses
  Windows,
  ECWWriter,
  i_EcwDll;

type
  TEcwDllSimple = class(TInterfacedObject, IEcwDll)
  private
    FDllHandle: HMODULE;
    FDllPath: string;
    FInitialized: Boolean;
    FCompressAllocClient: NCSEcwCompressAllocClient;
    FCompressOpen: NCSEcwCompressOpen;
    FCompress: NCSEcwCompress;
    FCompressClose: NCSEcwCompressClose;
    FCompressFreeClient: NCSEcwCompressFreeClient;
    function Init: Boolean;
  protected
    function GetCompressAllocClient: NCSEcwCompressAllocClient;
    function GetCompressOpen: NCSEcwCompressOpen;
    function GetCompress: NCSEcwCompress;
    function GetCompressClose: NCSEcwCompressClose;
    function GetCompressFreeClient: NCSEcwCompressFreeClient;
  public
    constructor Create(
      ADllPath: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TEcwDllSimple }

constructor TEcwDllSimple.Create(ADllPath: string);
begin
  FDllPath := ADllPath;
  FDllHandle := 0;
  FInitialized := False;
end;

destructor TEcwDllSimple.Destroy;
begin
  if FDllHandle <> 0 then begin
    FreeLibrary(FDllHandle);
    FDllHandle := 0;
  end;
  inherited;
end;

function TEcwDllSimple.Init: Boolean;
const
  CDllName = 'NCSEcwC.dll';
  CCompressAllocClientFunctionName = 'NCSEcwCompressAllocClient';
  CCompressOpenFunctionName = 'NCSEcwCompressOpen';
  CCompressFunctionName = 'NCSEcwCompress';
  CCompressCloseFunctionName = 'NCSEcwCompressClose';
  CCompressFreeClientFunctionName = 'NCSEcwCompressFreeClient';
begin
  Result := FInitialized;
  if not Result then begin
    FDllHandle := LoadLibrary(PChar(FDllPath + CDllName));
    if FDllHandle = 0 then begin
      RaiseLastOSError;
    end;
    FCompressAllocClient := GetProcAddress(FDllHandle, CCompressAllocClientFunctionName);
    if not Assigned(FCompressAllocClient) then begin
      raise Exception.CreateFmt('Function %s not found', [CCompressAllocClientFunctionName]);
    end;
    FCompressOpen := GetProcAddress(FDllHandle, CCompressOpenFunctionName);
    if not Assigned(FCompressOpen) then begin
      raise Exception.CreateFmt('Function %s not found', [CCompressOpenFunctionName]);
    end;
    FCompress := GetProcAddress(FDllHandle, CCompressFunctionName);
    if not Assigned(FCompress) then begin
      raise Exception.CreateFmt('Function %s not found', [CCompressFunctionName]);
    end;
    FCompressClose := GetProcAddress(FDllHandle, CCompressCloseFunctionName);
    if not Assigned(FCompressClose) then begin
      raise Exception.CreateFmt('Function %s not found', [CCompressCloseFunctionName]);
    end;
    FCompressFreeClient := GetProcAddress(FDllHandle, CCompressFreeClientFunctionName);
    if not Assigned(FCompressFreeClient) then begin
      raise Exception.CreateFmt('Function %s not found', [CCompressFreeClientFunctionName]);
    end;
    FInitialized := True;
    Result := FInitialized;
  end;
end;

function TEcwDllSimple.GetCompress: NCSEcwCompress;
begin
  Result := FCompress;
end;

function TEcwDllSimple.GetCompressAllocClient: NCSEcwCompressAllocClient;
begin
  if Init then begin
    Result := FCompressAllocClient;
  end else begin
    Result := nil;
  end;
end;

function TEcwDllSimple.GetCompressClose: NCSEcwCompressClose;
begin
  Result := FCompressClose
end;

function TEcwDllSimple.GetCompressFreeClient: NCSEcwCompressFreeClient;
begin
  Result := FCompressFreeClient
end;

function TEcwDllSimple.GetCompressOpen: NCSEcwCompressOpen;
begin
  Result := FCompressOpen;
end;

end.
