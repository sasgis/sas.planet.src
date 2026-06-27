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

unit u_IeEmbeddedProtocol;

interface

uses
  Windows,
  Classes,
  UrlMon,
  i_InternalDomainInfoProvider,
  u_BaseInterfacedObject;

type
  TIeEmbeddedProtocol = class(TBaseInterfacedObject, IInternetProtocolRoot, IInternetProtocol)
  private
    FDomainList: IInternalDomainInfoProviderList;
    FStream: TMemoryStream;

    FProtocolSink: IInternetProtocolSink;
    FBindInfo: IInternetBindInfo;

    function LoadDataToStream(const AUrl: string): Boolean;
  private
    { IInternetProtocolRoot }
    function Start(szUrl: LPCWSTR; OIProtSink: IInternetProtocolSink;
      OIBindInfo: IInternetBindInfo; grfPI, dwReserved: DWORD): HResult; stdcall;
    function Continue(const ProtocolData: TProtocolData): HResult; stdcall;
    function Abort(hrReason: HResult; dwOptions: DWORD): HResult; stdcall;
    function Terminate(dwOptions: DWORD): HResult; stdcall;
    function Suspend: HResult; stdcall;
    function Resume: HResult; stdcall;
  private
    { IInternetProtocol }
    function Read(pv: Pointer; cb: ULONG; out cbRead: ULONG): HResult; stdcall;
    function Seek(dlibMove: LARGE_INTEGER; dwOrigin: DWORD; out libNewPosition: ULARGE_INTEGER): HResult; stdcall;
    function LockRequest(dwOptions: DWORD): HResult; stdcall;
    function UnlockRequest: HResult; stdcall;
  public
    constructor Create(const ADomainList: IInternalDomainInfoProviderList);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_BinaryData,
  u_InternalDomainInfoProviderFunc;

{ TIeEmbeddedProtocol }

constructor TIeEmbeddedProtocol.Create(const ADomainList: IInternalDomainInfoProviderList);
begin
  inherited Create;
  FDomainList := ADomainList;
  FStream := TMemoryStream.Create;
end;

destructor TIeEmbeddedProtocol.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

function TIeEmbeddedProtocol.Abort(hrReason: HResult; dwOptions: DWORD): HResult;
begin
  Result := Inet_E_Invalid_Request;
end;

function TIeEmbeddedProtocol.Continue(const ProtocolData: TProtocolData): HResult;
begin
  Result := Inet_E_Invalid_Request;
end;

function TIeEmbeddedProtocol.LoadDataToStream(const AUrl: string): Boolean;
var
  VDomainName: string;
  VFilePath: string;
  VDomain: IInternalDomainInfoProvider;
  VContentType: AnsiString;
  VData: IBinaryData;
begin
  Result := TInternalDomainInfoProviderFunc.ParseUrl(AUrl, VDomainName, VFilePath);
  if Result then begin
    Result := False;
    try
      if FDomainList <> nil then begin
        VDomain := FDomainList.GetByName(VDomainName);
        if VDomain <> nil then begin
          VData := VDomain.LoadBinaryByFilePath(VFilePath, VContentType);
          FStream.SetSize(0);
          Result := VData <> nil;
          if Result then begin
            FStream.WriteBuffer(VData.Buffer^, VData.Size);
          end;
          FProtocolSink.ReportProgress(BINDSTATUS_MIMETYPEAVAILABLE, PWideChar(UnicodeString(VContentType)));
        end;
        FStream.Position := 0;
      end;
    except
      Result := False;
    end;
  end;
end;

function TIeEmbeddedProtocol.LockRequest(dwOptions: DWORD): HResult;
begin
  Result := S_OK;
end;

function TIeEmbeddedProtocol.Read(pv: Pointer; cb: ULONG; out cbRead: ULONG): HResult;
begin
  if FStream.Position < FStream.Size then begin
    cbRead := FStream.Read(pv^, cb);
    Result := S_OK;
  end else begin
    Result := S_FALSE;
  end;
end;

function TIeEmbeddedProtocol.Resume: HResult;
begin
  Result := Inet_E_Invalid_Request;
end;

function TIeEmbeddedProtocol.Seek(dlibMove: LARGE_INTEGER; dwOrigin: DWORD; out libNewPosition: ULARGE_INTEGER): HResult;
begin
  Result := E_Fail;
end;

function TIeEmbeddedProtocol.Start(szUrl: LPCWSTR; OIProtSink: IInternetProtocolSink; OIBindInfo: IInternetBindInfo;
  grfPI, dwReserved: DWORD): HResult;
begin
  if (szUrl = nil) or (OIProtSink = nil) then begin
    Result := E_INVALIDARG;
    Exit;
  end;

  FProtocolSink := OIProtSink;
  FBindInfo := OIBindInfo;

  if LoadDataToStream(szUrl) then begin
    FProtocolSink.ReportData(BSCF_FIRSTDATANOTIFICATION or BSCF_LASTDATANOTIFICATION or BSCF_DATAFULLYAVAILABLE, FStream.Size, FStream.Size);
    FProtocolSink.ReportResult(S_OK, 200, nil);
    Result := S_OK;
  end else begin
    Result := INET_E_DOWNLOAD_FAILURE;
  end;
end;

function TIeEmbeddedProtocol.Suspend: HResult;
begin
  Result := Inet_E_Invalid_Request;
end;

function TIeEmbeddedProtocol.Terminate(dwOptions: DWORD): HResult;
begin
  FProtocolSink := nil;
  FBindInfo := nil;
  Result := S_OK;
end;

function TIeEmbeddedProtocol.UnlockRequest: HResult;
begin
  Result := S_OK;
end;

end.
