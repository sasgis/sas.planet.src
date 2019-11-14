{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_WindowsCredentials;

interface

uses
  Windows;

type
  {$IFNDEF UNICODE}
  UnicodeString = WideString;
  NativeUInt = Cardinal;
  {$ENDIF}

  TCredUIInfo = record
    cbSize: DWORD;
    hwndParent: HWND;
    pszMessageText: PWideChar;
    pszCaptionText: PWideChar;
    hbmBanner: HBITMAP;
  end;
  PCredUIInfo = ^TCredUIInfo;

  TCredUIPromptForCredentials = function(pUiInfo: PCredUIInfo;
    pszTargetName: PWideChar; pContext: Pointer; dwAuthError: DWORD;
    pszUserName: PWideChar; ulUserNameBufferSize: ULONG; pszPassword: PWideChar;
    ulPasswordBufferSize: ULONG; var Save: BOOL; dwFlags: DWORD
  ): DWORD; stdcall;

  TCredUIPromptForWindowsCredentials = function(
    var pUiInfo: TCredUIInfo; dwAuthError: DWORD; var pulAuthPackage: NativeUInt;
    pvInAuthBuffer: PCardinal; ulInAuthBufferSize: ULONG;
    out ppvOutAuthBuffer: Cardinal; out pulOutAuthBufferSize: ULONG;
    pfSave: Pointer; dwFlags: DWORD
  ): DWORD; stdcall;

  TCredUnPackAuthenticationBuffer = function(
    dwFlags: DWORD; pAuthBuffer: Pointer; cbAuthBuffer: DWORD;
    pszUserName: LPWSTR; var pcchMaxUserName: DWORD;
    pszDomainName: LPWSTR; var pcchMaxDomainname: DWORD;
    pszPassword: LPWSTR; var pcchMaxPassword: DWORD
  ): LONGBOOL; stdcall;

  TWindowsCredentials = class
  private
    FHandle: THandle;

    FIsInitialized: Boolean;
    FIsCredentialsXPAvailable: Boolean;
    FIsCredentialsVistaAvailable: Boolean;

    FCredUIPromptForCredentials: TCredUIPromptForCredentials;
    FCredUIPromptForWindowsCredentials: TCredUIPromptForWindowsCredentials;
    FCredUnPackAuthenticationBuffer: TCredUnPackAuthenticationBuffer;

    procedure InternalInit;

    procedure InitCredInfo(
      const ACaption, ADescription: string;
      out ACredInfo: TCredUIInfo
    );

    function RequestCredentialsXP(
      const ACaption, ADescription: string;
      var AUser, APassword: string;
      var ASavePassword: Boolean
    ): Boolean;

    function RequestCredentialsVista(
      const ACaption, ADescription: string;
      var AUser, APassword: string;
      var ASavePassword: Boolean
    ): Boolean;
  public
    function RequestCredentials(
      const ACaption, ADescription: string;
      var AUser, APassword: string;
      var ASavePassword: Boolean
    ): Boolean;

    function IsApiAvailable: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  {$IFNDEF CONSOLE}
  Forms,
  {$ENDIF}
  SysUtils;

{ TWindowsCredentials }

constructor TWindowsCredentials.Create;
var
  VMajorVersion, VMinorVersion: Integer;
begin
  inherited Create;

  FHandle := 0;
  FIsInitialized := False;

  VMajorVersion := Win32MajorVersion;
  VMinorVersion := Win32MinorVersion;

  FIsCredentialsXPAvailable := (VMajorVersion >= 5) and (VMinorVersion >= 1);
  FIsCredentialsVistaAvailable := VMajorVersion >= 6;
end;

destructor TWindowsCredentials.Destroy;
begin
  if FHandle <> 0 then begin
    FreeLibrary(FHandle);
  end;
  inherited Destroy;
end;

procedure TWindowsCredentials.InternalInit;
begin
  if FIsInitialized then begin
    Assert(False);
    Exit;
  end;

  FIsInitialized := True;

  if FIsCredentialsXPAvailable or FIsCredentialsVistaAvailable then begin
    FHandle := LoadLibrary('credui.dll');

    if FHandle = 0 then begin
      FIsCredentialsXPAvailable := False;
      FIsCredentialsVistaAvailable := False;
      Exit;
    end;
  end;

  if FIsCredentialsVistaAvailable then begin
    Assert(FHandle <> 0);

    FCredUIPromptForWindowsCredentials := GetProcAddress(FHandle, 'CredUIPromptForWindowsCredentialsW');
    FCredUnPackAuthenticationBuffer := GetProcAddress(FHandle, 'CredUnPackAuthenticationBufferW');

    FIsCredentialsVistaAvailable :=
      Assigned(FCredUIPromptForWindowsCredentials) and
      Assigned(FCredUnPackAuthenticationBuffer);

    FIsCredentialsXPAvailable := not FIsCredentialsVistaAvailable;
  end;

  if FIsCredentialsXPAvailable then begin
    Assert(FHandle <> 0);
    FCredUIPromptForCredentials := GetProcAddress(FHandle, 'CredUIPromptForCredentialsW');
    FIsCredentialsXPAvailable := Assigned(FCredUIPromptForCredentials);
  end;
end;

function TWindowsCredentials.IsApiAvailable: Boolean;
begin
  if not FIsInitialized then begin
    InternalInit;
  end;
  Result := FIsCredentialsXPAvailable or FIsCredentialsVistaAvailable;
end;

function TWindowsCredentials.RequestCredentials(const ACaption,
  ADescription: string; var AUser, APassword: string; var ASavePassword: Boolean
): Boolean;
begin
  if not FIsInitialized then begin
    InternalInit;
  end;
  if FIsCredentialsVistaAvailable then begin
    Result := RequestCredentialsVista(ACaption, ADescription, AUser, APassword, ASavePassword);
  end else
  if FIsCredentialsXPAvailable then begin
    Result := RequestCredentialsXP(ACaption, ADescription, AUser, APassword, ASavePassword);
  end else begin
    raise Exception.Create('Windows Credentials API is not available!');
  end;
end;

procedure TWindowsCredentials.InitCredInfo(
  const ACaption, ADescription: string;
  out ACredInfo: TCredUIInfo
);
begin
  FillChar(ACredInfo, SizeOf(ACredInfo), 0);
  ACredInfo.cbSize := SizeOf(ACredInfo);

  {$IFNDEF CONSOLE}
  if Screen.FocusedForm <> nil then begin
    ACredInfo.hwndParent := Screen.FocusedForm.Handle;
  end else
  if Screen.ActiveForm <> nil then begin
    ACredInfo.hwndParent := Screen.ActiveForm.Handle;
  end;
  {$ENDIF}

  ACredInfo.pszCaptionText := PWideChar(UnicodeString(ACaption));
  ACredInfo.pszMessageText := PWideChar(UnicodeString(ADescription));
end;

function TWindowsCredentials.RequestCredentialsVista(const ACaption,
  ADescription: string; var AUser, APassword: string; var ASavePassword: Boolean
): Boolean;

const
  CREDUIWIN_GENERIC                     = $00000001;
  CREDUIWIN_CHECKBOX                    = $00000002;
  CREDUIWIN_AUTHPACKAGE_ONLY            = $00000010;
  CREDUIWIN_IN_CRED_ONLY                = $00000020;
  CREDUIWIN_ENUMERATE_ADMINS            = $00000100;
  CREDUIWIN_ENUMERATE_CURRENT_USER      = $00000200;
  CREDUIWIN_SECURE_PROMPT               = $00001000;
  CREDUIWIN_PREPROMPTING                = $00002000;
  CREDUIWIN_PACK_32_WOW                 = $10000000;

var
  VCredInfo: TCredUIInfo;
  VUserName, VPassword, VDomain: array [Byte] of WideChar;
  VMaxUserName, VMaxDomainName, VMaxPassword: DWORD;
  VAuthPackage: NativeUInt;
  VOutBuffer: Cardinal;
  VOutBufferSize: DWORD;
  VRetCode: DWORD;
begin
  InitCredInfo(ACaption, ADescription, VCredInfo);

  VAuthPackage := 0;

  VRetCode := FCredUIPromptForWindowsCredentials(VCredInfo, 0, VAuthPackage, nil, 0,
    VOutBuffer, VOutBufferSize, @ASavePassword, CREDUIWIN_GENERIC or CREDUIWIN_CHECKBOX);

  case VRetCode of
    ERROR_SUCCESS: begin
      VMaxUserName := 256;
      VMaxPassword := 256;
      VMaxDomainName := 256;

      Result := FCredUnPackAuthenticationBuffer(0, Pointer(VOutBuffer), VOutBufferSize,
        @VUserName, VMaxUserName, @VDomain, VMaxDomainName, @VPassword, VMaxPassword);

      if Result then begin
        AUser := string(VUserName);
        APassword := string(VPassword);
      end;
    end;
    ERROR_CANCELLED: begin
      Result := False;
    end
  else
    raise Exception.Create('CredUIPromptForWindowsCredentials failed');
  end;
end;

function TWindowsCredentials.RequestCredentialsXP(const ACaption,
  ADescription: string; var AUser, APassword: string; var ASavePassword: Boolean
): Boolean;

const
  CREDUI_FLAGS_INCORRECT_PASSWORD          = $00001;
  CREDUI_FLAGS_DO_NOT_PERSIST              = $00002;
  CREDUI_FLAGS_REQUEST_ADMINISTRATOR       = $00004;
  CREDUI_FLAGS_EXCLUDE_CERTIFICATES        = $00008;
  CREDUI_FLAGS_REQUIRE_CERTIFICATE         = $00010;
  CREDUI_FLAGS_SHOW_SAVE_CHECK_BOX         = $00040;
  CREDUI_FLAGS_ALWAYS_SHOW_UI              = $00080;
  CREDUI_FLAGS_REQUIRE_SMARTCARD           = $00100;
  CREDUI_FLAGS_PASSWORD_ONLY_OK            = $00200;
  CREDUI_FLAGS_VALIDATE_USERNAME           = $00400;
  CREDUI_FLAGS_COMPLETE_USERNAME           = $00800;
  CREDUI_FLAGS_PERSIST                     = $01000;
  CREDUI_FLAGS_SERVER_CREDENTIAL           = $04000;
  CREDUI_FLAGS_EXPECT_CONFIRMATION         = $20000;
  CREDUI_FLAGS_GENERIC_CREDENTIALS         = $40000;
  CREDUI_FLAGS_USERNAME_TARGET_CREDENTIALS = $80000;
  CREDUI_FLAGS_KEEP_USERNAME               = $100000;

const
  MAXBUFLEN = 512;
var
  VCredInfo: TCredUIInfo;
  VUser, VPassword: UnicodeString;
  VSave: BOOL;
  VRetCode: DWORD;
begin
  InitCredInfo(ACaption, ADescription, VCredInfo);

  VUser := AUser + #0;
  VPassword := APassword + #0;
  VSave := ASavePassword;
  SetLength(VUser, MAXBUFLEN);
  SetLength(VPassword, MAXBUFLEN);

  VRetCode := FCredUIPromptForCredentials(@VCredInfo, nil, nil, 0, PWideChar(VUser),
    MAXBUFLEN, PWideChar(VPassword), MAXBUFLEN, VSave, CREDUI_FLAGS_ALWAYS_SHOW_UI or
    CREDUI_FLAGS_DO_NOT_PERSIST or CREDUI_FLAGS_SHOW_SAVE_CHECK_BOX or
    CREDUI_FLAGS_GENERIC_CREDENTIALS);

  case VRetCode of
    NO_ERROR: begin
      AUser := PWideChar(VUser);
      APassword := PWideChar(VPassword);
      ASavePassword := VSave;
      Result := True;
    end;
    ERROR_CANCELLED: begin
      Result := False;
    end;
  else
    raise Exception.Create('CredUIPromptForCredentials failed');
  end;
end;

end.
