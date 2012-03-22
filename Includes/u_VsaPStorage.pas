{******************************************************************************}
{* LSA Secrets and CryptoAPI interface                                        *}
{* Copyright (C) 2011, Sergey Vasketsov                                       *}
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
{******************************************************************************}

unit u_VsaPStorage;

interface

uses
  Windows;

////{$define VSAPSTORAGE_USE_PREDEFINED}

type
  // передача параметров для криптования
  PDATA_BLOB = ^DATA_BLOB;
  DATA_BLOB = packed record
    cbData: DWORD;
    pbData: PByte;
  end;

  PLPWSTR = ^LPWSTR;
  USHORT = Word;
  PWSTR = PWideChar;
  NTSTATUS = Integer;

  PPLSA_UNICODE_STRING = ^PLSA_UNICODE_STRING;
  PLSA_UNICODE_STRING = ^LSA_UNICODE_STRING;
  LSA_UNICODE_STRING = packed record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: PWSTR;
  end;

  TVsaPStorage = class(TObject)
  private
    bInitialized: Boolean;

    // криптование
    hCrypt32DllHandle: THandle;
    pfn_CryptProtectData: Pointer;
    pfn_CryptUnprotectData: Pointer;

    // хранилище секретов
    hAdvApi32DllHandle: THandle;
    pfn_LsaOpenPolicy: Pointer;
    pfn_LsaStorePrivateData: Pointer;
    pfn_LsaRetrievePrivateData: Pointer;
    pfn_LsaClose: Pointer;
    pfn_LsaFreeMemory: Pointer;
    
  private
    function GetEncryptionAvailable: Boolean;
    function GetStorageAvailable: Boolean;

    procedure DoInit;
    procedure DoUninit;
  public
    constructor Create;
    destructor Destroy; override;

  protected
    // шифрование
    function Encrypt(const ADescript: LPCWSTR;
                     pIn: PDATA_BLOB;
                     pOptionalEntropy: PDATA_BLOB;
                     pOut: PDATA_BLOB): BOOL;
                     
    function Decrypt(pDescript: PLPWSTR;
                     pIn: PDATA_BLOB;
                     pOptionalEntropy: PDATA_BLOB;
                     pOut: PDATA_BLOB): BOOL;

    procedure FreeDecrypted(Buffer: PByte);


    // хранилище секретов
    function ReadFromSecret(const KeyName: PWideChar;
                            BufferAddress: PPLSA_UNICODE_STRING;
                            out NtResult: NTSTATUS): Boolean;

    function WriteToSecret(const KeyName: PWideChar;
                           const BufferAddress: Pointer;
                           const BufferLength: Integer;
                           out NtResult: NTSTATUS): Boolean;

    procedure FreeSecretBuffer(BufferAddress: Pointer);

  public
    function CryptAndSave(const ASecretKey, ASecretValue, AKeyName: WideString;
                          const ADescript: LPCWSTR): Boolean;

    function LoadAndDecrypt(const ASecretKey, AKeyName: WideString): WideString;

    property EncryptionAvailable: Boolean read GetEncryptionAvailable stored FALSE default FALSE;
    property StorageAvailable: Boolean read GetStorageAvailable stored FALSE default FALSE;
  end;

implementation

const
  Dll_Crypt32 = 'crypt32.dll';
  Dll_AdvApi32 = 'advapi32.dll';

  CRYPTPROTECT_UI_FORBIDDEN = 1;

  POLICY_VIEW_LOCAL_INFORMATION     = $00000001;
  POLICY_VIEW_AUDIT_INFORMATION     = $00000002;
  POLICY_GET_PRIVATE_INFORMATION    = $00000004;
  POLICY_TRUST_ADMIN                = $00000008;
  POLICY_CREATE_ACCOUNT             = $00000010;
  POLICY_CREATE_SECRET              = $00000020;
  POLICY_CREATE_PRIVILEGE           = $00000040;
  POLICY_SET_DEFAULT_QUOTA_LIMITS   = $00000080;
  POLICY_SET_AUDIT_REQUIREMENTS     = $00000100;
  POLICY_AUDIT_LOG_ADMIN            = $00000200;
  POLICY_SERVER_ADMIN               = $00000400;
  POLICY_LOOKUP_NAMES               = $00000800;
  POLICY_NOTIFICATION               = $00001000;

  // POLICY_READ                       = (STANDARD_RIGHTS_READ or POLICY_VIEW_AUDIT_INFORMATION or POLICY_GET_PRIVATE_INFORMATION);


type
  PVOID = Pointer;
  HANDLE = THandle;
  LSA_HANDLE = HANDLE;
  PLSA_HANDLE = ^LSA_HANDLE;

  PCRYPTPROTECT_PROMPTSTRUCT = ^CRYPTPROTECT_PROMPTSTRUCT;
  CRYPTPROTECT_PROMPTSTRUCT = packed record
    cbSize: DWORD;
    dwPromptFlags: DWORD;
    hwndApp: HWND;
    szPrompt: LPCWSTR;
  end;

  PLSA_OBJECT_ATTRIBUTES = ^LSA_OBJECT_ATTRIBUTES;
  LSA_OBJECT_ATTRIBUTES = packed record
    Length: ULONG;
    RootDirectory: HANDLE;
    ObjectName: PLSA_UNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: PVOID;
    SecurityQualityOfService: PVOID;
  end;

  Tpfn_CryptProtectData = function(pDataIn: PDATA_BLOB;
                                   szDataDescr: LPCWSTR;
                                   pOptionalEntropy: PDATA_BLOB;
                                   pvReserved: PVOID;
                                   pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT;
                                   dwFlags: DWORD;
                                   pDataOut: PDATA_BLOB): BOOL; stdcall;

  Tpfn_CryptUnprotectData = function(pDataIn: PDATA_BLOB;
                                     ppszDataDescr: PLPWSTR;
                                     pOptionalEntropy: PDATA_BLOB;
                                     pvReserved: PVOID;
                                     pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT;
                                     dwFlags: DWORD;
                                     pDataOut: PDATA_BLOB): BOOL; stdcall;

  Tpfn_LsaOpenPolicy = function(SystemName: PLSA_UNICODE_STRING;
                                ObjectAttributes: PLSA_OBJECT_ATTRIBUTES;
                                DesiredAccess: ACCESS_MASK;
                                PolicyHandle: PLSA_HANDLE): NTSTATUS; stdcall;

  Tpfn_LsaStorePrivateData = function(PolicyHandle: LSA_HANDLE;
                                      KeyName: PLSA_UNICODE_STRING;
                                      PrivateData: PLSA_UNICODE_STRING): NTSTATUS; stdcall;

  Tpfn_LsaClose = function(ObjectHandle: LSA_HANDLE): NTSTATUS; stdcall;

  Tpfn_LsaRetrievePrivateData = function(PolicyHandle: LSA_HANDLE;
                                         KeyName: PLSA_UNICODE_STRING;
                                         PrivateData: PPLSA_UNICODE_STRING): NTSTATUS; stdcall;

  Tpfn_LsaFreeMemory = function(Buffer: PVOID): NTSTATUS; stdcall;

{ TVsaPStorage }

constructor TVsaPStorage.Create;
begin
  bInitialized := FALSE;

  hCrypt32DllHandle := 0;
  hAdvApi32DllHandle := 0;

  pfn_CryptProtectData := nil;
  pfn_CryptUnprotectData := nil;

  pfn_LsaOpenPolicy := nil;
  pfn_LsaStorePrivateData := nil;
  pfn_LsaRetrievePrivateData := nil;
  pfn_LsaClose := nil;
  pfn_LsaFreeMemory := nil;  
end;

function TVsaPStorage.CryptAndSave(const ASecretKey, ASecretValue, AKeyName: WideString;
                                   const ADescript: LPCWSTR): Boolean;
var
  crypt_in, entropy, crypt_out: DATA_BLOB;
  ns: NTSTATUS;
begin
  Result := FALSE;
  crypt_out.cbData := 0;
  crypt_out.pbData := nil;
  try
    crypt_in.pbData := Pointer(PWideChar(ASecretValue));
    crypt_in.cbData := Length(ASecretValue);

    entropy.pbData := Pointer(PWideChar(ASecretKey));
    entropy.cbData := Length(ASecretKey);

    if Encrypt(ADescript, @crypt_in, @entropy, @crypt_out)<>FALSE then
    if WriteToSecret(PWideChar(AKeyName), crypt_out.pbData, crypt_out.cbData, ns) then
      Inc(Result);
  finally
    FreeDecrypted(crypt_out.pbData);
  end;  
end;

function TVsaPStorage.Decrypt(pDescript: PLPWSTR;
                              pIn: PDATA_BLOB;
                              pOptionalEntropy: PDATA_BLOB;
                              pOut: PDATA_BLOB): BOOL;
begin
  Result := EncryptionAvailable;

  if Result then
    Result := Tpfn_CryptUnprotectData(pfn_CryptUnprotectData)(pIn, pDescript, pOptionalEntropy, nil, nil, 0 {CRYPTPROTECT_UI_FORBIDDEN}, pOut);
end;

destructor TVsaPStorage.Destroy;
begin
  DoUninit;
  inherited;
end;

procedure TVsaPStorage.DoInit;
begin
  if bInitialized then
    Exit;

  bInitialized := TRUE;

  hAdvApi32DllHandle := LoadLibrary(Dll_AdvApi32);
  if (hAdvApi32DllHandle <> 0) then
    begin
      pfn_LsaOpenPolicy := GetProcAddress(hAdvApi32DllHandle, 'LsaOpenPolicy');
      pfn_LsaStorePrivateData := GetProcAddress(hAdvApi32DllHandle, 'LsaStorePrivateData');
      pfn_LsaRetrievePrivateData := GetProcAddress(hAdvApi32DllHandle, 'LsaRetrievePrivateData');
      pfn_LsaClose := GetProcAddress(hAdvApi32DllHandle, 'LsaClose');
      pfn_LsaFreeMemory := GetProcAddress(hAdvApi32DllHandle, 'LsaFreeMemory');
    end
  else
    begin
      pfn_LsaOpenPolicy := nil;
      pfn_LsaStorePrivateData := nil;
      pfn_LsaRetrievePrivateData := nil;
      pfn_LsaClose := nil;
      pfn_LsaFreeMemory := nil;
    end;

  hCrypt32DllHandle := LoadLibrary(Dll_Crypt32);
  if (hCrypt32DllHandle <> 0) then
    begin
      pfn_CryptProtectData := GetProcAddress(hCrypt32DllHandle, 'CryptProtectData');
      pfn_CryptUnprotectData := GetProcAddress(hCrypt32DllHandle, 'CryptUnprotectData');
    end
  else
    begin
      pfn_CryptProtectData := nil;
      pfn_CryptUnprotectData := nil;
    end;
end;

procedure TVsaPStorage.DoUninit;
begin
  if not bInitialized then
    Exit;

  bInitialized := FALSE;

  pfn_CryptProtectData := nil;
  pfn_CryptUnprotectData := nil;

  pfn_LsaOpenPolicy := nil;
  pfn_LsaStorePrivateData := nil;
  pfn_LsaRetrievePrivateData := nil;
  pfn_LsaClose := nil;
  
  if hCrypt32DllHandle <> 0 then
  begin
    FreeLibrary(hCrypt32DllHandle);
    hCrypt32DllHandle := 0;
  end;
  
  if hAdvApi32DllHandle <> 0 then
  begin
    FreeLibrary(hAdvApi32DllHandle);
    hAdvApi32DllHandle := 0;
  end;
end;

function TVsaPStorage.Encrypt(const ADescript: LPCWSTR;
                              pIn: PDATA_BLOB;
                              pOptionalEntropy: PDATA_BLOB;
                              pOut: PDATA_BLOB): BOOL;
begin
  Result := EncryptionAvailable;

  if Result then
    Result := Tpfn_CryptProtectData(pfn_CryptProtectData)(pIn, ADescript, pOptionalEntropy, nil, nil, 0{CRYPTPROTECT_UI_FORBIDDEN}, pOut);
end;

procedure TVsaPStorage.FreeDecrypted(Buffer: PByte);
begin
  if Buffer <> nil then
    LocalFree(HLOCAL(Buffer));
end;

procedure TVsaPStorage.FreeSecretBuffer(BufferAddress: Pointer);
begin
  if (BufferAddress <> nil) then
    Tpfn_LsaFreeMemory(pfn_LsaFreeMemory)(BufferAddress);
end;

function TVsaPStorage.GetEncryptionAvailable: Boolean;
begin
  DoInit;

  Result := (hCrypt32DllHandle <> 0) and
            (pfn_CryptProtectData <> nil) and
            (pfn_CryptUnprotectData <> nil);
end;

function TVsaPStorage.GetStorageAvailable: Boolean;
begin
  DoInit;

  Result := (hAdvApi32DllHandle <> 0) and
            (pfn_LsaOpenPolicy <> nil) and
            (pfn_LsaStorePrivateData <> nil) and
            (pfn_LsaRetrievePrivateData <> nil) and
            (pfn_LsaClose <> nil) and
            (pfn_LsaFreeMemory <> nil);
end;

function TVsaPStorage.LoadAndDecrypt(const ASecretKey, AKeyName: WideString): WideString;
var
  ns: NTSTATUS;
  buf: PLSA_UNICODE_STRING;
  crypt_in, entropy, crypt_out: DATA_BLOB;
begin
  Result := '';

  if ReadFromSecret(PWideChar(AKeyName), @buf, ns) then
  try
    crypt_in.cbData := buf.Length;
    crypt_in.pbData := Pointer(buf.Buffer);

    crypt_out.cbData := 0;
    crypt_out.pbData := nil;

    entropy.cbData := Length(ASecretKey);
    entropy.pbData := Pointer(PWideChar(ASecretKey));

    try
      if Decrypt(nil, @crypt_in, @entropy, @crypt_out) then begin
        SetString(Result, PWideChar(Pointer(crypt_out.pbData)), crypt_out.cbData);
      end;
    finally
      FreeDecrypted(crypt_out.pbData);
    end;
  finally
    FreeSecretBuffer(buf);
  end;
end;

function TVsaPStorage.ReadFromSecret(const KeyName: PWideChar;
                                     BufferAddress: PPLSA_UNICODE_STRING;
                                     out NtResult: NTSTATUS): Boolean;
var
  hPolicy: LSA_HANDLE;
  us_keyname: LSA_UNICODE_STRING;
  oa: LSA_OBJECT_ATTRIBUTES;
begin
  Result := StorageAvailable;

  if Result then
  begin
    Result := FALSE;

    FillChar(oa, Sizeof(oa), 0);
    oa.Length := Sizeof(oa);

    NtResult := Tpfn_LsaOpenPolicy(pfn_LsaOpenPolicy)(nil,@oa,POLICY_GET_PRIVATE_INFORMATION or STANDARD_RIGHTS_READ, @hPolicy);
    if NtResult >= 0 then
    try
      us_keyname.Length := Length(KeyName) * 2;
      us_keyname.MaximumLength := us_keyname.Length;
      us_keyname.Buffer := KeyName;

      NtResult := Tpfn_LsaRetrievePrivateData(pfn_LsaRetrievePrivateData)(hPolicy, @us_keyname, BufferAddress);
      if NtResult >= 0 then
        Result := TRUE;
    finally
      Tpfn_LsaClose(pfn_LsaClose)(hPolicy);
    end;
  end;
end;

function TVsaPStorage.WriteToSecret(const KeyName: PWideChar;
                                    const BufferAddress: Pointer;
                                    const BufferLength: Integer;
                                    out NtResult: NTSTATUS): Boolean;
var
  hPolicy: LSA_HANDLE;
  us_keyname: LSA_UNICODE_STRING;
  us_keydata: LSA_UNICODE_STRING;
  oa: LSA_OBJECT_ATTRIBUTES;
begin
  Result := FALSE;

  if StorageAvailable then
  begin
    FillChar(oa, Sizeof(oa), 0);
    oa.Length := Sizeof(oa);

    NtResult := Tpfn_LsaOpenPolicy(pfn_LsaOpenPolicy)(nil,@oa,POLICY_CREATE_SECRET or POLICY_GET_PRIVATE_INFORMATION or STANDARD_RIGHTS_READ, @hPolicy);
    if NtResult >= 0 then
    try
      us_keyname.Length := Length(KeyName) * SizeOf(WideChar);
      us_keyname.MaximumLength := us_keyname.Length;
      us_keyname.Buffer := KeyName;

      us_keydata.Length := BufferLength;
      us_keydata.MaximumLength := BufferLength;
      us_keydata.Buffer := BufferAddress;

      NtResult := Tpfn_LsaStorePrivateData(pfn_LsaStorePrivateData)(hPolicy, @us_keyname, @us_keydata);
      if NtResult >= 0 then
        Inc(Result);
    finally
      Tpfn_LsaClose(pfn_LsaClose)(hPolicy);
    end;
  end;
end;

  
end.