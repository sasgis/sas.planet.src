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
  // передача параметров дл€ криптовани€
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
    FRegistryKey: String;
    
  private
    function GetEncryptionAvailable: Boolean;
    function GetStorageAvailable: Boolean;

    procedure DivideRegistryKey(const full_key_value: String;
                                out real_key: String;
                                out real_value: String);
    
    procedure DoInit;
    procedure DoUninit;
  public
    constructor Create;
    destructor Destroy; override;

    // шифрование
    function Encrypt(const ADescript: LPCWSTR;
                     pIn: PDATA_BLOB;
                     pOut: PDATA_BLOB): BOOL;
                     
    function Decrypt(pDescript: PLPWSTR;
                     pIn: PDATA_BLOB;
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

    // сохранение в реестр и чтение из него
    function ReadFromRegistry(const KeyName: String;
                              pOut: PDATA_BLOB): Boolean;

    function WriteToRegistry(const KeyName: String;
                             const BufferAddress: Pointer;
                             const BufferLength: Integer): Boolean;

    procedure FreeRegistryBuffer(BufferAddress: Pointer);
    
  public
    property EncryptionAvailable: Boolean read GetEncryptionAvailable stored FALSE default FALSE;
    property StorageAvailable: Boolean read GetStorageAvailable stored FALSE default FALSE;
    property RegistryKey: String read FRegistryKey write FRegistryKey;
  end;


// —ледующие функции используют TVsaPStorage, а не наоборот

// сохран€ет и возвращает сохраненный текст
function VsaPStorage_GetTextAsSecretA(AStorage: TVsaPStorage;
                                      const AKeyName: PWideChar;
                                      const ADescript: LPCWSTR;
                                      out ns: NTSTATUS): AnsiString;
function VsaPStorage_SetTextAsSecretA(AStorage: TVsaPStorage;
                                      const AKeyName: PWideChar;
                                      const ADescript: LPCWSTR;
                                      const AText: AnsiString;
                                      out ns: NTSTATUS): Boolean;

// сохран€ет и возвращает сохраненный пароль, использу€ вспомогательный ключ

{$if defined(VSAPSTORAGE_USE_PREDEFINED)}
function VsaPStorage_GetPasswordWithSubKeyAsSecretA(AStorage: TVsaPStorage;
                                                    const ASubKeyName: WideString;
                                                    out ns: NTSTATUS): AnsiString;
{$ifend}

{$if defined(VSAPSTORAGE_USE_PREDEFINED)}
function VsaPStorage_SetPasswordWithSubKeyAsSecretA(AStorage: TVsaPStorage;
                                                    const ASubKeyName: WideString;
                                                    const APassword: AnsiString;
                                                    out ns: NTSTATUS): Boolean;
{$ifend}

// то же дл€ реестра

function VsaPStorage_GetTextInRegistryA(AStorage: TVsaPStorage;
                                        const AKeyName: PWideChar;
                                        const ADescript: LPCWSTR): AnsiString;
function VsaPStorage_SetTextInRegistryA(AStorage: TVsaPStorage;
                                        const AKeyName: PWideChar;
                                        const ADescript: LPCWSTR;
                                        const AText: AnsiString): Boolean;

{$if defined(VSAPSTORAGE_USE_PREDEFINED)}
function VsaPStorage_GetPasswordWithSubKeyInRegistryA(AStorage: TVsaPStorage;
                                                      const ASubKeyName: WideString): AnsiString;
{$ifend}

{$if defined(VSAPSTORAGE_USE_PREDEFINED)}
function VsaPStorage_SetPasswordWithSubKeyInRegistryA(AStorage: TVsaPStorage;
                                                      const ASubKeyName: WideString;
                                                      const APassword: AnsiString): Boolean;
{$ifend}

implementation

uses Registry;

const
  Dll_Crypt32 = 'crypt32.dll';
  Dll_AdvApi32 = 'advapi32.dll';

{$if defined(VSAPSTORAGE_USE_PREDEFINED)}
  // это использовать не об€зательно, просто значени€ по умолчанию дл€ лент€ев
  // на вс€кий случай замен€ть значени€ в каждом новом проекте
  VSAPSTORAGE_DESCRIPT_LSA = '{6A233205-472A-4163-8652-A192074D7DA1}';
  VSAPSTORAGE_DESCRIPT_REG = '{AB7A462A-F997-4C45-827C-E5E55A71451C}';
{$ifend}

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


function VsaPStorage_GetTextInRegistryA(AStorage: TVsaPStorage;
                                        const AKeyName: PWideChar;
                                        const ADescript: LPCWSTR): AnsiString;
var
  crypt_in, crypt_out: DATA_BLOB;
  pDesc: LPWSTR;
  sKeyName: String;
begin
  sKeyName := AKeyName;

  crypt_in.cbData := 0;
  crypt_in.pbData := nil;

  try
    if AStorage.ReadFromRegistry(sKeyName, @crypt_in) then
      begin
        crypt_out.cbData := 0;
        crypt_out.pbData := nil;
        pDesc := nil;
        try
          // ADescript
          if AStorage.Decrypt(@pDesc, @crypt_in, @crypt_out) then
            SetString(Result, PChar(Pointer(crypt_out.pbData)), crypt_out.cbData)
          else
            Result := '';
        finally
          AStorage.FreeDecrypted(Pointer(pDesc));
          AStorage.FreeDecrypted(crypt_out.pbData);
        end;
      end
    else
      Result := '';
  finally
    if crypt_in.pbData <> nil then
      AStorage.FreeRegistryBuffer(crypt_in.pbData);
  end;
end;

function VsaPStorage_SetTextInRegistryA(AStorage: TVsaPStorage;
                                        const AKeyName: PWideChar;
                                        const ADescript: LPCWSTR;
                                        const AText: AnsiString): Boolean;
var
  crypt_in, crypt_out: DATA_BLOB;
  sKeyName: String;
begin
  sKeyName := AKeyName;

  Result := FALSE;
  crypt_in.pbData := Pointer(PChar(AText));
  crypt_in.cbData := Length(AText);
  crypt_out.cbData := 0;
  crypt_out.pbData := nil;

  if AStorage.Encrypt(ADescript, @crypt_in, @crypt_out) then
  try
    if AStorage.WriteToRegistry(sKeyName, crypt_out.pbData, crypt_out.cbData) then
      Result := TRUE;
  finally
    AStorage.FreeDecrypted(crypt_out.pbData);
  end;
end;

{$if defined(VSAPSTORAGE_USE_PREDEFINED)}
function VsaPStorage_GetPasswordWithSubKeyInRegistryA(AStorage: TVsaPStorage;
                                                      const ASubKeyName: WideString): AnsiString;
begin
  Result := VsaPStorage_GetTextInRegistryA(AStorage, @(ASubKeyName[1]), VSAPSTORAGE_DESCRIPT_REG);
end;
{$ifend}

{$if defined(VSAPSTORAGE_USE_PREDEFINED)}
function VsaPStorage_SetPasswordWithSubKeyInRegistryA(AStorage: TVsaPStorage;
                                                      const ASubKeyName: WideString;
                                                      const APassword: AnsiString): Boolean;
begin
  Result := VsaPStorage_SetTextInRegistryA(AStorage, @(ASubKeyName[1]), VSAPSTORAGE_DESCRIPT_REG, APassword);
end;
{$ifend}

function VsaPStorage_GetTextAsSecretA(AStorage: TVsaPStorage;
                                      const AKeyName: PWideChar;
                                      const ADescript: LPCWSTR;
                                      out ns: NTSTATUS): AnsiString;
var
  buf: PLSA_UNICODE_STRING;
  crypt_in, crypt_out: DATA_BLOB;
  pDesc: LPWSTR;
begin
  ns:=0;
  if AStorage.ReadFromSecret(AKeyName, @buf, ns) then
    try
      crypt_in.cbData := buf.Length;
      crypt_in.pbData := PByte(buf.Buffer);

      crypt_out.cbData := 0;
      crypt_out.pbData := nil;
      pDesc := nil;
      try
        // ADescript
        if AStorage.Decrypt(@pDesc, @crypt_in, @crypt_out) then
          SetString(Result, PChar(Pointer(crypt_out.pbData)), crypt_out.cbData)
        else
          Result := '';
      finally
        AStorage.FreeDecrypted(Pointer(pDesc));
        AStorage.FreeDecrypted(crypt_out.pbData);
      end;
    finally
      AStorage.FreeSecretBuffer(buf);
    end
  else
    Result := '';
end;

function VsaPStorage_SetTextAsSecretA(AStorage: TVsaPStorage;
                                      const AKeyName: PWideChar;
                                      const ADescript: LPCWSTR;
                                      const AText: AnsiString;
                                      out ns: NTSTATUS): Boolean;
var
  crypt_in, crypt_out: DATA_BLOB;
begin
  ns := 0;
  Result := FALSE;
  crypt_in.pbData := Pointer(PAnsiChar(AText));
  crypt_in.cbData := Length(AText);
  crypt_out.cbData := 0;
  crypt_out.pbData := nil;

  if AStorage.Encrypt(ADescript, @crypt_in, @crypt_out) then
  try
    if AStorage.WriteToSecret(AKeyName, crypt_out.pbData, crypt_out.cbData, ns) then
      Result := TRUE;
  finally
    AStorage.FreeDecrypted(crypt_out.pbData);
  end;
end;

{$if defined(VSAPSTORAGE_USE_PREDEFINED)}
function VsaPStorage_GetPasswordWithSubKeyAsSecretA(AStorage: TVsaPStorage;
                                                    const ASubKeyName: WideString;
                                                    out ns: NTSTATUS): AnsiString;
begin
  Result := VsaPStorage_GetTextAsSecretA(AStorage, @(ASubKeyName[1]), VSAPSTORAGE_DESCRIPT_LSA, ns);
end;
{$ifend}

{$if defined(VSAPSTORAGE_USE_PREDEFINED)}
function VsaPStorage_SetPasswordWithSubKeyAsSecretA(AStorage: TVsaPStorage;
                                                    const ASubKeyName: WideString;
                                                    const APassword: AnsiString;
                                                    out ns: NTSTATUS): Boolean;
begin
  Result := VsaPStorage_SetTextAsSecretA(AStorage, @(ASubKeyName[1]), VSAPSTORAGE_DESCRIPT_LSA, APassword, ns);
end;
{$ifend}


{ TVsaPStorage }

constructor TVsaPStorage.Create;
begin
  FRegistryKey := '';
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

function TVsaPStorage.Decrypt(pDescript: PLPWSTR;
                              pIn: PDATA_BLOB;
                              pOut: PDATA_BLOB): BOOL;
begin
  Result := EncryptionAvailable;

  if Result then
    Result := Tpfn_CryptUnprotectData(pfn_CryptUnprotectData)(pIn, pDescript, nil, nil, nil, CRYPTPROTECT_UI_FORBIDDEN, pOut);
end;

destructor TVsaPStorage.Destroy;
begin
  DoUninit;
  inherited;
end;

procedure TVsaPStorage.DivideRegistryKey(const full_key_value: String;
                                         out real_key: String;
                                         out real_value: String);
begin
  // если ни одного слэша нет - все кладем в ключ, будет значение по умолчанию
  real_value:='';
  real_key:=full_key_value;

  if System.Pos('\',full_key_value)>0 then
  begin
    // хот€ бы один слэш есть - перетаскиваем с конца символы в параметр
    while (Length(real_key)>0) and (real_key[Length(real_key)]<>'\') do
    begin
      real_value:=real_key[Length(real_key)]+real_value;
      System.Delete(real_key,Length(real_key),1);
    end;

    if (Length(real_key)>0) and (real_key[Length(real_key)]='\') then
      System.Delete(real_key,Length(real_key),1);
  end;
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
                              pOut: PDATA_BLOB): BOOL;
begin
  Result := EncryptionAvailable;

  if Result then
    Result := Tpfn_CryptProtectData(pfn_CryptProtectData)(pIn, ADescript, nil, nil, nil, CRYPTPROTECT_UI_FORBIDDEN, pOut);
end;

procedure TVsaPStorage.FreeDecrypted(Buffer: PByte);
begin
  if Buffer <> nil then
    LocalFree(HLOCAL(Buffer));
end;

procedure TVsaPStorage.FreeRegistryBuffer(BufferAddress: Pointer);
begin
  if BufferAddress <> nil then
    FreeMem(BufferAddress);
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

function TVsaPStorage.ReadFromRegistry(const KeyName: String;
                                       pOut: PDATA_BLOB): Boolean;
var
  r: TRegistry;
  v: TRegDataInfo;
  real_key,real_value: String;
begin
  Result := FALSE;

  if RegistryKey = '' then
    begin
      // раздел реестра не установлен - будем выкусывать из переданного параметра
      DivideRegistryKey(KeyName,real_key,real_value);
    end
  else
    begin
      real_key:=RegistryKey;
      real_value:=KeyName;
    end;

  if (real_key='') then
    Exit;

  if pOut = nil then
    Exit;

  pOut^.cbData := 0;
  pOut^.pbData := nil;

  r := TRegistry.Create(KEY_READ);
  try
    r.RootKey := HKEY_CURRENT_USER;
    if r.OpenKeyReadOnly(real_key) then
    begin
      if r.GetDataInfo(real_value, v) then
      if v.DataSize > 0 then
      try
        pOut^.cbData := v.DataSize;
        GetMem(pOut^.pbData, v.DataSize);

        if r.ReadBinaryData(real_value, pOut^.pbData^, pOut^.cbData) = v.DataSize then
        begin
          // успешно прочитали все данные
          Result := TRUE;
        end;
      except
      end;
    end;
  finally
    r.Free;

    if (not Result) and (pOut^.pbData <> nil) then
    begin
      FreeRegistryBuffer(pOut^.pbData);
      pOut^.pbData := nil;
    end;
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

function TVsaPStorage.WriteToRegistry(const KeyName: String;
                                      const BufferAddress: Pointer;
                                      const BufferLength: Integer): Boolean;
var
  r: TRegistry;
  real_key,real_value: String;
begin
  Result := FALSE;
  
  if RegistryKey = '' then
    begin
      // раздел реестра не установлен - будем выкусывать из переданного параметра
      DivideRegistryKey(KeyName,real_key,real_value);
    end
  else
    begin
      real_key:=RegistryKey;
      real_value:=KeyName;
    end;

  if (real_key='') then
    Exit;

  r := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    r.RootKey := HKEY_CURRENT_USER;
    if r.OpenKey(real_key, TRUE) then
    begin
      // при ошибке будет Exception
      r.WriteBinaryData(real_value, BufferAddress^, BufferLength);
      Result := TRUE;
    end;
  finally
    r.Free;
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
  Result := StorageAvailable;

  if Result then
  begin
    Result := FALSE;

    FillChar(oa, Sizeof(oa), 0);
    oa.Length := Sizeof(oa);

    NtResult := Tpfn_LsaOpenPolicy(pfn_LsaOpenPolicy)(nil,@oa,POLICY_CREATE_SECRET or POLICY_GET_PRIVATE_INFORMATION or STANDARD_RIGHTS_READ, @hPolicy);
    if NtResult >= 0 then
    try
      us_keyname.Length := Length(KeyName) * 2;
      us_keyname.MaximumLength := us_keyname.Length;
      us_keyname.Buffer := KeyName;

      us_keydata.Length := BufferLength;
      us_keydata.MaximumLength := BufferLength;
      us_keydata.Buffer := BufferAddress;

      NtResult := Tpfn_LsaStorePrivateData(pfn_LsaStorePrivateData)(hPolicy, @us_keyname, @us_keydata);
      if NtResult >= 0 then
        Result := TRUE;
    finally
      Tpfn_LsaClose(pfn_LsaClose)(hPolicy);
    end;
  end;
end;

  
end.