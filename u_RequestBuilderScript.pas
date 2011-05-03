unit u_RequestBuilderScript;

interface

uses
  Windows,
  SyncObjs,
  SysUtils,
  i_RequestBuilderScript,
  i_ConfigDataProvider;

type
  TRequestBuilderScript = class(TInterfacedObject, IRequestBuilderScript)
  protected
    FCS: TCriticalSection;
    FUrlBase: string;
    FDefUrlBase: string;
    FRawRequestHeader: string;
    FDefRawRequestHeader: string;
    procedure Lock;
    procedure Unlock;
    procedure PrepareMainParams(AConfig: IConfigDataProvider);
  public
    constructor Create(AConfig: IConfigDataProvider);
    destructor Destroy; override;
    function  GenRequestUrl(ATileXY: TPoint; AZoom: Byte): string; virtual;
    procedure GenRequest(ATileXY: TPoint; AZoom: Byte; const ARawResponseHeader: string; out AUrl, ARawRequestHeader: string); virtual;
    function  GetUrlBase: string;
    procedure SetUrlBase(AValue: string);
    function  GetDefUrlBase: string;
    function  GetRawRequestHeader: string;
    procedure SetRawRequestHeader(AValue: string);
    function  GetDefRawRequestHeader: string;
    property UrlBase: string read GetUrlBase write SetUrlBase;
    property DefUrlBase: string read GetDefUrlBase;
    property RawRequestHeader: string read GetRawRequestHeader write SetRawRequestHeader;
    property DefRawRequestHeader: string read GetDefRawRequestHeader;
  end;

implementation

{ TRequestBuilderScript }

constructor TRequestBuilderScript.Create(AConfig: IConfigDataProvider);
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  PrepareMainParams(AConfig);
end;

destructor TRequestBuilderScript.Destroy;
begin
  FreeAndNil(FCS);
  inherited Destroy;
end;

procedure TRequestBuilderScript.PrepareMainParams(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FUrlBase := VParams.ReadString('URLBase', '');
  FDefUrlBase := VParams.ReadString('MAIN:URLBase', '');
  FRawRequestHeader := VParams.ReadString('RequestHead', '');
  FRawRequestHeader := StringReplace(FRawRequestHeader, '\r\n', #13#10, [rfIgnoreCase, rfReplaceAll]);
  FDefRawRequestHeader := VParams.ReadString('MAIN:RequestHead', '');
  FDefRawRequestHeader := StringReplace(FDefRawRequestHeader, '\r\n', #13#10, [rfIgnoreCase, rfReplaceAll]);
end;

function TRequestBuilderScript.GenRequestUrl(ATileXY: TPoint; AZoom: Byte): string;
begin
  Result := '';
end;

procedure TRequestBuilderScript.GenRequest(ATileXY: TPoint; AZoom: Byte; const ARawResponseHeader: string; out AUrl, ARawRequestHeader: string);
begin
  AUrl := '';
  ARawRequestHeader := '';
end;

function TRequestBuilderScript.GetUrlBase: string;
begin
  Lock;
  try
    Result := FUrlBase;
  finally
    Unlock;
  end;
end;

procedure TRequestBuilderScript.SetUrlBase(AValue: string);
begin
  Lock;
  try
    FUrlBase := AValue;
  finally
    Unlock;
  end;
end;

function TRequestBuilderScript.GetDefUrlBase: string;
begin
  Lock;
  try
    Result := FDefUrlBase;
  finally
    Unlock;
  end;
end;

function TRequestBuilderScript.GetRawRequestHeader: string;
begin
  Lock;
  try
    Result := FRawRequestHeader;
  finally
    Unlock;
  end;
end;

procedure TRequestBuilderScript.SetRawRequestHeader(AValue: string);
begin
  Lock;
  try
    FRawRequestHeader := AValue;
  finally
    Unlock;
  end;
end;

function TRequestBuilderScript.GetDefRawRequestHeader: string;
begin
  Lock;
  try
    Result := FDefRawRequestHeader;
  finally
    Unlock;
  end;
end;

procedure TRequestBuilderScript.Lock;
begin
  FCS.Acquire;
end;

procedure TRequestBuilderScript.Unlock;
begin
  FCS.Release;
end;

end.
