unit u_TileRequestBuilder;

interface

uses
  Windows,
  SyncObjs,
  SysUtils,
  i_TileRequestBuilder,
  i_TileRequestBuilderConfig;

type
  TTileRequestBuilder = class(TInterfacedObject, ITileRequestBuilder)
  protected
    FCS: TCriticalSection;
    FConfig: ITileRequestBuilderConfig;
    FLastResponseHead: string;
    function GetResponseHead: string;
    procedure SetResponseHead(const AValue: string);
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(AConfig: ITileRequestBuilderConfig);
    destructor Destroy; override;
    function  BuildRequestUrl(ATileXY: TPoint; AZoom: Byte): string; virtual;
    procedure BuildRequest(
      ATileXY: TPoint;
      AZoom: Byte;
      const APreviousResponseHeader: string;
      out AUrl: string;
      out ARequestHeader: string
    ); virtual;
    property ResponseHead: string read GetResponseHead write SetResponseHead;
  end;

implementation

{ TTileRequestBuilder }

constructor TTileRequestBuilder.Create(AConfig: ITileRequestBuilderConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FCS := TCriticalSection.Create;
end;

destructor TTileRequestBuilder.Destroy;
begin
  FreeAndNil(FCS);
  inherited Destroy;
end;

function TTileRequestBuilder.BuildRequestUrl(ATileXY: TPoint; AZoom: Byte): string;
begin
  Result := '';
end;

procedure TTileRequestBuilder.BuildRequest(
  ATileXY: TPoint;
  AZoom: Byte;
  const APreviousResponseHeader: string;
  out AUrl: string;
  out ARequestHeader: string
);
begin
  AUrl := '';
  ARequestHeader := '';
end;

procedure TTileRequestBuilder.SetResponseHead(const AValue: string);
begin
  Lock;
  try
    FLastResponseHead := AValue;
  finally
    Unlock;
  end;
end;

function TTileRequestBuilder.GetResponseHead: string;
begin
  Lock;
  try
    Result := FLastResponseHead;
  finally
    Unlock;
  end;
end;

procedure TTileRequestBuilder.Lock;
begin
  FCS.Acquire;
end;

procedure TTileRequestBuilder.Unlock;
begin
  FCS.Release;
end;

end.
