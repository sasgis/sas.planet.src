unit u_TileRequestBuilder;

interface

uses
  Windows,
  SyncObjs,
  SysUtils,
  i_TileRequestBuilder,
  i_MapVersionInfo,
  i_LastResponseInfo,
  i_TileDownloadRequest,
  i_TileRequestBuilderConfig;

type
  TTileRequestBuilder = class(TInterfacedObject, ITileRequestBuilder)
  private
    FCS: TCriticalSection;
  protected
    FConfig: ITileRequestBuilderConfig;
    procedure Lock;
    procedure Unlock;
  protected
    function BuildRequest(
      ATileXY: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionInfo;
      ALastResponseInfo: ILastResponseInfo
    ): ITileDownloadRequest; virtual; abstract;
  public
    constructor Create(AConfig: ITileRequestBuilderConfig);
    destructor Destroy; override;
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

procedure TTileRequestBuilder.Lock;
begin
  FCS.Acquire;
end;

procedure TTileRequestBuilder.Unlock;
begin
  FCS.Release;
end;

end.
