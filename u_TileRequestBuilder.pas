unit u_TileRequestBuilder;

interface

uses
  Windows,
  SyncObjs,
  SysUtils,
  i_TileRequestBuilder,
  i_MapVersionConfig,
  i_LastResponseInfo,
  i_TileRequestBuilderConfig;

type
  TTileRequestBuilder = class(TInterfacedObject, ITileRequestBuilder)
  protected
    FCS: TCriticalSection;
    FConfig: ITileRequestBuilderConfig;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(AConfig: ITileRequestBuilderConfig);
    destructor Destroy; override;
    function  BuildRequestUrl(
      ATileXY: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionConfigStatic
    ): string; virtual; abstract;
    procedure BuildRequest(
      ATileXY: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionConfigStatic;
      ALastResponseInfo: ILastResponseInfo;
      out AUrl: string;
      out ARequestHeader: string
    ); virtual; abstract;
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
