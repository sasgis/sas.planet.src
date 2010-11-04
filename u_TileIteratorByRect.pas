unit u_TileIteratorByRect;

interface

uses
  Types,
  u_TileIteratorAbstract;

type
  TTileIteratorByRectBase = class(TTileIteratorAbstract)
  protected
    FTilesTotal: Int64;
    FTilesRect: TRect;
    function GetTilesTotal: Int64; override;
    function GetTilesRect: TRect; override;
  public
    constructor Create(ARect: TRect); virtual;
  end;



  TTileIteratorByRect = class(TTileIteratorByRectBase)
  protected
    FEOI: Boolean;
    FCurrent: TPoint;
  public
    constructor Create(ARect: TRect); override;
    function Next(out ATile: TPoint): Boolean; override;
    procedure Reset; override;
  end;

implementation

{ TTileIteratorSpiralByRect }

constructor TTileIteratorByRect.Create(ARect: TRect);
begin
  inherited;
  Reset;
end;

function TTileIteratorByRect.Next(out ATile: TPoint): Boolean;
begin
  Result := False;
  if not FEOI then begin
    Result := True;
    ATile := FCurrent;
    Inc(FCurrent.X);
    if FCurrent.X >= FTilesRect.Right then begin
      FCurrent.X := FTilesRect.Left;
      Inc(FCurrent.Y);
      if FCurrent.Y >= FTilesRect.Bottom then begin
        FEOI := True;
      end;
    end;
  end;
end;

procedure TTileIteratorByRect.Reset;
begin
  if IsRectEmpty(FTilesRect) then begin
    FEOI := True;
    FTilesTotal := 0;
  end else begin
    FEOI := False;
    FTilesTotal := (FTilesRect.Right - FTilesRect.Left);
    FTilesTotal := FTilesTotal * (FTilesRect.Bottom - FTilesRect.Top);
    FCurrent := FTilesRect.TopLeft;
  end;
end;

{ TTileIteratorByRectBase }

constructor TTileIteratorByRectBase.Create(ARect: TRect);
begin
  FTilesRect := ARect;
end;

function TTileIteratorByRectBase.GetTilesRect: TRect;
begin
  Result := FTilesRect;
end;

function TTileIteratorByRectBase.GetTilesTotal: Int64;
begin
  Result := FTilesTotal;
end;

end.
