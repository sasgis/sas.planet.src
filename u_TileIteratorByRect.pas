unit u_TileIteratorByRect;

interface

uses
  Types,
  u_TileIteratorAbstract;

type
  TTileIteratorByRect = class(TTileIteratorAbstract)
  protected
    FEOI: Boolean;
    FCurrent: TPoint;
  public
    constructor Create(ARect: TRect);
    function Next(out ATile: TPoint): Boolean; override;
    procedure Reset;
  end;

implementation

{ TTileIteratorSpiralByRect }

constructor TTileIteratorByRect.Create(ARect: TRect);
begin
  FTilesRect := ARect;
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

end.
