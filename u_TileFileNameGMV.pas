unit u_TileFileNameGMV;

interface

uses
  Types,
  i_ITileFileNameGenerator;

type
  TTileFileNameGMV = class(TInterfacedObject, ITileFileNameGenerator)
  public
    function GetTileFileName(AXY: TPoint; Azoom:byte): string;
  end;
implementation

uses
  SysUtils;

{ TTileFileNameGMV }

function TTileFileNameGMV.GetTileFileName(AXY: TPoint;
  Azoom: byte): string;
var
  i: byte;
  VMask: Integer;
  c: Char;
begin
  if (Azoom >= 9) then begin
    Result := IntToStr(Azoom + 1);
  end else begin
    Result := '0' + IntToStr(Azoom + 1);
  end;
  Result := Result + '\t';
  if Azoom > 0 then begin
    VMask := 1 shl (Azoom - 1);
    for i := 1 to Azoom do begin
      if (AXY.X and VMask) = 0 then begin
        if (AXY.Y and VMask) = 0 then begin
          c := 'q';
        end else begin
          c := 't';
        end;
      end else begin
        if (AXY.Y and VMask) = 0 then begin
          c := 'r';
        end else begin
          c := 's';
        end;
      end;
      Result := Result + c;
      VMask := VMask shr 1;
    end;
  end;
end;

end.
