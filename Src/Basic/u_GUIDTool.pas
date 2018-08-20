{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2018, SAS.Planet development team.                      *}
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

unit u_GUIDTool;

interface

function CompareGUIDs(const G1, G2: TGUID): Integer; inline;

implementation

function CompareGUIDs(const G1, G2: TGUID): Integer; inline;
begin
  if G1.D1 > G2.D1 then begin
    Result := 1;
  end else begin
    if G1.D1 < G2.D1 then begin
      Result := -1;
    end else begin
      if G1.D2 > G2.D2 then begin
        Result := 1;
      end else begin
        if G1.D2 < G2.D2 then begin
          Result := -1;
        end else begin
          if G1.D3 > G2.D3 then begin
            Result := 1;
          end else begin
            if G1.D3 < G2.D3 then begin
              Result := -1;
            end else begin
              if G1.D4[0] > G2.D4[0] then begin
                Result := 1;
              end else begin
                if G1.D4[0] < G2.D4[0] then begin
                  Result := -1;
                end else begin
                  if G1.D4[1] > G2.D4[1] then begin
                    Result := 1;
                  end else begin
                    if G1.D4[1] < G2.D4[1] then begin
                      Result := -1;
                    end else begin
                      if G1.D4[2] > G2.D4[2] then begin
                        Result := 1;
                      end else begin
                        if G1.D4[2] < G2.D4[2] then begin
                          Result := -1;
                        end else begin
                          if G1.D4[3] > G2.D4[3] then begin
                            Result := 1;
                          end else begin
                            if G1.D4[3] < G2.D4[3] then begin
                              Result := -1;
                            end else begin
                              if G1.D4[4] > G2.D4[4] then begin
                                Result := 1;
                              end else begin
                                if G1.D4[4] < G2.D4[4] then begin
                                  Result := -1;
                                end else begin
                                  if G1.D4[5] > G2.D4[5] then begin
                                    Result := 1;
                                  end else begin
                                    if G1.D4[5] < G2.D4[5] then begin
                                      Result := -1;
                                    end else begin
                                      if G1.D4[6] > G2.D4[6] then begin
                                        Result := 1;
                                      end else begin
                                        if G1.D4[6] < G2.D4[6] then begin
                                          Result := -1;
                                        end else begin
                                          if G1.D4[7] > G2.D4[7] then begin
                                            Result := 1;
                                          end else begin
                                            if G1.D4[7] < G2.D4[7] then begin
                                              Result := -1;
                                            end else begin
                                              Result := 0;
                                            end;
                                          end;
                                        end;
                                      end;
                                    end;
                                  end;
                                end;
                              end;
                            end;
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
