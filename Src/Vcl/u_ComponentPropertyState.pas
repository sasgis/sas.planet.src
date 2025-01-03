{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ComponentPropertyState;

interface

uses
  Types,
  Classes,
  SysUtils,
  Generics.Collections,
  t_ComponentProperty,
  i_ComponentPropertyState,
  i_ComponentPropertyStorage,
  u_BaseInterfacedObject;

type
  ICustomPropertiesFilterInternal = interface(ICustomPropertiesFilter)
    ['{9BB718FF-26C0-4C17-9E91-7452BAADA3F4}']
    procedure Include(const AComponentName: string; const AProperties: TStringDynArray);
    procedure Exclude(const AComponentName: string; const AProperties: TStringDynArray);
    procedure ExcludeAll(const AComponentName: string);
  end;

  TComponentPropertyState = class(TBaseInterfacedObject, IComponentPropertyState)
  private
    FComponent: TComponent;
    FOptions: TComponentPropertyStateOptions;
    FIgnore: TComponentDynArray;
    FTemporary: TComponentDynArray;
    FStorageCache: TComponentPropertyStorageCache;
    FFilter: ICustomPropertiesFilterInternal;
    FComponentPropertyStorage: IComponentPropertyStorage;
    FWasRestored: Boolean;
    class procedure AddAssigned(
      const ASrc: TComponentDynArray;
      var ADest: TComponentDynArray
    ); static;
  private
    { IComponentPropertyState }
    procedure Save;
    procedure Restore;
    procedure Include(const AComponentName: string; const AProperties: TStringDynArray);
    procedure Exclude(const AComponentName: string; const AProperties: TStringDynArray);
    procedure ExcludeAll(const AComponentName: string);
    function GetOptions: TComponentPropertyStateOptions;
  public
    constructor Create(
      const AComponent: TComponent;
      const AIgnore: TComponentDynArray;
      const ATemporary: TComponentDynArray;
      const AOptions: TComponentPropertyStateOptions
    );
  end;

implementation

uses
  u_GlobalState;

type
  TCustomPropertiesFilter = class(TBaseInterfacedObject, ICustomPropertiesFilterInternal)
  private type
    TPropertiesByComponentName = TDictionary<string, TStringDynArray>;
  private
    FInclude: TPropertiesByComponentName;
    FExclude: TPropertiesByComponentName;
    procedure DoAdd(
      var ADict: TPropertiesByComponentName;
      const AComponentName: string;
      const AProperties: TStringDynArray
    );
    class function ConcatUnique(const A, B: TStringDynArray): TStringDynArray; static;
  private
    { ICustomPropertiesFilter }
    procedure Process(
      const AComponent: TComponent;
      var AProperties: TStringDynArray
    );
  private
    { ICustomPropertiesFilterInternal }
    procedure Include(const AComponentName: string; const AProperties: TStringDynArray);
    procedure Exclude(const AComponentName: string; const AProperties: TStringDynArray);
    procedure ExcludeAll(const AComponentName: string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TComponentPropertyState }

constructor TComponentPropertyState.Create(
  const AComponent: TComponent;
  const AIgnore, ATemporary: TComponentDynArray;
  const AOptions: TComponentPropertyStateOptions
);
begin
  inherited Create;

  FComponent := AComponent;

  AddAssigned(AIgnore, FIgnore);
  AddAssigned(ATemporary, FTemporary);

  FOptions := AOptions;

  FStorageCache.FNamePath := '';
  FStorageCache.FIgnoreNamePath := nil;
  FStorageCache.FTemporaryNamePath := nil;

  FComponentPropertyStorage := GState.ComponentPropertyStorage;
end;

procedure TComponentPropertyState.Include(const AComponentName: string; const AProperties: TStringDynArray);
begin
  if FFilter = nil then begin
    FFilter := TCustomPropertiesFilter.Create;
  end;
  FFilter.Include(AComponentName, AProperties);
end;

procedure TComponentPropertyState.Exclude(const AComponentName: string; const AProperties: TStringDynArray);
begin
  if FFilter = nil then begin
    FFilter := TCustomPropertiesFilter.Create;
  end;
  FFilter.Exclude(AComponentName, AProperties);
end;

procedure TComponentPropertyState.ExcludeAll(const AComponentName: string);
begin
  if FFilter = nil then begin
    FFilter := TCustomPropertiesFilter.Create;
  end;
  FFilter.ExcludeAll(AComponentName);
end;

procedure TComponentPropertyState.Save;
begin
  FComponentPropertyStorage.Save(FComponent, FIgnore, FTemporary, FStorageCache, FFilter);
end;

procedure TComponentPropertyState.Restore;
begin
  if FWasRestored and (cpsoIgnoreSecondaryRestoreCalls in FOptions) then begin
    Exit;
  end;
  FWasRestored := True;
  FComponentPropertyStorage.Restore(FComponent, FIgnore, FTemporary, FStorageCache, FFilter);
end;

function TComponentPropertyState.GetOptions: TComponentPropertyStateOptions;
begin
  Result := FOptions;
end;

class procedure TComponentPropertyState.AddAssigned(const ASrc: TComponentDynArray; var ADest: TComponentDynArray);
var
  I, J: Integer;
begin
  J := 0;
  SetLength(ADest, Length(ASrc));
  for I := 0 to Length(ASrc) - 1 do begin
    if ASrc[I] <> nil then begin
      ADest[J] := ASrc[I];
      Inc(J);
    end;
  end;
  if J <> Length(ADest) then begin
    SetLength(ADest, J);
  end;
end;

{ TCustomPropertiesFilter }

constructor TCustomPropertiesFilter.Create;
begin
  inherited Create;
end;

destructor TCustomPropertiesFilter.Destroy;
begin
  FreeAndNil(FInclude);
  FReeAndNil(FExclude);
  inherited Destroy;
end;

procedure TCustomPropertiesFilter.DoAdd(
  var ADict: TPropertiesByComponentName;
  const AComponentName: string;
  const AProperties: TStringDynArray
);
var
  VName: string;
  VPropArr: TStringDynArray;
begin
  Assert(Length(AProperties) > 0);
  VName := LowerCase(AComponentName);
  if ADict = nil then begin
    ADict := TPropertiesByComponentName.Create;
    VPropArr := AProperties;
  end else begin
    if ADict.TryGetValue(VName, VPropArr) then begin
      VPropArr := ConcatUnique(VPropArr, AProperties);
    end else begin
      VPropArr := AProperties;
    end;
  end;
  ADict.Add(VName, VPropArr);
end;

procedure TCustomPropertiesFilter.Include(const AComponentName: string; const AProperties: TStringDynArray);
begin
  DoAdd(FInclude, AComponentName, AProperties);
end;

procedure TCustomPropertiesFilter.Exclude(const AComponentName: string; const AProperties: TStringDynArray);
begin
  DoAdd(FExclude, AComponentName, AProperties);
end;

procedure TCustomPropertiesFilter.ExcludeAll(const AComponentName: string);
var
  VName: string;
begin
  VName := LowerCase(AComponentName);
  if FExclude = nil then begin
    FExclude := TPropertiesByComponentName.Create;
  end;
  FExclude.AddOrSetValue(VName, []);
end;

procedure TCustomPropertiesFilter.Process(const AComponent: TComponent; var AProperties: TStringDynArray);
var
  I, J: Integer;
  VName: string;
  VPropArr: TStringDynArray;
begin
  VName := LowerCase(AComponent.Name);
  if (FInclude <> nil) and FInclude.TryGetValue(VName, VPropArr) then begin
    AProperties := ConcatUnique(AProperties, VPropArr);
  end;
  if (FExclude <> nil) and FExclude.TryGetValue(VName, VPropArr) then begin
    if Length(VPropArr) = 0 then begin
      // exclude all
      SetLength(AProperties, 0);
    end else begin
      for I := 0 to Length(VPropArr) - 1 do begin
        for J := Length(AProperties) - 1 downto 0 do begin
          if SameText(VPropArr[I], AProperties[J]) then begin
            Delete(AProperties, J, 1);
            Break;
          end;
        end;
      end;
    end;
  end;
end;

class function TCustomPropertiesFilter.ConcatUnique(const A, B: TStringDynArray): TStringDynArray;
var
  I, J, K: Integer;
  VLenA, VLenB: Integer;
  VFound: Boolean;
begin
  VLenA := Length(A);
  VLenB := Length(B);

  if (VLenA > 0) and (VLenB > 0) then begin
    K := 0;
    SetLength(Result, VLenA + VLenB);

    // copy unique A items to Result
    for I := 0 to VLenA - 1 do begin
      VFound := False;
      for J := 0 to VLenB - 1 do begin
        if SameText(A[I], B[J]) then begin
          VFound := True;
          Break;
        end;
      end;
      if not VFound then begin
        Result[K] := A[I];
        Inc(K);
      end;
    end;

    // copy full B array to Result
    for J := 0 to VLenB - 1 do begin
      Result[K] := B[J];
      Inc(K);
    end;

    if K < Length(Result) then begin
      SetLength(Result, K);
    end;
  end else
  if VLenA > 0 then begin
    Result := Copy(A);
  end else begin
    Result := Copy(B);
  end;
end;

end.
