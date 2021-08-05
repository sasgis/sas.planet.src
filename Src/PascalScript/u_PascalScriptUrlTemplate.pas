{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_PascalScriptUrlTemplate;

interface

uses
  Types,
  Classes,
  SysUtils,
  uPSRuntime,
  uPSCompiler,
  i_TileRequest,
  i_ProjectionSet,
  i_TileDownloaderConfig,
  i_TileDownloadRequestBuilderConfig,
  i_LanguageManager,
  i_Listener,
  i_SimpleFlag;

procedure CompileTimeReg_UrlTemplate(const APSComp: TPSPascalCompiler);
procedure ExecTimeReg_UrlTemplate(const APSExec: TPSExec; const AObj: TObject);

type
  TPascalScriptUrlTemplate = class
  private
    type
      TItemValueType = (
        ivtText, ivtS, ivtX, ivtY, ivtYb, ivtZ, ivtZp1, ivtQ, ivtBBox,
        ivtTimeStamp, ivtX_1024, ivtY_1024, ivtVer, ivtLang, ivtSasPath
      );

    const
      CKnownItemValues: array [TItemValueType] of string = (
        '', 's', 'x', 'y', '-y', 'z', 'z+1', 'q', 'bbox',
        'timestamp', 'x/1024', 'y/1024', 'ver', 'lang', 'sas_path'
      );

    type
      TItemRec = record
        Value: string;
        ValueType: TItemValueType;
      end;
      PItemRec = ^TItemRec;
  private
    FItems: array of TItemRec;

    FUrlTmpl: string;
    FServerNames: array of string;
    FConfigListener: IListener;
    FConfigChangeFlag: ISimpleFlag;
    FRequestBuilderConfig: ITileDownloadRequestBuilderConfig;

    FProjectionSet: IProjectionSet;

    FLang: string;
    FLangListener: IListener;
    FLangChangeFlag: ISimpleFlag;
    FLangManager: ILanguageManager;

    FRequest: ITileRequest;

    procedure Parse(const ATmpl: string);
    procedure ParseServerNames(const ANames: string);

    function DoRender: string;
    function PSTemplateToUrl(const ATmpl: string): string;

    function GetLangValue: string; inline;
    function GetVersionValue: string; inline;
    function GetServerNameValue: string; inline;
    function GetBBox(const ATile: TPoint; const AZoom: Byte): string;
    class function GetQuadkey(X, Y: Integer; const AZoom: Byte): string;
    class function GetSasPathValue(const X, Y: Integer; const AZoom: Byte): string; static; inline;

    procedure OnConfigChange;
    procedure OnLangChange;
  public
    function Render(const ARequest: ITileRequest): string; inline;

    property Request: ITileRequest read FRequest write FRequest;

    constructor Create(
      const ALangManager: ILanguageManager;
      const AProjectionSet: IProjectionSet;
      const ARequestBuilderConfig: ITileDownloadRequestBuilderConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  StrUtils,
  DateUtils,
  c_CoordConverter,
  t_GeoTypes,
  i_Projection,
  i_ProjectionType,
  i_MapVersionInfo,
  u_GeoToStrFunc,
  u_ListenerByEvent,
  u_SimpleFlagWithInterlock;

procedure CompileTimeReg_UrlTemplate(const APSComp: TPSPascalCompiler);
begin
  APSComp.AddDelphiFunction('function TemplateToUrl(const ATmpl: string): string');
end;

procedure ExecTimeReg_UrlTemplate(const APSExec: TPSExec; const AObj: TObject);
begin
  Assert(AObj <> nil);

  APSExec.RegisterDelphiMethod(
    AObj as TPascalScriptUrlTemplate,
    @TPascalScriptUrlTemplate.PSTemplateToUrl,
    'TemplateToUrl',
    cdRegister
  );
end;

{ TPascalScriptUrlTemplate }

constructor TPascalScriptUrlTemplate.Create(
  const ALangManager: ILanguageManager;
  const AProjectionSet: IProjectionSet;
  const ARequestBuilderConfig: ITileDownloadRequestBuilderConfig
);
begin
  inherited Create;

  FLangManager := ALangManager;
  FProjectionSet := AProjectionSet;
  FRequestBuilderConfig := ARequestBuilderConfig;

  FRequest := nil;
  FItems := nil;
  FUrlTmpl := '';
  FServerNames := nil;

  FConfigChangeFlag := TSimpleFlagWithInterlock.Create;
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FRequestBuilderConfig.ChangeNotifier.Add(FConfigListener);

  OnConfigChange;

  FLangChangeFlag := TSimpleFlagWithInterlock.Create;
  FLangListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLangManager.ChangeNotifier.Add(FLangListener);

  OnLangChange;
end;

destructor TPascalScriptUrlTemplate.Destroy;
begin
  if (FRequestBuilderConfig <> nil) and (FConfigListener <> nil) then begin
    FRequestBuilderConfig.ChangeNotifier.Remove(FConfigListener);
    FConfigListener := nil;
  end;

  if (FLangManager <> nil) and (FLangListener <> nil) then begin
    FLangManager.ChangeNotifier.Remove(FLangListener);
    FLangListener := nil;
  end;

  inherited Destroy;
end;

procedure TPascalScriptUrlTemplate.OnConfigChange;
begin
  FConfigChangeFlag.SetFlag;
end;

procedure TPascalScriptUrlTemplate.OnLangChange;
begin
  FLangChangeFlag.SetFlag;
end;

function TPascalScriptUrlTemplate.PSTemplateToUrl(const ATmpl: string): string;
begin
  Parse(ATmpl);
  if FConfigChangeFlag.CheckFlagAndReset then begin
    ParseServerNames( string(FRequestBuilderConfig.ServerNames) );
  end;
  Result := DoRender;
end;

function TPascalScriptUrlTemplate.Render(const ARequest: ITileRequest): string;
begin
  FRequest := ARequest;
  if FConfigChangeFlag.CheckFlagAndReset then begin
    Parse( string(FRequestBuilderConfig.UrlBase) );
    ParseServerNames( string(FRequestBuilderConfig.ServerNames) );
  end;
  Result := DoRender;
end;

function TPascalScriptUrlTemplate.GetVersionValue: string;
var
  VInfo: IMapVersionInfo;
begin
  VInfo := FRequest.VersionInfo;
  if VInfo <> nil then begin
    Result := VInfo.UrlString;
  end else begin
    Result := '';
  end;
end;

function TPascalScriptUrlTemplate.GetLangValue: string;
begin
  if FLangChangeFlag.CheckFlagAndReset then begin
    FLang := FLangManager.GetCurrentLanguageCode;
  end;
  Result := FLang;
end;

function TPascalScriptUrlTemplate.GetServerNameValue: string;
var
  I: Integer;
begin
  I := Length(FServerNames);
  if I > 0 then begin
    Result := FServerNames[Random(I)];
  end else begin
    Result := '';
  end;
end;

class function TPascalScriptUrlTemplate.GetQuadkey(X, Y: Integer; const AZoom: Byte): string;
var
  I: Integer;
  P: PChar;
  VKey: Char;
  VMask: Integer;
begin
  // https://docs.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system

  if AZoom = 0 then begin
    Result := '';
    Exit;
  end;

  SetLength(Result, AZoom);
  P := Pointer(Result);

  for I := AZoom downto 1 do begin
    VKey := '0';
    VMask := 1 shl (I - 1);

    if X and VMask <> 0 then begin
      VKey := Succ(VKey);
    end;

    if Y and VMask <> 0 then begin
      VKey := Succ(VKey);
      VKey := Succ(VKey);
    end;

    P[AZoom-I] := VKey;
  end;
end;

function TPascalScriptUrlTemplate.GetBBox(const ATile: TPoint; const AZoom: Byte): string;

  function IsGeographicProjection(const AProjection: IProjection): Boolean;
  begin
    Result :=
      AProjection.ProjectionType.ProjectionEPSG = CGELonLatProjectionEPSG;
  end;

var
  VRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VProjection: IProjection;
  VProjectionType: IProjectionType;
begin
  VProjection := FProjectionSet.Zooms[AZoom];

  if IsGeographicProjection(VProjection) then begin
    // LonLat rect
    VRect := VProjection.TilePos2LonLatRect(ATile);
  end else begin
    // Metr rect
    VProjectionType := VProjection.ProjectionType;
    VLonLatRect := VProjection.TilePos2LonLatRect(ATile);
    VRect.TopLeft := VProjectionType.LonLat2Metr(VLonLatRect.TopLeft);
    VRect.BottomRight := VProjectionType.LonLat2Metr(VLonLatRect.BottomRight);
  end;

  Result :=
    RoundEx(VRect.Left, 8) + ',' +
    RoundEx(VRect.Bottom, 8) + ',' +
    RoundEx(VRect.Right, 8) + ',' +
    RoundEx(VRect.Top, 8);
end;

class function TPascalScriptUrlTemplate.GetSasPathValue(const X, Y: Integer;
  const AZoom: Byte): string;
begin
  Result :=
    'z' + IntToStr(AZoom + 1) + '/' +
    IntToStr(X div 1024) + '/' +
    'x' + IntToStr(X) + '/' +
    IntToStr(Y div 1024) + '/' +
    'y' + IntToStr(Y);
end;

procedure TPascalScriptUrlTemplate.ParseServerNames(const ANames: string);
const
  CSep: Char = ',';
var
  I: Integer;
  P, S: PChar;
begin
  FServerNames := nil;

  if ANames = '' then begin
    Exit;
  end;

  I := 0;
  S := PChar(ANames);

  while S^ <> #0 do begin
    P := S;

    while (S^ > ' ') and (S^ <> CSep) do begin
      Inc(S);
    end;

    SetLength(FServerNames, I+1);
    SetString(FServerNames[I], P, S-P);
    Inc(I);

    while (S^ <> #0) and ((S^ <= ' ') or (S^ = CSep)) do begin
      Inc(S);
    end;
  end;
end;

procedure TPascalScriptUrlTemplate.Parse(const ATmpl: string);

  procedure AddItem(const AVal: string; AType: TItemValueType);
  var
    I: Integer;
  begin
    I := Length(FItems);
    SetLength(FItems, I+1);
    FItems[I].Value := AVal;
    FItems[I].ValueType := AType;
  end;

  function GetNextTag(out ATag: string; var ATagBegin: Integer; out ATagEnd: Integer): Boolean;
  begin
    Result := False;
    ATagBegin := PosEx('{', ATmpl, ATagBegin);
    if ATagBegin > 0 then begin
      ATagEnd := PosEx('}', ATmpl, ATagBegin + 1);
      Result := ATagEnd > 0;
    end;
    if Result then begin
      ATag := LowerCase( Copy(ATmpl, ATagBegin + 1, ATagEnd - ATagBegin - 1) );
    end;
  end;

  function TagNameToType(const AName: string): TItemValueType;
  var
    I: TItemValueType;
  begin
    Result := ivtText;
    for I := Low(CKnownItemValues) to High(CKnownItemValues) do begin
      if CKnownItemValues[I] = AName then begin
        Result := I;
        Break;
      end;
    end;
  end;

var
  I, J, K: Integer;
  VTag: string;
  VTagType: TItemValueType;
begin
  if FUrlTmpl = ATmpl then begin
    Exit;
  end;

  SetLength(FItems, 0);
  I := 1;

  while True do begin
    K := I;
    if GetNextTag(VTag, I, J) then begin
      VTagType := TagNameToType(VTag);
      if VTagType = ivtText then begin
        // add text before tag and unrecognized tag
        AddItem( Copy(ATmpl, K, J - K + 1), ivtText);
      end else begin
        // add text before tag
        AddItem( Copy(ATmpl, K, I-K), ivtText);
        // add tag
        AddItem('', VTagType);
      end;
      I := J + 1;
    end else begin
      AddItem( Copy(ATmpl, K, Length(ATmpl) - K + 1), ivtText);
      Break;
    end;
  end;

  FUrlTmpl := ATmpl;
end;

function TPascalScriptUrlTemplate.DoRender: string;
var
  I: Integer;
  VTile: TPoint;
  VZoom: Byte;
  VItem: PItemRec;
  PUrl: PByte;
  VLen: Integer;
begin
  Result := '';

  if FRequest = nil then begin
    Assert(False);
    Exit;
  end;

  VLen := 0;

  VTile := FRequest.Tile;
  VZoom := FRequest.Zoom;

  for I := 0 to Length(FItems) - 1 do begin
    VItem := @FItems[I];
    case VItem.ValueType of
      ivtText:      { nothing to do } ;
      ivtS:         VItem.Value := GetServerNameValue;
      ivtX:         VItem.Value := IntToStr(VTile.X);
      ivtY:         VItem.Value := IntToStr(VTile.Y);
      ivtYb:        VItem.Value := IntToStr( (1 shl VZoom) - 1 - VTile.Y );
      ivtZ:         VItem.Value := IntToStr(VZoom);
      ivtZp1:       VItem.Value := IntToStr(VZoom + 1);
      ivtQ:         VItem.Value := GetQuadkey(VTile.X, VTile.Y, VZoom);
      ivtBBox:      VItem.Value := GetBBox(VTile, VZoom);
      ivtTimeStamp: VItem.Value := IntToStr(DateTimeToUnix(Now));
      ivtX_1024:    VItem.Value := IntToStr(VTile.X div 1024);
      ivtY_1024:    VItem.Value := IntToStr(VTile.Y div 1024);
      ivtVer:       VItem.Value := GetVersionValue;
      ivtLang:      VItem.Value := GetLangValue;
      ivtSasPath:   VItem.Value := GetSasPathValue(VTile.X, VTile.Y, VZoom);
    else
      Assert(False);
    end;
    Inc(VLen, Length(VItem.Value));
  end;

  if VLen > 0 then begin
    SetLength(Result, VLen);
    PUrl := Pointer(Result);
    for I := 0 to Length(FItems) - 1 do begin
      VItem := @FItems[I];
      VLen := Length(VItem.Value) * SizeOf(Char);
      if VLen > 0 then begin
        Move(Pointer(VItem.Value)^, PUrl^, VLen);
        Inc(PUrl, VLen);
      end;
    end;
  end;
end;

end.
