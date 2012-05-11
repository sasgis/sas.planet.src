{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MapAttachmentsPascalScript;

interface

uses
  Classes,
  Windows,
  SysUtils,
  uPSRuntime,
  uPSCompiler,
  uPSUtils,
  i_StringByLanguage,
  i_MapAttachmentsInfo,
  i_MapAttachmentsFactory,
  u_BasePascalCompiler;

type
  TMapAttachmentsFactoryPascalScript = class(TBaseFactoryPascalScript, IMapAttachmentsFactory)
  protected
    function DoCompilerOnAuxUses(
      ACompiler: TBasePascalCompiler;
      const AName: string
    ): Boolean; override;
  protected
    { IMapAttachmentsFactory }
    function GetCompiledData: String;
  public
    constructor Create(const AScriptText: string);
  end;

procedure RunParseAttachmentScript(
    var AMapAttachmentsFactory: IMapAttachmentsFactory;
    const AMapAttachmentsInfo: IMapAttachmentsInfo;
    const AParseAttachmentScript: String;
    var ADesc: String
  );

implementation

uses
  u_ResStrings,
  u_TileRequestBuilderHelpers;

{ TMapAttachmentsFactoryPascalScript }

constructor TMapAttachmentsFactoryPascalScript.Create(const AScriptText: string);
begin
  inherited;
  PreparePascalScript(AScriptText);
end;

function TMapAttachmentsFactoryPascalScript.DoCompilerOnAuxUses(
  ACompiler: TBasePascalCompiler;
  const AName: string
): Boolean;
var
  T: TPSType;
begin
  if SameText('SYSTEM', AName) then begin
    T := ACompiler.FindType('string');
    ACompiler.AddUsedVariable('ResultText', t);
    ACompiler.AddUsedVariable('SourceText', t);
    ACompiler.AddUsedVariable('AttachmentNumber', t);
    ACompiler.AddUsedVariable('AttachmentSubCache', t);

    T := ACompiler.FindType('integer');
    ACompiler.AddUsedVariable('MaxSubIndex', t);

    with ACompiler.AddInterface(ACompiler.FindInterface('IUnknown'), IStringByLanguage, 'IStringByLanguage') do begin
      RegisterMethod('function GetString(ALangIndex: Integer): string', cdRegister);
      RegisterMethod('function GetDefault: string', cdRegister);
    end;

    with ACompiler.AddInterface(ACompiler.FindInterface('IStringByLanguage'), IMapAttachmentsInfo, 'IMapAttachmentsInfo') do begin
      RegisterMethod('function GetGUID: TGUID', cdStdCall);
      RegisterMethod('function GetMaxSubIndex: Integer', cdStdCall);
      RegisterMethod('function GetParseNumberAfter: String', cdStdCall);
      RegisterMethod('function GetNameInCache(const AIndex: Integer): String', cdStdCall);
      RegisterMethod('function GetExt(const AIndex: Integer): String', cdStdCall);
      RegisterMethod('function GetEnabled(const AIndex: Integer): Boolean', cdStdCall);
      RegisterMethod('function GetDefURLBase(const AIndex: Integer): String', cdStdCall);
      RegisterMethod('function GetContentType(const AIndex: Integer): String', cdStdCall);
      RegisterMethod('function GetUseDwn: Boolean', cdStdCall);
      RegisterMethod('function GetUseDel: Boolean', cdStdCall);
    end;

    T := ACompiler.FindType('IMapAttachmentsInfo');
    ACompiler.AddUsedVariable('MapAttachmentsInfo', t);

    Result := TRUE;
  end else begin
    Result := FALSE;
  end;
end;

function TMapAttachmentsFactoryPascalScript.GetCompiledData: String;
begin
  Result := FCompiledData;
end;

procedure RunParseAttachmentScript(
  var AMapAttachmentsFactory: IMapAttachmentsFactory;
  const AMapAttachmentsInfo: IMapAttachmentsInfo;
  const AParseAttachmentScript: String;
  var ADesc: String
);
var
  VSasPSExec: TBasePascalScriptExec;
  VpSourceText: PPSVariantAString;
  VpResultText: PPSVariantAString;
  VpAttachmentNumber: PPSVariantAString;
  VpAttachmentSubCache: PPSVariantAString;
  VpMaxSubIndex: PPSVariantS32;
  VpMapAttachmentsInfo: PPSVariantInterface;
begin
  // compiler
  if not Assigned(AMapAttachmentsFactory) then begin
    // create
    AMapAttachmentsFactory := TMapAttachmentsFactoryPascalScript.Create(AParseAttachmentScript);
  end;

  // prepare
  VSasPSExec := TBasePascalScriptExec.Create;
  try
    // register common functions
    VSasPSExec.RegisterAppCommonRoutines;

    // special functions
    VSasPSExec.RegisterDelphiFunction(@DownloadFileToLocal, 'DownloadFileToLocal', cdRegister);

    if not VSasPSExec.LoadData(AMapAttachmentsFactory.GetCompiledData) then begin
      raise Exception.Create(SAS_ERR_UrlScriptByteCodeLoad);
    end;

    // link output parameter to parser
    VpResultText := PPSVariantAString(VSasPSExec.GetVar2('ResultText'));
    VpResultText^.Data := '';

    // link input parameters and functions
    VpSourceText := PPSVariantAString(VSasPSExec.GetVar2('SourceText'));
    VpSourceText^.Data := ADesc;

    VpMaxSubIndex := PPSVariantS32(VSasPSExec.GetVar2('MaxSubIndex'));
    VpMaxSubIndex^.Data := AMapAttachmentsInfo.MaxSubIndex;

    VpAttachmentNumber := PPSVariantAString(VSasPSExec.GetVar2('AttachmentNumber'));
    VpAttachmentNumber^.Data := GetNumberAfter(AMapAttachmentsInfo.GetParseNumberAfter, ADesc);

    VpAttachmentSubCache := PPSVariantAString(VSasPSExec.GetVar2('AttachmentSubCache'));
    VpAttachmentSubCache^.Data := GetDiv3Path(VpAttachmentNumber^.Data);

    VpMapAttachmentsInfo := PPSVariantInterface(VSasPSExec.GetVar2('MapAttachmentsInfo'));
    VpMapAttachmentsInfo^.Data := AMapAttachmentsInfo;

    // run parser
    VSasPSExec.RunScript;

    // subst result value
    if (Length(VpResultText^.Data) > 0) then begin
      ADesc := VpResultText^.Data;
    end;
  finally
    FreeAndNil(VSasPSExec);
  end;
end;

end.
