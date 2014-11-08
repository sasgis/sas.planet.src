{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit i_DownloadResult;

interface

uses
  i_BinaryData,
  i_DownloadRequest;

type
  IDownloadResult = interface
    ['{E93918EB-D64B-479E-B4D6-E49B30425824}']
    function GetRequest: IDownloadRequest;
    property Request: IDownloadRequest read GetRequest;

    function GetIsServerExists: Boolean;
    property IsServerExists: Boolean read GetIsServerExists;
  end;

  IDownloadResultCanceled = interface(IDownloadResult)
    ['{2A22DD2C-6D70-4F27-AC7F-FB5ADB66B5A6}']
  end;

  IDownloadResultWithServerRespond = interface
    ['{55E0EC03-1D16-408A-B972-A706FE4D4247}']
    function GetStatusCode: Cardinal;
    property StatusCode: Cardinal read GetStatusCode;

    function GetRawResponseHeader: AnsiString;
    property RawResponseHeader: AnsiString read GetRawResponseHeader;
  end;

  IDownloadResultOk = interface(IDownloadResult)
    ['{EBBAA70B-60D4-421C-829D-F75CFFB43068}']
    function GetStatusCode: Cardinal;
    property StatusCode: Cardinal read GetStatusCode;

    function GetRawResponseHeader: AnsiString;
    property RawResponseHeader: AnsiString read GetRawResponseHeader;

    function GetContentType: AnsiString;
    property ContentType: AnsiString read GetContentType;

    function GetData: IBinaryData;
    property Data: IBinaryData read GetData;
  end;

  IDownloadResultError = interface(IDownloadResult)
    ['{E1C06FFC-605C-4E0D-977F-DCB6FE77041D}']
    function GetErrorText: string;
    property ErrorText: string read GetErrorText;
  end;

  IDownloadResultProxyError = interface(IDownloadResultError)
    ['{E41CC6C1-5B0B-4D6F-875D-4B800DAB5A51}']
  end;

  IDownloadResultNoConnetctToServer = interface(IDownloadResultError)
    ['{0C6013F7-CD38-44EC-808D-1CA3D1B0712B}']
  end;

  IDownloadResultUnknownError = interface(IDownloadResultError)
    ['{A5D55886-393E-4E85-97D4-17D824E2C75F}']
  end;

  IDownloadResultBanned = interface(IDownloadResultError)
    ['{C51C4998-89C4-440C-9605-A57F85BB7491}']
  end;

  IDownloadResultBadContentType = interface(IDownloadResultError)
    ['{A8C2F27E-D1DA-43CA-8F34-4156F906D50B}']
    function GetContentType: AnsiString;
    property ContentType: AnsiString read GetContentType;

    function GetRawResponseHeader: AnsiString;
    property RawResponseHeader: AnsiString read GetRawResponseHeader;
  end;

  IDownloadResultDataNotExists = interface(IDownloadResult)
    ['{BA3CF11A-2BD7-4541-B8F7-415E85047C20}']
    function GetReasonText: string;
    property ReasonText: string read GetReasonText;

    function GetRawResponseHeader: AnsiString;
    property RawResponseHeader: AnsiString read GetRawResponseHeader;
  end;

  IDownloadResultNotNecessary = interface(IDownloadResult)
    ['{C5E02C4F-733F-4E37-A565-700D3848E9DB}']
    function GetReasonText: string;
    property ReasonText: string read GetReasonText;

    function GetRawResponseHeader: AnsiString;
    property RawResponseHeader: AnsiString read GetRawResponseHeader;
  end;

implementation

end.
