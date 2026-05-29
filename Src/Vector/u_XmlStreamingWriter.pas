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

unit u_XmlStreamingWriter;

interface

uses
  Math,
  Classes,
  SysUtils;

type
  TXmlStreamingWriter = class
  private const
    CBufferSizeKB = 4 * 1024;
    CVersionDefault = '1.0';
    CStandAloneDefault = 'yes';
    CIndentCharsDefault = '  ';
    CLineBreakDefault = #10;
  private type
    TStringStack = class
    private
      FItems: array of string;
      FCount: Integer;
      FCapacity: Integer;
    public
      procedure Push(const AValue: string);
      function Pop: string;
      property Count: Integer read FCount;
    end;
  private
    FVersion: string;
    FEncoding: string;
    FStandAlone: string;
    FIndentChars: string;
    FLineBreak: string;

    FWriter: TStreamWriter;
    FOpenTags: TStringStack;
    FIndentLevel: Integer;
    FIsEmptyTag: Boolean;
    FDocumentEnded: Boolean;

    procedure InternalCloseTag(const AWithNewLine: Boolean);
    procedure InternalWrite(const AStr: string); inline;
    procedure InternalWriteIndent;
  public
    procedure StartDocument;
    procedure EndDocument;

    procedure StartElement(const AName: string);
    procedure EndElement(const AWriteIndent: Boolean = True);

    procedure WriteAttribute(const AName, AValue: string);
    procedure WriteString(const AText: string);
    procedure WriteElementString(const AName, AValue: string);
    procedure WriteRaw(const AText: string);
  public
    constructor Create(const AStream: TStream; const AEncoding: TEncoding);
    destructor Destroy; override;
  public
    property Version: string read FVersion write FVersion;
    property Encoding: string read FEncoding write FEncoding;
    property StandAlone: string read FStandAlone write FStandAlone;
    property IndentChars: string read FIndentChars write FIndentChars;
    property LineBreak: string read FLineBreak write FLineBreak;
  end;

  EXmlStreamingWriter = class(Exception);
  EXmlStreamingWriterStringStack = class(Exception);

implementation

uses
  u_XmlEscape;

{ TXmlStreamingWriter }

constructor TXmlStreamingWriter.Create(
  const AStream: TStream;
  const AEncoding: TEncoding
);
begin
  inherited Create;

  FVersion := CVersionDefault;
  FEncoding := AEncoding.MIMEName;
  FIndentChars := CIndentCharsDefault;
  FLineBreak := CLineBreakDefault;

  FWriter := TStreamWriter.Create(AStream, AEncoding, CBufferSizeKB * 1024);
  FWriter.AutoFlush := False;

  FOpenTags := TStringStack.Create;

  FIndentLevel := 0;
  FIsEmptyTag := False;
  FDocumentEnded := False;
end;

destructor TXmlStreamingWriter.Destroy;
begin
  if not FDocumentEnded then begin
    EndDocument;
  end;
  FreeAndNil(FOpenTags);
  FreeAndNil(FWriter);
  inherited Destroy;
end;

procedure TXmlStreamingWriter.InternalWrite(const AStr: string);
begin
  FWriter.Write(AStr);
end;

procedure TXmlStreamingWriter.InternalWriteIndent;
var
  I: Integer;
begin
  if FIndentChars <> '' then begin
    for I := 1 to FIndentLevel do begin
      InternalWrite(FIndentChars);
    end;
  end;
end;

procedure TXmlStreamingWriter.InternalCloseTag(const AWithNewLine: Boolean);
begin
  if FIsEmptyTag then begin
    FIsEmptyTag := False;
    InternalWrite('>');
    if AWithNewLine then begin
      InternalWrite(FLineBreak);
    end;
  end;
end;

procedure TXmlStreamingWriter.StartDocument;
var
  VProlog: string;
begin
  VProlog := '<?xml version="' + FVersion + '" encoding="' + FEncoding + '"';
  if FStandAlone <> '' then begin
    VProlog := VProlog + ' standalone="' + FStandAlone + '"';
  end;
  InternalWrite(VProlog + '?>' + FLineBreak);
end;

procedure TXmlStreamingWriter.EndDocument;
begin
  while FOpenTags.Count > 0 do begin
    EndElement;
  end;
  FWriter.Flush;
  FDocumentEnded := True;
end;

procedure TXmlStreamingWriter.StartElement(const AName: string);
begin
  InternalCloseTag(True);
  InternalWriteIndent;
  InternalWrite('<' + AName);
  FOpenTags.Push(AName);
  FIsEmptyTag := True;
  Inc(FIndentLevel);
end;

procedure TXmlStreamingWriter.EndElement(const AWriteIndent: Boolean);
begin
  if FOpenTags.Count = 0 then begin
    Exit;
  end;

  Dec(FIndentLevel);
  if FIsEmptyTag then begin
    FIsEmptyTag := False;
    InternalWrite(' />' + FLineBreak);
    FOpenTags.Pop;
  end else begin
    if AWriteIndent then begin
      InternalWriteIndent;
    end;
    InternalWrite('</' + FOpenTags.Pop + '>' + FLineBreak);
  end;
end;

procedure TXmlStreamingWriter.WriteAttribute(const AName, AValue: string);
begin
  if not FIsEmptyTag then begin
    raise EXmlStreamingWriter.Create('Cannot write attribute outside of an element start.');
  end;
  InternalWrite(' ' + AName + '="' + XmlEscapeAttr(AValue) + '"');
end;

procedure TXmlStreamingWriter.WriteRaw(const AText: string);
begin
  InternalCloseTag(False);
  InternalWrite(AText);
end;

procedure TXmlStreamingWriter.WriteString(const AText: string);
begin
  InternalCloseTag(False);
  InternalWrite(XmlEscapeText(AText));
end;

procedure TXmlStreamingWriter.WriteElementString(const AName, AValue: string);
begin
  StartElement(AName);
  WriteString(AValue);
  EndElement(False);
end;

{ TXmlStreamingWriter.TStringStack }

procedure TXmlStreamingWriter.TStringStack.Push(const AValue: string);
begin
  if FCount = FCapacity then begin
    FCapacity := GrowCollection(FCapacity, FCount + 1);
    SetLength(FItems, FCapacity);
  end;

  FItems[FCount] := AValue;
  Inc(FCount);
end;

function TXmlStreamingWriter.TStringStack.Pop: string;
begin
  if FCount = 0 then begin
    raise EXmlStreamingWriterStringStack.Create('Pop: stack is empty');
  end;

  Dec(FCount);
  Result := FItems[FCount];
  FItems[FCount] := ''; // release string reference
end;

end.
