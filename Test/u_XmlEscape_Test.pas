unit u_XmlEscape_Test;

interface

uses
  TestFramework;

type
  TestXmlEscape = class(TTestCase)
  published
    procedure TestEmptyString;
    procedure TestNoSpecialChars;

    procedure TestTextNamed;
    procedure TestTextNumeric;

    procedure TestAttrNamed;
    procedure TestAttrNumeric;

    procedure TestIllegalCharsStripped;
  end;

implementation

uses
  u_XmlEscape;

{ TestXmlEscape }

procedure TestXmlEscape.TestEmptyString;
begin
  CheckEquals('', XmlEscapeText(''), 'Empty string (Text) failed');
  CheckEquals('', XmlEscapeAttr(''), 'Empty string (Attr) failed');
end;

procedure TestXmlEscape.TestNoSpecialChars;
const
  cStr = 'Hello World! 123';
begin
  CheckEquals(cStr, XmlEscapeText(cStr), 'Normal string should not be modified');
end;

procedure TestXmlEscape.TestTextNamed;
const
  cInput = 'A < B & B > A';
  cOutput = 'A &lt; B &amp; B &gt; A';
begin
  CheckEquals(cOutput, XmlEscapeText(cInput, False), 'Text Named entities failed');
end;

procedure TestXmlEscape.TestTextNumeric;
const
  cInput = 'A < B & B > A';
  cOutput = 'A &#60; B &#38; B &#62; A';
begin
  CheckEquals(cOutput, XmlEscapeText(cInput, True), 'Text Numeric entities failed');
end;

procedure TestXmlEscape.TestAttrNamed;
const
  cInput = 'Node "Name" & ''Value''';
  cOutput = 'Node &quot;Name&quot; &amp; &apos;Value&apos;';
begin
  CheckEquals(cOutput, XmlEscapeAttr(cInput, False), 'Attribute Named entities failed');
  CheckEquals('Node "Name" &amp; ''Value''', XmlEscapeText(cInput, False), 'XmlEscapeText should NOT escape quotes');
end;

procedure TestXmlEscape.TestAttrNumeric;
const
  cInput = 'Node "Name" & ''Value''';
  cOutput = 'Node &#34;Name&#34; &#38; &#39;Value&#39;';
begin
  CheckEquals(cOutput, XmlEscapeAttr(cInput, True), 'Attribute Numeric entities failed');
end;

procedure TestXmlEscape.TestIllegalCharsStripped;
const
  cInput = 'Bad' + #1 + #2 + 'Chars' + #9 + 'Tab';
  cOutput = 'BadChars&#x09;Tab';
begin
  CheckEquals(cOutput, XmlEscapeText(cInput, False), 'Illegal chars should be stripped');
end;

initialization
  RegisterTest(TestXmlEscape.Suite);

end.