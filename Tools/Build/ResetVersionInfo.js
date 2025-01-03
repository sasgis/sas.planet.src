function ReadFile(fso, fileName){
	var textFile = fso.OpenTextFile(fileName, 1, false);
	var text = "";
	if (!textFile.AtEndOfStream){
		text = textFile.ReadAll();
	};
	textFile.Close();
	return text;
}

var fso = WScript.CreateObject("Scripting.FileSystemObject");

var VersionInfoString = "1,0,0,0";
var VersionInfoRc = ReadFile(fso, "./Resources/Version/Version.rc");

var MatchStr = "FILEVERSION (.*)?\r\n";
var ReplaceStr = "FILEVERSION " + VersionInfoString + "\r\n" 
var re = new RegExp(MatchStr, "i");
if (VersionInfoRc.match(re)){
	VersionInfoRc = VersionInfoRc.replace(re, ReplaceStr);
}

MatchStr = "VALUE \"FileVersion\", (.*)?\r\n";
ReplaceStr = "VALUE \"FileVersion\", \"" + VersionInfoString.replace(/,/g, ".") + "\\000\"\r\n" 
var re = new RegExp(MatchStr, "i");
if (VersionInfoRc.match(re)){
	VersionInfoRc = VersionInfoRc.replace(re, ReplaceStr);
}

var VersionInfoRcFile = fso.OpenTextFile("./Resources/Version/Version.rc", 2, false);
VersionInfoRcFile.write(VersionInfoRc);