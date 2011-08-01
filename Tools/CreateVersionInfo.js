function ReadFile(fso, fileName){
	var textFile = fso.OpenTextFile(fileName, 1, false);
	var text = "";
	if (!textFile.AtEndOfStream){
		text = textFile.ReadAll();
	};
	textFile.Close();
	return text;
}

function DateToVersionInfoString(dtmDate){
   if (dtmDate == null){
      return "null date";
   }
   var Year, Month, Day;
   Year = ""+dtmDate.getFullYear()+"";
   Year = Year.substr(2, 2)
   Month = dtmDate.getMonth() + 1;
   Day = dtmDate.getDate();
   
   return (""+Year+","+Month+","+Day);
}

var fso = WScript.CreateObject("Scripting.FileSystemObject");

var RevisionString = ReadFile(fso, "./Tools/revision.txt");
    if (RevisionString == "") {
        RevisionString = "0";
    } 
var now = new Date();
var VersionInfoString = DateToVersionInfoString(now) + "," + RevisionString; 
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