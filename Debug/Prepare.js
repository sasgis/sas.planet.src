
function ReadFile( fso, fileName){
	var textFile = fso.OpenTextFile(fileName, 1, false);
	var text = "";
	if (!textFile.AtEndOfStream){
		text = textFile.ReadAll();
	};
	textFile.Close();
	return text;
}

var fso = WScript.CreateObject("Scripting.FileSystemObject");

var VersionPostfix = ReadFile(fso, "VersionPostfix.inc");
VersionPostfix = VersionPostfix + ' -= Debug =-';
var fileVersionPostfix = fso.OpenTextFile("VersionPostfix.inc", 2, false);
fileVersionPostfix.write(VersionPostfix);
fileVersionPostfix.Close();

var dpr = ReadFile(fso, "SASPlanet.dpr");
var reg = /EurekaLog/i;
if (!reg.test(dpr)){
	dpr = dpr.replace('uses\r\n', 'uses\r\n  EurekaLog in \'Debug\\EurekaLog\.pas\',\r\n');
	var dprFile = fso.OpenTextFile("SASPlanet.dpr", 2, false);
	dprFile.write(dpr);
	dprFile.Close();
}

var eof = ReadFile(fso, "Debug/SASPlanet.eof");
var dproj = ReadFile(fso, "SASPlanet.dproj");
reg = /<!-- EurekaLog First Line/i;
if (!reg.test(dproj)){
	dproj = dproj + '\r\n' + "<!-- EurekaLog First Line" + '\r\n' + eof + '\r\n' + "EurekaLog Last Line -->";
	var dprojFile = fso.OpenTextFile("SASPlanet.dproj", 2, false);
	dprojFile.write(dproj);
	dprojFile.Close();
}