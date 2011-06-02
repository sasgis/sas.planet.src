
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
VersionPostfix = VersionPostfix.replace(' -= Debug =-', "");
var fileVersionPostfix = fso.OpenTextFile("VersionPostfix.inc", 2, false);
fileVersionPostfix.write(VersionPostfix);
fileVersionPostfix.Close();

var dpr = ReadFile(fso, "SASPlanet.dpr");
dpr = dpr.replace('  EurekaLog in \'Debug\\EurekaLog\.pas\',\r\n', "");
var dprFile = fso.OpenTextFile("SASPlanet.dpr", 2, false);
dprFile.write(dpr);

var dproj = ReadFile(fso, "SASPlanet.dproj");
var reg = /EurekaLog First Line/;
if( reg.test(dproj) ){
	dproj = dproj.slice(0, dproj.indexOf("<!-- EurekaLog First Line") );
	var dprojFile = fso.OpenTextFile("SASPlanet.dproj", 2, false);
	dprojFile.write(dproj);
	dprojFile.Close();
}