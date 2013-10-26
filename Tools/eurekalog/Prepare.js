
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

var dpr = ReadFile(fso, "SASPlanet.dpr");
var reg = /EurekaLog/i;
if (!reg.test(dpr)){
	dpr = dpr.replace('uses\r\n', 'uses\r\n  EurekaLog,\r\n');
	var dprFile = fso.OpenTextFile("SASPlanet.dpr", 2, false);
	dprFile.write(dpr);
	dprFile.Close();
}

var dproj = ReadFile(fso, "SASPlanet.dproj");
var reg = /(<DCC_Define>DEBUG)(.*?)(<\/DCC_Define>)/i;
if (reg.test(dproj)){
	dproj = dproj.replace(reg, '$1$2;EUREKALOG;EUREKALOG_VER6$3');
	var dprojFile = fso.OpenTextFile("SASPlanet.dproj", 2, false);
	dprojFile.write(dproj);
	dprojFile.Close();
}


