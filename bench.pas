program Bench;

{$mode objfpc}{$H+}{$J-}

uses SysUtils, Classes, {$ifdef fgl}D5FGL{$else}D5Generics{$endif};

var
	vLine: String;
	vInput: TStringList;
begin
	vInput := TStringList.Create;

	repeat
		readln(vLine);
		vInput.Add(vLine);
	until eof;

	writeln(RunPart(2, vInput));
end.

