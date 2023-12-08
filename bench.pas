program Bench;

{$mode objfpc}{$H+}{$J-}

uses SysUtils, Classes, {$ifdef fgl}D5FGL{$else}{$ifdef fglc}D5FGLC{$else}{$ifdef arrays}D5Arrays{$else}D5Generics{$endif}{$endif}{$endif};

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

