unit D5FGL;

{$mode objfpc}{$H+}{$J-}
{$modeswitch advancedrecords}

interface

uses SysUtils, Classes, FGL, Math, Character;

function RunPart(Part: Integer; InputData: TStringList): String;

type
	TNumber = Int64;

	TRange = record
		Lower: TNumber;
		Upper: TNumber;

		class operator = (const R1, R2: TRange): Boolean;
	end;

	TRangeList = specialize TFPGList<TRange>;

	TMapping = class
	strict private
		FRangeFrom: TRange;
		FBaseTo: TNumber;

	public
		constructor Create(const Range: TRange; MapTo: TNumber);

		function TryMap(var Value: TNumber): Boolean;
		function TryMapRange(Range: TRange; RangesMapped, RangesUnmapped: TRangeList): Boolean;
	end;

	TMappingList = specialize TFPGObjectList<TMapping>;
	TNumberList = specialize TFPGList<TNumber>;

	TAlmanacMap = class
	strict private
		FMappings: TMappingList;

	public
		constructor Create();
		destructor Destroy; override;

		procedure AddMapping(MapTo, MapFrom, MapLength: TNumber);
		function MapNumber(Value: TNumber): TNumber;
		function MapRanges(Range: TRange): TRangeList;
	end;

	TAlmanacMapList = specialize TFPGObjectList<TAlmanacMap>;

implementation

procedure ParseInput(InputData: TStringList; Numbers: TNumberList; Maps: TAlmanacMapList);
var
	LLine: String;
	LStringPart: String;
	LSplit: TStringArray;
	LLastMap: TAlmanacMap;
begin
	for LLine in InputData do begin
		if Length(LLine) = 0 then
			continue;

		if LLine.StartsWith('seeds:') then begin
			LSplit := copy(LLine, 8).Split([' ']);
			for LStringPart in LSplit do
				Numbers.Add(StrToInt64(LStringPart));
		end

		else if IsNumber(LLine[1]) then begin
			LSplit := LLine.Split([' ']);
			LLastMap.AddMapping(
				StrToInt64(LSplit[0]),
				StrToInt64(LSplit[1]),
				StrToInt64(LSplit[2])
			);
		end

		else begin
			LLastMap := TAlmanacMap.Create;
			Maps.Add(LLastMap);
		end;
	end;
end;

function PartTwo(Numbers: TNumberList; Maps: TAlmanacMapList): TNumber;
var
	LRange: TRange;
	LRanges: TRangeList;
	LNewRanges: TRangeList;
	LTmpRanges: TRangeList;
	LInd: Integer;
	LMap: TAlmanacMap;
begin
	LRanges := TRangeList.Create;
	LNewRanges := TRangeList.Create;

	for LInd := 0 to (Numbers.Count div 2) - 1 do begin
		LRange.Lower := Numbers[LInd * 2];
		LRange.Upper := LRange.Lower + Numbers[LInd * 2 + 1] - 1;
		LRanges.Add(LRange);
	end;

	for LMap in Maps do begin
		for LRange in LRanges do begin
			LTmpRanges := LMap.MapRanges(LRange);
			LNewRanges.AddList(LTmpRanges);
			LTmpRanges.Free;
		end;

		LRanges.Clear;
		LRanges.AddList(LNewRanges);
		LNewRanges.Clear;
	end;

	LNewRanges.Free;

	result := LRanges[0].Lower;
	for LRange in LRanges do
		result := Min(result, LRange.Lower);

	LRanges.Free;
end;

function RunPart(Part: Integer; InputData: TStringList): String;
var
	LNumbers: TNumberList;
	LMaps: TAlmanacMapList;
begin
	LNumbers := TNumberList.Create;
	LMaps := TAlmanacMapList.Create;
	ParseInput(InputData, LNumbers, LMaps);

	case Part of
		2: result := IntToStr(PartTwo(LNumbers, LMaps));
		else
			result := 'No such part number!';
	end;

	LNumbers.Free;
	LMaps.Free;
end;

class operator TRange.= (const R1, R2: TRange): Boolean;
begin
	result := (R1.Lower = R2.Lower) and (R1.Upper = R2.Upper);
end;

constructor TMapping.Create(const Range: TRange; MapTo: TNumber);
begin
	FRangeFrom := Range;
	FBaseTo := MapTo;
end;

function TMapping.TryMap(var Value: TNumber): Boolean;
begin
	result := (Value >= FRangeFrom.Lower) and (Value <= FRangeFrom.Upper);
	if result then
		Value := FBaseTo + (Value - FRangeFrom.Lower);
end;

function TMapping.TryMapRange(Range: TRange; RangesMapped, RangesUnmapped: TRangeList): Boolean;
var
	LNewRange: TRange;
begin
	result := (Range.Lower <= FRangeFrom.Upper) and (Range.Upper >= FRangeFrom.Lower);
	if not result then exit;

	if Range.Lower < FRangeFrom.Lower then begin
		LNewRange.Lower := Range.Lower;
		LNewRange.Upper := FRangeFrom.Lower - 1;
		RangesUnmapped.Add(LNewRange);

		Range.Lower := FRangeFrom.Lower;
	end;

	if Range.Upper > FRangeFrom.Upper then begin
		LNewRange.Upper := Range.Upper;
		LNewRange.Lower := FRangeFrom.Upper + 1;
		RangesUnmapped.Add(LNewRange);

		Range.Upper := FRangeFrom.Upper;
	end;

	LNewRange.Lower := FBaseTo + (Range.Lower - FRangeFrom.Lower);
	LNewRange.Upper := FBaseTo + (Range.Upper - FRangeFrom.Lower);
	RangesMapped.Add(LNewRange);
end;

constructor TAlmanacMap.Create();
begin
	FMappings := TMappingList.Create;
end;

destructor TAlmanacMap.Destroy;
begin
	FMappings.Free;
end;

procedure TAlmanacMap.AddMapping(MapTo, MapFrom, MapLength: TNumber);
var
	LRange: TRange;
begin
	LRange.Lower := MapFrom;
	LRange.Upper := MapFrom + MapLength - 1;

	FMappings.Add(TMapping.Create(LRange, MapTo));
end;

function TAlmanacMap.MapNumber(Value: TNumber): TNumber;
var
	LMapping: TMapping;
begin
	result := Value;
	for LMapping in FMappings do begin
		if LMapping.TryMap(result) then exit;
	end;
end;

function TAlmanacMap.MapRanges(Range: TRange): TRangeList;
var
	LMapping: TMapping;
	LRange: TRange;
	LRanges: TRangeList;
	LNewRanges: TRangeList;
begin
	result := TRangeList.Create;
	LRanges := TRangeList.Create;
	LNewRanges := TRangeList.Create;
	LRanges.Add(Range);

	for LMapping in FMappings do begin
		for LRange in LRanges do begin
			if not LMapping.TryMapRange(LRange, result, LNewRanges) then
				LNewRanges.Add(LRange);
		end;

		LRanges.Clear;
		LRanges.AddList(LNewRanges);
		LNewRanges.Clear;
	end;

	result.AddList(LRanges);
	LRanges.Free;
	LNewRanges.Free;
end;

end.

