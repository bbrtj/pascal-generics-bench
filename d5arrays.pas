unit D5Arrays;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Classes, FGL, Math;

function RunPart(Part: Integer; InputData: TStringList): String;

type
	TNumber = Int64;

	TRange = class
		Lower: TNumber;
		Upper: TNumber;

		constructor Create(const aLower, aUpper: TNumber);
	end;

	generic TCustomList<T> = class
	private
		type TRangeArray = array of T;

	private
		FLength: Integer;
		FCount: Integer;
		FItems: TRangeArray;
		FFreeObjects: Boolean;

		procedure AdjustLength(NewLength: Integer); inline;
		function GetItem(Index: Integer): T; inline;
		procedure SetItem(Index: Integer; Value: T); inline;

	public
		constructor Create(aFreeObjects: Boolean = True);
		destructor Destroy; override;

		procedure Add(Item: T); inline;
		procedure AddList(Other: TCustomList); inline;
		procedure Clear(); inline;

		property FreeObjects: Boolean read FFreeObjects write FFreeObjects;
		property Count: Integer read FCount;
		property Items[Index: Integer]: T read GetItem write SetItem; default;
	end;

	TRangeList = specialize TCustomList<TRange>;

	TMapping = class
	strict private
		FRangeFrom: TRange;
		FBaseTo: TNumber;

	public
		constructor Create(aLower, aUpper, MapTo: TNumber);
		destructor Destroy; override;

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
		procedure MapRanges(Range: TRange; RangeList: TRangeList);
	end;

	TAlmanacMapList = specialize TFPGObjectList<TAlmanacMap>;

implementation

procedure ParseInput(InputData: TStringList; var Numbers: TNumberList; var Maps: TAlmanacMapList);
var
	lLine: String;
	lSplit: TStringArray;
	lLastMap: TAlmanacMap;
	i: Integer;
begin
	for lLine in InputData do begin
		if Length(lLine) = 0 then
			continue;

		if lLine.StartsWith('seeds:') then begin
			lSplit := copy(lLine, 8).Split([' ']);
			for i := Low(lSplit) to High(lSplit) do
				Numbers.Add(StrToInt64(lSplit[i]));
		end

		else if lLine[1] in ['0' .. '9'] then begin
			lSplit := lLine.Split([' ']);
			lLastMap.AddMapping(
				StrToInt64(lSplit[0]),
				StrToInt64(lSplit[1]),
				StrToInt64(lSplit[2])
			);
		end

		else begin
			lLastMap := TAlmanacMap.Create;
			Maps.Add(lLastMap);
		end;
	end;
end;

function PartTwo(Numbers: TNumberList; Maps: TAlmanacMapList): TNumber;
var
	lRanges: TRangeList;
	lNewRanges: TRangeList;
	i, j: Integer;

begin
	lRanges := TRangeList.Create(False);
	lNewRanges := TRangeList.Create(False);

	for i := 0 to (Numbers.Count div 2) - 1 do begin
		lRanges.Add(TRange.Create(
			Numbers[i * 2],
			Numbers[i * 2] + Numbers[i * 2 + 1] - 1)
		);
	end;

	for i := 0 to Maps.Count - 1 do begin
		for j := 0 to lRanges.Count - 1 do begin
			Maps[i].MapRanges(lRanges.Items[j], lNewRanges);
		end;

		lRanges.Clear;
		lRanges.AddList(lNewRanges);
		lNewRanges.Clear;
	end;

	lNewRanges.Free;

	result := lRanges.Items[0].Lower;
	for i := 0 to lRanges.Count - 1 do
		result := Min(result, lRanges.Items[i].Lower);

	lRanges.FreeObjects := True;
	lRanges.Free;
end;

function RunPart(Part: Integer; InputData: TStringList): String;
var
	lNumbers: TNumberList;
	lMaps: TAlmanacMapList;
begin
	lNumbers := TNumberList.Create;
	lMaps := TAlmanacMapList.Create;
	ParseInput(InputData, lNumbers, lMaps);

	case Part of
		2: result := IntToStr(PartTwo(lNumbers, lMaps));
		else
			result := 'No such part number!';
	end;

	lNumbers.Free;
	lMaps.Free;
end;

constructor TRange.Create(const aLower, aUpper: TNumber);
begin
	self.Lower := aLower;
	self.Upper := aUpper;
end;

constructor TCustomList.Create(aFreeObjects: Boolean = True);
begin
	self.AdjustLength(2);
	FFreeObjects := aFreeObjects;
	FCount := 0;
end;

destructor TCustomList.Destroy;
begin
	self.Clear;
end;

function TCustomList.GetItem(Index: Integer): T;
begin
	result := FItems[Index];
end;

procedure TCustomList.SetItem(Index: Integer; Value: T);
begin
	FItems[Index] := Value;
end;

procedure TCustomList.AdjustLength(NewLength: Integer);
begin
	FLength := NewLength;
	SetLength(FItems, NewLength);
end;

procedure TCustomList.Add(Item: T);
begin
	FItems[FCount] := Item;

	Inc(FCount);
	if FCount = FLength then
		self.AdjustLength(FLength * 2);
end;

procedure TCustomList.AddList(Other: TCustomList);
var
	i: Integer;
begin
	for i := 0 to Other.Count - 1 do
		self.Add(Other[i]);
end;

procedure TCustomList.Clear();
var
	i: Integer;
begin
	if FFreeObjects then begin
		for i := FCount - 1 downto 0 do
			FItems[i].Free;
	end;

	FCount := 0;
end;


constructor TMapping.Create(aLower, aUpper, MapTo: TNumber);
begin
	FRangeFrom := TRange.Create(aLower, aUpper);
	FBaseTo := MapTo;
end;

destructor TMapping.Destroy;
begin
	FRangeFrom.Free;
	inherited;
end;

function TMapping.TryMap(var Value: TNumber): Boolean;
begin
	result := (Value >= FRangeFrom.Lower) and (Value <= FRangeFrom.Upper);
	if result then
		Value := FBaseTo + (Value - FRangeFrom.Lower);
end;

function TMapping.TryMapRange(Range: TRange; RangesMapped, RangesUnmapped: TRangeList): Boolean;
begin
	result := (Range.Lower <= FRangeFrom.Upper) and (Range.Upper >= FRangeFrom.Lower);
	if not result then exit;

	if Range.Lower < FRangeFrom.Lower then begin
		RangesUnmapped.Add(TRange.Create(Range.Lower, FRangeFrom.Lower - 1));

		Range.Lower := FRangeFrom.Lower;
	end;

	if Range.Upper > FRangeFrom.Upper then begin
		RangesUnmapped.Add(TRange.Create(FRangeFrom.Upper + 1, Range.Upper));

		Range.Upper := FRangeFrom.Upper;
	end;

	Range.Lower := FBaseTo + (Range.Lower - FRangeFrom.Lower);
	Range.Upper := FBaseTo + (Range.Upper - FRangeFrom.Lower);
	RangesMapped.Add(Range);
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
begin
	FMappings.Add(TMapping.Create(MapFrom, MapFrom + MapLength - 1, MapTo));
end;

function TAlmanacMap.MapNumber(Value: TNumber): TNumber;
var
	i: Integer;
begin
	result := Value;
	for i := 0 to FMappings.Count - 1 do begin
		if FMappings[i].TryMap(result) then exit;
	end;
end;

procedure TAlmanacMap.MapRanges(Range: TRange; RangeList: TRangeList);
var
	lRanges: TRangeList;
	lNewRanges: TRangeList;
	i, j: Integer;
begin
	lRanges := TRangeList.Create(False);
	lNewRanges := TRangeList.Create(False);
	lRanges.Add(Range);

	for i := 0 to FMappings.Count - 1 do begin
		for j := 0 to lRanges.Count - 1 do begin
			if not FMappings[i].TryMapRange(lRanges.Items[j], RangeList, lNewRanges) then
				lNewRanges.Add(lRanges.Items[j]);
		end;

		lRanges.Clear;
		lRanges.AddList(lNewRanges);
		lNewRanges.Clear;
	end;

	RangeList.AddList(lRanges);
	lRanges.Free;
	lNewRanges.Free;
end;

end.

