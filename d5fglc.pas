unit D5FGLC;

{
	Thanks to paweld from #pascal on libera IRC for this modification of code
}

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Classes, FGL, Math;

function RunPart(Part: Integer; InputData: TStringList): String;

type
  TNumber = Int64;

  { TRange }

  TRange = class
    Lower: TNumber;
    Upper: TNumber;
    constructor Create;
    constructor Create(aLower, aUpper: TNumber);
  end;

  { TRangeList }

  TRangeList = class(specialize TFPGObjectList<TRange>)
    procedure AddList(aSrc: TRangeList);
  end;

  { TMapping }

  TMapping = class
  strict private
    FRangeFrom: TRange;
    FBaseTo: TNumber;
  public
    constructor Create(aLower, aUpper, MapTo: TNumber);
    destructor Destroy; override;
    function TryMap(var Value: TNumber): Boolean;
    function TryMapRange(Range: TRange; var RangesMapped: TRangeList; var RangesUnmapped: TRangeList): Boolean;
  end;

  TMappingList = specialize TFPGObjectList<TMapping>;
  TNumberList = specialize TFPGList<TNumber>;

  { TAlmanacMap }

  TAlmanacMap = class
  strict private
    FMappings: TMappingList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddMapping(MapTo, MapFrom, MapLength: TNumber);
    function MapNumber(Value: TNumber): TNumber;
    function MapRanges(Range: TRange): TRangeList;
  end;

  TAlmanacMapList = specialize TFPGObjectList<TAlmanacMap>;

implementation

procedure ParseInput(InputData: TStringList; var Numbers: TNumberList; var Maps: TAlmanacMapList);
var
  LLine: String;
  LStringPart: String;
  LSplit: TStringArray;
  LLastMap: TAlmanacMap;
begin
  for LLine in InputData do
  begin
    if LLine = '' then
      continue;

    if LLine.StartsWith('seeds:') then
    begin
      LSplit := copy(LLine, 8).Split([' ']);
      for LStringPart in LSplit do
        Numbers.Add(StrToInt64(LStringPart));
    end
    else if LLine[1] in ['0'..'9'] then
    begin
      LSplit := LLine.Split([' ']);
      if Maps.Count > 0 then
        Maps[Maps.Count - 1].AddMapping(
          StrToInt64(LSplit[0]),
          StrToInt64(LSplit[1]),
          StrToInt64(LSplit[2])
          );
    end
    else
    begin
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
  i: Integer;
begin
  LRanges := TRangeList.Create;
  LNewRanges := TRangeList.Create;
  for LInd := 0 to (Numbers.Count div 2) - 1 do
  begin
    LRange := TRange.Create;
    LRange.Lower := Numbers[LInd * 2];
    LRange.Upper := LRange.Lower + Numbers[LInd * 2 + 1] - 1;
    LRanges.Add(LRange);
  end;
  for LMap in Maps do
  begin
    for i := 0 to LRanges.Count - 1 do
    begin
      LTmpRanges := LMap.MapRanges(LRanges[i]);
      LNewRanges.AddList(LTmpRanges);
      LTmpRanges.Free;
    end;
    LRanges.Clear;
    LRanges.AddList(LNewRanges);
    LNewRanges.Clear;
  end;

  LNewRanges.Free;

  Result := LRanges[0].Lower;
  for i := 0 to LRanges.Count - 1 do
    Result := Min(Result, LRanges[i].Lower);

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
    2: Result := IntToStr(PartTwo(LNumbers, LMaps));
    else
      Result := 'No such part number!';
  end;

  LNumbers.Free;
  LMaps.Free;
end;

{ TRange }

constructor TRange.Create;
begin
  inherited Create;
end;

constructor TRange.Create(aLower, aUpper: TNumber);
begin
  Create;
  Lower := aLower;
  Upper := aUpper;
end;

{ TRangeList }

procedure TRangeList.AddList(aSrc: TRangeList);
var
  i: Integer;
  LRange: TRange;
begin
  for i := 0 to aSrc.Count - 1 do
  begin
    LRange := TRange.Create(aSrc[i].Lower, aSrc[i].Upper);
    Add(LRange);
  end;
end;

{ TMapping }

constructor TMapping.Create(aLower, aUpper, MapTo: TNumber);
begin
  FRangeFrom := TRange.Create(aLower, aUpper);
  FBaseTo := MapTo;
end;

destructor TMapping.Destroy;
begin
  FRangeFrom.Free;
  inherited Destroy;
end;

function TMapping.TryMap(var Value: TNumber): Boolean;
begin
  Result := (Value >= FRangeFrom.Lower) and (Value <= FRangeFrom.Upper);
  if Result then
    Value := FBaseTo + (Value - FRangeFrom.Lower);
end;

function TMapping.TryMapRange(Range: TRange; var RangesMapped: TRangeList; var RangesUnmapped: TRangeList): Boolean;
var
  LNewRange: TRange;
begin
  Result := (Range.Lower <= FRangeFrom.Upper) and (Range.Upper >= FRangeFrom.Lower);
  if not Result then
    exit;

  if Range.Lower < FRangeFrom.Lower then
  begin
    LNewRange := TRange.Create(Range.Lower, FRangeFrom.Lower - 1);
    RangesUnmapped.Add(LNewRange);
    Range.Lower := FRangeFrom.Lower;
  end;

  if Range.Upper > FRangeFrom.Upper then
  begin
    LNewRange := TRange.Create(FRangeFrom.Upper + 1, Range.Upper);
    RangesUnmapped.Add(LNewRange);
    Range.Upper := FRangeFrom.Upper;
  end;

  LNewRange := TRange.Create;
  LNewRange.Lower := FBaseTo + (Range.Lower - FRangeFrom.Lower);
  LNewRange.Upper := FBaseTo + (Range.Upper - FRangeFrom.Lower);
  RangesMapped.Add(LNewRange);
end;

{ TAlmanacMap }

constructor TAlmanacMap.Create();
begin
  FMappings := TMappingList.Create;
end;

destructor TAlmanacMap.Destroy;
begin
  FMappings.Free;
  inherited Destroy;
end;

procedure TAlmanacMap.AddMapping(MapTo, MapFrom, MapLength: TNumber);
var
  LMapping: TMapping;
begin
  LMapping := TMapping.Create(MapFrom, MapFrom + MapLength - 1, MapTo);
  FMappings.Add(LMapping);
end;

function TAlmanacMap.MapNumber(Value: TNumber): TNumber;
var
  LMapping: TMapping;
begin
  Result := Value;
  for LMapping in FMappings do
  begin
    if LMapping.TryMap(Result) then exit;
  end;
end;

function TAlmanacMap.MapRanges(Range: TRange): TRangeList;
var
  LMapping: TMapping;
  LRange: TRange;
  LRanges: TRangeList;
  LNewRanges: TRangeList;
  i: Integer;
begin
  Result := TRangeList.Create;
  LRanges := TRangeList.Create;
  LNewRanges := TRangeList.Create;
  LRange := TRange.Create(Range.Lower, Range.Upper);
  LRanges.Add(LRange);
  for LMapping in FMappings do
  begin
    for i := 0 to LRanges.Count - 1 do
    begin
      if not LMapping.TryMapRange(LRanges[i], Result, LNewRanges) then
      begin
        LRange := TRange.Create(LRanges[i].Lower, LRanges[i].Upper);
        LNewRanges.Add(LRange);
      end;
    end;
    LRanges.Clear;
    LRanges.AddList(LNewRanges);
    LNewRanges.Clear;
  end;
  Result.AddList(LRanges);
  LRanges.Free;
  LNewRanges.Free;
end;

end.

