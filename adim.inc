{$IFDEF INTF_QUANTITY}
{$UNDEF INTF_QUANTITY}
type
  TQuantity = record
  private
    FValue: double;
  public
    function Abs: TQuantity;
    function Value: double;
    function Value(const APrefixes: TPrefixes): double;
    function ToString: string;
    function ToVerboseString: string;
    function ToString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
    function ToVerboseString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
    class operator +  (const AValue: TQuantity): TQuantity;
    class operator -  (const AValue: TQuantity): TQuantity;
    class operator +  (const ALeft, ARight: TQuantity): TQuantity;
    class operator -  (const ALeft, ARight: TQuantity): TQuantity;
    class operator *  (const AValue: double; const ASelf: TQuantity): TQuantity;
    class operator *  (const ASelf: TQuantity; const AValue: double): TQuantity;
    class operator /  (const ASelf: TQuantity; const AValue: double): TQuantity;
    class operator /  (const ALeft, ARight: TQuantity): double;
    class operator mod(const ALeft, ARight: TQuantity): TQuantity;
    class operator =  (const ALeft, ARight: TQuantity): boolean;
    class operator <  (const ALeft, ARight: TQuantity): boolean;
    class operator >  (const ALeft, ARight: TQuantity): boolean;
    class operator <= (const ALeft, ARight: TQuantity): boolean;
    class operator >= (const ALeft, ARight: TQuantity): boolean;
{$ENDIF}

{$IFDEF INTF_END}
{$UNDEF INTF_END}
end;
{$ENDIF}

{$IFDEF IMPL_QUANTITY}
{$UNDEF IMPL_QUANTITY}
function TQuantity.Abs: TQuantity;
begin
  result.FValue := System.Abs(FValue);
end;

function TQuantity.Value: double;
begin
  result := FValue;
end;

function TQuantity.Value(const APrefixes: TPrefixes): double;
var
  I: longint;
  Exponent: longint;
  PrefixCount: longint;
begin
  PrefixCount := Length(APrefixes);
  if PrefixCount = Length(CPREFIXES) then
  begin
    Exponent := 0;
    for I := 0 to PrefixCount -1 do
      Inc(Exponent, PrefixTable[CPREFIXES[I]].Exponent * CEXPONENTS[I]);

    for I := 0 to PrefixCount -1 do
      Dec(Exponent, PrefixTable[APrefixes[I]].Exponent * CEXPONENTS[I]);
    
    if Exponent <> 0 then  
      result := FValue * IntPower(10, Exponent)
    else  
      result := FValue;
  end else   
    if PrefixCount = 0 then 
      result := FValue
    else  
      raise Exception.Create('Wrong number of prefixes.');   
end;     

function TQuantity.ToString: string;
begin
  result := FloatToStr(FValue) + ' ' + GetSymbol(CSYMBOL, CPREFIXES);
end;

function TQuantity.ToVerboseString: string;
begin
  if (FValue < -1) or (FValue > 1) then
    result := FloatToStr(FValue) + ' ' + GetName(CPLURALNAME, CPREFIXES)
  else
    result := FloatToStr(FValue) + ' ' + GetName(CSINGULARNAME, CPREFIXES);
end;

function TQuantity.ToString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
var
  FactoredValue: double;
begin
  FactoredValue := Value(APrefixes);

  if Length(APrefixes) = 0 then
    result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetSymbol(CSYMBOL, CPREFIXES)
  else
    result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetSymbol(CSYMBOL, APrefixes);
end;

function TQuantity.ToVerboseString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
var
  FactoredValue: double;
begin
  FactoredValue := Value(APrefixes);

  if (FactoredValue < -1) or (FactoredValue > 1) then
  begin
    if Length(APrefixes) = 0 then
      result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetName(CPLURALNAME, CPREFIXES)
    else
      result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetName(CPLURALNAME, APrefixes);
  end else
  begin
    if Length(APrefixes) = 0 then
      result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetName(CSINGULARNAME, CPREFIXES)
    else
      result := FloatToStrF(FactoredValue, ffGeneral, APrecision, ADigits) + ' ' + GetName(CSINGULARNAME, APrefixes)
  end; 
end;                                             

class operator TQuantity.+(const AValue: TQuantity): TQuantity;
begin
  result.FValue := AValue.FValue;
end;

class operator TQuantity.-(const AValue: TQuantity): TQuantity;
begin
  result.FValue := -AValue.FValue;
end;

class operator TQuantity.+(const ALeft, ARight: TQuantity): TQuantity;
begin
  result.FValue := ALeft.FValue + ARight.FValue;
end;

class operator TQuantity.-(const ALeft, ARight: TQuantity): TQuantity;
begin
  result.FValue := ALeft.FValue - ARight.FValue;
end;

class operator TQuantity.*(const AValue: double; const ASelf: TQuantity): TQuantity;
begin
  result.FValue := AValue * ASelf.FValue;
end;

class operator TQuantity.*(const ASelf: TQuantity; const AValue: double): TQuantity;
begin
  result.FValue := ASelf.FValue * AValue;
end;

class operator TQuantity./(const ASelf: TQuantity; const AValue: double): TQuantity;
begin
  result.FValue := ASelf.FValue / AValue;
end;

class operator TQuantity./(const ALeft, ARight: TQuantity): double;
begin
  result := ALeft.FValue / ARight.FValue;
end;

class operator TQuantity.mod(const ALeft, ARight: TQuantity): TQuantity;
begin
  result.FValue := ALeft.FValue mod ARight.FValue;
end;

class operator TQuantity.=(const ALeft, ARight: TQuantity): boolean;
begin
  result := ALeft.FValue = ARight.FValue;
end;

class operator TQuantity.<(const ALeft, ARight: TQuantity): boolean;
begin
  result := ALeft.FValue < ARight.FValue;
end;

class operator TQuantity.>(const ALeft, ARight: TQuantity): boolean;
begin
  result := ALeft.FValue > ARight.FValue;
end;

class operator TQuantity.<=(const ALeft, ARight: TQuantity): boolean;
begin
  result := ALeft.FValue <= ARight.FValue;
end;

class operator TQuantity.>=(const ALeft, ARight: TQuantity): boolean;
begin
  result := ALeft.FValue >= ARight.FValue;
end;
{$ENDIF}

{$IFDEF INTF_UNIT}
{$UNDEF INTF_UNIT}
type
  TUnit = record
    class function From(const AQuantity: TQuantity): TQuantity; inline; static;
    class operator *(const AValue: double; const ASelf: TUnit): TQuantity;
{$ENDIF}

{$IFDEF IMPL_UNIT}
{$UNDEF IMPL_UNIT}
class function TUnit.From(const AQuantity: TQuantity): TQuantity;
begin
  result.FValue := AQuantity.FValue;
end;

class operator TUnit.*(const AValue: double; const ASelf: TUnit): TQuantity;
begin
  result.FValue := AValue;
end;
{$ENDIF}

