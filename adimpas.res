        ��  ��                  �      �� ��               <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
 <assemblyIdentity version="1.0.0.0" processorArchitecture="*" name="CompanyName.ProductName.AppName" type="win32"/>
 <description>Your application description.</description>
 <dependency>
  <dependentAssembly>
   <assemblyIdentity type="win32" name="Microsoft.Windows.Common-Controls" version="6.0.0.0" processorArchitecture="*" publicKeyToken="6595b64144ccf1df" language="*"/>
  </dependentAssembly>
 </dependency>
 <trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">
  <security>
   <requestedPrivileges>
    <requestedExecutionLevel level="asInvoker" uiAccess="false"/>
   </requestedPrivileges>
  </security>
 </trustInfo>
 <compatibility xmlns="urn:schemas-microsoft-com:compatibility.v1">
  <application>
   <!-- Windows Vista -->
   <supportedOS Id="{e2011457-1546-43c5-a5fe-008deee3d3f0}" />
   <!-- Windows 7 -->
   <supportedOS Id="{35138b9a-5d96-4fbd-8e2d-a2440225f93a}" />
   <!-- Windows 8 -->
   <supportedOS Id="{4a2f28e3-53b9-4441-ba9c-d69d4a4a6e38}" />
   <!-- Windows 8.1 -->
   <supportedOS Id="{1f676c76-80e1-4239-95bb-83d0f6d0da78}" />
   <!-- Windows 10 -->
   <supportedOS Id="{8e0f7a12-bfb3-4fe8-b9a5-48fd50a15a9a}" />
   </application>
  </compatibility>
 <asmv3:application xmlns:asmv3="urn:schemas-microsoft-com:asm.v3">
  <asmv3:windowsSettings xmlns="http://schemas.microsoft.com/SMI/2005/WindowsSettings">
   <dpiAware>True</dpiAware>
  </asmv3:windowsSettings>
  <asmv3:windowsSettings xmlns="http://schemas.microsoft.com/SMI/2016/WindowsSettings">
   
   <longPathAware>false</longPathAware>
   
  </asmv3:windowsSettings>
 </asmv3:application>
</assembly>   0   �� M A I N I C O N                              �     4   ��
 S E C T I O N - A 0                   {
  Description: ADimPas library.

  Copyright (C) 2023 Melchiorre Caruso <melchiorrecaruso@gmail.com>
    
  This library is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
} �  4   ��
 S E C T I O N - A 1                   unit ADim;

{$H+}{$modeSwitch advancedRecords}  
{$WARN NO_RETVAL OFF}

interface

uses SysUtils;

type
  { TQuantity }
  generic TQuantity<U> = record
    type TSelf = specialize TQuantity<U>;
  private
    Value: double;
  public
    function Abs: TSelf;
    function ToString: string;
    function ToString(Precision, Digits: longint): string;
    function ToVerboseString: string;
    function ToVerboseString(Precision, Digits: longint): string;      
    class operator +  (const ALeft, ARight: TSelf): TSelf;
    class operator -  (const ALeft, ARight: TSelf): TSelf;
    class operator *  (const AValue: double; const ASelf: TSelf): TSelf;
    class operator *  (const ASelf: TSelf; const AValue: double): TSelf;
    class operator /  (const ASelf: TSelf; const AValue: double): TSelf;
    class operator /  (const ALeft, ARight: TSelf): double;
    class operator mod(const ALeft, ARight: TSelf): TSelf;    
    class operator =  (const ALeft, ARight: TSelf): boolean;
    class operator <  (const ALeft, ARight: TSelf): boolean;
    class operator >  (const ALeft, ARight: TSelf): boolean;
    class operator <= (const ALeft, ARight: TSelf): boolean;
    class operator >= (const ALeft, ARight: TSelf): boolean;
  end;

  { TUnitId }
  generic TUnitId<U> = record
    type TSelf = specialize TUnitId<U>;
    type TBaseQuantity = specialize TQuantity<U>;
  public
    class function From(const AQuantity: TBaseQuantity): TBaseQuantity; inline; static;
    class operator *(const AValue: double; const {%H-}ASelf: TSelf): TBaseQuantity;
  end;

  { TFactoredUnitId }
  generic TFactoredUnitId<BaseU, U> = record
    type TSelf = specialize TFactoredUnitId<BaseU, U>;
    type TBaseQuantity = specialize TQuantity<BaseU>;
    type TFactoredQuantity = specialize TQuantity<U>;
  public
    class function From(const AQuantity: TBaseQuantity): TFactoredQuantity; inline; static;
  end;
   4   ��
 S E C T I O N - A 4                   { Trigonometric functions } 

function Cos(const AQuantity: TRadians): double;
function Sin(const AQuantity: TRadians): double;
function Tan(const AQuantity: TRadians): double;
function Cotan(const AQuantity: TRadians): double;
function Secant(const AQuantity: TRadians): double;
function Cosecant(const AQuantity: TRadians): double;

function ArcCos(const AValue: double): TRadians;
function ArcSin(const AValue: double): TRadians;
function ArcTan(const AValue: double): TRadians;
function ArcTan2(const x, y: double): TRadians;
 I
  4   ��
 S E C T I O N - B 1                   implementation

uses Math;

{ TQuantity }

function TQuantity.Abs: TSelf;
begin
  result.Value := System.Abs(Value);
end;

function TQuantity.ToString: string;
begin
  result := FloatToStr(Value) + ' ' + U.Symbol;
end;

function TQuantity.ToString(Precision, Digits: longint): string;
begin
  result := FloatToStrF(Value, ffGeneral, Precision, Digits)  + ' ' + U.Symbol;
end;

function TQuantity.ToVerboseString: string;
begin
  result := FloatToStr(Value) + ' ' + U.Name;
end;

function TQuantity.ToVerboseString(Precision, Digits: longint): string;
begin
  result := FloatToStrF(Value, ffGeneral, Precision, Digits)  + ' ' + U.Name;
end; 

class operator TQuantity.+(const ALeft, ARight: TSelf): TSelf;
begin
  result.Value := ALeft.Value + ARight.Value;
end;

class operator TQuantity.-(const ALeft, ARight: TSelf): TSelf;
begin
  result.Value := ALeft.Value - ARight.Value;
end;

class operator TQuantity.*(const AValue: double; const ASelf: TSelf): TSelf;
begin
  result.Value := AValue * ASelf.Value;
end;

class operator TQuantity.*(const ASelf: TSelf; const AValue: double): TSelf;
begin
  result.Value := ASelf.Value * AValue;
end;

class operator TQuantity./(const ASelf: TSelf; const AValue: double): TSelf;
begin
  result.Value := ASelf.Value / AValue;
end;

class operator TQuantity./(const ALeft, ARight: TSelf): double;
begin
  result := ALeft.Value / ARight.Value;
end;

class operator TQuantity.mod(const ALeft, ARight: TSelf): TSelf;
begin
  result.Value := ALeft.Value mod ARight.Value;
end;

class operator TQuantity.=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value = ARight.Value;
end;

class operator TQuantity.<(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value < ARight.Value;
end;

class operator TQuantity.>(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value > ARight.Value;
end;

class operator TQuantity.<=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value <= ARight.Value;
end;

class operator TQuantity.>=(const ALeft, ARight: TSelf): boolean;
begin
  result := ALeft.Value >= ARight.Value;
end;

{ TUnitId }

class function TUnitId.From(const AQuantity: TBaseQuantity): TBaseQuantity;
begin
  result.Value := AQuantity.Value;
end;

class operator TUnitId.*(const AValue: double; const ASelf: TSelf): TBaseQuantity;
begin
  result.Value := AValue;
end;

{ TFactoredUnitId }

class function TFactoredUnitId.From(const AQuantity: TBaseQuantity): TFactoredQuantity;
begin
  result.Value := AQuantity.Value / U.Factor;
end;
   P  4   ��
 S E C T I O N - B 4                   { Trigonometric functions } 

function Cos(const AQuantity: TRadians): double;
begin
  result := System.Cos(AQuantity.Value);
end;

function Sin(const AQuantity: TRadians): double;
begin
  result := System.Sin(AQuantity.Value);
end;

function Tan(const AQuantity: TRadians): double;
begin
  result := Math.Tan(AQuantity.Value);
end;

function Cotan(const AQuantity: TRadians): double;
begin
  result := Math.Cotan(AQuantity.Value);
end;

function Secant(const AQuantity: TRadians): double;
begin
  result := Math.Secant(AQuantity.Value);
end;

function Cosecant(const AQuantity: TRadians): double;
begin
  result := Math.Cosecant(AQuantity.Value);
end;

function ArcCos(const AValue: double): TRadians;
begin
  result.Value := Math.ArcCos(AValue);
end;

function ArcSin(const AValue: double): TRadians;
begin
  result.Value := Math.ArcSin(AValue);
end;

function ArcTan(const AValue: double): TRadians;
begin
  result.Value := System.ArcTan(AValue);
end;

function ArcTan2(const x, y: double): TRadians;
begin
  result.Value := Math.ArcTan2(x, y);
end;
�      �� ��               (       @             d   d                                                                                                                                                                                                                                                                                           Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu����������ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ�������������Ӥu�                                    Ӥu����������ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ��ݻ�������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu����������ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ�������������Ӥu�                                    Ӥu����������ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ��ԧ�������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu�������������������������������������������������������������������������������������Ӥu�                                    Ӥu���������������������������������������������������������������������������������޻��ը|�                                    Ӥu�����������������������������������������������������������������������������޼��֨{�ےm                                    Ӥu�������������������������������������������������������������������������޼��֨{�ےm                                        Ӥu���������������������������������������������������������������������޼��֨{�ےm                                            Ӥu�����������������������������������������������������������������޽��֨|�ےm                                                Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�Ӥu�֩}�ժ�                                                                                                                                                                                                                                                                                                                                                                                                                                    