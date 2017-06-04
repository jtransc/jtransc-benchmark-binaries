// JTransc 0.6.1 : https://github.com/jtransc/jtransc

import 'dart:typed_data';
import 'dart:math' as Math;
import 'dart:io';
//import 'dart:developer' as debugger;


main() {
	Bootstrap.Main(new List(0));
}

// https://www.dartlang.org/articles/dart-vm/numeric-computation
class N {
	static final int MIN_INT32 = -2147483648;
	static final int MAX_INT32 = 2147483647;

	static final Int64 MIN_INT64 = N.lnew(-9223372036854775808);
	static final Int64 MAX_INT64 = N.lnew(9223372036854775807);

	static final double DOUBLE_NAN = longBitsToDouble(N.lnew(0x7FF8000000000000));

	static final Int8List _tempI8 = new Int8List(8);
	static final Float32List _tempF32 = _tempI8.buffer.asFloat32List();
	static final Int32List _tempI32 = _tempI8.buffer.asInt32List();
	static final Float64List _tempF64 = _tempI8.buffer.asFloat64List();
	static final Int64List _tempI64 = _tempI8.buffer.asInt64List();

	static void init() {
	}

	// FAILS
	//static int  I(int v) { _tempI32[0] = v.toInt(); return _tempI32[0]; }
	//static int  L(int v) { _tempI64[0] = v.toInt(); return _tempI64[0]; }
	//static double F(int v) { _tempF32[0] = v.toDouble(); return _tempF32[0]; }
	//static double D(int v) { _tempF64[0] = v.toDouble(); return _tempF64[0]; }

	//static int  I(int v) { Int32List   out = new Int32List(1)  ; out[0] = v.toInt(); return out[0]; }
	//static int  L(int v) { Int64List   out = new Int64List(1)  ; out[0] = v.toInt(); return out[0]; }
	//static double F(int v) { Float32List out = new Float32List(1); out[0] = v.toDouble(); return out[0]; }
	//static double D(int v) { Float64List out = new Float64List(1); out[0] = v.toDouble(); return out[0]; }

	// https://github.com/dart-lang/fixnum/blob/master/lib/src/int.dart
	static int  I(int v) { return (v & 0x7fffffff) - (v & 0x80000000); }
	static double F(int v) { return v.toDouble(); }
	static double D(int v) { return v.toDouble(); }

	static int ineg(int r) {
		if (r == MIN_INT32) return MIN_INT32;
		return I(-r);
	}

	//static CHECK_CAST(i, clazz) {
	//	if (i == null) return null;
	//	if (!(i is clazz)) {
	//		throw new WrappedThrowable((new java_lang_ClassCastException()).java_lang_ClassCastException_init___V());
	//	}
	//	return i;
	//}

	static getJavaException(ee) {
		if (ee is WrappedThrowable) return ee.t;
		if (ee is CastError) return (new java_lang_ClassCastException()).java_lang_ClassCastException_init___V();
		return ee;
		//return new WrappedThrowable((new java_lang_ClassCastException()).java_lang_ClassCastException_init___V());
	}


	static int iadd(int l, int r) { return I(l + r); }
	static int isub(int l, int r) { return I(l - r); }
	static int imul(int l, int r) { return I(l * r); }
	static int idiv(int l, int r) { return I(l ~/ r); }
	static int irem(int l, int r) { return I(l.remainder(r)); }
	static int iand(int l, int r) { return I(l & r); }
	static int ixor(int l, int r) { return I(l ^ r); }
	static int ior(int l, int r) { return I(l | r); }

	static int FIXSHIFT(int r) {
		if (r < 0) {
			return (32 - ((-r) & 0x1F)) & 0x1F;
		} else {
			return r & 0x1F;
		}
	}

	static int LFIXSHIFT(int r) {
		if (r < 0) {
			return (64 - ((-r) & 0x3F)) & 0x3F;
		} else {
			return r & 0x3F;
		}
	}

	static int ishl(int l, int r) { return I(l << FIXSHIFT(r)); }
	static int ishr(int l, int r) { return I(l >> FIXSHIFT(r)); }
	static int iushr(int l, int r) { return I((l & 0xffffffff) >> FIXSHIFT(r)); }

	static int ishl_opt(int l, int r) { return I(l << r); }
	static int ishr_opt(int l, int r) { return I(l >> r); }
	static int iushr_opt(int l, int r) { return I((l & 0xffffffff) >> r); }

	static void fillSecureRandomBytes(Int8List data) {
		var random = new Math.Random.secure();
		for (var n = 0; n < data.length; n++) data[n] = random.nextInt(0xFF);
	}

	//static int lnew(int high, int low) { return (high << 32) | low; }

	static double i2f(int v) { return v.toDouble(); }
	static double i2d(int v) { return v.toDouble(); }
	static int floatToIntBits(double v) { _tempF32[0] = v; return _tempI32[0]; }
	static double intBitsToFloat(int v) { _tempI32[0] = v; return _tempF32[0]; }

	// LONG related

	static int inew(int v) { return v; }

	static Int64  lnew(int v) { return new Int64(v); }
	static Int64  lneg(int v) { return -v; }
	static Int64  ladd(Int64 l, Int64 r) { return (l + r); }
	static Int64  lsub(Int64 l, Int64 r) { return (l - r); }
	static Int64  lmul(Int64 l, Int64 r) { return (l * r); }
	static Int64  lrem(Int64 l, Int64 r) { return (l.remainder(r)); }
	static Int64  ldiv(Int64 l, Int64 r) { return (l ~/ r); }
	static Int64  lxor(Int64 l, Int64 r) { return (l ^ r); }
	static Int64  lor (Int64 l, Int64 r) { return (l | r); }
	static Int64  land(Int64 l, Int64 r) { return (l & r); }
	static Int64  lshl(Int64 l, int r) { return (l << LFIXSHIFT(r)); }
	static Int64  lshr(Int64 l, int r) { return (l >> LFIXSHIFT(r)); }
	static Int64  lushr(Int64 l, int r) { return l.shiftRightUnsigned(LFIXSHIFT(r)); }
	static Int64  lushr_opt(Int64 l, int r) { return l.shiftRightUnsigned(r); }
	static int    lcmp(Int64 l, Int64 r) { return l.compareTo(r); }
	static double j2f(Int64  v) { return v.toDouble(); }
	static double j2d(Int64  v) { return v.toDouble(); }
	static int    j2i(Int64  v) { return v.toInt32_v(); }
	static int    i2j(int    v) { return new Int64(v.toInt()); }
	static int    d2j(double v) { return new Int64(v.toInt()); }
	static int    f2j(double v) { return new Int64(v.toInt()); }

	static Int64 doubleToLongBits(double v) { _tempF64[0] = v.toDouble(); return new Int64(_tempI64[0]); }
	static double longBitsToDouble(Int64 v) { _tempI64[0] = v.toInt(); return _tempF64[0]; }

	// DOUBLE

	static int cmp (double a, double b) { return (a < b) ? (-1) : ((a > b) ? (1) : 0); }
	static int cmpl(double a, double b) { return (a.isNaN || b.isNaN) ? (-1) : N.cmp(a, b); }
	static int cmpg(double a, double b) { return (a.isNaN || b.isNaN) ? (1) : N.cmp(a, b); }

	static int z2i(bool v)   { return v ? 1 : 0; }

	static int i(int v) { return (v.toInt()); }
	static int f2i(double v) { if (v.isNaN || !v.isFinite) return 0; return I(v.toInt()); }
	static int d2i(double v) { if (v.isNaN || !v.isFinite) return 0; return I(v.toInt()); }

	static int i2b(int v) { return (v & 0x7F) - (v & 0x80); }
	static int i2s(int v) { return (v & 0x7FFF) - (v & 0x8000); }
	static int i2c(int v) { return (v & 0xFFFF); }

	static String charArrayToString(JA_C array) {
		return new String.fromCharCodes(array.data);
	}

	static String ichar(int v) {
		return new String.fromCharCode(v);
	}

	static JA_C stringToCharArray(String str) {
		var out = new JA_C(str.length);
		for (var n = 0; n < str.length; n++) out.data[n] = str.codeUnitAt(n);
		return out;
	}

	static java_lang_String str(String str) {
		if (str == null) return null;
		var out = new java_lang_String();
		out._str = str;
		return out;
	}

	static java_lang_String strLitEscape(String str) { return N.str(str); }
	static String istr(java_lang_String o) { return (o != null) ? o._str : null; }

	static JA_L strArray(List<String> strs) {
		int len = strs.length;
		JA_L o = new JA_L(len, "[Ljava/lang/String;");
		for (int n = 0; n < len; n++) o.data[n] = N.str(strs[n]);
		return o;
	}

	static java_lang_RuntimeException runtimeException(String msg) {
		print("runtimeException: '$msg'");
		return (new java_lang_RuntimeException()).java_lang_RuntimeException_init__Ljava_lang_String__V(N.str(msg));
	}

	static java_lang_Class resolveClass(String name) {
		return java_lang_Class.forName_Ljava_lang_String__Ljava_lang_Class_(N.str(name));
	}

	static completeFuture(com_jtransc_async_JTranscAsyncHandler handler, callback) {
		(() async {
			var result;
			try {
				result = await callback();
			} catch (e) {
				handler.complete_Ljava_lang_Object_Ljava_lang_Throwable__V(null, N.runtimeException("$e"));
				return;
			}
			handler.complete_Ljava_lang_Object_Ljava_lang_Throwable__V(result, null);
		})();
	}

	static void monitorEnter(java_lang_Object o) {
	}

	static void monitorExit(java_lang_Object o) {
	}

	static bool   unboxBool  (java_lang_Boolean   i) { return i.booleanValue__Z(); }
	static int    unboxByte  (java_lang_Byte      i) { return i.byteValue__B(); }
	static int    unboxShort (java_lang_Short     i) { return i.shortValue__S(); }
	static int    unboxChar  (java_lang_Character i) { return i.charValue__C(); }
	static int    unboxInt   (java_lang_Integer   i) { return i.intValue__I(); }
	static int    unboxLong  (java_lang_Long      i) { return i.longValue__J(); }
	static double unboxFloat (java_lang_Float     i) { return i.floatValue__F(); }
	static double unboxDouble(java_lang_Double    i) { return i.doubleValue__D(); }

	static java_lang_Object    boxVoid  (        ) { return null; }
	static java_lang_Boolean   boxBool  (bool   v) { return java_lang_Boolean.valueOf_Z_Ljava_lang_Boolean_(v); }
	static java_lang_Byte      boxByte  (int    v) { return java_lang_Byte.valueOf_B_Ljava_lang_Byte_(v); }
	static java_lang_Short     boxShort (int    v) { return java_lang_Short.valueOf_S_Ljava_lang_Short_(v); }
	static java_lang_Character boxChar  (int    v) { return java_lang_Character.valueOf_C_Ljava_lang_Character_(v); }
	static java_lang_Integer   boxInt   (int    v) { return java_lang_Integer.valueOf_I_Ljava_lang_Integer_(v); }
	static java_lang_Long      boxLong  (int    v) { return java_lang_Long.valueOf_J_Ljava_lang_Long_(v); }
	static java_lang_Float     boxFloat (double v) { return java_lang_Float.valueOf_F_Ljava_lang_Float_(v); }
	static java_lang_Double    boxDouble(double v) { return java_lang_Double.valueOf_D_Ljava_lang_Double_(v); }

	static void arraycopy(java_lang_Object src, int srcPos, java_lang_Object dest, int destPos, int length) {
		if (src is JA_0) return src.copyTo(dest, srcPos, destPos, length);
		throw new Exception("Not implemented arraycopy for " + src.toString());
	}

	//static JA_L getStackTrace(Error error, int skip) {
	static JA_L getStackTrace(StackTrace st, int skip) {
		//var st = StackTrace.current;
		//var st = error.stackTrace;
		var lines = st.toString().split('\n');
		var o = new JA_L(lines.length - skip, "[Ljava/lang/StackTraceElement;");
		for (var n = 0; n < lines.length; n++) {
			var line = lines[n];

			// @TODO: Parse stacktrace elements
			var clazz = line;
			var method = '';
			var file = '';
			var lineNumber = 0;

			if (n >= skip) {
				o.data[n - skip] = (new java_lang_StackTraceElement()).java_lang_StackTraceElement_init__Ljava_lang_String_Ljava_lang_String_Ljava_lang_String_I_V(
					N.str(clazz), N.str(method), N.str(file), lineNumber
				);
			}
		}
		return o;
	}
}

abstract class JA_0 extends java_lang_Object {
	int length;
	String desc;

	JA_0(int length, String desc) {
		this.length = length;
		this.desc = desc;
	}

	void setArraySlice(int index, List data);
	void copyTo(JA_0 dest, int srcPos, int destPos, int length);
}

class JA_B extends JA_0 {
	Int8List data;
	JA_B(int length, [String desc = '[B']) : super(length, desc) { data = new Int8List(length); }
	void setArraySlice(int index, List data) {
		for (var n = 0; n < data.length; n++) this.data[index + n] = data[n];
	}
	void copyTo(JA_0 dest, int srcPos, int destPos, int length) {
		(dest as JA_B).data.setRange(destPos, destPos + length, this.data, srcPos);
	}
}

class JA_Z extends JA_B {
	JA_Z(int length, [String desc = '[Z']) : super(length, desc) { }
}

class JA_C extends JA_0 {
	Int32List data;
	JA_C(int length) : super(length, '[C') { data = new Int32List(length); }
	void setArraySlice(int index, List data) {
		for (var n = 0; n < data.length; n++) this.data[index + n] = data[n];
	}
	void copyTo(JA_0 dest, int srcPos, int destPos, int length) {
		(dest as JA_C).data.setRange(destPos, destPos + length, this.data, srcPos);
	}
}

class JA_S extends JA_0 {
	Int16List data;
	JA_S(int length) : super(length, '[S') { data = new Int16List(length); }
	void setArraySlice(int index, List data) {
		for (var n = 0; n < data.length; n++) this.data[index + n] = data[n];
	}
	void copyTo(JA_0 dest, int srcPos, int destPos, int length) {
		(dest as JA_S).data.setRange(destPos, destPos + length, this.data, srcPos);
	}
}

class JA_I extends JA_0 {
	Int32List data;
	JA_I(int length) : super(length, '[I') { data = new Int32List(length); }
	static JA_I T(List<int> values) {
		var out = new JA_I(values.length);
		for (var n = 0; n < out.length; n++) out.data[n] = values[n];
		return out;
	}
	void setArraySlice(int index, List data) {
		for (var n = 0; n < data.length; n++) this.data[index + n] = data[n];
	}
	void copyTo(JA_0 dest, int srcPos, int destPos, int length) {
		(dest as JA_I).data.setRange(destPos, destPos + length, this.data, srcPos);
	}
}

class JA_F extends JA_0 {
	Float32List data;
	JA_F(int length) : super(length, '[F') { data = new Float32List(length); }
	void setArraySlice(int index, List data) {
		for (var n = 0; n < data.length; n++) this.data[index + n] = data[n];
	}
	void copyTo(JA_0 dest, int srcPos, int destPos, int length) {
		(dest as JA_F).data.setRange(destPos, destPos + length, this.data, srcPos);
	}
}

class JA_D extends JA_0 {
	Float64List data;
	JA_D(int length) : super(length, '[D') { data = new Float64List(length); }
	void setArraySlice(int index, List data) {
		for (var n = 0; n < data.length; n++) this.data[index + n] = data[n];
	}
	void copyTo(JA_0 dest, int srcPos, int destPos, int length) {
		(dest as JA_D).data.setRange(destPos, destPos + length, this.data, srcPos);
	}
}

class JA_J extends JA_0 {
	List data;
	JA_J(int length) : super(length, '[J') { data = new List.filled(length, Int64.ZERO); }
	void setArraySlice(int index, List<Int64> data) {
		for (var n = 0; n < data.length; n++) this.data[index + n] = data[n];
	}
	void copyTo(JA_0 dest, int srcPos, int destPos, int length) {
		(dest as JA_J).data.setRange(destPos, destPos + length, this.data, srcPos);
	}
}

class JA_L extends JA_0 {
	List data;
	JA_L(int length, String desc) : super(length, desc) { data = new List.filled(length, null); }

	static JA_0 createMultiSure(String desc, List<int> sizes) {
		return _createMultiSure(desc, 0, sizes);
	}

	static JA_0 _createMultiSure(String desc, int index, List<int> sizes) {
		if (!desc.startsWith("[")) return null;
		if (index >= sizes.length - 1) return JA_L.create(sizes[index], desc);
		int len = sizes[index];
		JA_L o = new JA_L(len, desc);
		String desc2 = desc.substring(1);
		for (int n = 0; n < len; n++) {
			o.data[n] = JA_L._createMultiSure(desc2, index + 1, sizes);
		}
		return o;
	}

	static create(int size, String desc) {
		switch (desc) {
			case "[Z": return new JA_Z(size);
			case "[B": return new JA_B(size);
			case "[C": return new JA_C(size);
			case "[S": return new JA_S(size);
			case "[I": return new JA_I(size);
			case "[J": return new JA_J(size);
			case "[F": return new JA_F(size);
			case "[D": return new JA_D(size);
			default: return new JA_L(size, desc);
		}
	}

	void setArraySlice(int index, List data) {
		for (var n = 0; n < data.length; n++) this.data[index + n] = data[n];
	}

	void copyTo(JA_0 dest, int srcPos, int destPos, int length) {
		(dest as JA_L).data.setRange(destPos, destPos + length, this.data, srcPos);
	}
}

class WrappedThrowable extends Error {
	java_lang_Throwable t;

	WrappedThrowable(java_lang_Throwable t) {
		this.t = t;
	}

	String toString() {
		return t.toString();
	}
}

// https://github.com/dart-lang/fixnum/tree/2d95f7d21690be6f077128f6bd5c29d875f71fee/lib/src

abstract class IntX implements Comparable<dynamic> {
	IntX operator +(other);
	IntX operator -(other);
	IntX operator -();
	IntX operator *(other);
	IntX operator %(other);
	IntX operator ~/(other);
	IntX remainder(other);
	IntX operator &(other);
	IntX operator |(other);
	IntX operator ^(other);
	IntX operator ~();
	IntX operator <<(int shiftAmount);
	IntX operator >>(int shiftAmount);
	IntX shiftRightUnsigned(int shiftAmount);
	int compareTo(other);
	bool operator ==(other);
	bool operator <(other);
	bool operator <=(other);
	bool operator >(other);
	bool operator >=(other);
	bool get isEven;
	bool get isMaxValue;
	bool get isMinValue;
	bool get isNegative;
	bool get isOdd;
	bool get isZero;
	int get hashCode;
	IntX abs();
	IntX clamp(lowerLimit, upperLimit);
	int get bitLength;
	int numberOfLeadingZeros();
	int numberOfTrailingZeros();
	IntX toSigned(int width);
	IntX toUnsigned(int width);
	List<int> toBytes();
	double toDouble();
	int toInt();
	Int32 toInt32();
	int toInt32_v();
	Int64 toInt64();
	String toString();
	String toHexString();
	String toRadixString(int radix);
}

class Int64 implements IntX {
	final int _l, _m, _h;
	static const int _BITS = 22;
	static const int _BITS01 = 44; // 2 * _BITS
	static const int _BITS2 = 20; // 64 - _BITS01
	static const int _MASK = 4194303; // (1 << _BITS) - 1
	static const int _MASK2 = 1048575; // (1 << _BITS2) - 1
	static const int _SIGN_BIT = 19; // _BITS2 - 1
	static const int _SIGN_BIT_MASK = 1 << _SIGN_BIT;
	static const Int64 MAX_VALUE = const Int64._bits(_MASK, _MASK, _MASK2 >> 1);
	static const Int64 MIN_VALUE = const Int64._bits(0, 0, _SIGN_BIT_MASK);
	static const Int64 ZERO = const Int64._bits(0, 0, 0);
	static const Int64 ONE = const Int64._bits(1, 0, 0);
	static const Int64 TWO = const Int64._bits(2, 0, 0);
	const Int64._bits(int this._l, int this._m, int this._h);
	static Int64 parseRadix(String s, int radix) {
		return _parseRadix(s, Int32._validateRadix(radix));
	}

	static Int64 _parseRadix(String s, int radix) {
		int i = 0;
		bool negative = false;
		if (s[0] == '-') {
			negative = true;
			i++;
		}
		int d0 = 0, d1 = 0, d2 = 0;  //  low, middle, high components.
		for (; i < s.length; i++) {
			int c = s.codeUnitAt(i);
			int digit = Int32._decodeDigit(c);
			if (digit < 0 || digit >= radix) throw new FormatException("Non-radix char code: $c");

			d0 = d0 * radix + digit;
			int carry = d0 >> _BITS;
			d0 = _MASK & d0;

			d1 = d1 * radix + carry;
			carry = d1 >> _BITS;
			d1 = _MASK & d1;

			d2 = d2 * radix + carry;
			d2 = _MASK2 & d2;
		}

		if (negative) return _negate(d0, d1, d2);

		return Int64._masked(d0, d1, d2);
	}

	static Int64 parseInt(String s) => _parseRadix(s, 10);
	static Int64 parseHex(String s) => _parseRadix(s, 16);

	factory Int64([int value=0]) {
		int v0 = 0, v1 = 0, v2 = 0;
		bool negative = false;
		if (value < 0) {
			negative = true;
			value = -value - 1;
		}
		v2 = value ~/ 17592186044416; // 2^44
		value -= v2 * 17592186044416;
		v1 = value ~/ 4194304; // 2^22
		value -= v1 * 4194304;
		v0 = value;

		if (negative) {
			v0 = ~v0;
			v1 = ~v1;
			v2 = ~v2;
		}
		return Int64._masked(v0, v1, v2);
	}

	factory Int64.fromBytes(List<int> bytes) {
		int top = bytes[7] & 0xff;
		top <<= 8;
		top |= bytes[6] & 0xff;
		top <<= 8;
		top |= bytes[5] & 0xff;
		top <<= 8;
		top |= bytes[4] & 0xff;

		int bottom = bytes[3] & 0xff;
		bottom <<= 8;
		bottom |= bytes[2] & 0xff;
		bottom <<= 8;
		bottom |= bytes[1] & 0xff;
		bottom <<= 8;
		bottom |= bytes[0] & 0xff;

		return new Int64.fromInts(top, bottom);
	}

	factory Int64.fromBytesBigEndian(List<int> bytes) {
		int top = bytes[0] & 0xff;
		top <<= 8;
		top |= bytes[1] & 0xff;
		top <<= 8;
		top |= bytes[2] & 0xff;
		top <<= 8;
		top |= bytes[3] & 0xff;

		int bottom = bytes[4] & 0xff;
		bottom <<= 8;
		bottom |= bytes[5] & 0xff;
		bottom <<= 8;
		bottom |= bytes[6] & 0xff;
		bottom <<= 8;
		bottom |= bytes[7] & 0xff;

		return new Int64.fromInts(top, bottom);
	}

	factory Int64.fromInts(int top, int bottom) {
		top &= 0xffffffff;
		bottom &= 0xffffffff;
		int d0 = _MASK & bottom;
		int d1 = ((0xfff & top) << 10) | (0x3ff & (bottom >> _BITS));
		int d2 = _MASK2 & (top >> 12);
		return  Int64._masked(d0, d1, d2);
	}

	static Int64 _promote(value) {
		if (value is Int64) return value;
		if (value is int) return new Int64(value);
		if (value is Int32) return value.toInt64();
		throw new ArgumentError.value(value);
	}

	Int64 operator +(other) {
		Int64 o = _promote(other);
		int sum0 = _l + o._l;
		int sum1 = _m + o._m + (sum0 >> _BITS);
		int sum2 = _h + o._h + (sum1 >> _BITS);
		return Int64._masked(sum0, sum1, sum2);
	}

	Int64 operator -(other) {
		Int64 o = _promote(other);
		return _sub(_l, _m, _h, o._l, o._m, o._h);
	}

	Int64 operator -() => _negate(_l, _m, _h);

	Int64 operator *(other) {
		Int64 o = _promote(other);

		int a0 = _l & 0x1fff;
		int a1 = (_l >> 13) | ((_m & 0xf) << 9);
		int a2 = (_m >> 4) & 0x1fff;
		int a3 = (_m >> 17) | ((_h & 0xff) << 5);
		int a4 = (_h & 0xfff00) >> 8;

		int b0 = o._l & 0x1fff;
		int b1 = (o._l >> 13) | ((o._m & 0xf) << 9);
		int b2 = (o._m >> 4) & 0x1fff;
		int b3 = (o._m >> 17) | ((o._h & 0xff) << 5);
		int b4 = (o._h & 0xfff00) >> 8;

		int p0 = a0 * b0; // << 0
		int p1 = a1 * b0; // << 13
		int p2 = a2 * b0; // << 26
		int p3 = a3 * b0; // << 39
		int p4 = a4 * b0; // << 52

		if (b1 != 0) {
			p1 += a0 * b1;
			p2 += a1 * b1;
			p3 += a2 * b1;
			p4 += a3 * b1;
		}
		if (b2 != 0) {
			p2 += a0 * b2;
			p3 += a1 * b2;
			p4 += a2 * b2;
		}
		if (b3 != 0) {
			p3 += a0 * b3;
			p4 += a1 * b3;
		}
		if (b4 != 0) {
			p4 += a0 * b4;
		}

		int c00 = p0 & 0x3fffff;
		int c01 = (p1 & 0x1ff) << 13;
		int c0 = c00 + c01;

		int c10 = p0 >> 22;
		int c11 = p1 >> 9;
		int c12 = (p2 & 0x3ffff) << 4;
		int c13 = (p3 & 0x1f) << 17;
		int c1 = c10 + c11 + c12 + c13;

		int c22 = p2 >> 18;
		int c23 = p3 >> 5;
		int c24 = (p4 & 0xfff) << 8;
		int c2 = c22 + c23 + c24;

		// Propagate high bits from c0 -> c1, c1 -> c2.
		c1 += c0 >> _BITS;
		c2 += c1 >> _BITS;

		return Int64._masked(c0, c1, c2);
	}

	Int64 operator %(other) => _divide(this, other, _RETURN_MOD);

	Int64 operator ~/(other) => _divide(this, other, _RETURN_DIV);

	Int64 remainder(other) => _divide(this, other, _RETURN_REM);

	Int64 operator &(other) {
		Int64 o = _promote(other);
		int a0 = _l & o._l;
		int a1 = _m & o._m;
		int a2 = _h & o._h;
		return Int64._masked(a0, a1, a2);
	}

	Int64 operator |(other) {
		Int64 o = _promote(other);
		int a0 = _l | o._l;
		int a1 = _m | o._m;
		int a2 = _h | o._h;
		return Int64._masked(a0, a1, a2);
	}

	Int64 operator ^(other) {
		Int64 o = _promote(other);
		int a0 = _l ^ o._l;
		int a1 = _m ^ o._m;
		int a2 = _h ^ o._h;
		return Int64._masked(a0, a1, a2);
	}

	Int64 operator ~() => Int64._masked(~_l, ~_m, ~_h);

	Int64 operator <<(int n) {
		if (n < 0) throw new ArgumentError.value(n);
		n &= 63;

		int res0, res1, res2;
		if (n < _BITS) {
		res0 = _l << n;
		res1 = (_m << n) | (_l >> (_BITS - n));
		res2 = (_h << n) | (_m >> (_BITS - n));
		} else if (n < _BITS01) {
		res0 = 0;
		res1 = _l << (n - _BITS);
		res2 = (_m << (n - _BITS)) | (_l >> (_BITS01 - n));
		} else {
		res0 = 0;
		res1 = 0;
		res2 = _l << (n - _BITS01);
		}

		return Int64._masked(res0, res1, res2);
	}

	Int64 operator >>(int n) {
		if (n < 0) throw new ArgumentError.value(n);
		n &= 63;

		int res0, res1, res2;

		int a2 = _h;
		bool negative = (a2 & _SIGN_BIT_MASK) != 0;
		if (negative && _MASK > _MASK2) a2 += (_MASK - _MASK2);

		if (n < _BITS) {
			res2 = _shiftRight(a2, n);
			if (negative) {
			res2 |= _MASK2 & ~(_MASK2 >> n);
			}
			res1 = _shiftRight(_m, n) | (a2 << (_BITS - n));
			res0 = _shiftRight(_l, n) | (_m << (_BITS - n));
		} else if (n < _BITS01) {
			res2 = negative ? _MASK2 : 0;
			res1 = _shiftRight(a2, n - _BITS);
			if (negative) {
			res1 |= _MASK & ~(_MASK >> (n - _BITS));
			}
			res0 = _shiftRight(_m, n - _BITS) | (a2 << (_BITS01 - n));
		} else {
			res2 = negative ? _MASK2 : 0;
			res1 = negative ? _MASK : 0;
			res0 = _shiftRight(a2, n - _BITS01);
			if (negative) {
				res0 |= _MASK & ~(_MASK >> (n - _BITS01));
			}
		}

		return Int64._masked(res0, res1, res2);
	}

	Int64 shiftRightUnsigned(int n) {
		if (n < 0) throw new ArgumentError.value(n);
		n &= 63;

		int res0, res1, res2;
		int a2 = _MASK2 & _h; // Ensure a2 is positive.
		if (n < _BITS) {
			res2 = a2 >> n;
			res1 = (_m >> n) | (a2 << (_BITS - n));
			res0 = (_l >> n) | (_m << (_BITS - n));
		} else if (n < _BITS01) {
			res2 = 0;
			res1 = a2 >> (n - _BITS);
			res0 = (_m >> (n - _BITS)) | (_h << (_BITS01 - n));
		} else {
			res2 = 0;
			res1 = 0;
			res0 = a2 >> (n - _BITS01);
		}

		return Int64._masked(res0, res1, res2);
	}

	bool operator ==(other) {
		Int64 o;
		if (other is Int64) {
		o = other;
		} else if (other is int) {
		if (_h == 0 && _m == 0) return _l == other;
		if ((_MASK & other) == other) return false;
			o = new Int64(other);
		} else if (other is Int32) {
			o = other.toInt64();
		}
		if (o != null) return _l == o._l && _m == o._m && _h == o._h;
		return false;
	}

	int compareTo(other) => _compareTo(other);

	int _compareTo(other) {
		Int64 o = _promote(other);
		int signa = _h >> (_BITS2 - 1);
		int signb = o._h >> (_BITS2 - 1);
		if (signa != signb) {
		return signa == 0 ? 1 : -1;
		}
		if (_h > o._h) {
		return 1;
		} else if (_h < o._h) {
		return -1;
		}
		if (_m > o._m) {
		return 1;
		} else if (_m < o._m) {
		return -1;
		}
		if (_l > o._l) {
		return 1;
		} else if (_l < o._l) {
		return -1;
		}
		return 0;
	}

	bool operator <(other) => _compareTo(other) < 0;
	bool operator <=(other) => _compareTo(other) <= 0;
	bool operator >(other) => this._compareTo(other) > 0;
	bool operator >=(other) => _compareTo(other) >= 0;

	bool get isEven => (_l & 0x1) == 0;
	bool get isMaxValue => (_h == _MASK2 >> 1) && _m == _MASK && _l == _MASK;
	bool get isMinValue => _h == _SIGN_BIT_MASK && _m == 0 && _l == 0;
	bool get isNegative => (_h & _SIGN_BIT_MASK) != 0;
	bool get isOdd => (_l & 0x1) == 1;
	bool get isZero => _h == 0 && _m == 0 && _l == 0;

	int get bitLength {
		if (isZero) return 0;
		int a0 = _l, a1 = _m, a2 = _h;
		if (isNegative) {
			a0 = _MASK & ~a0;
			a1 = _MASK & ~a1;
			a2 = _MASK2 & ~a2;
		}
		if (a2 != 0) return _BITS01 + a2.bitLength;
		if (a1 != 0) return _BITS + a1.bitLength;
		return a0.bitLength;
	}

	int get hashCode {
		// TODO(sra): Should we ensure that hashCode values match corresponding int?
		// i.e. should `new Int64(x).hashCode == x.hashCode`?
		int bottom = ((_m & 0x3ff) << _BITS) | _l;
		int top = (_h << 12) | ((_m >> 10) & 0xfff);
		return bottom ^ top;
	}

	Int64 abs() => this.isNegative ? -this : this;

	Int64 clamp(lowerLimit, upperLimit) {
		Int64 lower = _promote(lowerLimit);
		Int64 upper = _promote(upperLimit);
		if (this < lower) return lower;
		if (this > upper) return upper;
		return this;
	}

	int numberOfLeadingZeros() {
		int b2 = Int32._numberOfLeadingZeros(_h);
		if (b2 == 32) {
			int b1 = Int32._numberOfLeadingZeros(_m);
			if (b1 == 32) {
				return Int32._numberOfLeadingZeros(_l) + 32;
			} else {
				return b1 + _BITS2 - (32 - _BITS);
			}
		} else {
			return b2 - (32 - _BITS2);
		}
	}

	int numberOfTrailingZeros() {
		int zeros = Int32._numberOfTrailingZeros(_l);
		if (zeros < 32) return zeros;

		zeros = Int32._numberOfTrailingZeros(_m);
		if (zeros < 32) return _BITS + zeros;

		zeros = Int32._numberOfTrailingZeros(_h);
		if (zeros < 32) return _BITS01 + zeros;

		return 64;
	}

	Int64 toSigned(int width) {
		if (width < 1 || width > 64) throw new RangeError.range(width, 1, 64);
		if (width > _BITS01) {
		return Int64._masked(_l, _m, _h.toSigned(width - _BITS01));
		} else if (width > _BITS) {
		int m = _m.toSigned(width - _BITS);
		return m.isNegative
		? Int64._masked(_l, m, _MASK2)
		: Int64._masked(_l, m, 0);  // Masking for type inferrer.
		} else {
		int l = _l.toSigned(width);
		return l.isNegative
		? Int64._masked(l, _MASK, _MASK2)
		: Int64._masked(l, 0, 0);  // Masking for type inferrer.
		}
	}

	Int64 toUnsigned(int width) {
		if (width < 0 || width > 64) throw new RangeError.range(width, 0, 64);
		if (width > _BITS01) {
			int h = _h.toUnsigned(width - _BITS01);
			return Int64._masked(_l, _m, h);
		}
		if (width > _BITS) {
			int m = _m.toUnsigned(width - _BITS);
			return Int64._masked(_l, m, 0);
		}

		int l = _l.toUnsigned(width);
		return Int64._masked(l, 0, 0);
	}

	List<int> toBytes() {
		List<int> result = new List<int>(8);
		result[0] = _l & 0xff;
		result[1] = (_l >> 8) & 0xff;
		result[2] = ((_m << 6) & 0xfc) | ((_l >> 16) & 0x3f);
		result[3] = (_m >> 2) & 0xff;
		result[4] = (_m >> 10) & 0xff;
		result[5] = ((_h << 4) & 0xf0) | ((_m >> 18) & 0xf);
		result[6] = (_h >> 4) & 0xff;
		result[7] = (_h >> 12) & 0xff;
		return result;
	}

	double toDouble() => toInt().toDouble();

	int toInt() {
		int l = _l;
		int m = _m;
		int h = _h;
		if ((_h & _SIGN_BIT_MASK) != 0) {
			l = _MASK & ~_l;
			m = _MASK & ~_m;
			h = _MASK2 & ~_h;
			return -((1 + l) + (4194304 * m) + (17592186044416 * h));
		} else {
			return l + (4194304 * m) + (17592186044416 * h);
		}
	}

	Int32 toInt32() => new Int32(((_m & 0x3ff) << _BITS) | _l);
	int toInt32_v() => N.I(((_m & 0x3ff) << _BITS) | _l);
	Int64 toInt64() => this;
	String toString() => _toRadixString(10);

	// TODO(rice) - Make this faster by avoiding arithmetic.
	String toHexString() {
		if (isZero) return "0";
		Int64 x = this;
		String hexStr = "";
		while (!x.isZero) {
			int digit = x._l & 0xf;
			hexStr = "${_hexDigit(digit)}$hexStr";
			x = x.shiftRightUnsigned(4);
		}
		return hexStr;
	}

	String toRadixString(int radix) {
		return _toRadixString(Int32._validateRadix(radix));
	}

	String _toRadixString(int radix) {
		int d0 = _l;
		int d1 = _m;
		int d2 = _h;

		if (d0 == 0 && d1 == 0 && d2 == 0) return '0';

		String sign = '';
		if ((d2 & _SIGN_BIT_MASK) != 0) {
			sign = '-';
			d0 = 0 - d0;
			int borrow = (d0 >> _BITS) & 1;
			d0 &= _MASK;
			d1 = 0 - d1 - borrow;
			borrow = (d1 >> _BITS) & 1;
			d1 &= _MASK;
			d2 = 0 - d2 - borrow;
			d2 &= _MASK2;
		}

		int d4 = (d2 << 4) | (d1 >> 18);
		int d3 = (d1 >> 8) & 0x3ff;
		d2 = ((d1 << 2) | (d0 >> 20)) & 0x3ff;
		d1 = (d0 >> 10) & 0x3ff;
		d0 = d0 & 0x3ff;

		int fatRadix = _fatRadixTable[radix];

		String chunk1 = "", chunk2 = "", chunk3 = "";

		while (!(d4 == 0 && d3 == 0)) {
			int q = d4 ~/ fatRadix;
			int r = d4 - q * fatRadix;
			d4 = q;
			d3 += r << 10;

			q = d3 ~/ fatRadix;
			r = d3 - q * fatRadix;
			d3 = q;
			d2 += r << 10;

			q = d2 ~/ fatRadix;
			r = d2 - q * fatRadix;
			d2 = q;
			d1 += r << 10;

			q = d1 ~/ fatRadix;
			r = d1 - q * fatRadix;
			d1 = q;
			d0 += r << 10;

			q = d0 ~/ fatRadix;
			r = d0 - q * fatRadix;
			d0 = q;

			assert(chunk3 == "");
			chunk3 = chunk2;
			chunk2 = chunk1;
			chunk1 = (fatRadix + r).toRadixString(radix).substring(1);
		}
		int residue = (d2 << 20) + (d1 << 10) + d0;
		String leadingDigits = residue == 0 ? '' : residue.toRadixString(radix);
		return '$sign$leadingDigits$chunk1$chunk2$chunk3';
	}

	static const _fatRadixTable = const <int>[
		0,
		0,
		2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2
		* 2,
		3 * 3 * 3 * 3 * 3 * 3 * 3 * 3 * 3 * 3 * 3 * 3,
		4 * 4 * 4 * 4 * 4 * 4 * 4 * 4 * 4 * 4,
		5 * 5 * 5 * 5 * 5 * 5 * 5 * 5,
		6 * 6 * 6 * 6 * 6 * 6 * 6,
		7 * 7 * 7 * 7 * 7 * 7 * 7,
		8 * 8 * 8 * 8 * 8 * 8,
		9 * 9 * 9 * 9 * 9 * 9,
		10 * 10 * 10 * 10 * 10 * 10,
		11 * 11 * 11 * 11 * 11,
		12 * 12 * 12 * 12 * 12,
		13 * 13 * 13 * 13 * 13,
		14 * 14 * 14 * 14 * 14,
		15 * 15 * 15 * 15 * 15,
		16 * 16 * 16 * 16 * 16,
		17 * 17 * 17 * 17,
		18 * 18 * 18 * 18,
		19 * 19 * 19 * 19,
		20 * 20 * 20 * 20,
		21 * 21 * 21 * 21,
		22 * 22 * 22 * 22,
		23 * 23 * 23 * 23,
		24 * 24 * 24 * 24,
		25 * 25 * 25 * 25,
		26 * 26 * 26 * 26,
		27 * 27 * 27 * 27,
		28 * 28 * 28 * 28,
		29 * 29 * 29 * 29,
		30 * 30 * 30 * 30,
		31 * 31 * 31 * 31,
		32 * 32 * 32 * 32,
		33 * 33 * 33,
		34 * 34 * 34,
		35 * 35 * 35,
		36 * 36 * 36
	];

	String toDebugString() => "Int64[_l=$_l, _m=$_m, _h=$_h]";

	static Int64 _masked(int a0, int a1, int a2) => new Int64._bits(_MASK & a0, _MASK & a1, _MASK2 & a2);

	static Int64 _sub(int a0, int a1, int a2, int b0, int b1, int b2) {
		int diff0 = a0 - b0;
		int diff1 = a1 - b1 - ((diff0 >> _BITS) & 1);
		int diff2 = a2 - b2 - ((diff1 >> _BITS) & 1);
		return _masked(diff0, diff1, diff2);
	}

	static Int64 _negate(int b0, int b1, int b2) => _sub(0, 0, 0, b0, b1, b2);

	String _hexDigit(int digit) => "0123456789ABCDEF"[digit];

	// Work around dart2js bugs with negative arguments to '>>' operator.
	static int _shiftRight(int x, int n) {
		if (x >= 0) return x >> n;
		int shifted = x >> n;
		if (shifted >= 0x80000000) shifted -= 4294967296;
		return shifted;
	}

	// Implementation of '~/', '%' and 'remainder'.

	static Int64 _divide(Int64 a, other, int what) {
		Int64 b = _promote(other);
		if (b.isZero) throw new IntegerDivisionByZeroException();
		if (a.isZero) return ZERO;

		bool aNeg = a.isNegative;
		bool bNeg = b.isNegative;
		a = a.abs();
		b = b.abs();

		int a0 = a._l;
		int a1 = a._m;
		int a2 = a._h;

		int b0 = b._l;
		int b1 = b._m;
		int b2 = b._h;
		return _divideHelper(a0, a1, a2, aNeg, b0, b1, b2, bNeg, what);
	}

	static const _RETURN_DIV = 1;
	static const _RETURN_REM = 2;
	static const _RETURN_MOD = 3;

	static _divideHelper(int a0, int a1, int a2, bool aNeg,  int b0, int b1, int b2, bool bNeg,  int what) {
		int q0 = 0, q1 = 0, q2 = 0;  // result Q.
		int r0 = 0, r1 = 0, r2 = 0;  // result R.

		if (b2 == 0 && b1 == 0 && b0 < (1 << (30 - _BITS))) {
			q2 = a2 ~/ b0;
			int carry = a2 - q2 * b0;
			int d1 = a1 + (carry << _BITS);
			q1 = d1 ~/ b0;
			carry = d1 - q1 * b0;
			int d0 = a0 + (carry << _BITS);
			q0 = d0 ~/ b0;
			r0 = d0 - q0 * b0;
		} else {
			const double K2 = 17592186044416.0; // 2^44
			const double K1 = 4194304.0; // 2^22

			// Approximate double values for [a] and [b].
			double ad = a0 + K1 * a1 + K2 * a2;
			double bd = b0 + K1 * b1 + K2 * b2;
			// Approximate quotient.
			double qd = (ad / bd).floorToDouble();

			// Extract components of [qd] using double arithmetic.
			double q2d = (qd / K2).floorToDouble();
			qd = qd - K2 * q2d;
			double q1d = (qd / K1).floorToDouble();
			double q0d = qd - K1 * q1d;
			q2 = q2d.toInt();
			q1 = q1d.toInt();
			q0 = q0d.toInt();

			assert(q0 + K1 * q1 + K2 * q2 == (ad / bd).floorToDouble());
			assert(q2 == 0 || b2 == 0);  // Q and B can't both be big since Q*B <= A.

			// P = Q * B, using doubles to hold intermediates.
			// We don't need all partial sums since Q*B <= A.
			double p0d = q0d * b0;
			double p0carry = (p0d / K1).floorToDouble();
			p0d = p0d - p0carry * K1;
			double p1d = q1d * b0 + q0d * b1 + p0carry;
			double p1carry = (p1d / K1).floorToDouble();
			p1d = p1d - p1carry * K1;
			double p2d = q2d * b0 + q1d * b1 + q0d * b2 + p1carry;
			assert(p2d <= _MASK2);  // No partial sum overflow.

			// R = A - P
			int diff0 = a0 - p0d.toInt();
			int diff1 = a1 - p1d.toInt() - ((diff0 >> _BITS) & 1);
			int diff2 = a2 - p2d.toInt() - ((diff1 >> _BITS) & 1);
			r0 = _MASK & diff0;
			r1 = _MASK & diff1;
			r2 = _MASK2 & diff2;

			while ( r2 >= _SIGN_BIT_MASK || r2 > b2 || (r2 == b2 && (r1 > b1 || (r1 == b1 && r0 >= b0)))) {
				// Direction multiplier for adjustment.
				int m = (r2 & _SIGN_BIT_MASK) == 0 ? 1 : -1;
				// R = R - B  or  R = R + B
				int d0 = r0 - m * b0;
				int d1 = r1 - m * (b1 + ((d0 >> _BITS) & 1));
				int d2 = r2 - m * (b2 + ((d1 >> _BITS) & 1));
				r0 = _MASK & d0;
				r1 = _MASK & d1;
				r2 = _MASK2 & d2;

				// Q = Q + 1  or  Q = Q - 1
				d0 = q0 + m;
				d1 = q1 + m * ((d0 >> _BITS) & 1);
				d2 = q2 + m * ((d1 >> _BITS) & 1);
				q0 = _MASK & d0;
				q1 = _MASK & d1;
				q2 = _MASK2 & d2;
			}
		}

		// 0 <= R < B
		assert(Int64.ZERO <= new Int64._bits(r0, r1, r2));
		assert(r2 < b2 ||  // Handles case where B = -(MIN_VALUE)
		new Int64._bits(r0, r1, r2) < new Int64._bits(b0, b1, b2));

		assert(what == _RETURN_DIV || what == _RETURN_MOD || what == _RETURN_REM);
		if (what == _RETURN_DIV) {
			if (aNeg != bNeg) return _negate(q0, q1, q2);
			return Int64._masked(q0, q1, q2);  // Masking for type inferrer.
		}

		if (!aNeg) {
			return Int64._masked(r0, r1, r2);  // Masking for type inferrer.
		}

		if (what == _RETURN_MOD) {
			if (r0 == 0 && r1 == 0 && r2 == 0) return ZERO;
			return _sub(b0, b1, b2, r0, r1, r2);
		} else {
			return _negate(r0, r1, r2);
		}
	}
}

class Int32 implements IntX {
	static const Int32 MAX_VALUE = const Int32._internal(0x7FFFFFFF);
	static const Int32 MIN_VALUE = const Int32._internal(-0x80000000);
	static const Int32 ZERO = const Int32._internal(0);
	static const Int32 ONE = const Int32._internal(1);
	static const Int32 TWO = const Int32._internal(2);

	static const int _CC_0 = 48; // '0'.codeUnitAt(0)
	static const int _CC_9 = 57; // '9'.codeUnitAt(0)
	static const int _CC_a = 97; // 'a'.codeUnitAt(0)
	static const int _CC_z = 122; // 'z'.codeUnitAt(0)
	static const int _CC_A = 65; // 'A'.codeUnitAt(0)
	static const int _CC_Z = 90; // 'Z'.codeUnitAt(0)

	static int _decodeDigit(int c) {
		if (c >= _CC_0 && c <= _CC_9) return c - _CC_0;
		if (c >= _CC_a && c <= _CC_z) return c - _CC_a + 10;
		if (c >= _CC_A && c <= _CC_Z) return c - _CC_A + 10;
		return -1; // bad char code
	}

	static int _validateRadix(int radix) {
		if (2 <= radix && radix <= 36) return radix;
		throw new RangeError.range(radix, 2, 36, 'radix');
	}

	// TODO(rice) - Make this faster by converting several digits at once.
	static Int32 parseRadix(String s, int radix) {
		_validateRadix(radix);
		Int32 x = ZERO;
		for (int i = 0; i < s.length; i++) {
			int c = s.codeUnitAt(i);
			int digit = _decodeDigit(c);
			if (digit < 0 || digit >= radix) throw new FormatException("Non-radix code unit: $c");
			x = (x * radix) + digit;
		}
		return x;
	}

	static Int32 parseInt(String s) => new Int32(int.parse(s));
	static Int32 parseHex(String s) => parseRadix(s, 16);

	// Assumes i is <= 32-bit.
	static int _bitCount(int i) {
		i -= ((i >> 1) & 0x55555555);
		i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
		i = ((i + (i >> 4)) & 0x0F0F0F0F);
		i += (i >> 8);
		i += (i >> 16);
		return (i & 0x0000003F);
	}

	static int _numberOfLeadingZeros(int i) {
		i |= i >> 1;
		i |= i >> 2;
		i |= i >> 4;
		i |= i >> 8;
		i |= i >> 16;
		return _bitCount(~i);
	}

	static int _numberOfTrailingZeros(int i) => _bitCount((i & -i) - 1);

	final int _i;

	const Int32._internal(int i) : _i = i;

	Int32([int i = 0]) : _i = (i & 0x7fffffff) - (i & 0x80000000);

	int _toInt(val) {
		if (val is Int32) return val._i;
		if (val is int) return val;
		throw new ArgumentError(val);
	}

	IntX operator +(other) {
		if (other is Int64) return this.toInt64() + other;
		return new Int32(_i + _toInt(other));
	}

	IntX operator -(other) {
		if (other is Int64) return this.toInt64() - other;
		return new Int32(_i - _toInt(other));
	}

	Int32 operator -() => new Int32(-_i);

	IntX operator *(other) {
		if (other is Int64) return this.toInt64() * other;
		// TODO(rice) - optimize
		return (this.toInt64() * other).toInt32();
	}

	Int32 operator %(other) {
		// Result will be Int32
		if (other is Int64) return (this.toInt64() % other).toInt32();
		return new Int32(_i % _toInt(other));
	}

	Int32 operator ~/(other) {
		if (other is Int64) return (this.toInt64() ~/ other).toInt32();
		return new Int32(_i ~/ _toInt(other));
	}

	Int32 remainder(other) {
		if (other is Int64) {
			Int64 t = this.toInt64();
			return (t - (t ~/ other) * other).toInt32();
		}
		return this - (this ~/ other) * other;
	}

	Int32 operator &(other) {
		if (other is Int64) return (this.toInt64() & other).toInt32();
		return new Int32(_i & _toInt(other));
	}

	Int32 operator |(other) {
		if (other is Int64) return (this.toInt64() | other).toInt32();
		return new Int32(_i | _toInt(other));
	}

	Int32 operator ^(other) {
		if (other is Int64) return (this.toInt64() ^ other).toInt32();
		return new Int32(_i ^ _toInt(other));
	}

	Int32 operator ~() => new Int32(~_i);

	Int32 operator <<(int n) {
		if (n < 0) throw new ArgumentError(n);
		n &= 31;
		return new Int32(_i << n);
	}

	Int32 operator >>(int n) {
		if (n < 0) throw new ArgumentError(n);
		n &= 31;
		int value;
		if (_i >= 0) {
			value = _i >> n;
		} else {
			value = (_i >> n) | (0xffffffff << (32 - n));
		}
		return new Int32(value);
	}

	Int32 shiftRightUnsigned(int n) {
		if (n < 0) throw new ArgumentError(n);
		n &= 31;
		int value;
		if (_i >= 0) {
			value = _i >> n;
		} else {
			value = (_i >> n) & ((1 << (32 - n)) - 1);
		}
		return new Int32(value);
	}

	bool operator ==(other) {
		if (other is Int32) return _i == other._i;
		if (other is Int64) return this.toInt64() == other;
		if (other is int) return _i == other;
		return false;
	}

	int compareTo(other) {
		if (other is Int64) return this.toInt64().compareTo(other);
		return _i.compareTo(_toInt(other));
	}

	bool operator <(other) {
		if (other is Int64) return this.toInt64() < other;
		return _i < _toInt(other);
	}

	bool operator <=(other) {
		if (other is Int64) return this.toInt64() <= other;
		return _i <= _toInt(other);
	}

	bool operator >(other) {
		if (other is Int64) return this.toInt64() > other;
		return _i > _toInt(other);
	}

	bool operator >=(other) {
		if (other is Int64) return this.toInt64() >= other;
		return _i >= _toInt(other);
	}

	bool get isEven => (_i & 0x1) == 0;
	bool get isMaxValue => _i == 2147483647;
	bool get isMinValue => _i == -2147483648;
	bool get isNegative => _i < 0;
	bool get isOdd => (_i & 0x1) == 1;
	bool get isZero => _i == 0;
	int get bitLength => _i.bitLength;

	int get hashCode => _i;

	Int32 abs() => _i < 0 ? new Int32(-_i) : this;

	Int32 clamp(lowerLimit, upperLimit) {
		if (this < lowerLimit) {
			if (lowerLimit is IntX) return lowerLimit.toInt32();
			if (lowerLimit is int) return new Int32(lowerLimit);
			throw new ArgumentError(lowerLimit);
		}

		if (this > upperLimit) {
			if (upperLimit is IntX) return upperLimit.toInt32();
			if (upperLimit is int) return new Int32(upperLimit);
			throw new ArgumentError(upperLimit);
		}

		return this;
	}

	int numberOfLeadingZeros() => _numberOfLeadingZeros(_i);
	int numberOfTrailingZeros() => _numberOfTrailingZeros(_i);

	Int32 toSigned(int width) {
		if (width < 1 || width > 32) throw new RangeError.range(width, 1, 32);
		return new Int32(_i.toSigned(width));
	}

	Int32 toUnsigned(int width) {
		if (width < 0 || width > 32) throw new RangeError.range(width, 0, 32);
		return new Int32(_i.toUnsigned(width));
	}

	List<int> toBytes() {
		List<int> result = new List<int>(4);
		result[0] = _i & 0xff;
		result[1] = (_i >> 8) & 0xff;
		result[2] = (_i >> 16) & 0xff;
		result[3] = (_i >> 24) & 0xff;
		return result;
	}

	double toDouble() => _i.toDouble();
	int toInt() => _i;
	Int32 toInt32() => this;
	Int64 toInt64() => new Int64(_i);

	String toString() => _i.toString();
	String toHexString() => _i.toRadixString(16);
	String toRadixString(int radix) => _i.toRadixString(radix);

	int toInt32_v() => _i;
}


abstract class java_lang_annotation_Annotation   {

}
class java_lang_annotation_Annotation_IFields {

	static void SI() { }
}
class java_lang_Object   {

	int ___id = 0;
	 java_lang_Object java_lang_Object_init___V() {
		return this;
		return this;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getClass__Ljava_lang_Class_().getName__Ljava_lang_String_()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_0).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_Integer.toHexString_I_Ljava_lang_String_(this.hashCode__I())).toString__Ljava_lang_String_();
	}
	 int hashCode__I() {
		return java_lang_SystemInt.identityHashCode_Ljava_lang_Object__I(this);
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this != p0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 java_lang_Class getClass__Ljava_lang_Class_() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(java_lang_jtransc_JTranscCoreReflection.isArray_Ljava_lang_Object__Z(this))) {
						G = 1;
						continue;
					}
					return java_lang_jtransc_JTranscCoreReflection.getClassByName_Ljava_lang_String__Ljava_lang_Class_(java_lang_jtransc_JTranscCoreReflection.getArrayDescriptor_Ljava_lang_Object__Ljava_lang_String_(this));
				case 1:
					return java_lang_jtransc_JTranscCoreReflection.getClassById_I_Ljava_lang_Class_(java_lang_jtransc_JTranscCoreReflection.getClassId_Ljava_lang_Object__I(this));
				default:
					break;
			}
		}
		return null;
	}
	 java_lang_Object clone__Ljava_lang_Object_() {
		int G = 0;
		java_lang_Object lA2 = null;
		java_lang_Object lA1 = null;
		java_lang_Object lA3 = null;
		java_lang_Object lA6 = null;
		int lI1 = 0;
		int lI4 = 0;
		int lI5 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							if (!(java_lang_jtransc_JTranscCoreReflection.isArray_Ljava_lang_Object__Z(this))) {
								G = 1;
								continue;
							}
							lI1 = java_lang_reflect_Array.getLength_Ljava_lang_Object__I(this);
							lA2 = java_lang_reflect_Array.newInstance_Ljava_lang_Class_I_Ljava_lang_Object_(this.getClass__Ljava_lang_Class_().getComponentType__Ljava_lang_Class_(), lI1);
							java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(this, 0, lA2, 0, lI1);
							return lA2;
						case 1:
							lA1 = this.getClass__Ljava_lang_Class_();
							lA2 = (lA1 as java_lang_Class).newInstance__Ljava_lang_Object_();
							lA3 = (lA1 as java_lang_Class).getDeclaredFields___Ljava_lang_reflect_Field_();
							lI4 = (lA3 as JA_0).length;
							lI5 = 0;
							G = 2;
							continue;
						case 2:
							if (((lI5 >= lI4))) {
								G = 3;
								continue;
							}
							lA6 = ((lA3 as JA_L)).data[lI5];
							(lA6 as java_lang_reflect_Field).set_Ljava_lang_Object_Ljava_lang_Object__V(lA2, (lA6 as java_lang_reflect_Field).get_Ljava_lang_Object__Ljava_lang_Object_(this));
							lI5 = (N.I(lI5 + 1));
							G = 2;
							continue;
						case 3:
							fA0 = lA2;
							G = 4;
							continue;
						case 4: return fA0;
						case 5:
							fA0 = J__exception__;
							lA1 = fA0;
							tA0 = (new java_lang_CloneNotSupportedException());
							fA0 = tA0;
							(tA0 as java_lang_CloneNotSupportedException).java_lang_CloneNotSupportedException_init__Ljava_lang_String__V((lA1 as java_lang_Throwable).toString__Ljava_lang_String_());
							throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
							break;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 4)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 5;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	String toString() {
		return N.istr(toString__Ljava_lang_String_());
	}
	int __DART__CLASS_ID;
	java_lang_Object([int CLASS_ID = 656]) { this.__DART__CLASS_ID = CLASS_ID; }
	static void SI() { }
}
class java_lang_annotation_Annotation_Impl extends java_lang_Object implements java_lang_annotation_Annotation {

	 java_lang_annotation_Annotation_Impl java_lang_annotation_Annotation_Impl_init___V() {
		return this;
	}
	 java_lang_Class annotationType__Ljava_lang_Class_() {
		return N.resolveClass("Ljava/lang/annotation/Annotation;");
	}
	 java_lang_Class getClass__Ljava_lang_Class_() {
		return N.resolveClass("Ljava/lang/annotation/Annotation;");
	}
	 java_lang_String toString__Ljava_lang_String_() {
		return (new java_lang_StringBuilder()).java_lang_StringBuilder_init___V().append_Ljava_lang_Object__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_1).toString__Ljava_lang_String_();
	}
	java_lang_annotation_Annotation_Impl([int CLASS_ID = 932]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_SafeVarargs  implements java_lang_annotation_Annotation {

}
class java_lang_SafeVarargs_IFields {

	static void SI() { }
}
class java_lang_SafeVarargs_Impl extends java_lang_Object implements java_lang_SafeVarargs, java_lang_annotation_Annotation {

	 java_lang_SafeVarargs_Impl java_lang_SafeVarargs_Impl_init___V() {
		return this;
	}
	 java_lang_Class annotationType__Ljava_lang_Class_() {
		return N.resolveClass("Ljava/lang/SafeVarargs;");
	}
	 java_lang_Class getClass__Ljava_lang_Class_() {
		return N.resolveClass("Ljava/lang/SafeVarargs;");
	}
	 java_lang_String toString__Ljava_lang_String_() {
		return (new java_lang_StringBuilder()).java_lang_StringBuilder_init___V().append_Ljava_lang_Object__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_1).toString__Ljava_lang_String_();
	}
	java_lang_SafeVarargs_Impl([int CLASS_ID = 931]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_ref_ReferenceQueue extends java_lang_Object  {

	 java_lang_ref_ReferenceQueue java_lang_ref_ReferenceQueue_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	java_lang_ref_ReferenceQueue([int CLASS_ID = 930]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_ref_Reference extends java_lang_Object  {

	java_lang_Object _referent = null;
	java_lang_ref_ReferenceQueue _queue = null;
	 java_lang_Object get__Ljava_lang_Object_() {
		return this._referent;
	}
	 java_lang_ref_Reference java_lang_ref_Reference_init__Ljava_lang_Object__V(java_lang_Object p0) {
		this.java_lang_ref_Reference_init__Ljava_lang_Object_Ljava_lang_ref_ReferenceQueue__V(p0, null);
		return this;
		return this;
	}
	 java_lang_ref_Reference java_lang_ref_Reference_init__Ljava_lang_Object_Ljava_lang_ref_ReferenceQueue__V(java_lang_Object p0, java_lang_ref_ReferenceQueue p1) {
		this.java_lang_Object_init___V();
		this._referent = p0;
		this._queue = p1;
		return this;
		return this;
	}
	java_lang_ref_Reference([int CLASS_ID = 929]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_ref_WeakReference extends java_lang_ref_Reference  {

	 java_lang_ref_WeakReference java_lang_ref_WeakReference_init__Ljava_lang_Object__V(java_lang_Object p0) {
		this.java_lang_ref_Reference_init__Ljava_lang_Object__V(p0);
		return this;
		return this;
	}
	 java_lang_ref_WeakReference java_lang_ref_WeakReference_init__Ljava_lang_Object_Ljava_lang_ref_ReferenceQueue__V(java_lang_Object p0, java_lang_ref_ReferenceQueue p1) {
		this.java_lang_ref_Reference_init__Ljava_lang_Object_Ljava_lang_ref_ReferenceQueue__V(p0, p1);
		return this;
		return this;
	}
	java_lang_ref_WeakReference([int CLASS_ID = 928]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_Comparable   {

}
class java_lang_Comparable_IFields {

	static void SI() { }
}
class java_io_ObjectStreamField extends java_lang_Object implements java_lang_Comparable {

	java_lang_String _name = null;
	java_lang_Object _type = null;
	 java_lang_String toString__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getClass__Ljava_lang_Class_().getName__Ljava_lang_String_()).append_C_Ljava_lang_StringBuilder_(40).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getName__Ljava_lang_String_()).append_C_Ljava_lang_StringBuilder_(58).append_Ljava_lang_Object__Ljava_lang_StringBuilder_(this.getTypeInternal__Ljava_lang_Class_()).append_C_Ljava_lang_StringBuilder_(41).toString__Ljava_lang_String_();
	}
	 java_lang_String getName__Ljava_lang_String_() {
		return this._name;
	}
	 java_lang_Class getTypeInternal__Ljava_lang_Class_() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(((this._type) is java_lang_ref_WeakReference))) {
						G = 1;
						continue;
					}
					return ((((this._type) as java_lang_ref_WeakReference).get__Ljava_lang_Object_()) as java_lang_Class);
				case 1:
					return ((this._type) as java_lang_Class);
				default:
					break;
			}
		}
		return null;
	}
	 java_io_ObjectStreamField java_io_ObjectStreamField_init__Ljava_lang_String_Ljava_lang_Class__V(java_lang_String p0, java_lang_Class p1) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		while (true) {
			switch (G) {
				case 0:
					this.java_lang_Object_init___V();
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_NullPointerException());
					fA0 = tA0;
					(tA0 as java_lang_NullPointerException).java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_2);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					if (((p1 != null))) {
						G = 2;
						continue;
					}
					tA1 = (new java_lang_NullPointerException());
					fA0 = tA1;
					(tA1 as java_lang_NullPointerException).java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_3);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					this._name = p0;
					fA0 = this;
					tA2 = (new java_lang_ref_WeakReference());
					fA1 = tA2;
					(tA2 as java_lang_ref_WeakReference).java_lang_ref_WeakReference_init__Ljava_lang_Object__V(p1);
					(fA0 as java_io_ObjectStreamField)._type = fA1;
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	java_io_ObjectStreamField([int CLASS_ID = 927]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_Hashtable_HashIterator extends java_lang_Object  {

	int _expectedModCount = 0;
	int _nextIndex = 0;
	java_util_Hashtable _this_0 = null;
	java_util_Hashtable_HashtableEntry _nextEntry = null;
	java_util_Hashtable_HashtableEntry _lastEntryReturned = null;
	 java_util_Hashtable_HashIterator java_util_Hashtable_HashIterator_init__Ljava_util_Hashtable__V(java_util_Hashtable p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		java_lang_Object lA3 = null;
		int fI1 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		int tI1 = 0;
		java_lang_Object tA2 = null;
		while (true) {
			switch (G) {
				case 0:
					this._this_0 = p0;
					this.java_lang_Object_init___V();
					this._expectedModCount = java_util_Hashtable.access_500_Ljava_util_Hashtable__I(this._this_0);
					lA2 = java_util_Hashtable.access_600_Ljava_util_Hashtable___Ljava_util_Hashtable_HashtableEntry_(p0);
					lA3 = null;
					G = 1;
					continue;
				case 1:
					if (((lA3 != null))) {
						G = 2;
						continue;
					}
					if (((this._nextIndex >= (lA2 as JA_0).length))) {
						G = 2;
						continue;
					}
					fA0 = lA2;
					fA1 = this;
					tA2 = fA1;
					tI1 = this._nextIndex;
					fI1 = tI1;
					(tA2 as java_util_Hashtable_HashIterator)._nextIndex = (N.I(tI1 + 1));
					lA3 = ((fA0 as JA_L)).data[fI1];
					G = 1;
					continue;
				case 2:
					this._nextEntry = (lA3 as java_util_Hashtable_HashtableEntry);
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 bool hasNext__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._nextEntry == null))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 java_util_Hashtable_HashtableEntry nextEntry__Ljava_util_Hashtable_HashtableEntry_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA2 = null;
		java_lang_Object lA3 = null;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA4 = null;
		java_lang_Object tA6 = null;
		java_lang_Object tA5 = null;
		int fI1 = 0;
		int tI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((java_util_Hashtable.access_500_Ljava_util_Hashtable__I(this._this_0) == this._expectedModCount))) {
						G = 1;
						continue;
					}
					tA0 = (new java_util_ConcurrentModificationException());
					fA0 = tA0;
					(tA0 as java_util_ConcurrentModificationException).java_util_ConcurrentModificationException_init___V();
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					if (((this._nextEntry != null))) {
						G = 2;
						continue;
					}
					tA1 = (new java_util_NoSuchElementException());
					fA0 = tA1;
					(tA1 as java_util_NoSuchElementException).java_util_NoSuchElementException_init___V();
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					lA1 = this._nextEntry;
					lA2 = java_util_Hashtable.access_600_Ljava_util_Hashtable___Ljava_util_Hashtable_HashtableEntry_(this._this_0);
					lA3 = (lA1 as java_util_Hashtable_HashtableEntry)._next;
					G = 3;
					continue;
				case 3:
					if (((lA3 != null))) {
						G = 4;
						continue;
					}
					if (((this._nextIndex >= (lA2 as JA_0).length))) {
						G = 4;
						continue;
					}
					fA0 = lA2;
					fA1 = this;
					tA4 = fA1;
					tI3 = this._nextIndex;
					fI1 = tI3;
					(tA4 as java_util_Hashtable_HashIterator)._nextIndex = (N.I(tI3 + 1));
					lA3 = ((fA0 as JA_L)).data[fI1];
					G = 3;
					continue;
				case 4:
					this._nextEntry = (lA3 as java_util_Hashtable_HashtableEntry);
					fA0 = this;
					tA6 = fA0;
					tA5 = lA1;
					fA0 = tA5;
					(tA6 as java_util_Hashtable_HashIterator)._lastEntryReturned = (tA5 as java_util_Hashtable_HashtableEntry);
					return (fA0 as java_util_Hashtable_HashtableEntry);
				default:
					break;
			}
		}
		return null;
	}
	java_util_Hashtable_HashIterator([int CLASS_ID = 926]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_Iterator   {

	 java_lang_Object next__Ljava_lang_Object_();
	 bool hasNext__Z();
}
class java_util_Iterator_IFields {

	static void SI() { }
}
class java_util_Hashtable_EntryIterator extends java_util_Hashtable_HashIterator implements java_util_Iterator {

	java_util_Hashtable _this_0_ = null;
	 java_util_Hashtable_EntryIterator java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable__V(java_util_Hashtable p0) {
		this._this_0_ = p0;
		this.java_util_Hashtable_HashIterator_init__Ljava_util_Hashtable__V(p0);
		return this;
		return this;
	}
	 java_lang_Object next__Ljava_lang_Object_() {
		return this.next__Ljava_util_Map_Entry_();
	}
	 java_util_Map_Entry next__Ljava_util_Map_Entry_() {
		return this.nextEntry__Ljava_util_Hashtable_HashtableEntry_();
	}
	 java_util_Hashtable_EntryIterator java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V(java_util_Hashtable p0, java_util_Hashtable_1 p1) {
		this.java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable__V(p0);
		return this;
		return this;
	}
	java_util_Hashtable_EntryIterator([int CLASS_ID = 925]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_Iterable   {

	 java_util_Iterator iterator__Ljava_util_Iterator_();
}
class java_lang_Iterable_IFields {

	static void SI() { }
}
abstract class java_util_Collection  implements java_lang_Iterable {

	 int size__I();
	 bool isEmpty__Z();
	 java_util_Iterator iterator__Ljava_util_Iterator_();
	 int hashCode__I();
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0);
	 bool contains_Ljava_lang_Object__Z(java_lang_Object p0);
	 bool containsAll_Ljava_util_Collection__Z(java_util_Collection p0);
	 bool add_Ljava_lang_Object__Z(java_lang_Object p0);
	 JA_L toArray__Ljava_lang_Object___Ljava_lang_Object_(JA_L p0);
}
class java_util_Collection_IFields {

	static void SI() { }
}
abstract class java_util_AbstractCollection extends java_lang_Object implements java_util_Collection {

	 java_util_AbstractCollection java_util_AbstractCollection_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA3 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(this.isEmpty__Z())) {
						G = 1;
						continue;
					}
					return Bootstrap.STRINGLIT_4;
				case 1:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init__I_V((N.I(this.size__I() * 16)));
					lA1 = fA0;
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(91);
					lA2 = this.iterator__Ljava_util_Iterator_();
					G = 2;
					continue;
				case 2:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					lA3 = lA2.next__Ljava_lang_Object_();
					if (((lA3 == this))) {
						G = 4;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_Object__Ljava_lang_StringBuilder_(lA3);
					G = 5;
					continue;
				case 4:
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_5);
					G = 5;
					continue;
				case 5:
					if (!(lA2.hasNext__Z())) {
						G = 6;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_6);
					G = 6;
					continue;
				case 6:
					G = 2;
					continue;
				case 3:
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(93);
					return (lA1 as java_lang_StringBuilder).toString__Ljava_lang_String_();
				default:
					break;
			}
		}
		return null;
	}
	 int size__I() {
		throw new Exception("Missing body java.util.AbstractCollection.size\u0028\u0029I");
	}
	 bool isEmpty__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this.size__I() != 0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 java_util_Iterator iterator__Ljava_util_Iterator_() {
		throw new Exception("Missing body java.util.AbstractCollection.iterator\u0028\u0029Ljava/util/Iterator;");
	}
	 bool containsAll_Ljava_util_Collection__Z(java_util_Collection p0) {
		int G = 0;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lA2 = p0.iterator__Ljava_util_Iterator_();
					G = 1;
					continue;
				case 1:
					if (!(lA2.hasNext__Z())) {
						G = 2;
						continue;
					}
					if (this.contains_Ljava_lang_Object__Z(lA2.next__Ljava_lang_Object_())) {
						G = 1;
						continue;
					}
					return false;
				case 2:
					return true;
				default:
					break;
			}
		}
		return false;
	}
	 bool contains_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lA2 = this.iterator__Ljava_util_Iterator_();
					if (((p0 == null))) {
						G = 1;
						continue;
					}
					G = 2;
					continue;
				case 2:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z(lA2.next__Ljava_lang_Object_()))) {
						G = 2;
						continue;
					}
					return true;
				case 1:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					if (((lA2.next__Ljava_lang_Object_() != null))) {
						G = 1;
						continue;
					}
					return true;
				case 3:
					return false;
				default:
					break;
			}
		}
		return false;
	}
	 bool add_Ljava_lang_Object__Z(java_lang_Object p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_UnsupportedOperationException());
		fA0 = tA0;
		(tA0 as java_lang_UnsupportedOperationException).java_lang_UnsupportedOperationException_init___V();
		throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
	}
	 JA_L toArray__Ljava_lang_Object___Ljava_lang_Object_(JA_L p0) {
		return this.toArrayList__Ljava_util_ArrayList_().toArray__Ljava_lang_Object___Ljava_lang_Object_(p0);
	}
	 java_util_ArrayList toArrayList__Ljava_util_ArrayList_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA3 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_util_ArrayList());
					fA0 = tA0;
					(tA0 as java_util_ArrayList).java_util_ArrayList_init__I_V(this.size__I());
					lA1 = fA0;
					lA2 = this.iterator__Ljava_util_Iterator_();
					G = 1;
					continue;
				case 1:
					if (!(lA2.hasNext__Z())) {
						G = 2;
						continue;
					}
					lA3 = lA2.next__Ljava_lang_Object_();
					(lA1 as java_util_ArrayList).add_Ljava_lang_Object__Z(lA3);
					G = 1;
					continue;
				case 2:
					return (lA1 as java_util_ArrayList);
				default:
					break;
			}
		}
		return null;
	}
	java_util_AbstractCollection([int CLASS_ID = 721]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_Set  implements java_util_Collection {

	 int size__I();
	 bool isEmpty__Z();
	 java_util_Iterator iterator__Ljava_util_Iterator_();
	 int hashCode__I();
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0);
	 bool contains_Ljava_lang_Object__Z(java_lang_Object p0);
	 bool containsAll_Ljava_util_Collection__Z(java_util_Collection p0);
	 bool add_Ljava_lang_Object__Z(java_lang_Object p0);
	 JA_L toArray__Ljava_lang_Object___Ljava_lang_Object_(JA_L p0);
}
class java_util_Set_IFields {

	static void SI() { }
}
abstract class java_util_AbstractSet extends java_util_AbstractCollection implements java_util_Set {

	 java_util_AbstractSet java_util_AbstractSet_init___V() {
		this.java_util_AbstractCollection_init___V();
		return this;
		return this;
	}
	 int hashCode__I() {
		int G = 0;
		java_lang_Object lA3 = null;
		int fI0 = 0;
		int fI1 = 0;
		int lI1 = 0;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					lA2 = this.iterator__Ljava_util_Iterator_();
					G = 1;
					continue;
				case 1:
					if (!(lA2.hasNext__Z())) {
						G = 2;
						continue;
					}
					lA3 = lA2.next__Ljava_lang_Object_();
					fI0 = lI1;
					if (((lA3 != null))) {
						G = 3;
						continue;
					}
					fI1 = 0;
					G = 4;
					continue;
				case 3:
					fI1 = lA3.hashCode__I();
					G = 4;
					continue;
				case 4:
					lI1 = (N.I(fI0 + fI1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		int fI0 = 0;
		java_lang_Object lA3 = null;
		java_lang_Object fA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							if (((this != p0))) {
								G = 1;
								continue;
							}
							return true;
						case 1:
							if (!(((p0) is java_util_Set))) {
								G = 2;
								continue;
							}
							lA2 = ((p0) as java_util_Set);
							G = 3;
							continue;
						case 3:
							if (((this.size__I() != (lA2 as java_util_Set).size__I()))) {
								G = 4;
								continue;
							}
							if (!(this.containsAll_Ljava_util_Collection__Z((lA2 as java_util_Collection)))) {
								G = 4;
								continue;
							}
							fI0 = 1;
							G = 5;
							continue;
						case 4:
							fI0 = 0;
							G = 5;
							continue;
						case 5: return ((fI0)!=0);
						case 6:
							fA0 = J__exception__;
							lA3 = fA0;
							return false;
						case 7:
							fA0 = J__exception__;
							lA3 = fA0;
							return false;
						case 2:
							return false;
						default:
							break;
					}
				}
				return false;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 3)) && ((G < 5)))) && ((J__exception__) is java_lang_NullPointerException)))) {
					G = 6;
					continue;
				}
				if (((((((G >= 3)) && ((G < 5)))) && ((J__exception__) is java_lang_ClassCastException)))) {
					G = 7;
					continue;
				}
				rethrow;
			}
		}
		return false;
	}
	java_util_AbstractSet([int CLASS_ID = 720]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_Hashtable_EntrySet extends java_util_AbstractSet  {

	java_util_Hashtable _this_0 = null;
	 int hashCode__I() {
		return this._this_0.hashCode__I();
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		int fI0 = 0;
		java_lang_Object lA3 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							tA0 = this._this_0;
							fA0 = tA0;
							lA2 = tA0;
							N.monitorEnter(fA0);
							G = 1;
							continue;
						case 1:
							fI0 = (N.z2i(super.equals_Ljava_lang_Object__Z(p0)));
							N.monitorExit(lA2);
							G = 2;
							continue;
						case 2: return ((fI0)!=0);
						case 3:
							fA0 = J__exception__;
							lA3 = fA0;
							N.monitorExit(lA2);
							G = 4;
							continue;
						case 4: throw (lA3).prepareThrow__Ljava_lang_Throwable_().dartError; break;
						default:
							break;
					}
				}
				return false;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 3;
					continue;
				}
				if (((((((G >= 3)) && ((G < 4)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return false;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA2 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							tA0 = this._this_0;
							fA0 = tA0;
							lA1 = tA0;
							N.monitorEnter(fA0);
							G = 1;
							continue;
						case 1:
							fA0 = super.toString__Ljava_lang_String_();
							N.monitorExit(lA1);
							G = 2;
							continue;
						case 2: return (fA0 as java_lang_String);
						case 3:
							fA0 = J__exception__;
							lA2 = fA0;
							N.monitorExit(lA1);
							G = 4;
							continue;
						case 4: throw (lA2).prepareThrow__Ljava_lang_Throwable_().dartError; break;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 3;
					continue;
				}
				if (((((((G >= 3)) && ((G < 4)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	 int size__I() {
		return this._this_0.size__I();
	}
	 java_util_Iterator iterator__Ljava_util_Iterator_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_util_Hashtable_EntryIterator());
		fA0 = tA0;
		(tA0 as java_util_Hashtable_EntryIterator).java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V(this._this_0, null);
		return (fA0 as java_util_Iterator);
	}
	 bool containsAll_Ljava_util_Collection__Z(java_util_Collection p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		int fI0 = 0;
		java_lang_Object lA3 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							tA0 = this._this_0;
							fA0 = tA0;
							lA2 = tA0;
							N.monitorEnter(fA0);
							G = 1;
							continue;
						case 1:
							fI0 = (N.z2i(super.containsAll_Ljava_util_Collection__Z(p0)));
							N.monitorExit(lA2);
							G = 2;
							continue;
						case 2: return ((fI0)!=0);
						case 3:
							fA0 = J__exception__;
							lA3 = fA0;
							N.monitorExit(lA2);
							G = 4;
							continue;
						case 4: throw (lA3).prepareThrow__Ljava_lang_Throwable_().dartError; break;
						default:
							break;
					}
				}
				return false;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 3;
					continue;
				}
				if (((((((G >= 3)) && ((G < 4)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return false;
	}
	 bool contains_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_util_Map_Entry lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_util_Map_Entry)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					lA2 = ((p0) as java_util_Map_Entry);
					return java_util_Hashtable.access_1100_Ljava_util_Hashtable_Ljava_lang_Object_Ljava_lang_Object__Z(this._this_0, lA2.getKey__Ljava_lang_Object_(), lA2.getValue__Ljava_lang_Object_());
				default:
					break;
			}
		}
		return false;
	}
	 JA_L toArray__Ljava_lang_Object___Ljava_lang_Object_(JA_L p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		java_lang_Object lA3 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							tA0 = this._this_0;
							fA0 = tA0;
							lA2 = tA0;
							N.monitorEnter(fA0);
							G = 1;
							continue;
						case 1:
							fA0 = super.toArray__Ljava_lang_Object___Ljava_lang_Object_(p0);
							N.monitorExit(lA2);
							G = 2;
							continue;
						case 2: return (fA0 as JA_L);
						case 3:
							fA0 = J__exception__;
							lA3 = fA0;
							N.monitorExit(lA2);
							G = 4;
							continue;
						case 4: throw (lA3).prepareThrow__Ljava_lang_Throwable_().dartError; break;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 3;
					continue;
				}
				if (((((((G >= 3)) && ((G < 4)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	 java_util_Hashtable_EntrySet java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V(java_util_Hashtable p0, java_util_Hashtable_1 p1) {
		this.java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable__V(p0);
		return this;
		return this;
	}
	 java_util_Hashtable_EntrySet java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable__V(java_util_Hashtable p0) {
		this._this_0 = p0;
		this.java_util_AbstractSet_init___V();
		return this;
		return this;
	}
	java_util_Hashtable_EntrySet([int CLASS_ID = 924]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_Hashtable_1 extends java_lang_Object  {

	java_util_Hashtable_1([int CLASS_ID = 923]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_Map_Entry   {

	 java_lang_Object getKey__Ljava_lang_Object_();
	 java_lang_Object getValue__Ljava_lang_Object_();
	 int hashCode__I();
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0);
}
class java_util_Map_Entry_IFields {

	static void SI() { }
}
class java_util_Hashtable_HashtableEntry extends java_lang_Object implements java_util_Map_Entry {

	java_lang_Object _value = null;
	java_util_Hashtable_HashtableEntry _next = null;
	java_lang_Object _key = null;
	int _hash = 0;
	 java_util_Hashtable_HashtableEntry java_util_Hashtable_HashtableEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_Hashtable_HashtableEntry__V(java_lang_Object p0, java_lang_Object p1, int p2, java_util_Hashtable_HashtableEntry p3) {
		this.java_lang_Object_init___V();
		this._key = p0;
		this._value = p1;
		this._hash = p2;
		this._next = p3;
		return this;
		return this;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		return (fA0 as java_lang_StringBuilder).append_Ljava_lang_Object__Ljava_lang_StringBuilder_(this._key).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_7).append_Ljava_lang_Object__Ljava_lang_StringBuilder_(this._value).toString__Ljava_lang_String_();
	}
	 int hashCode__I() {
		return (N.I(this._key.hashCode__I() ^ this._value.hashCode__I()));
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		java_util_Map_Entry lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_util_Map_Entry)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					lA2 = ((p0) as java_util_Map_Entry);
					if (!(this._key.equals_Ljava_lang_Object__Z(lA2.getKey__Ljava_lang_Object_()))) {
						G = 2;
						continue;
					}
					if (!(this._value.equals_Ljava_lang_Object__Z(lA2.getValue__Ljava_lang_Object_()))) {
						G = 2;
						continue;
					}
					fI0 = 1;
					G = 3;
					continue;
				case 2:
					fI0 = 0;
					G = 3;
					continue;
				case 3: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 java_lang_Object getKey__Ljava_lang_Object_() {
		return this._key;
	}
	 java_lang_Object getValue__Ljava_lang_Object_() {
		return this._value;
	}
	java_util_Hashtable_HashtableEntry([int CLASS_ID = 922]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_Dictionary extends java_lang_Object  {

	 java_util_Dictionary java_util_Dictionary_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int size__I() {
		throw new Exception("Missing body java.util.Dictionary.size\u0028\u0029I");
	}
	 bool isEmpty__Z() {
		throw new Exception("Missing body java.util.Dictionary.isEmpty\u0028\u0029Z");
	}
	 java_lang_Object get_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0) {
		throw new Exception("Missing body java.util.Dictionary.get\u0028Ljava/lang/Object;\u0029Ljava/lang/Object;");
	}
	 java_lang_Object put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0, java_lang_Object p1) {
		throw new Exception("Missing body java.util.Dictionary.put\u0028Ljava/lang/Object;Ljava/lang/Object;\u0029Ljava/lang/Object;");
	}
	java_util_Dictionary([int CLASS_ID = 921]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_Map   {

	 java_util_Set entrySet__Ljava_util_Set_();
	 int size__I();
	 bool isEmpty__Z();
	 int hashCode__I();
	 java_lang_Object get_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0);
	 bool containsKey_Ljava_lang_Object__Z(java_lang_Object p0);
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0);
	 java_lang_Object put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0, java_lang_Object p1);
}
class java_util_Map_IFields {

	static void SI() { }
}
abstract class java_lang_Cloneable   {

}
class java_lang_Cloneable_IFields {

	static void SI() { }
}
abstract class java_io_Serializable   {

}
class java_io_Serializable_IFields {

	static void SI() { }
}
class java_util_Hashtable extends java_util_Dictionary implements java_util_Map, java_lang_Cloneable, java_io_Serializable {

	int _threshold = 0;
	JA_L _table = null;
	static JA_L _EMPTY_TABLE = null;
	int _size = 0;
	int _modCount = 0;
	java_util_Set _entrySet = null;
	java_util_Set _keySet = null;
	java_util_Collection _values = null;
	static JA_L _serialPersistentFields = null;
	 java_util_Hashtable java_util_Hashtable_init___V() {
		this.java_util_Dictionary_init___V();
		this._table = ((java_util_Hashtable._EMPTY_TABLE) as JA_L);
		this._threshold = -1;
		return this;
		return this;
	}
	 int size__I() {
		return this._size;
	}
	 bool isEmpty__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._size != 0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 java_lang_Object get_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA3 = null;
		java_lang_Object lA4 = null;
		java_lang_Object lA5 = null;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI2 = java_util_Collections.secondaryHash_Ljava_lang_Object__I(p0);
					lA3 = this._table;
					lA4 = ((lA3 as JA_L)).data[(N.I(lI2 & (N.I((lA3 as JA_0).length - 1))))];
					G = 1;
					continue;
				case 1:
					if (((lA4 == null))) {
						G = 2;
						continue;
					}
					lA5 = (lA4 as java_util_Hashtable_HashtableEntry)._key;
					if (((lA5 == p0))) {
						G = 3;
						continue;
					}
					if ((((lA4 as java_util_Hashtable_HashtableEntry)._hash != lI2))) {
						G = 4;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z(lA5))) {
						G = 4;
						continue;
					}
					G = 3;
					continue;
				case 3:
					return (lA4 as java_util_Hashtable_HashtableEntry)._value;
				case 4:
					lA4 = (lA4 as java_util_Hashtable_HashtableEntry)._next;
					G = 1;
					continue;
				case 2:
					return null;
				default:
					break;
			}
		}
		return null;
	}
	 java_lang_Object put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0, java_lang_Object p1) {
		int G = 0;
		java_lang_Object lA4 = null;
		java_lang_Object lA6 = null;
		java_lang_Object lA7 = null;
		java_lang_Object lA8 = null;
		int lI3 = 0;
		int lI5 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA5 = null;
		java_lang_Object tA6 = null;
		int fI0 = 0;
		int fI1 = 0;
		int tI4 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_NullPointerException());
					fA0 = tA0;
					(tA0 as java_lang_NullPointerException).java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_8);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					if (((p1 != null))) {
						G = 2;
						continue;
					}
					tA1 = (new java_lang_NullPointerException());
					fA0 = tA1;
					(tA1 as java_lang_NullPointerException).java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_9);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					lI3 = java_util_Collections.secondaryHash_Ljava_lang_Object__I(p0);
					lA4 = this._table;
					lI5 = (N.I(lI3 & (N.I((lA4 as JA_0).length - 1))));
					lA6 = ((lA4 as JA_L)).data[lI5];
					lA7 = lA6;
					G = 3;
					continue;
				case 3:
					if (((lA7 == null))) {
						G = 4;
						continue;
					}
					if ((((lA7 as java_util_Hashtable_HashtableEntry)._hash != lI3))) {
						G = 5;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z((lA7 as java_util_Hashtable_HashtableEntry)._key))) {
						G = 5;
						continue;
					}
					lA8 = (lA7 as java_util_Hashtable_HashtableEntry)._value;
					(lA7 as java_util_Hashtable_HashtableEntry)._value = p1;
					return lA8;
				case 5:
					lA7 = (lA7 as java_util_Hashtable_HashtableEntry)._next;
					G = 3;
					continue;
				case 4:
					this._modCount = (N.I(this._modCount + 1));
					fA0 = this;
					tA5 = fA0;
					tI4 = this._size;
					fI0 = tI4;
					(tA5 as java_util_Hashtable)._size = (N.I(tI4 + 1));
					if (((fI0 <= this._threshold))) {
						G = 6;
						continue;
					}
					this.rehash__V();
					lA4 = this.doubleCapacity___Ljava_util_Hashtable_HashtableEntry_();
					lI5 = (N.I(lI3 & (N.I((lA4 as JA_0).length - 1))));
					lA6 = ((lA4 as JA_L)).data[lI5];
					G = 6;
					continue;
				case 6:
					fA0 = lA4;
					fI1 = lI5;
					tA6 = (new java_util_Hashtable_HashtableEntry());
					fA2 = tA6;
					(tA6 as java_util_Hashtable_HashtableEntry).java_util_Hashtable_HashtableEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_Hashtable_HashtableEntry__V(p0, p1, lI3, (lA6 as java_util_Hashtable_HashtableEntry));
					(fA0 as JA_L).data[fI1] = fA2;
					return null;
				default:
					break;
			}
		}
		return null;
	}
	 void rehash__V() {
		return;
	}
	 JA_L doubleCapacity___Ljava_util_Hashtable_HashtableEntry_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA4 = null;
		java_lang_Object lA6 = null;
		java_lang_Object lA8 = null;
		java_lang_Object lA9 = null;
		int lI2 = 0;
		int lI3 = 0;
		int lI5 = 0;
		int lI7 = 0;
		int lI10 = 0;
		while (true) {
			switch (G) {
				case 0:
					lA1 = this._table;
					lI2 = (lA1 as JA_0).length;
					if (((lI2 != 1073741824))) {
						G = 1;
						continue;
					}
					return (lA1 as JA_L);
				case 1:
					lI3 = (N.I(lI2 * 2));
					lA4 = this.makeTable_I__Ljava_util_Hashtable_HashtableEntry_(lI3);
					if (((this._size != 0))) {
						G = 2;
						continue;
					}
					return (lA4 as JA_L);
				case 2:
					lI5 = 0;
					G = 3;
					continue;
				case 3:
					if (((lI5 >= lI2))) {
						G = 4;
						continue;
					}
					lA6 = ((lA1 as JA_L)).data[lI5];
					if (((lA6 != null))) {
						G = 5;
						continue;
					}
					G = 6;
					continue;
				case 5:
					lI7 = (N.I((lA6 as java_util_Hashtable_HashtableEntry)._hash & lI2));
					lA8 = null;
					(lA4 as JA_L).data[(N.I(lI5 | lI7))] = lA6;
					lA9 = (lA6 as java_util_Hashtable_HashtableEntry)._next;
					G = 7;
					continue;
				case 7:
					if (((lA9 == null))) {
						G = 8;
						continue;
					}
					lI10 = (N.I((lA9 as java_util_Hashtable_HashtableEntry)._hash & lI2));
					if (((lI10 == lI7))) {
						G = 9;
						continue;
					}
					if (((lA8 != null))) {
						G = 10;
						continue;
					}
					(lA4 as JA_L).data[(N.I(lI5 | lI10))] = lA9;
					G = 11;
					continue;
				case 10:
					(lA8 as java_util_Hashtable_HashtableEntry)._next = (lA9 as java_util_Hashtable_HashtableEntry);
					G = 11;
					continue;
				case 11:
					lA8 = lA6;
					lI7 = lI10;
					G = 9;
					continue;
				case 9:
					lA6 = lA9;
					lA9 = (lA9 as java_util_Hashtable_HashtableEntry)._next;
					G = 7;
					continue;
				case 8:
					if (((lA8 == null))) {
						G = 6;
						continue;
					}
					(lA8 as java_util_Hashtable_HashtableEntry)._next = null;
					G = 6;
					continue;
				case 6:
					lI5 = (N.I(lI5 + 1));
					G = 3;
					continue;
				case 4:
					return (lA4 as JA_L);
				default:
					break;
			}
		}
		return null;
	}
	 JA_L makeTable_I__Ljava_util_Hashtable_HashtableEntry_(int p0) {
		JA_L lA2 = null;
		lA2 = new JA_L(p0, "[Ljava.util.Hashtable\$HashtableEntry;");
		this._table = lA2;
		this._threshold = (N.I((N.I(p0 >> 1)) + (N.I(p0 >> 2))));
		return lA2;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA5 = null;
		java_lang_Object lA6 = null;
		int fI0 = 0;
		int lI3 = 0;
		java_util_Map_Entry lA4 = null;
		java_lang_Object fA0 = null;
		int tI5 = 0;
		java_lang_String fA1 = null;
		java_lang_Object tA0 = null;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init__I_V((N.I(15 * this._size)));
					lA1 = fA0;
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(123);
					lA2 = this.entrySet__Ljava_util_Set_().iterator__Ljava_util_Iterator_();
					lI3 = (N.z2i(lA2.hasNext__Z()));
					G = 1;
					continue;
				case 1:
					if (((lI3 == 0))) {
						G = 2;
						continue;
					}
					lA4 = ((lA2.next__Ljava_lang_Object_()) as java_util_Map_Entry);
					lA5 = lA4.getKey__Ljava_lang_Object_();
					fA0 = lA1;
					if (((lA5 != this))) {
						G = 3;
						continue;
					}
					fA1 = Bootstrap.STRINGLIT_10;
					G = 4;
					continue;
				case 3:
					fA1 = lA5.toString__Ljava_lang_String_();
					G = 4;
					continue;
				case 4:
					(fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(fA1);
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(61);
					lA6 = lA4.getValue__Ljava_lang_Object_();
					fA0 = lA1;
					if (((lA6 != this))) {
						G = 5;
						continue;
					}
					fA1 = Bootstrap.STRINGLIT_10;
					G = 6;
					continue;
				case 5:
					fA1 = lA6.toString__Ljava_lang_String_();
					G = 6;
					continue;
				case 6:
					(fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(fA1);
					tI5 = (N.z2i(lA2.hasNext__Z()));
					fI0 = tI5;
					lI3 = tI5;
					if (((fI0 == 0))) {
						G = 7;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_6);
					G = 7;
					continue;
				case 7:
					G = 1;
					continue;
				case 2:
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(125);
					return (lA1 as java_lang_StringBuilder).toString__Ljava_lang_String_();
				default:
					break;
			}
		}
		return null;
	}
	 java_util_Set entrySet__Ljava_util_Set_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA2 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = this._entrySet;
					if (((lA1 == null))) {
						G = 1;
						continue;
					}
					fA0 = lA1;
					G = 2;
					continue;
				case 1:
					fA0 = this;
					tA0 = (new java_util_Hashtable_EntrySet());
					fA1 = tA0;
					(tA0 as java_util_Hashtable_EntrySet).java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V(this, null);
					tA2 = fA0;
					tA1 = fA1;
					fA0 = tA1;
					(tA2 as java_util_Hashtable)._entrySet = (tA1 as java_util_Set);
					G = 2;
					continue;
				case 2: return (fA0 as java_util_Set);
				default:
					break;
			}
		}
		return null;
	}
	 int hashCode__I() {
		int G = 0;
		java_lang_Object lA4 = null;
		java_lang_Object lA5 = null;
		int fI0 = 0;
		int fI1 = 0;
		int fI2 = 0;
		int lI1 = 0;
		java_util_Map_Entry lA3 = null;
		java_lang_Object fA2 = null;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					lA2 = this.entrySet__Ljava_util_Set_().iterator__Ljava_util_Iterator_();
					G = 1;
					continue;
				case 1:
					if (!(lA2.hasNext__Z())) {
						G = 2;
						continue;
					}
					lA3 = ((lA2.next__Ljava_lang_Object_()) as java_util_Map_Entry);
					lA4 = lA3.getKey__Ljava_lang_Object_();
					lA5 = lA3.getValue__Ljava_lang_Object_();
					if (((lA4 == this))) {
						G = 1;
						continue;
					}
					if (((lA5 != this))) {
						G = 3;
						continue;
					}
					G = 1;
					continue;
				case 3:
					fI0 = lI1;
					if (((lA4 == null))) {
						G = 4;
						continue;
					}
					fI1 = lA4.hashCode__I();
					G = 5;
					continue;
				case 4:
					fI1 = 0;
					G = 5;
					continue;
				case 5:
					if (((lA5 == null))) {
						G = 6;
						continue;
					}
					fA2 = lA5;
					fI2 = fA2.hashCode__I();
					G = 7;
					continue;
				case 6:
					fI2 = 0;
					G = 7;
					continue;
				case 7:
					lI1 = (N.I(fI0 + (N.I(fI1 ^ fI2))));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	static int access_500_Ljava_util_Hashtable__I(java_util_Hashtable p0) {
		return p0._modCount;
	}
	static JA_L access_600_Ljava_util_Hashtable___Ljava_util_Hashtable_HashtableEntry_(java_util_Hashtable p0) {
		return p0._table;
	}
	static bool access_1100_Ljava_util_Hashtable_Ljava_lang_Object_Ljava_lang_Object__Z(java_util_Hashtable p0, java_lang_Object p1, java_lang_Object p2) {
		return p0.containsMapping_Ljava_lang_Object_Ljava_lang_Object__Z(p1, p2);
	}
	 bool containsMapping_Ljava_lang_Object_Ljava_lang_Object__Z(java_lang_Object p0, java_lang_Object p1) {
		int G = 0;
		java_lang_Object lA4 = null;
		java_lang_Object lA6 = null;
		int lI3 = 0;
		int lI5 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI3 = java_util_Collections.secondaryHash_Ljava_lang_Object__I(p0);
					lA4 = this._table;
					lI5 = (N.I(lI3 & (N.I((lA4 as JA_0).length - 1))));
					lA6 = ((lA4 as JA_L)).data[lI5];
					G = 1;
					continue;
				case 1:
					if (((lA6 == null))) {
						G = 2;
						continue;
					}
					if ((((lA6 as java_util_Hashtable_HashtableEntry)._hash != lI3))) {
						G = 3;
						continue;
					}
					if (!((lA6 as java_util_Hashtable_HashtableEntry)._key.equals_Ljava_lang_Object__Z(p0))) {
						G = 3;
						continue;
					}
					return (lA6 as java_util_Hashtable_HashtableEntry)._value.equals_Ljava_lang_Object__Z(p1);
				case 3:
					lA6 = (lA6 as java_util_Hashtable_HashtableEntry)._next;
					G = 1;
					continue;
				case 2:
					return false;
				default:
					break;
			}
		}
		return false;
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(((p0) is java_util_Map))) {
						G = 1;
						continue;
					}
					if (!(this.entrySet__Ljava_util_Set_().equals_Ljava_lang_Object__Z(((p0) as java_util_Map).entrySet__Ljava_util_Set_()))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2:
					return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 java_lang_Object clone__Ljava_lang_Object_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA2 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							G = 1;
							continue;
						case 1:
							lA1 = ((super.clone__Ljava_lang_Object_()) as java_util_Hashtable);
							G = 2;
							continue;
						case 2:
							G = 3;
							continue;
						case 4:
							fA0 = J__exception__;
							lA2 = fA0;
							tA1 = (new java_lang_AssertionError());
							fA0 = tA1;
							(tA1 as java_lang_AssertionError).java_lang_AssertionError_init__Ljava_lang_Object__V(lA2);
							throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
							G = 3;
							continue;
						case 3:
							(lA1 as java_util_Hashtable).makeTable_I__Ljava_util_Hashtable_HashtableEntry_((this._table as JA_0).length);
							(lA1 as java_util_Hashtable)._size = 0;
							(lA1 as java_util_Hashtable)._keySet = null;
							(lA1 as java_util_Hashtable)._entrySet = null;
							(lA1 as java_util_Hashtable)._values = null;
							(lA1 as java_util_Hashtable).constructorPutAll_Ljava_util_Map__V(this);
							return lA1;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_CloneNotSupportedException)))) {
					G = 4;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	 void constructorPutAll_Ljava_util_Map__V(java_util_Map p0) {
		int G = 0;
		java_util_Map_Entry lA3 = null;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((this._table != java_util_Hashtable._EMPTY_TABLE))) {
						G = 1;
						continue;
					}
					this.doubleCapacity___Ljava_util_Hashtable_HashtableEntry_();
					G = 1;
					continue;
				case 1:
					lA2 = p0.entrySet__Ljava_util_Set_().iterator__Ljava_util_Iterator_();
					G = 2;
					continue;
				case 2:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					lA3 = ((lA2.next__Ljava_lang_Object_()) as java_util_Map_Entry);
					this.constructorPut_Ljava_lang_Object_Ljava_lang_Object__V(lA3.getKey__Ljava_lang_Object_(), lA3.getValue__Ljava_lang_Object_());
					G = 2;
					continue;
				case 3:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void constructorPut_Ljava_lang_Object_Ljava_lang_Object__V(java_lang_Object p0, java_lang_Object p1) {
		int G = 0;
		java_lang_Object lA4 = null;
		java_lang_Object lA6 = null;
		java_lang_Object lA7 = null;
		int fI1 = 0;
		int lI3 = 0;
		int lI5 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_NullPointerException());
					fA0 = tA0;
					(tA0 as java_lang_NullPointerException).java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_8);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					if (((p1 != null))) {
						G = 2;
						continue;
					}
					tA1 = (new java_lang_NullPointerException());
					fA0 = tA1;
					(tA1 as java_lang_NullPointerException).java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_9);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					lI3 = java_util_Collections.secondaryHash_Ljava_lang_Object__I(p0);
					lA4 = this._table;
					lI5 = (N.I(lI3 & (N.I((lA4 as JA_0).length - 1))));
					lA6 = ((lA4 as JA_L)).data[lI5];
					lA7 = lA6;
					G = 3;
					continue;
				case 3:
					if (((lA7 == null))) {
						G = 4;
						continue;
					}
					if ((((lA7 as java_util_Hashtable_HashtableEntry)._hash != lI3))) {
						G = 5;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z((lA7 as java_util_Hashtable_HashtableEntry)._key))) {
						G = 5;
						continue;
					}
					(lA7 as java_util_Hashtable_HashtableEntry)._value = p1;
					return;
					G = 5;
					continue;
				case 5:
					lA7 = (lA7 as java_util_Hashtable_HashtableEntry)._next;
					G = 3;
					continue;
				case 4:
					fA0 = lA4;
					fI1 = lI5;
					tA2 = (new java_util_Hashtable_HashtableEntry());
					fA2 = tA2;
					(tA2 as java_util_Hashtable_HashtableEntry).java_util_Hashtable_HashtableEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_Hashtable_HashtableEntry__V(p0, p1, lI3, (lA6 as java_util_Hashtable_HashtableEntry));
					(fA0 as JA_L).data[fI1] = fA2;
					this._size = (N.I(this._size + 1));
					return;
				default:
					break;
			}
		}
		return;
	}
	 bool containsKey_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA3 = null;
		java_lang_Object lA4 = null;
		java_lang_Object lA5 = null;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI2 = java_util_Collections.secondaryHash_Ljava_lang_Object__I(p0);
					lA3 = this._table;
					lA4 = ((lA3 as JA_L)).data[(N.I(lI2 & (N.I((lA3 as JA_0).length - 1))))];
					G = 1;
					continue;
				case 1:
					if (((lA4 == null))) {
						G = 2;
						continue;
					}
					lA5 = (lA4 as java_util_Hashtable_HashtableEntry)._key;
					if (((lA5 == p0))) {
						G = 3;
						continue;
					}
					if ((((lA4 as java_util_Hashtable_HashtableEntry)._hash != lI2))) {
						G = 4;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z(lA5))) {
						G = 4;
						continue;
					}
					G = 3;
					continue;
				case 3:
					return true;
				case 4:
					lA4 = (lA4 as java_util_Hashtable_HashtableEntry)._next;
					G = 1;
					continue;
				case 2:
					return false;
				default:
					break;
			}
		}
		return false;
	}
	static void java_util_Hashtable_clinit___V() {
		int fI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA3 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA3 = null;
		java_util_Hashtable._EMPTY_TABLE = (new JA_L(2, "[Ljava.util.Hashtable\$HashtableEntry;") as JA_L);
		tA0 = new JA_L(2, "[Ljava.io.ObjectStreamField;");
		fA0 = tA0;
		fA1 = tA0;
		fI2 = 0;
		tA1 = (new java_io_ObjectStreamField());
		fA3 = tA1;
		(tA1 as java_io_ObjectStreamField).java_io_ObjectStreamField_init__Ljava_lang_String_Ljava_lang_Class__V(Bootstrap.STRINGLIT_11, java_lang_Integer._TYPE);
		(fA1 as JA_L).data[fI2] = fA3;
		fA1 = fA0;
		fI2 = 1;
		tA3 = (new java_io_ObjectStreamField());
		fA3 = tA3;
		(tA3 as java_io_ObjectStreamField).java_io_ObjectStreamField_init__Ljava_lang_String_Ljava_lang_Class__V(Bootstrap.STRINGLIT_12, java_lang_Float._TYPE);
		(fA1 as JA_L).data[fI2] = fA3;
		java_util_Hashtable._serialPersistentFields = (fA0 as JA_L);
		return;
	}
	java_util_Hashtable([int CLASS_ID = 920]) : super(CLASS_ID) { }
	static void SI() {
		java_util_Hashtable._EMPTY_TABLE = null;
		java_util_Hashtable._serialPersistentFields = null;
		java_util_Hashtable.java_util_Hashtable_clinit___V();
	}
}
class java_util_Properties extends java_util_Hashtable  {

	java_util_Properties _defaults = null;
	 java_util_Properties java_util_Properties_init___V() {
		this.java_util_Hashtable_init___V();
		return this;
		return this;
	}
	 java_lang_String getProperty_Ljava_lang_String__Ljava_lang_String_(java_lang_String p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		java_lang_Object lA3 = null;
		java_lang_Object fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lA2 = super.get_Ljava_lang_Object__Ljava_lang_Object_(p0);
					if (!(((lA2) is java_lang_String))) {
						G = 1;
						continue;
					}
					fA0 = ((lA2) as java_lang_String);
					G = 2;
					continue;
				case 1:
					fA0 = null;
					G = 2;
					continue;
				case 2:
					lA3 = fA0;
					if (((lA3 != null))) {
						G = 3;
						continue;
					}
					if (((this._defaults == null))) {
						G = 3;
						continue;
					}
					lA3 = this._defaults.getProperty_Ljava_lang_String__Ljava_lang_String_(p0);
					G = 3;
					continue;
				case 3:
					return (lA3 as java_lang_String);
				default:
					break;
			}
		}
		return null;
	}
	java_util_Properties([int CLASS_ID = 919]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_simd_MutableMatrixFloat32x4x4Utils extends java_lang_Object  {

	static Float32x4 _vtemp2 = null;
	static Float32x4 _vtemp1 = null;
	 com_jtransc_simd_MutableMatrixFloat32x4x4Utils com_jtransc_simd_MutableMatrixFloat32x4x4Utils_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void com_jtransc_simd_MutableMatrixFloat32x4x4Utils_clinit___V() {
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp1 = new Float32x4(0.0, 0.0, 0.0, 0.0);
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp2 = new Float32x4(0.0, 0.0, 0.0, 0.0);
		return;
	}
	static Float32x4List _setToMul44_Lcom_jtransc_simd_MutableMatrixFloat32x4x4_Lcom_jtransc_simd_MutableMatrixFloat32x4x4_Lcom_jtransc_simd_MutableMatrixFloat32x4x4__Lcom_jtransc_simd_MutableMatrixFloat32x4x4_(Float32x4List p0, Float32x4List p1, Float32x4List p2) {
		Float32x4 lA10 = null;
		Float32x4 lA11 = null;
		Float32x4 lA12 = null;
		Float32x4 lA3 = null;
		Float32x4 lA4 = null;
		Float32x4 lA5 = null;
		Float32x4 lA6 = null;
		Float32x4 lA7 = null;
		Float32x4 lA8 = null;
		Float32x4 lA9 = null;
		lA3 = p1[0];
		lA4 = p2[0];
		lA5 = p1[1];
		lA6 = p2[1];
		lA7 = p1[2];
		lA8 = p2[2];
		lA9 = p1[3];
		lA10 = p2[3];
		lA11 = com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp1;
		lA12 = com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp2;
		lA12 = new Float32x4(0.0, 0.0, 0.0, 0.0);;
		lA11 = new Float32x4(lA4.x, lA4.x, lA4.x, lA4.x);;
		lA12 = lA12 + (lA11 * lA3);;
		lA11 = new Float32x4(lA4.y, lA4.y, lA4.y, lA4.y);;
		lA12 = lA12 + (lA11 * lA5);;
		lA11 = new Float32x4(lA4.z, lA4.z, lA4.z, lA4.z);;
		lA12 = lA12 + (lA11 * lA7);;
		lA11 = new Float32x4(lA4.w, lA4.w, lA4.w, lA4.w);;
		lA12 = lA12 + (lA11 * lA9);;
		p0[0] = lA12;;
		lA12 = new Float32x4(0.0, 0.0, 0.0, 0.0);;
		lA11 = new Float32x4(lA6.x, lA6.x, lA6.x, lA6.x);;
		lA12 = lA12 + (lA11 * lA3);;
		lA11 = new Float32x4(lA6.y, lA6.y, lA6.y, lA6.y);;
		lA12 = lA12 + (lA11 * lA5);;
		lA11 = new Float32x4(lA6.z, lA6.z, lA6.z, lA6.z);;
		lA12 = lA12 + (lA11 * lA7);;
		lA11 = new Float32x4(lA6.w, lA6.w, lA6.w, lA6.w);;
		lA12 = lA12 + (lA11 * lA9);;
		p0[1] = lA12;;
		lA12 = new Float32x4(0.0, 0.0, 0.0, 0.0);;
		lA11 = new Float32x4(lA8.x, lA8.x, lA8.x, lA8.x);;
		lA12 = lA12 + (lA11 * lA3);;
		lA11 = new Float32x4(lA8.y, lA8.y, lA8.y, lA8.y);;
		lA12 = lA12 + (lA11 * lA5);;
		lA11 = new Float32x4(lA8.z, lA8.z, lA8.z, lA8.z);;
		lA12 = lA12 + (lA11 * lA7);;
		lA11 = new Float32x4(lA8.w, lA8.w, lA8.w, lA8.w);;
		lA12 = lA12 + (lA11 * lA9);;
		p0[2] = lA12;;
		lA12 = new Float32x4(0.0, 0.0, 0.0, 0.0);;
		lA11 = new Float32x4(lA10.x, lA10.x, lA10.x, lA10.x);;
		lA12 = lA12 + (lA11 * lA3);;
		lA11 = new Float32x4(lA10.y, lA10.y, lA10.y, lA10.y);;
		lA12 = lA12 + (lA11 * lA5);;
		lA11 = new Float32x4(lA10.z, lA10.z, lA10.z, lA10.z);;
		lA12 = lA12 + (lA11 * lA7);;
		lA11 = new Float32x4(lA10.w, lA10.w, lA10.w, lA10.w);;
		lA12 = lA12 + (lA11 * lA9);;
		p0[3] = lA12;;
		return p0;
	}
	static double _getSumAll_Lcom_jtransc_simd_MutableMatrixFloat32x4x4__F(Float32x4List p0) {
		return ((((((p0[0].x+p0[0].y+p0[0].z+p0[0].w + p0[1].x+p0[1].y+p0[1].z+p0[1].w)) + p0[2].x+p0[2].y+p0[2].z+p0[2].w)) + p0[3].x+p0[3].y+p0[3].z+p0[3].w));
	}
	com_jtransc_simd_MutableMatrixFloat32x4x4Utils([int CLASS_ID = 918]) : super(CLASS_ID) { }
	static void SI() {
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp2 = null;
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp1 = null;
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils.com_jtransc_simd_MutableMatrixFloat32x4x4Utils_clinit___V();
	}
}
class com_jtransc_simd_MutableFloat32x4Utils extends java_lang_Object  {

	static Float32x4 _temp = null;
	 com_jtransc_simd_MutableFloat32x4Utils com_jtransc_simd_MutableFloat32x4Utils_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void com_jtransc_simd_MutableFloat32x4Utils_clinit___V() {
		com_jtransc_simd_MutableFloat32x4Utils._temp = new Float32x4(0.0, 0.0, 0.0, 0.0);
		return;
	}
	static java_lang_String toStringInternal_Lcom_jtransc_simd_MutableFloat32x4__Ljava_lang_String_(Float32x4 p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_13).append_F_Ljava_lang_StringBuilder_(p0.x).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_6).append_F_Ljava_lang_StringBuilder_(p0.y).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_6).append_F_Ljava_lang_StringBuilder_(p0.z).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_6).append_F_Ljava_lang_StringBuilder_(p0.w).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_1).toString__Ljava_lang_String_();
	}
	com_jtransc_simd_MutableFloat32x4Utils([int CLASS_ID = 916]) : super(CLASS_ID) { }
	static void SI() {
		com_jtransc_simd_MutableFloat32x4Utils._temp = null;
		com_jtransc_simd_MutableFloat32x4Utils.com_jtransc_simd_MutableFloat32x4Utils_clinit___V();
	}
}
class com_jtransc_simd_Simd extends java_lang_Object  {

	 com_jtransc_simd_Simd com_jtransc_simd_Simd_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void ref__V() {
		return;
	}
	com_jtransc_simd_Simd([int CLASS_ID = 914]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_Test2 extends java_lang_Object  {

	 Benchmark_Test2 Benchmark_Test2_init__LBenchmark_1__V(Benchmark_1 p0) {
		this.Benchmark_Test2_init___V();
		return this;
		return this;
	}
	 Benchmark_Test2 Benchmark_Test2_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	Benchmark_Test2([int CLASS_ID = 912]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_Test1 extends java_lang_Object  {

	 Benchmark_Test1 Benchmark_Test1_init__LBenchmark_1__V(Benchmark_1 p0) {
		this.Benchmark_Test1_init___V();
		return this;
		return this;
	}
	 Benchmark_Test1 Benchmark_Test1_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	Benchmark_Test1([int CLASS_ID = 911]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_zip_Deflater extends java_lang_Object  {

	com_jtransc_compression_jzlib_Deflater _impl = null;
	int _inLength = 0;
	int _inRead = 0;
	bool _noHeader = false;
	int _compressLevel = 0;
	Int64 _streamHandle = N.lnew(0);
	int _flushParm = 0;
	int _strategy = 0;
	 void setInput__BII_V(JA_B p0, int p1, int p2) {
		this._inLength = p2;
		this._inRead = 0;
		this._impl.setInput__BIIZ_V(p0, p1, p2, false);
		return;
	}
	 int deflate__BIII_I(JA_B p0, int p1, int p2, int p3) {
		return this.deflateImpl__BIII_I(p0, p1, p2, p3);
	}
	 int deflateImpl__BIII_I(JA_B p0, int p1, int p2, int p3) {
		Int64 lJ5 = N.lnew(0);
		Int64 lJ8 = N.lnew(0);
		this._impl.setOutput__BII_V(p0, p1, p2);
		lJ5 = this._impl.getTotalOut__J();
		this._impl.deflate_I_I(p3);
		lJ8 = this._impl.getTotalOut__J();
		return N.j2i(((lJ8-lJ5)));
	}
	 java_util_zip_Deflater java_util_zip_Deflater_init__IZ_V(int p0, bool p1) {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		this.java_lang_Object_init___V();
		this._flushParm = 0;
		this._compressLevel = -1;
		this._strategy = 0;
		this._streamHandle = N.lnew(-1);
		this._compressLevel = p0;
		this._noHeader = p1;
		tA0 = (new com_jtransc_compression_jzlib_Deflater());
		fA1 = tA0;
		(tA0 as com_jtransc_compression_jzlib_Deflater).com_jtransc_compression_jzlib_Deflater_init__IZ_V(p0, p1);
		this._impl = (fA1 as com_jtransc_compression_jzlib_Deflater);
		return this;
		return this;
	}
	java_util_zip_Deflater([int CLASS_ID = 910]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class com_jtransc_compression_jzlib_Checksum   {

	 int getValue__I();
	 void reset__V();
	 void update__BII_V(JA_B p0, int p1, int p2);
}
class com_jtransc_compression_jzlib_Checksum_IFields {

	static void SI() { }
}
class com_jtransc_compression_jzlib_Adler32 extends java_lang_Object implements com_jtransc_compression_jzlib_Checksum {

	int _s1 = 0;
	int _s2 = 0;
	 com_jtransc_compression_jzlib_Adler32 com_jtransc_compression_jzlib_Adler32_init___V() {
		this.java_lang_Object_init___V();
		this._s1 = 1;
		this._s2 = 0;
		return this;
		return this;
	}
	 int getValue__I() {
		return (N.I((N.I(this._s2 << 16)) | this._s1));
	}
	 void reset__V() {
		this._s1 = 1;
		this._s2 = 0;
		return;
	}
	 void update__BII_V(JA_B p0, int p1, int p2) {
		int G = 0;
		JA_B fA2 = null;
		int lI2 = 0;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		int lI6 = 0;
		com_jtransc_compression_jzlib_Adler32 tA10 = null;
		int fI1 = 0;
		int fI3 = 0;
		int fI0 = 0;
		com_jtransc_compression_jzlib_Adler32 fA0 = null;
		com_jtransc_compression_jzlib_Adler32 tA5 = null;
		com_jtransc_compression_jzlib_Adler32 tA7 = null;
		com_jtransc_compression_jzlib_Adler32 tA9 = null;
		com_jtransc_compression_jzlib_Adler32 tA1 = null;
		com_jtransc_compression_jzlib_Adler32 tA3 = null;
		while (true) {
			switch (G) {
				case 0:
					lI2 = p1;
					lI3 = p2;
					if (((lI3 != 1))) {
						G = 1;
						continue;
					}
					fA0 = this;
					fI1 = this._s1;
					fA2 = p0;
					fI3 = lI2;
					lI2 = (N.I(lI2 + 1));
					fA0._s1 = (N.I(fI1 + (N.I((fA2).data[fI3] & 255))));
					tA1 = this;
					tA1._s2 = (N.I(tA1._s2 + this._s1));
					this._s1 = (N.I(this._s1.remainder(65521)));
					tA3 = this;
					tA3._s2 = (N.I(tA3._s2.remainder(65521)));
					return;
					G = 1;
					continue;
				case 1:
					lI4 = (N.I(lI3 ~/ 5552));
					lI5 = (N.I(lI3.remainder(5552)));
					G = 2;
					continue;
				case 2:
					fI0 = lI4;
					lI4 = (N.I(lI4 + -1));
					if (((fI0 <= 0))) {
						G = 3;
						continue;
					}
					lI6 = 5552;
					lI3 = (N.I(lI3 - lI6));
					G = 4;
					continue;
				case 4:
					fI0 = lI6;
					lI6 = (N.I(lI6 + -1));
					if (((fI0 <= 0))) {
						G = 5;
						continue;
					}
					fA0 = this;
					fI1 = this._s1;
					fA2 = p0;
					fI3 = lI2;
					lI2 = (N.I(lI2 + 1));
					fA0._s1 = (N.I(fI1 + (N.I((fA2).data[fI3] & 255))));
					tA5 = this;
					tA5._s2 = (N.I(tA5._s2 + this._s1));
					G = 4;
					continue;
				case 5:
					this._s1 = (N.I(this._s1.remainder(65521)));
					tA7 = this;
					tA7._s2 = (N.I(tA7._s2.remainder(65521)));
					G = 2;
					continue;
				case 3:
					lI6 = lI5;
					lI3 = (N.I(lI3 - lI6));
					G = 6;
					continue;
				case 6:
					fI0 = lI6;
					lI6 = (N.I(lI6 + -1));
					if (((fI0 <= 0))) {
						G = 7;
						continue;
					}
					fA0 = this;
					fI1 = this._s1;
					fA2 = p0;
					fI3 = lI2;
					lI2 = (N.I(lI2 + 1));
					fA0._s1 = (N.I(fI1 + (N.I((fA2).data[fI3] & 255))));
					tA9 = this;
					tA9._s2 = (N.I(tA9._s2 + this._s1));
					G = 6;
					continue;
				case 7:
					tA10 = this;
					tA10._s1 = (N.I(tA10._s1.remainder(65521)));
					this._s2 = (N.I(this._s2.remainder(65521)));
					return;
				default:
					break;
			}
		}
		return;
	}
	com_jtransc_compression_jzlib_Adler32([int CLASS_ID = 909]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_Throwable extends java_lang_Object implements java_io_Serializable {
	Error dartError; StackTrace currentStackTrace;
	bool _thrown = false;
	static JA_L _EMPTY_ARRAY = null;
	java_lang_String _message = null;
	bool _writableStackTrace = false;
	bool _enableSuppression = false;
	java_lang_Throwable _cause = null;
	JA_L _stackTrace = null;
	java_util_ArrayList _supressed = null;
	 java_lang_Throwable prepareThrow__Ljava_lang_Throwable_() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (this._thrown) {
						G = 1;
						continue;
					}
					this.init_exception__V();
					G = 1;
					continue;
				case 1:
					this._thrown = true;
					return this;
				default:
					break;
			}
		}
		return null;
	}
	 void init_exception__V() {
		this.dartError = new WrappedThrowable(this);
	}
	static void java_lang_Throwable_clinit___V() {
		java_lang_Throwable._EMPTY_ARRAY = new JA_L(0, "[Ljava.lang.Throwable;");
		return;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_14).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this._message).toString__Ljava_lang_String_();
	}
	 java_lang_Throwable java_lang_Throwable_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_Object_init___V();
		this._enableSuppression = false;
		this._writableStackTrace = false;
		this._thrown = false;
		this.init_Ljava_lang_String_Ljava_lang_Throwable_ZZ_V(p0, null, false, false);
		return this;
		return this;
	}
	 void init_Ljava_lang_String_Ljava_lang_Throwable_ZZ_V(java_lang_String p0, java_lang_Throwable p1, bool p2, bool p3) {
		this._message = p0;
		this._cause = p1;
		this._enableSuppression = p2;
		this._writableStackTrace = p3;
		return;
	}
	 java_lang_Throwable java_lang_Throwable_init__Ljava_lang_String_Ljava_lang_Throwable__V(java_lang_String p0, java_lang_Throwable p1) {
		this.java_lang_Object_init___V();
		this._enableSuppression = false;
		this._writableStackTrace = false;
		this._thrown = false;
		this.init_Ljava_lang_String_Ljava_lang_Throwable_ZZ_V(p0, p1, false, false);
		return this;
		return this;
	}
	 java_lang_Throwable java_lang_Throwable_init___V() {
		this.java_lang_Object_init___V();
		this._enableSuppression = false;
		this._writableStackTrace = false;
		this._thrown = false;
		this.init_Ljava_lang_String_Ljava_lang_Throwable_ZZ_V(null, null, false, false);
		return this;
		return this;
	}
	 java_lang_Throwable java_lang_Throwable_init__Ljava_lang_Throwable__V(java_lang_Throwable p0) {
		this.java_lang_Object_init___V();
		this._enableSuppression = false;
		this._writableStackTrace = false;
		this._thrown = false;
		this.init_Ljava_lang_String_Ljava_lang_Throwable_ZZ_V(null, p0, false, false);
		return this;
		return this;
	}
	 void printStackTrace__V() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA2 = null;
		java_lang_Object lA5 = null;
		int lI3 = 0;
		int lI4 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					com_jtransc_io_JTranscConsole.error_Ljava_lang_Object__V(this);
					lA1 = this.getStackTraceLazy___Ljava_lang_StackTraceElement_();
					lA2 = lA1;
					lI3 = (lA2 as JA_0).length;
					lI4 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI4 >= lI3))) {
						G = 2;
						continue;
					}
					lA5 = ((lA2 as JA_L)).data[lI4];
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					com_jtransc_io_JTranscConsole.error_Ljava_lang_Object__V((fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_15).append_Ljava_lang_Object__Ljava_lang_StringBuilder_(lA5).toString__Ljava_lang_String_());
					lI4 = (N.I(lI4 + 1));
					G = 1;
					continue;
				case 2:
					lA2 = this.getSuppressed___Ljava_lang_Throwable_();
					lI3 = (lA2 as JA_0).length;
					lI4 = 0;
					G = 3;
					continue;
				case 3:
					if (((lI4 >= lI3))) {
						G = 4;
						continue;
					}
					lA5 = ((lA2 as JA_L)).data[lI4];
					com_jtransc_io_JTranscConsole.error_Ljava_lang_Object__V(Bootstrap.STRINGLIT_16);
					(lA5 as java_lang_Throwable).printStackTrace__V();
					lI4 = (N.I(lI4 + 1));
					G = 3;
					continue;
				case 4:
					lA2 = this.getCause__Ljava_lang_Throwable_();
					if (((lA2 == null))) {
						G = 5;
						continue;
					}
					com_jtransc_io_JTranscConsole.error_Ljava_lang_Object__V(Bootstrap.STRINGLIT_17);
					(lA2 as java_lang_Throwable).printStackTrace__V();
					G = 5;
					continue;
				case 5:
					return;
				default:
					break;
			}
		}
		return;
	}
	 JA_L getStackTraceLazy___Ljava_lang_StackTraceElement_() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._stackTrace != null))) {
						G = 1;
						continue;
					}
					this.fillInStackTrace_I_V(0);
					G = 1;
					continue;
				case 1:
					return this._stackTrace;
				default:
					break;
			}
		}
		return null;
	}
	 void fillInStackTrace_I_V(int p0) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(this._thrown)) {
						G = 1;
						continue;
					}
					this.genStackTraceFromError__V();
					G = 2;
					continue;
				case 1:
					this.genStackTrace__V();
					G = 2;
					continue;
				case 2:
					this.setStackTrace__Ljava_lang_StackTraceElement__V(this.getStackTraceInternal___Ljava_lang_StackTraceElement_());
					return;
				default:
					break;
			}
		}
		return;
	}
	 void genStackTrace__V() {
		this.currentStackTrace = StackTrace.current;
	}
	 JA_L getStackTraceInternal___Ljava_lang_StackTraceElement_() {
		return N.getStackTrace(this.currentStackTrace, 0);
	}
	 void setStackTrace__Ljava_lang_StackTraceElement__V(JA_L p0) {
		this._stackTrace = ((p0.clone__Ljava_lang_Object_()) as JA_L);
		return;
	}
	 java_lang_String getMessage__Ljava_lang_String_() {
		return this._message;
	}
	 java_lang_Throwable initCause_Ljava_lang_Throwable__Ljava_lang_Throwable_(java_lang_Throwable p0) {
		this._cause = p0;
		return this._cause;
	}
	 void genStackTraceFromError__V() {
		this.currentStackTrace = this.dartError.stackTrace;
	}
	 JA_L getSuppressed___Ljava_lang_Throwable_() {
		int G = 0;
		JA_L fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((this._supressed == null))) {
						G = 1;
						continue;
					}
					fA0 = ((this._supressed.toArray__Ljava_lang_Object___Ljava_lang_Object_((java_lang_Throwable._EMPTY_ARRAY as JA_L))) as JA_L);
					G = 2;
					continue;
				case 1:
					fA0 = java_lang_Throwable._EMPTY_ARRAY;
					G = 2;
					continue;
				case 2: return fA0;
				default:
					break;
			}
		}
		return null;
	}
	 java_lang_Throwable getCause__Ljava_lang_Throwable_() {
		return this._cause;
	}
	java_lang_Throwable([int CLASS_ID = 681]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_Throwable._EMPTY_ARRAY = null;
		java_lang_Throwable.java_lang_Throwable_clinit___V();
	}
}
class java_lang_Exception extends java_lang_Throwable  {

	 java_lang_Exception java_lang_Exception_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_Throwable_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	 java_lang_Exception java_lang_Exception_init__Ljava_lang_String_Ljava_lang_Throwable__V(java_lang_String p0, java_lang_Throwable p1) {
		this.java_lang_Throwable_init__Ljava_lang_String_Ljava_lang_Throwable__V(p0, p1);
		return this;
		return this;
	}
	 java_lang_Exception java_lang_Exception_init___V() {
		this.java_lang_Throwable_init___V();
		return this;
		return this;
	}
	 java_lang_Exception java_lang_Exception_init__Ljava_lang_Throwable__V(java_lang_Throwable p0) {
		this.java_lang_Throwable_init__Ljava_lang_Throwable__V(p0);
		return this;
		return this;
	}
	java_lang_Exception([int CLASS_ID = 680]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_RuntimeException extends java_lang_Exception  {

	 java_lang_RuntimeException java_lang_RuntimeException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_Exception_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	 java_lang_RuntimeException java_lang_RuntimeException_init__Ljava_lang_String_Ljava_lang_Throwable__V(java_lang_String p0, java_lang_Throwable p1) {
		this.java_lang_Exception_init__Ljava_lang_String_Ljava_lang_Throwable__V(p0, p1);
		return this;
		return this;
	}
	 java_lang_RuntimeException java_lang_RuntimeException_init___V() {
		this.java_lang_Exception_init___V();
		return this;
		return this;
	}
	 java_lang_RuntimeException java_lang_RuntimeException_init__Ljava_lang_Throwable__V(java_lang_Throwable p0) {
		this.java_lang_Exception_init__Ljava_lang_Throwable__V(p0);
		return this;
		return this;
	}
	java_lang_RuntimeException([int CLASS_ID = 696]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_compression_jzlib_GZIPException extends java_lang_RuntimeException  {

	 com_jtransc_compression_jzlib_GZIPException com_jtransc_compression_jzlib_GZIPException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_RuntimeException_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	com_jtransc_compression_jzlib_GZIPException([int CLASS_ID = 908]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_compression_jzlib_StaticTree extends java_lang_Object  {

	int _elems = 0;
	int _max_length = 0;
	int _extra_base = 0;
	JA_S _static_tree = null;
	JA_I _extra_bits = null;
	static JA_S _static_ltree = null;
	static JA_S _static_dtree = null;
	static com_jtransc_compression_jzlib_StaticTree _static_bl_desc = null;
	static com_jtransc_compression_jzlib_StaticTree _static_d_desc = null;
	static com_jtransc_compression_jzlib_StaticTree _static_l_desc = null;
	 com_jtransc_compression_jzlib_StaticTree com_jtransc_compression_jzlib_StaticTree_init___S_IIII_V(JA_S p0, JA_I p1, int p2, int p3, int p4) {
		this.java_lang_Object_init___V();
		this._static_tree = p0;
		this._extra_bits = p1;
		this._extra_base = p2;
		this._elems = p3;
		this._max_length = p4;
		return this;
		return this;
	}
	static void com_jtransc_compression_jzlib_StaticTree_clinit___V() {
		java_lang_Object tA636 = null;
		java_lang_Object tA637 = null;
		java_lang_Object tA638 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA576 = null;
		java_lang_Object tA0 = null;
		tA0 = new JA_S(576);
		fA0 = tA0;
		(tA0 as JA_S).data[0] = 12;
		(fA0 as JA_S).setArraySlice(1, [ 8, 140, 8, 76, 8, 204, 8, 44, 8, 172, 8, 108, 8, 236, 8, 28, 8, 156, 8, 92, 8, 220, 8, 60, 8, 188, 8, 124, 8, 252, 8, 2, 8, 130, 8, 66, 8, 194, 8, 34, 8, 162, 8, 98, 8, 226, 8, 18, 8, 146, 8, 82, 8, 210, 8, 50, 8, 178, 8, 114, 8, 242, 8, 10, 8, 138, 8, 74, 8, 202, 8, 42, 8, 170, 8, 106, 8, 234, 8, 26, 8, 154, 8, 90, 8, 218, 8, 58, 8, 186, 8, 122, 8, 250, 8, 6, 8, 134, 8, 70, 8, 198, 8, 38, 8, 166, 8, 102, 8, 230, 8, 22, 8, 150, 8, 86, 8, 214, 8, 54, 8, 182, 8, 118, 8, 246, 8, 14, 8, 142, 8, 78, 8, 206, 8, 46, 8, 174, 8, 110, 8, 238, 8, 30, 8, 158, 8, 94, 8, 222, 8, 62, 8, 190, 8, 126, 8, 254, 8, 1, 8, 129, 8, 65, 8, 193, 8, 33, 8, 161, 8, 97, 8, 225, 8, 17, 8, 145, 8, 81, 8, 209, 8, 49, 8, 177, 8, 113, 8, 241, 8, 9, 8, 137, 8, 73, 8, 201, 8, 41, 8, 169, 8, 105, 8, 233, 8, 25, 8, 153, 8, 89, 8, 217, 8, 57, 8, 185, 8, 121, 8, 249, 8, 5, 8, 133, 8, 69, 8, 197, 8, 37, 8, 165, 8, 101, 8, 229, 8, 21, 8, 149, 8, 85, 8, 213, 8, 53, 8, 181, 8, 117, 8, 245, 8, 13, 8, 141, 8, 77, 8, 205, 8, 45, 8, 173, 8, 109, 8, 237, 8, 29, 8, 157, 8, 93, 8, 221, 8, 61, 8, 189, 8, 125, 8, 253, 8, 19, 9, 275, 9, 147, 9, 403, 9, 83, 9, 339, 9, 211, 9, 467, 9, 51, 9, 307, 9, 179, 9, 435, 9, 115, 9, 371, 9, 243, 9, 499, 9, 11, 9, 267, 9, 139, 9, 395, 9, 75, 9, 331, 9, 203, 9, 459, 9, 43, 9, 299, 9, 171, 9, 427, 9, 107, 9, 363, 9, 235, 9, 491, 9, 27, 9, 283, 9, 155, 9, 411, 9, 91, 9, 347, 9, 219, 9, 475, 9, 59, 9, 315, 9, 187, 9, 443, 9, 123, 9, 379, 9, 251, 9, 507, 9, 7, 9, 263, 9, 135, 9, 391, 9, 71, 9, 327, 9, 199, 9, 455, 9, 39, 9, 295, 9, 167, 9, 423, 9, 103, 9, 359, 9, 231, 9, 487, 9, 23, 9, 279, 9, 151, 9, 407, 9, 87, 9, 343, 9, 215, 9, 471, 9, 55, 9, 311, 9, 183, 9, 439, 9, 119, 9, 375, 9, 247, 9, 503, 9, 15, 9, 271, 9, 143, 9, 399, 9, 79, 9, 335, 9, 207, 9, 463, 9, 47, 9, 303, 9, 175, 9, 431, 9, 111, 9, 367, 9, 239, 9, 495, 9, 31, 9, 287, 9, 159, 9, 415, 9, 95, 9, 351, 9, 223, 9, 479, 9, 63, 9, 319, 9, 191, 9, 447, 9, 127, 9, 383, 9, 255, 9, 511, 9, 0, 7, 64, 7, 32, 7, 96, 7, 16, 7, 80, 7, 48, 7, 112, 7, 8, 7, 72, 7, 40, 7, 104, 7, 24, 7, 88, 7, 56, 7, 120, 7, 4, 7, 68, 7, 36, 7, 100, 7, 20, 7, 84, 7, 52, 7, 116, 7, 3, 8, 131, 8, 67, 8, 195, 8, 35, 8, 163, 8, 99, 8, 227, 8 ]);
		com_jtransc_compression_jzlib_StaticTree._static_ltree = (fA0 as JA_S);
		tA576 = new JA_S(60);
		fA0 = tA576;
		(tA576 as JA_S).data[0] = 0;
		(fA0 as JA_S).setArraySlice(1, [ 5, 16, 5, 8, 5, 24, 5, 4, 5, 20, 5, 12, 5, 28, 5, 2, 5, 18, 5, 10, 5, 26, 5, 6, 5, 22, 5, 14, 5, 30, 5, 1, 5, 17, 5, 9, 5, 25, 5, 5, 5, 21, 5, 13, 5, 29, 5, 3, 5, 19, 5, 11, 5, 27, 5, 7, 5, 23, 5 ]);
		com_jtransc_compression_jzlib_StaticTree._static_dtree = (fA0 as JA_S);
		tA636 = (new com_jtransc_compression_jzlib_StaticTree());
		fA0 = tA636;
		(tA636 as com_jtransc_compression_jzlib_StaticTree).com_jtransc_compression_jzlib_StaticTree_init___S_IIII_V(com_jtransc_compression_jzlib_StaticTree._static_ltree, com_jtransc_compression_jzlib_Tree._extra_lbits, 257, 286, 15);
		com_jtransc_compression_jzlib_StaticTree._static_l_desc = (fA0 as com_jtransc_compression_jzlib_StaticTree);
		tA637 = (new com_jtransc_compression_jzlib_StaticTree());
		fA0 = tA637;
		(tA637 as com_jtransc_compression_jzlib_StaticTree).com_jtransc_compression_jzlib_StaticTree_init___S_IIII_V(com_jtransc_compression_jzlib_StaticTree._static_dtree, com_jtransc_compression_jzlib_Tree._extra_dbits, 0, 30, 15);
		com_jtransc_compression_jzlib_StaticTree._static_d_desc = (fA0 as com_jtransc_compression_jzlib_StaticTree);
		tA638 = (new com_jtransc_compression_jzlib_StaticTree());
		fA0 = tA638;
		(tA638 as com_jtransc_compression_jzlib_StaticTree).com_jtransc_compression_jzlib_StaticTree_init___S_IIII_V(null, com_jtransc_compression_jzlib_Tree._extra_blbits, 0, 19, 7);
		com_jtransc_compression_jzlib_StaticTree._static_bl_desc = (fA0 as com_jtransc_compression_jzlib_StaticTree);
		return;
	}
	com_jtransc_compression_jzlib_StaticTree([int CLASS_ID = 907]) : super(CLASS_ID) { }
	static void SI() {
		com_jtransc_compression_jzlib_StaticTree._static_ltree = null;
		com_jtransc_compression_jzlib_StaticTree._static_dtree = null;
		com_jtransc_compression_jzlib_StaticTree._static_bl_desc = null;
		com_jtransc_compression_jzlib_StaticTree._static_d_desc = null;
		com_jtransc_compression_jzlib_StaticTree._static_l_desc = null;
		com_jtransc_compression_jzlib_StaticTree.com_jtransc_compression_jzlib_StaticTree_clinit___V();
	}
}
class com_jtransc_compression_jzlib_Deflate_Config extends java_lang_Object  {

	int _nice_length = 0;
	int _max_chain = 0;
	int _max_lazy = 0;
	int _good_length = 0;
	int _func = 0;
	 com_jtransc_compression_jzlib_Deflate_Config com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(int p0, int p1, int p2, int p3, int p4) {
		this.java_lang_Object_init___V();
		this._good_length = p0;
		this._max_lazy = p1;
		this._nice_length = p2;
		this._max_chain = p3;
		this._func = p4;
		return this;
		return this;
	}
	com_jtransc_compression_jzlib_Deflate_Config([int CLASS_ID = 906]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_compression_jzlib_GZIPHeader extends java_lang_Object implements java_lang_Cloneable {

	bool _text = false;
	bool _fhcrc = false;
	Int64 _mtime = N.lnew(0);
	int _os = 0;
	bool _done = false;
	JA_B _comment = null;
	JA_B _extra = null;
	JA_B _name = null;
	Int64 _crc = N.lnew(0);
	 com_jtransc_compression_jzlib_GZIPHeader com_jtransc_compression_jzlib_GZIPHeader_init___V() {
		this.java_lang_Object_init___V();
		this._text = false;
		this._fhcrc = false;
		this._os = 255;
		this._done = false;
		this._mtime = N.lnew(0);
		return this;
		return this;
	}
	 java_lang_Object clone__Ljava_lang_Object_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = ((super.clone__Ljava_lang_Object_()) as com_jtransc_compression_jzlib_GZIPHeader);
					if ((((lA1 as com_jtransc_compression_jzlib_GZIPHeader)._extra == null))) {
						G = 1;
						continue;
					}
					lA2 = new JA_B(((lA1 as com_jtransc_compression_jzlib_GZIPHeader)._extra as JA_0).length);
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V((lA1 as com_jtransc_compression_jzlib_GZIPHeader)._extra, 0, lA2, 0, (lA2 as JA_0).length);
					(lA1 as com_jtransc_compression_jzlib_GZIPHeader)._extra = (lA2 as JA_B);
					G = 1;
					continue;
				case 1:
					if ((((lA1 as com_jtransc_compression_jzlib_GZIPHeader)._name == null))) {
						G = 2;
						continue;
					}
					lA2 = new JA_B(((lA1 as com_jtransc_compression_jzlib_GZIPHeader)._name as JA_0).length);
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V((lA1 as com_jtransc_compression_jzlib_GZIPHeader)._name, 0, lA2, 0, (lA2 as JA_0).length);
					(lA1 as com_jtransc_compression_jzlib_GZIPHeader)._name = (lA2 as JA_B);
					G = 2;
					continue;
				case 2:
					if ((((lA1 as com_jtransc_compression_jzlib_GZIPHeader)._comment == null))) {
						G = 3;
						continue;
					}
					lA2 = new JA_B(((lA1 as com_jtransc_compression_jzlib_GZIPHeader)._comment as JA_0).length);
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V((lA1 as com_jtransc_compression_jzlib_GZIPHeader)._comment, 0, lA2, 0, (lA2 as JA_0).length);
					(lA1 as com_jtransc_compression_jzlib_GZIPHeader)._comment = (lA2 as JA_B);
					G = 3;
					continue;
				case 3:
					return lA1;
				default:
					break;
			}
		}
		return null;
	}
	 void put_Lcom_jtransc_compression_jzlib_Deflate__V(com_jtransc_compression_jzlib_Deflate p0) {
		int G = 0;
		int lI2 = 0;
		int lI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI2 = 0;
					if (!(this._text)) {
						G = 1;
						continue;
					}
					lI2 = (N.I(lI2 | 1));
					G = 1;
					continue;
				case 1:
					if (!(this._fhcrc)) {
						G = 2;
						continue;
					}
					lI2 = (N.I(lI2 | 2));
					G = 2;
					continue;
				case 2:
					if (((this._extra == null))) {
						G = 3;
						continue;
					}
					lI2 = (N.I(lI2 | 4));
					G = 3;
					continue;
				case 3:
					if (((this._name == null))) {
						G = 4;
						continue;
					}
					lI2 = (N.I(lI2 | 8));
					G = 4;
					continue;
				case 4:
					if (((this._comment == null))) {
						G = 5;
						continue;
					}
					lI2 = (N.I(lI2 | 16));
					G = 5;
					continue;
				case 5:
					lI3 = 0;
					if (((p0._level != 1))) {
						G = 6;
						continue;
					}
					lI3 = (N.I(lI3 | 4));
					G = 7;
					continue;
				case 6:
					if (((p0._level != 9))) {
						G = 7;
						continue;
					}
					lI3 = (N.I(lI3 | 2));
					G = 7;
					continue;
				case 7:
					p0.put_short_I_V(-29921);
					p0.put_byte_B_V(8);
					p0.put_byte_B_V(N.i2b(lI2));
					p0.put_byte_B_V(N.i2b(N.j2i(this._mtime)));
					p0.put_byte_B_V(N.i2b(N.j2i(((this._mtime >> 8)))));
					p0.put_byte_B_V(N.i2b(N.j2i(((this._mtime >> 16)))));
					p0.put_byte_B_V(N.i2b(N.j2i(((this._mtime >> 24)))));
					p0.put_byte_B_V(N.i2b(lI3));
					p0.put_byte_B_V(N.i2b(this._os));
					if (((this._extra == null))) {
						G = 8;
						continue;
					}
					p0.put_byte_B_V(N.i2b((this._extra as JA_0).length));
					p0.put_byte_B_V(N.i2b((N.I((this._extra as JA_0).length >> 8))));
					p0.put_byte__BII_V(this._extra, 0, (this._extra as JA_0).length);
					G = 8;
					continue;
				case 8:
					if (((this._name == null))) {
						G = 9;
						continue;
					}
					p0.put_byte__BII_V(this._name, 0, (this._name as JA_0).length);
					p0.put_byte_B_V(0);
					G = 9;
					continue;
				case 9:
					if (((this._comment == null))) {
						G = 10;
						continue;
					}
					p0.put_byte__BII_V(this._comment, 0, (this._comment as JA_0).length);
					p0.put_byte_B_V(0);
					G = 10;
					continue;
				case 10:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void setCRC_J_V(Int64 p0) {
		this._crc = p0;
		return;
	}
	com_jtransc_compression_jzlib_GZIPHeader([int CLASS_ID = 905]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_compression_jzlib_Tree extends java_lang_Object  {

	static JA_I _base_dist = null;
	static JA_I _extra_lbits = null;
	static JA_B __dist_code = null;
	static JA_I _extra_dbits = null;
	static JA_B __length_code = null;
	static JA_I _base_length = null;
	static JA_I _extra_blbits = null;
	static JA_B _bl_order = null;
	JA_S _dyn_tree = null;
	int _max_code = 0;
	com_jtransc_compression_jzlib_StaticTree _stat_desc = null;
	 com_jtransc_compression_jzlib_Tree com_jtransc_compression_jzlib_Tree_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void com_jtransc_compression_jzlib_Tree_clinit___V() {
		java_lang_Object tA97 = null;
		java_lang_Object tA609 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA29 = null;
		java_lang_Object tA59 = null;
		java_lang_Object tA78 = null;
		java_lang_Object tA865 = null;
		java_lang_Object tA894 = null;
		java_lang_Object tA0 = null;
		tA0 = new JA_I(29);
		fA0 = tA0;
		(tA0 as JA_I).data[0] = 0;
		(fA0 as JA_I).setArraySlice(1, [ 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0 ]);
		com_jtransc_compression_jzlib_Tree._extra_lbits = (fA0 as JA_I);
		tA29 = new JA_I(30);
		fA0 = tA29;
		(tA29 as JA_I).data[0] = 0;
		(fA0 as JA_I).setArraySlice(1, [ 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 ]);
		com_jtransc_compression_jzlib_Tree._extra_dbits = (fA0 as JA_I);
		tA59 = new JA_I(19);
		fA0 = tA59;
		(tA59 as JA_I).data[0] = 0;
		(fA0 as JA_I).setArraySlice(1, [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 7 ]);
		com_jtransc_compression_jzlib_Tree._extra_blbits = (fA0 as JA_I);
		tA78 = new JA_B(19);
		fA0 = tA78;
		(tA78 as JA_B).data[0] = 16;
		(fA0 as JA_B).setArraySlice(1, [ 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 ]);
		com_jtransc_compression_jzlib_Tree._bl_order = (fA0 as JA_B);
		tA97 = new JA_B(512);
		fA0 = tA97;
		(tA97 as JA_B).data[0] = 0;
		(fA0 as JA_B).setArraySlice(1, [ 1, 2, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 0, 0, 16, 17, 18, 18, 19, 19, 20, 20, 20, 20, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29 ]);
		com_jtransc_compression_jzlib_Tree.__dist_code = (fA0 as JA_B);
		tA609 = new JA_B(256);
		fA0 = tA609;
		(tA609 as JA_B).data[0] = 0;
		(fA0 as JA_B).setArraySlice(1, [ 1, 2, 3, 4, 5, 6, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 12, 12, 13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28 ]);
		com_jtransc_compression_jzlib_Tree.__length_code = (fA0 as JA_B);
		tA865 = new JA_I(29);
		fA0 = tA865;
		(tA865 as JA_I).data[0] = 0;
		(fA0 as JA_I).setArraySlice(1, [ 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 20, 24, 28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 0 ]);
		com_jtransc_compression_jzlib_Tree._base_length = (fA0 as JA_I);
		tA894 = new JA_I(30);
		fA0 = tA894;
		(tA894 as JA_I).data[0] = 0;
		(fA0 as JA_I).setArraySlice(1, [ 1, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 64, 96, 128, 192, 256, 384, 512, 768, 1024, 1536, 2048, 3072, 4096, 6144, 8192, 12288, 16384, 24576 ]);
		com_jtransc_compression_jzlib_Tree._base_dist = (fA0 as JA_I);
		return;
	}
	 void build_tree_Lcom_jtransc_compression_jzlib_Deflate__V(com_jtransc_compression_jzlib_Deflate p0) {
		java_lang_Object lA3 = null;
		int lI4 = 0;
		int lI8 = 0;
		int lI6 = 0;
		java_lang_Object fA1 = null;
		java_lang_Object fA3 = null;
		int tI21 = 0;
		int tI25 = 0;
		java_lang_Object tA9 = null;
		int fI2 = 0;
		int fI0 = 0;
		java_lang_Object tA20 = null;
		java_lang_Object tA26 = null;
		int tI8 = 0;
		java_lang_Object tA14 = null;
		int tI16 = 0;
		int G = 0;
		java_lang_Object lA2 = null;
		int lI7 = 0;
		int lI5 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA2 = null;
		int tI22 = 0;
		java_lang_Object tA2 = null;
		java_lang_Object tA6 = null;
		int fI1 = 0;
		int fI3 = 0;
		java_lang_Object tA23 = null;
		int tI1 = 0;
		int tI3 = 0;
		int tI5 = 0;
		int tI7 = 0;
		int tI13 = 0;
		java_lang_Object tA17 = null;
		int tI19 = 0;
		while (true) {
			switch (G) {
				case 0:
					lA2 = this._dyn_tree;
					lA3 = this._stat_desc._static_tree;
					lI4 = this._stat_desc._elems;
					lI7 = -1;
					p0._heap_len = 0;
					p0._heap_max = 573;
					lI5 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI5 >= lI4))) {
						G = 2;
						continue;
					}
					if (((((lA2 as JA_S)).data[(N.I(lI5 * 2))] == 0))) {
						G = 3;
						continue;
					}
					fA0 = p0._heap;
					fA1 = p0;
					tA2 = fA1;
					tI1 = (N.I(p0._heap_len + 1));
					fI1 = tI1;
					(tA2 as com_jtransc_compression_jzlib_Deflate)._heap_len = tI1;
					tI3 = lI5;
					fI2 = tI3;
					lI7 = tI3;
					(fA0 as JA_I).data[fI1] = fI2;
					p0._depth.data[lI5] = 0;
					G = 4;
					continue;
				case 3:
					(lA2 as JA_S).data[(N.I((N.I(lI5 * 2)) + 1))] = 0;
					G = 4;
					continue;
				case 4:
					lI5 = (N.I(lI5 + 1));
					G = 1;
					continue;
				case 2:
					if (((p0._heap_len >= 2))) {
						G = 5;
						continue;
					}
					fA0 = p0._heap;
					fA1 = p0;
					tA6 = fA1;
					tI5 = (N.I(p0._heap_len + 1));
					fI1 = tI5;
					(tA6 as com_jtransc_compression_jzlib_Deflate)._heap_len = tI5;
					if (((lI7 >= 2))) {
						G = 6;
						continue;
					}
					lI7 = (N.I(lI7 + 1));
					fI2 = lI7;
					G = 7;
					continue;
				case 6:
					fI2 = 0;
					G = 7;
					continue;
				case 7:
					tA9 = fA0;
					tI8 = fI1;
					tI7 = fI2;
					fI0 = tI7;
					(tA9 as JA_I).data[tI8] = tI7;
					lI8 = fI0;
					(lA2 as JA_S).data[(N.I(lI8 * 2))] = 1;
					p0._depth.data[lI8] = 0;
					p0._opt_len = (N.I(p0._opt_len - 1));
					if (((lA3 == null))) {
						G = 2;
						continue;
					}
					p0._static_len = (N.I(p0._static_len - ((lA3 as JA_S)).data[(N.I((N.I(lI8 * 2)) + 1))]));
					G = 2;
					continue;
				case 5:
					this._max_code = lI7;
					lI5 = (N.I(p0._heap_len ~/ 2));
					G = 8;
					continue;
				case 8:
					if (((lI5 < 1))) {
						G = 9;
						continue;
					}
					p0.pqdownheap__SI_V((lA2 as JA_S), lI5);
					lI5 = (N.I(lI5 + -1));
					G = 8;
					continue;
				case 9:
					lI8 = lI4;
					G = 10;
					continue;
				case 10:
					lI5 = (p0._heap).data[1];
					fA0 = p0._heap;
					fI1 = 1;
					fA2 = p0._heap;
					fA3 = p0;
					tA14 = fA3;
					tI13 = p0._heap_len;
					fI3 = tI13;
					(tA14 as com_jtransc_compression_jzlib_Deflate)._heap_len = (N.I(tI13 - 1));
					(fA0 as JA_I).data[fI1] = ((fA2 as JA_I)).data[fI3];
					p0.pqdownheap__SI_V((lA2 as JA_S), 1);
					lI6 = (p0._heap).data[1];
					fA0 = p0._heap;
					fA1 = p0;
					tA17 = fA1;
					tI16 = (N.I(p0._heap_max - 1));
					fI1 = tI16;
					(tA17 as com_jtransc_compression_jzlib_Deflate)._heap_max = tI16;
					(fA0 as JA_I).data[fI1] = lI5;
					fA0 = p0._heap;
					fA1 = p0;
					tA20 = fA1;
					tI19 = (N.I(p0._heap_max - 1));
					fI1 = tI19;
					(tA20 as com_jtransc_compression_jzlib_Deflate)._heap_max = tI19;
					(fA0 as JA_I).data[fI1] = lI6;
					(lA2 as JA_S).data[(N.I(lI8 * 2))] = N.i2s((N.I(((lA2 as JA_S)).data[(N.I(lI5 * 2))] + ((lA2 as JA_S)).data[(N.I(lI6 * 2))])));
					p0._depth.data[lI8] = N.i2b((N.I(java_lang_Math.max_II_I(((p0._depth).data[lI5]), ((p0._depth).data[lI6])) + 1)));
					fA0 = lA2;
					fI1 = (N.I((N.I(lI5 * 2)) + 1));
					fA2 = lA2;
					fI3 = (N.I((N.I(lI6 * 2)) + 1));
					tA23 = fA2;
					tI22 = fI3;
					tI21 = (N.i2s(lI8));
					fI2 = tI21;
					(tA23 as JA_S).data[tI22] = N.i2s(tI21);
					(fA0 as JA_S).data[fI1] = N.i2s(fI2);
					fA0 = p0._heap;
					fI1 = 1;
					fI2 = lI8;
					lI8 = (N.I(lI8 + 1));
					(fA0 as JA_I).data[fI1] = fI2;
					p0.pqdownheap__SI_V((lA2 as JA_S), 1);
					if (((p0._heap_len >= 2))) {
						G = 10;
						continue;
					}
					fA0 = p0._heap;
					fA1 = p0;
					tA26 = fA1;
					tI25 = (N.I(p0._heap_max - 1));
					fI1 = tI25;
					(tA26 as com_jtransc_compression_jzlib_Deflate)._heap_max = tI25;
					(fA0 as JA_I).data[fI1] = (p0._heap).data[1];
					this.gen_bitlen_Lcom_jtransc_compression_jzlib_Deflate__V(p0);
					com_jtransc_compression_jzlib_Tree.gen_codes__SI_S_S_V((lA2 as JA_S), lI7, p0._bl_count, p0._next_code);
					return;
				default:
					break;
			}
		}
		return;
	}
	 void gen_bitlen_Lcom_jtransc_compression_jzlib_Deflate__V(com_jtransc_compression_jzlib_Deflate p0) {
		int G = 0;
		java_lang_Object lA3 = null;
		int lI13 = 0;
		int lI11 = 0;
		int lI12 = 0;
		int lI5 = 0;
		int lI6 = 0;
		int lI10 = 0;
		int lI7 = 0;
		int lI8 = 0;
		int lI9 = 0;
		JA_S tA1 = null;
		JA_S tA5 = null;
		JA_S tA7 = null;
		JA_S tA9 = null;
		JA_S lA2 = null;
		int tI0 = 0;
		int tI4 = 0;
		int tI6 = 0;
		int tI8 = 0;
		JA_I fA0 = null;
		JA_I lA4 = null;
		while (true) {
			switch (G) {
				case 0:
					lA2 = this._dyn_tree;
					lA3 = this._stat_desc._static_tree;
					lA4 = this._stat_desc._extra_bits;
					lI5 = this._stat_desc._extra_base;
					lI6 = this._stat_desc._max_length;
					lI13 = 0;
					lI10 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI10 > 15))) {
						G = 2;
						continue;
					}
					p0._bl_count.data[lI10] = 0;
					lI10 = (N.I(lI10 + 1));
					G = 1;
					continue;
				case 2:
					lA2.data[(N.I((N.I((p0._heap).data[p0._heap_max] * 2)) + 1))] = 0;
					lI7 = (N.I(p0._heap_max + 1));
					G = 3;
					continue;
				case 3:
					if (((lI7 >= 573))) {
						G = 4;
						continue;
					}
					lI8 = (p0._heap).data[lI7];
					lI10 = (N.I((lA2).data[(N.I((N.I((lA2).data[(N.I((N.I(lI8 * 2)) + 1))] * 2)) + 1))] + 1));
					if (((lI10 <= lI6))) {
						G = 5;
						continue;
					}
					lI10 = lI6;
					lI13 = (N.I(lI13 + 1));
					G = 5;
					continue;
				case 5:
					lA2.data[(N.I((N.I(lI8 * 2)) + 1))] = N.i2s(lI10);
					if (((lI8 <= this._max_code))) {
						G = 6;
						continue;
					}
					G = 7;
					continue;
				case 6:
					tA1 = p0._bl_count;
					tI0 = lI10;
					tA1.data[tI0] = N.i2s((N.I((tA1).data[tI0] + 1)));
					lI11 = 0;
					if (((lI8 < lI5))) {
						G = 8;
						continue;
					}
					lI11 = (lA4).data[(N.I(lI8 - lI5))];
					G = 8;
					continue;
				case 8:
					lI12 = ((lA2).data[(N.I(lI8 * 2))]);
					p0._opt_len = (N.I(p0._opt_len + (N.I(lI12 * (N.I(lI10 + lI11))))));
					if (((lA3 == null))) {
						G = 7;
						continue;
					}
					p0._static_len = (N.I(p0._static_len + (N.I(lI12 * (N.I(((lA3 as JA_S)).data[(N.I((N.I(lI8 * 2)) + 1))] + lI11))))));
					G = 7;
					continue;
				case 7:
					lI7 = (N.I(lI7 + 1));
					G = 3;
					continue;
				case 4:
					if (((lI13 != 0))) {
						G = 9;
						continue;
					}
					return;
					G = 9;
					continue;
				case 9:
					lI10 = (N.I(lI6 - 1));
					G = 10;
					continue;
				case 10:
					if ((((p0._bl_count).data[lI10] != 0))) {
						G = 11;
						continue;
					}
					lI10 = (N.I(lI10 + -1));
					G = 10;
					continue;
				case 11:
					tA5 = p0._bl_count;
					tI4 = lI10;
					tA5.data[tI4] = N.i2s((N.I((tA5).data[tI4] - 1)));
					tA7 = p0._bl_count;
					tI6 = (N.I(lI10 + 1));
					tA7.data[tI6] = N.i2s((N.I((tA7).data[tI6] + 2)));
					tA9 = p0._bl_count;
					tI8 = lI6;
					tA9.data[tI8] = N.i2s((N.I((tA9).data[tI8] - 1)));
					lI13 = (N.I(lI13 + -2));
					if (((lI13 > 0))) {
						G = 9;
						continue;
					}
					lI10 = lI6;
					G = 12;
					continue;
				case 12:
					if (((lI10 == 0))) {
						G = 13;
						continue;
					}
					lI8 = ((p0._bl_count).data[lI10]);
					G = 14;
					continue;
				case 14:
					if (((lI8 == 0))) {
						G = 15;
						continue;
					}
					fA0 = p0._heap;
					lI7 = (N.I(lI7 + -1));
					lI9 = (fA0).data[lI7];
					if (((lI9 <= this._max_code))) {
						G = 16;
						continue;
					}
					G = 14;
					continue;
				case 16:
					if ((((lA2).data[(N.I((N.I(lI9 * 2)) + 1))] == lI10))) {
						G = 17;
						continue;
					}
					p0._opt_len = N.j2i(((N.i2j(p0._opt_len)+((((N.i2j(lI10)-N.i2j((lA2).data[(N.I((N.I(lI9 * 2)) + 1))])))*N.i2j((lA2).data[(N.I(lI9 * 2))]))))));
					lA2.data[(N.I((N.I(lI9 * 2)) + 1))] = N.i2s(lI10);
					G = 17;
					continue;
				case 17:
					lI8 = (N.I(lI8 + -1));
					G = 14;
					continue;
				case 15:
					lI10 = (N.I(lI10 + -1));
					G = 12;
					continue;
				case 13:
					return;
				default:
					break;
			}
		}
		return;
	}
	static void gen_codes__SI_S_S_V(JA_S p0, int p1, JA_S p2, JA_S p3) {
		int G = 0;
		int lI4 = 0;
		int lI5 = 0;
		int lI6 = 0;
		int lI7 = 0;
		java_lang_Object fA2 = null;
		java_lang_Object tA5 = null;
		JA_S fA0 = null;
		int fI1 = 0;
		int fI2 = 0;
		int fI3 = 0;
		int tI0 = 0;
		int tI1 = 0;
		int tI4 = 0;
		int tI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI4 = 0;
					p3.data[0] = 0;
					lI5 = 1;
					G = 1;
					continue;
				case 1:
					if (((lI5 > 15))) {
						G = 2;
						continue;
					}
					fA0 = p3;
					fI1 = lI5;
					tI0 = (N.i2s((N.I((N.I(lI4 + (p2).data[(N.I(lI5 - 1))])) << 1))));
					fI2 = tI0;
					lI4 = tI0;
					fA0.data[fI1] = N.i2s(fI2);
					lI5 = (N.I(lI5 + 1));
					G = 1;
					continue;
				case 2:
					lI6 = 0;
					G = 3;
					continue;
				case 3:
					if (((lI6 > p1))) {
						G = 4;
						continue;
					}
					lI7 = ((p0).data[(N.I((N.I(lI6 * 2)) + 1))]);
					if (((lI7 != 0))) {
						G = 5;
						continue;
					}
					G = 6;
					continue;
				case 5:
					fA0 = p0;
					fI1 = (N.I(lI6 * 2));
					tI1 = lI7;
					fA2 = p3;
					fI3 = tI1;
					tA5 = fA2;
					tI4 = fI3;
					tI3 = ((p3).data[tI1]);
					fI2 = tI3;
					(tA5 as JA_S).data[tI4] = N.i2s((N.I(tI3 + 1)));
					fA0.data[fI1] = N.i2s(com_jtransc_compression_jzlib_Tree.bi_reverse_II_I(fI2, lI7));
					G = 6;
					continue;
				case 6:
					lI6 = (N.I(lI6 + 1));
					G = 3;
					continue;
				case 4:
					return;
				default:
					break;
			}
		}
		return;
	}
	static int bi_reverse_II_I(int p0, int p1) {
		int G = 0;
		int lI0 = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI0 = p0;
					lI1 = p1;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					lI2 = (N.I(lI2 | (N.I(lI0 & 1))));
					lI0 = (N.iushr_opt(lI0, 1));
					lI2 = (N.I(lI2 << 1));
					lI1 = (N.I(lI1 + -1));
					if (((lI1 > 0))) {
						G = 1;
						continue;
					}
					return (N.iushr_opt(lI2, 1));
				default:
					break;
			}
		}
		return 0;
	}
	static int d_code_I_I(int p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 >= 256))) {
						G = 1;
						continue;
					}
					fI0 = ((com_jtransc_compression_jzlib_Tree.__dist_code).data[p0]);
					G = 2;
					continue;
				case 1:
					fI0 = ((com_jtransc_compression_jzlib_Tree.__dist_code).data[(N.I(256 + (N.iushr_opt(p0, 7))))]);
					G = 2;
					continue;
				case 2: return fI0;
				default:
					break;
			}
		}
		return 0;
	}
	com_jtransc_compression_jzlib_Tree([int CLASS_ID = 904]) : super(CLASS_ID) { }
	static void SI() {
		com_jtransc_compression_jzlib_Tree._base_dist = null;
		com_jtransc_compression_jzlib_Tree._extra_lbits = null;
		com_jtransc_compression_jzlib_Tree.__dist_code = null;
		com_jtransc_compression_jzlib_Tree._extra_dbits = null;
		com_jtransc_compression_jzlib_Tree.__length_code = null;
		com_jtransc_compression_jzlib_Tree._base_length = null;
		com_jtransc_compression_jzlib_Tree._extra_blbits = null;
		com_jtransc_compression_jzlib_Tree._bl_order = null;
		com_jtransc_compression_jzlib_Tree.com_jtransc_compression_jzlib_Tree_clinit___V();
	}
}
class com_jtransc_compression_jzlib_Deflate extends java_lang_Object implements java_lang_Cloneable {

	com_jtransc_compression_jzlib_Tree _bl_desc = null;
	com_jtransc_compression_jzlib_Tree _l_desc = null;
	JA_S _dyn_ltree = null;
	JA_S _next_code = null;
	JA_B _depth = null;
	com_jtransc_compression_jzlib_ZStream _strm = null;
	JA_S _dyn_dtree = null;
	com_jtransc_compression_jzlib_Tree _d_desc = null;
	com_jtransc_compression_jzlib_GZIPHeader _gheader = null;
	int _wrap = 0;
	JA_S _bl_count = null;
	JA_I _heap = null;
	JA_S _bl_tree = null;
	static JA_L _z_errmsg = null;
	static JA_L _config_table = null;
	JA_S _prev = null;
	int _d_buf = 0;
	JA_B _pending_buf = null;
	JA_S _head = null;
	JA_B _l_buf = null;
	JA_B _window = null;
	int _pending = 0;
	int _level = 0;
	int _lookahead = 0;
	int _last_flush = 0;
	int _w_bits = 0;
	int _hash_size = 0;
	int _status = 0;
	int _strstart = 0;
	int _pending_out = 0;
	int _max_lazy_match = 0;
	int _w_size = 0;
	int _match_start = 0;
	int _strategy = 0;
	int _hash_mask = 0;
	int _match_available = 0;
	int _ins_h = 0;
	int _hash_shift = 0;
	int _w_mask = 0;
	int _prev_match = 0;
	int _match_length = 0;
	int _prev_length = 0;
	int _block_start = 0;
	int _static_len = 0;
	int _data_type = 0;
	int _opt_len = 0;
	int _bi_valid = 0;
	int _bi_buf = 0;
	int _matches = 0;
	int _last_lit = 0;
	int _heap_len = 0;
	int _heap_max = 0;
	int _last_eob_len = 0;
	int _max_chain_length = 0;
	int _nice_match = 0;
	int _good_match = 0;
	int _window_size = 0;
	int _lit_bufsize = 0;
	int _pending_buf_size = 0;
	int _hash_bits = 0;
	int _method = 0;
	 com_jtransc_compression_jzlib_Deflate com_jtransc_compression_jzlib_Deflate_init__Lcom_jtransc_compression_jzlib_ZStream__V(com_jtransc_compression_jzlib_ZStream p0) {
		java_lang_Object fA1 = null;
		com_jtransc_compression_jzlib_Deflate fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		this.java_lang_Object_init___V();
		this._wrap = 1;
		fA0 = this;
		tA0 = (new com_jtransc_compression_jzlib_Tree());
		fA1 = tA0;
		(tA0 as com_jtransc_compression_jzlib_Tree).com_jtransc_compression_jzlib_Tree_init___V();
		fA0._l_desc = (fA1 as com_jtransc_compression_jzlib_Tree);
		fA0 = this;
		tA1 = (new com_jtransc_compression_jzlib_Tree());
		fA1 = tA1;
		(tA1 as com_jtransc_compression_jzlib_Tree).com_jtransc_compression_jzlib_Tree_init___V();
		fA0._d_desc = (fA1 as com_jtransc_compression_jzlib_Tree);
		fA0 = this;
		tA2 = (new com_jtransc_compression_jzlib_Tree());
		fA1 = tA2;
		(tA2 as com_jtransc_compression_jzlib_Tree).com_jtransc_compression_jzlib_Tree_init___V();
		fA0._bl_desc = (fA1 as com_jtransc_compression_jzlib_Tree);
		this._bl_count = new JA_S(16);
		this._next_code = new JA_S(16);
		this._heap = new JA_I(573);
		this._depth = new JA_B(573);
		this._gheader = null;
		this._strm = p0;
		this._dyn_ltree = new JA_S(1146);
		this._dyn_dtree = new JA_S(122);
		this._bl_tree = new JA_S(78);
		return this;
		return this;
	}
	static void com_jtransc_compression_jzlib_Deflate_clinit___V() {
		java_lang_Object tA1 = null;
		java_lang_Object tA3 = null;
		java_lang_Object tA5 = null;
		java_lang_Object tA7 = null;
		java_lang_Object tA9 = null;
		java_lang_Object tA10 = null;
		java_lang_Object fA0 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA2 = null;
		java_lang_Object tA4 = null;
		java_lang_Object tA6 = null;
		java_lang_Object tA8 = null;
		int fI1 = 0;
		com_jtransc_compression_jzlib_Deflate._config_table = new JA_L(10, "[Lcom.jtransc.compression.jzlib.Deflate\$Config;");
		fA0 = com_jtransc_compression_jzlib_Deflate._config_table;
		fI1 = 0;
		tA0 = (new com_jtransc_compression_jzlib_Deflate_Config());
		fA2 = tA0;
		(tA0 as com_jtransc_compression_jzlib_Deflate_Config).com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(0, 0, 0, 0, 0);
		(fA0 as JA_L).data[fI1] = fA2;
		fA0 = com_jtransc_compression_jzlib_Deflate._config_table;
		fI1 = 1;
		tA1 = (new com_jtransc_compression_jzlib_Deflate_Config());
		fA2 = tA1;
		(tA1 as com_jtransc_compression_jzlib_Deflate_Config).com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(4, 4, 8, 4, 1);
		(fA0 as JA_L).data[fI1] = fA2;
		fA0 = com_jtransc_compression_jzlib_Deflate._config_table;
		fI1 = 2;
		tA2 = (new com_jtransc_compression_jzlib_Deflate_Config());
		fA2 = tA2;
		(tA2 as com_jtransc_compression_jzlib_Deflate_Config).com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(4, 5, 16, 8, 1);
		(fA0 as JA_L).data[fI1] = fA2;
		fA0 = com_jtransc_compression_jzlib_Deflate._config_table;
		fI1 = 3;
		tA3 = (new com_jtransc_compression_jzlib_Deflate_Config());
		fA2 = tA3;
		(tA3 as com_jtransc_compression_jzlib_Deflate_Config).com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(4, 6, 32, 32, 1);
		(fA0 as JA_L).data[fI1] = fA2;
		fA0 = com_jtransc_compression_jzlib_Deflate._config_table;
		fI1 = 4;
		tA4 = (new com_jtransc_compression_jzlib_Deflate_Config());
		fA2 = tA4;
		(tA4 as com_jtransc_compression_jzlib_Deflate_Config).com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(4, 4, 16, 16, 2);
		(fA0 as JA_L).data[fI1] = fA2;
		fA0 = com_jtransc_compression_jzlib_Deflate._config_table;
		fI1 = 5;
		tA5 = (new com_jtransc_compression_jzlib_Deflate_Config());
		fA2 = tA5;
		(tA5 as com_jtransc_compression_jzlib_Deflate_Config).com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(8, 16, 32, 32, 2);
		(fA0 as JA_L).data[fI1] = fA2;
		fA0 = com_jtransc_compression_jzlib_Deflate._config_table;
		fI1 = 6;
		tA6 = (new com_jtransc_compression_jzlib_Deflate_Config());
		fA2 = tA6;
		(tA6 as com_jtransc_compression_jzlib_Deflate_Config).com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(8, 16, 128, 128, 2);
		(fA0 as JA_L).data[fI1] = fA2;
		fA0 = com_jtransc_compression_jzlib_Deflate._config_table;
		fI1 = 7;
		tA7 = (new com_jtransc_compression_jzlib_Deflate_Config());
		fA2 = tA7;
		(tA7 as com_jtransc_compression_jzlib_Deflate_Config).com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(8, 32, 128, 256, 2);
		(fA0 as JA_L).data[fI1] = fA2;
		fA0 = com_jtransc_compression_jzlib_Deflate._config_table;
		fI1 = 8;
		tA8 = (new com_jtransc_compression_jzlib_Deflate_Config());
		fA2 = tA8;
		(tA8 as com_jtransc_compression_jzlib_Deflate_Config).com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(32, 128, 258, 1024, 2);
		(fA0 as JA_L).data[fI1] = fA2;
		fA0 = com_jtransc_compression_jzlib_Deflate._config_table;
		fI1 = 9;
		tA9 = (new com_jtransc_compression_jzlib_Deflate_Config());
		fA2 = tA9;
		(tA9 as com_jtransc_compression_jzlib_Deflate_Config).com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(32, 258, 258, 4096, 2);
		(fA0 as JA_L).data[fI1] = fA2;
		tA10 = new JA_L(10, "[Ljava.lang.String;");
		fA0 = tA10;
		(tA10 as JA_L).data[0] = Bootstrap.STRINGLIT_18;
		(fA0 as JA_L).setArraySlice(1, [ Bootstrap.STRINGLIT_19, Bootstrap.STRINGLIT_20, Bootstrap.STRINGLIT_21, Bootstrap.STRINGLIT_22, Bootstrap.STRINGLIT_23, Bootstrap.STRINGLIT_24, Bootstrap.STRINGLIT_25, Bootstrap.STRINGLIT_26, Bootstrap.STRINGLIT_20 ]);
		com_jtransc_compression_jzlib_Deflate._z_errmsg = (fA0 as JA_L);
		return;
	}
	 java_lang_Object clone__Ljava_lang_Object_() {
		int G = 0;
		java_lang_Object lA1 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = ((super.clone__Ljava_lang_Object_()) as com_jtransc_compression_jzlib_Deflate);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._pending_buf = this.dup__B__B((lA1 as com_jtransc_compression_jzlib_Deflate)._pending_buf);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._l_buf = this.dup__B__B((lA1 as com_jtransc_compression_jzlib_Deflate)._l_buf);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._window = this.dup__B__B((lA1 as com_jtransc_compression_jzlib_Deflate)._window);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._prev = this.dup__S__S((lA1 as com_jtransc_compression_jzlib_Deflate)._prev);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._head = this.dup__S__S((lA1 as com_jtransc_compression_jzlib_Deflate)._head);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._dyn_ltree = this.dup__S__S((lA1 as com_jtransc_compression_jzlib_Deflate)._dyn_ltree);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._dyn_dtree = this.dup__S__S((lA1 as com_jtransc_compression_jzlib_Deflate)._dyn_dtree);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._bl_tree = this.dup__S__S((lA1 as com_jtransc_compression_jzlib_Deflate)._bl_tree);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._bl_count = this.dup__S__S((lA1 as com_jtransc_compression_jzlib_Deflate)._bl_count);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._next_code = this.dup__S__S((lA1 as com_jtransc_compression_jzlib_Deflate)._next_code);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._heap = this.dup__I__I((lA1 as com_jtransc_compression_jzlib_Deflate)._heap);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._depth = this.dup__B__B((lA1 as com_jtransc_compression_jzlib_Deflate)._depth);
					(lA1 as com_jtransc_compression_jzlib_Deflate)._l_desc._dyn_tree = (lA1 as com_jtransc_compression_jzlib_Deflate)._dyn_ltree;
					(lA1 as com_jtransc_compression_jzlib_Deflate)._d_desc._dyn_tree = (lA1 as com_jtransc_compression_jzlib_Deflate)._dyn_dtree;
					(lA1 as com_jtransc_compression_jzlib_Deflate)._bl_desc._dyn_tree = (lA1 as com_jtransc_compression_jzlib_Deflate)._bl_tree;
					if ((((lA1 as com_jtransc_compression_jzlib_Deflate)._gheader == null))) {
						G = 1;
						continue;
					}
					(lA1 as com_jtransc_compression_jzlib_Deflate)._gheader = (((lA1 as com_jtransc_compression_jzlib_Deflate)._gheader.clone__Ljava_lang_Object_()) as com_jtransc_compression_jzlib_GZIPHeader);
					G = 1;
					continue;
				case 1:
					return lA1;
				default:
					break;
			}
		}
		return null;
	}
	 JA_S dup__S__S(JA_S p0) {
		java_lang_Object lA2 = null;
		lA2 = new JA_S((p0 as JA_0).length);
		java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(p0, 0, lA2, 0, (lA2 as JA_0).length);
		return (lA2 as JA_S);
	}
	 JA_I dup__I__I(JA_I p0) {
		java_lang_Object lA2 = null;
		lA2 = new JA_I((p0 as JA_0).length);
		java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(p0, 0, lA2, 0, (lA2 as JA_0).length);
		return (lA2 as JA_I);
	}
	 JA_B dup__B__B(JA_B p0) {
		java_lang_Object lA2 = null;
		lA2 = new JA_B((p0 as JA_0).length);
		java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(p0, 0, lA2, 0, (lA2 as JA_0).length);
		return (lA2 as JA_B);
	}
	 int deflate_I_I(int p0) {
		int G = 0;
		int fI0 = 0;
		int lI2 = 0;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 > 4))) {
						G = 1;
						continue;
					}
					if (((p0 >= 0))) {
						G = 2;
						continue;
					}
					G = 1;
					continue;
				case 1:
					return -2;
				case 2:
					if (((this._strm._next_out == null))) {
						G = 3;
						continue;
					}
					if (((this._strm._next_in != null))) {
						G = 4;
						continue;
					}
					if (((this._strm._avail_in != 0))) {
						G = 3;
						continue;
					}
					G = 4;
					continue;
				case 4:
					if (((this._status != 666))) {
						G = 5;
						continue;
					}
					if (((p0 == 4))) {
						G = 5;
						continue;
					}
					G = 3;
					continue;
				case 3:
					this._strm._msg = (((com_jtransc_compression_jzlib_Deflate._z_errmsg as JA_L)).data[4] as java_lang_String);
					return -2;
				case 5:
					if (((this._strm._avail_out != 0))) {
						G = 6;
						continue;
					}
					this._strm._msg = (((com_jtransc_compression_jzlib_Deflate._z_errmsg as JA_L)).data[7] as java_lang_String);
					return -5;
				case 6:
					lI2 = this._last_flush;
					this._last_flush = p0;
					if (((this._status != 42))) {
						G = 7;
						continue;
					}
					if (((this._wrap != 2))) {
						G = 8;
						continue;
					}
					this.getGZIPHeader__Lcom_jtransc_compression_jzlib_GZIPHeader_().put_Lcom_jtransc_compression_jzlib_Deflate__V(this);
					this._status = 113;
					this._strm._adler.reset__V();
					G = 7;
					continue;
				case 8:
					lI3 = (N.I((N.I(8 + (N.I((N.I(this._w_bits - 8)) << 4)))) << 8));
					lI4 = (N.I((N.I((N.I(this._level - 1)) & 255)) >> 1));
					if (((lI4 <= 3))) {
						G = 9;
						continue;
					}
					lI4 = 3;
					G = 9;
					continue;
				case 9:
					lI3 = (N.I(lI3 | (N.I(lI4 << 6))));
					if (((this._strstart == 0))) {
						G = 10;
						continue;
					}
					lI3 = (N.I(lI3 | 32));
					G = 10;
					continue;
				case 10:
					lI3 = (N.I(lI3 + (N.I(31 - (N.I(lI3.remainder(31)))))));
					this._status = 113;
					this.putShortMSB_I_V(lI3);
					if (((this._strstart == 0))) {
						G = 11;
						continue;
					}
					lI5 = this._strm._adler.getValue__I();
					this.putShortMSB_I_V((N.iushr_opt(lI5, 16)));
					this.putShortMSB_I_V((N.I(lI5 & 65535)));
					G = 11;
					continue;
				case 11:
					this._strm._adler.reset__V();
					G = 7;
					continue;
				case 7:
					if (((this._pending == 0))) {
						G = 12;
						continue;
					}
					this._strm.flush_pending__V();
					if (((this._strm._avail_out != 0))) {
						G = 13;
						continue;
					}
					this._last_flush = -1;
					return 0;
				case 12:
					if (((this._strm._avail_in != 0))) {
						G = 13;
						continue;
					}
					if (((p0 > lI2))) {
						G = 13;
						continue;
					}
					if (((p0 == 4))) {
						G = 13;
						continue;
					}
					this._strm._msg = (((com_jtransc_compression_jzlib_Deflate._z_errmsg as JA_L)).data[7] as java_lang_String);
					return -5;
				case 13:
					if (((this._status != 666))) {
						G = 14;
						continue;
					}
					if (((this._strm._avail_in == 0))) {
						G = 14;
						continue;
					}
					this._strm._msg = (((com_jtransc_compression_jzlib_Deflate._z_errmsg as JA_L)).data[7] as java_lang_String);
					return -5;
				case 14:
					if (((this._strm._avail_in != 0))) {
						G = 15;
						continue;
					}
					if (((this._lookahead != 0))) {
						G = 15;
						continue;
					}
					if (((p0 == 0))) {
						G = 16;
						continue;
					}
					if (((this._status == 666))) {
						G = 16;
						continue;
					}
					G = 15;
					continue;
				case 15:
					lI3 = -1;
					switch ((((com_jtransc_compression_jzlib_Deflate._config_table as JA_L)).data[this._level] as com_jtransc_compression_jzlib_Deflate_Config)._func) {
						case 0:
							G = 18;
							continue;
						case 1:
							G = 19;
							continue;
						case 2:
							G = 20;
							continue;
						default:
							G = 17;
							continue;
					}
					G = 18;
					continue;
				case 18:
					lI3 = this.deflate_stored_I_I(p0);
					G = 17;
					continue;
				case 19:
					lI3 = this.deflate_fast_I_I(p0);
					G = 17;
					continue;
				case 20:
					lI3 = this.deflate_slow_I_I(p0);
					G = 17;
					continue;
				case 17:
					if (((lI3 == 2))) {
						G = 21;
						continue;
					}
					if (((lI3 != 3))) {
						G = 22;
						continue;
					}
					G = 21;
					continue;
				case 21:
					this._status = 666;
					G = 22;
					continue;
				case 22:
					if (((lI3 == 0))) {
						G = 23;
						continue;
					}
					if (((lI3 != 2))) {
						G = 24;
						continue;
					}
					G = 23;
					continue;
				case 23:
					if (((this._strm._avail_out != 0))) {
						G = 25;
						continue;
					}
					this._last_flush = -1;
					G = 25;
					continue;
				case 25:
					return 0;
				case 24:
					if (((lI3 != 1))) {
						G = 16;
						continue;
					}
					if (((p0 != 1))) {
						G = 26;
						continue;
					}
					this._tr_align__V();
					G = 27;
					continue;
				case 26:
					this._tr_stored_block_IIZ_V(0, 0, false);
					if (((p0 != 3))) {
						G = 27;
						continue;
					}
					lI4 = 0;
					G = 28;
					continue;
				case 28:
					if (((lI4 >= this._hash_size))) {
						G = 27;
						continue;
					}
					this._head.data[lI4] = 0;
					lI4 = (N.I(lI4 + 1));
					G = 28;
					continue;
				case 27:
					this._strm.flush_pending__V();
					if (((this._strm._avail_out != 0))) {
						G = 16;
						continue;
					}
					this._last_flush = -1;
					return 0;
				case 16:
					if (((p0 == 4))) {
						G = 29;
						continue;
					}
					return 0;
				case 29:
					if (((this._wrap > 0))) {
						G = 30;
						continue;
					}
					return 1;
				case 30:
					if (((this._wrap != 2))) {
						G = 31;
						continue;
					}
					lI3 = this._strm._adler.getValue__I();
					this.put_byte_B_V(N.i2b((N.I(lI3 & 255))));
					this.put_byte_B_V(N.i2b((N.I((N.I(lI3 >> 8)) & 255))));
					this.put_byte_B_V(N.i2b((N.I((N.I(lI3 >> 16)) & 255))));
					this.put_byte_B_V(N.i2b((N.I((N.I(lI3 >> 24)) & 255))));
					this.put_byte_B_V(N.i2b(N.j2i(((this._strm._total_in&N.lnew(255))))));
					this.put_byte_B_V(N.i2b(N.j2i(((((this._strm._total_in >> 8))&N.lnew(255))))));
					this.put_byte_B_V(N.i2b(N.j2i(((((this._strm._total_in >> 16))&N.lnew(255))))));
					this.put_byte_B_V(N.i2b(N.j2i(((((this._strm._total_in >> 24))&N.lnew(255))))));
					this.getGZIPHeader__Lcom_jtransc_compression_jzlib_GZIPHeader_().setCRC_J_V(N.i2j(lI3));
					G = 32;
					continue;
				case 31:
					lI3 = this._strm._adler.getValue__I();
					this.putShortMSB_I_V((N.iushr_opt(lI3, 16)));
					this.putShortMSB_I_V((N.I(lI3 & 65535)));
					G = 32;
					continue;
				case 32:
					this._strm.flush_pending__V();
					if (((this._wrap <= 0))) {
						G = 33;
						continue;
					}
					this._wrap = (N.ineg(this._wrap));
					G = 33;
					continue;
				case 33:
					if (((this._pending == 0))) {
						G = 34;
						continue;
					}
					fI0 = 0;
					G = 35;
					continue;
				case 34:
					fI0 = 1;
					G = 35;
					continue;
				case 35: return fI0;
				default:
					break;
			}
		}
		return 0;
	}
	 void put_byte_B_V(int p0) {
		JA_B fA0 = null;
		int fI1 = 0;
		java_lang_Object fA1 = null;
		int tI1 = 0;
		java_lang_Object tA2 = null;
		fA0 = this._pending_buf;
		fA1 = this;
		tA2 = fA1;
		tI1 = this._pending;
		fI1 = tI1;
		(tA2 as com_jtransc_compression_jzlib_Deflate)._pending = (N.I(tI1 + 1));
		fA0.data[fI1] = p0;
		return;
	}
	 void put_short_I_V(int p0) {
		this.put_byte_B_V(N.i2b(p0));
		this.put_byte_B_V(N.i2b((N.iushr_opt(p0, 8))));
		return;
	}
	 void put_byte__BII_V(JA_B p0, int p1, int p2) {
		java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(p0, p1, this._pending_buf, this._pending, p2);
		this._pending = (N.I(this._pending + p2));
		return;
	}
	 int deflate_slow_I_I(int p0) {
		int G = 0;
		int fI0 = 0;
		int fI1 = 0;
		int lI2 = 0;
		int lI4 = 0;
		int lI3 = 0;
		java_lang_Object fA0 = null;
		int tI3 = 0;
		int tI6 = 0;
		java_lang_Object tA4 = null;
		java_lang_Object tA7 = null;
		while (true) {
			switch (G) {
				case 0:
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((this._lookahead >= 262))) {
						G = 2;
						continue;
					}
					this.fill_window__V();
					if (((this._lookahead >= 262))) {
						G = 3;
						continue;
					}
					if (((p0 != 0))) {
						G = 3;
						continue;
					}
					return 0;
				case 3:
					if (((this._lookahead != 0))) {
						G = 2;
						continue;
					}
					G = 4;
					continue;
				case 2:
					if (((this._lookahead < 3))) {
						G = 5;
						continue;
					}
					this._ins_h = (N.I((N.I((N.ishl(this._ins_h, this._hash_shift)) ^ (N.I((this._window).data[(N.I(this._strstart + 2))] & 255)))) & this._hash_mask));
					lI2 = (N.I((this._head).data[this._ins_h] & 65535));
					this._prev.data[(N.I(this._strstart & this._w_mask))] = (this._head).data[this._ins_h];
					this._head.data[this._ins_h] = N.i2s(this._strstart);
					G = 5;
					continue;
				case 5:
					this._prev_length = this._match_length;
					this._prev_match = this._match_start;
					this._match_length = 2;
					if (((lI2 == 0))) {
						G = 6;
						continue;
					}
					if (((this._prev_length >= this._max_lazy_match))) {
						G = 6;
						continue;
					}
					if ((((N.I((N.I(this._strstart - lI2)) & 65535)) > (N.I(this._w_size - 262))))) {
						G = 6;
						continue;
					}
					if (((this._strategy == 2))) {
						G = 7;
						continue;
					}
					this._match_length = this.longest_match_I_I(lI2);
					G = 7;
					continue;
				case 7:
					if (((this._match_length > 5))) {
						G = 6;
						continue;
					}
					if (((this._strategy == 1))) {
						G = 8;
						continue;
					}
					if (((this._match_length != 3))) {
						G = 6;
						continue;
					}
					if ((((N.I(this._strstart - this._match_start)) <= 4096))) {
						G = 6;
						continue;
					}
					G = 8;
					continue;
				case 8:
					this._match_length = 2;
					G = 6;
					continue;
				case 6:
					if (((this._prev_length < 3))) {
						G = 9;
						continue;
					}
					if (((this._match_length > this._prev_length))) {
						G = 9;
						continue;
					}
					lI4 = (N.I((N.I(this._strstart + this._lookahead)) - 3));
					lI3 = (N.z2i(this._tr_tally_II_Z((N.I((N.I(this._strstart - 1)) - this._prev_match)), (N.I(this._prev_length - 3)))));
					this._lookahead = (N.I(this._lookahead - (N.I(this._prev_length - 1))));
					this._prev_length = (N.I(this._prev_length - 2));
					G = 10;
					continue;
				case 10:
					fA0 = this;
					tA4 = fA0;
					tI3 = (N.I(this._strstart + 1));
					fI0 = tI3;
					(tA4 as com_jtransc_compression_jzlib_Deflate)._strstart = tI3;
					if (((fI0 > lI4))) {
						G = 11;
						continue;
					}
					this._ins_h = (N.I((N.I((N.ishl(this._ins_h, this._hash_shift)) ^ (N.I((this._window).data[(N.I(this._strstart + 2))] & 255)))) & this._hash_mask));
					lI2 = (N.I((this._head).data[this._ins_h] & 65535));
					this._prev.data[(N.I(this._strstart & this._w_mask))] = (this._head).data[this._ins_h];
					this._head.data[this._ins_h] = N.i2s(this._strstart);
					G = 11;
					continue;
				case 11:
					fA0 = this;
					tA7 = fA0;
					tI6 = (N.I(this._prev_length - 1));
					fI0 = tI6;
					(tA7 as com_jtransc_compression_jzlib_Deflate)._prev_length = tI6;
					if (((fI0 != 0))) {
						G = 10;
						continue;
					}
					this._match_available = 0;
					this._match_length = 2;
					this._strstart = (N.I(this._strstart + 1));
					if (((lI3 == 0))) {
						G = 12;
						continue;
					}
					this.flush_block_only_Z_V(false);
					if (((this._strm._avail_out != 0))) {
						G = 12;
						continue;
					}
					return 0;
				case 12:
					G = 1;
					continue;
				case 9:
					if (((this._match_available == 0))) {
						G = 13;
						continue;
					}
					lI3 = (N.z2i(this._tr_tally_II_Z(0, (N.I((this._window).data[(N.I(this._strstart - 1))] & 255)))));
					if (((lI3 == 0))) {
						G = 14;
						continue;
					}
					this.flush_block_only_Z_V(false);
					G = 14;
					continue;
				case 14:
					this._strstart = (N.I(this._strstart + 1));
					this._lookahead = (N.I(this._lookahead - 1));
					if (((this._strm._avail_out != 0))) {
						G = 1;
						continue;
					}
					return 0;
				case 13:
					this._match_available = 1;
					this._strstart = (N.I(this._strstart + 1));
					this._lookahead = (N.I(this._lookahead - 1));
					G = 1;
					continue;
				case 4:
					if (((this._match_available == 0))) {
						G = 15;
						continue;
					}
					lI3 = (N.z2i(this._tr_tally_II_Z(0, (N.I((this._window).data[(N.I(this._strstart - 1))] & 255)))));
					this._match_available = 0;
					G = 15;
					continue;
				case 15:
					fA0 = this;
					if (((p0 != 4))) {
						G = 16;
						continue;
					}
					fI1 = 1;
					G = 17;
					continue;
				case 16:
					fI1 = 0;
					G = 17;
					continue;
				case 17:
					(fA0 as com_jtransc_compression_jzlib_Deflate).flush_block_only_Z_V(((fI1)!=0));
					if (((this._strm._avail_out != 0))) {
						G = 18;
						continue;
					}
					if (((p0 != 4))) {
						G = 19;
						continue;
					}
					return 2;
				case 19:
					return 0;
				case 18:
					if (((p0 != 4))) {
						G = 20;
						continue;
					}
					fI0 = 3;
					G = 21;
					continue;
				case 20:
					fI0 = 1;
					G = 21;
					continue;
				case 21: return fI0;
				default:
					break;
			}
		}
		return 0;
	}
	 void flush_block_only_Z_V(bool p0) {
		int G = 0;
		int fI1 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._block_start < 0))) {
						G = 1;
						continue;
					}
					fI1 = this._block_start;
					G = 2;
					continue;
				case 1:
					fI1 = -1;
					G = 2;
					continue;
				case 2:
					this._tr_flush_block_IIZ_V(fI1, (N.I(this._strstart - this._block_start)), p0);
					this._block_start = this._strstart;
					this._strm.flush_pending__V();
					return;
				default:
					break;
			}
		}
		return;
	}
	 void _tr_flush_block_IIZ_V(int p0, int p1, bool p2) {
		int G = 0;
		int fI0 = 0;
		int fI1 = 0;
		int fI2 = 0;
		int lI6 = 0;
		int lI4 = 0;
		int lI5 = 0;
		com_jtransc_compression_jzlib_Deflate fA0 = null;
		int tI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI6 = 0;
					if (((this._level <= 0))) {
						G = 1;
						continue;
					}
					if (((this._data_type != 2))) {
						G = 2;
						continue;
					}
					this.set_data_type__V();
					G = 2;
					continue;
				case 2:
					this._l_desc.build_tree_Lcom_jtransc_compression_jzlib_Deflate__V(this);
					this._d_desc.build_tree_Lcom_jtransc_compression_jzlib_Deflate__V(this);
					lI6 = this.build_bl_tree__I();
					lI4 = (N.iushr_opt((N.I((N.I(this._opt_len + 3)) + 7)), 3));
					lI5 = (N.iushr_opt((N.I((N.I(this._static_len + 3)) + 7)), 3));
					if (((lI5 > lI4))) {
						G = 3;
						continue;
					}
					lI4 = lI5;
					G = 3;
					continue;
				case 1:
					tI0 = (N.I(p1 + 5));
					fI0 = tI0;
					lI5 = tI0;
					lI4 = fI0;
					G = 3;
					continue;
				case 3:
					if ((((N.I(p1 + 4)) > lI4))) {
						G = 4;
						continue;
					}
					if (((p0 == -1))) {
						G = 4;
						continue;
					}
					this._tr_stored_block_IIZ_V(p0, p1, p2);
					G = 5;
					continue;
				case 4:
					if (((lI5 != lI4))) {
						G = 6;
						continue;
					}
					fA0 = this;
					fI1 = 2;
					if (!(p2)) {
						G = 7;
						continue;
					}
					fI2 = 1;
					G = 8;
					continue;
				case 7:
					fI2 = 0;
					G = 8;
					continue;
				case 8:
					fA0.send_bits_II_V((N.I(fI1 + fI2)), 3);
					this.compress_block__S_S_V(com_jtransc_compression_jzlib_StaticTree._static_ltree, com_jtransc_compression_jzlib_StaticTree._static_dtree);
					G = 5;
					continue;
				case 6:
					fA0 = this;
					fI1 = 4;
					if (!(p2)) {
						G = 9;
						continue;
					}
					fI2 = 1;
					G = 10;
					continue;
				case 9:
					fI2 = 0;
					G = 10;
					continue;
				case 10:
					fA0.send_bits_II_V((N.I(fI1 + fI2)), 3);
					this.send_all_trees_III_V((N.I(this._l_desc._max_code + 1)), (N.I(this._d_desc._max_code + 1)), (N.I(lI6 + 1)));
					this.compress_block__S_S_V(this._dyn_ltree, this._dyn_dtree);
					G = 5;
					continue;
				case 5:
					this.init_block__V();
					if (!(p2)) {
						G = 11;
						continue;
					}
					this.bi_windup__V();
					G = 11;
					continue;
				case 11:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void send_all_trees_III_V(int p0, int p1, int p2) {
		int G = 0;
		int lI4 = 0;
		while (true) {
			switch (G) {
				case 0:
					this.send_bits_II_V((N.I(p0 - 257)), 5);
					this.send_bits_II_V((N.I(p1 - 1)), 5);
					this.send_bits_II_V((N.I(p2 - 4)), 4);
					lI4 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI4 >= p2))) {
						G = 2;
						continue;
					}
					this.send_bits_II_V(((this._bl_tree).data[(N.I((N.I((com_jtransc_compression_jzlib_Tree._bl_order).data[lI4] * 2)) + 1))]), 3);
					lI4 = (N.I(lI4 + 1));
					G = 1;
					continue;
				case 2:
					this.send_tree__SI_V(this._dyn_ltree, (N.I(p0 - 1)));
					this.send_tree__SI_V(this._dyn_dtree, (N.I(p1 - 1)));
					return;
				default:
					break;
			}
		}
		return;
	}
	 void send_tree__SI_V(JA_S p0, int p1) {
		int G = 0;
		int lI4 = 0;
		int lI6 = 0;
		int lI7 = 0;
		int lI8 = 0;
		int lI9 = 0;
		int lI3 = 0;
		int lI5 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI4 = -1;
					lI6 = ((p0).data[1]);
					lI7 = 0;
					lI8 = 7;
					lI9 = 4;
					if (((lI6 != 0))) {
						G = 1;
						continue;
					}
					lI8 = 138;
					lI9 = 3;
					G = 1;
					continue;
				case 1:
					lI3 = 0;
					G = 2;
					continue;
				case 2:
					if (((lI3 > p1))) {
						G = 3;
						continue;
					}
					lI5 = lI6;
					lI6 = ((p0).data[(N.I((N.I((N.I(lI3 + 1)) * 2)) + 1))]);
					lI7 = (N.I(lI7 + 1));
					if (((lI7 >= lI8))) {
						G = 4;
						continue;
					}
					if (((lI5 != lI6))) {
						G = 4;
						continue;
					}
					G = 5;
					continue;
				case 4:
					if (((lI7 >= lI9))) {
						G = 6;
						continue;
					}
					G = 7;
					continue;
				case 7:
					this.send_code_I_S_V(lI5, this._bl_tree);
					lI7 = (N.I(lI7 + -1));
					if (((lI7 != 0))) {
						G = 7;
						continue;
					}
					G = 8;
					continue;
				case 6:
					if (((lI5 == 0))) {
						G = 9;
						continue;
					}
					if (((lI5 == lI4))) {
						G = 10;
						continue;
					}
					this.send_code_I_S_V(lI5, this._bl_tree);
					lI7 = (N.I(lI7 + -1));
					G = 10;
					continue;
				case 10:
					this.send_code_I_S_V(16, this._bl_tree);
					this.send_bits_II_V((N.I(lI7 - 3)), 2);
					G = 8;
					continue;
				case 9:
					if (((lI7 > 10))) {
						G = 11;
						continue;
					}
					this.send_code_I_S_V(17, this._bl_tree);
					this.send_bits_II_V((N.I(lI7 - 3)), 3);
					G = 8;
					continue;
				case 11:
					this.send_code_I_S_V(18, this._bl_tree);
					this.send_bits_II_V((N.I(lI7 - 11)), 7);
					G = 8;
					continue;
				case 8:
					lI7 = 0;
					lI4 = lI5;
					if (((lI6 != 0))) {
						G = 12;
						continue;
					}
					lI8 = 138;
					lI9 = 3;
					G = 5;
					continue;
				case 12:
					if (((lI5 != lI6))) {
						G = 13;
						continue;
					}
					lI8 = 6;
					lI9 = 3;
					G = 5;
					continue;
				case 13:
					lI8 = 7;
					lI9 = 4;
					G = 5;
					continue;
				case 5:
					lI3 = (N.I(lI3 + 1));
					G = 2;
					continue;
				case 3:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void send_code_I_S_V(int p0, JA_S p1) {
		int lI3 = 0;
		lI3 = (N.I(p0 * 2));
		this.send_bits_II_V((N.I((p1).data[lI3] & 65535)), (N.I((p1).data[(N.I(lI3 + 1))] & 65535)));
		return;
	}
	 void send_bits_II_V(int p0, int p1) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._bi_valid <= (N.I(16 - p1))))) {
						G = 1;
						continue;
					}
					this._bi_buf = N.i2s((N.I(this._bi_buf | (N.I((N.ishl(p0, this._bi_valid)) & 65535)))));
					this.put_short_I_V((this._bi_buf));
					this._bi_buf = N.i2s((N.iushr(p0, (N.I(16 - this._bi_valid)))));
					this._bi_valid = (N.I(this._bi_valid + (N.I(p1 - 16))));
					G = 2;
					continue;
				case 1:
					this._bi_buf = N.i2s((N.I(this._bi_buf | (N.I((N.ishl(p0, this._bi_valid)) & 65535)))));
					this._bi_valid = (N.I(this._bi_valid + p1));
					G = 2;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void bi_windup__V() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._bi_valid <= 8))) {
						G = 1;
						continue;
					}
					this.put_short_I_V((this._bi_buf));
					G = 2;
					continue;
				case 1:
					if (((this._bi_valid <= 0))) {
						G = 2;
						continue;
					}
					this.put_byte_B_V(N.i2b(this._bi_buf));
					G = 2;
					continue;
				case 2:
					this._bi_buf = 0;
					this._bi_valid = 0;
					return;
				default:
					break;
			}
		}
		return;
	}
	 void init_block__V() {
		int G = 0;
		int fI1 = 0;
		int lI1 = 0;
		java_lang_Object fA1 = null;
		com_jtransc_compression_jzlib_Deflate fA0 = null;
		int tI0 = 0;
		int tI2 = 0;
		java_lang_Object tA1 = null;
		java_lang_Object tA3 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= 286))) {
						G = 2;
						continue;
					}
					this._dyn_ltree.data[(N.I(lI1 * 2))] = 0;
					lI1 = (N.I(lI1 + 1));
					G = 1;
					continue;
				case 2:
					lI1 = 0;
					G = 3;
					continue;
				case 3:
					if (((lI1 >= 30))) {
						G = 4;
						continue;
					}
					this._dyn_dtree.data[(N.I(lI1 * 2))] = 0;
					lI1 = (N.I(lI1 + 1));
					G = 3;
					continue;
				case 4:
					lI1 = 0;
					G = 5;
					continue;
				case 5:
					if (((lI1 >= 19))) {
						G = 6;
						continue;
					}
					this._bl_tree.data[(N.I(lI1 * 2))] = 0;
					lI1 = (N.I(lI1 + 1));
					G = 5;
					continue;
				case 6:
					this._dyn_ltree.data[512] = 1;
					fA0 = this;
					fA1 = this;
					tA1 = fA1;
					tI0 = 0;
					fI1 = tI0;
					(tA1 as com_jtransc_compression_jzlib_Deflate)._static_len = tI0;
					fA0._opt_len = fI1;
					fA0 = this;
					fA1 = this;
					tA3 = fA1;
					tI2 = 0;
					fI1 = tI2;
					(tA3 as com_jtransc_compression_jzlib_Deflate)._matches = tI2;
					fA0._last_lit = fI1;
					return;
				default:
					break;
			}
		}
		return;
	}
	 void set_data_type__V() {
		int G = 0;
		int fI1 = 0;
		int lI1 = 0;
		int lI2 = 0;
		int lI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					lI2 = 0;
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= 7))) {
						G = 2;
						continue;
					}
					lI3 = (N.I(lI3 + (this._dyn_ltree).data[(N.I(lI1 * 2))]));
					lI1 = (N.I(lI1 + 1));
					G = 1;
					continue;
				case 2:
					if (((lI1 >= 128))) {
						G = 3;
						continue;
					}
					lI2 = (N.I(lI2 + (this._dyn_ltree).data[(N.I(lI1 * 2))]));
					lI1 = (N.I(lI1 + 1));
					G = 2;
					continue;
				case 3:
					if (((lI1 >= 256))) {
						G = 4;
						continue;
					}
					lI3 = (N.I(lI3 + (this._dyn_ltree).data[(N.I(lI1 * 2))]));
					lI1 = (N.I(lI1 + 1));
					G = 3;
					continue;
				case 4:
					if (((lI3 <= (N.iushr_opt(lI2, 2))))) {
						G = 5;
						continue;
					}
					fI1 = 0;
					G = 6;
					continue;
				case 5:
					fI1 = 1;
					G = 6;
					continue;
				case 6:
					this._data_type = N.i2b(fI1);
					return;
				default:
					break;
			}
		}
		return;
	}
	 void pqdownheap__SI_V(JA_S p0, int p1) {
		int G = 0;
		JA_B fA3 = null;
		int fI1 = 0;
		int fI2 = 0;
		int lI2 = 0;
		int lI3 = 0;
		int lI4 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI2 = p1;
					lI3 = (this._heap).data[lI2];
					lI4 = (N.I(lI2 << 1));
					G = 1;
					continue;
				case 1:
					if (((lI4 > this._heap_len))) {
						G = 2;
						continue;
					}
					if (((lI4 >= this._heap_len))) {
						G = 3;
						continue;
					}
					fI1 = (this._heap).data[(N.I(lI4 + 1))];
					fI2 = (this._heap).data[lI4];
					fA3 = this._depth;
					if (!(com_jtransc_compression_jzlib_Deflate.smaller__SII_B_Z(p0, fI1, fI2, fA3))) {
						G = 3;
						continue;
					}
					lI4 = (N.I(lI4 + 1));
					G = 3;
					continue;
				case 3:
					if (!(com_jtransc_compression_jzlib_Deflate.smaller__SII_B_Z(p0, lI3, (this._heap).data[lI4], this._depth))) {
						G = 4;
						continue;
					}
					G = 2;
					continue;
				case 4:
					this._heap.data[lI2] = (this._heap).data[lI4];
					lI2 = lI4;
					lI4 = (N.I(lI4 << 1));
					G = 1;
					continue;
				case 2:
					this._heap.data[lI2] = lI3;
					return;
				default:
					break;
			}
		}
		return;
	}
	static bool smaller__SII_B_Z(JA_S p0, int p1, int p2, JA_B p3) {
		int G = 0;
		int fI0 = 0;
		int lI4 = 0;
		int lI5 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI4 = ((p0).data[(N.I(p1 * 2))]);
					lI5 = ((p0).data[(N.I(p2 * 2))]);
					if (((lI4 < lI5))) {
						G = 1;
						continue;
					}
					if (((lI4 != lI5))) {
						G = 2;
						continue;
					}
					if ((((p3).data[p1] > (p3).data[p2]))) {
						G = 2;
						continue;
					}
					G = 1;
					continue;
				case 1:
					fI0 = 1;
					G = 3;
					continue;
				case 2:
					fI0 = 0;
					G = 3;
					continue;
				case 3: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 int build_bl_tree__I() {
		int G = 0;
		int lI1 = 0;
		while (true) {
			switch (G) {
				case 0:
					this.scan_tree__SI_V(this._dyn_ltree, this._l_desc._max_code);
					this.scan_tree__SI_V(this._dyn_dtree, this._d_desc._max_code);
					this._bl_desc.build_tree_Lcom_jtransc_compression_jzlib_Deflate__V(this);
					lI1 = 18;
					G = 1;
					continue;
				case 1:
					if (((lI1 < 3))) {
						G = 2;
						continue;
					}
					if ((((this._bl_tree).data[(N.I((N.I((com_jtransc_compression_jzlib_Tree._bl_order).data[lI1] * 2)) + 1))] == 0))) {
						G = 3;
						continue;
					}
					G = 2;
					continue;
				case 3:
					lI1 = (N.I(lI1 + -1));
					G = 1;
					continue;
				case 2:
					this._opt_len = (N.I(this._opt_len + (N.I((N.I((N.I((N.I(3 * (N.I(lI1 + 1)))) + 5)) + 5)) + 4))));
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	 void scan_tree__SI_V(JA_S p0, int p1) {
		int G = 0;
		int lI4 = 0;
		int lI6 = 0;
		int lI7 = 0;
		int lI8 = 0;
		int lI9 = 0;
		int lI3 = 0;
		int lI5 = 0;
		JA_S tA1 = null;
		JA_S tA3 = null;
		JA_S tA5 = null;
		JA_S tA7 = null;
		JA_S tA9 = null;
		int tI0 = 0;
		int tI2 = 0;
		int tI4 = 0;
		int tI6 = 0;
		int tI8 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI4 = -1;
					lI6 = ((p0).data[1]);
					lI7 = 0;
					lI8 = 7;
					lI9 = 4;
					if (((lI6 != 0))) {
						G = 1;
						continue;
					}
					lI8 = 138;
					lI9 = 3;
					G = 1;
					continue;
				case 1:
					p0.data[(N.I((N.I((N.I(p1 + 1)) * 2)) + 1))] = -1;
					lI3 = 0;
					G = 2;
					continue;
				case 2:
					if (((lI3 > p1))) {
						G = 3;
						continue;
					}
					lI5 = lI6;
					lI6 = ((p0).data[(N.I((N.I((N.I(lI3 + 1)) * 2)) + 1))]);
					lI7 = (N.I(lI7 + 1));
					if (((lI7 >= lI8))) {
						G = 4;
						continue;
					}
					if (((lI5 != lI6))) {
						G = 4;
						continue;
					}
					G = 5;
					continue;
				case 4:
					if (((lI7 >= lI9))) {
						G = 6;
						continue;
					}
					tA1 = this._bl_tree;
					tI0 = (N.I(lI5 * 2));
					tA1.data[tI0] = N.i2s((N.I((tA1).data[tI0] + lI7)));
					G = 7;
					continue;
				case 6:
					if (((lI5 == 0))) {
						G = 8;
						continue;
					}
					if (((lI5 == lI4))) {
						G = 9;
						continue;
					}
					tA3 = this._bl_tree;
					tI2 = (N.I(lI5 * 2));
					tA3.data[tI2] = N.i2s((N.I((tA3).data[tI2] + 1)));
					G = 9;
					continue;
				case 9:
					tA5 = this._bl_tree;
					tI4 = 32;
					tA5.data[tI4] = N.i2s((N.I((tA5).data[tI4] + 1)));
					G = 7;
					continue;
				case 8:
					if (((lI7 > 10))) {
						G = 10;
						continue;
					}
					tA7 = this._bl_tree;
					tI6 = 34;
					tA7.data[tI6] = N.i2s((N.I((tA7).data[tI6] + 1)));
					G = 7;
					continue;
				case 10:
					tA9 = this._bl_tree;
					tI8 = 36;
					tA9.data[tI8] = N.i2s((N.I((tA9).data[tI8] + 1)));
					G = 7;
					continue;
				case 7:
					lI7 = 0;
					lI4 = lI5;
					if (((lI6 != 0))) {
						G = 11;
						continue;
					}
					lI8 = 138;
					lI9 = 3;
					G = 5;
					continue;
				case 11:
					if (((lI5 != lI6))) {
						G = 12;
						continue;
					}
					lI8 = 6;
					lI9 = 3;
					G = 5;
					continue;
				case 12:
					lI8 = 7;
					lI9 = 4;
					G = 5;
					continue;
				case 5:
					lI3 = (N.I(lI3 + 1));
					G = 2;
					continue;
				case 3:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void compress_block__S_S_V(JA_S p0, JA_S p1) {
		int G = 0;
		int lI5 = 0;
		int lI3 = 0;
		int lI4 = 0;
		int lI6 = 0;
		int lI7 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI5 = 0;
					if (((this._last_lit == 0))) {
						G = 1;
						continue;
					}
					G = 2;
					continue;
				case 2:
					lI3 = (N.I((N.I((N.I((this._pending_buf).data[(N.I(this._d_buf + (N.I(lI5 * 2))))] << 8)) & 65280)) | (N.I((this._pending_buf).data[(N.I((N.I(this._d_buf + (N.I(lI5 * 2)))) + 1))] & 255))));
					lI4 = (N.I((this._l_buf).data[lI5] & 255));
					lI5 = (N.I(lI5 + 1));
					if (((lI3 != 0))) {
						G = 3;
						continue;
					}
					this.send_code_I_S_V(lI4, p0);
					G = 4;
					continue;
				case 3:
					lI6 = ((com_jtransc_compression_jzlib_Tree.__length_code).data[lI4]);
					this.send_code_I_S_V((N.I((N.I(lI6 + 256)) + 1)), p0);
					lI7 = (com_jtransc_compression_jzlib_Tree._extra_lbits).data[lI6];
					if (((lI7 == 0))) {
						G = 5;
						continue;
					}
					lI4 = (N.I(lI4 - (com_jtransc_compression_jzlib_Tree._base_length).data[lI6]));
					this.send_bits_II_V(lI4, lI7);
					G = 5;
					continue;
				case 5:
					lI3 = (N.I(lI3 + -1));
					lI6 = com_jtransc_compression_jzlib_Tree.d_code_I_I(lI3);
					this.send_code_I_S_V(lI6, p1);
					lI7 = (com_jtransc_compression_jzlib_Tree._extra_dbits).data[lI6];
					if (((lI7 == 0))) {
						G = 4;
						continue;
					}
					lI3 = (N.I(lI3 - (com_jtransc_compression_jzlib_Tree._base_dist).data[lI6]));
					this.send_bits_II_V(lI3, lI7);
					G = 4;
					continue;
				case 4:
					if (((lI5 < this._last_lit))) {
						G = 2;
						continue;
					}
					G = 1;
					continue;
				case 1:
					this.send_code_I_S_V(256, p0);
					this._last_eob_len = ((p0).data[513]);
					return;
				default:
					break;
			}
		}
		return;
	}
	 void _tr_stored_block_IIZ_V(int p0, int p1, bool p2) {
		int G = 0;
		int fI1 = 0;
		int fI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					fI1 = 0;
					if (!(p2)) {
						G = 1;
						continue;
					}
					fI2 = 1;
					G = 2;
					continue;
				case 1:
					fI2 = 0;
					G = 2;
					continue;
				case 2:
					this.send_bits_II_V((N.I(fI1 + fI2)), 3);
					this.copy_block_IIZ_V(p0, p1, true);
					return;
				default:
					break;
			}
		}
		return;
	}
	 void copy_block_IIZ_V(int p0, int p1, bool p2) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					this.bi_windup__V();
					this._last_eob_len = 8;
					if (!(p2)) {
						G = 1;
						continue;
					}
					this.put_short_I_V((N.i2s(p1)));
					this.put_short_I_V((N.i2s((N.I(p1 ^ -1)))));
					G = 1;
					continue;
				case 1:
					this.put_byte__BII_V(this._window, p0, p1);
					return;
				default:
					break;
			}
		}
		return;
	}
	 int longest_match_I_I(int p0) {
		JA_B fA0 = null;
		int lI12 = 0;
		int lI2 = 0;
		int lI6 = 0;
		int lI8 = 0;
		int lI10 = 0;
		int lI4 = 0;
		int fI0 = 0;
		int tI0 = 0;
		int G = 0;
		JA_B fA1 = null;
		int lI11 = 0;
		int lI1 = 0;
		int lI3 = 0;
		int lI7 = 0;
		int lI9 = 0;
		int lI5 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = p0;
					lI2 = this._max_chain_length;
					lI3 = this._strstart;
					lI6 = this._prev_length;
					if (((this._strstart <= (N.I(this._w_size - 262))))) {
						G = 1;
						continue;
					}
					fI0 = (N.I(this._strstart - (N.I(this._w_size - 262))));
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2:
					lI7 = fI0;
					lI8 = this._nice_match;
					lI9 = this._w_mask;
					lI10 = (N.I(this._strstart + 258));
					lI11 = ((this._window).data[(N.I((N.I(lI3 + lI6)) - 1))]);
					lI12 = ((this._window).data[(N.I(lI3 + lI6))]);
					if (((this._prev_length < this._good_match))) {
						G = 3;
						continue;
					}
					lI2 = (N.I(lI2 >> 2));
					G = 3;
					continue;
				case 3:
					if (((lI8 <= this._lookahead))) {
						G = 4;
						continue;
					}
					lI8 = this._lookahead;
					G = 4;
					continue;
				case 4:
					lI4 = lI1;
					if ((((this._window).data[(N.I(lI4 + lI6))] != lI12))) {
						G = 5;
						continue;
					}
					if ((((this._window).data[(N.I((N.I(lI4 + lI6)) - 1))] != lI11))) {
						G = 5;
						continue;
					}
					if ((((this._window).data[lI4] != (this._window).data[lI3]))) {
						G = 5;
						continue;
					}
					fA0 = this._window;
					lI4 = (N.I(lI4 + 1));
					if ((((fA0).data[lI4] == (this._window).data[(N.I(lI3 + 1))]))) {
						G = 6;
						continue;
					}
					G = 5;
					continue;
				case 6:
					lI3 = (N.I(lI3 + 2));
					lI4 = (N.I(lI4 + 1));
					G = 7;
					continue;
				case 7:
					fA0 = this._window;
					lI3 = (N.I(lI3 + 1));
					fI0 = ((fA0).data[lI3]);
					fA1 = this._window;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1).data[lI4]))) {
						G = 8;
						continue;
					}
					fA0 = this._window;
					lI3 = (N.I(lI3 + 1));
					fI0 = ((fA0).data[lI3]);
					fA1 = this._window;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1).data[lI4]))) {
						G = 8;
						continue;
					}
					fA0 = this._window;
					lI3 = (N.I(lI3 + 1));
					fI0 = ((fA0).data[lI3]);
					fA1 = this._window;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1).data[lI4]))) {
						G = 8;
						continue;
					}
					fA0 = this._window;
					lI3 = (N.I(lI3 + 1));
					fI0 = ((fA0).data[lI3]);
					fA1 = this._window;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1).data[lI4]))) {
						G = 8;
						continue;
					}
					fA0 = this._window;
					lI3 = (N.I(lI3 + 1));
					fI0 = ((fA0).data[lI3]);
					fA1 = this._window;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1).data[lI4]))) {
						G = 8;
						continue;
					}
					fA0 = this._window;
					lI3 = (N.I(lI3 + 1));
					fI0 = ((fA0).data[lI3]);
					fA1 = this._window;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1).data[lI4]))) {
						G = 8;
						continue;
					}
					fA0 = this._window;
					lI3 = (N.I(lI3 + 1));
					fI0 = ((fA0).data[lI3]);
					fA1 = this._window;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1).data[lI4]))) {
						G = 8;
						continue;
					}
					fA0 = this._window;
					lI3 = (N.I(lI3 + 1));
					fI0 = ((fA0).data[lI3]);
					fA1 = this._window;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1).data[lI4]))) {
						G = 8;
						continue;
					}
					if (((lI3 < lI10))) {
						G = 7;
						continue;
					}
					G = 8;
					continue;
				case 8:
					lI5 = (N.I(258 - (N.I(lI10 - lI3))));
					lI3 = (N.I(lI10 - 258));
					if (((lI5 <= lI6))) {
						G = 5;
						continue;
					}
					this._match_start = lI1;
					lI6 = lI5;
					if (((lI5 < lI8))) {
						G = 9;
						continue;
					}
					G = 10;
					continue;
				case 9:
					lI11 = ((this._window).data[(N.I((N.I(lI3 + lI6)) - 1))]);
					lI12 = ((this._window).data[(N.I(lI3 + lI6))]);
					G = 5;
					continue;
				case 5:
					tI0 = (N.I((this._prev).data[(N.I(lI1 & lI9))] & 65535));
					fI0 = tI0;
					lI1 = tI0;
					if (((fI0 <= lI7))) {
						G = 10;
						continue;
					}
					lI2 = (N.I(lI2 + -1));
					if (((lI2 != 0))) {
						G = 4;
						continue;
					}
					G = 10;
					continue;
				case 10:
					if (((lI6 > this._lookahead))) {
						G = 11;
						continue;
					}
					return lI6;
				case 11:
					return this._lookahead;
				default:
					break;
			}
		}
		return 0;
	}
	 void fill_window__V() {
		int G = 0;
		JA_S fA0 = null;
		int fI1 = 0;
		int fI2 = 0;
		int lI4 = 0;
		int lI1 = 0;
		int lI3 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					G = 1;
					continue;
				case 1:
					lI4 = (N.I((N.I(this._window_size - this._lookahead)) - this._strstart));
					if (((lI4 != 0))) {
						G = 2;
						continue;
					}
					if (((this._strstart != 0))) {
						G = 2;
						continue;
					}
					if (((this._lookahead != 0))) {
						G = 2;
						continue;
					}
					lI4 = this._w_size;
					G = 3;
					continue;
				case 2:
					if (((lI4 != -1))) {
						G = 4;
						continue;
					}
					lI4 = (N.I(lI4 + -1));
					G = 3;
					continue;
				case 4:
					if (((this._strstart < (N.I((N.I(this._w_size + this._w_size)) - 262))))) {
						G = 3;
						continue;
					}
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(this._window, this._w_size, this._window, 0, this._w_size);
					this._match_start = (N.I(this._match_start - this._w_size));
					this._strstart = (N.I(this._strstart - this._w_size));
					this._block_start = (N.I(this._block_start - this._w_size));
					lI1 = this._hash_size;
					lI3 = lI1;
					G = 5;
					continue;
				case 5:
					fA0 = this._head;
					lI3 = (N.I(lI3 + -1));
					lI2 = (N.I((fA0).data[lI3] & 65535));
					fA0 = this._head;
					fI1 = lI3;
					if (((lI2 < this._w_size))) {
						G = 6;
						continue;
					}
					fI2 = (N.i2s((N.I(lI2 - this._w_size))));
					G = 7;
					continue;
				case 6:
					fI2 = 0;
					G = 7;
					continue;
				case 7:
					fA0.data[fI1] = N.i2s(fI2);
					lI1 = (N.I(lI1 + -1));
					if (((lI1 != 0))) {
						G = 5;
						continue;
					}
					lI1 = this._w_size;
					lI3 = lI1;
					G = 8;
					continue;
				case 8:
					fA0 = this._prev;
					lI3 = (N.I(lI3 + -1));
					lI2 = (N.I((fA0).data[lI3] & 65535));
					fA0 = this._prev;
					fI1 = lI3;
					if (((lI2 < this._w_size))) {
						G = 9;
						continue;
					}
					fI2 = (N.i2s((N.I(lI2 - this._w_size))));
					G = 10;
					continue;
				case 9:
					fI2 = 0;
					G = 10;
					continue;
				case 10:
					fA0.data[fI1] = N.i2s(fI2);
					lI1 = (N.I(lI1 + -1));
					if (((lI1 != 0))) {
						G = 8;
						continue;
					}
					lI4 = (N.I(lI4 + this._w_size));
					G = 3;
					continue;
				case 3:
					if (((this._strm._avail_in != 0))) {
						G = 11;
						continue;
					}
					return;
					G = 11;
					continue;
				case 11:
					lI1 = this._strm.read_buf__BII_I(this._window, (N.I(this._strstart + this._lookahead)), lI4);
					this._lookahead = (N.I(this._lookahead + lI1));
					if (((this._lookahead < 3))) {
						G = 12;
						continue;
					}
					this._ins_h = (N.I((this._window).data[this._strstart] & 255));
					this._ins_h = (N.I((N.I((N.ishl(this._ins_h, this._hash_shift)) ^ (N.I((this._window).data[(N.I(this._strstart + 1))] & 255)))) & this._hash_mask));
					G = 12;
					continue;
				case 12:
					if (((this._lookahead >= 262))) {
						G = 13;
						continue;
					}
					if (((this._strm._avail_in != 0))) {
						G = 1;
						continue;
					}
					G = 13;
					continue;
				case 13:
					return;
				default:
					break;
			}
		}
		return;
	}
	 bool _tr_tally_II_Z(int p0, int p1) {
		int G = 0;
		int fI0 = 0;
		int lI1 = 0;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		JA_S tA2 = null;
		JA_S tA5 = null;
		JA_S tA7 = null;
		int tI1 = 0;
		int tI4 = 0;
		int tI6 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = p0;
					this._pending_buf.data[(N.I(this._d_buf + (N.I(this._last_lit * 2))))] = N.i2b((N.iushr_opt(lI1, 8)));
					this._pending_buf.data[(N.I((N.I(this._d_buf + (N.I(this._last_lit * 2)))) + 1))] = N.i2b(lI1);
					this._l_buf.data[this._last_lit] = N.i2b(p1);
					this._last_lit = (N.I(this._last_lit + 1));
					if (((lI1 != 0))) {
						G = 1;
						continue;
					}
					tA2 = this._dyn_ltree;
					tI1 = (N.I(p1 * 2));
					tA2.data[tI1] = N.i2s((N.I((tA2).data[tI1] + 1)));
					G = 2;
					continue;
				case 1:
					this._matches = (N.I(this._matches + 1));
					lI1 = (N.I(lI1 + -1));
					tA5 = this._dyn_ltree;
					tI4 = (N.I((N.I((N.I((com_jtransc_compression_jzlib_Tree.__length_code).data[p1] + 256)) + 1)) * 2));
					tA5.data[tI4] = N.i2s((N.I((tA5).data[tI4] + 1)));
					tA7 = this._dyn_dtree;
					tI6 = (N.I(com_jtransc_compression_jzlib_Tree.d_code_I_I(lI1) * 2));
					tA7.data[tI6] = N.i2s((N.I((tA7).data[tI6] + 1)));
					G = 2;
					continue;
				case 2:
					if ((((N.I(this._last_lit & 8191)) != 0))) {
						G = 3;
						continue;
					}
					if (((this._level <= 2))) {
						G = 3;
						continue;
					}
					lI3 = (N.I(this._last_lit * 8));
					lI4 = (N.I(this._strstart - this._block_start));
					lI5 = 0;
					G = 4;
					continue;
				case 4:
					if (((lI5 >= 30))) {
						G = 5;
						continue;
					}
					lI3 = N.j2i(((N.i2j(lI3)+((N.i2j((this._dyn_dtree).data[(N.I(lI5 * 2))])*((N.lnew(5)+N.i2j((com_jtransc_compression_jzlib_Tree._extra_dbits).data[lI5]))))))));
					lI5 = (N.I(lI5 + 1));
					G = 4;
					continue;
				case 5:
					lI3 = (N.iushr_opt(lI3, 3));
					if (((this._matches >= (N.I(this._last_lit ~/ 2))))) {
						G = 3;
						continue;
					}
					if (((lI3 >= (N.I(lI4 ~/ 2))))) {
						G = 3;
						continue;
					}
					return true;
				case 3:
					if (((this._last_lit != (N.I(this._lit_bufsize - 1))))) {
						G = 6;
						continue;
					}
					fI0 = 1;
					G = 7;
					continue;
				case 6:
					fI0 = 0;
					G = 7;
					continue;
				case 7: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 int deflate_fast_I_I(int p0) {
		int G = 0;
		int fI0 = 0;
		int fI1 = 0;
		int lI2 = 0;
		int lI3 = 0;
		java_lang_Object fA0 = null;
		com_jtransc_compression_jzlib_Deflate tA1 = null;
		int tI4 = 0;
		com_jtransc_compression_jzlib_Deflate tA7 = null;
		com_jtransc_compression_jzlib_Deflate tA9 = null;
		java_lang_Object tA3 = null;
		java_lang_Object tA5 = null;
		while (true) {
			switch (G) {
				case 0:
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((this._lookahead >= 262))) {
						G = 2;
						continue;
					}
					this.fill_window__V();
					if (((this._lookahead >= 262))) {
						G = 3;
						continue;
					}
					if (((p0 != 0))) {
						G = 3;
						continue;
					}
					return 0;
				case 3:
					if (((this._lookahead != 0))) {
						G = 2;
						continue;
					}
					G = 4;
					continue;
				case 2:
					if (((this._lookahead < 3))) {
						G = 5;
						continue;
					}
					this._ins_h = (N.I((N.I((N.ishl(this._ins_h, this._hash_shift)) ^ (N.I((this._window).data[(N.I(this._strstart + 2))] & 255)))) & this._hash_mask));
					lI2 = (N.I((this._head).data[this._ins_h] & 65535));
					this._prev.data[(N.I(this._strstart & this._w_mask))] = (this._head).data[this._ins_h];
					this._head.data[this._ins_h] = N.i2s(this._strstart);
					G = 5;
					continue;
				case 5:
					if ((((N.lcmp(N.i2j(lI2), N.lnew(0))) == 0))) {
						G = 6;
						continue;
					}
					if ((((N.I((N.I(this._strstart - lI2)) & 65535)) > (N.I(this._w_size - 262))))) {
						G = 6;
						continue;
					}
					if (((this._strategy == 2))) {
						G = 6;
						continue;
					}
					this._match_length = this.longest_match_I_I(lI2);
					G = 6;
					continue;
				case 6:
					if (((this._match_length < 3))) {
						G = 7;
						continue;
					}
					lI3 = (N.z2i(this._tr_tally_II_Z((N.I(this._strstart - this._match_start)), (N.I(this._match_length - 3)))));
					this._lookahead = (N.I(this._lookahead - this._match_length));
					if (((this._match_length > this._max_lazy_match))) {
						G = 8;
						continue;
					}
					if (((this._lookahead < 3))) {
						G = 8;
						continue;
					}
					tA1 = this;
					tA1._match_length = (N.I(tA1._match_length - 1));
					G = 9;
					continue;
				case 9:
					this._strstart = (N.I(this._strstart + 1));
					this._ins_h = (N.I((N.I((N.ishl(this._ins_h, this._hash_shift)) ^ (N.I((this._window).data[(N.I(this._strstart + 2))] & 255)))) & this._hash_mask));
					lI2 = (N.I((this._head).data[this._ins_h] & 65535));
					this._prev.data[(N.I(this._strstart & this._w_mask))] = (this._head).data[this._ins_h];
					this._head.data[this._ins_h] = N.i2s(this._strstart);
					tA3 = this;
					fA0 = tA3;
					tA5 = fA0;
					tI4 = (N.I((tA3 as com_jtransc_compression_jzlib_Deflate)._match_length - 1));
					fI0 = tI4;
					(tA5 as com_jtransc_compression_jzlib_Deflate)._match_length = tI4;
					if (((fI0 != 0))) {
						G = 9;
						continue;
					}
					this._strstart = (N.I(this._strstart + 1));
					G = 10;
					continue;
				case 8:
					tA7 = this;
					tA7._strstart = (N.I(tA7._strstart + this._match_length));
					this._match_length = 0;
					this._ins_h = (N.I((this._window).data[this._strstart] & 255));
					this._ins_h = (N.I((N.I((N.ishl(this._ins_h, this._hash_shift)) ^ (N.I((this._window).data[(N.I(this._strstart + 1))] & 255)))) & this._hash_mask));
					G = 10;
					continue;
				case 7:
					lI3 = (N.z2i(this._tr_tally_II_Z(0, (N.I((this._window).data[this._strstart] & 255)))));
					this._lookahead = (N.I(this._lookahead - 1));
					tA9 = this;
					tA9._strstart = (N.I(tA9._strstart + 1));
					G = 10;
					continue;
				case 10:
					if (((lI3 == 0))) {
						G = 1;
						continue;
					}
					this.flush_block_only_Z_V(false);
					if (((this._strm._avail_out != 0))) {
						G = 1;
						continue;
					}
					return 0;
				case 4:
					fA0 = this;
					if (((p0 != 4))) {
						G = 11;
						continue;
					}
					fI1 = 1;
					G = 12;
					continue;
				case 11:
					fI1 = 0;
					G = 12;
					continue;
				case 12:
					(fA0 as com_jtransc_compression_jzlib_Deflate).flush_block_only_Z_V(((fI1)!=0));
					if (((this._strm._avail_out != 0))) {
						G = 13;
						continue;
					}
					if (((p0 != 4))) {
						G = 14;
						continue;
					}
					return 2;
				case 14:
					return 0;
				case 13:
					if (((p0 != 4))) {
						G = 15;
						continue;
					}
					fI0 = 3;
					G = 16;
					continue;
				case 15:
					fI0 = 1;
					G = 16;
					continue;
				case 16: return fI0;
				default:
					break;
			}
		}
		return 0;
	}
	 void _tr_align__V() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					this.send_bits_II_V(2, 3);
					this.send_code_I_S_V(256, com_jtransc_compression_jzlib_StaticTree._static_ltree);
					this.bi_flush__V();
					if ((((N.I((N.I((N.I(1 + this._last_eob_len)) + 10)) - this._bi_valid)) >= 9))) {
						G = 1;
						continue;
					}
					this.send_bits_II_V(2, 3);
					this.send_code_I_S_V(256, com_jtransc_compression_jzlib_StaticTree._static_ltree);
					this.bi_flush__V();
					G = 1;
					continue;
				case 1:
					this._last_eob_len = 7;
					return;
				default:
					break;
			}
		}
		return;
	}
	 void bi_flush__V() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._bi_valid != 16))) {
						G = 1;
						continue;
					}
					this.put_short_I_V((this._bi_buf));
					this._bi_buf = 0;
					this._bi_valid = 0;
					G = 2;
					continue;
				case 1:
					if (((this._bi_valid < 8))) {
						G = 2;
						continue;
					}
					this.put_byte_B_V(N.i2b(this._bi_buf));
					this._bi_buf = N.i2s((N.iushr_opt(this._bi_buf, 8)));
					this._bi_valid = (N.I(this._bi_valid - 8));
					G = 2;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	 int deflate_stored_I_I(int p0) {
		int G = 0;
		int fI1 = 0;
		int fI0 = 0;
		int lI2 = 0;
		int lI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI2 = 65535;
					if (((lI2 <= (N.I(this._pending_buf_size - 5))))) {
						G = 1;
						continue;
					}
					lI2 = (N.I(this._pending_buf_size - 5));
					G = 1;
					continue;
				case 1:
					if (((this._lookahead > 1))) {
						G = 2;
						continue;
					}
					this.fill_window__V();
					if (((this._lookahead != 0))) {
						G = 3;
						continue;
					}
					if (((p0 != 0))) {
						G = 3;
						continue;
					}
					return 0;
				case 3:
					if (((this._lookahead != 0))) {
						G = 2;
						continue;
					}
					G = 4;
					continue;
				case 2:
					this._strstart = (N.I(this._strstart + this._lookahead));
					this._lookahead = 0;
					lI3 = (N.I(this._block_start + lI2));
					if (((this._strstart == 0))) {
						G = 5;
						continue;
					}
					if (((this._strstart < lI3))) {
						G = 6;
						continue;
					}
					G = 5;
					continue;
				case 5:
					this._lookahead = (N.I(this._strstart - lI3));
					this._strstart = lI3;
					this.flush_block_only_Z_V(false);
					if (((this._strm._avail_out != 0))) {
						G = 6;
						continue;
					}
					return 0;
				case 6:
					if ((((N.I(this._strstart - this._block_start)) < (N.I(this._w_size - 262))))) {
						G = 1;
						continue;
					}
					this.flush_block_only_Z_V(false);
					if (((this._strm._avail_out != 0))) {
						G = 1;
						continue;
					}
					return 0;
				case 4:
					if (((p0 != 4))) {
						G = 7;
						continue;
					}
					fI1 = 1;
					G = 8;
					continue;
				case 7:
					fI1 = 0;
					G = 8;
					continue;
				case 8:
					this.flush_block_only_Z_V(((fI1)!=0));
					if (((this._strm._avail_out != 0))) {
						G = 9;
						continue;
					}
					if (((p0 != 4))) {
						G = 10;
						continue;
					}
					fI0 = 2;
					G = 11;
					continue;
				case 10:
					fI0 = 0;
					G = 11;
					continue;
				case 11: return fI0;
				case 9:
					if (((p0 != 4))) {
						G = 12;
						continue;
					}
					fI0 = 3;
					G = 13;
					continue;
				case 12:
					fI0 = 1;
					G = 13;
					continue;
				case 13: return fI0;
				default:
					break;
			}
		}
		return 0;
	}
	 com_jtransc_compression_jzlib_GZIPHeader getGZIPHeader__Lcom_jtransc_compression_jzlib_GZIPHeader_() {
		int G = 0;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((this._gheader != null))) {
						G = 1;
						continue;
					}
					tA0 = (new com_jtransc_compression_jzlib_GZIPHeader());
					fA1 = tA0;
					(tA0 as com_jtransc_compression_jzlib_GZIPHeader).com_jtransc_compression_jzlib_GZIPHeader_init___V();
					this._gheader = (fA1 as com_jtransc_compression_jzlib_GZIPHeader);
					G = 1;
					continue;
				case 1:
					return this._gheader;
				default:
					break;
			}
		}
		return null;
	}
	 void putShortMSB_I_V(int p0) {
		this.put_byte_B_V(N.i2b((N.I(p0 >> 8))));
		this.put_byte_B_V(N.i2b(p0));
		return;
	}
	 int deflateInit_II_I(int p0, int p1) {
		return this.deflateInit_IIIII_I(p0, 8, p1, 8, 0);
	}
	 int deflateInit_IIIII_I(int p0, int p1, int p2, int p3, int p4) {
		int G = 0;
		com_jtransc_compression_jzlib_ZStream fA0 = null;
		int lI1 = 0;
		int lI3 = 0;
		int lI6 = 0;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = p0;
					lI3 = p2;
					lI6 = 1;
					this._strm._msg = null;
					if (((lI1 != -1))) {
						G = 1;
						continue;
					}
					lI1 = 6;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 0))) {
						G = 2;
						continue;
					}
					lI6 = 0;
					lI3 = (N.ineg(lI3));
					G = 3;
					continue;
				case 2:
					if (((lI3 <= 15))) {
						G = 3;
						continue;
					}
					lI6 = 2;
					lI3 = (N.I(lI3 + -16));
					fA0 = this._strm;
					tA0 = (new com_jtransc_compression_jzlib_CRC32());
					fA1 = tA0;
					(tA0 as com_jtransc_compression_jzlib_CRC32).com_jtransc_compression_jzlib_CRC32_init___V();
					fA0._adler = (fA1 as com_jtransc_compression_jzlib_Checksum);
					G = 3;
					continue;
				case 3:
					if (((p3 < 1))) {
						G = 4;
						continue;
					}
					if (((p3 > 9))) {
						G = 4;
						continue;
					}
					if (((p1 != 8))) {
						G = 4;
						continue;
					}
					if (((lI3 < 9))) {
						G = 4;
						continue;
					}
					if (((lI3 > 15))) {
						G = 4;
						continue;
					}
					if (((lI1 < 0))) {
						G = 4;
						continue;
					}
					if (((lI1 > 9))) {
						G = 4;
						continue;
					}
					if (((p4 < 0))) {
						G = 4;
						continue;
					}
					if (((p4 <= 2))) {
						G = 5;
						continue;
					}
					G = 4;
					continue;
				case 4:
					return -2;
				case 5:
					this._strm._dstate = this;
					this._wrap = lI6;
					this._w_bits = lI3;
					this._w_size = (N.ishl(1, this._w_bits));
					this._w_mask = (N.I(this._w_size - 1));
					this._hash_bits = (N.I(p3 + 7));
					this._hash_size = (N.ishl(1, this._hash_bits));
					this._hash_mask = (N.I(this._hash_size - 1));
					this._hash_shift = (N.I((N.I((N.I(this._hash_bits + 3)) - 1)) ~/ 3));
					this._window = new JA_B((N.I(this._w_size * 2)));
					this._prev = new JA_S(this._w_size);
					this._head = new JA_S(this._hash_size);
					this._lit_bufsize = (N.ishl(1, (N.I(p3 + 6))));
					this._pending_buf = new JA_B((N.I(this._lit_bufsize * 3)));
					this._pending_buf_size = (N.I(this._lit_bufsize * 3));
					this._d_buf = this._lit_bufsize;
					this._l_buf = new JA_B(this._lit_bufsize);
					this._level = lI1;
					this._strategy = p4;
					this._method = N.i2b(p1);
					return this.deflateReset__I();
				default:
					break;
			}
		}
		return 0;
	}
	 int deflateReset__I() {
		int G = 0;
		Int64 fJ1 = N.lnew(0);
		int fI1 = 0;
		Int64 tJ0 = N.lnew(0);
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					fA0 = this._strm;
					fA1 = this._strm;
					tA1 = fA1;
					tJ0 = N.lnew(0);
					fJ1 = tJ0;
					(tA1 as com_jtransc_compression_jzlib_ZStream)._total_out = tJ0;
					(fA0 as com_jtransc_compression_jzlib_ZStream)._total_in = fJ1;
					this._strm._msg = null;
					this._strm._data_type = 2;
					this._pending = 0;
					this._pending_out = 0;
					if (((this._wrap >= 0))) {
						G = 1;
						continue;
					}
					this._wrap = (N.ineg(this._wrap));
					G = 1;
					continue;
				case 1:
					fA0 = this;
					if (((this._wrap != 0))) {
						G = 2;
						continue;
					}
					fI1 = 113;
					G = 3;
					continue;
				case 2:
					fI1 = 42;
					G = 3;
					continue;
				case 3:
					(fA0 as com_jtransc_compression_jzlib_Deflate)._status = fI1;
					this._strm._adler.reset__V();
					this._last_flush = 0;
					this.tr_init__V();
					this.lm_init__V();
					return 0;
				default:
					break;
			}
		}
		return 0;
	}
	 void lm_init__V() {
		int G = 0;
		int fI1 = 0;
		int lI1 = 0;
		int tI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					this._window_size = (N.I(2 * this._w_size));
					this._head.data[(N.I(this._hash_size - 1))] = 0;
					lI1 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= (N.I(this._hash_size - 1))))) {
						G = 2;
						continue;
					}
					this._head.data[lI1] = 0;
					lI1 = (N.I(lI1 + 1));
					G = 1;
					continue;
				case 2:
					this._max_lazy_match = (((com_jtransc_compression_jzlib_Deflate._config_table as JA_L)).data[this._level] as com_jtransc_compression_jzlib_Deflate_Config)._max_lazy;
					this._good_match = (((com_jtransc_compression_jzlib_Deflate._config_table as JA_L)).data[this._level] as com_jtransc_compression_jzlib_Deflate_Config)._good_length;
					this._nice_match = (((com_jtransc_compression_jzlib_Deflate._config_table as JA_L)).data[this._level] as com_jtransc_compression_jzlib_Deflate_Config)._nice_length;
					this._max_chain_length = (((com_jtransc_compression_jzlib_Deflate._config_table as JA_L)).data[this._level] as com_jtransc_compression_jzlib_Deflate_Config)._max_chain;
					this._strstart = 0;
					this._block_start = 0;
					this._lookahead = 0;
					tI0 = 2;
					fI1 = tI0;
					this._prev_length = tI0;
					this._match_length = fI1;
					this._match_available = 0;
					this._ins_h = 0;
					return;
				default:
					break;
			}
		}
		return;
	}
	 void tr_init__V() {
		this._l_desc._dyn_tree = this._dyn_ltree;
		this._l_desc._stat_desc = com_jtransc_compression_jzlib_StaticTree._static_l_desc;
		this._d_desc._dyn_tree = this._dyn_dtree;
		this._d_desc._stat_desc = com_jtransc_compression_jzlib_StaticTree._static_d_desc;
		this._bl_desc._dyn_tree = this._bl_tree;
		this._bl_desc._stat_desc = com_jtransc_compression_jzlib_StaticTree._static_bl_desc;
		this._bi_buf = 0;
		this._bi_valid = 0;
		this._last_eob_len = 8;
		this.init_block__V();
		return;
	}
	com_jtransc_compression_jzlib_Deflate([int CLASS_ID = 903]) : super(CLASS_ID) { }
	static void SI() {
		com_jtransc_compression_jzlib_Deflate._z_errmsg = null;
		com_jtransc_compression_jzlib_Deflate._config_table = null;
		com_jtransc_compression_jzlib_Deflate.com_jtransc_compression_jzlib_Deflate_clinit___V();
	}
}
class com_jtransc_compression_jzlib_ZStream extends java_lang_Object  {

	com_jtransc_compression_jzlib_Deflate _dstate = null;
	int _avail_out = 0;
	JA_B _next_out = null;
	JA_B _next_in = null;
	int _avail_in = 0;
	java_lang_String _msg = null;
	Int64 _total_in = N.lnew(0);
	com_jtransc_compression_jzlib_Checksum _adler = null;
	int _next_out_index = 0;
	Int64 _total_out = N.lnew(0);
	int _next_in_index = 0;
	int _data_type = 0;
	 void flush_pending__V() {
		int G = 0;
		int lI1 = 0;
		com_jtransc_compression_jzlib_Deflate tA1 = null;
		com_jtransc_compression_jzlib_Deflate tA4 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = this._dstate._pending;
					if (((lI1 <= this._avail_out))) {
						G = 1;
						continue;
					}
					lI1 = this._avail_out;
					G = 1;
					continue;
				case 1:
					if (((lI1 != 0))) {
						G = 2;
						continue;
					}
					return;
					G = 2;
					continue;
				case 2:
					if ((((this._dstate._pending_buf as JA_0).length <= this._dstate._pending_out))) {
						G = 3;
						continue;
					}
					if ((((this._next_out as JA_0).length <= this._next_out_index))) {
						G = 3;
						continue;
					}
					if ((((this._dstate._pending_buf as JA_0).length < (N.I(this._dstate._pending_out + lI1))))) {
						G = 3;
						continue;
					}
					if ((((this._next_out as JA_0).length >= (N.I(this._next_out_index + lI1))))) {
						G = 3;
						continue;
					}
					G = 3;
					continue;
				case 3:
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(this._dstate._pending_buf, this._dstate._pending_out, this._next_out, this._next_out_index, lI1);
					this._next_out_index = (N.I(this._next_out_index + lI1));
					tA1 = this._dstate;
					tA1._pending_out = (N.I(tA1._pending_out + lI1));
					this._total_out = ((this._total_out+N.i2j(lI1)));
					this._avail_out = (N.I(this._avail_out - lI1));
					tA4 = this._dstate;
					tA4._pending = (N.I(tA4._pending - lI1));
					if (((this._dstate._pending != 0))) {
						G = 4;
						continue;
					}
					this._dstate._pending_out = 0;
					G = 4;
					continue;
				case 4:
					return;
				default:
					break;
			}
		}
		return;
	}
	 int read_buf__BII_I(JA_B p0, int p1, int p2) {
		int G = 0;
		int lI4 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI4 = this._avail_in;
					if (((lI4 <= p2))) {
						G = 1;
						continue;
					}
					lI4 = p2;
					G = 1;
					continue;
				case 1:
					if (((lI4 != 0))) {
						G = 2;
						continue;
					}
					return 0;
				case 2:
					this._avail_in = (N.I(this._avail_in - lI4));
					if (((this._dstate._wrap == 0))) {
						G = 3;
						continue;
					}
					this._adler.update__BII_V(this._next_in, this._next_in_index, lI4);
					G = 3;
					continue;
				case 3:
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(this._next_in, this._next_in_index, p0, p1, lI4);
					this._next_in_index = (N.I(this._next_in_index + lI4));
					this._total_in = ((this._total_in+N.i2j(lI4)));
					return lI4;
				default:
					break;
			}
		}
		return 0;
	}
	 int deflate_I_I(int p0) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._dstate != null))) {
						G = 1;
						continue;
					}
					return -2;
				case 1:
					return this._dstate.deflate_I_I(p0);
				default:
					break;
			}
		}
		return 0;
	}
	 com_jtransc_compression_jzlib_ZStream com_jtransc_compression_jzlib_ZStream_init___V() {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		tA0 = (new com_jtransc_compression_jzlib_Adler32());
		fA1 = tA0;
		(tA0 as com_jtransc_compression_jzlib_Adler32).com_jtransc_compression_jzlib_Adler32_init___V();
		this.com_jtransc_compression_jzlib_ZStream_init__Lcom_jtransc_compression_jzlib_Checksum__V((fA1 as com_jtransc_compression_jzlib_Checksum));
		return this;
		return this;
	}
	 com_jtransc_compression_jzlib_ZStream com_jtransc_compression_jzlib_ZStream_init__Lcom_jtransc_compression_jzlib_Checksum__V(com_jtransc_compression_jzlib_Checksum p0) {
		this.java_lang_Object_init___V();
		this._adler = p0;
		return this;
		return this;
	}
	 void setOutput__BII_V(JA_B p0, int p1, int p2) {
		this._next_out = p0;
		this._next_out_index = p1;
		this._avail_out = p2;
		return;
	}
	 void setInput__BIIZ_V(JA_B p0, int p1, int p2, bool p3) {
		int G = 0;
		java_lang_Object lA5 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p2 > 0))) {
						G = 1;
						continue;
					}
					if (!(p3)) {
						G = 1;
						continue;
					}
					if (((this._next_in == null))) {
						G = 1;
						continue;
					}
					return;
					G = 1;
					continue;
				case 1:
					if (((this._avail_in <= 0))) {
						G = 2;
						continue;
					}
					if (!(p3)) {
						G = 2;
						continue;
					}
					lA5 = new JA_B((N.I(this._avail_in + p2)));
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(this._next_in, this._next_in_index, lA5, 0, this._avail_in);
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(p0, p1, lA5, this._avail_in, p2);
					this._next_in = (lA5 as JA_B);
					this._next_in_index = 0;
					this._avail_in = (N.I(this._avail_in + p2));
					G = 3;
					continue;
				case 2:
					this._next_in = p0;
					this._next_in_index = p1;
					this._avail_in = p2;
					G = 3;
					continue;
				case 3:
					return;
				default:
					break;
			}
		}
		return;
	}
	 Int64 getTotalOut__J() {
		return this._total_out;
	}
	com_jtransc_compression_jzlib_ZStream([int CLASS_ID = 902]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_compression_jzlib_Deflater extends com_jtransc_compression_jzlib_ZStream  {

	bool _finished = false;
	 int deflate_I_I(int p0) {
		int G = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._dstate != null))) {
						G = 1;
						continue;
					}
					return -2;
				case 1:
					lI2 = this._dstate.deflate_I_I(p0);
					if (((lI2 != 1))) {
						G = 2;
						continue;
					}
					this._finished = true;
					G = 2;
					continue;
				case 2:
					return lI2;
				default:
					break;
			}
		}
		return 0;
	}
	 com_jtransc_compression_jzlib_Deflater com_jtransc_compression_jzlib_Deflater_init__IZ_V(int p0, bool p1) {
		this.com_jtransc_compression_jzlib_Deflater_init__IIZ_V(p0, 15, p1);
		return this;
		return this;
	}
	 com_jtransc_compression_jzlib_Deflater com_jtransc_compression_jzlib_Deflater_init__IIZ_V(int p0, int p1, bool p2) {
		int G = 0;
		int lI4 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					this.com_jtransc_compression_jzlib_ZStream_init___V();
					this._finished = false;
					lI4 = this.init_IIZ_I(p0, p1, p2);
					if (((lI4 == 0))) {
						G = 1;
						continue;
					}
					tA0 = (new com_jtransc_compression_jzlib_GZIPException());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as com_jtransc_compression_jzlib_GZIPException).com_jtransc_compression_jzlib_GZIPException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_I_Ljava_lang_StringBuilder_(lI4).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_27).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this._msg).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 com_jtransc_compression_jzlib_Deflater com_jtransc_compression_jzlib_Deflater_init___V() {
		this.com_jtransc_compression_jzlib_ZStream_init___V();
		this._finished = false;
		return this;
		return this;
	}
	 int init_IIZ_I(int p0, int p1, bool p2) {
		int G = 0;
		int fI1 = 0;
		int fI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					this._finished = false;
					fA0 = this;
					tA0 = (new com_jtransc_compression_jzlib_Deflate());
					fA1 = tA0;
					(tA0 as com_jtransc_compression_jzlib_Deflate).com_jtransc_compression_jzlib_Deflate_init__Lcom_jtransc_compression_jzlib_ZStream__V(this);
					(fA0 as com_jtransc_compression_jzlib_Deflater)._dstate = (fA1 as com_jtransc_compression_jzlib_Deflate);
					fA0 = this._dstate;
					fI1 = p0;
					if (!(p2)) {
						G = 1;
						continue;
					}
					fI2 = (N.ineg(p1));
					G = 2;
					continue;
				case 1:
					fI2 = p1;
					G = 2;
					continue;
				case 2: return (fA0 as com_jtransc_compression_jzlib_Deflate).deflateInit_II_I(fI1, fI2);
				default:
					break;
			}
		}
		return 0;
	}
	com_jtransc_compression_jzlib_Deflater([int CLASS_ID = 901]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_compression_jzlib_CRC32 extends java_lang_Object implements com_jtransc_compression_jzlib_Checksum {

	int _v = 0;
	static JA_I _crc_table = null;
	 com_jtransc_compression_jzlib_CRC32 com_jtransc_compression_jzlib_CRC32_init___V() {
		this.java_lang_Object_init___V();
		this._v = 0;
		return this;
		return this;
	}
	static void com_jtransc_compression_jzlib_CRC32_clinit___V() {
		int G = 0;
		int lI0 = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					com_jtransc_compression_jzlib_CRC32._crc_table = null;
					com_jtransc_compression_jzlib_CRC32._crc_table = new JA_I(256);
					lI0 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI0 >= 256))) {
						G = 2;
						continue;
					}
					lI1 = lI0;
					lI2 = 8;
					G = 3;
					continue;
				case 3:
					lI2 = (N.I(lI2 + -1));
					if (((lI2 < 0))) {
						G = 4;
						continue;
					}
					if ((((N.I(lI1 & 1)) == 0))) {
						G = 5;
						continue;
					}
					lI1 = (N.I(-306674912 ^ (N.iushr_opt(lI1, 1))));
					G = 3;
					continue;
				case 5:
					lI1 = (N.iushr_opt(lI1, 1));
					G = 3;
					continue;
				case 4:
					com_jtransc_compression_jzlib_CRC32._crc_table.data[lI0] = lI1;
					lI0 = (N.I(lI0 + 1));
					G = 1;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	 int getValue__I() {
		return this._v;
	}
	 void reset__V() {
		this._v = 0;
		return;
	}
	 void update__BII_V(JA_B p0, int p1, int p2) {
		int G = 0;
		int fI1 = 0;
		int fI3 = 0;
		int lI2 = 0;
		int lI3 = 0;
		int lI4 = 0;
		JA_I fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lI2 = p1;
					lI3 = p2;
					lI4 = (N.I(this._v ^ -1));
					G = 1;
					continue;
				case 1:
					lI3 = (N.I(lI3 + -1));
					if (((lI3 < 0))) {
						G = 2;
						continue;
					}
					fA0 = com_jtransc_compression_jzlib_CRC32._crc_table;
					fI1 = lI4;
					fI3 = lI2;
					lI2 = (N.I(lI2 + 1));
					lI4 = (N.I((fA0).data[(N.I((N.I(fI1 ^ (p0).data[fI3])) & 255))] ^ (N.iushr_opt(lI4, 8))));
					G = 1;
					continue;
				case 2:
					this._v = (N.I(lI4 ^ -1));
					return;
				default:
					break;
			}
		}
		return;
	}
	com_jtransc_compression_jzlib_CRC32([int CLASS_ID = 899]) : super(CLASS_ID) { }
	static void SI() {
		com_jtransc_compression_jzlib_CRC32._crc_table = null;
		com_jtransc_compression_jzlib_CRC32.com_jtransc_compression_jzlib_CRC32_clinit___V();
	}
}
abstract class java_util_zip_Checksum   {

	 Int64 getValue__J();
	 void reset__V();
	 void update__BII_V(JA_B p0, int p1, int p2);
}
class java_util_zip_Checksum_IFields {

	static void SI() { }
}
class java_util_zip_CRC32 extends java_lang_Object implements java_util_zip_Checksum {

	com_jtransc_compression_jzlib_CRC32 _impl = null;
	Int64 _tbytes = N.lnew(0);
	static JA_B _temp = null;
	 java_util_zip_CRC32 java_util_zip_CRC32_init___V() {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		this.java_lang_Object_init___V();
		tA0 = (new com_jtransc_compression_jzlib_CRC32());
		fA1 = tA0;
		(tA0 as com_jtransc_compression_jzlib_CRC32).com_jtransc_compression_jzlib_CRC32_init___V();
		this._impl = (fA1 as com_jtransc_compression_jzlib_CRC32);
		this._tbytes = N.lnew(0);
		return this;
		return this;
	}
	static void java_util_zip_CRC32_clinit___V() {
		java_util_zip_CRC32._temp = new JA_B(1);
		return;
	}
	 Int64 getValue__J() {
		return ((N.i2j(this._impl.getValue__I())&N.lnew(4294967295)));
	}
	 void reset__V() {
		this._impl.reset__V();
		this._tbytes = N.lnew(0);
		return;
	}
	 void update__BII_V(JA_B p0, int p1, int p2) {
		this._update__BII_V(p0, p1, p2);
		return;
	}
	 void _update__BII_V(JA_B p0, int p1, int p2) {
		this._impl.update__BII_V(p0, p1, p2);
		this._tbytes = ((this._tbytes+N.i2j(p2)));
		return;
	}
	java_util_zip_CRC32([int CLASS_ID = 897]) : super(CLASS_ID) { }
	static void SI() {
		java_util_zip_CRC32._temp = null;
		java_util_zip_CRC32.java_util_zip_CRC32_clinit___V();
	}
}
abstract class java_nio_internal_ByteBufferAs   {

}
class java_nio_internal_ByteBufferAs_IFields {

	static void SI() { }
}
abstract class java_nio_Buffer extends java_lang_Object  {

	int __elementSizeShift = 0;
	int _mark = 0;
	java_nio_internal_MemoryBlock _block = null;
	int _position = 0;
	int _limit = 0;
	int _capacity = 0;
	 java_nio_Buffer java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(int p0, int p1, java_nio_internal_MemoryBlock p2) {
		int G = 0;
		int fI1 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA3 = null;
		while (true) {
			switch (G) {
				case 0:
					this.java_lang_Object_init___V();
					this._mark = -1;
					this._position = 0;
					this.__elementSizeShift = p0;
					if (((p1 >= 0))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_IllegalArgumentException());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_IllegalArgumentException).java_lang_IllegalArgumentException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_28).append_I_Ljava_lang_StringBuilder_(p1).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					fA0 = this;
					fA1 = this;
					tA3 = fA1;
					fI1 = p1;
					(tA3 as java_nio_Buffer)._limit = p1;
					(fA0 as java_nio_Buffer)._capacity = fI1;
					this._block = p2;
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getClass__Ljava_lang_Class_().getName__Ljava_lang_String_()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_32).append_I_Ljava_lang_StringBuilder_(this._position).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_31).append_I_Ljava_lang_StringBuilder_(this._limit).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_30).append_I_Ljava_lang_StringBuilder_(this._capacity).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_29).toString__Ljava_lang_String_();
	}
	 int remaining__I() {
		return (N.I(this._limit - this._position));
	}
	 void checkIndex_I_V(int p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 < 0))) {
						G = 1;
						continue;
					}
					if (((p0 < this._limit))) {
						G = 2;
						continue;
					}
					G = 1;
					continue;
				case 1:
					tA0 = (new java_lang_IndexOutOfBoundsException());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_IndexOutOfBoundsException).java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_34).append_I_Ljava_lang_StringBuilder_(p0).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_33).append_I_Ljava_lang_StringBuilder_(this._limit).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	 int capacity__I() {
		return this._capacity;
	}
	 java_nio_Buffer clear__Ljava_nio_Buffer_() {
		this._position = 0;
		this._mark = -1;
		this._limit = this._capacity;
		return this;
	}
	java_nio_Buffer([int CLASS_ID = 862]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_nio_IntBuffer extends java_nio_Buffer implements java_lang_Comparable {

	 java_nio_IntBuffer java_nio_IntBuffer_init__I_V(int p0) {
		this.java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(2, p0, null);
		return this;
		return this;
	}
	 int hashCode__I() {
		int G = 0;
		int fI0 = 0;
		int fI2 = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = this._position;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= this._limit))) {
						G = 2;
						continue;
					}
					fI0 = lI2;
					fI2 = lI1;
					lI1 = (N.I(lI1 + 1));
					lI2 = (N.I(fI0 + this.get_I_I(fI2)));
					G = 1;
					continue;
				case 2:
					return lI2;
				default:
					break;
			}
		}
		return 0;
	}
	 int get_I_I(int p0) {
		throw new Exception("Missing body java.nio.IntBuffer.get\u0028I\u0029I");
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		int fI1 = 0;
		int fI0 = 0;
		int fI2 = 0;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		java_lang_Object fA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_nio_IntBuffer)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					lA2 = ((p0) as java_nio_IntBuffer);
					if (((this.remaining__I() == (lA2 as java_nio_IntBuffer).remaining__I()))) {
						G = 2;
						continue;
					}
					return false;
				case 2:
					lI3 = this._position;
					lI4 = (lA2 as java_nio_IntBuffer)._position;
					lI5 = 1;
					G = 3;
					continue;
				case 3:
					if (((lI5 == 0))) {
						G = 4;
						continue;
					}
					if (((lI3 >= this._limit))) {
						G = 4;
						continue;
					}
					fI1 = lI3;
					lI3 = (N.I(lI3 + 1));
					fI0 = this.get_I_I(fI1);
					fA1 = lA2;
					fI2 = lI4;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1 as java_nio_IntBuffer).get_I_I(fI2)))) {
						G = 5;
						continue;
					}
					fI0 = 1;
					G = 6;
					continue;
				case 5:
					fI0 = 0;
					G = 6;
					continue;
				case 6:
					lI5 = fI0;
					G = 3;
					continue;
				case 4:
					return ((lI5)!=0);
				default:
					break;
			}
		}
		return false;
	}
	java_nio_IntBuffer([int CLASS_ID = 861]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_nio_ByteBufferAsIntBuffer extends java_nio_IntBuffer implements java_nio_internal_ByteBufferAs {
	Int32List tarray;
	java_nio_ByteBuffer _byteBuffer = null;
	JA_B _bytes = null;
	 java_nio_ByteBufferAsIntBuffer java_nio_ByteBufferAsIntBuffer_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_IntBuffer_init__I_V((N.I(p0.capacity__I() ~/ 4)));
		this._byteBuffer = p0;
		this._byteBuffer.clear__Ljava_nio_Buffer_();
		this._bytes = p0.array___B();
		this.init__B_V(p0.array___B());
		return this;
		return this;
	}
	 void init__B_V(JA_B p0) {
		this.tarray = new Int32List.view(p0.data.buffer);
	}
	 int get_I_I(int p0) {
		return this.tarray[p0];
	}
	static java_nio_IntBuffer asIntBuffer_Ljava_nio_ByteBuffer__Ljava_nio_IntBuffer_(java_nio_ByteBuffer p0) {
		java_nio_ByteBuffer lA1 = null;
		lA1 = p0.slice__Ljava_nio_ByteBuffer_();
		lA1.order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(p0.order__Ljava_nio_ByteOrder_());
		return java_nio_ByteBufferAsIntBuffer.create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsIntBuffer_(lA1, p0._isLittleEndian);
	}
	static java_nio_ByteBufferAsIntBuffer create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsIntBuffer_(java_nio_ByteBuffer p0, bool p1) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(p1)) {
						G = 1;
						continue;
					}
					tA0 = (new java_nio_ByteBufferAsIntBuffer_LE());
					fA0 = tA0;
					(tA0 as java_nio_ByteBufferAsIntBuffer_LE).java_nio_ByteBufferAsIntBuffer_LE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 1:
					tA1 = (new java_nio_ByteBufferAsIntBuffer_BE());
					fA0 = tA1;
					(tA1 as java_nio_ByteBufferAsIntBuffer_BE).java_nio_ByteBufferAsIntBuffer_BE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 2: return (fA0 as java_nio_ByteBufferAsIntBuffer);
				default:
					break;
			}
		}
		return null;
	}
	java_nio_ByteBufferAsIntBuffer([int CLASS_ID = 894]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsIntBuffer_LE extends java_nio_ByteBufferAsIntBuffer  {

	 java_nio_ByteBufferAsIntBuffer_LE java_nio_ByteBufferAsIntBuffer_LE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsIntBuffer_init__Ljava_nio_ByteBuffer__V(p0);
		return this;
		return this;
	}
	 int get_I_I(int p0) {
		return this.tarray[p0];
	}
	java_nio_ByteBufferAsIntBuffer_LE([int CLASS_ID = 896]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsIntBuffer_BE extends java_nio_ByteBufferAsIntBuffer  {

	 java_nio_ByteBufferAsIntBuffer_BE java_nio_ByteBufferAsIntBuffer_BE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsIntBuffer_init__Ljava_nio_ByteBuffer__V(p0);
		return this;
		return this;
	}
	 int get_I_I(int p0) {
		return libcore_io_Memory.peekAlignedIntBE__BI_I(this._bytes, p0);
	}
	java_nio_ByteBufferAsIntBuffer_BE([int CLASS_ID = 895]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_nio_ShortBuffer extends java_nio_Buffer implements java_lang_Comparable {

	 java_nio_ShortBuffer java_nio_ShortBuffer_init__I_V(int p0) {
		this.java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(1, p0, null);
		return this;
		return this;
	}
	 int hashCode__I() {
		int G = 0;
		int fI0 = 0;
		int fI2 = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = this._position;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= this._limit))) {
						G = 2;
						continue;
					}
					fI0 = lI2;
					fI2 = lI1;
					lI1 = (N.I(lI1 + 1));
					lI2 = (N.I(fI0 + this.get_I_S(fI2)));
					G = 1;
					continue;
				case 2:
					return lI2;
				default:
					break;
			}
		}
		return 0;
	}
	 int get_I_S(int p0) {
		throw new Exception("Missing body java.nio.ShortBuffer.get\u0028I\u0029S");
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		int fI1 = 0;
		int fI0 = 0;
		int fI2 = 0;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		java_lang_Object fA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_nio_ShortBuffer)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					lA2 = ((p0) as java_nio_ShortBuffer);
					if (((this.remaining__I() == (lA2 as java_nio_ShortBuffer).remaining__I()))) {
						G = 2;
						continue;
					}
					return false;
				case 2:
					lI3 = this._position;
					lI4 = (lA2 as java_nio_ShortBuffer)._position;
					lI5 = 1;
					G = 3;
					continue;
				case 3:
					if (((lI5 == 0))) {
						G = 4;
						continue;
					}
					if (((lI3 >= this._limit))) {
						G = 4;
						continue;
					}
					fI1 = lI3;
					lI3 = (N.I(lI3 + 1));
					fI0 = (this.get_I_S(fI1));
					fA1 = lA2;
					fI2 = lI4;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1 as java_nio_ShortBuffer).get_I_S(fI2)))) {
						G = 5;
						continue;
					}
					fI0 = 1;
					G = 6;
					continue;
				case 5:
					fI0 = 0;
					G = 6;
					continue;
				case 6:
					lI5 = fI0;
					G = 3;
					continue;
				case 4:
					return ((lI5)!=0);
				default:
					break;
			}
		}
		return false;
	}
	java_nio_ShortBuffer([int CLASS_ID = 870]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsShortBuffer extends java_nio_ShortBuffer implements java_nio_internal_ByteBufferAs {
	Int16List tarray;
	java_nio_ByteBuffer _byteBuffer = null;
	JA_B _bytes = null;
	 int get_I_S(int p0) {
		return this.tarray[p0];
	}
	static java_nio_ShortBuffer asShortBuffer_Ljava_nio_ByteBuffer__Ljava_nio_ShortBuffer_(java_nio_ByteBuffer p0) {
		java_nio_ByteBuffer lA1 = null;
		lA1 = p0.slice__Ljava_nio_ByteBuffer_();
		lA1.order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(p0.order__Ljava_nio_ByteOrder_());
		return java_nio_ByteBufferAsShortBuffer.create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsShortBuffer_(lA1, p0._isLittleEndian);
	}
	static java_nio_ByteBufferAsShortBuffer create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsShortBuffer_(java_nio_ByteBuffer p0, bool p1) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(p1)) {
						G = 1;
						continue;
					}
					tA0 = (new java_nio_ByteBufferAsShortBuffer_LE());
					fA0 = tA0;
					(tA0 as java_nio_ByteBufferAsShortBuffer_LE).java_nio_ByteBufferAsShortBuffer_LE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 1:
					tA1 = (new java_nio_ByteBufferAsShortBuffer_BE());
					fA0 = tA1;
					(tA1 as java_nio_ByteBufferAsShortBuffer_BE).java_nio_ByteBufferAsShortBuffer_BE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 2: return (fA0 as java_nio_ByteBufferAsShortBuffer);
				default:
					break;
			}
		}
		return null;
	}
	 java_nio_ByteBufferAsShortBuffer java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsShortBuffer_1__V(java_nio_ByteBuffer p0, java_nio_ByteBufferAsShortBuffer_1 p1) {
		this.java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer__V(p0);
		return this;
		return this;
	}
	 java_nio_ByteBufferAsShortBuffer java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ShortBuffer_init__I_V((N.I(p0.capacity__I() ~/ 2)));
		this._byteBuffer = p0;
		this._byteBuffer.clear__Ljava_nio_Buffer_();
		this._bytes = p0.array___B();
		this.init__B_V(p0.array___B());
		return this;
		return this;
	}
	 void init__B_V(JA_B p0) {
		this.tarray = new Int16List.view(p0.data.buffer);
	}
	java_nio_ByteBufferAsShortBuffer([int CLASS_ID = 890]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsShortBuffer_BE extends java_nio_ByteBufferAsShortBuffer  {

	 java_nio_ByteBufferAsShortBuffer_BE java_nio_ByteBufferAsShortBuffer_BE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsShortBuffer_1__V(p0, null);
		return this;
		return this;
	}
	 int get_I_S(int p0) {
		return java_lang_Short.reverseBytes_S_S(super.get_I_S(p0));
	}
	java_nio_ByteBufferAsShortBuffer_BE([int CLASS_ID = 893]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsShortBuffer_1 extends java_lang_Object  {

	java_nio_ByteBufferAsShortBuffer_1([int CLASS_ID = 892]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsShortBuffer_LE extends java_nio_ByteBufferAsShortBuffer  {

	 java_nio_ByteBufferAsShortBuffer_LE java_nio_ByteBufferAsShortBuffer_LE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsShortBuffer_1__V(p0, null);
		return this;
		return this;
	}
	 int get_I_S(int p0) {
		return this.tarray[p0];
	}
	java_nio_ByteBufferAsShortBuffer_LE([int CLASS_ID = 891]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_Readable   {

}
class java_lang_Readable_IFields {

	static void SI() { }
}
abstract class java_lang_Appendable   {

}
class java_lang_Appendable_IFields {

	static void SI() { }
}
abstract class java_lang_CharSequence   {

	 java_lang_String toString__Ljava_lang_String_();
	 int length__I();
	 int charAt_I_C(int p0);
}
class java_lang_CharSequence_IFields {

	static void SI() { }
}
abstract class java_nio_CharBuffer extends java_nio_Buffer implements java_lang_Comparable, java_lang_CharSequence, java_lang_Appendable, java_lang_Readable {

	 java_nio_CharBuffer java_nio_CharBuffer_init__I_V(int p0) {
		this.java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(1, p0, null);
		return this;
		return this;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		int G = 0;
		java_lang_Object lA1 = null;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init__I_V((N.I(this._limit - this._position)));
					lA1 = fA0;
					lI2 = this._position;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= this._limit))) {
						G = 2;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(this.get_I_C(lI2));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return (lA1 as java_lang_StringBuilder).toString__Ljava_lang_String_();
				default:
					break;
			}
		}
		return null;
	}
	 int get_I_C(int p0) {
		throw new Exception("Missing body java.nio.CharBuffer.get\u0028I\u0029C");
	}
	 int hashCode__I() {
		int G = 0;
		int fI0 = 0;
		int fI2 = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = this._position;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= this._limit))) {
						G = 2;
						continue;
					}
					fI0 = lI2;
					fI2 = lI1;
					lI1 = (N.I(lI1 + 1));
					lI2 = (N.I(fI0 + this.get_I_C(fI2)));
					G = 1;
					continue;
				case 2:
					return lI2;
				default:
					break;
			}
		}
		return 0;
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		int fI1 = 0;
		int fI0 = 0;
		int fI2 = 0;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		java_lang_Object fA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_nio_CharBuffer)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					lA2 = ((p0) as java_nio_CharBuffer);
					if (((this.remaining__I() == (lA2 as java_nio_CharBuffer).remaining__I()))) {
						G = 2;
						continue;
					}
					return false;
				case 2:
					lI3 = this._position;
					lI4 = (lA2 as java_nio_CharBuffer)._position;
					lI5 = 1;
					G = 3;
					continue;
				case 3:
					if (((lI5 == 0))) {
						G = 4;
						continue;
					}
					if (((lI3 >= this._limit))) {
						G = 4;
						continue;
					}
					fI1 = lI3;
					lI3 = (N.I(lI3 + 1));
					fI0 = (this.get_I_C(fI1));
					fA1 = lA2;
					fI2 = lI4;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1 as java_nio_CharBuffer).get_I_C(fI2)))) {
						G = 5;
						continue;
					}
					fI0 = 1;
					G = 6;
					continue;
				case 5:
					fI0 = 0;
					G = 6;
					continue;
				case 6:
					lI5 = fI0;
					G = 3;
					continue;
				case 4:
					return ((lI5)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 int length__I() {
		return this.remaining__I();
	}
	 int charAt_I_C(int p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 < 0))) {
						G = 1;
						continue;
					}
					if (((p0 < this.remaining__I()))) {
						G = 2;
						continue;
					}
					G = 1;
					continue;
				case 1:
					tA0 = (new java_lang_IndexOutOfBoundsException());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_IndexOutOfBoundsException).java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_34).append_I_Ljava_lang_StringBuilder_(p0).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_35).append_I_Ljava_lang_StringBuilder_(this.remaining__I()).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					return this.get_I_C((N.I(this._position + p0)));
				default:
					break;
			}
		}
		return 0;
	}
	 java_nio_CharBuffer put_IC_Ljava_nio_CharBuffer_(int p0, int p1) {
		throw new Exception("Missing body java.nio.CharBuffer.put\u0028IC\u0029Ljava/nio/CharBuffer;");
	}
	java_nio_CharBuffer([int CLASS_ID = 867]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_nio_ByteBufferAsCharBuffer extends java_nio_CharBuffer implements java_nio_internal_ByteBufferAs {
	Uint16List tarray;
	JA_B _bytes = null;
	java_nio_ByteBuffer _byteBuffer = null;
	 int get_I_C(int p0) {
		return this.tarray[p0];
	}
	static java_nio_CharBuffer asCharBuffer_Ljava_nio_ByteBuffer__Ljava_nio_CharBuffer_(java_nio_ByteBuffer p0) {
		java_nio_ByteBuffer lA1 = null;
		lA1 = p0.slice__Ljava_nio_ByteBuffer_();
		lA1.order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(p0.order__Ljava_nio_ByteOrder_());
		return java_nio_ByteBufferAsCharBuffer.create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsCharBuffer_(lA1, p0._isLittleEndian);
	}
	static java_nio_ByteBufferAsCharBuffer create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsCharBuffer_(java_nio_ByteBuffer p0, bool p1) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(p1)) {
						G = 1;
						continue;
					}
					tA0 = (new java_nio_ByteBufferAsCharBuffer_LE());
					fA0 = tA0;
					(tA0 as java_nio_ByteBufferAsCharBuffer_LE).java_nio_ByteBufferAsCharBuffer_LE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 1:
					tA1 = (new java_nio_ByteBufferAsCharBuffer_BE());
					fA0 = tA1;
					(tA1 as java_nio_ByteBufferAsCharBuffer_BE).java_nio_ByteBufferAsCharBuffer_BE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 2: return (fA0 as java_nio_ByteBufferAsCharBuffer);
				default:
					break;
			}
		}
		return null;
	}
	 java_nio_ByteBufferAsCharBuffer java_nio_ByteBufferAsCharBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsCharBuffer_1__V(java_nio_ByteBuffer p0, java_nio_ByteBufferAsCharBuffer_1 p1) {
		this.java_nio_ByteBufferAsCharBuffer_init__Ljava_nio_ByteBuffer__V(p0);
		return this;
		return this;
	}
	 java_nio_ByteBufferAsCharBuffer java_nio_ByteBufferAsCharBuffer_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_CharBuffer_init__I_V((N.I(p0.capacity__I() ~/ 2)));
		this._byteBuffer = p0;
		this._byteBuffer.clear__Ljava_nio_Buffer_();
		this._bytes = p0.array___B();
		this.init__B_V(p0.array___B());
		return this;
		return this;
	}
	 void init__B_V(JA_B p0) {
		this.tarray = new Uint16List.view(p0.data.buffer);
	}
	 java_nio_CharBuffer put_IC_Ljava_nio_CharBuffer_(int p0, int p1) {
		this.tarray[p0] = p1; return this;
	}
	java_nio_ByteBufferAsCharBuffer([int CLASS_ID = 886]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsCharBuffer_BE extends java_nio_ByteBufferAsCharBuffer  {

	 java_nio_ByteBufferAsCharBuffer_BE java_nio_ByteBufferAsCharBuffer_BE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsCharBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsCharBuffer_1__V(p0, null);
		return this;
		return this;
	}
	 int get_I_C(int p0) {
		return java_lang_Character.reverseBytes_C_C(super.get_I_C(p0));
	}
	 java_nio_CharBuffer put_IC_Ljava_nio_CharBuffer_(int p0, int p1) {
		return super.put_IC_Ljava_nio_CharBuffer_(p0, java_lang_Character.reverseBytes_C_C(p1));
	}
	java_nio_ByteBufferAsCharBuffer_BE([int CLASS_ID = 889]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsCharBuffer_1 extends java_lang_Object  {

	java_nio_ByteBufferAsCharBuffer_1([int CLASS_ID = 888]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsCharBuffer_LE extends java_nio_ByteBufferAsCharBuffer  {

	 java_nio_ByteBufferAsCharBuffer_LE java_nio_ByteBufferAsCharBuffer_LE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsCharBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsCharBuffer_1__V(p0, null);
		return this;
		return this;
	}
	 int get_I_C(int p0) {
		return this.tarray[p0];
	}
	 java_nio_CharBuffer put_IC_Ljava_nio_CharBuffer_(int p0, int p1) {
		this.tarray[p0] = p1; return this;
	}
	java_nio_ByteBufferAsCharBuffer_LE([int CLASS_ID = 887]) : super(CLASS_ID) { }
	static void SI() { }
}
class libcore_io_Memory extends java_lang_Object  {

	static java_nio_ByteOrder _SWAPPED = null;
	static java_nio_ByteOrder _NATIVE = null;
	 libcore_io_Memory libcore_io_Memory_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void libcore_io_Memory_clinit___V() {
		int G = 0;
		java_nio_ByteOrder fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					libcore_io_Memory._NATIVE = java_nio_ByteOrder.nativeOrder__Ljava_nio_ByteOrder_();
					if (((libcore_io_Memory._NATIVE != java_nio_ByteOrder._LITTLE_ENDIAN))) {
						G = 1;
						continue;
					}
					fA0 = java_nio_ByteOrder._BIG_ENDIAN;
					G = 2;
					continue;
				case 1:
					fA0 = java_nio_ByteOrder._LITTLE_ENDIAN;
					G = 2;
					continue;
				case 2:
					libcore_io_Memory._SWAPPED = fA0;
					return;
				default:
					break;
			}
		}
		return;
	}
	static Int64 peekAlignedLongLE__BI_J(JA_B p0, int p1) {
		return libcore_io_Memory.peekLongLE__BI_J(p0, (N.I(p1 * 8)));
	}
	static Int64 peekLongLE__BI_J(JA_B p0, int p1) {
		JA_B fA0 = null;
		JA_B fA1 = null;
		int fI1 = 0;
		int fI0 = 0;
		int fI2 = 0;
		int lI1 = 0;
		int lI2 = 0;
		int lI3 = 0;
		lI1 = p1;
		fA0 = p0;
		fI1 = lI1;
		lI1 = (N.I(lI1 + 1));
		fI0 = (N.I((N.I((fA0).data[fI1] & 255)) << 0));
		fA1 = p0;
		fI2 = lI1;
		lI1 = (N.I(lI1 + 1));
		fI0 = (N.I(fI0 | (N.I((N.I((fA1).data[fI2] & 255)) << 8))));
		fA1 = p0;
		fI2 = lI1;
		lI1 = (N.I(lI1 + 1));
		fI0 = (N.I(fI0 | (N.I((N.I((fA1).data[fI2] & 255)) << 16))));
		fA1 = p0;
		fI2 = lI1;
		lI1 = (N.I(lI1 + 1));
		lI2 = (N.I(fI0 | (N.I((N.I((fA1).data[fI2] & 255)) << 24))));
		fA0 = p0;
		fI1 = lI1;
		lI1 = (N.I(lI1 + 1));
		fI0 = (N.I((N.I((fA0).data[fI1] & 255)) << 0));
		fA1 = p0;
		fI2 = lI1;
		lI1 = (N.I(lI1 + 1));
		fI0 = (N.I(fI0 | (N.I((N.I((fA1).data[fI2] & 255)) << 8))));
		fA1 = p0;
		fI2 = lI1;
		lI1 = (N.I(lI1 + 1));
		lI3 = (N.I((N.I(fI0 | (N.I((N.I((fA1).data[fI2] & 255)) << 16)))) | (N.I((N.I((p0).data[lI1] & 255)) << 24))));
		return ((((N.i2j(lI3) << 32))|((N.i2j(lI2)&N.lnew(4294967295)))));
	}
	static int peekAlignedIntBE__BI_I(JA_B p0, int p1) {
		return java_lang_Integer.reverseBytes_I_I(libcore_io_Memory.peekAlignedIntLE__BI_I(p0, p1));
	}
	static int peekAlignedIntLE__BI_I(JA_B p0, int p1) {
		return libcore_io_Memory.peekIntLE__BI_I(p0, (N.I(p1 * 4)));
	}
	static int peekIntLE__BI_I(JA_B p0, int p1) {
		JA_B fA1 = null;
		int fI1 = 0;
		int fI0 = 0;
		int fI2 = 0;
		int lI1 = 0;
		lI1 = p1;
		fI1 = lI1;
		lI1 = (N.I(lI1 + 1));
		fI0 = (N.I((N.I((p0).data[fI1] & 255)) << 0));
		fA1 = p0;
		fI2 = lI1;
		lI1 = (N.I(lI1 + 1));
		fI0 = (N.I(fI0 | (N.I((N.I((fA1).data[fI2] & 255)) << 8))));
		fA1 = p0;
		fI2 = lI1;
		lI1 = (N.I(lI1 + 1));
		return (N.I((N.I(fI0 | (N.I((N.I((fA1).data[fI2] & 255)) << 16)))) | (N.I((N.I((p0).data[lI1] & 255)) << 24))));
	}
	static void pokeAlignedLongLE__BIJ_V(JA_B p0, int p1, Int64 p2) {
		com_jtransc_JTranscBits.writeLongLE__BIJ_V(p0, (N.I(p1 * 8)), p2);
		return;
	}
	libcore_io_Memory([int CLASS_ID = 885]) : super(CLASS_ID) { }
	static void SI() {
		libcore_io_Memory._SWAPPED = null;
		libcore_io_Memory._NATIVE = null;
		libcore_io_Memory.libcore_io_Memory_clinit___V();
	}
}
abstract class java_nio_DoubleBuffer extends java_nio_Buffer implements java_lang_Comparable {

	 java_nio_DoubleBuffer java_nio_DoubleBuffer_init__I_V(int p0) {
		this.java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(3, p0, null);
		return this;
		return this;
	}
	 int hashCode__I() {
		int G = 0;
		int fI1 = 0;
		int lI1 = 0;
		int lI2 = 0;
		Int64 lJ3 = N.lnew(0);
		while (true) {
			switch (G) {
				case 0:
					lI1 = this._position;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= this._limit))) {
						G = 2;
						continue;
					}
					fI1 = lI1;
					lI1 = (N.I(lI1 + 1));
					lJ3 = java_lang_Double.doubleToLongBits_D_J(this.get_I_D(fI1));
					lI2 = (N.I((N.I(lI2 + N.j2i(lJ3))) ^ N.j2i(((lJ3 >> 32)))));
					G = 1;
					continue;
				case 2:
					return lI2;
				default:
					break;
			}
		}
		return 0;
	}
	 double get_I_D(int p0) {
		throw new Exception("Missing body java.nio.DoubleBuffer.get\u0028I\u0029D");
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		int fI1 = 0;
		int fI0 = 0;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		java_lang_Object fA0 = null;
		double lD6 = 0.0;
		double lD8 = 0.0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_nio_DoubleBuffer)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					lA2 = ((p0) as java_nio_DoubleBuffer);
					if (((this.remaining__I() == (lA2 as java_nio_DoubleBuffer).remaining__I()))) {
						G = 2;
						continue;
					}
					return false;
				case 2:
					lI3 = this._position;
					lI4 = (lA2 as java_nio_DoubleBuffer)._position;
					lI5 = 1;
					G = 3;
					continue;
				case 3:
					if (((lI5 == 0))) {
						G = 4;
						continue;
					}
					if (((lI3 >= this._limit))) {
						G = 4;
						continue;
					}
					fA0 = this;
					fI1 = lI3;
					lI3 = (N.I(lI3 + 1));
					lD6 = (fA0 as java_nio_DoubleBuffer).get_I_D(fI1);
					fA0 = lA2;
					fI1 = lI4;
					lI4 = (N.I(lI4 + 1));
					lD8 = (fA0 as java_nio_DoubleBuffer).get_I_D(fI1);
					if ((((N.cmpl(lD6, lD8)) == 0))) {
						G = 5;
						continue;
					}
					if ((((N.cmpl(lD6, lD6)) == 0))) {
						G = 6;
						continue;
					}
					if ((((N.cmpl(lD8, lD8)) == 0))) {
						G = 6;
						continue;
					}
					G = 5;
					continue;
				case 5:
					fI0 = 1;
					G = 7;
					continue;
				case 6:
					fI0 = 0;
					G = 7;
					continue;
				case 7:
					lI5 = fI0;
					G = 3;
					continue;
				case 4:
					return ((lI5)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 java_nio_DoubleBuffer put_ID_Ljava_nio_DoubleBuffer_(int p0, double p1) {
		throw new Exception("Missing body java.nio.DoubleBuffer.put\u0028ID\u0029Ljava/nio/DoubleBuffer;");
	}
	java_nio_DoubleBuffer([int CLASS_ID = 869]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_nio_ByteBufferAsDoubleBuffer extends java_nio_DoubleBuffer implements java_nio_internal_ByteBufferAs {
	Float64List tarray;
	java_nio_ByteBuffer _byteBuffer = null;
	JA_B _bytes = null;
	 double get_I_D(int p0) {
		return this.tarray[p0];
	}
	static java_nio_DoubleBuffer asDoubleBuffer_Ljava_nio_ByteBuffer__Ljava_nio_DoubleBuffer_(java_nio_ByteBuffer p0) {
		java_nio_ByteBuffer lA1 = null;
		lA1 = p0.slice__Ljava_nio_ByteBuffer_();
		lA1.order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(p0.order__Ljava_nio_ByteOrder_());
		return java_nio_ByteBufferAsDoubleBuffer.create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsDoubleBuffer_(lA1, p0._isLittleEndian);
	}
	static java_nio_ByteBufferAsDoubleBuffer create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsDoubleBuffer_(java_nio_ByteBuffer p0, bool p1) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(p1)) {
						G = 1;
						continue;
					}
					tA0 = (new java_nio_ByteBufferAsDoubleBuffer_LE());
					fA0 = tA0;
					(tA0 as java_nio_ByteBufferAsDoubleBuffer_LE).java_nio_ByteBufferAsDoubleBuffer_LE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 1:
					tA1 = (new java_nio_ByteBufferAsDoubleBuffer_BE());
					fA0 = tA1;
					(tA1 as java_nio_ByteBufferAsDoubleBuffer_BE).java_nio_ByteBufferAsDoubleBuffer_BE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 2: return (fA0 as java_nio_ByteBufferAsDoubleBuffer);
				default:
					break;
			}
		}
		return null;
	}
	 java_nio_ByteBufferAsDoubleBuffer java_nio_ByteBufferAsDoubleBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsDoubleBuffer_1__V(java_nio_ByteBuffer p0, java_nio_ByteBufferAsDoubleBuffer_1 p1) {
		this.java_nio_ByteBufferAsDoubleBuffer_init__Ljava_nio_ByteBuffer__V(p0);
		return this;
		return this;
	}
	 java_nio_ByteBufferAsDoubleBuffer java_nio_ByteBufferAsDoubleBuffer_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_DoubleBuffer_init__I_V((N.I(p0.capacity__I() ~/ 8)));
		this._byteBuffer = p0;
		this._byteBuffer.clear__Ljava_nio_Buffer_();
		this._bytes = p0.array___B();
		this.init__B_V(p0.array___B());
		return this;
		return this;
	}
	 void init__B_V(JA_B p0) {
		this.tarray = new Float64List.view(p0.data.buffer);
	}
	 Int64 getLong_I_J(int p0) {
		return libcore_io_Memory.peekAlignedLongLE__BI_J(this._bytes, p0);
	}
	 java_nio_DoubleBuffer put_ID_Ljava_nio_DoubleBuffer_(int p0, double p1) {
		this.tarray[p0] = p1; return this;
	}
	 java_nio_DoubleBuffer putLong_IJ_Ljava_nio_DoubleBuffer_(int p0, Int64 p1) {
		libcore_io_Memory.pokeAlignedLongLE__BIJ_V(this._bytes, p0, p1);
		return this;
	}
	java_nio_ByteBufferAsDoubleBuffer([int CLASS_ID = 881]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsDoubleBuffer_BE extends java_nio_ByteBufferAsDoubleBuffer  {

	 java_nio_ByteBufferAsDoubleBuffer_BE java_nio_ByteBufferAsDoubleBuffer_BE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsDoubleBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsDoubleBuffer_1__V(p0, null);
		return this;
		return this;
	}
	 double get_I_D(int p0) {
		return java_lang_Double.longBitsToDouble_J_D(java_lang_Long.reverseBytes_J_J(this.getLong_I_J(p0)));
	}
	 Int64 getLong_I_J(int p0) {
		return super.getLong_I_J(p0);
	}
	 java_nio_DoubleBuffer put_ID_Ljava_nio_DoubleBuffer_(int p0, double p1) {
		return this.putLong_IJ_Ljava_nio_DoubleBuffer_(p0, com_jtransc_JTranscBits.reverseBytes_J_J(java_lang_Double.doubleToRawLongBits_D_J(p1)));
	}
	 java_nio_DoubleBuffer putLong_IJ_Ljava_nio_DoubleBuffer_(int p0, Int64 p1) {
		return super.putLong_IJ_Ljava_nio_DoubleBuffer_(p0, p1);
	}
	java_nio_ByteBufferAsDoubleBuffer_BE([int CLASS_ID = 884]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsDoubleBuffer_1 extends java_lang_Object  {

	java_nio_ByteBufferAsDoubleBuffer_1([int CLASS_ID = 883]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsDoubleBuffer_LE extends java_nio_ByteBufferAsDoubleBuffer  {

	 java_nio_ByteBufferAsDoubleBuffer_LE java_nio_ByteBufferAsDoubleBuffer_LE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsDoubleBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsDoubleBuffer_1__V(p0, null);
		return this;
		return this;
	}
	 double get_I_D(int p0) {
		return this.tarray[p0];
	}
	 Int64 getLong_I_J(int p0) {
		return super.getLong_I_J(p0);
	}
	 java_nio_DoubleBuffer put_ID_Ljava_nio_DoubleBuffer_(int p0, double p1) {
		this.tarray[p0] = p1; return this;
	}
	 java_nio_DoubleBuffer putLong_IJ_Ljava_nio_DoubleBuffer_(int p0, Int64 p1) {
		return super.putLong_IJ_Ljava_nio_DoubleBuffer_(p0, p1);
	}
	java_nio_ByteBufferAsDoubleBuffer_LE([int CLASS_ID = 882]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_nio_LongBuffer extends java_nio_Buffer implements java_lang_Comparable {

	 java_nio_LongBuffer java_nio_LongBuffer_init__I_V(int p0) {
		this.java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(3, p0, null);
		return this;
		return this;
	}
	 int hashCode__I() {
		int G = 0;
		int fI1 = 0;
		int lI1 = 0;
		int lI2 = 0;
		Int64 lJ3 = N.lnew(0);
		while (true) {
			switch (G) {
				case 0:
					lI1 = this._position;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= this._limit))) {
						G = 2;
						continue;
					}
					fI1 = lI1;
					lI1 = (N.I(lI1 + 1));
					lJ3 = this.get_I_J(fI1);
					lI2 = (N.I((N.I(lI2 + N.j2i(lJ3))) ^ N.j2i(((lJ3 >> 32)))));
					G = 1;
					continue;
				case 2:
					return lI2;
				default:
					break;
			}
		}
		return 0;
	}
	 Int64 get_I_J(int p0) {
		throw new Exception("Missing body java.nio.LongBuffer.get\u0028I\u0029J");
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		Int64 fJ0 = N.lnew(0);
		java_lang_Object lA2 = null;
		int fI1 = 0;
		int fI2 = 0;
		int fI0 = 0;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		java_lang_Object fA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_nio_LongBuffer)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					lA2 = ((p0) as java_nio_LongBuffer);
					if (((this.remaining__I() == (lA2 as java_nio_LongBuffer).remaining__I()))) {
						G = 2;
						continue;
					}
					return false;
				case 2:
					lI3 = this._position;
					lI4 = (lA2 as java_nio_LongBuffer)._position;
					lI5 = 1;
					G = 3;
					continue;
				case 3:
					if (((lI5 == 0))) {
						G = 4;
						continue;
					}
					if (((lI3 >= this._limit))) {
						G = 4;
						continue;
					}
					fI1 = lI3;
					lI3 = (N.I(lI3 + 1));
					fJ0 = this.get_I_J(fI1);
					fA1 = lA2;
					fI2 = lI4;
					lI4 = (N.I(lI4 + 1));
					if ((((N.lcmp(fJ0, (fA1 as java_nio_LongBuffer).get_I_J(fI2))) != 0))) {
						G = 5;
						continue;
					}
					fI0 = 1;
					G = 6;
					continue;
				case 5:
					fI0 = 0;
					G = 6;
					continue;
				case 6:
					lI5 = fI0;
					G = 3;
					continue;
				case 4:
					return ((lI5)!=0);
				default:
					break;
			}
		}
		return false;
	}
	java_nio_LongBuffer([int CLASS_ID = 871]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_nio_ByteBufferAsLongBuffer extends java_nio_LongBuffer implements java_nio_internal_ByteBufferAs {
	Int32List tarray;
	JA_B _bytes = null;
	java_nio_ByteBuffer _byteBuffer = null;
	 Int64 get_I_J(int p0) {
		return N.lnew(this.tarray[p0]);
	}
	static java_nio_LongBuffer asLongBuffer_Ljava_nio_ByteBuffer__Ljava_nio_LongBuffer_(java_nio_ByteBuffer p0) {
		java_nio_ByteBuffer lA1 = null;
		lA1 = p0.slice__Ljava_nio_ByteBuffer_();
		lA1.order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(p0.order__Ljava_nio_ByteOrder_());
		return java_nio_ByteBufferAsLongBuffer.create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsLongBuffer_(lA1, p0._isLittleEndian);
	}
	static java_nio_ByteBufferAsLongBuffer create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsLongBuffer_(java_nio_ByteBuffer p0, bool p1) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(p1)) {
						G = 1;
						continue;
					}
					tA0 = (new java_nio_ByteBufferAsLongBuffer_LE());
					fA0 = tA0;
					(tA0 as java_nio_ByteBufferAsLongBuffer_LE).java_nio_ByteBufferAsLongBuffer_LE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 1:
					tA1 = (new java_nio_ByteBufferAsLongBuffer_BE());
					fA0 = tA1;
					(tA1 as java_nio_ByteBufferAsLongBuffer_BE).java_nio_ByteBufferAsLongBuffer_BE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 2: return (fA0 as java_nio_ByteBufferAsLongBuffer);
				default:
					break;
			}
		}
		return null;
	}
	 java_nio_ByteBufferAsLongBuffer java_nio_ByteBufferAsLongBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsLongBuffer_1__V(java_nio_ByteBuffer p0, java_nio_ByteBufferAsLongBuffer_1 p1) {
		this.java_nio_ByteBufferAsLongBuffer_init__Ljava_nio_ByteBuffer__V(p0);
		return this;
		return this;
	}
	 java_nio_ByteBufferAsLongBuffer java_nio_ByteBufferAsLongBuffer_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_LongBuffer_init__I_V((N.I(p0.capacity__I() ~/ 8)));
		this._byteBuffer = p0;
		this._byteBuffer.clear__Ljava_nio_Buffer_();
		this._bytes = p0.array___B();
		this.init__B_V(p0.array___B());
		return this;
		return this;
	}
	 void init__B_V(JA_B p0) {
		this.tarray = new Int64List.view(p0.data.buffer);
	}
	java_nio_ByteBufferAsLongBuffer([int CLASS_ID = 877]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsLongBuffer_BE extends java_nio_ByteBufferAsLongBuffer  {

	 java_nio_ByteBufferAsLongBuffer_BE java_nio_ByteBufferAsLongBuffer_BE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsLongBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsLongBuffer_1__V(p0, null);
		return this;
		return this;
	}
	 Int64 get_I_J(int p0) {
		return com_jtransc_JTranscBits.reverseBytes_J_J(super.get_I_J(p0));
	}
	java_nio_ByteBufferAsLongBuffer_BE([int CLASS_ID = 880]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsLongBuffer_1 extends java_lang_Object  {

	java_nio_ByteBufferAsLongBuffer_1([int CLASS_ID = 879]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsLongBuffer_LE extends java_nio_ByteBufferAsLongBuffer  {

	 java_nio_ByteBufferAsLongBuffer_LE java_nio_ByteBufferAsLongBuffer_LE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsLongBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsLongBuffer_1__V(p0, null);
		return this;
		return this;
	}
	 Int64 get_I_J(int p0) {
		return N.lnew(this.tarray[p0]);
	}
	java_nio_ByteBufferAsLongBuffer_LE([int CLASS_ID = 878]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_nio_FloatBuffer extends java_nio_Buffer implements java_lang_Comparable {

	 java_nio_FloatBuffer java_nio_FloatBuffer_init__I_V(int p0) {
		this.java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(2, p0, null);
		return this;
		return this;
	}
	 int hashCode__I() {
		int G = 0;
		int fI0 = 0;
		int fI2 = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = this._position;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= this._limit))) {
						G = 2;
						continue;
					}
					fI0 = lI2;
					fI2 = lI1;
					lI1 = (N.I(lI1 + 1));
					lI2 = (N.I(fI0 + java_lang_Float.floatToIntBits_F_I(this.get_I_F(fI2))));
					G = 1;
					continue;
				case 2:
					return lI2;
				default:
					break;
			}
		}
		return 0;
	}
	 double get_I_F(int p0) {
		throw new Exception("Missing body java.nio.FloatBuffer.get\u0028I\u0029F");
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		double lF6 = 0.0;
		double lF7 = 0.0;
		java_lang_Object lA2 = null;
		int fI1 = 0;
		int fI0 = 0;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		java_lang_Object fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_nio_FloatBuffer)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					lA2 = ((p0) as java_nio_FloatBuffer);
					if (((this.remaining__I() == (lA2 as java_nio_FloatBuffer).remaining__I()))) {
						G = 2;
						continue;
					}
					return false;
				case 2:
					lI3 = this._position;
					lI4 = (lA2 as java_nio_FloatBuffer)._position;
					lI5 = 1;
					G = 3;
					continue;
				case 3:
					if (((lI5 == 0))) {
						G = 4;
						continue;
					}
					if (((lI3 >= this._limit))) {
						G = 4;
						continue;
					}
					fA0 = this;
					fI1 = lI3;
					lI3 = (N.I(lI3 + 1));
					lF6 = (fA0 as java_nio_FloatBuffer).get_I_F(fI1);
					fA0 = lA2;
					fI1 = lI4;
					lI4 = (N.I(lI4 + 1));
					lF7 = (fA0 as java_nio_FloatBuffer).get_I_F(fI1);
					if ((((N.cmpl(lF6, lF7)) == 0))) {
						G = 5;
						continue;
					}
					if ((((N.cmpl(lF6, lF6)) == 0))) {
						G = 6;
						continue;
					}
					if ((((N.cmpl(lF7, lF7)) == 0))) {
						G = 6;
						continue;
					}
					G = 5;
					continue;
				case 5:
					fI0 = 1;
					G = 7;
					continue;
				case 6:
					fI0 = 0;
					G = 7;
					continue;
				case 7:
					lI5 = fI0;
					G = 3;
					continue;
				case 4:
					return ((lI5)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 java_nio_FloatBuffer put_IF_Ljava_nio_FloatBuffer_(int p0, double p1) {
		throw new Exception("Missing body java.nio.FloatBuffer.put\u0028IF\u0029Ljava/nio/FloatBuffer;");
	}
	java_nio_FloatBuffer([int CLASS_ID = 864]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsFloatBuffer extends java_nio_FloatBuffer implements java_nio_internal_ByteBufferAs {
	Int32List iarray; Float32List farray;
	java_nio_ByteBuffer _byteBuffer = null;
	JA_B _bytes = null;
	 java_nio_ByteBufferAsFloatBuffer java_nio_ByteBufferAsFloatBuffer_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_FloatBuffer_init__I_V((N.I(p0.capacity__I() ~/ 4)));
		this._byteBuffer = p0;
		this._byteBuffer.clear__Ljava_nio_Buffer_();
		this._bytes = p0.array___B();
		this.init__B_V(p0.array___B());
		return this;
		return this;
	}
	 void init__B_V(JA_B p0) {
		this.iarray = new Int32List.view(p0.data.buffer); this.farray = new Float32List.view(p0.data.buffer);
	}
	 double get_I_F(int p0) {
		return this.farray[p0];
	}
	 java_nio_FloatBuffer put_IF_Ljava_nio_FloatBuffer_(int p0, double p1) {
		this.farray[p0] = p1; return this;
	}
	static java_nio_FloatBuffer asFloatBuffer_Ljava_nio_ByteBuffer__Ljava_nio_FloatBuffer_(java_nio_ByteBuffer p0) {
		java_nio_ByteBuffer lA1 = null;
		lA1 = p0.slice__Ljava_nio_ByteBuffer_();
		lA1.order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(p0.order__Ljava_nio_ByteOrder_());
		return java_nio_ByteBufferAsFloatBuffer.create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsFloatBuffer_(lA1, p0._isLittleEndian);
	}
	static java_nio_ByteBufferAsFloatBuffer create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsFloatBuffer_(java_nio_ByteBuffer p0, bool p1) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(p1)) {
						G = 1;
						continue;
					}
					tA0 = (new java_nio_ByteBufferAsFloatBuffer_LE());
					fA0 = tA0;
					(tA0 as java_nio_ByteBufferAsFloatBuffer_LE).java_nio_ByteBufferAsFloatBuffer_LE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 1:
					tA1 = (new java_nio_ByteBufferAsFloatBuffer_BE());
					fA0 = tA1;
					(tA1 as java_nio_ByteBufferAsFloatBuffer_BE).java_nio_ByteBufferAsFloatBuffer_BE_init__Ljava_nio_ByteBuffer__V(p0);
					G = 2;
					continue;
				case 2: return (fA0 as java_nio_ByteBufferAsFloatBuffer);
				default:
					break;
			}
		}
		return null;
	}
	 int getInt_I_I(int p0) {
		return this.iarray[p0];
	}
	 java_nio_FloatBuffer putInt_II_Ljava_nio_FloatBuffer_(int p0, int p1) {
		this.iarray[p0] = p1; return this;
	}
	java_nio_ByteBufferAsFloatBuffer([int CLASS_ID = 872]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsFloatBuffer_LE extends java_nio_ByteBufferAsFloatBuffer  {

	 java_nio_ByteBufferAsFloatBuffer_LE java_nio_ByteBufferAsFloatBuffer_LE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsFloatBuffer_init__Ljava_nio_ByteBuffer__V(p0);
		return this;
		return this;
	}
	 double get_I_F(int p0) {
		return this.farray[p0];
	}
	 java_nio_FloatBuffer put_IF_Ljava_nio_FloatBuffer_(int p0, double p1) {
		this.farray[p0] = p1; return this;
	}
	 int getInt_I_I(int p0) {
		return this.iarray[p0];
	}
	 java_nio_FloatBuffer putInt_II_Ljava_nio_FloatBuffer_(int p0, int p1) {
		this.iarray[p0] = p1; return this;
	}
	java_nio_ByteBufferAsFloatBuffer_LE([int CLASS_ID = 876]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBufferAsFloatBuffer_BE extends java_nio_ByteBufferAsFloatBuffer  {

	 java_nio_ByteBufferAsFloatBuffer_BE java_nio_ByteBufferAsFloatBuffer_BE_init__Ljava_nio_ByteBuffer__V(java_nio_ByteBuffer p0) {
		this.java_nio_ByteBufferAsFloatBuffer_init__Ljava_nio_ByteBuffer__V(p0);
		return this;
		return this;
	}
	 double get_I_F(int p0) {
		return java_lang_Float.intBitsToFloat_I_F(java_lang_Integer.reverseBytes_I_I(this.getInt_I_I(p0)));
	}
	 int getInt_I_I(int p0) {
		return this.iarray[p0];
	}
	 java_nio_FloatBuffer put_IF_Ljava_nio_FloatBuffer_(int p0, double p1) {
		return this.putInt_II_Ljava_nio_FloatBuffer_(p0, java_lang_Integer.reverseBytes_I_I(java_lang_Float.floatToRawIntBits_F_I(p1)));
	}
	 java_nio_FloatBuffer putInt_II_Ljava_nio_FloatBuffer_(int p0, int p1) {
		this.iarray[p0] = p1; return this;
	}
	java_nio_ByteBufferAsFloatBuffer_BE([int CLASS_ID = 875]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteOrder extends java_lang_Object  {

	java_lang_String _name = null;
	bool _needsSwap = false;
	static java_nio_ByteOrder _LITTLE_ENDIAN = null;
	static java_nio_ByteOrder _NATIVE_ORDER = null;
	static java_nio_ByteOrder _BIG_ENDIAN = null;
	static bool _isLittleEndian = false;
	 java_nio_ByteOrder java_nio_ByteOrder_init__Ljava_lang_String_Z_V(java_lang_String p0, bool p1) {
		this.java_lang_Object_init___V();
		this._name = p0;
		this._needsSwap = p1;
		return this;
		return this;
	}
	static void java_nio_ByteOrder_clinit___V() {
		int G = 0;
		int fI3 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_String fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					java_nio_ByteOrder._isLittleEndian = com_jtransc_JTranscBits.isLittleEndian__Z();
					tA0 = (new java_nio_ByteOrder());
					fA0 = tA0;
					(tA0 as java_nio_ByteOrder).java_nio_ByteOrder_init__Ljava_lang_String_Z_V(Bootstrap.STRINGLIT_36, java_nio_ByteOrder._isLittleEndian);
					java_nio_ByteOrder._BIG_ENDIAN = (fA0 as java_nio_ByteOrder);
					tA1 = (new java_nio_ByteOrder());
					fA0 = tA1;
					fA1 = tA1;
					fA2 = Bootstrap.STRINGLIT_37;
					if (java_nio_ByteOrder._isLittleEndian) {
						G = 1;
						continue;
					}
					fI3 = 1;
					G = 2;
					continue;
				case 1:
					fI3 = 0;
					G = 2;
					continue;
				case 2:
					(fA1 as java_nio_ByteOrder).java_nio_ByteOrder_init__Ljava_lang_String_Z_V(fA2, ((fI3)!=0));
					java_nio_ByteOrder._LITTLE_ENDIAN = (fA0 as java_nio_ByteOrder);
					if (!(java_nio_ByteOrder._isLittleEndian)) {
						G = 3;
						continue;
					}
					fA0 = java_nio_ByteOrder._LITTLE_ENDIAN;
					G = 4;
					continue;
				case 3:
					fA0 = java_nio_ByteOrder._BIG_ENDIAN;
					G = 4;
					continue;
				case 4:
					java_nio_ByteOrder._NATIVE_ORDER = (fA0 as java_nio_ByteOrder);
					return;
				default:
					break;
			}
		}
		return;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		return this._name;
	}
	static java_nio_ByteOrder nativeOrder__Ljava_nio_ByteOrder_() {
		return java_nio_ByteOrder._NATIVE_ORDER;
	}
	java_nio_ByteOrder([int CLASS_ID = 874]) : super(CLASS_ID) { }
	static void SI() {
		java_nio_ByteOrder._LITTLE_ENDIAN = null;
		java_nio_ByteOrder._NATIVE_ORDER = null;
		java_nio_ByteOrder._BIG_ENDIAN = null;
		java_nio_ByteOrder._isLittleEndian = false;
		java_nio_ByteOrder.java_nio_ByteOrder_clinit___V();
	}
}
class java_lang_UnsupportedOperationException extends java_lang_RuntimeException  {

	 java_lang_UnsupportedOperationException java_lang_UnsupportedOperationException_init___V() {
		this.java_lang_RuntimeException_init___V();
		return this;
		return this;
	}
	java_lang_UnsupportedOperationException([int CLASS_ID = 739]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ReadOnlyBufferException extends java_lang_UnsupportedOperationException  {

	 java_nio_ReadOnlyBufferException java_nio_ReadOnlyBufferException_init___V() {
		this.java_lang_UnsupportedOperationException_init___V();
		return this;
		return this;
	}
	java_nio_ReadOnlyBufferException([int CLASS_ID = 866]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_ByteBuffer extends java_nio_Buffer implements java_lang_Comparable {

	int _arrayOffset = 0;
	JA_B _backingArray = null;
	bool _isReadOnly = false;
	bool _isLittleEndian = false;
	bool _isNativeOrder = false;
	java_nio_ByteOrder _order = null;
	bool _isDirect = false;
	 int hashCode__I() {
		int G = 0;
		int fI0 = 0;
		int fI2 = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = this._position;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= this._limit))) {
						G = 2;
						continue;
					}
					fI0 = lI2;
					fI2 = lI1;
					lI1 = (N.I(lI1 + 1));
					lI2 = (N.I(fI0 + this.get_I_B(fI2)));
					G = 1;
					continue;
				case 2:
					return lI2;
				default:
					break;
			}
		}
		return 0;
	}
	 int get_I_B(int p0) {
		this.checkIndex_I_V(p0);
		return (this._backingArray).data[(N.I(this._arrayOffset + p0))];
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		int fI1 = 0;
		int fI0 = 0;
		int fI2 = 0;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		java_lang_Object fA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_nio_ByteBuffer)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					lA2 = ((p0) as java_nio_ByteBuffer);
					if (((this.remaining__I() == (lA2 as java_nio_ByteBuffer).remaining__I()))) {
						G = 2;
						continue;
					}
					return false;
				case 2:
					lI3 = this._position;
					lI4 = (lA2 as java_nio_ByteBuffer)._position;
					lI5 = 1;
					G = 3;
					continue;
				case 3:
					if (((lI5 == 0))) {
						G = 4;
						continue;
					}
					if (((lI3 >= this._limit))) {
						G = 4;
						continue;
					}
					fI1 = lI3;
					lI3 = (N.I(lI3 + 1));
					fI0 = (this.get_I_B(fI1));
					fA1 = lA2;
					fI2 = lI4;
					lI4 = (N.I(lI4 + 1));
					if (((fI0 != (fA1 as java_nio_ByteBuffer).get_I_B(fI2)))) {
						G = 5;
						continue;
					}
					fI0 = 1;
					G = 6;
					continue;
				case 5:
					fI0 = 0;
					G = 6;
					continue;
				case 6:
					lI5 = fI0;
					G = 3;
					continue;
				case 4:
					return ((lI5)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 JA_B array___B() {
		this._checkWritable__V();
		return this._backingArray;
	}
	 void _checkWritable__V() {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(this._isReadOnly)) {
						G = 1;
						continue;
					}
					tA0 = (new java_nio_ReadOnlyBufferException());
					fA0 = tA0;
					(tA0 as java_nio_ReadOnlyBufferException).java_nio_ReadOnlyBufferException_init___V();
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					return;
				default:
					break;
			}
		}
		return;
	}
	 java_nio_FloatBuffer asFloatBuffer__Ljava_nio_FloatBuffer_() {
		return java_nio_ByteBufferAsFloatBuffer.asFloatBuffer_Ljava_nio_ByteBuffer__Ljava_nio_FloatBuffer_(this);
	}
	 java_nio_ByteBuffer slice__Ljava_nio_ByteBuffer_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_nio_ByteBuffer());
		fA0 = tA0;
		(tA0 as java_nio_ByteBuffer).java_nio_ByteBuffer_init__I_BIZ_V(this.remaining__I(), this._backingArray, (N.I(this._arrayOffset + this._position)), this._isReadOnly);
		return (fA0 as java_nio_ByteBuffer);
	}
	 java_nio_ByteBuffer java_nio_ByteBuffer_init__I_BIZ_V(int p0, JA_B p1, int p2, bool p3) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		while (true) {
			switch (G) {
				case 0:
					this.java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(0, p0, null);
					this._backingArray = p1;
					this._arrayOffset = p2;
					this._isReadOnly = p3;
					this.order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder._BIG_ENDIAN);
					if ((((N.I(p2 + p0)) <= (p1 as JA_0).length))) {
						G = 1;
						continue;
					}
					tA1 = (new java_lang_IndexOutOfBoundsException());
					fA0 = tA1;
					fA1 = tA1;
					tA2 = (new java_lang_StringBuilder());
					fA2 = tA2;
					(tA2 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_IndexOutOfBoundsException).java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_40).append_I_Ljava_lang_StringBuilder_((p1 as JA_0).length).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_39).append_I_Ljava_lang_StringBuilder_(p0).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_38).append_I_Ljava_lang_StringBuilder_(p2).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 java_nio_ByteBuffer order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder p0) {
		int G = 0;
		java_lang_Object lA1 = null;
		int fI1 = 0;
		java_nio_ByteBuffer fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = p0;
					if (((lA1 != null))) {
						G = 1;
						continue;
					}
					lA1 = java_nio_ByteOrder._LITTLE_ENDIAN;
					G = 1;
					continue;
				case 1:
					this._order = (lA1 as java_nio_ByteOrder);
					fA0 = this;
					if (((lA1 != java_nio_ByteOrder.nativeOrder__Ljava_nio_ByteOrder_()))) {
						G = 2;
						continue;
					}
					fI1 = 1;
					G = 3;
					continue;
				case 2:
					fI1 = 0;
					G = 3;
					continue;
				case 3:
					fA0._isNativeOrder = ((fI1)!=0);
					fA0 = this;
					if (((lA1 != java_nio_ByteOrder._LITTLE_ENDIAN))) {
						G = 4;
						continue;
					}
					fI1 = 1;
					G = 5;
					continue;
				case 4:
					fI1 = 0;
					G = 5;
					continue;
				case 5:
					fA0._isLittleEndian = ((fI1)!=0);
					return this;
				default:
					break;
			}
		}
		return null;
	}
	 java_nio_ByteOrder order__Ljava_nio_ByteOrder_() {
		return this._order;
	}
	 java_nio_LongBuffer asLongBuffer__Ljava_nio_LongBuffer_() {
		return java_nio_ByteBufferAsLongBuffer.asLongBuffer_Ljava_nio_ByteBuffer__Ljava_nio_LongBuffer_(this);
	}
	 java_nio_DoubleBuffer asDoubleBuffer__Ljava_nio_DoubleBuffer_() {
		return java_nio_ByteBufferAsDoubleBuffer.asDoubleBuffer_Ljava_nio_ByteBuffer__Ljava_nio_DoubleBuffer_(this);
	}
	 java_nio_CharBuffer asCharBuffer__Ljava_nio_CharBuffer_() {
		return java_nio_ByteBufferAsCharBuffer.asCharBuffer_Ljava_nio_ByteBuffer__Ljava_nio_CharBuffer_(this);
	}
	 java_nio_ShortBuffer asShortBuffer__Ljava_nio_ShortBuffer_() {
		return java_nio_ByteBufferAsShortBuffer.asShortBuffer_Ljava_nio_ByteBuffer__Ljava_nio_ShortBuffer_(this);
	}
	 java_nio_IntBuffer asIntBuffer__Ljava_nio_IntBuffer_() {
		return java_nio_ByteBufferAsIntBuffer.asIntBuffer_Ljava_nio_ByteBuffer__Ljava_nio_IntBuffer_(this);
	}
	static java_nio_ByteBuffer allocateDirect_I_Ljava_nio_ByteBuffer_(int p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 >= 0))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_IllegalArgumentException());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_IllegalArgumentException).java_lang_IllegalArgumentException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_28).append_I_Ljava_lang_StringBuilder_(p0).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					tA2 = (new java_nio_ByteBuffer());
					fA0 = tA2;
					(tA2 as java_nio_ByteBuffer).java_nio_ByteBuffer_init___BZ_V(new JA_B(p0), true);
					return (fA0 as java_nio_ByteBuffer);
				default:
					break;
			}
		}
		return null;
	}
	 java_nio_ByteBuffer java_nio_ByteBuffer_init___BZ_V(JA_B p0, bool p1) {
		this.java_nio_ByteBuffer_init__I_BIZ_V((p0 as JA_0).length, p0, 0, false);
		this._isDirect = p1;
		return this;
		return this;
	}
	static java_nio_ByteBuffer allocate_I_Ljava_nio_ByteBuffer_(int p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 >= 0))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_IllegalArgumentException());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_IllegalArgumentException).java_lang_IllegalArgumentException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_28).append_I_Ljava_lang_StringBuilder_(p0).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					tA2 = (new java_nio_ByteBuffer());
					fA0 = tA2;
					(tA2 as java_nio_ByteBuffer).java_nio_ByteBuffer_init___B_V(new JA_B(p0));
					return (fA0 as java_nio_ByteBuffer);
				default:
					break;
			}
		}
		return null;
	}
	 java_nio_ByteBuffer java_nio_ByteBuffer_init___B_V(JA_B p0) {
		this.java_nio_ByteBuffer_init__I_BIZ_V((p0 as JA_0).length, p0, 0, false);
		this._isDirect = false;
		return this;
		return this;
	}
	java_nio_ByteBuffer([int CLASS_ID = 865]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_internal_MemoryBlock extends java_lang_Object  {

	 java_nio_internal_MemoryBlock java_nio_internal_MemoryBlock_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	java_nio_internal_MemoryBlock([int CLASS_ID = 863]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_FastMemory extends java_lang_Object  {

	java_nio_IntBuffer _dataInt = null;
	java_nio_FloatBuffer _dataFloat = null;
	java_nio_ByteBuffer _data = null;
	java_nio_ShortBuffer _dataShort = null;
	java_nio_DoubleBuffer _dataDouble = null;
	java_nio_CharBuffer _dataChar = null;
	java_nio_LongBuffer _dataLong = null;
	int _length = 0;
	 int getAlignedInt32_I_I(int p0) {
		return this._dataInt.get_I_I(p0);
	}
	 void setAlignedFloat32_IF_V(int p0, double p1) {
		this._dataFloat.put_IF_Ljava_nio_FloatBuffer_(p0, p1);
		return;
	}
	static com_jtransc_FastMemory alloc_I_Lcom_jtransc_FastMemory_(int p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new com_jtransc_FastMemory());
		fA0 = tA0;
		(tA0 as com_jtransc_FastMemory).com_jtransc_FastMemory_init__I_V(p0);
		return (fA0 as com_jtransc_FastMemory);
	}
	 com_jtransc_FastMemory com_jtransc_FastMemory_init__I_V(int p0) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					this.java_lang_Object_init___V();
					this._initWithSize_I_V(p0);
					this._createViews__V();
					if (!(com_jtransc_JTranscSystem.isCpp__Z())) {
						G = 1;
						continue;
					}
					this._createViewsExtra__B_V(this._data.array___B());
					G = 1;
					continue;
				case 1:
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 void _createViews__V() {
		this._dataChar = this._data.asCharBuffer__Ljava_nio_CharBuffer_();
		this._dataShort = this._data.asShortBuffer__Ljava_nio_ShortBuffer_();
		this._dataInt = this._data.asIntBuffer__Ljava_nio_IntBuffer_();
		this._dataLong = this._data.asLongBuffer__Ljava_nio_LongBuffer_();
		this._dataFloat = this._data.asFloatBuffer__Ljava_nio_FloatBuffer_();
		this._dataDouble = this._data.asDoubleBuffer__Ljava_nio_DoubleBuffer_();
		return;
	}
	 void _initWithSize_I_V(int p0) {
		this._length = p0;
		this._data = java_nio_ByteBuffer.allocateDirect_I_Ljava_nio_ByteBuffer_((N.I((N.I(p0 + 15)) & -16))).order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder.nativeOrder__Ljava_nio_ByteOrder_());
		return;
	}
	 void _createViewsExtra__B_V(JA_B p0) {
		return;
	}
	com_jtransc_FastMemory([int CLASS_ID = 860]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_MyClass extends java_lang_Object  {

	int _b = 0;
	java_lang_String _d = null;
	java_lang_String _c = null;
	int _a = 0;
	 Benchmark_MyClass Benchmark_MyClass_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_Object_init___V();
		this._a = 10;
		this._b = 20;
		this._c = Bootstrap.STRINGLIT_41;
		this._d = p0;
		return this;
		return this;
	}
	Benchmark_MyClass([int CLASS_ID = 859]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_JTranscSystemProperties extends java_lang_Object  {

	 com_jtransc_JTranscSystemProperties com_jtransc_JTranscSystemProperties_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static java_lang_String lineSeparator__Ljava_lang_String_() {
		return Bootstrap.STRINGLIT_42;
	}
	static java_lang_String userHome__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = new JA_L(1, "[Ljava.lang.String;");
		fA0 = tA0;
		(tA0 as JA_L).data[0] = Bootstrap.STRINGLIT_43;
		return com_jtransc_JTranscSystemProperties.getenvs__Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_((fA0 as JA_L), Bootstrap.STRINGLIT_44);
	}
	static java_lang_String getenvs__Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_(JA_L p0, java_lang_String p1) {
		int G = 0;
		java_lang_Object lA5 = null;
		java_lang_Object lA6 = null;
		int lI3 = 0;
		int lI4 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI3 = (p0 as JA_0).length;
					lI4 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI4 >= lI3))) {
						G = 2;
						continue;
					}
					lA5 = ((p0 as JA_L)).data[lI4];
					lA6 = java_lang_System.getenv_Ljava_lang_String__Ljava_lang_String_((lA5 as java_lang_String));
					if (((lA6 == null))) {
						G = 3;
						continue;
					}
					return (lA6 as java_lang_String);
				case 3:
					lI4 = (N.I(lI4 + 1));
					G = 1;
					continue;
				case 2:
					return p1;
				default:
					break;
			}
		}
		return null;
	}
	static java_lang_String userLanguage__Ljava_lang_String_() {
		return Bootstrap.STRINGLIT_45;
	}
	static java_lang_String tmpdir__Ljava_lang_String_() {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_String lA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA3 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = new JA_L(3, "[Ljava.lang.String;");
					fA0 = tA0;
					(tA0 as JA_L).data[0] = Bootstrap.STRINGLIT_46;
					(fA0 as JA_L).setArraySlice(1, [ Bootstrap.STRINGLIT_47, Bootstrap.STRINGLIT_48 ]);
					lA0 = com_jtransc_JTranscSystemProperties.getenvs__Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_((fA0 as JA_L), Bootstrap.STRINGLIT_44);
					if (!(com_jtransc_JTranscSystem.isWindows__Z())) {
						G = 1;
						continue;
					}
					if (!(lA0.endsWith_Ljava_lang_String__Z(Bootstrap.STRINGLIT_49))) {
						G = 2;
						continue;
					}
					if (lA0.endsWith_Ljava_lang_String__Z(Bootstrap.STRINGLIT_50)) {
						G = 1;
						continue;
					}
					G = 2;
					continue;
				case 2:
					tA3 = (new java_lang_StringBuilder());
					fA0 = tA3;
					(tA3 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA0 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(lA0).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_50).toString__Ljava_lang_String_();
					G = 1;
					continue;
				case 1:
					return lA0;
				default:
					break;
			}
		}
		return null;
	}
	static java_lang_String fileEncoding__Ljava_lang_String_() {
		return Bootstrap.STRINGLIT_51;
	}
	static java_lang_String pathSeparator__Ljava_lang_String_() {
		return Bootstrap.STRINGLIT_52;
	}
	static java_lang_String userDir__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = new JA_L(1, "[Ljava.lang.String;");
		fA0 = tA0;
		(tA0 as JA_L).data[0] = Bootstrap.STRINGLIT_43;
		return com_jtransc_JTranscSystemProperties.getenvs__Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_((fA0 as JA_L), Bootstrap.STRINGLIT_44);
	}
	static java_lang_String userName__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = new JA_L(2, "[Ljava.lang.String;");
		fA0 = tA0;
		(tA0 as JA_L).data[0] = Bootstrap.STRINGLIT_53;
		(fA0 as JA_L).data[1] = Bootstrap.STRINGLIT_54;
		return com_jtransc_JTranscSystemProperties.getenvs__Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_((fA0 as JA_L), Bootstrap.STRINGLIT_55);
	}
	static java_lang_String userVariant__Ljava_lang_String_() {
		return Bootstrap.STRINGLIT_20;
	}
	static java_lang_String fileSeparator__Ljava_lang_String_() {
		return Bootstrap.STRINGLIT_49;
	}
	static java_lang_String userRegion__Ljava_lang_String_() {
		return Bootstrap.STRINGLIT_56;
	}
	com_jtransc_JTranscSystemProperties([int CLASS_ID = 858]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_JTranscArrays extends java_lang_Object  {

	static JA_B _EMPTY_BYTE = null;
	static JA_L _EMPTY_CLASS = null;
	 com_jtransc_JTranscArrays com_jtransc_JTranscArrays_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void com_jtransc_JTranscArrays_clinit___V() {
		com_jtransc_JTranscArrays._EMPTY_BYTE = new JA_B(0);
		com_jtransc_JTranscArrays._EMPTY_CLASS = new JA_L(0, "[Ljava.lang.Class;");
		return;
	}
	static void checkOffsetAndCount_III_V(int p0, int p1, int p2) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if ((((N.I(p1 | p2)) < 0))) {
						G = 1;
						continue;
					}
					if (((p1 > p0))) {
						G = 1;
						continue;
					}
					if ((((N.I(p0 - p1)) >= p2))) {
						G = 2;
						continue;
					}
					G = 1;
					continue;
				case 1:
					tA0 = (new java_lang_ArrayIndexOutOfBoundsException());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_ArrayIndexOutOfBoundsException).java_lang_ArrayIndexOutOfBoundsException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_59).append_I_Ljava_lang_StringBuilder_(p0).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_58).append_I_Ljava_lang_StringBuilder_(p1).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_57).append_I_Ljava_lang_StringBuilder_(p2).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	com_jtransc_JTranscArrays([int CLASS_ID = 857]) : super(CLASS_ID) { }
	static void SI() {
		com_jtransc_JTranscArrays._EMPTY_BYTE = null;
		com_jtransc_JTranscArrays._EMPTY_CLASS = null;
		com_jtransc_JTranscArrays.com_jtransc_JTranscArrays_clinit___V();
	}
}
abstract class java_lang_ClassLoader extends java_lang_Object  {

	java_util_ArrayList _nativeLibs = null;
	java_lang_ClassLoader _parent = null;
	 java_lang_ClassLoader java_lang_ClassLoader_init___V() {
		this.java_lang_ClassLoader_init__Ljava_lang_ClassLoader__V(null);
		return this;
		return this;
	}
	 java_lang_ClassLoader java_lang_ClassLoader_init__Ljava_lang_ClassLoader__V(java_lang_ClassLoader p0) {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		this.java_lang_Object_init___V();
		tA0 = (new java_util_ArrayList());
		fA1 = tA0;
		(tA0 as java_util_ArrayList).java_util_ArrayList_init___V();
		this._nativeLibs = (fA1 as java_util_ArrayList);
		this._parent = p0;
		return this;
		return this;
	}
	static java_lang_ClassLoader getSystemClassLoader__Ljava_lang_ClassLoader_() {
		return java_lang__ClassInternalUtils.getSystemClassLoader__Ljava_lang_ClassLoader_();
	}
	java_lang_ClassLoader([int CLASS_ID = 854]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang__ClassInternalUtils_1 extends java_lang_ClassLoader  {

	 java_lang__ClassInternalUtils_1 java_lang__ClassInternalUtils_1_init___V() {
		this.java_lang_ClassLoader_init___V();
		return this;
		return this;
	}
	java_lang__ClassInternalUtils_1([int CLASS_ID = 856]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang__ClassInternalUtils extends java_lang_Object  {

	static java_lang_ClassLoader _classLoader = null;
	 java_lang__ClassInternalUtils java_lang__ClassInternalUtils_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static java_lang_ClassLoader getSystemClassLoader__Ljava_lang_ClassLoader_() {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((java_lang__ClassInternalUtils._classLoader != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang__ClassInternalUtils_1());
					fA0 = tA0;
					(tA0 as java_lang__ClassInternalUtils_1).java_lang__ClassInternalUtils_1_init___V();
					java_lang__ClassInternalUtils._classLoader = (fA0 as java_lang_ClassLoader);
					G = 1;
					continue;
				case 1:
					return java_lang__ClassInternalUtils._classLoader;
				default:
					break;
			}
		}
		return null;
	}
	java_lang__ClassInternalUtils([int CLASS_ID = 855]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_Thread_UncaughtExceptionHandler   {

}
class java_lang_Thread_UncaughtExceptionHandler_IFields {

	static void SI() { }
}
class java_lang_ThreadGroup extends java_lang_Object implements java_lang_Thread_UncaughtExceptionHandler {

	 java_lang_String toString__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getClass__Ljava_lang_Class_().getName__Ljava_lang_String_()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_61).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getName__Ljava_lang_String_()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_60).append_I_Ljava_lang_StringBuilder_(this.getMaxPriority__I()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_29).toString__Ljava_lang_String_();
	}
	 int getMaxPriority__I() {
		throw new Exception("Missing body java.lang.ThreadGroup.getMaxPriority\u0028\u0029I");
	}
	 java_lang_String getName__Ljava_lang_String_() {
		throw new Exception("Missing body java.lang.ThreadGroup.getName\u0028\u0029Ljava/lang/String;");
	}
	java_lang_ThreadGroup([int CLASS_ID = 852]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_Runnable   {

}
class java_lang_Runnable_IFields {

	static void SI() { }
}
class java_lang_Thread extends java_lang_Object implements java_lang_Runnable {

	java_lang_String _name = null;
	java_lang_ThreadGroup _group = null;
	static java_lang_Thread __currentThread = null;
	java_lang_ClassLoader _classLoader = null;
	 java_lang_String toString__Ljava_lang_String_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = this.getThreadGroup__Ljava_lang_ThreadGroup_();
					if (((lA1 == null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_63).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getName__Ljava_lang_String_()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_62).append_I_Ljava_lang_StringBuilder_(this.getPriority__I()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_62).append_Ljava_lang_String__Ljava_lang_StringBuilder_((lA1 as java_lang_ThreadGroup).getName__Ljava_lang_String_()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_29).toString__Ljava_lang_String_();
				case 1:
					tA1 = (new java_lang_StringBuilder());
					fA0 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_63).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getName__Ljava_lang_String_()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_62).append_I_Ljava_lang_StringBuilder_(this.getPriority__I()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_64).toString__Ljava_lang_String_();
				default:
					break;
			}
		}
		return null;
	}
	 java_lang_String getName__Ljava_lang_String_() {
		return this._name;
	}
	 int getPriority__I() {
		return 5;
	}
	 java_lang_ThreadGroup getThreadGroup__Ljava_lang_ThreadGroup_() {
		return this._group;
	}
	static java_lang_Thread currentThread__Ljava_lang_Thread_() {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((java_lang_Thread.__currentThread != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_Thread());
					fA0 = tA0;
					(tA0 as java_lang_Thread).java_lang_Thread_init___V();
					java_lang_Thread.__currentThread = (fA0 as java_lang_Thread);
					G = 1;
					continue;
				case 1:
					return java_lang_Thread.__currentThread;
				default:
					break;
			}
		}
		return null;
	}
	 java_lang_Thread java_lang_Thread_init___V() {
		this.java_lang_Object_init___V();
		this._classLoader = null;
		return this;
		return this;
	}
	 java_lang_ClassLoader getContextClassLoader__Ljava_lang_ClassLoader_() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._classLoader != null))) {
						G = 1;
						continue;
					}
					this._classLoader = java_lang__ClassInternalUtils.getSystemClassLoader__Ljava_lang_ClassLoader_();
					G = 1;
					continue;
				case 1:
					return this._classLoader;
				default:
					break;
			}
		}
		return null;
	}
	java_lang_Thread([int CLASS_ID = 850]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class com_jtransc_charset_JTranscCharset extends java_lang_Object  {

	int _max = 0;
	int _min = 0;
	double _avg = 0.0;
	JA_L _names = null;
	static com_jtransc_ds_FastStringMap _charsets = null;
	static bool _loadedCharsets = false;
	 com_jtransc_charset_JTranscCharset com_jtransc_charset_JTranscCharset_init___Ljava_lang_String_IFI_V(JA_L p0, int p1, double p2, int p3) {
		this.java_lang_Object_init___V();
		this._names = p0;
		this._min = p1;
		this._avg = p2;
		this._max = p3;
		return this;
		return this;
	}
	static void com_jtransc_charset_JTranscCharset_clinit___V() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		com_jtransc_charset_JTranscCharset._loadedCharsets = false;
		tA0 = (new com_jtransc_ds_FastStringMap());
		fA0 = tA0;
		(tA0 as com_jtransc_ds_FastStringMap).com_jtransc_ds_FastStringMap_init___V();
		com_jtransc_charset_JTranscCharset._charsets = (fA0 as com_jtransc_ds_FastStringMap);
		return;
	}
	 JA_B encode_Ljava_lang_String___B(java_lang_String p0) {
		java_lang_Object lA2 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_io_ByteArrayOutputStream());
		fA0 = tA0;
		(tA0 as java_io_ByteArrayOutputStream).java_io_ByteArrayOutputStream_init__I_V((N.f2i(((N.i2f(p0.length__I()) * this.avgBytesPerCharacter__F())))));
		lA2 = fA0;
		this.encode__CIILjava_io_ByteArrayOutputStream__V(p0.toCharArray___C(), 0, p0.length__I(), (lA2 as java_io_ByteArrayOutputStream));
		return (lA2 as java_io_ByteArrayOutputStream).toByteArray___B();
	}
	 JA_C decodeChars__BII__C(JA_B p0, int p1, int p2) {
		return this.decode__BII_Ljava_lang_String_(p0, p1, p2).toCharArray___C();
	}
	 java_lang_String decode__BII_Ljava_lang_String_(JA_B p0, int p1, int p2) {
		java_lang_Object lA4 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init__I_V((N.f2i(((N.i2f((p0 as JA_0).length) / this.avgBytesPerCharacter__F())))));
		lA4 = fA0;
		this.decode__BIILjava_lang_StringBuilder__V(p0, p1, p2, (lA4 as java_lang_StringBuilder));
		return (lA4 as java_lang_StringBuilder).toString__Ljava_lang_String_();
	}
	 void decode__BIILjava_lang_StringBuilder__V(JA_B p0, int p1, int p2, java_lang_StringBuilder p3) {
		throw new Exception("Missing body com.jtransc.charset.JTranscCharset.decode\u0028[BIILjava/lang/StringBuilder;\u0029V");
	}
	 double avgBytesPerCharacter__F() {
		return this._avg;
	}
	static com_jtransc_charset_JTranscCharset forName_Ljava_lang_String__Lcom_jtransc_charset_JTranscCharset_(java_lang_String p0) {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					com_jtransc_charset_JTranscCharset.ensureRegister__V();
					lA1 = ((com_jtransc_charset_JTranscCharset._charsets.get_Ljava_lang_String__Ljava_lang_Object_(p0.toUpperCase__Ljava_lang_String_().trim__Ljava_lang_String_())) as com_jtransc_charset_JTranscCharset);
					if (((lA1 != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_nio_charset_UnsupportedCharsetException());
					fA0 = tA0;
					(tA0 as java_nio_charset_UnsupportedCharsetException).java_nio_charset_UnsupportedCharsetException_init__Ljava_lang_String__V(p0);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					return (lA1 as com_jtransc_charset_JTranscCharset);
				default:
					break;
			}
		}
		return null;
	}
	static void ensureRegister__V() {
		int G = 0;
		com_jtransc_charset_JTranscCharset lA1 = null;
		java_util_Iterator lA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(com_jtransc_charset_JTranscCharset._loadedCharsets)) {
						G = 1;
						continue;
					}
					return;
					G = 1;
					continue;
				case 1:
					com_jtransc_charset_JTranscCharset._loadedCharsets = true;
					lA0 = java_util_ServiceLoader.load_Ljava_lang_Class__Ljava_util_ServiceLoader_(N.resolveClass("Lcom/jtransc/charset/JTranscCharset;")).iterator__Ljava_util_Iterator_();
					G = 2;
					continue;
				case 2:
					if (!(lA0.hasNext__Z())) {
						G = 3;
						continue;
					}
					lA1 = ((lA0.next__Ljava_lang_Object_()) as com_jtransc_charset_JTranscCharset);
					com_jtransc_charset_JTranscCharset.registerCharset_Lcom_jtransc_charset_JTranscCharset__V(lA1);
					G = 2;
					continue;
				case 3:
					return;
				default:
					break;
			}
		}
		return;
	}
	static void registerCharset_Lcom_jtransc_charset_JTranscCharset__V(com_jtransc_charset_JTranscCharset p0) {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA4 = null;
		int lI2 = 0;
		int lI3 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((com_jtransc_charset_JTranscCharset._charsets != null))) {
						G = 1;
						continue;
					}
					tA0 = (new com_jtransc_ds_FastStringMap());
					fA0 = tA0;
					(tA0 as com_jtransc_ds_FastStringMap).com_jtransc_ds_FastStringMap_init___V();
					com_jtransc_charset_JTranscCharset._charsets = (fA0 as com_jtransc_ds_FastStringMap);
					G = 1;
					continue;
				case 1:
					lA1 = p0.getAliases___Ljava_lang_String_();
					lI2 = (lA1 as JA_0).length;
					lI3 = 0;
					G = 2;
					continue;
				case 2:
					if (((lI3 >= lI2))) {
						G = 3;
						continue;
					}
					lA4 = ((lA1 as JA_L)).data[lI3];
					com_jtransc_charset_JTranscCharset._charsets.set_Ljava_lang_String_Ljava_lang_Object__V((lA4 as java_lang_String).toUpperCase__Ljava_lang_String_().trim__Ljava_lang_String_(), p0);
					lI3 = (N.I(lI3 + 1));
					G = 2;
					continue;
				case 3:
					return;
				default:
					break;
			}
		}
		return;
	}
	 JA_L getAliases___Ljava_lang_String_() {
		return this._names;
	}
	 void encode__CIILjava_io_ByteArrayOutputStream__V(JA_C p0, int p1, int p2, java_io_ByteArrayOutputStream p3) {
		throw new Exception("Missing body com.jtransc.charset.JTranscCharset.encode\u0028[CIILjava/io/ByteArrayOutputStream;\u0029V");
	}
	com_jtransc_charset_JTranscCharset([int CLASS_ID = 826]) : super(CLASS_ID) { }
	static void SI() {
		com_jtransc_charset_JTranscCharset._charsets = null;
		com_jtransc_charset_JTranscCharset._loadedCharsets = false;
		com_jtransc_charset_JTranscCharset.com_jtransc_charset_JTranscCharset_clinit___V();
	}
}
abstract class com_jtransc_charset_charsets_JTranscCharsetUTF16Base extends com_jtransc_charset_JTranscCharset  {

	bool _littleEndian = false;
	 com_jtransc_charset_charsets_JTranscCharsetUTF16Base com_jtransc_charset_charsets_JTranscCharsetUTF16Base_init___Ljava_lang_String_Z_V(JA_L p0, bool p1) {
		this.com_jtransc_charset_JTranscCharset_init___Ljava_lang_String_IFI_V(p0, 2, 2.0, 2);
		this._littleEndian = p1;
		return this;
		return this;
	}
	 void decode__BIILjava_lang_StringBuilder__V(JA_B p0, int p1, int p2, java_lang_StringBuilder p3) {
		int G = 0;
		int lI5 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI5 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI5 >= p2))) {
						G = 2;
						continue;
					}
					p3.append_C_Ljava_lang_StringBuilder_(N.i2c(com_jtransc_JTranscBits.readInt16__BIZ_S(p0, (N.I(p1 + lI5)), this._littleEndian)));
					lI5 = (N.I(lI5 + 2));
					G = 1;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void encode__CIILjava_io_ByteArrayOutputStream__V(JA_C p0, int p1, int p2, java_io_ByteArrayOutputStream p3) {
		int G = 0;
		int lI5 = 0;
		int lI6 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI5 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI5 >= p2))) {
						G = 2;
						continue;
					}
					lI6 = ((p0).data[(N.I(p1 + lI5))]);
					if (!(this._littleEndian)) {
						G = 3;
						continue;
					}
					p3.write_I_V((N.I((N.iushr_opt(lI6, 0)) & 255)));
					p3.write_I_V((N.I((N.iushr_opt(lI6, 8)) & 255)));
					G = 4;
					continue;
				case 3:
					p3.write_I_V((N.I((N.iushr_opt(lI6, 8)) & 255)));
					p3.write_I_V((N.I((N.iushr_opt(lI6, 0)) & 255)));
					G = 4;
					continue;
				case 4:
					lI5 = (N.I(lI5 + 1));
					G = 1;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	com_jtransc_charset_charsets_JTranscCharsetUTF16Base([int CLASS_ID = 842]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_charset_charsets_JTranscCharsetUTF16BE extends com_jtransc_charset_charsets_JTranscCharsetUTF16Base  {

	 com_jtransc_charset_charsets_JTranscCharsetUTF16BE com_jtransc_charset_charsets_JTranscCharsetUTF16BE_init___V() {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		tA0 = new JA_L(4, "[Ljava.lang.String;");
		fA1 = tA0;
		(tA0 as JA_L).data[0] = Bootstrap.STRINGLIT_65;
		(fA1 as JA_L).setArraySlice(1, [ Bootstrap.STRINGLIT_66, Bootstrap.STRINGLIT_67, Bootstrap.STRINGLIT_68 ]);
		this.com_jtransc_charset_charsets_JTranscCharsetUTF16Base_init___Ljava_lang_String_Z_V((fA1 as JA_L), false);
		return this;
		return this;
	}
	 void decode__BIILjava_lang_StringBuilder__V(JA_B p0, int p1, int p2, java_lang_StringBuilder p3) {
		super.decode__BIILjava_lang_StringBuilder__V(p0, p1, p2, p3);
		return;
	}
	 void encode__CIILjava_io_ByteArrayOutputStream__V(JA_C p0, int p1, int p2, java_io_ByteArrayOutputStream p3) {
		super.encode__CIILjava_io_ByteArrayOutputStream__V(p0, p1, p2, p3);
		return;
	}
	com_jtransc_charset_charsets_JTranscCharsetUTF16BE([int CLASS_ID = 849]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_charset_JTranscCharsetSingleByte extends com_jtransc_charset_JTranscCharset  {

	int _invalidChar = 0;
	java_lang_String _decode = null;
	java_util_Map _encode = null;
	 com_jtransc_charset_JTranscCharsetSingleByte com_jtransc_charset_JTranscCharsetSingleByte_init___Ljava_lang_String_Ljava_lang_String__V(JA_L p0, java_lang_String p1) {
		int G = 0;
		int lI3 = 0;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					this.com_jtransc_charset_JTranscCharset_init___Ljava_lang_String_IFI_V(p0, 1, 1.0, 1);
					this._invalidChar = 63;
					this._decode = p1;
					tA0 = (new java_util_HashMap());
					fA1 = tA0;
					(tA0 as java_util_HashMap).java_util_HashMap_init__I_V(p1.length__I());
					this._encode = (fA1 as java_util_Map);
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= p1.length__I()))) {
						G = 2;
						continue;
					}
					this._encode.put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Character.valueOf_C_Ljava_lang_Character_(p1.charAt_I_C(lI3)), java_lang_Byte.valueOf_B_Ljava_lang_Byte_(N.i2b(lI3)));
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 void decode__BIILjava_lang_StringBuilder__V(JA_B p0, int p1, int p2, java_lang_StringBuilder p3) {
		int G = 0;
		int lI5 = 0;
		int lI6 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI5 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI5 >= p2))) {
						G = 2;
						continue;
					}
					lI6 = (N.I((p0).data[(N.I(p1 + lI5))] & 255));
					p3.append_C_Ljava_lang_StringBuilder_(this._decode.charAt_I_C(lI6));
					lI5 = (N.I(lI5 + 1));
					G = 1;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void encode__CIILjava_io_ByteArrayOutputStream__V(JA_C p0, int p1, int p2, java_io_ByteArrayOutputStream p3) {
		int G = 0;
		java_lang_Object lA7 = null;
		int lI5 = 0;
		int lI6 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI5 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI5 >= p2))) {
						G = 2;
						continue;
					}
					lI6 = ((p0).data[(N.I(p1 + lI5))]);
					lA7 = ((this._encode.get_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Character.valueOf_C_Ljava_lang_Character_(N.i2c(lI6)))) as java_lang_Byte);
					if (((lA7 == null))) {
						G = 3;
						continue;
					}
					p3.write_I_V(((lA7 as java_lang_Byte).byteValue__B()));
					G = 4;
					continue;
				case 3:
					p3.write_I_V(63);
					G = 4;
					continue;
				case 4:
					lI5 = (N.I(lI5 + 1));
					G = 1;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	com_jtransc_charset_JTranscCharsetSingleByte([int CLASS_ID = 831]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_charset_charsets_JTranscCharsetLatin1 extends com_jtransc_charset_JTranscCharsetSingleByte  {

	 com_jtransc_charset_charsets_JTranscCharsetLatin1 com_jtransc_charset_charsets_JTranscCharsetLatin1_init___V() {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		tA0 = new JA_L(15, "[Ljava.lang.String;");
		fA1 = tA0;
		(tA0 as JA_L).data[0] = Bootstrap.STRINGLIT_69;
		(fA1 as JA_L).setArraySlice(1, [ Bootstrap.STRINGLIT_70, Bootstrap.STRINGLIT_71, Bootstrap.STRINGLIT_72, Bootstrap.STRINGLIT_73, Bootstrap.STRINGLIT_74, Bootstrap.STRINGLIT_75, Bootstrap.STRINGLIT_76, Bootstrap.STRINGLIT_77, Bootstrap.STRINGLIT_78, Bootstrap.STRINGLIT_79, Bootstrap.STRINGLIT_80, Bootstrap.STRINGLIT_81, Bootstrap.STRINGLIT_82, Bootstrap.STRINGLIT_83 ]);
		this.com_jtransc_charset_JTranscCharsetSingleByte_init___Ljava_lang_String_Ljava_lang_String__V((fA1 as JA_L), Bootstrap.STRINGLIT_84);
		return this;
		return this;
	}
	com_jtransc_charset_charsets_JTranscCharsetLatin1([int CLASS_ID = 848]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_mix_JTranscProcessMulti_Creator extends java_lang_Object  {

	 com_jtransc_mix_JTranscProcessMulti_Creator com_jtransc_mix_JTranscProcessMulti_Creator_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	com_jtransc_mix_JTranscProcessMulti_Creator([int CLASS_ID = 847]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_Process extends java_lang_Object  {

	 java_lang_Process java_lang_Process_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	java_lang_Process([int CLASS_ID = 846]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class com_jtransc_JTranscProcess extends java_lang_Process  {

	 com_jtransc_JTranscProcess com_jtransc_JTranscProcess_init___V() {
		this.java_lang_Process_init___V();
		return this;
		return this;
	}
	com_jtransc_JTranscProcess([int CLASS_ID = 845]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_mix_JTranscProcessMulti extends com_jtransc_JTranscProcess  {

	static com_jtransc_mix_JTranscProcessMulti_Creator _creator = null;
	 com_jtransc_mix_JTranscProcessMulti com_jtransc_mix_JTranscProcessMulti_init___V() {
		this.com_jtransc_JTranscProcess_init___V();
		return this;
		return this;
	}
	static void com_jtransc_mix_JTranscProcessMulti_clinit___V() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new com_jtransc_mix_JTranscProcessMulti_Creator());
		fA0 = tA0;
		(tA0 as com_jtransc_mix_JTranscProcessMulti_Creator).com_jtransc_mix_JTranscProcessMulti_Creator_init___V();
		com_jtransc_mix_JTranscProcessMulti._creator = (fA0 as com_jtransc_mix_JTranscProcessMulti_Creator);
		return;
	}
	com_jtransc_mix_JTranscProcessMulti([int CLASS_ID = 844]) : super(CLASS_ID) { }
	static void SI() {
		com_jtransc_mix_JTranscProcessMulti._creator = null;
		com_jtransc_mix_JTranscProcessMulti.com_jtransc_mix_JTranscProcessMulti_clinit___V();
	}
}
class com_jtransc_JTranscBits extends java_lang_Object  {

	 com_jtransc_JTranscBits com_jtransc_JTranscBits_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static int readInt16__BIZ_S(JA_B p0, int p1, bool p2) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(p2)) {
						G = 1;
						continue;
					}
					fI0 = (com_jtransc_JTranscBits.readInt16LE__BI_S(p0, p1));
					G = 2;
					continue;
				case 1:
					fI0 = (com_jtransc_JTranscBits.readInt16BE__BI_S(p0, p1));
					G = 2;
					continue;
				case 2: return N.i2s(fI0);
				default:
					break;
			}
		}
		return 0;
	}
	static int readInt16BE__BI_S(JA_B p0, int p1) {
		return N.i2s((N.I((N.I((N.I((p0).data[(N.I(p1 + 0))] & 255)) << 8)) | (N.I((N.I((p0).data[(N.I(p1 + 1))] & 255)) << 0)))));
	}
	static int readInt16LE__BI_S(JA_B p0, int p1) {
		return N.i2s((N.I((N.I((N.I((p0).data[(N.I(p1 + 1))] & 255)) << 8)) | (N.I((N.I((p0).data[(N.I(p1 + 0))] & 255)) << 0)))));
	}
	static bool isLittleEndian__Z() {
		return true;
	}
	static Int64 reverseBytes_J_J(Int64 p0) {
		return java_lang_Long.reverseBytes_J_J(p0);
	}
	static void writeLongLE__BIJ_V(JA_B p0, int p1, Int64 p2) {
		int lI4 = 0;
		int lI5 = 0;
		lI4 = N.j2i(((p2 >> 32)));
		lI5 = N.j2i(((p2 >> 0)));
		p0.data[(N.I(p1 + 7))] = N.i2b((N.iushr_opt(lI4, 24)));
		p0.data[(N.I(p1 + 6))] = N.i2b((N.iushr_opt(lI4, 16)));
		p0.data[(N.I(p1 + 5))] = N.i2b((N.iushr_opt(lI4, 8)));
		p0.data[(N.I(p1 + 4))] = N.i2b((N.iushr_opt(lI4, 0)));
		p0.data[(N.I(p1 + 3))] = N.i2b((N.iushr_opt(lI5, 24)));
		p0.data[(N.I(p1 + 2))] = N.i2b((N.iushr_opt(lI5, 16)));
		p0.data[(N.I(p1 + 1))] = N.i2b((N.iushr_opt(lI5, 8)));
		p0.data[(N.I(p1 + 0))] = N.i2b((N.iushr_opt(lI5, 0)));
		return;
	}
	com_jtransc_JTranscBits([int CLASS_ID = 843]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_charset_charsets_JTranscCharsetUTF16LE extends com_jtransc_charset_charsets_JTranscCharsetUTF16Base  {

	 com_jtransc_charset_charsets_JTranscCharsetUTF16LE com_jtransc_charset_charsets_JTranscCharsetUTF16LE_init___V() {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		tA0 = new JA_L(4, "[Ljava.lang.String;");
		fA1 = tA0;
		(tA0 as JA_L).data[0] = Bootstrap.STRINGLIT_85;
		(fA1 as JA_L).setArraySlice(1, [ Bootstrap.STRINGLIT_86, Bootstrap.STRINGLIT_87, Bootstrap.STRINGLIT_88 ]);
		this.com_jtransc_charset_charsets_JTranscCharsetUTF16Base_init___Ljava_lang_String_Z_V((fA1 as JA_L), false);
		return this;
		return this;
	}
	 void decode__BIILjava_lang_StringBuilder__V(JA_B p0, int p1, int p2, java_lang_StringBuilder p3) {
		super.decode__BIILjava_lang_StringBuilder__V(p0, p1, p2, p3);
		return;
	}
	 void encode__CIILjava_io_ByteArrayOutputStream__V(JA_C p0, int p1, int p2, java_io_ByteArrayOutputStream p3) {
		super.encode__CIILjava_io_ByteArrayOutputStream__V(p0, p1, p2, p3);
		return;
	}
	com_jtransc_charset_charsets_JTranscCharsetUTF16LE([int CLASS_ID = 841]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_charset_charsets_JTranscCharsetUSASCII extends com_jtransc_charset_JTranscCharsetSingleByte  {

	 com_jtransc_charset_charsets_JTranscCharsetUSASCII com_jtransc_charset_charsets_JTranscCharsetUSASCII_init___V() {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		tA0 = new JA_L(15, "[Ljava.lang.String;");
		fA1 = tA0;
		(tA0 as JA_L).data[0] = Bootstrap.STRINGLIT_89;
		(fA1 as JA_L).setArraySlice(1, [ Bootstrap.STRINGLIT_90, Bootstrap.STRINGLIT_91, Bootstrap.STRINGLIT_92, Bootstrap.STRINGLIT_93, Bootstrap.STRINGLIT_94, Bootstrap.STRINGLIT_95, Bootstrap.STRINGLIT_96, Bootstrap.STRINGLIT_97, Bootstrap.STRINGLIT_98, Bootstrap.STRINGLIT_99, Bootstrap.STRINGLIT_100, Bootstrap.STRINGLIT_101, Bootstrap.STRINGLIT_102, Bootstrap.STRINGLIT_56 ]);
		this.com_jtransc_charset_JTranscCharsetSingleByte_init___Ljava_lang_String_Ljava_lang_String__V((fA1 as JA_L), Bootstrap.STRINGLIT_103);
		return this;
		return this;
	}
	com_jtransc_charset_charsets_JTranscCharsetUSASCII([int CLASS_ID = 840]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_charset_charsets_JTranscCharsetUTF8 extends com_jtransc_charset_JTranscCharset  {

	 com_jtransc_charset_charsets_JTranscCharsetUTF8 com_jtransc_charset_charsets_JTranscCharsetUTF8_init___V() {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		tA0 = new JA_L(2, "[Ljava.lang.String;");
		fA1 = tA0;
		(tA0 as JA_L).data[0] = Bootstrap.STRINGLIT_51;
		(fA1 as JA_L).data[1] = Bootstrap.STRINGLIT_104;
		this.com_jtransc_charset_JTranscCharset_init___Ljava_lang_String_IFI_V((fA1 as JA_L), 1, 1.2, 4);
		return this;
		return this;
	}
	 void decode__BIILjava_lang_StringBuilder__V(JA_B p0, int p1, int p2, java_lang_StringBuilder p3) {
		int G = 0;
		JA_B fA2 = null;
		int fI1 = 0;
		int fI3 = 0;
		int lI5 = 0;
		int lI6 = 0;
		int lI7 = 0;
		java_lang_Object fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lI5 = p1;
					lI6 = (N.I(p1 + p2));
					G = 1;
					continue;
				case 1:
					if (((lI5 >= lI6))) {
						G = 2;
						continue;
					}
					fA0 = p0;
					fI1 = lI5;
					lI5 = (N.I(lI5 + 1));
					lI7 = (N.I(((fA0 as JA_B)).data[fI1] & 255));
					switch ((N.I(lI7 >> 4))) {
						case 0:
							G = 4;
							continue;
						case 1:
							G = 4;
							continue;
						case 2:
							G = 4;
							continue;
						case 3:
							G = 4;
							continue;
						case 4:
							G = 4;
							continue;
						case 5:
							G = 4;
							continue;
						case 6:
							G = 4;
							continue;
						case 7:
							G = 4;
							continue;
						case 8:
							G = 3;
							continue;
						case 9:
							G = 3;
							continue;
						case 10:
							G = 3;
							continue;
						case 11:
							G = 3;
							continue;
						case 12:
							G = 5;
							continue;
						case 13:
							G = 5;
							continue;
						case 14:
							G = 6;
							continue;
						default:
							G = 3;
							continue;
					}
					G = 4;
					continue;
				case 4:
					p3.append_C_Ljava_lang_StringBuilder_(N.i2c(lI7));
					G = 3;
					continue;
				case 5:
					fA0 = p3;
					fI1 = (N.I((N.I(lI7 & 31)) << 6));
					fA2 = p0;
					fI3 = lI5;
					lI5 = (N.I(lI5 + 1));
					(fA0 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(N.i2c((N.I(fI1 | (N.I((fA2).data[fI3] & 63))))));
					G = 3;
					continue;
				case 6:
					fA0 = p3;
					fI1 = (N.I((N.I(lI7 & 15)) << 12));
					fA2 = p0;
					fI3 = lI5;
					lI5 = (N.I(lI5 + 1));
					fI1 = (N.I(fI1 | (N.I((N.I((fA2).data[fI3] & 63)) << 6))));
					fA2 = p0;
					fI3 = lI5;
					lI5 = (N.I(lI5 + 1));
					(fA0 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(N.i2c((N.I(fI1 | (N.I((N.I((fA2).data[fI3] & 63)) << 0))))));
					G = 3;
					continue;
				case 3:
					G = 1;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void encode__CIILjava_io_ByteArrayOutputStream__V(JA_C p0, int p1, int p2, java_io_ByteArrayOutputStream p3) {
		int G = 0;
		int lI5 = 0;
		int lI6 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI5 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI5 >= p2))) {
						G = 2;
						continue;
					}
					lI6 = ((p0).data[(N.I(p1 + lI5))]);
					if ((((N.I(lI6 & -128)) != 0))) {
						G = 3;
						continue;
					}
					p3.write_I_V(lI6);
					G = 4;
					continue;
				case 3:
					if ((((N.I(lI6 & -2048)) != 0))) {
						G = 5;
						continue;
					}
					p3.write_I_V((N.I((N.I((N.I(lI6 >> 6)) & 31)) | 192)));
					G = 6;
					continue;
				case 5:
					if ((((N.I(lI6 & -65536)) != 0))) {
						G = 7;
						continue;
					}
					p3.write_I_V((N.I((N.I((N.I(lI6 >> 12)) & 15)) | 224)));
					p3.write_I_V(com_jtransc_charset_charsets_JTranscCharsetUTF8.createByte_II_I(lI6, 6));
					G = 6;
					continue;
				case 7:
					if ((((N.I(lI6 & -2097152)) != 0))) {
						G = 6;
						continue;
					}
					p3.write_I_V((N.I((N.I((N.I(lI6 >> 18)) & 7)) | 240)));
					p3.write_I_V(com_jtransc_charset_charsets_JTranscCharsetUTF8.createByte_II_I(lI6, 12));
					p3.write_I_V(com_jtransc_charset_charsets_JTranscCharsetUTF8.createByte_II_I(lI6, 6));
					G = 6;
					continue;
				case 6:
					p3.write_I_V((N.I((N.I(lI6 & 63)) | 128)));
					G = 4;
					continue;
				case 4:
					lI5 = (N.I(lI5 + 1));
					G = 1;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	static int createByte_II_I(int p0, int p1) {
		return (N.I((N.I((N.ishr(p0, p1)) & 63)) | 128));
	}
	com_jtransc_charset_charsets_JTranscCharsetUTF8([int CLASS_ID = 839]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_Collections_EmptySet extends java_util_AbstractSet implements java_io_Serializable {

	 int size__I() {
		return 0;
	}
	 java_util_Iterator iterator__Ljava_util_Iterator_() {
		return java_util_Collections.access_000__Ljava_util_Iterator_();
	}
	 bool contains_Ljava_lang_Object__Z(java_lang_Object p0) {
		return false;
	}
	 java_util_Collections_EmptySet java_util_Collections_EmptySet_init__Ljava_util_Collections_1__V(java_util_Collections_1 p0) {
		this.java_util_Collections_EmptySet_init___V();
		return this;
		return this;
	}
	 java_util_Collections_EmptySet java_util_Collections_EmptySet_init___V() {
		this.java_util_AbstractSet_init___V();
		return this;
		return this;
	}
	java_util_Collections_EmptySet([int CLASS_ID = 838]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_AbstractMap extends java_lang_Object implements java_util_Map {

	java_util_Collection _valuesCollection = null;
	java_util_Set _keySet = null;
	 java_util_AbstractMap java_util_AbstractMap_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA4 = null;
		java_lang_Object lA5 = null;
		java_util_Map_Entry lA3 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(this.isEmpty__Z())) {
						G = 1;
						continue;
					}
					return Bootstrap.STRINGLIT_105;
				case 1:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init__I_V((N.I(this.size__I() * 28)));
					lA1 = fA0;
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(123);
					lA2 = this.entrySet__Ljava_util_Set_().iterator__Ljava_util_Iterator_();
					G = 2;
					continue;
				case 2:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					lA3 = ((lA2.next__Ljava_lang_Object_()) as java_util_Map_Entry);
					lA4 = lA3.getKey__Ljava_lang_Object_();
					if (((lA4 == this))) {
						G = 4;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_Object__Ljava_lang_StringBuilder_(lA4);
					G = 5;
					continue;
				case 4:
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_10);
					G = 5;
					continue;
				case 5:
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(61);
					lA5 = lA3.getValue__Ljava_lang_Object_();
					if (((lA5 == this))) {
						G = 6;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_Object__Ljava_lang_StringBuilder_(lA5);
					G = 7;
					continue;
				case 6:
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_10);
					G = 7;
					continue;
				case 7:
					if (!(lA2.hasNext__Z())) {
						G = 8;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_6);
					G = 8;
					continue;
				case 8:
					G = 2;
					continue;
				case 3:
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(125);
					return (lA1 as java_lang_StringBuilder).toString__Ljava_lang_String_();
				default:
					break;
			}
		}
		return null;
	}
	 java_util_Set entrySet__Ljava_util_Set_() {
		throw new Exception("Missing body java.util.AbstractMap.entrySet\u0028\u0029Ljava/util/Set;");
	}
	 bool isEmpty__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this.size__I() != 0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 int size__I() {
		return this.entrySet__Ljava_util_Set_().size__I();
	}
	 int hashCode__I() {
		int G = 0;
		int lI1 = 0;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					lA2 = this.entrySet__Ljava_util_Set_().iterator__Ljava_util_Iterator_();
					G = 1;
					continue;
				case 1:
					if (!(lA2.hasNext__Z())) {
						G = 2;
						continue;
					}
					lI1 = (N.I(lI1 + ((lA2.next__Ljava_lang_Object_()) as java_util_Map_Entry).hashCode__I()));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA3 = null;
		java_lang_Object lA5 = null;
		java_lang_Object lA6 = null;
		java_lang_Object lA7 = null;
		int fI0 = 0;
		java_util_Map_Entry lA4 = null;
		java_lang_Object fA0 = null;
		java_util_Map lA2 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							if (((this != p0))) {
								G = 1;
								continue;
							}
							return true;
						case 1:
							if (!(((p0) is java_util_Map))) {
								G = 2;
								continue;
							}
							lA2 = ((p0) as java_util_Map);
							if (((this.size__I() == lA2.size__I()))) {
								G = 3;
								continue;
							}
							return false;
						case 3:
							lA3 = this.entrySet__Ljava_util_Set_().iterator__Ljava_util_Iterator_();
							G = 4;
							continue;
						case 4:
							if (!((lA3 as java_util_Iterator).hasNext__Z())) {
								G = 5;
								continue;
							}
							lA4 = (((lA3 as java_util_Iterator).next__Ljava_lang_Object_()) as java_util_Map_Entry);
							lA5 = lA4.getKey__Ljava_lang_Object_();
							lA6 = lA4.getValue__Ljava_lang_Object_();
							lA7 = lA2.get_Ljava_lang_Object__Ljava_lang_Object_(lA5);
							if (((lA6 != null))) {
								G = 6;
								continue;
							}
							if (((lA7 != null))) {
								G = 7;
								continue;
							}
							if (lA2.containsKey_Ljava_lang_Object__Z(lA5)) {
								G = 8;
								continue;
							}
							G = 7;
							continue;
						case 7:
							fI0 = 0;
							G = 9;
							continue;
						case 9: return ((fI0)!=0);
						case 6:
							if (lA6.equals_Ljava_lang_Object__Z(lA7)) {
								G = 8;
								continue;
							}
							fI0 = 0;
							G = 10;
							continue;
						case 10: return ((fI0)!=0);
						case 8:
							G = 4;
							continue;
						case 5:
							G = 11;
							continue;
						case 12:
							fA0 = J__exception__;
							lA3 = fA0;
							return false;
						case 13:
							fA0 = J__exception__;
							lA3 = fA0;
							return false;
						case 11:
							return true;
						case 2:
							return false;
						default:
							break;
					}
				}
				return false;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 3)) && ((G < 9)))) && ((J__exception__) is java_lang_NullPointerException)))) {
					G = 12;
					continue;
				}
				if (((((((G >= 6)) && ((G < 10)))) && ((J__exception__) is java_lang_NullPointerException)))) {
					G = 12;
					continue;
				}
				if (((((((G >= 8)) && ((G < 5)))) && ((J__exception__) is java_lang_NullPointerException)))) {
					G = 12;
					continue;
				}
				if (((((((G >= 3)) && ((G < 9)))) && ((J__exception__) is java_lang_ClassCastException)))) {
					G = 13;
					continue;
				}
				if (((((((G >= 6)) && ((G < 10)))) && ((J__exception__) is java_lang_ClassCastException)))) {
					G = 13;
					continue;
				}
				if (((((((G >= 8)) && ((G < 5)))) && ((J__exception__) is java_lang_ClassCastException)))) {
					G = 13;
					continue;
				}
				rethrow;
			}
		}
		return false;
	}
	 java_lang_Object get_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0) {
		int G = 0;
		java_util_Map_Entry lA3 = null;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lA2 = this.entrySet__Ljava_util_Set_().iterator__Ljava_util_Iterator_();
					if (((p0 == null))) {
						G = 1;
						continue;
					}
					G = 2;
					continue;
				case 2:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					lA3 = ((lA2.next__Ljava_lang_Object_()) as java_util_Map_Entry);
					if (!(p0.equals_Ljava_lang_Object__Z(lA3.getKey__Ljava_lang_Object_()))) {
						G = 4;
						continue;
					}
					return lA3.getValue__Ljava_lang_Object_();
				case 4:
					G = 2;
					continue;
				case 1:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					lA3 = ((lA2.next__Ljava_lang_Object_()) as java_util_Map_Entry);
					if (((lA3.getKey__Ljava_lang_Object_() != null))) {
						G = 5;
						continue;
					}
					return lA3.getValue__Ljava_lang_Object_();
				case 5:
					G = 1;
					continue;
				case 3:
					return null;
				default:
					break;
			}
		}
		return null;
	}
	 bool containsKey_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lA2 = this.entrySet__Ljava_util_Set_().iterator__Ljava_util_Iterator_();
					if (((p0 == null))) {
						G = 1;
						continue;
					}
					G = 2;
					continue;
				case 2:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z(((lA2.next__Ljava_lang_Object_()) as java_util_Map_Entry).getKey__Ljava_lang_Object_()))) {
						G = 2;
						continue;
					}
					return true;
				case 1:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					if (((((lA2.next__Ljava_lang_Object_()) as java_util_Map_Entry).getKey__Ljava_lang_Object_() != null))) {
						G = 1;
						continue;
					}
					return true;
				case 3:
					return false;
				default:
					break;
			}
		}
		return false;
	}
	 java_lang_Object put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0, java_lang_Object p1) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_UnsupportedOperationException());
		fA0 = tA0;
		(tA0 as java_lang_UnsupportedOperationException).java_lang_UnsupportedOperationException_init___V();
		throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
	}
	 java_lang_Object clone__Ljava_lang_Object_() {
		java_lang_Object lA1 = null;
		lA1 = ((super.clone__Ljava_lang_Object_()) as java_util_AbstractMap);
		(lA1 as java_util_AbstractMap)._keySet = null;
		(lA1 as java_util_AbstractMap)._valuesCollection = null;
		return lA1;
	}
	java_util_AbstractMap([int CLASS_ID = 710]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_Collections_EmptyMap extends java_util_AbstractMap implements java_io_Serializable {

	 java_util_Set entrySet__Ljava_util_Set_() {
		return java_util_Collections._EMPTY_SET;
	}
	 java_lang_Object get_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0) {
		return null;
	}
	 bool containsKey_Ljava_lang_Object__Z(java_lang_Object p0) {
		return false;
	}
	 java_util_Collections_EmptyMap java_util_Collections_EmptyMap_init__Ljava_util_Collections_1__V(java_util_Collections_1 p0) {
		this.java_util_Collections_EmptyMap_init___V();
		return this;
		return this;
	}
	 java_util_Collections_EmptyMap java_util_Collections_EmptyMap_init___V() {
		this.java_util_AbstractMap_init___V();
		return this;
		return this;
	}
	java_util_Collections_EmptyMap([int CLASS_ID = 837]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_Enumeration   {

}
class java_util_Enumeration_IFields {

	static void SI() { }
}
class java_util_Collections_2 extends java_lang_Object implements java_util_Enumeration {

	 java_util_Collections_2 java_util_Collections_2_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	java_util_Collections_2([int CLASS_ID = 835]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_List  implements java_util_Collection {

	 int size__I();
	 bool isEmpty__Z();
	 java_util_Iterator iterator__Ljava_util_Iterator_();
	 int hashCode__I();
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0);
	 bool containsAll_Ljava_util_Collection__Z(java_util_Collection p0);
	 bool contains_Ljava_lang_Object__Z(java_lang_Object p0);
	 java_lang_Object get_I_Ljava_lang_Object_(int p0);
	 java_util_ListIterator listIterator_I_Ljava_util_ListIterator_(int p0);
	 java_util_ListIterator listIterator__Ljava_util_ListIterator_();
	 int indexOf_Ljava_lang_Object__I(java_lang_Object p0);
	 void add_ILjava_lang_Object__V(int p0, java_lang_Object p1);
	 bool add_Ljava_lang_Object__Z(java_lang_Object p0);
	 JA_L toArray__Ljava_lang_Object___Ljava_lang_Object_(JA_L p0);
}
class java_util_List_IFields {

	static void SI() { }
}
abstract class java_util_AbstractList extends java_util_AbstractCollection implements java_util_List {

	int _modCount = 0;
	 java_util_AbstractList java_util_AbstractList_init___V() {
		this.java_util_AbstractCollection_init___V();
		return this;
		return this;
	}
	 int hashCode__I() {
		int G = 0;
		java_lang_Object lA3 = null;
		int fI0 = 0;
		int fI1 = 0;
		int lI1 = 0;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 1;
					lA2 = this.iterator__Ljava_util_Iterator_();
					G = 1;
					continue;
				case 1:
					if (!(lA2.hasNext__Z())) {
						G = 2;
						continue;
					}
					lA3 = lA2.next__Ljava_lang_Object_();
					fI0 = (N.I(31 * lI1));
					if (((lA3 != null))) {
						G = 3;
						continue;
					}
					fI1 = 0;
					G = 4;
					continue;
				case 3:
					fI1 = lA3.hashCode__I();
					G = 4;
					continue;
				case 4:
					lI1 = (N.I(fI0 + fI1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA5 = null;
		java_lang_Object lA6 = null;
		java_util_List lA2 = null;
		java_util_Iterator lA3 = null;
		java_util_Iterator lA4 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((this != p0))) {
						G = 1;
						continue;
					}
					return true;
				case 1:
					if (!(((p0) is java_util_List))) {
						G = 2;
						continue;
					}
					lA2 = ((p0) as java_util_List);
					if (((lA2.size__I() == this.size__I()))) {
						G = 3;
						continue;
					}
					return false;
				case 3:
					lA3 = this.iterator__Ljava_util_Iterator_();
					lA4 = lA2.iterator__Ljava_util_Iterator_();
					G = 4;
					continue;
				case 4:
					if (!(lA3.hasNext__Z())) {
						G = 5;
						continue;
					}
					lA5 = lA3.next__Ljava_lang_Object_();
					lA6 = lA4.next__Ljava_lang_Object_();
					if (((lA5 != null))) {
						G = 6;
						continue;
					}
					if (((lA6 != null))) {
						G = 7;
						continue;
					}
					G = 8;
					continue;
				case 6:
					if (lA5.equals_Ljava_lang_Object__Z(lA6)) {
						G = 8;
						continue;
					}
					G = 7;
					continue;
				case 7:
					return false;
				case 8:
					G = 4;
					continue;
				case 5:
					return true;
				case 2:
					return false;
				default:
					break;
			}
		}
		return false;
	}
	 int indexOf_Ljava_lang_Object__I(java_lang_Object p0) {
		int G = 0;
		java_util_ListIterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lA2 = this.listIterator__Ljava_util_ListIterator_();
					if (((p0 == null))) {
						G = 1;
						continue;
					}
					G = 2;
					continue;
				case 2:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z(lA2.next__Ljava_lang_Object_()))) {
						G = 2;
						continue;
					}
					return lA2.previousIndex__I();
				case 1:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					if (((lA2.next__Ljava_lang_Object_() != null))) {
						G = 1;
						continue;
					}
					return lA2.previousIndex__I();
				case 3:
					return -1;
				default:
					break;
			}
		}
		return 0;
	}
	 java_util_ListIterator listIterator__Ljava_util_ListIterator_() {
		return this.listIterator_I_Ljava_util_ListIterator_(0);
	}
	 java_util_ListIterator listIterator_I_Ljava_util_ListIterator_(int p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_util_AbstractList_FullListIterator());
		fA0 = tA0;
		(tA0 as java_util_AbstractList_FullListIterator).java_util_AbstractList_FullListIterator_init__Ljava_util_AbstractList_I_V(this, p0);
		return (fA0 as java_util_ListIterator);
	}
	 java_lang_Object get_I_Ljava_lang_Object_(int p0) {
		throw new Exception("Missing body java.util.AbstractList.get\u0028I\u0029Ljava/lang/Object;");
	}
	 java_util_Iterator iterator__Ljava_util_Iterator_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_util_AbstractList_SimpleListIterator());
		fA0 = tA0;
		(tA0 as java_util_AbstractList_SimpleListIterator).java_util_AbstractList_SimpleListIterator_init__Ljava_util_AbstractList__V(this);
		return (fA0 as java_util_Iterator);
	}
	 bool add_Ljava_lang_Object__Z(java_lang_Object p0) {
		this.add_ILjava_lang_Object__V(this.size__I(), p0);
		return true;
	}
	 void add_ILjava_lang_Object__V(int p0, java_lang_Object p1) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_UnsupportedOperationException());
		fA0 = tA0;
		(tA0 as java_lang_UnsupportedOperationException).java_lang_UnsupportedOperationException_init___V();
		throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
	}
	java_util_AbstractList([int CLASS_ID = 757]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_RandomAccess   {

}
class java_util_RandomAccess_IFields {

	static void SI() { }
}
class java_util_Collections_EmptyList extends java_util_AbstractList implements java_util_RandomAccess, java_io_Serializable {

	 java_lang_Object get_I_Ljava_lang_Object_(int p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_IndexOutOfBoundsException());
		fA0 = tA0;
		(tA0 as java_lang_IndexOutOfBoundsException).java_lang_IndexOutOfBoundsException_init___V();
		throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
	}
	 int size__I() {
		return 0;
	}
	 bool contains_Ljava_lang_Object__Z(java_lang_Object p0) {
		return false;
	}
	 java_util_Collections_EmptyList java_util_Collections_EmptyList_init__Ljava_util_Collections_1__V(java_util_Collections_1 p0) {
		this.java_util_Collections_EmptyList_init___V();
		return this;
		return this;
	}
	 java_util_Collections_EmptyList java_util_Collections_EmptyList_init___V() {
		this.java_util_AbstractList_init___V();
		return this;
		return this;
	}
	java_util_Collections_EmptyList([int CLASS_ID = 834]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_Collections_1 extends java_lang_Object implements java_util_Iterator {

	 java_util_Collections_1 java_util_Collections_1_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 java_lang_Object next__Ljava_lang_Object_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_util_NoSuchElementException());
		fA0 = tA0;
		(tA0 as java_util_NoSuchElementException).java_util_NoSuchElementException_init___V();
		throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
	}
	 bool hasNext__Z() {
		return false;
	}
	java_util_Collections_1([int CLASS_ID = 833]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_Collections extends java_lang_Object  {

	static java_util_Set _EMPTY_SET = null;
	static java_util_Iterator _EMPTY_ITERATOR = null;
	static java_util_Enumeration _EMPTY_ENUMERATION = null;
	static java_util_Map _EMPTY_MAP = null;
	static java_util_List _EMPTY_LIST = null;
	 java_util_Collections java_util_Collections_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void java_util_Collections_clinit___V() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		java_lang_Object tA3 = null;
		java_lang_Object tA4 = null;
		tA0 = (new java_util_Collections_1());
		fA0 = tA0;
		(tA0 as java_util_Collections_1).java_util_Collections_1_init___V();
		java_util_Collections._EMPTY_ITERATOR = (fA0 as java_util_Iterator);
		tA1 = (new java_util_Collections_2());
		fA0 = tA1;
		(tA1 as java_util_Collections_2).java_util_Collections_2_init___V();
		java_util_Collections._EMPTY_ENUMERATION = (fA0 as java_util_Enumeration);
		tA2 = (new java_util_Collections_EmptyList());
		fA0 = tA2;
		(tA2 as java_util_Collections_EmptyList).java_util_Collections_EmptyList_init__Ljava_util_Collections_1__V(null);
		java_util_Collections._EMPTY_LIST = (fA0 as java_util_List);
		tA3 = (new java_util_Collections_EmptySet());
		fA0 = tA3;
		(tA3 as java_util_Collections_EmptySet).java_util_Collections_EmptySet_init__Ljava_util_Collections_1__V(null);
		java_util_Collections._EMPTY_SET = (fA0 as java_util_Set);
		tA4 = (new java_util_Collections_EmptyMap());
		fA0 = tA4;
		(tA4 as java_util_Collections_EmptyMap).java_util_Collections_EmptyMap_init__Ljava_util_Collections_1__V(null);
		java_util_Collections._EMPTY_MAP = (fA0 as java_util_Map);
		return;
	}
	static java_util_Iterator access_000__Ljava_util_Iterator_() {
		return java_util_Collections._EMPTY_ITERATOR;
	}
	static int roundUpToPowerOfTwo_I_I(int p0) {
		int lI0 = 0;
		lI0 = p0;
		lI0 = (N.I(lI0 + -1));
		lI0 = (N.I(lI0 | (N.iushr_opt(lI0, 1))));
		lI0 = (N.I(lI0 | (N.iushr_opt(lI0, 2))));
		lI0 = (N.I(lI0 | (N.iushr_opt(lI0, 4))));
		lI0 = (N.I(lI0 | (N.iushr_opt(lI0, 8))));
		lI0 = (N.I(lI0 | (N.iushr_opt(lI0, 16))));
		return (N.I(lI0 + 1));
	}
	static int secondaryHash_Ljava_lang_Object__I(java_lang_Object p0) {
		return java_util_Collections.secondaryHash_I_I(p0.hashCode__I());
	}
	static int secondaryHash_I_I(int p0) {
		int lI0 = 0;
		lI0 = p0;
		lI0 = (N.I(lI0 + (N.I((N.I(lI0 << 15)) ^ -12931))));
		lI0 = (N.I(lI0 ^ (N.iushr_opt(lI0, 10))));
		lI0 = (N.I(lI0 + (N.I(lI0 << 3))));
		lI0 = (N.I(lI0 ^ (N.iushr_opt(lI0, 6))));
		lI0 = (N.I(lI0 + (N.I((N.I(lI0 << 2)) + (N.I(lI0 << 14))))));
		return (N.I(lI0 ^ (N.iushr_opt(lI0, 16))));
	}
	java_util_Collections([int CLASS_ID = 832]) : super(CLASS_ID) { }
	static void SI() {
		java_util_Collections._EMPTY_SET = null;
		java_util_Collections._EMPTY_ITERATOR = null;
		java_util_Collections._EMPTY_ENUMERATION = null;
		java_util_Collections._EMPTY_MAP = null;
		java_util_Collections._EMPTY_LIST = null;
		java_util_Collections.java_util_Collections_clinit___V();
	}
}
class com_jtransc_charset_charsets_JTranscCharsetIBM866 extends com_jtransc_charset_JTranscCharsetSingleByte  {

	 com_jtransc_charset_charsets_JTranscCharsetIBM866 com_jtransc_charset_charsets_JTranscCharsetIBM866_init___V() {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		tA0 = new JA_L(5, "[Ljava.lang.String;");
		fA1 = tA0;
		(tA0 as JA_L).data[0] = Bootstrap.STRINGLIT_106;
		(fA1 as JA_L).setArraySlice(1, [ Bootstrap.STRINGLIT_107, Bootstrap.STRINGLIT_108, Bootstrap.STRINGLIT_109, Bootstrap.STRINGLIT_110 ]);
		this.com_jtransc_charset_JTranscCharsetSingleByte_init___Ljava_lang_String_Ljava_lang_String__V((fA1 as JA_L), Bootstrap.STRINGLIT_111);
		return this;
		return this;
	}
	com_jtransc_charset_charsets_JTranscCharsetIBM866([int CLASS_ID = 830]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_ServiceLoader extends java_lang_Object implements java_lang_Iterable {

	java_lang_Class _service = null;
	java_util_List _list = null;
	 java_util_ServiceLoader java_util_ServiceLoader_init__Ljava_lang_Class__V(java_lang_Class p0) {
		this.java_lang_Object_init___V();
		java_util_Objects.requireNonNull_Ljava_lang_Object__Ljava_lang_Object_(p0);
		this._service = p0;
		this._list = java_util_Arrays.asList__Ljava_lang_Object__Ljava_util_List_(this.getInstances_Ljava_lang_String___Ljava_lang_Object_(p0.getName__Ljava_lang_String_()));
		this.reload__V();
		return this;
		return this;
	}
	 JA_L getInstances_Ljava_lang_String___Ljava_lang_Object_(java_lang_String p0) {
		JA_L _out = null;
		_out = null;
		if (java_util_Objects.equals_Ljava_lang_Object_Ljava_lang_Object__Z(p0, Bootstrap.STRINGLIT_112)) {
			_out = new JA_L(6, "[Ljava.lang.Object;");
			_out.data[0] = (new com_jtransc_charset_charsets_JTranscCharsetUTF8()).com_jtransc_charset_charsets_JTranscCharsetUTF8_init___V();
			_out.data[1] = (new com_jtransc_charset_charsets_JTranscCharsetIBM866()).com_jtransc_charset_charsets_JTranscCharsetIBM866_init___V();
			_out.data[2] = (new com_jtransc_charset_charsets_JTranscCharsetLatin1()).com_jtransc_charset_charsets_JTranscCharsetLatin1_init___V();
			_out.data[3] = (new com_jtransc_charset_charsets_JTranscCharsetUSASCII()).com_jtransc_charset_charsets_JTranscCharsetUSASCII_init___V();
			_out.data[4] = (new com_jtransc_charset_charsets_JTranscCharsetUTF16LE()).com_jtransc_charset_charsets_JTranscCharsetUTF16LE_init___V();
			_out.data[5] = (new com_jtransc_charset_charsets_JTranscCharsetUTF16BE()).com_jtransc_charset_charsets_JTranscCharsetUTF16BE_init___V();
			return _out;
		}
		if (java_util_Objects.equals_Ljava_lang_Object_Ljava_lang_Object__Z(p0, Bootstrap.STRINGLIT_113)) {
			_out = new JA_L(1, "[Ljava.lang.Object;");
			_out.data[0] = (new com_jtransc_mix_JTranscProcessMulti()).com_jtransc_mix_JTranscProcessMulti_init___V();
			return _out;
		}
		return new JA_L(0, "[Ljava.lang.Object;");
	}
	 void reload__V() {
		return;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_114).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this._service.getName__Ljava_lang_String_()).toString__Ljava_lang_String_();
	}
	 java_util_Iterator iterator__Ljava_util_Iterator_() {
		return this._list.iterator__Ljava_util_Iterator_();
	}
	static java_util_ServiceLoader load_Ljava_lang_Class__Ljava_util_ServiceLoader_(java_lang_Class p0) {
		return java_util_ServiceLoader.load_Ljava_lang_Class_Ljava_lang_ClassLoader__Ljava_util_ServiceLoader_(p0, java_lang_Thread.currentThread__Ljava_lang_Thread_().getContextClassLoader__Ljava_lang_ClassLoader_());
	}
	static java_util_ServiceLoader load_Ljava_lang_Class_Ljava_lang_ClassLoader__Ljava_util_ServiceLoader_(java_lang_Class p0, java_lang_ClassLoader p1) {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = p1;
					if (((lA1 != null))) {
						G = 1;
						continue;
					}
					lA1 = java_lang_ClassLoader.getSystemClassLoader__Ljava_lang_ClassLoader_();
					G = 1;
					continue;
				case 1:
					tA0 = (new java_util_ServiceLoader());
					fA0 = tA0;
					(tA0 as java_util_ServiceLoader).java_util_ServiceLoader_init__Ljava_lang_Class__V(p0);
					return (fA0 as java_util_ServiceLoader);
				default:
					break;
			}
		}
		return null;
	}
	java_util_ServiceLoader([int CLASS_ID = 829]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_IllegalArgumentException extends java_lang_RuntimeException  {

	 java_lang_IllegalArgumentException java_lang_IllegalArgumentException_init___V() {
		this.java_lang_RuntimeException_init___V();
		return this;
		return this;
	}
	 java_lang_IllegalArgumentException java_lang_IllegalArgumentException_init__Ljava_lang_Throwable__V(java_lang_Throwable p0) {
		this.java_lang_RuntimeException_init__Ljava_lang_Throwable__V(p0);
		return this;
		return this;
	}
	 java_lang_IllegalArgumentException java_lang_IllegalArgumentException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_RuntimeException_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	java_lang_IllegalArgumentException([int CLASS_ID = 705]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_nio_charset_UnsupportedCharsetException extends java_lang_IllegalArgumentException  {

	java_lang_String _charsetName = null;
	 java_nio_charset_UnsupportedCharsetException java_nio_charset_UnsupportedCharsetException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_IllegalArgumentException_init__Ljava_lang_String__V(java_lang_String.valueOf_Ljava_lang_Object__Ljava_lang_String_(p0));
		this._charsetName = p0;
		return this;
		return this;
	}
	java_nio_charset_UnsupportedCharsetException([int CLASS_ID = 828]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_io_Flushable   {

	 void flush__V();
}
class java_io_Flushable_IFields {

	static void SI() { }
}
abstract class java_lang_AutoCloseable   {

}
class java_lang_AutoCloseable_IFields {

	static void SI() { }
}
abstract class java_io_Closeable  implements java_lang_AutoCloseable {

}
class java_io_Closeable_IFields {

	static void SI() { }
}
abstract class java_io_OutputStream extends java_lang_Object implements java_io_Closeable, java_io_Flushable {

	 java_io_OutputStream java_io_OutputStream_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 void write_I_V(int p0) {
		throw new Exception("Missing body java.io.OutputStream.write\u0028I\u0029V");
	}
	 void flush__V() {
		return;
	}
	 void write__BII_V(JA_B p0, int p1, int p2) {
		int G = 0;
		int lI4 = 0;
		while (true) {
			switch (G) {
				case 0:
					com_jtransc_JTranscArrays.checkOffsetAndCount_III_V((p0 as JA_0).length, p1, p2);
					lI4 = p1;
					G = 1;
					continue;
				case 1:
					if (((lI4 >= (N.I(p1 + p2))))) {
						G = 2;
						continue;
					}
					this.write_I_V(((p0).data[lI4]));
					lI4 = (N.I(lI4 + 1));
					G = 1;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void write__B_V(JA_B p0) {
		this.write__BII_V(p0, 0, (p0 as JA_0).length);
		return;
	}
	java_io_OutputStream([int CLASS_ID = 693]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_io_ByteArrayOutputStream extends java_io_OutputStream  {

	int _count = 0;
	JA_B _buf = null;
	 java_lang_String toString__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_String());
		fA0 = tA0;
		(tA0 as java_lang_String).java_lang_String_init___BII_V(this._buf, 0, this._count);
		return (fA0 as java_lang_String);
	}
	 JA_B toByteArray___B() {
		java_lang_Object lA1 = null;
		lA1 = new JA_B(this._count);
		java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(this._buf, 0, lA1, 0, this._count);
		return (lA1 as JA_B);
	}
	 java_io_ByteArrayOutputStream java_io_ByteArrayOutputStream_init__I_V(int p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					this.java_io_OutputStream_init___V();
					if (((p0 < 0))) {
						G = 1;
						continue;
					}
					this._buf = new JA_B(p0);
					G = 2;
					continue;
				case 1:
					tA0 = (new java_lang_IllegalArgumentException());
					fA0 = tA0;
					(tA0 as java_lang_IllegalArgumentException).java_lang_IllegalArgumentException_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_115);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 void write_I_V(int p0) {
		int G = 0;
		JA_B fA0 = null;
		int fI1 = 0;
		java_lang_Object fA1 = null;
		int tI1 = 0;
		java_lang_Object tA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((this._count != (this._buf as JA_0).length))) {
						G = 1;
						continue;
					}
					this.expand_I_V(1);
					G = 1;
					continue;
				case 1:
					fA0 = this._buf;
					fA1 = this;
					tA2 = fA1;
					tI1 = this._count;
					fI1 = tI1;
					(tA2 as java_io_ByteArrayOutputStream)._count = (N.I(tI1 + 1));
					fA0.data[fI1] = N.i2b(p0);
					return;
				default:
					break;
			}
		}
		return;
	}
	 void expand_I_V(int p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if ((((N.I(this._count + p0)) > (this._buf as JA_0).length))) {
						G = 1;
						continue;
					}
					return;
					G = 1;
					continue;
				case 1:
					lA2 = new JA_B((N.I((N.I(this._count + p0)) * 2)));
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(this._buf, 0, lA2, 0, this._count);
					this._buf = (lA2 as JA_B);
					return;
				default:
					break;
			}
		}
		return;
	}
	 void write__BII_V(JA_B p0, int p1, int p2) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					com_jtransc_JTranscArrays.checkOffsetAndCount_III_V((p0 as JA_0).length, p1, p2);
					if (((p2 != 0))) {
						G = 1;
						continue;
					}
					return;
					G = 1;
					continue;
				case 1:
					this.expand_I_V(p2);
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(p0, p1, this._buf, this._count, p2);
					this._count = (N.I(this._count + p2));
					return;
				default:
					break;
			}
		}
		return;
	}
	java_io_ByteArrayOutputStream([int CLASS_ID = 827]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_io_IOException extends java_lang_Exception  {

	java_io_IOException([int CLASS_ID = 825]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_time_JTranscClock_Impl extends java_lang_Object  {

	com_jtransc_time_JTranscClock_Impl _parent = null;
	 com_jtransc_time_JTranscClock_Impl com_jtransc_time_JTranscClock_Impl_init__Lcom_jtransc_time_JTranscClock_Impl__V(com_jtransc_time_JTranscClock_Impl p0) {
		this.java_lang_Object_init___V();
		this._parent = p0;
		return this;
		return this;
	}
	 double fastTime__D() {
		return new DateTime.now().millisecondsSinceEpoch.toDouble();
	}
	com_jtransc_time_JTranscClock_Impl([int CLASS_ID = 824]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_time_JTranscClock_1 extends com_jtransc_time_JTranscClock_Impl  {

	 com_jtransc_time_JTranscClock_1 com_jtransc_time_JTranscClock_1_init__Lcom_jtransc_time_JTranscClock_Impl__V(com_jtransc_time_JTranscClock_Impl p0) {
		this.com_jtransc_time_JTranscClock_Impl_init__Lcom_jtransc_time_JTranscClock_Impl__V(p0);
		return this;
		return this;
	}
	com_jtransc_time_JTranscClock_1([int CLASS_ID = 823]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_time_JTranscClock extends java_lang_Object  {

	static com_jtransc_time_JTranscClock_Impl _impl = null;
	 com_jtransc_time_JTranscClock com_jtransc_time_JTranscClock_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void com_jtransc_time_JTranscClock_clinit___V() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new com_jtransc_time_JTranscClock_1());
		fA0 = tA0;
		(tA0 as com_jtransc_time_JTranscClock_1).com_jtransc_time_JTranscClock_1_init__Lcom_jtransc_time_JTranscClock_Impl__V(null);
		com_jtransc_time_JTranscClock._impl = (fA0 as com_jtransc_time_JTranscClock_Impl);
		return;
	}
	com_jtransc_time_JTranscClock([int CLASS_ID = 822]) : super(CLASS_ID) { }
	static void SI() {
		com_jtransc_time_JTranscClock._impl = null;
		com_jtransc_time_JTranscClock.com_jtransc_time_JTranscClock_clinit___V();
	}
}
class com_jtransc_JTranscVersion extends java_lang_Object  {

	 com_jtransc_JTranscVersion com_jtransc_JTranscVersion_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static java_lang_String getVersion__Ljava_lang_String_() {
		return Bootstrap.STRINGLIT_116;
	}
	com_jtransc_JTranscVersion([int CLASS_ID = 821]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class Benchmark_Task   {

	 int run__I();
}
class Benchmark_Task_IFields {

	static void SI() { }
}
class Benchmark_20 extends java_lang_Object implements Benchmark_Task {

	JA_F _val_farray = null;
	 Benchmark_20 Benchmark_20_init___F_V(JA_F p0) {
		this._val_farray = p0;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= 1000000))) {
						G = 2;
						continue;
					}
					this._val_farray.data[lI1] = N.i2f((N.I(lI1 * 1000)));
					lI1 = (N.I(lI1 + 1));
					G = 1;
					continue;
				case 2:
					return (N.f2i((this._val_farray).data[7]));
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_20([int CLASS_ID = 820]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_21 extends java_lang_Object implements Benchmark_Task {

	JA_D _val_darray = null;
	 Benchmark_21 Benchmark_21_init___D_V(JA_D p0) {
		this._val_darray = p0;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= 1000000))) {
						G = 2;
						continue;
					}
					this._val_darray.data[lI1] = N.i2d((N.I(lI1 * 1000)));
					lI1 = (N.I(lI1 + 1));
					G = 1;
					continue;
				case 2:
					return (N.d2i((this._val_darray).data[7]));
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_21([int CLASS_ID = 819]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_22 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_22 Benchmark_22_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA1 = null;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA1 = fA0;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 100000))) {
						G = 2;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_I_Ljava_lang_StringBuilder_(lI2);
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return (lA1 as java_lang_StringBuilder).toString__Ljava_lang_String_().hashCode__I();
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_22([int CLASS_ID = 818]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_27 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_27 Benchmark_27_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		Float32x4List lA1 = null;
		Float32x4List lA2 = null;
		int G = 0;
		int lI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					lA1 = new Float32x4List(4);
					lA1[0] = new Float32x4(1.0, 9.0, 1.0, 7.0); lA1[1] = new Float32x4(3.0, 2.0, 4.0, 5.0); lA1[2] = new Float32x4(3.0, 7.0, 3.0, 3.0); lA1[3] = new Float32x4(3.0, 8.0, 4.0, 4.0);;
					lA2 = new Float32x4List(4);
					lA2[0] = new Float32x4(2.0, 3.0, 4.0, 5.0); lA2[1] = new Float32x4(2.0, 3.0, 4.0, 5.0); lA2[2] = new Float32x4(2.0, 3.0, 4.0, 5.0); lA2[3] = new Float32x4(2.0, 3.0, 4.0, 5.0);;
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 100000))) {
						G = 2;
						continue;
					}
					lA1 = com_jtransc_simd_MutableMatrixFloat32x4x4Utils._setToMul44_Lcom_jtransc_simd_MutableMatrixFloat32x4x4_Lcom_jtransc_simd_MutableMatrixFloat32x4x4_Lcom_jtransc_simd_MutableMatrixFloat32x4x4__Lcom_jtransc_simd_MutableMatrixFloat32x4x4_(lA1, lA1, lA2);;
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return (N.f2i(com_jtransc_simd_MutableMatrixFloat32x4x4Utils._getSumAll_Lcom_jtransc_simd_MutableMatrixFloat32x4x4__F(lA1)));
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_27([int CLASS_ID = 817]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_28 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_28 Benchmark_28_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA1 = null;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA1 = fA0;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 100000))) {
						G = 2;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_41);
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(119);
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_117);
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return (lA1 as java_lang_StringBuilder).toString__Ljava_lang_String_().length__I();
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_28([int CLASS_ID = 816]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_29 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_29 Benchmark_29_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA1 = null;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA1 = fA0;
					(lA1 as java_lang_StringBuilder).ensureCapacity_I_V(1000000);
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 100000))) {
						G = 2;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_41);
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(119);
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_117);
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return (lA1 as java_lang_StringBuilder).toString__Ljava_lang_String_().length__I();
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_29([int CLASS_ID = 815]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_23 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_23 Benchmark_23_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA1 = null;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA1 = fA0;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 100000))) {
						G = 2;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_118);
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return (lA1 as java_lang_StringBuilder).toString__Ljava_lang_String_().hashCode__I();
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_23([int CLASS_ID = 814]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_24 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_24 Benchmark_24_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI3 = 0;
		Int64 lJ1 = N.lnew(0);
		while (true) {
			switch (G) {
				case 0:
					lJ1 = N.lnew(0);
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 10000))) {
						G = 2;
						continue;
					}
					lJ1 = ((((N.lnew(17777)*N.i2j(lI3)))+((lJ1~/N.lnew(3)))));
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return N.j2i(lJ1);
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_24([int CLASS_ID = 813]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_25 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_25 Benchmark_25_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI3 = 0;
		Float32x4 lA1 = null;
		Float32x4 lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = new Float32x4(0.0, 0.0, 0.0, 0.0);
					lA2 = new Float32x4(2.0, 3.0, 4.0, 5.0);
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 1000000))) {
						G = 2;
						continue;
					}
					lA1 = lA1 + lA2;;
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return (N.I((N.I((N.I((N.f2i(lA1.x)) + (N.f2i(lA1.y)))) + (N.f2i(lA1.z)))) + (N.f2i(lA1.w))));
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_25([int CLASS_ID = 812]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_26 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_26 Benchmark_26_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI3 = 0;
		Float32x4 lA1 = null;
		Float32x4 lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = new Float32x4(0.0, 0.0, 0.0, 0.0);
					lA2 = new Float32x4(2.0, 3.0, 4.0, 5.0);
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 1000000))) {
						G = 2;
						continue;
					}
					lA1 = ((lA1) + (lA2));
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return (N.I((N.I((N.I((N.f2i(((lA1).x))) + (N.f2i(((lA1).y))))) + (N.f2i(((lA1).z))))) + (N.f2i(((lA1).w)))));
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_26([int CLASS_ID = 811]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_10 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_10 Benchmark_10_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 305419896;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 1000000))) {
						G = 2;
						continue;
					}
					lI1 = (N.I(lI1 + (N.I((N.iushr(lI1, lI2)) + (N.iushr(lI1, (N.ineg(lI2))))))));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_10([int CLASS_ID = 810]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_11 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_11 Benchmark_11_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 1000000))) {
						G = 2;
						continue;
					}
					lI1 = (N.I(lI1 + Benchmark.calc_II_I(lI1, lI2)));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_11([int CLASS_ID = 809]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_16 extends java_lang_Object implements Benchmark_Task {

	JA_B _val_barray = null;
	 Benchmark_16 Benchmark_16_init___B_V(JA_B p0) {
		this._val_barray = p0;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= 1000000))) {
						G = 2;
						continue;
					}
					this._val_barray.data[lI1] = N.i2b((N.I(lI1 * 123456711)));
					lI1 = (N.I(lI1 + 1));
					G = 1;
					continue;
				case 2:
					return ((this._val_barray).data[7]);
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_16([int CLASS_ID = 808]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_17 extends java_lang_Object implements Benchmark_Task {

	JA_S _val_sarray = null;
	 Benchmark_17 Benchmark_17_init___S_V(JA_S p0) {
		this._val_sarray = p0;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= 1000000))) {
						G = 2;
						continue;
					}
					this._val_sarray.data[lI1] = N.i2s((N.I(lI1 * 1000)));
					lI1 = (N.I(lI1 + 1));
					G = 1;
					continue;
				case 2:
					return ((this._val_sarray).data[7]);
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_17([int CLASS_ID = 807]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_18 extends java_lang_Object implements Benchmark_Task {

	JA_C _val_carray = null;
	 Benchmark_18 Benchmark_18_init___C_V(JA_C p0) {
		this._val_carray = p0;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= 1000000))) {
						G = 2;
						continue;
					}
					this._val_carray.data[lI1] = N.i2c((N.I(lI1 * 1000)));
					lI1 = (N.I(lI1 + 1));
					G = 1;
					continue;
				case 2:
					return ((this._val_carray).data[7]);
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_18([int CLASS_ID = 806]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_19 extends java_lang_Object implements Benchmark_Task {

	JA_I _val_iarray = null;
	 Benchmark_19 Benchmark_19_init___I_V(JA_I p0) {
		this._val_iarray = p0;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= 1000000))) {
						G = 2;
						continue;
					}
					this._val_iarray.data[lI1] = (N.I(lI1 * 1000));
					lI1 = (N.I(lI1 + 1));
					G = 1;
					continue;
				case 2:
					return (this._val_iarray).data[7];
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_19([int CLASS_ID = 805]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_12 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_12 Benchmark_12_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 1000000))) {
						G = 2;
						continue;
					}
					lI1 = (N.I(lI1 + this.calc_II_I(lI1, lI2)));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	 int calc_II_I(int p0, int p1) {
		return (N.I((N.I(p0 + p1)) * (N.I(p0 + p1))));
	}
	Benchmark_12([int CLASS_ID = 804]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_13 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_13 Benchmark_13_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 1;
					lI2 = 1;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 1000000))) {
						G = 2;
						continue;
					}
					lI1 = (N.I(lI1 + this.calc_II_I(lI1, lI2)));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	 int calc_II_I(int p0, int p1) {
		return (N.I((N.I(p0 - p1)) ~/ (N.I(p0 + p1))));
	}
	Benchmark_13([int CLASS_ID = 803]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_14 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_14 Benchmark_14_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA3 = null;
		java_lang_Object lA4 = null;
		int lI1 = 0;
		int lI2 = 0;
		int lI5 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 1;
					lI2 = this.rand_I_I(2);
					lA3 = this.genObj_I_Ljava_lang_Object_((N.I((N.I(lI2 + 0)).remainder(2))));
					lA4 = this.genObj_I_Ljava_lang_Object_((N.I((N.I(lI2 + 1)).remainder(2))));
					lI5 = 1;
					G = 1;
					continue;
				case 1:
					if (((lI5 >= 1000000))) {
						G = 2;
						continue;
					}
					if (!(((lA3) is Benchmark_Test1))) {
						G = 3;
						continue;
					}
					lI1 = (N.I(lI1 + (N.I(lI5 - 1))));
					G = 4;
					continue;
				case 3:
					if (!(((lA3) is Benchmark_Test2))) {
						G = 4;
						continue;
					}
					lI1 = (N.I(lI1 + (N.I(lI5 + 2))));
					G = 4;
					continue;
				case 4:
					if (!(((lA4) is Benchmark_Test1))) {
						G = 5;
						continue;
					}
					lI1 = (N.I(lI1 + (N.I(lI5 - 3))));
					G = 6;
					continue;
				case 5:
					if (!(((lA4) is Benchmark_Test2))) {
						G = 6;
						continue;
					}
					lI1 = (N.I(lI1 + (N.I(lI5 + 4))));
					G = 6;
					continue;
				case 6:
					lI5 = (N.I(lI5 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	 int rand_I_I(int p0) {
		return N.j2i((N.lrem(java_lang_System.currentTimeMillis__J(), N.i2j(p0))));
	}
	 java_lang_Object genObj_I_Ljava_lang_Object_(int p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					switch (p0) {
						case 0:
							G = 2;
							continue;
						default:
							G = 1;
							continue;
					}
					G = 2;
					continue;
				case 2:
					tA1 = (new Benchmark_Test1());
					fA0 = tA1;
					(tA1 as Benchmark_Test1).Benchmark_Test1_init__LBenchmark_1__V(null);
					return fA0;
				case 1:
					tA0 = (new Benchmark_Test2());
					fA0 = tA0;
					(tA0 as Benchmark_Test2).Benchmark_Test2_init__LBenchmark_1__V(null);
					return fA0;
				default:
					break;
			}
		}
		return null;
	}
	Benchmark_14([int CLASS_ID = 802]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_15 extends java_lang_Object implements Benchmark_Task {

	JA_I _val_srcI = null;
	JA_I _val_dstI = null;
	 Benchmark_15 Benchmark_15_init___I_I_V(JA_I p0, JA_I p1) {
		this._val_srcI = p0;
		this._val_dstI = p1;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI1 >= 1024))) {
						G = 2;
						continue;
					}
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(this._val_srcI, 0, this._val_dstI, lI1, 8192);
					lI1 = (N.I(lI1 + 1));
					G = 1;
					continue;
				case 2:
					return 0;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_15([int CLASS_ID = 801]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_9 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_9 Benchmark_9_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 305419896;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 1000000))) {
						G = 2;
						continue;
					}
					lI1 = (N.I(lI1 + (N.I((N.ishr(lI1, lI2)) + (N.ishr(lI1, (N.ineg(lI2))))))));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_9([int CLASS_ID = 800]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_8 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_8 Benchmark_8_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 305419896;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 1000000))) {
						G = 2;
						continue;
					}
					lI1 = (N.I(lI1 + (N.I((N.ishl(lI1, lI2)) + (N.ishl(lI1, (N.ineg(lI2))))))));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_8([int CLASS_ID = 799]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_7 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_7 Benchmark_7_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI3 = 0;
		Int64 lJ1 = N.lnew(0);
		while (true) {
			switch (G) {
				case 0:
					lJ1 = N.lnew(305419896);
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 1000000))) {
						G = 2;
						continue;
					}
					lJ1 = ((lJ1+(N.lushr_opt(lJ1, 1))));
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return N.j2i(lJ1);
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_7([int CLASS_ID = 798]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_41 extends java_lang_Object implements Benchmark_Task {

	JA_B _val_bytes = null;
	 Benchmark_41 Benchmark_41_init___B_V(JA_B p0) {
		this._val_bytes = p0;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA2 = null;
		int fI0 = 0;
		int lI3 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							G = 1;
							continue;
						case 1:
							lA1 = new JA_B(131072);
							tA0 = (new java_util_zip_Deflater());
							fA0 = tA0;
							(tA0 as java_util_zip_Deflater).java_util_zip_Deflater_init__IZ_V(9, false);
							lA2 = fA0;
							(lA2 as java_util_zip_Deflater).setInput__BII_V(this._val_bytes, 0, (this._val_bytes as JA_0).length);
							lI3 = (lA2 as java_util_zip_Deflater).deflate__BIII_I((lA1 as JA_B), 0, (lA1 as JA_0).length, 3);
							fI0 = lI3;
							G = 2;
							continue;
						case 2: return fI0;
						case 3:
							fA0 = J__exception__;
							lA1 = fA0;
							(lA1 as java_lang_Throwable).printStackTrace__V();
							return 0;
						default:
							break;
					}
				}
				return 0;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return 0;
	}
	Benchmark_41([int CLASS_ID = 797]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_6 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_6 Benchmark_6_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI3 = 0;
		Int64 lJ1 = N.lnew(0);
		while (true) {
			switch (G) {
				case 0:
					lJ1 = N.lnew(305419896);
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 1000000))) {
						G = 2;
						continue;
					}
					lJ1 = ((lJ1+((lJ1 >> 1))));
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return N.j2i(lJ1);
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_6([int CLASS_ID = 796]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_42 extends java_lang_Object implements Benchmark_Task {

	JA_B _val_bytes = null;
	 Benchmark_42 Benchmark_42_init___B_V(JA_B p0) {
		this._val_bytes = p0;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA2 = null;
		int fI0 = 0;
		int lI3 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							G = 1;
							continue;
						case 1:
							lA1 = new JA_B(131072);
							tA0 = (new com_jtransc_compression_jzlib_Deflater());
							fA0 = tA0;
							(tA0 as com_jtransc_compression_jzlib_Deflater).com_jtransc_compression_jzlib_Deflater_init__IZ_V(9, false);
							lA2 = fA0;
							(lA2 as com_jtransc_compression_jzlib_Deflater).setInput__BIIZ_V(this._val_bytes, 0, (this._val_bytes as JA_0).length, false);
							(lA2 as com_jtransc_compression_jzlib_Deflater).setOutput__BII_V((lA1 as JA_B), 0, (lA1 as JA_0).length);
							lI3 = (lA2 as com_jtransc_compression_jzlib_Deflater).deflate_I_I(3);
							fI0 = lI3;
							G = 2;
							continue;
						case 2: return fI0;
						case 3:
							fA0 = J__exception__;
							lA1 = fA0;
							(lA1 as java_lang_Throwable).printStackTrace__V();
							return 0;
						default:
							break;
					}
				}
				return 0;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return 0;
	}
	Benchmark_42([int CLASS_ID = 795]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_5 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_5 Benchmark_5_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI3 = 0;
		Int64 lJ1 = N.lnew(0);
		while (true) {
			switch (G) {
				case 0:
					lJ1 = N.lnew(305419896);
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 1000000))) {
						G = 2;
						continue;
					}
					lJ1 = ((lJ1+((lJ1 << 1))));
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return N.j2i(lJ1);
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_5([int CLASS_ID = 794]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_43 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_43 Benchmark_43_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA2 = null;
		int lI3 = 0;
		int lI4 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_util_Random());
					fA0 = tA0;
					(tA0 as java_util_Random).java_util_Random_init__J_V(N.lnew(0));
					lA1 = fA0;
					lA2 = new JA_B(65536);
					lI3 = 0;
					lI4 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI4 >= (lA2 as JA_0).length))) {
						G = 2;
						continue;
					}
					(lA2 as JA_B).data[lI4] = N.i2b((lA1 as java_util_Random).nextInt__I());
					lI3 = (N.I(lI3 + ((lA2 as JA_B)).data[lI4]));
					lI4 = (N.I(lI4 + 1));
					G = 1;
					continue;
				case 2:
					return lI3;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_43([int CLASS_ID = 793]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_4 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_4 Benchmark_4_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 305419896;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 1000000))) {
						G = 2;
						continue;
					}
					lI1 = (N.I(lI1 + (N.iushr_opt(lI1, 1))));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_4([int CLASS_ID = 792]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_44 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_44 Benchmark_44_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							lI1 = 0;
							lI2 = 0;
							G = 1;
							continue;
						case 1:
							if (((lI2 >= 1000))) {
								G = 2;
								continue;
							}
							G = 3;
							continue;
						case 3:
							tA0 = (new java_lang_Throwable());
							fA0 = tA0;
							(tA0 as java_lang_Throwable).java_lang_Throwable_init___V();
							throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
							G = 4;
							continue;
						case 4:
							fA0 = J__exception__;
							lI1 = (N.I(lI1 + 1));
							lI2 = (N.I(lI2 + 1));
							G = 1;
							continue;
						case 2:
							return lI1;
						default:
							break;
					}
				}
				return 0;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 3)) && ((G < 4)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 4;
					continue;
				}
				rethrow;
			}
		}
		return 0;
	}
	Benchmark_44([int CLASS_ID = 791]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_3 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_3 Benchmark_3_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 305419896;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 1000000))) {
						G = 2;
						continue;
					}
					lI1 = (N.I(lI1 + (N.I(lI1 >> 1))));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_3([int CLASS_ID = 790]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_2 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_2 Benchmark_2_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 305419896;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 1000000))) {
						G = 2;
						continue;
					}
					lI1 = (N.I(lI1 + (N.I(lI1 << 1))));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_2([int CLASS_ID = 789]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_1 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_1 Benchmark_1_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 1000000))) {
						G = 2;
						continue;
					}
					lI1 = (N.I(lI1 + lI2));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_1([int CLASS_ID = 788]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_40 extends java_lang_Object implements Benchmark_Task {

	JA_B _val_hexData = null;
	 Benchmark_40 Benchmark_40_init___B_V(JA_B p0) {
		this._val_hexData = p0;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA2 = null;
		int lI1 = 0;
		int lI3 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					tA0 = (new com_jtransc_compression_jzlib_CRC32());
					fA0 = tA0;
					(tA0 as com_jtransc_compression_jzlib_CRC32).com_jtransc_compression_jzlib_CRC32_init___V();
					lA2 = fA0;
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 10000))) {
						G = 2;
						continue;
					}
					(lA2 as com_jtransc_compression_jzlib_CRC32).reset__V();
					(lA2 as com_jtransc_compression_jzlib_CRC32).update__BII_V(this._val_hexData, 0, (this._val_hexData as JA_0).length);
					lI1 = (N.I(lI1 + (lA2 as com_jtransc_compression_jzlib_CRC32).getValue__I()));
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_40([int CLASS_ID = 787]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_Runtime extends java_lang_Object  {

	static java_lang_Runtime _current = null;
	 java_lang_Runtime java_lang_Runtime_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 void gc__V() {
		return;
	}
	static java_lang_Runtime getRuntime__Ljava_lang_Runtime_() {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((java_lang_Runtime._current != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_Runtime());
					fA0 = tA0;
					(tA0 as java_lang_Runtime).java_lang_Runtime_init___V();
					java_lang_Runtime._current = (fA0 as java_lang_Runtime);
					G = 1;
					continue;
				case 1:
					return java_lang_Runtime._current;
				default:
					break;
			}
		}
		return null;
	}
	 Int64 freeMemory__J() {
		return N.lnew(8589934592);
	}
	 Int64 maxMemory__J() {
		return N.lnew(8589934592);
	}
	 Int64 totalMemory__J() {
		return N.lnew(8589934592);
	}
	java_lang_Runtime([int CLASS_ID = 786]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_30 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_30 Benchmark_30_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		java_nio_IntBuffer lA2 = null;
		int G = 0;
		java_nio_ByteBuffer lA1 = null;
		int lI4 = 0;
		int lI5 = 0;
		java_nio_FloatBuffer lA3 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = java_nio_ByteBuffer.allocate_I_Ljava_nio_ByteBuffer_(1024).order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder.nativeOrder__Ljava_nio_ByteOrder_());
					lA2 = lA1.asIntBuffer__Ljava_nio_IntBuffer_();
					lA3 = lA1.asFloatBuffer__Ljava_nio_FloatBuffer_();
					lI4 = 0;
					lI5 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI5 >= 100000))) {
						G = 2;
						continue;
					}
					lA3.put_IF_Ljava_nio_FloatBuffer_(0, N.i2f(lI5));
					lI4 = (N.I(lI4 + lA2.get_I_I(0)));
					lI5 = (N.I(lI5 + 1));
					G = 1;
					continue;
				case 2:
					return lI4;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_30([int CLASS_ID = 785]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_31 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_31 Benchmark_31_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		java_nio_IntBuffer lA2 = null;
		int G = 0;
		java_nio_ByteBuffer lA1 = null;
		int lI4 = 0;
		int lI5 = 0;
		java_nio_FloatBuffer lA3 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = java_nio_ByteBuffer.allocateDirect_I_Ljava_nio_ByteBuffer_(1024).order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder.nativeOrder__Ljava_nio_ByteOrder_());
					lA2 = lA1.asIntBuffer__Ljava_nio_IntBuffer_();
					lA3 = lA1.asFloatBuffer__Ljava_nio_FloatBuffer_();
					lI4 = 0;
					lI5 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI5 >= 100000))) {
						G = 2;
						continue;
					}
					lA3.put_IF_Ljava_nio_FloatBuffer_(0, N.i2f(lI5));
					lI4 = (N.I(lI4 + lA2.get_I_I(0)));
					lI5 = (N.I(lI5 + 1));
					G = 1;
					continue;
				case 2:
					return lI4;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_31([int CLASS_ID = 784]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_32 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_32 Benchmark_32_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_nio_ByteBuffer lA1 = null;
		int lI4 = 0;
		int lI5 = 0;
		java_nio_ShortBuffer lA2 = null;
		java_nio_CharBuffer lA3 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = java_nio_ByteBuffer.allocateDirect_I_Ljava_nio_ByteBuffer_(1024).order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder.nativeOrder__Ljava_nio_ByteOrder_());
					lA2 = lA1.asShortBuffer__Ljava_nio_ShortBuffer_();
					lA3 = lA1.asCharBuffer__Ljava_nio_CharBuffer_();
					lI4 = 0;
					lI5 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI5 >= 100000))) {
						G = 2;
						continue;
					}
					lA3.put_IC_Ljava_nio_CharBuffer_(0, N.i2c(lI5));
					lI4 = (N.I(lI4 + lA2.get_I_S(0)));
					lI5 = (N.I(lI5 + 1));
					G = 1;
					continue;
				case 2:
					return lI4;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_32([int CLASS_ID = 783]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_33 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_33 Benchmark_33_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_nio_ByteBuffer lA1 = null;
		int lI4 = 0;
		int lI5 = 0;
		java_nio_LongBuffer lA2 = null;
		java_nio_DoubleBuffer lA3 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = java_nio_ByteBuffer.allocateDirect_I_Ljava_nio_ByteBuffer_(1024).order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder.nativeOrder__Ljava_nio_ByteOrder_());
					lA2 = lA1.asLongBuffer__Ljava_nio_LongBuffer_();
					lA3 = lA1.asDoubleBuffer__Ljava_nio_DoubleBuffer_();
					lI4 = 0;
					lI5 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI5 >= 100000))) {
						G = 2;
						continue;
					}
					lA3.put_ID_Ljava_nio_DoubleBuffer_(0, N.i2d(lI5));
					lI4 = N.j2i(((N.i2j(lI4)+lA2.get_I_J(0))));
					lI5 = (N.I(lI5 + 1));
					G = 1;
					continue;
				case 2:
					return lI4;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_33([int CLASS_ID = 782]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_Random extends java_lang_Object implements java_io_Serializable {

	Int64 _seed = N.lnew(0);
	bool _haveNextNextGaussian = false;
	 int nextInt__I() {
		return this.next_I_I(32);
	}
	 int next_I_I(int p0) {
		this._seed = ((((((this._seed*N.lnew(25214903917)))+N.lnew(11)))&N.lnew(281474976710655)));
		return N.j2i((N.lushr(this._seed, (N.I(48 - p0)))));
	}
	 java_util_Random java_util_Random_init__J_V(Int64 p0) {
		this.java_lang_Object_init___V();
		this.setSeed_J_V(p0);
		return this;
		return this;
	}
	 void setSeed_J_V(Int64 p0) {
		this._seed = ((((p0^N.lnew(25214903917)))&N.lnew(281474976710655)));
		this._haveNextNextGaussian = false;
		return;
	}
	java_util_Random([int CLASS_ID = 781]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_38 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_38 Benchmark_38_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA3 = null;
		int lI1 = 0;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 100000))) {
						G = 2;
						continue;
					}
					tA0 = (new Benchmark_MyClass());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as Benchmark_MyClass).Benchmark_MyClass_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_119).append_I_Ljava_lang_StringBuilder_(lI2).toString__Ljava_lang_String_());
					lA3 = fA0;
					lI1 = (N.I(lI1 + (lA3 as Benchmark_MyClass)._b));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_38([int CLASS_ID = 780]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_39 extends java_lang_Object implements Benchmark_Task {

	JA_B _val_hexData = null;
	 Benchmark_39 Benchmark_39_init___B_V(JA_B p0) {
		this._val_hexData = p0;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA2 = null;
		int lI1 = 0;
		int lI3 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					tA0 = (new java_util_zip_CRC32());
					fA0 = tA0;
					(tA0 as java_util_zip_CRC32).java_util_zip_CRC32_init___V();
					lA2 = fA0;
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 10000))) {
						G = 2;
						continue;
					}
					(lA2 as java_util_zip_CRC32).reset__V();
					(lA2 as java_util_zip_CRC32).update__BII_V(this._val_hexData, 0, (this._val_hexData as JA_0).length);
					lI1 = N.j2i(((N.i2j(lI1)+(lA2 as java_util_zip_CRC32).getValue__J())));
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_39([int CLASS_ID = 779]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_JTranscSystem extends java_lang_Object  {

	static double _start = 0.0;
	 com_jtransc_JTranscSystem com_jtransc_JTranscSystem_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void com_jtransc_JTranscSystem_clinit___V() {
		com_jtransc_JTranscSystem._start = -1.0;
		return;
	}
	static double stamp__D() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(((com_jtransc_JTranscSystem._start < 0.0)))) {
						G = 1;
						continue;
					}
					com_jtransc_JTranscSystem._start = com_jtransc_JTranscSystem.fastTime__D();
					G = 1;
					continue;
				case 1:
					return com_jtransc_JTranscSystem.elapsedTime_DD_D(com_jtransc_JTranscSystem._start, com_jtransc_JTranscSystem.fastTime__D());
				default:
					break;
			}
		}
		return 0.0;
	}
	static double elapsedTime_DD_D(double p0, double p1) {
		return ((p1 - p0));
	}
	static double fastTime__D() {
		return com_jtransc_time_JTranscClock._impl.fastTime__D();
	}
	static java_lang_String lineSeparator__Ljava_lang_String_() {
		return com_jtransc_JTranscSystemProperties.lineSeparator__Ljava_lang_String_();
	}
	static bool isCpp__Z() {
		return false;
	}
	static java_lang_String getRuntimeKind__Ljava_lang_String_() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (com_jtransc_JTranscSystem.usingJTransc__Z()) {
						G = 1;
						continue;
					}
					return Bootstrap.STRINGLIT_120;
				case 1:
					if (!(com_jtransc_JTranscSystem.isHaxe__Z())) {
						G = 2;
						continue;
					}
					if (!(com_jtransc_JTranscSystem.isJs__Z())) {
						G = 3;
						continue;
					}
					return Bootstrap.STRINGLIT_121;
				case 3:
					if (!(com_jtransc_JTranscSystem.isAs3__Z())) {
						G = 4;
						continue;
					}
					return Bootstrap.STRINGLIT_122;
				case 4:
					if (!(com_jtransc_JTranscSystem.isNeko__Z())) {
						G = 5;
						continue;
					}
					return Bootstrap.STRINGLIT_123;
				case 5:
					if (!(com_jtransc_JTranscSystem.isCpp__Z())) {
						G = 6;
						continue;
					}
					return Bootstrap.STRINGLIT_124;
				case 6:
					return Bootstrap.STRINGLIT_125;
				case 2:
					if (!(com_jtransc_JTranscSystem.isSwf__Z())) {
						G = 7;
						continue;
					}
					return Bootstrap.STRINGLIT_126;
				case 7:
					if (!(com_jtransc_JTranscSystem.isJvm__Z())) {
						G = 8;
						continue;
					}
					return Bootstrap.STRINGLIT_120;
				case 8:
					if (!(com_jtransc_JTranscSystem.isCsharp__Z())) {
						G = 9;
						continue;
					}
					return Bootstrap.STRINGLIT_127;
				case 9:
					if (!(com_jtransc_JTranscSystem.isNeko__Z())) {
						G = 10;
						continue;
					}
					return Bootstrap.STRINGLIT_128;
				case 10:
					if (!(com_jtransc_JTranscSystem.isPhp__Z())) {
						G = 11;
						continue;
					}
					return Bootstrap.STRINGLIT_129;
				case 11:
					if (!(com_jtransc_JTranscSystem.isPython__Z())) {
						G = 12;
						continue;
					}
					return Bootstrap.STRINGLIT_130;
				case 12:
					if (!(com_jtransc_JTranscSystem.isAs3__Z())) {
						G = 13;
						continue;
					}
					return Bootstrap.STRINGLIT_131;
				case 13:
					if (!(com_jtransc_JTranscSystem.isDart__Z())) {
						G = 14;
						continue;
					}
					return Bootstrap.STRINGLIT_132;
				case 14:
					if (!(com_jtransc_JTranscSystem.isCpp__Z())) {
						G = 15;
						continue;
					}
					return Bootstrap.STRINGLIT_133;
				case 15:
					return Bootstrap.STRINGLIT_134;
				default:
					break;
			}
		}
		return null;
	}
	static bool isJs__Z() {
		return false;
	}
	static bool isNeko__Z() {
		return false;
	}
	static bool isCsharp__Z() {
		return false;
	}
	static bool isPhp__Z() {
		return false;
	}
	static bool usingJTransc__Z() {
		return true;
	}
	static bool isAs3__Z() {
		return false;
	}
	static bool isJvm__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (com_jtransc_JTranscSystem.isJTransc__Z()) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	static bool isJTransc__Z() {
		return com_jtransc_JTranscSystem.usingJTransc__Z();
	}
	static bool isSwf__Z() {
		return false;
	}
	static bool isPython__Z() {
		return false;
	}
	static bool isHaxe__Z() {
		return false;
	}
	static bool isDart__Z() {
		return true;
	}
	static java_lang_String getOS__Ljava_lang_String_() {
		int G = 0;
		java_lang_String lA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lA0 = com_jtransc_JTranscSystem.getOSRaw__Ljava_lang_String_().toLowerCase__Ljava_lang_String_();
					if (!(lA0.startsWith_Ljava_lang_String__Z(Bootstrap.STRINGLIT_135))) {
						G = 1;
						continue;
					}
					return Bootstrap.STRINGLIT_136;
				case 1:
					if (!(lA0.startsWith_Ljava_lang_String__Z(Bootstrap.STRINGLIT_137))) {
						G = 2;
						continue;
					}
					return Bootstrap.STRINGLIT_138;
				case 2:
					if (lA0.startsWith_Ljava_lang_String__Z(Bootstrap.STRINGLIT_139)) {
						G = 3;
						continue;
					}
					if (!(lA0.startsWith_Ljava_lang_String__Z(Bootstrap.STRINGLIT_140))) {
						G = 4;
						continue;
					}
					G = 3;
					continue;
				case 3: return Bootstrap.STRINGLIT_139;
				case 4:
					if (!(lA0.startsWith_Ljava_lang_String__Z(Bootstrap.STRINGLIT_141))) {
						G = 5;
						continue;
					}
					return Bootstrap.STRINGLIT_142;
				case 5:
					return lA0;
				default:
					break;
			}
		}
		return null;
	}
	static java_lang_String getOSRaw__Ljava_lang_String_() {
		return N.str(Platform.operatingSystem);
	}
	static bool isWindows__Z() {
		return com_jtransc_JTranscSystem.getOS__Ljava_lang_String_().toLowerCase__Ljava_lang_String_().startsWith_Ljava_lang_String__Z(Bootstrap.STRINGLIT_135);
	}
	static java_lang_String getRuntimeName__Ljava_lang_String_() {
		return Bootstrap.STRINGLIT_120;
	}
	static java_lang_String getJavaHome__Ljava_lang_String_() {
		return java_lang_System.getenv_Ljava_lang_String__Ljava_lang_String_(Bootstrap.STRINGLIT_143);
	}
	static java_lang_String getArch__Ljava_lang_String_() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(com_jtransc_JTranscSystem.isJvm__Z())) {
						G = 1;
						continue;
					}
					return java_lang_System.getProperty_Ljava_lang_String__Ljava_lang_String_(Bootstrap.STRINGLIT_144);
				case 1:
					return Bootstrap.STRINGLIT_134;
				default:
					break;
			}
		}
		return null;
	}
	static java_lang_String pathSeparator__Ljava_lang_String_() {
		return com_jtransc_JTranscSystemProperties.pathSeparator__Ljava_lang_String_();
	}
	static java_lang_String fileSeparator__Ljava_lang_String_() {
		return com_jtransc_JTranscSystemProperties.fileSeparator__Ljava_lang_String_();
	}
	com_jtransc_JTranscSystem([int CLASS_ID = 778]) : super(CLASS_ID) { }
	static void SI() {
		com_jtransc_JTranscSystem._start = 0.0;
		com_jtransc_JTranscSystem.com_jtransc_JTranscSystem_clinit___V();
	}
}
class Benchmark_34 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_34 Benchmark_34_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		int lI2 = 0;
		int lI3 = 0;
		com_jtransc_FastMemory lA1 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = com_jtransc_FastMemory.alloc_I_Lcom_jtransc_FastMemory_(1024);
					lI2 = 0;
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 100000))) {
						G = 2;
						continue;
					}
					lA1.setAlignedFloat32_IF_V(0, N.i2f(lI3));
					lI2 = (N.I(lI2 + lA1.getAlignedInt32_I_I(0)));
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return lI2;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_34([int CLASS_ID = 777]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_35 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_35 Benchmark_35_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA3 = null;
		int lI1 = 0;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= 100000))) {
						G = 2;
						continue;
					}
					tA0 = (new Benchmark_MyClass());
					fA0 = tA0;
					(tA0 as Benchmark_MyClass).Benchmark_MyClass_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_119);
					lA3 = fA0;
					lI1 = (N.I(lI1 + (lA3 as Benchmark_MyClass)._b));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_35([int CLASS_ID = 776]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_36 extends java_lang_Object implements Benchmark_Task {

	 Benchmark_36 Benchmark_36_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA4 = null;
		int lI1 = 0;
		int lI3 = 0;
		java_lang_Object fA0 = null;
		java_lang_String lA2 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					lA2 = Bootstrap.STRINGLIT_119;
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 100000))) {
						G = 2;
						continue;
					}
					tA0 = (new Benchmark_MyClass2());
					fA0 = tA0;
					(tA0 as Benchmark_MyClass2).Benchmark_MyClass2_init__Ljava_lang_String_I_V(lA2, (N.I(lI3 * lI1)));
					lA4 = fA0;
					lI1 = (N.I(lI1 + (lA4 as Benchmark_MyClass2)._b));
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_36([int CLASS_ID = 775]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_MyClass2 extends java_lang_Object  {

	int _a = 0;
	java_lang_String _c = null;
	java_lang_String _d = null;
	int _b = 0;
	 Benchmark_MyClass2 Benchmark_MyClass2_init__Ljava_lang_String_I_V(java_lang_String p0, int p1) {
		this.java_lang_Object_init___V();
		this._a = 10;
		this._b = 20;
		this._c = Bootstrap.STRINGLIT_41;
		this._d = p0;
		this._b = p1;
		return this;
		return this;
	}
	Benchmark_MyClass2([int CLASS_ID = 774]) : super(CLASS_ID) { }
	static void SI() { }
}
class Benchmark_37 extends java_lang_Object implements Benchmark_Task {

	JA_L _val_objects = null;
	 Benchmark_37 Benchmark_37_init___LBenchmark_MyClass2__V(JA_L p0) {
		this._val_objects = p0;
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 int run__I() {
		int G = 0;
		java_lang_Object lA4 = null;
		int lI1 = 0;
		int lI3 = 0;
		java_lang_Object fA0 = null;
		java_lang_String lA2 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = 0;
					lA2 = Bootstrap.STRINGLIT_119;
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= 100000))) {
						G = 2;
						continue;
					}
					tA0 = (new Benchmark_MyClass2());
					fA0 = tA0;
					(tA0 as Benchmark_MyClass2).Benchmark_MyClass2_init__Ljava_lang_String_I_V(lA2, (N.I(lI3 * lI1)));
					lA4 = fA0;
					(this._val_objects as JA_L).data[lI3] = lA4;
					lI1 = (N.I(lI1 + (lA4 as Benchmark_MyClass2)._b));
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	Benchmark_37([int CLASS_ID = 772]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class com_jtransc_async_JTranscAsyncHandler   {

	 void complete_Ljava_lang_Object_Ljava_lang_Throwable__V(java_lang_Object p0, java_lang_Throwable p1);
}
class com_jtransc_async_JTranscAsyncHandler_IFields {

	static void SI() { }
}
class java_lang_Error extends java_lang_Throwable  {

	 java_lang_Error java_lang_Error_init__Ljava_lang_Throwable__V(java_lang_Throwable p0) {
		this.java_lang_Throwable_init__Ljava_lang_Throwable__V(p0);
		return this;
		return this;
	}
	 java_lang_Error java_lang_Error_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_Throwable_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	java_lang_Error([int CLASS_ID = 752]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_AssertionError extends java_lang_Error  {

	 java_lang_AssertionError java_lang_AssertionError_init__Ljava_lang_Object__V(java_lang_Object p0) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					this.java_lang_Error_init__Ljava_lang_String__V(java_lang_String.valueOf_Ljava_lang_Object__Ljava_lang_String_(p0));
					if (!(((p0) is java_lang_Throwable))) {
						G = 1;
						continue;
					}
					this.initCause_Ljava_lang_Throwable__Ljava_lang_Throwable_(((p0) as java_lang_Throwable));
					G = 1;
					continue;
				case 1:
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	java_lang_AssertionError([int CLASS_ID = 770]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_Arrays_ArrayList extends java_util_AbstractList implements java_util_List, java_io_Serializable, java_util_RandomAccess {

	JA_L _a = null;
	 java_util_Arrays_ArrayList java_util_Arrays_ArrayList_init___Ljava_lang_Object__V(JA_L p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					this.java_util_AbstractList_init___V();
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_NullPointerException());
					fA0 = tA0;
					(tA0 as java_lang_NullPointerException).java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_145);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					this._a = p0;
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 int indexOf_Ljava_lang_Object__I(java_lang_Object p0) {
		int G = 0;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 == null))) {
						G = 1;
						continue;
					}
					lI2 = 0;
					G = 2;
					continue;
				case 2:
					if (((lI2 >= (this._a as JA_0).length))) {
						G = 3;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z((this._a).data[lI2]))) {
						G = 4;
						continue;
					}
					return lI2;
				case 4:
					lI2 = (N.I(lI2 + 1));
					G = 2;
					continue;
				case 3:
					G = 5;
					continue;
				case 1:
					lI2 = 0;
					G = 6;
					continue;
				case 6:
					if (((lI2 >= (this._a as JA_0).length))) {
						G = 5;
						continue;
					}
					if ((((this._a).data[lI2] != null))) {
						G = 7;
						continue;
					}
					return lI2;
				case 7:
					lI2 = (N.I(lI2 + 1));
					G = 6;
					continue;
				case 5:
					return -1;
				default:
					break;
			}
		}
		return 0;
	}
	 java_lang_Object get_I_Ljava_lang_Object_(int p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							G = 1;
							continue;
						case 1:
							fA0 = (this._a).data[p0];
							G = 2;
							continue;
						case 2: return fA0;
						case 3:
							fA0 = J__exception__;
							tA0 = (new java_lang_IndexOutOfBoundsException());
							fA0 = tA0;
							(tA0 as java_lang_IndexOutOfBoundsException).java_lang_IndexOutOfBoundsException_init___V();
							throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
							break;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_ArrayIndexOutOfBoundsException)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	 int size__I() {
		return (this._a as JA_0).length;
	}
	 bool contains_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		java_lang_Object lA5 = null;
		int lI3 = 0;
		int lI4 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 == null))) {
						G = 1;
						continue;
					}
					lA2 = this._a;
					lI3 = (lA2 as JA_0).length;
					lI4 = 0;
					G = 2;
					continue;
				case 2:
					if (((lI4 >= lI3))) {
						G = 3;
						continue;
					}
					lA5 = ((lA2 as JA_L)).data[lI4];
					if (!(p0.equals_Ljava_lang_Object__Z(lA5))) {
						G = 4;
						continue;
					}
					return true;
				case 4:
					lI4 = (N.I(lI4 + 1));
					G = 2;
					continue;
				case 3:
					G = 5;
					continue;
				case 1:
					lA2 = this._a;
					lI3 = (lA2 as JA_0).length;
					lI4 = 0;
					G = 6;
					continue;
				case 6:
					if (((lI4 >= lI3))) {
						G = 5;
						continue;
					}
					lA5 = ((lA2 as JA_L)).data[lI4];
					if (((lA5 != null))) {
						G = 7;
						continue;
					}
					return true;
				case 7:
					lI4 = (N.I(lI4 + 1));
					G = 6;
					continue;
				case 5:
					return false;
				default:
					break;
			}
		}
		return false;
	}
	 JA_L toArray__Ljava_lang_Object___Ljava_lang_Object_(JA_L p0) {
		int G = 0;
		java_lang_Object lA1 = null;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					lA1 = p0;
					lI2 = this.size__I();
					if (((lI2 <= (lA1 as JA_0).length))) {
						G = 1;
						continue;
					}
					lA1 = java_util_Arrays.access_000__Ljava_lang_Object_I__Ljava_lang_Object_((lA1 as JA_L), lI2);
					G = 1;
					continue;
				case 1:
					java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(this._a, 0, lA1, 0, lI2);
					if (((lI2 >= (lA1 as JA_0).length))) {
						G = 2;
						continue;
					}
					(lA1 as JA_L).data[lI2] = null;
					G = 2;
					continue;
				case 2:
					return (lA1 as JA_L);
				default:
					break;
			}
		}
		return null;
	}
	java_util_Arrays_ArrayList([int CLASS_ID = 767]) : super(CLASS_ID) { }
	static void SI() { }
}
class j_ProgramReflection_AllConstructors extends java_lang_Object  {

	 j_ProgramReflection_AllConstructors j_ProgramReflection_AllConstructors_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static JA_L getConstructors_I__Lj_MemberInfo_(int p0) {
		if (((p0 >= 863))) {
			return j_ProgramReflection_AllConstructors.getConstructors2_I__Lj_MemberInfo_(p0);
		}
		if (((p0 >= 760))) {
			return j_ProgramReflection_AllConstructors.getConstructors1_I__Lj_MemberInfo_(p0);
		}
		if (((p0 >= 655))) {
			return j_ProgramReflection_AllConstructors.getConstructors0_I__Lj_MemberInfo_(p0);
		}
		return null;
	}
	static JA_L getConstructors0_I__Lj_MemberInfo_(int p0) {
		JA_L _out = null;
		switch (p0) {
			case 655:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7216, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 656:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7217, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 657:
				_out = new JA_L(3, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7280, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_148, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7853, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_149, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7854, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_150, null);
				return _out;
			case 661:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7222, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_151, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7223, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 664:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7227, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7228, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_152, null);
				return _out;
			case 666:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7242, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_153, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7556, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_154, null);
				return _out;
			case 671:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7230, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 672:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7239, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_152, null);
				return _out;
			case 673:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7234, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 674:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7598, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_155, Bootstrap.STRINGLIT_156);
				return _out;
			case 676:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7599, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_157, null);
				return _out;
			case 678:
				_out = new JA_L(3, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7295, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7301, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_158, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7316, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 679:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7294, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7296, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_158, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7315, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7317, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_159, null);
				return _out;
			case 680:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7291, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7297, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_158, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7313, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7318, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_159, null);
				return _out;
			case 681:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7292, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7298, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_158, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7314, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7319, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_159, null);
				return _out;
			case 682:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7264, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 683:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7267, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_160, null);
				return _out;
			case 684:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7271, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 685:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7273, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 686:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7275, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_161, null);
				return _out;
			case 687:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7281, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 688:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7284, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 689:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7288, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_162, null);
				return _out;
			case 692:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7286, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_162, null);
				return _out;
			case 693:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7287, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 695:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7289, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7300, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_158, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7321, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_159, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7323, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 696:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7290, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7299, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_158, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7312, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7320, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_159, null);
				return _out;
			case 697:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7302, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 698:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7303, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 699:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7304, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_163, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7308, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_164, null);
				return _out;
			case 700:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7305, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 701:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7306, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_163, null);
				return _out;
			case 702:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7307, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 703:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7325, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7661, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				return _out;
			case 704:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7324, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7660, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				return _out;
			case 705:
				_out = new JA_L(3, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7311, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7322, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_159, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7862, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				return _out;
			case 706:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7329, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_165, null);
				return _out;
			case 708:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7348, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7871, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_152, null);
				return _out;
			case 710:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7347, null, Bootstrap.STRINGLIT_146, 4, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 713:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7349, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_166, Bootstrap.STRINGLIT_167);
				return _out;
			case 719:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7386, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_168, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7387, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_169, null);
				return _out;
			case 720:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7357, null, Bootstrap.STRINGLIT_146, 4, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 721:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7358, null, Bootstrap.STRINGLIT_146, 4, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 722:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7376, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_169, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7377, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_168, null);
				return _out;
			case 723:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7375, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_169, null);
				return _out;
			case 724:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7382, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 725:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7383, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 726:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7400, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_170, null);
				return _out;
			case 727:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7405, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 728:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7410, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_171, null);
				return _out;
			case 729:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7413, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 730:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7417, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_163, null);
				return _out;
			case 731:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7421, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_172, null);
				return _out;
			case 732:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7427, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 733:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7757, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 734:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7476, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 735:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7494, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 736:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7496, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 737:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7498, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_173, null);
				return _out;
			case 738:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7503, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 739:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7514, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 740:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7568, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_174, null);
				return _out;
			case 741:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7582, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 742:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7606, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				return _out;
			case 743:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7586, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 744:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7594, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_175, null);
				return _out;
			case 745:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7601, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 746:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7731, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				return _out;
			case 747:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7722, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				return _out;
			case 748:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7610, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_155, Bootstrap.STRINGLIT_156);
				return _out;
			case 749:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7611, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_155, Bootstrap.STRINGLIT_156);
				return _out;
			case 752:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7671, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_159, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7694, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				return _out;
			case 754:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7636, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7637, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_152, null);
				return _out;
			case 757:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7635, null, Bootstrap.STRINGLIT_146, 4, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 759:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7653, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_176, null);
				return _out;
			default:
				break;
		}
		return null;
	}
	static JA_L getConstructors1_I__Lj_MemberInfo_(int p0) {
		JA_L _out = null;
		switch (p0) {
			case 760:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7654, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_177, null);
				return _out;
			case 761:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7670, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_159, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7695, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				return _out;
			case 765:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7720, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 766:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7729, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 767:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7733, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_178, Bootstrap.STRINGLIT_179);
				return _out;
			case 770:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7746, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_180, null);
				return _out;
			case 772:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7782, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_181, null);
				return _out;
			case 774:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7783, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_182, null);
				return _out;
			case 775:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7784, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 776:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7785, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 777:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7786, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 778:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7787, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 779:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7789, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_183, null);
				return _out;
			case 780:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7790, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 781:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8180, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_170, null);
				return _out;
			case 782:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7791, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 783:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7792, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 784:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7793, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 785:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7794, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 786:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7795, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 787:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7796, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_183, null);
				return _out;
			case 788:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7797, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 789:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7798, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 790:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7799, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 791:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7800, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 792:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7801, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 793:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7802, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 794:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7803, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 795:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7804, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_183, null);
				return _out;
			case 796:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7805, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 797:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7806, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_183, null);
				return _out;
			case 798:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7807, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 799:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7808, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 800:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7809, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 801:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7810, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_184, null);
				return _out;
			case 802:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7811, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 803:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7812, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 804:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7813, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 805:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7814, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_185, null);
				return _out;
			case 806:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7815, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_148, null);
				return _out;
			case 807:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7816, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_186, null);
				return _out;
			case 808:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7817, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_183, null);
				return _out;
			case 809:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7818, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 810:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7819, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 811:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7820, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 812:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7821, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 813:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7822, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 814:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7823, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 815:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7824, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 816:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7825, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 817:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7826, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 818:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7827, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 819:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7828, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_187, null);
				return _out;
			case 820:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7829, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_188, null);
				return _out;
			case 821:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7830, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 822:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7835, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 823:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7837, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_189, null);
				return _out;
			case 824:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7838, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_189, null);
				return _out;
			case 826:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7849, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_190, null);
				return _out;
			case 827:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7939, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_152, null);
				return _out;
			case 828:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7861, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				return _out;
			case 829:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7866, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_191, Bootstrap.STRINGLIT_192);
				return _out;
			case 830:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7869, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 831:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7870, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_193, null);
				return _out;
			case 832:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7872, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 833:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7874, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 834:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7888, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_194, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7889, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 835:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7880, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 837:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7890, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_194, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7891, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 838:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7892, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_194, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7893, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 839:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7896, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 840:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7898, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 841:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7899, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 842:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7900, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_195, null);
				return _out;
			case 844:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7907, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 845:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7908, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 846:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7909, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 847:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7911, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 848:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7912, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 849:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7913, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 850:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7929, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 854:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7934, null, Bootstrap.STRINGLIT_146, 4, Bootstrap.STRINGLIT_147, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7935, null, Bootstrap.STRINGLIT_146, 4, Bootstrap.STRINGLIT_196, null);
				return _out;
			case 855:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7931, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 856:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7933, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 857:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7962, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 858:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7970, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 859:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7980, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_154, null);
				return _out;
			case 861:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7983, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_152, null);
				return _out;
			case 862:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7985, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_197, null);
				return _out;
			default:
				break;
		}
		return null;
	}
	static JA_L getConstructors2_I__Lj_MemberInfo_(int p0) {
		JA_L _out = null;
		switch (p0) {
			case 863:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7984, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 864:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7992, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_152, null);
				return _out;
			case 865:
				_out = new JA_L(3, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8039, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_198, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8131, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_199, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8170, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_183, null);
				return _out;
			case 866:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8005, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 867:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8007, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_152, null);
				return _out;
			case 869:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8014, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_152, null);
				return _out;
			case 870:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8018, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_152, null);
				return _out;
			case 871:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8022, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_152, null);
				return _out;
			case 872:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8027, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 874:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8034, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_153, null);
				return _out;
			case 875:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8043, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 876:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8053, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 877:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8064, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_201, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8065, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 878:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8063, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 880:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8068, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 881:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8077, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_202, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8078, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 882:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8076, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 884:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8081, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 885:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8085, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 886:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8096, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_203, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8097, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 887:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8095, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 889:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8100, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 890:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8108, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_204, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8109, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 891:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8107, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 893:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8112, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 894:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8116, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 895:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8121, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 896:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8126, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_200, null);
				return _out;
			case 897:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8134, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 906:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8191, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_205, null);
				return _out;
			case 910:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8262, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_206, null);
				return _out;
			case 911:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8271, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_207, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8272, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 912:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8273, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_207, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8274, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 919:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8374, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 920:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8367, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 921:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8366, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 922:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8368, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_208, Bootstrap.STRINGLIT_209);
				return _out;
			case 924:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8410, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_210, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8411, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_211, null);
				return _out;
			case 925:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8399, null, Bootstrap.STRINGLIT_146, 2, Bootstrap.STRINGLIT_211, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8404, null, Bootstrap.STRINGLIT_146, 4096, Bootstrap.STRINGLIT_210, null);
				return _out;
			case 926:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8396, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_211, null);
				return _out;
			case 927:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8422, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_212, Bootstrap.STRINGLIT_213);
				return _out;
			case 928:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8423, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_180, Bootstrap.STRINGLIT_214);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8427, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_215, Bootstrap.STRINGLIT_216);
				return _out;
			case 929:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8424, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_180, Bootstrap.STRINGLIT_214);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8426, null, Bootstrap.STRINGLIT_146, 0, Bootstrap.STRINGLIT_215, Bootstrap.STRINGLIT_216);
				return _out;
			case 930:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8425, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 931:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8457, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			case 932:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8461, null, Bootstrap.STRINGLIT_146, 1, Bootstrap.STRINGLIT_147, null);
				return _out;
			default:
				break;
		}
		return null;
	}
	j_ProgramReflection_AllConstructors([int CLASS_ID = 766]) : super(CLASS_ID) { }
	static void SI() { }
}
class j_ProgramReflection_DynamicNewInvoke extends java_lang_Object  {

	 j_ProgramReflection_DynamicNewInvoke j_ProgramReflection_DynamicNewInvoke_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static java_lang_Object dynamicNew_II_Ljava_lang_Object__Ljava_lang_Object_(int p0, int p1, JA_L p2) {
		if (((p1 >= 8126))) {
			return j_ProgramReflection_DynamicNewInvoke.dynamicNew2_II_Ljava_lang_Object__Ljava_lang_Object_(p0, p1, p2);
		}
		if (((p1 >= 7783))) {
			return j_ProgramReflection_DynamicNewInvoke.dynamicNew1_II_Ljava_lang_Object__Ljava_lang_Object_(p0, p1, p2);
		}
		if (((p1 >= 7216))) {
			return j_ProgramReflection_DynamicNewInvoke.dynamicNew0_II_Ljava_lang_Object__Ljava_lang_Object_(p0, p1, p2);
		}
		return null;
	}
	static java_lang_Object dynamicNew0_II_Ljava_lang_Object__Ljava_lang_Object_(int p0, int p1, JA_L p2) {
		switch (p1) {
			case 7216: return (new Benchmark()).Benchmark_init___V();
			case 7217: return (new java_lang_Object()).java_lang_Object_init___V();
			case 7222: return (new java_lang_String_CaseInsensitiveComparator()).java_lang_String_CaseInsensitiveComparator_init__Ljava_lang_String_1__V(((p2).data[0] as java_lang_String_1));
			case 7223: return (new java_lang_String_CaseInsensitiveComparator()).java_lang_String_CaseInsensitiveComparator_init___V();
			case 7227: return (new java_lang_StringBuilder()).java_lang_StringBuilder_init___V();
			case 7228: return (new java_lang_StringBuilder()).java_lang_StringBuilder_init__I_V(N.unboxInt(((p2).data[0] as java_lang_Integer)));
			case 7230: return (new java_lang_reflect_Modifier()).java_lang_reflect_Modifier_init___V();
			case 7239: return (new java_lang_Integer()).java_lang_Integer_init__I_V(N.unboxInt(((p2).data[0] as java_lang_Integer)));
			case 7242: return (new java_lang_Class()).java_lang_Class_init__Ljava_lang_String_Z_V(((p2).data[0] as java_lang_String), N.unboxBool(((p2).data[1] as java_lang_Boolean)));
			case 7264: return (new java_lang_Void()).java_lang_Void_init___V();
			case 7267: return (new java_lang_Float()).java_lang_Float_init__F_V(N.unboxFloat(((p2).data[0] as java_lang_Float)));
			case 7271: return (new com_jtransc_text_JTranscStringTools()).com_jtransc_text_JTranscStringTools_init___V();
			case 7273: return (new java_lang_Math()).java_lang_Math_init___V();
			case 7275: return (new java_lang_Character()).java_lang_Character_init__C_V(N.unboxChar(((p2).data[0] as java_lang_Character)));
			case 7280: return (new java_lang_String()).java_lang_String_init___C_V(((p2).data[0] as JA_C));
			case 7281: return (new java_util_Arrays()).java_util_Arrays_init___V();
			case 7284: return (new java_lang_System()).java_lang_System_init___V();
			case 7286: return (new java_io_FilterOutputStream()).java_io_FilterOutputStream_init__Ljava_io_OutputStream__V(((p2).data[0] as java_io_OutputStream));
			case 7288: return (new java_io_PrintStream()).java_io_PrintStream_init__Ljava_io_OutputStream__V(((p2).data[0] as java_io_OutputStream));
			case 7289: return (new java_lang_NullPointerException()).java_lang_NullPointerException_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7290: return (new java_lang_RuntimeException()).java_lang_RuntimeException_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7291: return (new java_lang_Exception()).java_lang_Exception_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7292: return (new java_lang_Throwable()).java_lang_Throwable_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7294: return (new java_lang_ReflectiveOperationException()).java_lang_ReflectiveOperationException_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7295: return (new java_lang_ClassNotFoundException()).java_lang_ClassNotFoundException_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7296: return (new java_lang_ReflectiveOperationException()).java_lang_ReflectiveOperationException_init__Ljava_lang_String_Ljava_lang_Throwable__V(((p2).data[0] as java_lang_String), ((p2).data[1] as java_lang_Throwable));
			case 7297: return (new java_lang_Exception()).java_lang_Exception_init__Ljava_lang_String_Ljava_lang_Throwable__V(((p2).data[0] as java_lang_String), ((p2).data[1] as java_lang_Throwable));
			case 7298: return (new java_lang_Throwable()).java_lang_Throwable_init__Ljava_lang_String_Ljava_lang_Throwable__V(((p2).data[0] as java_lang_String), ((p2).data[1] as java_lang_Throwable));
			case 7299: return (new java_lang_RuntimeException()).java_lang_RuntimeException_init__Ljava_lang_String_Ljava_lang_Throwable__V(((p2).data[0] as java_lang_String), ((p2).data[1] as java_lang_Throwable));
			case 7300: return (new java_lang_NullPointerException()).java_lang_NullPointerException_init__Ljava_lang_String_Ljava_lang_Throwable__V(((p2).data[0] as java_lang_String), ((p2).data[1] as java_lang_Throwable));
			case 7301: return (new java_lang_ClassNotFoundException()).java_lang_ClassNotFoundException_init__Ljava_lang_String_Ljava_lang_Throwable__V(((p2).data[0] as java_lang_String), ((p2).data[1] as java_lang_Throwable));
			case 7302: return (new java_lang_System_1()).java_lang_System_1_init___V();
			case 7304: return (new com_jtransc_io_JTranscConsolePrintStream()).com_jtransc_io_JTranscConsolePrintStream_init__Z_V(N.unboxBool(((p2).data[0] as java_lang_Boolean)));
			case 7305: return (new com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream()).com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream_init___V();
			case 7307: return (new com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream()).com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream_init___V();
			case 7308: return (new com_jtransc_io_JTranscConsolePrintStream()).com_jtransc_io_JTranscConsolePrintStream_init__Lcom_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_Z_V(((p2).data[0] as com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream), N.unboxBool(((p2).data[1] as java_lang_Boolean)));
			case 7311: return (new java_lang_IllegalArgumentException()).java_lang_IllegalArgumentException_init___V();
			case 7312: return (new java_lang_RuntimeException()).java_lang_RuntimeException_init___V();
			case 7313: return (new java_lang_Exception()).java_lang_Exception_init___V();
			case 7314: return (new java_lang_Throwable()).java_lang_Throwable_init___V();
			case 7315: return (new java_lang_ReflectiveOperationException()).java_lang_ReflectiveOperationException_init___V();
			case 7316: return (new java_lang_ClassNotFoundException()).java_lang_ClassNotFoundException_init___V();
			case 7317: return (new java_lang_ReflectiveOperationException()).java_lang_ReflectiveOperationException_init__Ljava_lang_Throwable__V(((p2).data[0] as java_lang_Throwable));
			case 7318: return (new java_lang_Exception()).java_lang_Exception_init__Ljava_lang_Throwable__V(((p2).data[0] as java_lang_Throwable));
			case 7319: return (new java_lang_Throwable()).java_lang_Throwable_init__Ljava_lang_Throwable__V(((p2).data[0] as java_lang_Throwable));
			case 7320: return (new java_lang_RuntimeException()).java_lang_RuntimeException_init__Ljava_lang_Throwable__V(((p2).data[0] as java_lang_Throwable));
			case 7321: return (new java_lang_NullPointerException()).java_lang_NullPointerException_init__Ljava_lang_Throwable__V(((p2).data[0] as java_lang_Throwable));
			case 7322: return (new java_lang_IllegalArgumentException()).java_lang_IllegalArgumentException_init__Ljava_lang_Throwable__V(((p2).data[0] as java_lang_Throwable));
			case 7323: return (new java_lang_NullPointerException()).java_lang_NullPointerException_init___V();
			case 7324: return (new java_lang_IndexOutOfBoundsException()).java_lang_IndexOutOfBoundsException_init___V();
			case 7325: return (new java_lang_ArrayIndexOutOfBoundsException()).java_lang_ArrayIndexOutOfBoundsException_init___V();
			case 7329: return (new java_lang_Double()).java_lang_Double_init__D_V(N.unboxDouble(((p2).data[0] as java_lang_Double)));
			case 7348: return (new java_util_HashMap()).java_util_HashMap_init___V();
			case 7349: return (new java_util_HashMap_HashMapEntry()).java_util_HashMap_HashMapEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__V((p2).data[0], (p2).data[1], N.unboxInt(((p2).data[2] as java_lang_Integer)), ((p2).data[3] as java_util_HashMap_HashMapEntry));
			case 7376: return (new java_util_HashMap_EntryIterator()).java_util_HashMap_EntryIterator_init__Ljava_util_HashMap__V(((p2).data[0] as java_util_HashMap));
			case 7377: return (new java_util_HashMap_EntryIterator()).java_util_HashMap_EntryIterator_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V(((p2).data[0] as java_util_HashMap), ((p2).data[1] as java_util_HashMap_1));
			case 7382: return (new java_util_NoSuchElementException()).java_util_NoSuchElementException_init___V();
			case 7383: return (new java_util_ConcurrentModificationException()).java_util_ConcurrentModificationException_init___V();
			case 7386: return (new java_util_HashMap_EntrySet()).java_util_HashMap_EntrySet_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V(((p2).data[0] as java_util_HashMap), ((p2).data[1] as java_util_HashMap_1));
			case 7387: return (new java_util_HashMap_EntrySet()).java_util_HashMap_EntrySet_init__Ljava_util_HashMap__V(((p2).data[0] as java_util_HashMap));
			case 7400: return (new java_lang_Long()).java_lang_Long_init__J_V(N.unboxLong(((p2).data[0] as java_lang_Long)));
			case 7405: return (new com_jtransc_internal_JTranscCType()).com_jtransc_internal_JTranscCType_init___V();
			case 7410: return (new java_lang_Short()).java_lang_Short_init__S_V(N.unboxShort(((p2).data[0] as java_lang_Short)));
			case 7413: return (new com_jtransc_io_JTranscConsole()).com_jtransc_io_JTranscConsole_init___V();
			case 7417: return (new java_lang_Boolean()).java_lang_Boolean_init__Z_V(N.unboxBool(((p2).data[0] as java_lang_Boolean)));
			case 7421: return (new java_lang_Byte()).java_lang_Byte_init__B_V(N.unboxByte(((p2).data[0] as java_lang_Byte)));
			case 7427: return (new java_lang_SystemInt()).java_lang_SystemInt_init___V();
			case 7476: return (new java_util_Objects()).java_util_Objects_init___V();
			case 7494: return (new java_lang_jtransc_JTranscCoreReflection()).java_lang_jtransc_JTranscCoreReflection_init___V();
			case 7496: return (new j_ProgramReflection()).j_ProgramReflection_init___V();
			case 7498: return (new j_ClassInfo()).j_ClassInfo_init__ILjava_lang_String_Ljava_lang_String_II_I_I_V(N.unboxInt(((p2).data[0] as java_lang_Integer)), ((p2).data[1] as java_lang_String), ((p2).data[2] as java_lang_String), N.unboxInt(((p2).data[3] as java_lang_Integer)), N.unboxInt(((p2).data[4] as java_lang_Integer)), ((p2).data[5] as JA_I), ((p2).data[6] as JA_I));
			case 7503: return (new j_ProgramReflection_AllClasses()).j_ProgramReflection_AllClasses_init___V();
			case 7514: return (new java_lang_UnsupportedOperationException()).java_lang_UnsupportedOperationException_init___V();
			case 7556: return (new java_lang_Class()).java_lang_Class_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7568: return (new java_lang_StackTraceElement()).java_lang_StackTraceElement_init__Ljava_lang_String_Ljava_lang_String_Ljava_lang_String_I_V(((p2).data[0] as java_lang_String), ((p2).data[1] as java_lang_String), ((p2).data[2] as java_lang_String), N.unboxInt(((p2).data[3] as java_lang_Integer)));
			case 7582: return (new java_lang_reflect_Array()).java_lang_reflect_Array_init___V();
			case 7586: return (new j_ProgramReflection_DynamicGetSet()).j_ProgramReflection_DynamicGetSet_init___V();
			case 7594: return (new j_MemberInfo()).j_MemberInfo_init__ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__V(N.unboxInt(((p2).data[0] as java_lang_Integer)), ((p2).data[1] as java_lang_String), ((p2).data[2] as java_lang_String), N.unboxInt(((p2).data[3] as java_lang_Integer)), ((p2).data[4] as java_lang_String), ((p2).data[5] as java_lang_String));
			case 7598: return (new java_lang_reflect_Field()).java_lang_reflect_Field_init__Ljava_lang_Class_Lj_MemberInfo__V(((p2).data[0] as java_lang_Class), ((p2).data[1] as j_MemberInfo));
			case 7601: return (new j_ProgramReflection_AllFields()).j_ProgramReflection_AllFields_init___V();
			case 7606: return (new java_lang_CloneNotSupportedException()).java_lang_CloneNotSupportedException_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7610: return (new java_lang_reflect_Constructor()).java_lang_reflect_Constructor_init__Ljava_lang_Class_Lj_MemberInfo__V(((p2).data[0] as java_lang_Class), ((p2).data[1] as j_MemberInfo));
			case 7636: return (new java_util_ArrayList()).java_util_ArrayList_init___V();
			case 7637: return (new java_util_ArrayList()).java_util_ArrayList_init__I_V(N.unboxInt(((p2).data[0] as java_lang_Integer)));
			case 7653: return (new java_util_AbstractList_FullListIterator()).java_util_AbstractList_FullListIterator_init__Ljava_util_AbstractList_I_V(((p2).data[0] as java_util_AbstractList), N.unboxInt(((p2).data[1] as java_lang_Integer)));
			case 7654: return (new java_util_AbstractList_SimpleListIterator()).java_util_AbstractList_SimpleListIterator_init__Ljava_util_AbstractList__V(((p2).data[0] as java_util_AbstractList));
			case 7660: return (new java_lang_IndexOutOfBoundsException()).java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7661: return (new java_lang_ArrayIndexOutOfBoundsException()).java_lang_ArrayIndexOutOfBoundsException_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7670: return (new java_lang_InternalError()).java_lang_InternalError_init__Ljava_lang_Throwable__V(((p2).data[0] as java_lang_Throwable));
			case 7671: return (new java_lang_Error()).java_lang_Error_init__Ljava_lang_Throwable__V(((p2).data[0] as java_lang_Throwable));
			case 7694: return (new java_lang_Error()).java_lang_Error_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7695: return (new java_lang_InternalError()).java_lang_InternalError_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7720: return (new j_ProgramReflection_DynamicNewInvoke()).j_ProgramReflection_DynamicNewInvoke_init___V();
			case 7722: return (new java_lang_InstantiationException()).java_lang_InstantiationException_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7729: return (new j_ProgramReflection_AllConstructors()).j_ProgramReflection_AllConstructors_init___V();
			case 7731: return (new java_lang_NoSuchMethodException()).java_lang_NoSuchMethodException_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7733: return (new java_util_Arrays_ArrayList()).java_util_Arrays_ArrayList_init___Ljava_lang_Object__V(((p2).data[0] as JA_L));
			case 7746: return (new java_lang_AssertionError()).java_lang_AssertionError_init__Ljava_lang_Object__V((p2).data[0]);
			case 7757: return (new java_lang_ClassCastException()).java_lang_ClassCastException_init___V();
			case 7782: return (new Benchmark_37()).Benchmark_37_init___LBenchmark_MyClass2__V(((p2).data[0] as JA_L));
			default:
				break;
		}
		return null;
	}
	static java_lang_Object dynamicNew1_II_Ljava_lang_Object__Ljava_lang_Object_(int p0, int p1, JA_L p2) {
		switch (p1) {
			case 7783: return (new Benchmark_MyClass2()).Benchmark_MyClass2_init__Ljava_lang_String_I_V(((p2).data[0] as java_lang_String), N.unboxInt(((p2).data[1] as java_lang_Integer)));
			case 7784: return (new Benchmark_36()).Benchmark_36_init___V();
			case 7785: return (new Benchmark_35()).Benchmark_35_init___V();
			case 7786: return (new Benchmark_34()).Benchmark_34_init___V();
			case 7787: return (new com_jtransc_JTranscSystem()).com_jtransc_JTranscSystem_init___V();
			case 7789: return (new Benchmark_39()).Benchmark_39_init___B_V(((p2).data[0] as JA_B));
			case 7790: return (new Benchmark_38()).Benchmark_38_init___V();
			case 7791: return (new Benchmark_33()).Benchmark_33_init___V();
			case 7792: return (new Benchmark_32()).Benchmark_32_init___V();
			case 7793: return (new Benchmark_31()).Benchmark_31_init___V();
			case 7794: return (new Benchmark_30()).Benchmark_30_init___V();
			case 7795: return (new java_lang_Runtime()).java_lang_Runtime_init___V();
			case 7796: return (new Benchmark_40()).Benchmark_40_init___B_V(((p2).data[0] as JA_B));
			case 7797: return (new Benchmark_1()).Benchmark_1_init___V();
			case 7798: return (new Benchmark_2()).Benchmark_2_init___V();
			case 7799: return (new Benchmark_3()).Benchmark_3_init___V();
			case 7800: return (new Benchmark_44()).Benchmark_44_init___V();
			case 7801: return (new Benchmark_4()).Benchmark_4_init___V();
			case 7802: return (new Benchmark_43()).Benchmark_43_init___V();
			case 7803: return (new Benchmark_5()).Benchmark_5_init___V();
			case 7804: return (new Benchmark_42()).Benchmark_42_init___B_V(((p2).data[0] as JA_B));
			case 7805: return (new Benchmark_6()).Benchmark_6_init___V();
			case 7806: return (new Benchmark_41()).Benchmark_41_init___B_V(((p2).data[0] as JA_B));
			case 7807: return (new Benchmark_7()).Benchmark_7_init___V();
			case 7808: return (new Benchmark_8()).Benchmark_8_init___V();
			case 7809: return (new Benchmark_9()).Benchmark_9_init___V();
			case 7810: return (new Benchmark_15()).Benchmark_15_init___I_I_V(((p2).data[0] as JA_I), ((p2).data[1] as JA_I));
			case 7811: return (new Benchmark_14()).Benchmark_14_init___V();
			case 7812: return (new Benchmark_13()).Benchmark_13_init___V();
			case 7813: return (new Benchmark_12()).Benchmark_12_init___V();
			case 7814: return (new Benchmark_19()).Benchmark_19_init___I_V(((p2).data[0] as JA_I));
			case 7815: return (new Benchmark_18()).Benchmark_18_init___C_V(((p2).data[0] as JA_C));
			case 7816: return (new Benchmark_17()).Benchmark_17_init___S_V(((p2).data[0] as JA_S));
			case 7817: return (new Benchmark_16()).Benchmark_16_init___B_V(((p2).data[0] as JA_B));
			case 7818: return (new Benchmark_11()).Benchmark_11_init___V();
			case 7819: return (new Benchmark_10()).Benchmark_10_init___V();
			case 7820: return (new Benchmark_26()).Benchmark_26_init___V();
			case 7821: return (new Benchmark_25()).Benchmark_25_init___V();
			case 7822: return (new Benchmark_24()).Benchmark_24_init___V();
			case 7823: return (new Benchmark_23()).Benchmark_23_init___V();
			case 7824: return (new Benchmark_29()).Benchmark_29_init___V();
			case 7825: return (new Benchmark_28()).Benchmark_28_init___V();
			case 7826: return (new Benchmark_27()).Benchmark_27_init___V();
			case 7827: return (new Benchmark_22()).Benchmark_22_init___V();
			case 7828: return (new Benchmark_21()).Benchmark_21_init___D_V(((p2).data[0] as JA_D));
			case 7829: return (new Benchmark_20()).Benchmark_20_init___F_V(((p2).data[0] as JA_F));
			case 7830: return (new com_jtransc_JTranscVersion()).com_jtransc_JTranscVersion_init___V();
			case 7835: return (new com_jtransc_time_JTranscClock()).com_jtransc_time_JTranscClock_init___V();
			case 7837: return (new com_jtransc_time_JTranscClock_1()).com_jtransc_time_JTranscClock_1_init__Lcom_jtransc_time_JTranscClock_Impl__V(((p2).data[0] as com_jtransc_time_JTranscClock_Impl));
			case 7838: return (new com_jtransc_time_JTranscClock_Impl()).com_jtransc_time_JTranscClock_Impl_init__Lcom_jtransc_time_JTranscClock_Impl__V(((p2).data[0] as com_jtransc_time_JTranscClock_Impl));
			case 7853: return (new java_lang_String()).java_lang_String_init___BII_V(((p2).data[0] as JA_B), N.unboxInt(((p2).data[1] as java_lang_Integer)), N.unboxInt(((p2).data[2] as java_lang_Integer)));
			case 7854: return (new java_lang_String()).java_lang_String_init___BIILjava_lang_String_Z_V(((p2).data[0] as JA_B), N.unboxInt(((p2).data[1] as java_lang_Integer)), N.unboxInt(((p2).data[2] as java_lang_Integer)), ((p2).data[3] as java_lang_String), N.unboxBool(((p2).data[4] as java_lang_Boolean)));
			case 7861: return (new java_nio_charset_UnsupportedCharsetException()).java_nio_charset_UnsupportedCharsetException_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7862: return (new java_lang_IllegalArgumentException()).java_lang_IllegalArgumentException_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7866: return (new java_util_ServiceLoader()).java_util_ServiceLoader_init__Ljava_lang_Class__V(((p2).data[0] as java_lang_Class));
			case 7869: return (new com_jtransc_charset_charsets_JTranscCharsetIBM866()).com_jtransc_charset_charsets_JTranscCharsetIBM866_init___V();
			case 7870: return (new com_jtransc_charset_JTranscCharsetSingleByte()).com_jtransc_charset_JTranscCharsetSingleByte_init___Ljava_lang_String_Ljava_lang_String__V(((p2).data[0] as JA_L), ((p2).data[1] as java_lang_String));
			case 7871: return (new java_util_HashMap()).java_util_HashMap_init__I_V(N.unboxInt(((p2).data[0] as java_lang_Integer)));
			case 7872: return (new java_util_Collections()).java_util_Collections_init___V();
			case 7874: return (new java_util_Collections_1()).java_util_Collections_1_init___V();
			case 7880: return (new java_util_Collections_2()).java_util_Collections_2_init___V();
			case 7888: return (new java_util_Collections_EmptyList()).java_util_Collections_EmptyList_init__Ljava_util_Collections_1__V(((p2).data[0] as java_util_Collections_1));
			case 7889: return (new java_util_Collections_EmptyList()).java_util_Collections_EmptyList_init___V();
			case 7890: return (new java_util_Collections_EmptyMap()).java_util_Collections_EmptyMap_init__Ljava_util_Collections_1__V(((p2).data[0] as java_util_Collections_1));
			case 7891: return (new java_util_Collections_EmptyMap()).java_util_Collections_EmptyMap_init___V();
			case 7892: return (new java_util_Collections_EmptySet()).java_util_Collections_EmptySet_init__Ljava_util_Collections_1__V(((p2).data[0] as java_util_Collections_1));
			case 7893: return (new java_util_Collections_EmptySet()).java_util_Collections_EmptySet_init___V();
			case 7896: return (new com_jtransc_charset_charsets_JTranscCharsetUTF8()).com_jtransc_charset_charsets_JTranscCharsetUTF8_init___V();
			case 7898: return (new com_jtransc_charset_charsets_JTranscCharsetUSASCII()).com_jtransc_charset_charsets_JTranscCharsetUSASCII_init___V();
			case 7899: return (new com_jtransc_charset_charsets_JTranscCharsetUTF16LE()).com_jtransc_charset_charsets_JTranscCharsetUTF16LE_init___V();
			case 7907: return (new com_jtransc_mix_JTranscProcessMulti()).com_jtransc_mix_JTranscProcessMulti_init___V();
			case 7911: return (new com_jtransc_mix_JTranscProcessMulti_Creator()).com_jtransc_mix_JTranscProcessMulti_Creator_init___V();
			case 7912: return (new com_jtransc_charset_charsets_JTranscCharsetLatin1()).com_jtransc_charset_charsets_JTranscCharsetLatin1_init___V();
			case 7913: return (new com_jtransc_charset_charsets_JTranscCharsetUTF16BE()).com_jtransc_charset_charsets_JTranscCharsetUTF16BE_init___V();
			case 7929: return (new java_lang_Thread()).java_lang_Thread_init___V();
			case 7931: return (new java_lang__ClassInternalUtils()).java_lang__ClassInternalUtils_init___V();
			case 7933: return (new java_lang__ClassInternalUtils_1()).java_lang__ClassInternalUtils_1_init___V();
			case 7939: return (new java_io_ByteArrayOutputStream()).java_io_ByteArrayOutputStream_init__I_V(N.unboxInt(((p2).data[0] as java_lang_Integer)));
			case 7962: return (new com_jtransc_JTranscArrays()).com_jtransc_JTranscArrays_init___V();
			case 7970: return (new com_jtransc_JTranscSystemProperties()).com_jtransc_JTranscSystemProperties_init___V();
			case 7980: return (new Benchmark_MyClass()).Benchmark_MyClass_init__Ljava_lang_String__V(((p2).data[0] as java_lang_String));
			case 7984: return (new java_nio_internal_MemoryBlock()).java_nio_internal_MemoryBlock_init___V();
			case 8005: return (new java_nio_ReadOnlyBufferException()).java_nio_ReadOnlyBufferException_init___V();
			case 8027: return (new java_nio_ByteBufferAsFloatBuffer()).java_nio_ByteBufferAsFloatBuffer_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8034: return (new java_nio_ByteOrder()).java_nio_ByteOrder_init__Ljava_lang_String_Z_V(((p2).data[0] as java_lang_String), N.unboxBool(((p2).data[1] as java_lang_Boolean)));
			case 8039: return (new java_nio_ByteBuffer()).java_nio_ByteBuffer_init__I_BIZ_V(N.unboxInt(((p2).data[0] as java_lang_Integer)), ((p2).data[1] as JA_B), N.unboxInt(((p2).data[2] as java_lang_Integer)), N.unboxBool(((p2).data[3] as java_lang_Boolean)));
			case 8043: return (new java_nio_ByteBufferAsFloatBuffer_BE()).java_nio_ByteBufferAsFloatBuffer_BE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8053: return (new java_nio_ByteBufferAsFloatBuffer_LE()).java_nio_ByteBufferAsFloatBuffer_LE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8063: return (new java_nio_ByteBufferAsLongBuffer_LE()).java_nio_ByteBufferAsLongBuffer_LE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8068: return (new java_nio_ByteBufferAsLongBuffer_BE()).java_nio_ByteBufferAsLongBuffer_BE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8076: return (new java_nio_ByteBufferAsDoubleBuffer_LE()).java_nio_ByteBufferAsDoubleBuffer_LE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8081: return (new java_nio_ByteBufferAsDoubleBuffer_BE()).java_nio_ByteBufferAsDoubleBuffer_BE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8085: return (new libcore_io_Memory()).libcore_io_Memory_init___V();
			case 8095: return (new java_nio_ByteBufferAsCharBuffer_LE()).java_nio_ByteBufferAsCharBuffer_LE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8100: return (new java_nio_ByteBufferAsCharBuffer_BE()).java_nio_ByteBufferAsCharBuffer_BE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8107: return (new java_nio_ByteBufferAsShortBuffer_LE()).java_nio_ByteBufferAsShortBuffer_LE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8108: return (new java_nio_ByteBufferAsShortBuffer()).java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsShortBuffer_1__V(((p2).data[0] as java_nio_ByteBuffer), ((p2).data[1] as java_nio_ByteBufferAsShortBuffer_1));
			case 8109: return (new java_nio_ByteBufferAsShortBuffer()).java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8112: return (new java_nio_ByteBufferAsShortBuffer_BE()).java_nio_ByteBufferAsShortBuffer_BE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8121: return (new java_nio_ByteBufferAsIntBuffer_BE()).java_nio_ByteBufferAsIntBuffer_BE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			default:
				break;
		}
		return null;
	}
	static java_lang_Object dynamicNew2_II_Ljava_lang_Object__Ljava_lang_Object_(int p0, int p1, JA_L p2) {
		switch (p1) {
			case 8126: return (new java_nio_ByteBufferAsIntBuffer_LE()).java_nio_ByteBufferAsIntBuffer_LE_init__Ljava_nio_ByteBuffer__V(((p2).data[0] as java_nio_ByteBuffer));
			case 8131: return (new java_nio_ByteBuffer()).java_nio_ByteBuffer_init___BZ_V(((p2).data[0] as JA_B), N.unboxBool(((p2).data[1] as java_lang_Boolean)));
			case 8134: return (new java_util_zip_CRC32()).java_util_zip_CRC32_init___V();
			case 8170: return (new java_nio_ByteBuffer()).java_nio_ByteBuffer_init___B_V(((p2).data[0] as JA_B));
			case 8180: return (new java_util_Random()).java_util_Random_init__J_V(N.unboxLong(((p2).data[0] as java_lang_Long)));
			case 8191: return (new com_jtransc_compression_jzlib_Deflate_Config()).com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(N.unboxInt(((p2).data[0] as java_lang_Integer)), N.unboxInt(((p2).data[1] as java_lang_Integer)), N.unboxInt(((p2).data[2] as java_lang_Integer)), N.unboxInt(((p2).data[3] as java_lang_Integer)), N.unboxInt(((p2).data[4] as java_lang_Integer)));
			case 8262: return (new java_util_zip_Deflater()).java_util_zip_Deflater_init__IZ_V(N.unboxInt(((p2).data[0] as java_lang_Integer)), N.unboxBool(((p2).data[1] as java_lang_Boolean)));
			case 8271: return (new Benchmark_Test1()).Benchmark_Test1_init__LBenchmark_1__V(((p2).data[0] as Benchmark_1));
			case 8272: return (new Benchmark_Test1()).Benchmark_Test1_init___V();
			case 8273: return (new Benchmark_Test2()).Benchmark_Test2_init__LBenchmark_1__V(((p2).data[0] as Benchmark_1));
			case 8274: return (new Benchmark_Test2()).Benchmark_Test2_init___V();
			case 8367: return (new java_util_Hashtable()).java_util_Hashtable_init___V();
			case 8368: return (new java_util_Hashtable_HashtableEntry()).java_util_Hashtable_HashtableEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_Hashtable_HashtableEntry__V((p2).data[0], (p2).data[1], N.unboxInt(((p2).data[2] as java_lang_Integer)), ((p2).data[3] as java_util_Hashtable_HashtableEntry));
			case 8374: return (new java_util_Properties()).java_util_Properties_init___V();
			case 8399: return (new java_util_Hashtable_EntryIterator()).java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable__V(((p2).data[0] as java_util_Hashtable));
			case 8404: return (new java_util_Hashtable_EntryIterator()).java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V(((p2).data[0] as java_util_Hashtable), ((p2).data[1] as java_util_Hashtable_1));
			case 8410: return (new java_util_Hashtable_EntrySet()).java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V(((p2).data[0] as java_util_Hashtable), ((p2).data[1] as java_util_Hashtable_1));
			case 8411: return (new java_util_Hashtable_EntrySet()).java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable__V(((p2).data[0] as java_util_Hashtable));
			case 8422: return (new java_io_ObjectStreamField()).java_io_ObjectStreamField_init__Ljava_lang_String_Ljava_lang_Class__V(((p2).data[0] as java_lang_String), ((p2).data[1] as java_lang_Class));
			case 8423: return (new java_lang_ref_WeakReference()).java_lang_ref_WeakReference_init__Ljava_lang_Object__V((p2).data[0]);
			case 8425: return (new java_lang_ref_ReferenceQueue()).java_lang_ref_ReferenceQueue_init___V();
			case 8427: return (new java_lang_ref_WeakReference()).java_lang_ref_WeakReference_init__Ljava_lang_Object_Ljava_lang_ref_ReferenceQueue__V((p2).data[0], ((p2).data[1] as java_lang_ref_ReferenceQueue));
			case 8457: return (new java_lang_SafeVarargs_Impl()).java_lang_SafeVarargs_Impl_init___V();
			case 8461: return (new java_lang_annotation_Annotation_Impl()).java_lang_annotation_Annotation_Impl_init___V();
			default:
				break;
		}
		return null;
	}
	j_ProgramReflection_DynamicNewInvoke([int CLASS_ID = 765]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_ReflectiveOperationException extends java_lang_Exception  {

	 java_lang_ReflectiveOperationException java_lang_ReflectiveOperationException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_Exception_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	 java_lang_ReflectiveOperationException java_lang_ReflectiveOperationException_init__Ljava_lang_String_Ljava_lang_Throwable__V(java_lang_String p0, java_lang_Throwable p1) {
		this.java_lang_Exception_init__Ljava_lang_String_Ljava_lang_Throwable__V(p0, p1);
		return this;
		return this;
	}
	 java_lang_ReflectiveOperationException java_lang_ReflectiveOperationException_init___V() {
		this.java_lang_Exception_init___V();
		return this;
		return this;
	}
	 java_lang_ReflectiveOperationException java_lang_ReflectiveOperationException_init__Ljava_lang_Throwable__V(java_lang_Throwable p0) {
		this.java_lang_Exception_init__Ljava_lang_Throwable__V(p0);
		return this;
		return this;
	}
	java_lang_ReflectiveOperationException([int CLASS_ID = 679]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_reflect_InvocationTargetException extends java_lang_ReflectiveOperationException  {

	java_lang_reflect_InvocationTargetException([int CLASS_ID = 764]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_reflect_Type   {

}
class java_lang_reflect_Type_IFields {

	static void SI() { }
}
abstract class java_lang_reflect_ParameterizedType  implements java_lang_reflect_Type {

}
class java_lang_reflect_ParameterizedType_IFields {

	static void SI() { }
}
class java_lang_reflect_ParameterizedTypeImpl extends java_lang_Object implements java_lang_reflect_ParameterizedType {

	java_lang_reflect_Type _ownerType = null;
	JA_L _actualTypeArguments = null;
	java_lang_reflect_Type _rawType = null;
	 java_lang_reflect_ParameterizedTypeImpl java_lang_reflect_ParameterizedTypeImpl_init___Ljava_lang_reflect_Type_Ljava_lang_reflect_Type_Ljava_lang_reflect_Type__V(JA_L p0, java_lang_reflect_Type p1, java_lang_reflect_Type p2) {
		this.java_lang_Object_init___V();
		this._actualTypeArguments = p0;
		this._rawType = p1;
		this._ownerType = p2;
		return this;
		return this;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		int G = 0;
		java_lang_Object lA1 = null;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA1 = fA0;
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils.getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(this._rawType));
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(60);
					lI2 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI2 >= (this._actualTypeArguments as JA_0).length))) {
						G = 2;
						continue;
					}
					if (((lI2 == 0))) {
						G = 3;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_6);
					G = 3;
					continue;
				case 3:
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils.getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_((((this._actualTypeArguments as JA_L)).data[lI2] as java_lang_reflect_Type)));
					lI2 = (N.I(lI2 + 1));
					G = 1;
					continue;
				case 2:
					(lA1 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(62);
					return (lA1 as java_lang_StringBuilder).toString__Ljava_lang_String_();
				default:
					break;
			}
		}
		return null;
	}
	java_lang_reflect_ParameterizedTypeImpl([int CLASS_ID = 762]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_InternalError extends java_lang_Error  {

	 java_lang_InternalError java_lang_InternalError_init__Ljava_lang_Throwable__V(java_lang_Throwable p0) {
		this.java_lang_Error_init__Ljava_lang_Throwable__V(p0);
		return this;
		return this;
	}
	 java_lang_InternalError java_lang_InternalError_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_Error_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	java_lang_InternalError([int CLASS_ID = 761]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_AbstractList_SimpleListIterator extends java_lang_Object implements java_util_Iterator {

	int _expectedModCount = 0;
	java_util_AbstractList _this_0 = null;
	int _pos = 0;
	int _lastPosition = 0;
	 java_util_AbstractList_SimpleListIterator java_util_AbstractList_SimpleListIterator_init__Ljava_util_AbstractList__V(java_util_AbstractList p0) {
		this._this_0 = p0;
		this.java_lang_Object_init___V();
		this._pos = -1;
		this._lastPosition = -1;
		this._expectedModCount = p0._modCount;
		return this;
		return this;
	}
	 java_lang_Object next__Ljava_lang_Object_() {
		int G = 0;
		java_lang_Object lA1 = null;
		int fI1 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		int tI1 = 0;
		java_lang_Object tA2 = null;
		java_lang_Object tA4 = null;
		java_lang_Object tA3 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							if (((this._expectedModCount != this._this_0._modCount))) {
								G = 1;
								continue;
							}
							G = 2;
							continue;
						case 2:
							lA1 = this._this_0.get_I_Ljava_lang_Object_((N.I(this._pos + 1)));
							fA0 = this;
							fA1 = this;
							tA2 = fA1;
							tI1 = (N.I(this._pos + 1));
							fI1 = tI1;
							(tA2 as java_util_AbstractList_SimpleListIterator)._pos = tI1;
							(fA0 as java_util_AbstractList_SimpleListIterator)._lastPosition = fI1;
							fA0 = lA1;
							G = 3;
							continue;
						case 3: return fA0;
						case 4:
							fA0 = J__exception__;
							lA1 = fA0;
							tA4 = (new java_util_NoSuchElementException());
							fA0 = tA4;
							(tA4 as java_util_NoSuchElementException).java_util_NoSuchElementException_init___V();
							throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
							G = 1;
							continue;
						case 1:
							tA3 = (new java_util_ConcurrentModificationException());
							fA0 = tA3;
							(tA3 as java_util_ConcurrentModificationException).java_util_ConcurrentModificationException_init___V();
							throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
							break;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 2)) && ((G < 3)))) && ((J__exception__) is java_lang_IndexOutOfBoundsException)))) {
					G = 4;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	 bool hasNext__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if ((((N.I(this._pos + 1)) >= this._this_0.size__I()))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	java_util_AbstractList_SimpleListIterator([int CLASS_ID = 760]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_ListIterator  implements java_util_Iterator {

	 java_lang_Object next__Ljava_lang_Object_();
	 bool hasNext__Z();
	 int previousIndex__I();
}
class java_util_ListIterator_IFields {

	static void SI() { }
}
class java_util_AbstractList_FullListIterator extends java_util_AbstractList_SimpleListIterator implements java_util_ListIterator {

	java_util_AbstractList _this_0_ = null;
	 java_util_AbstractList_FullListIterator java_util_AbstractList_FullListIterator_init__Ljava_util_AbstractList_I_V(java_util_AbstractList p0, int p1) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					this._this_0_ = p0;
					this.java_util_AbstractList_SimpleListIterator_init__Ljava_util_AbstractList__V(p0);
					if (((p1 < 0))) {
						G = 1;
						continue;
					}
					if (((p1 > p0.size__I()))) {
						G = 1;
						continue;
					}
					this._pos = (N.I(p1 - 1));
					G = 2;
					continue;
				case 1:
					tA0 = (new java_lang_IndexOutOfBoundsException());
					fA0 = tA0;
					(tA0 as java_lang_IndexOutOfBoundsException).java_lang_IndexOutOfBoundsException_init___V();
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 int previousIndex__I() {
		return this._pos;
	}
	java_util_AbstractList_FullListIterator([int CLASS_ID = 759]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_ArrayList extends java_util_AbstractList implements java_util_List, java_util_RandomAccess, java_lang_Cloneable, java_io_Serializable {

	int _length = 0;
	JA_L _buffer = null;
	 int size__I() {
		return this._length;
	}
	 bool isEmpty__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this.size__I() != 0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 java_util_ArrayList java_util_ArrayList_init___V() {
		this.java_util_ArrayList_init__I_V(0);
		return this;
		return this;
	}
	 java_util_ArrayList java_util_ArrayList_init__I_V(int p0) {
		this.java_util_AbstractList_init___V();
		this._buffer = new JA_L(p0, "[Ljava.lang.Object;");
		this._length = 0;
		return this;
		return this;
	}
	 bool contains_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this.indexOf_Ljava_lang_Object__I(p0) < 0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 int indexOf_Ljava_lang_Object__I(java_lang_Object p0) {
		int G = 0;
		int lI2 = 0;
		int lI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI2 = this.size__I();
					lI3 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI3 >= lI2))) {
						G = 2;
						continue;
					}
					if (!(java_util_Objects.equals_Ljava_lang_Object_Ljava_lang_Object__Z(p0, this._get_I_Ljava_lang_Object_(lI3)))) {
						G = 3;
						continue;
					}
					return lI3;
				case 3:
					lI3 = (N.I(lI3 + 1));
					G = 1;
					continue;
				case 2:
					return -1;
				default:
					break;
			}
		}
		return 0;
	}
	 java_lang_Object _get_I_Ljava_lang_Object_(int p0) {
		return (this._buffer).data[p0];
	}
	 java_lang_Object get_I_Ljava_lang_Object_(int p0) {
		this.rangeCheck_I_V(p0);
		return this._get_I_Ljava_lang_Object_(p0);
	}
	 void rangeCheck_I_V(int p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 < this.size__I()))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_IndexOutOfBoundsException());
					fA0 = tA0;
					(tA0 as java_lang_IndexOutOfBoundsException).java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V(this.outOfBoundsMsg_I_Ljava_lang_String_(p0));
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					return;
				default:
					break;
			}
		}
		return;
	}
	 java_lang_String outOfBoundsMsg_I_Ljava_lang_String_(int p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_218).append_I_Ljava_lang_StringBuilder_(p0).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_217).append_I_Ljava_lang_StringBuilder_(this.size__I()).toString__Ljava_lang_String_();
	}
	 java_util_Iterator iterator__Ljava_util_Iterator_() {
		return this.listIterator__Ljava_util_ListIterator_();
	}
	 java_lang_Object clone__Ljava_lang_Object_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							G = 1;
							continue;
						case 1:
							lA1 = ((super.clone__Ljava_lang_Object_()) as java_util_ArrayList);
							java_util_ArrayList._copy_Ljava_util_ArrayList_Ljava_util_ArrayList__V((lA1 as java_util_ArrayList), this);
							(lA1 as java_util_ArrayList)._modCount = 0;
							fA0 = lA1;
							G = 2;
							continue;
						case 2: return fA0;
						case 3:
							fA0 = J__exception__;
							lA1 = fA0;
							tA0 = (new java_lang_InternalError());
							fA0 = tA0;
							(tA0 as java_lang_InternalError).java_lang_InternalError_init__Ljava_lang_Throwable__V((lA1 as java_lang_Throwable));
							throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
							break;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_CloneNotSupportedException)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	static void _copy_Ljava_util_ArrayList_Ljava_util_ArrayList__V(java_util_ArrayList p0, java_util_ArrayList p1) {
		p0._buffer = java_util_Arrays.copyOf__Ljava_lang_Object_I__Ljava_lang_Object_(p1._buffer, p1._length);
		p0._length = p1._length;
		return;
	}
	 bool add_Ljava_lang_Object__Z(java_lang_Object p0) {
		this._add_Ljava_lang_Object__V(p0);
		return true;
	}
	 void _add_Ljava_lang_Object__V(java_lang_Object p0) {
		int fI1 = 0;
		JA_L fA0 = null;
		java_lang_Object fA1 = null;
		int tI1 = 0;
		java_lang_Object tA2 = null;
		this.ensure_I_V((N.I(this._length + 1)));
		fA0 = this._buffer;
		fA1 = this;
		tA2 = fA1;
		tI1 = this._length;
		fI1 = tI1;
		(tA2 as java_util_ArrayList)._length = (N.I(tI1 + 1));
		fA0.data[fI1] = p0;
		return;
	}
	 void ensure_I_V(int p0) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 <= (this._buffer as JA_0).length))) {
						G = 1;
						continue;
					}
					this._buffer = java_util_Arrays.copyOf__Ljava_lang_Object_I__Ljava_lang_Object_(this._buffer, java_lang_Math.max_II_I(p0, (N.I((N.I((this._buffer as JA_0).length * 2)) + 2))));
					G = 1;
					continue;
				case 1:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void add_ILjava_lang_Object__V(int p0, java_lang_Object p1) {
		this.rangeCheckForAdd_I_V(p0);
		this._insert_ILjava_lang_Object__V(p0, p1);
		return;
	}
	 void _insert_ILjava_lang_Object__V(int p0, java_lang_Object p1) {
		this.makeHole_II_V(p0, 1);
		this._buffer.data[p0] = p1;
		return;
	}
	 void makeHole_II_V(int p0, int p1) {
		this.ensure_I_V((N.I(this._length + p1)));
		java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(this._buffer, p0, this._buffer, (N.I(p0 + p1)), (N.I((N.I(this._length - p0)) - p1)));
		this._length = (N.I(this._length + p1));
		return;
	}
	 void rangeCheckForAdd_I_V(int p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 > this.size__I()))) {
						G = 1;
						continue;
					}
					if (((p0 >= 0))) {
						G = 2;
						continue;
					}
					G = 1;
					continue;
				case 1:
					tA0 = (new java_lang_IndexOutOfBoundsException());
					fA0 = tA0;
					(tA0 as java_lang_IndexOutOfBoundsException).java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V(this.outOfBoundsMsg_I_Ljava_lang_String_(p0));
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	 JA_L toArray__Ljava_lang_Object___Ljava_lang_Object_(JA_L p0) {
		int G = 0;
		java_lang_Object lA1 = null;
		int lI2 = 0;
		int lI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					lA1 = p0;
					lI2 = this.size__I();
					if ((((lA1 as JA_0).length >= lI2))) {
						G = 1;
						continue;
					}
					lA1 = java_util_Arrays.copyOf__Ljava_lang_Object_ILjava_lang_Class___Ljava_lang_Object_(new JA_L(0, "[Ljava.lang.Object;"), lI2, lA1.getClass__Ljava_lang_Class_());
					G = 1;
					continue;
				case 1:
					lI3 = 0;
					G = 2;
					continue;
				case 2:
					if (((lI3 >= lI2))) {
						G = 3;
						continue;
					}
					(lA1 as JA_L).data[lI3] = this._get_I_Ljava_lang_Object_(lI3);
					lI3 = (N.I(lI3 + 1));
					G = 2;
					continue;
				case 3:
					return (lA1 as JA_L);
				default:
					break;
			}
		}
		return null;
	}
	java_util_ArrayList([int CLASS_ID = 754]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_reflect_ArrayType extends java_lang_Object implements java_lang_reflect_Type {

	java_lang_reflect_Type _element = null;
	 java_lang_reflect_ArrayType java_lang_reflect_ArrayType_init__Ljava_lang_reflect_Type__V(java_lang_reflect_Type p0) {
		this.java_lang_Object_init___V();
		this._element = p0;
		return this;
		return this;
	}
	java_lang_reflect_ArrayType([int CLASS_ID = 753]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_text_MStringReader extends java_lang_Object  {

	int _length = 0;
	int _offset = 0;
	java_lang_String _str = null;
	 com_jtransc_text_MStringReader com_jtransc_text_MStringReader_init__Ljava_lang_String__V(java_lang_String p0) {
		this.com_jtransc_text_MStringReader_init__Ljava_lang_String_I_V(p0, 0);
		return this;
		return this;
	}
	 com_jtransc_text_MStringReader com_jtransc_text_MStringReader_init__Ljava_lang_String_I_V(java_lang_String p0, int p1) {
		this.java_lang_Object_init___V();
		this._str = p0;
		this._length = p0.length__I();
		this._offset = p1;
		return this;
		return this;
	}
	 bool hasMore__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._offset >= this._length))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 int peek__C() {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (this.hasMore__Z()) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_Error());
					fA0 = tA0;
					(tA0 as java_lang_Error).java_lang_Error_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_219);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					return this._str.charAt_I_C(this._offset);
				default:
					break;
			}
		}
		return 0;
	}
	 int read__C() {
		int G = 0;
		int lI1 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (this.hasMore__Z()) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_Error());
					fA0 = tA0;
					(tA0 as java_lang_Error).java_lang_Error_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_219);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					lI1 = (this.peek__C());
					this.skip__V();
					return N.i2c(lI1);
				default:
					break;
			}
		}
		return 0;
	}
	 void skip__V() {
		this.skip_I_V(1);
		return;
	}
	 void skip_I_V(int p0) {
		this._offset = (N.I(this._offset + p0));
		return;
	}
	 java_lang_String readUntil_CCZ_Ljava_lang_String_(int p0, int p1, bool p2) {
		return this.readUntil_CCCZ_Ljava_lang_String_(p0, p1, p1, p2);
	}
	 java_lang_String readUntil_CCCZ_Ljava_lang_String_(int p0, int p1, int p2, bool p3) {
		int G = 0;
		int lI5 = 0;
		int lI6 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI5 = this._offset;
					G = 1;
					continue;
				case 1:
					if (!(this.hasMore__Z())) {
						G = 2;
						continue;
					}
					lI6 = (this.read__C());
					if (((lI6 == (p0)))) {
						G = 3;
						continue;
					}
					if (((lI6 == (p1)))) {
						G = 3;
						continue;
					}
					if (((lI6 != (p2)))) {
						G = 4;
						continue;
					}
					G = 3;
					continue;
				case 3:
					if (p3) {
						G = 2;
						continue;
					}
					this.skip_I_V(-1);
					G = 2;
					continue;
				case 4:
					G = 1;
					continue;
				case 2:
					return this._str.substring_II_Ljava_lang_String_(lI5, this._offset);
				default:
					break;
			}
		}
		return null;
	}
	 void expect_C_V(int p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		java_lang_Object tA3 = null;
		while (true) {
			switch (G) {
				case 0:
					if (this.hasMore__Z()) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_Error());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_Error).java_lang_Error_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_221).append_C_Ljava_lang_StringBuilder_(p0).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_220).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					if (((this.read__C() == (p0)))) {
						G = 2;
						continue;
					}
					tA2 = (new java_lang_Error());
					fA0 = tA2;
					fA1 = tA2;
					tA3 = (new java_lang_StringBuilder());
					fA2 = tA3;
					(tA3 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_Error).java_lang_Error_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_221).append_C_Ljava_lang_StringBuilder_(p0).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	com_jtransc_text_MStringReader([int CLASS_ID = 751]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_reflect_MethodTypeImpl extends java_lang_Object implements java_lang_reflect_Type {

	JA_L _args = null;
	java_lang_reflect_Type _rettype = null;
	 java_lang_reflect_MethodTypeImpl java_lang_reflect_MethodTypeImpl_init___Ljava_lang_reflect_Type_Ljava_lang_reflect_Type__V(JA_L p0, java_lang_reflect_Type p1) {
		this.java_lang_Object_init___V();
		this._args = p0;
		this._rettype = p1;
		return this;
		return this;
	}
	java_lang_reflect_MethodTypeImpl([int CLASS_ID = 750]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_reflect_AnnotatedElement   {

}
class java_lang_reflect_AnnotatedElement_IFields {

	static void SI() { }
}
abstract class java_lang_reflect_AccessibleObject extends java_lang_Object implements java_lang_reflect_AnnotatedElement {

	j_MemberInfo _info = null;
	 java_lang_reflect_AccessibleObject java_lang_reflect_AccessibleObject_init__Lj_MemberInfo__V(j_MemberInfo p0) {
		this.java_lang_Object_init___V();
		this._info = p0;
		return this;
		return this;
	}
	java_lang_reflect_AccessibleObject([int CLASS_ID = 676]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_reflect_MethodConstructor extends java_lang_reflect_AccessibleObject  {

	java_lang_Class _clazz = null;
	java_lang_String _name = null;
	int _id = 0;
	JA_L _exceptionTypes = null;
	java_lang_String _genericSignature = null;
	int _modifiers = 0;
	int _slot = 0;
	java_lang_String _signature = null;
	java_lang_reflect_MethodTypeImpl _methodType = null;
	 java_lang_reflect_MethodConstructor java_lang_reflect_MethodConstructor_init__Ljava_lang_Class_Lj_MemberInfo__V(java_lang_Class p0, j_MemberInfo p1) {
		this.java_lang_reflect_AccessibleObject_init__Lj_MemberInfo__V(p1);
		this._exceptionTypes = new JA_L(0, "[Ljava.lang.Class;");
		this._clazz = p0;
		this._id = p1._id;
		this._slot = p1._id;
		this._name = p1._name;
		this._signature = p1._desc;
		this._genericSignature = p1._genericDesc;
		this._modifiers = p1._modifiers;
		return this;
		return this;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		int G = 0;
		java_lang_Object lA4 = null;
		java_lang_Object lA7 = null;
		int lI1 = 0;
		int lI3 = 0;
		int lI5 = 0;
		int lI6 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		java_lang_Object tA3 = null;
		java_lang_Object tA4 = null;
		java_lang_Object tA5 = null;
		java_lang_Object tA6 = null;
		java_lang_Object tA7 = null;
		java_lang_String lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = this.getModifiers__I();
					lA2 = Bootstrap.STRINGLIT_20;
					if (((lI1 == 0))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA2 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(lA2).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect_Modifier.toString_I_Ljava_lang_String_(lI1)).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_222).toString__Ljava_lang_String_();
					G = 1;
					continue;
				case 1:
					if (((this.getReturnType__Ljava_lang_Class_() == null))) {
						G = 2;
						continue;
					}
					tA1 = (new java_lang_StringBuilder());
					fA0 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA2 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(lA2).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils.getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(this.getReturnType__Ljava_lang_Class_())).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_222).toString__Ljava_lang_String_();
					G = 2;
					continue;
				case 2:
					tA2 = (new java_lang_StringBuilder());
					fA0 = tA2;
					(tA2 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA2 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(lA2).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils.getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(this.getDeclaringClass__Ljava_lang_Class_())).toString__Ljava_lang_String_();
					if (this.isConstructor__Z()) {
						G = 3;
						continue;
					}
					tA3 = (new java_lang_StringBuilder());
					fA0 = tA3;
					(tA3 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA2 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(lA2).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_223).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getName__Ljava_lang_String_()).toString__Ljava_lang_String_();
					G = 3;
					continue;
				case 3:
					tA4 = (new java_lang_StringBuilder());
					fA0 = tA4;
					(tA4 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA2 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(lA2).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_224).toString__Ljava_lang_String_();
					lI3 = 1;
					lA4 = this.getParameterTypes___Ljava_lang_Class_();
					lI5 = (lA4 as JA_0).length;
					lI6 = 0;
					G = 4;
					continue;
				case 4:
					if (((lI6 >= lI5))) {
						G = 5;
						continue;
					}
					lA7 = ((lA4 as JA_L)).data[lI6];
					if (((lI3 != 0))) {
						G = 6;
						continue;
					}
					tA5 = (new java_lang_StringBuilder());
					fA0 = tA5;
					(tA5 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA2 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(lA2).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_62).toString__Ljava_lang_String_();
					G = 6;
					continue;
				case 6:
					tA6 = (new java_lang_StringBuilder());
					fA0 = tA6;
					(tA6 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA2 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(lA2).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils.getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_((lA7 as java_lang_reflect_Type))).toString__Ljava_lang_String_();
					lI3 = 0;
					lI6 = (N.I(lI6 + 1));
					G = 4;
					continue;
				case 5:
					tA7 = (new java_lang_StringBuilder());
					fA0 = tA7;
					(tA7 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA2 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(lA2).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_1).toString__Ljava_lang_String_();
					return lA2;
				default:
					break;
			}
		}
		return null;
	}
	 int getModifiers__I() {
		return this._modifiers;
	}
	 java_lang_String getName__Ljava_lang_String_() {
		return null;
	}
	 java_lang_Class getDeclaringClass__Ljava_lang_Class_() {
		return this._clazz;
	}
	 bool isConstructor__Z() {
		throw new Exception("Missing body java.lang.reflect.MethodConstructor.isConstructor\u0028\u0029Z");
	}
	 java_lang_Class getReturnType__Ljava_lang_Class_() {
		return null;
	}
	 JA_L getParameterTypes___Ljava_lang_Class_() {
		return ((this.methodType__Ljava_lang_reflect_MethodTypeImpl_()._args) as JA_L);
	}
	 java_lang_reflect_MethodTypeImpl methodType__Ljava_lang_reflect_MethodTypeImpl_() {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._methodType != null))) {
						G = 1;
						continue;
					}
					this._methodType = java_lang_reflect__InternalUtils.parseMethodType_Ljava_lang_String_Ljava_lang_reflect_Type__Ljava_lang_reflect_MethodTypeImpl_(this._signature, null);
					G = 1;
					continue;
				case 1:
					return this._methodType;
				default:
					break;
			}
		}
		return null;
	}
	java_lang_reflect_MethodConstructor([int CLASS_ID = 749]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_reflect_Member   {

	 java_lang_Class getDeclaringClass__Ljava_lang_Class_();
	 java_lang_String getName__Ljava_lang_String_();
	 int getModifiers__I();
}
class java_lang_reflect_Member_IFields {

	static void SI() { }
}
abstract class java_lang_reflect_GenericDeclaration  implements java_lang_reflect_AnnotatedElement {

}
class java_lang_reflect_GenericDeclaration_IFields {

	static void SI() { }
}
class java_lang_reflect_Constructor extends java_lang_reflect_MethodConstructor implements java_lang_reflect_Member, java_lang_reflect_GenericDeclaration {

	 java_lang_reflect_Constructor java_lang_reflect_Constructor_init__Ljava_lang_Class_Lj_MemberInfo__V(java_lang_Class p0, j_MemberInfo p1) {
		this.java_lang_reflect_MethodConstructor_init__Ljava_lang_Class_Lj_MemberInfo__V(p0, p1);
		return this;
		return this;
	}
	 java_lang_String getName__Ljava_lang_String_() {
		return this.getDeclaringClass__Ljava_lang_Class_().getName__Ljava_lang_String_();
	}
	 java_lang_Class getDeclaringClass__Ljava_lang_Class_() {
		return this._clazz;
	}
	 bool isConstructor__Z() {
		return true;
	}
	 JA_L getParameterTypes___Ljava_lang_Class_() {
		return ((this.methodType__Ljava_lang_reflect_MethodTypeImpl_()._args) as JA_L);
	}
	 int hashCode__I() {
		return this.getDeclaringClass__Ljava_lang_Class_().getName__Ljava_lang_String_().hashCode__I();
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		throw new Exception("Missing body java.lang.reflect.Constructor.equals\u0028Ljava/lang/Object;\u0029Z");
	}
	 java_lang_Object newInstance__Ljava_lang_Object__Ljava_lang_Object_(JA_L p0) {
		return j_ProgramReflection.dynamicNew_II_Ljava_lang_Object__Ljava_lang_Object_(this._clazz._id, this._slot, p0);
	}
	java_lang_reflect_Constructor([int CLASS_ID = 748]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_InstantiationException extends java_lang_ReflectiveOperationException  {

	 java_lang_InstantiationException java_lang_InstantiationException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_ReflectiveOperationException_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	java_lang_InstantiationException([int CLASS_ID = 747]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_NoSuchMethodException extends java_lang_Exception  {

	 java_lang_NoSuchMethodException java_lang_NoSuchMethodException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_Exception_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	java_lang_NoSuchMethodException([int CLASS_ID = 746]) : super(CLASS_ID) { }
	static void SI() { }
}
class j_ProgramReflection_AllFields extends java_lang_Object  {

	 j_ProgramReflection_AllFields j_ProgramReflection_AllFields_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static JA_L getFields_I__Lj_MemberInfo_(int p0) {
		if (((p0 >= 863))) {
			return j_ProgramReflection_AllFields.getFields2_I__Lj_MemberInfo_(p0);
		}
		if (((p0 >= 760))) {
			return j_ProgramReflection_AllFields.getFields1_I__Lj_MemberInfo_(p0);
		}
		if (((p0 >= 655))) {
			return j_ProgramReflection_AllFields.getFields0_I__Lj_MemberInfo_(p0);
		}
		return null;
	}
	static JA_L getFields0_I__Lj_MemberInfo_(int p0) {
		JA_L _out = null;
		switch (p0) {
			case 655:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2270, null, Bootstrap.STRINGLIT_225, 10, Bootstrap.STRINGLIT_226, null);
				return _out;
			case 657:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2139, null, Bootstrap.STRINGLIT_227, 25, Bootstrap.STRINGLIT_228, Bootstrap.STRINGLIT_229);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2159, null, Bootstrap.STRINGLIT_230, 2, Bootstrap.STRINGLIT_231, null);
				return _out;
			case 666:
				_out = new JA_L(10, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2140, null, Bootstrap.STRINGLIT_232, 2, Bootstrap.STRINGLIT_233, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2141, null, Bootstrap.STRINGLIT_234, 2, Bootstrap.STRINGLIT_235, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2142, null, Bootstrap.STRINGLIT_236, 2, Bootstrap.STRINGLIT_231, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2205, null, Bootstrap.STRINGLIT_237, 10, Bootstrap.STRINGLIT_238, Bootstrap.STRINGLIT_239);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2218, null, Bootstrap.STRINGLIT_240, 194, Bootstrap.STRINGLIT_241, Bootstrap.STRINGLIT_242);
				_out.data[5] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2219, null, Bootstrap.STRINGLIT_243, 2, Bootstrap.STRINGLIT_244, null);
				_out.data[6] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2220, null, Bootstrap.STRINGLIT_245, 2, Bootstrap.STRINGLIT_244, null);
				_out.data[7] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2221, null, Bootstrap.STRINGLIT_246, 1, Bootstrap.STRINGLIT_231, null);
				_out.data[8] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2222, null, Bootstrap.STRINGLIT_247, 1, Bootstrap.STRINGLIT_248, null);
				_out.data[9] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2223, null, Bootstrap.STRINGLIT_249, 1, Bootstrap.STRINGLIT_250, null);
				return _out;
			case 672:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2143, null, Bootstrap.STRINGLIT_251, 2, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2144, null, Bootstrap.STRINGLIT_252, 10, Bootstrap.STRINGLIT_253, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2145, null, Bootstrap.STRINGLIT_254, 26, Bootstrap.STRINGLIT_255, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2146, null, Bootstrap.STRINGLIT_256, 25, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_258);
				return _out;
			case 674:
				_out = new JA_L(6, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2147, null, Bootstrap.STRINGLIT_259, 1, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_260);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2148, null, Bootstrap.STRINGLIT_232, 1, Bootstrap.STRINGLIT_233, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2149, null, Bootstrap.STRINGLIT_236, 4, Bootstrap.STRINGLIT_231, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2153, null, Bootstrap.STRINGLIT_261, 129, Bootstrap.STRINGLIT_233, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2229, null, Bootstrap.STRINGLIT_262, 1, Bootstrap.STRINGLIT_231, null);
				_out.data[5] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2236, null, Bootstrap.STRINGLIT_263, 129, Bootstrap.STRINGLIT_233, null);
				return _out;
			case 676:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2237, null, Bootstrap.STRINGLIT_249, 1, Bootstrap.STRINGLIT_264, null);
				return _out;
			case 678:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2165, null, Bootstrap.STRINGLIT_265, 2, Bootstrap.STRINGLIT_266, null);
				return _out;
			case 681:
				_out = new JA_L(8, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2150, null, Bootstrap.STRINGLIT_267, 2, Bootstrap.STRINGLIT_235, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2151, null, Bootstrap.STRINGLIT_268, 10, Bootstrap.STRINGLIT_269, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2152, null, Bootstrap.STRINGLIT_270, 2, Bootstrap.STRINGLIT_233, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2162, null, Bootstrap.STRINGLIT_271, 2, Bootstrap.STRINGLIT_235, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2163, null, Bootstrap.STRINGLIT_272, 2, Bootstrap.STRINGLIT_235, null);
				_out.data[5] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2164, null, Bootstrap.STRINGLIT_273, 2, Bootstrap.STRINGLIT_266, null);
				_out.data[6] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2228, null, Bootstrap.STRINGLIT_274, 2, Bootstrap.STRINGLIT_275, null);
				_out.data[7] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2269, null, Bootstrap.STRINGLIT_276, 2, Bootstrap.STRINGLIT_277, Bootstrap.STRINGLIT_278);
				return _out;
			case 682:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2154, null, Bootstrap.STRINGLIT_256, 25, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_279);
				return _out;
			case 683:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2155, null, Bootstrap.STRINGLIT_251, 18, Bootstrap.STRINGLIT_280, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2156, null, Bootstrap.STRINGLIT_256, 25, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_281);
				return _out;
			case 686:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2157, null, Bootstrap.STRINGLIT_251, 18, Bootstrap.STRINGLIT_282, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2158, null, Bootstrap.STRINGLIT_256, 25, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_283);
				return _out;
			case 688:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2166, null, Bootstrap.STRINGLIT_284, 25, Bootstrap.STRINGLIT_285, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2167, null, Bootstrap.STRINGLIT_286, 25, Bootstrap.STRINGLIT_287, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2168, null, Bootstrap.STRINGLIT_288, 25, Bootstrap.STRINGLIT_287, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2534, null, Bootstrap.STRINGLIT_289, 10, Bootstrap.STRINGLIT_290, null);
				return _out;
			case 689:
				_out = new JA_L(3, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2161, null, Bootstrap.STRINGLIT_291, 2, Bootstrap.STRINGLIT_233, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2320, null, Bootstrap.STRINGLIT_292, 2, Bootstrap.STRINGLIT_235, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2321, null, Bootstrap.STRINGLIT_293, 2, Bootstrap.STRINGLIT_235, null);
				return _out;
			case 692:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2160, null, Bootstrap.STRINGLIT_286, 4, Bootstrap.STRINGLIT_294, null);
				return _out;
			case 699:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2171, null, Bootstrap.STRINGLIT_295, 16, Bootstrap.STRINGLIT_235, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2172, null, Bootstrap.STRINGLIT_296, 16, Bootstrap.STRINGLIT_297, null);
				return _out;
			case 701:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2169, null, Bootstrap.STRINGLIT_298, 1, Bootstrap.STRINGLIT_299, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2170, null, Bootstrap.STRINGLIT_295, 18, Bootstrap.STRINGLIT_235, null);
				return _out;
			case 706:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2173, null, Bootstrap.STRINGLIT_251, 18, Bootstrap.STRINGLIT_226, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2174, null, Bootstrap.STRINGLIT_256, 25, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_300);
				return _out;
			case 708:
				_out = new JA_L(9, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2179, null, Bootstrap.STRINGLIT_301, 128, Bootstrap.STRINGLIT_302, Bootstrap.STRINGLIT_303);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2180, null, Bootstrap.STRINGLIT_11, 130, Bootstrap.STRINGLIT_231, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2181, null, Bootstrap.STRINGLIT_304, 26, Bootstrap.STRINGLIT_305, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2182, null, Bootstrap.STRINGLIT_306, 128, Bootstrap.STRINGLIT_231, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2185, null, Bootstrap.STRINGLIT_307, 128, Bootstrap.STRINGLIT_308, Bootstrap.STRINGLIT_309);
				_out.data[5] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2186, null, Bootstrap.STRINGLIT_310, 128, Bootstrap.STRINGLIT_231, null);
				_out.data[6] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2192, null, Bootstrap.STRINGLIT_311, 130, Bootstrap.STRINGLIT_312, Bootstrap.STRINGLIT_313);
				_out.data[7] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2267, null, Bootstrap.STRINGLIT_252, 130, Bootstrap.STRINGLIT_314, Bootstrap.STRINGLIT_315);
				_out.data[8] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2268, null, Bootstrap.STRINGLIT_316, 130, Bootstrap.STRINGLIT_312, Bootstrap.STRINGLIT_317);
				return _out;
			case 710:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2265, null, Bootstrap.STRINGLIT_318, 0, Bootstrap.STRINGLIT_314, Bootstrap.STRINGLIT_315);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2266, null, Bootstrap.STRINGLIT_316, 0, Bootstrap.STRINGLIT_312, Bootstrap.STRINGLIT_317);
				return _out;
			case 713:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2175, null, Bootstrap.STRINGLIT_319, 16, Bootstrap.STRINGLIT_320, Bootstrap.STRINGLIT_321);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2176, null, Bootstrap.STRINGLIT_322, 0, Bootstrap.STRINGLIT_308, Bootstrap.STRINGLIT_309);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2177, null, Bootstrap.STRINGLIT_251, 0, Bootstrap.STRINGLIT_320, Bootstrap.STRINGLIT_323);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2178, null, Bootstrap.STRINGLIT_230, 16, Bootstrap.STRINGLIT_231, null);
				return _out;
			case 719:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2183, null, Bootstrap.STRINGLIT_324, 4112, Bootstrap.STRINGLIT_325, null);
				return _out;
			case 722:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2190, null, Bootstrap.STRINGLIT_324, 4112, Bootstrap.STRINGLIT_325, null);
				return _out;
			case 723:
				_out = new JA_L(5, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2184, null, Bootstrap.STRINGLIT_326, 0, Bootstrap.STRINGLIT_308, Bootstrap.STRINGLIT_309);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2187, null, Bootstrap.STRINGLIT_327, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2188, null, Bootstrap.STRINGLIT_324, 4112, Bootstrap.STRINGLIT_325, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2189, null, Bootstrap.STRINGLIT_328, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2191, null, Bootstrap.STRINGLIT_329, 0, Bootstrap.STRINGLIT_308, Bootstrap.STRINGLIT_309);
				return _out;
			case 726:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2194, null, Bootstrap.STRINGLIT_251, 18, Bootstrap.STRINGLIT_330, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2195, null, Bootstrap.STRINGLIT_256, 25, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_331);
				return _out;
			case 728:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2196, null, Bootstrap.STRINGLIT_251, 18, Bootstrap.STRINGLIT_332, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2197, null, Bootstrap.STRINGLIT_256, 25, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_333);
				return _out;
			case 730:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2198, null, Bootstrap.STRINGLIT_256, 25, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_334);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2199, null, Bootstrap.STRINGLIT_335, 25, Bootstrap.STRINGLIT_336, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2200, null, Bootstrap.STRINGLIT_337, 25, Bootstrap.STRINGLIT_336, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2201, null, Bootstrap.STRINGLIT_251, 18, Bootstrap.STRINGLIT_235, null);
				return _out;
			case 731:
				_out = new JA_L(3, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2202, null, Bootstrap.STRINGLIT_338, 26, Bootstrap.STRINGLIT_339, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2203, null, Bootstrap.STRINGLIT_251, 18, Bootstrap.STRINGLIT_340, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2204, null, Bootstrap.STRINGLIT_256, 25, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_341);
				return _out;
			case 732:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2206, null, Bootstrap.STRINGLIT_342, 10, Bootstrap.STRINGLIT_231, null);
				return _out;
			case 736:
				_out = new JA_L(3, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2215, null, Bootstrap.STRINGLIT_343, 9, Bootstrap.STRINGLIT_344, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2216, null, Bootstrap.STRINGLIT_345, 9, Bootstrap.STRINGLIT_346, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2217, null, Bootstrap.STRINGLIT_347, 9, Bootstrap.STRINGLIT_238, Bootstrap.STRINGLIT_348);
				return _out;
			case 740:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2224, null, Bootstrap.STRINGLIT_349, 2, Bootstrap.STRINGLIT_233, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2225, null, Bootstrap.STRINGLIT_350, 2, Bootstrap.STRINGLIT_231, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2226, null, Bootstrap.STRINGLIT_351, 2, Bootstrap.STRINGLIT_233, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2227, null, Bootstrap.STRINGLIT_352, 2, Bootstrap.STRINGLIT_233, null);
				return _out;
			case 749:
				_out = new JA_L(8, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2238, null, Bootstrap.STRINGLIT_259, 4, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_260);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2239, null, Bootstrap.STRINGLIT_232, 4, Bootstrap.STRINGLIT_233, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2240, null, Bootstrap.STRINGLIT_246, 4, Bootstrap.STRINGLIT_231, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2241, null, Bootstrap.STRINGLIT_353, 4, Bootstrap.STRINGLIT_354, Bootstrap.STRINGLIT_355);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2242, null, Bootstrap.STRINGLIT_263, 132, Bootstrap.STRINGLIT_233, null);
				_out.data[5] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2243, null, Bootstrap.STRINGLIT_236, 4, Bootstrap.STRINGLIT_231, null);
				_out.data[6] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2244, null, Bootstrap.STRINGLIT_262, 4, Bootstrap.STRINGLIT_231, null);
				_out.data[7] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2245, null, Bootstrap.STRINGLIT_261, 132, Bootstrap.STRINGLIT_233, null);
				return _out;
			case 754:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2253, null, Bootstrap.STRINGLIT_356, 2, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2254, null, Bootstrap.STRINGLIT_357, 2, Bootstrap.STRINGLIT_241, null);
				return _out;
			case 757:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2256, null, Bootstrap.STRINGLIT_310, 132, Bootstrap.STRINGLIT_231, null);
				return _out;
			case 759:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2260, null, Bootstrap.STRINGLIT_324, 4112, Bootstrap.STRINGLIT_358, null);
				return _out;
			default:
				break;
		}
		return null;
	}
	static JA_L getFields1_I__Lj_MemberInfo_(int p0) {
		JA_L _out = null;
		switch (p0) {
			case 760:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2255, null, Bootstrap.STRINGLIT_328, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2257, null, Bootstrap.STRINGLIT_324, 4112, Bootstrap.STRINGLIT_358, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2258, null, Bootstrap.STRINGLIT_359, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2259, null, Bootstrap.STRINGLIT_360, 0, Bootstrap.STRINGLIT_231, null);
				return _out;
			case 767:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2264, null, Bootstrap.STRINGLIT_118, 18, Bootstrap.STRINGLIT_241, Bootstrap.STRINGLIT_361);
				return _out;
			case 772:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2275, null, Bootstrap.STRINGLIT_362, 4112, Bootstrap.STRINGLIT_363, null);
				return _out;
			case 774:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2271, null, Bootstrap.STRINGLIT_118, 1, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2272, null, Bootstrap.STRINGLIT_364, 1, Bootstrap.STRINGLIT_233, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2273, null, Bootstrap.STRINGLIT_365, 1, Bootstrap.STRINGLIT_233, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2274, null, Bootstrap.STRINGLIT_366, 1, Bootstrap.STRINGLIT_231, null);
				return _out;
			case 778:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2276, null, Bootstrap.STRINGLIT_367, 8, Bootstrap.STRINGLIT_226, null);
				return _out;
			case 779:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2277, null, Bootstrap.STRINGLIT_368, 4112, Bootstrap.STRINGLIT_255, null);
				return _out;
			case 781:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2374, null, Bootstrap.STRINGLIT_369, 2, Bootstrap.STRINGLIT_330, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2375, null, Bootstrap.STRINGLIT_370, 2, Bootstrap.STRINGLIT_235, null);
				return _out;
			case 786:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2291, null, Bootstrap.STRINGLIT_371, 10, Bootstrap.STRINGLIT_372, null);
				return _out;
			case 787:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2278, null, Bootstrap.STRINGLIT_368, 4112, Bootstrap.STRINGLIT_255, null);
				return _out;
			case 795:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2279, null, Bootstrap.STRINGLIT_373, 4112, Bootstrap.STRINGLIT_255, null);
				return _out;
			case 797:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2280, null, Bootstrap.STRINGLIT_373, 4112, Bootstrap.STRINGLIT_255, null);
				return _out;
			case 801:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2281, null, Bootstrap.STRINGLIT_374, 4112, Bootstrap.STRINGLIT_248, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2282, null, Bootstrap.STRINGLIT_375, 4112, Bootstrap.STRINGLIT_248, null);
				return _out;
			case 805:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2283, null, Bootstrap.STRINGLIT_376, 4112, Bootstrap.STRINGLIT_248, null);
				return _out;
			case 806:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2284, null, Bootstrap.STRINGLIT_377, 4112, Bootstrap.STRINGLIT_378, null);
				return _out;
			case 807:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2285, null, Bootstrap.STRINGLIT_379, 4112, Bootstrap.STRINGLIT_380, null);
				return _out;
			case 808:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2286, null, Bootstrap.STRINGLIT_381, 4112, Bootstrap.STRINGLIT_255, null);
				return _out;
			case 819:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2287, null, Bootstrap.STRINGLIT_382, 4112, Bootstrap.STRINGLIT_383, null);
				return _out;
			case 820:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2288, null, Bootstrap.STRINGLIT_384, 4112, Bootstrap.STRINGLIT_385, null);
				return _out;
			case 822:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2290, null, Bootstrap.STRINGLIT_386, 9, Bootstrap.STRINGLIT_387, null);
				return _out;
			case 824:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2289, null, Bootstrap.STRINGLIT_388, 1, Bootstrap.STRINGLIT_387, null);
				return _out;
			case 826:
				_out = new JA_L(6, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2292, null, Bootstrap.STRINGLIT_389, 18, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2293, null, Bootstrap.STRINGLIT_390, 18, Bootstrap.STRINGLIT_231, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2294, null, Bootstrap.STRINGLIT_391, 18, Bootstrap.STRINGLIT_280, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2295, null, Bootstrap.STRINGLIT_392, 18, Bootstrap.STRINGLIT_346, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2296, null, Bootstrap.STRINGLIT_393, 10, Bootstrap.STRINGLIT_238, Bootstrap.STRINGLIT_394);
				_out.data[5] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2297, null, Bootstrap.STRINGLIT_395, 10, Bootstrap.STRINGLIT_235, null);
				return _out;
			case 827:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2298, null, Bootstrap.STRINGLIT_396, 4, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2299, null, Bootstrap.STRINGLIT_397, 4, Bootstrap.STRINGLIT_255, null);
				return _out;
			case 828:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2300, null, Bootstrap.STRINGLIT_398, 2, Bootstrap.STRINGLIT_233, null);
				return _out;
			case 829:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2301, null, Bootstrap.STRINGLIT_399, 18, Bootstrap.STRINGLIT_257, Bootstrap.STRINGLIT_400);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2302, null, Bootstrap.STRINGLIT_401, 18, Bootstrap.STRINGLIT_402, Bootstrap.STRINGLIT_403);
				return _out;
			case 831:
				_out = new JA_L(3, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2303, null, Bootstrap.STRINGLIT_404, 16, Bootstrap.STRINGLIT_340, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2304, null, Bootstrap.STRINGLIT_405, 16, Bootstrap.STRINGLIT_233, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2305, null, Bootstrap.STRINGLIT_406, 16, Bootstrap.STRINGLIT_407, Bootstrap.STRINGLIT_408);
				return _out;
			case 832:
				_out = new JA_L(5, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2306, null, Bootstrap.STRINGLIT_409, 25, Bootstrap.STRINGLIT_312, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2307, null, Bootstrap.STRINGLIT_410, 26, Bootstrap.STRINGLIT_411, Bootstrap.STRINGLIT_412);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2308, null, Bootstrap.STRINGLIT_413, 26, Bootstrap.STRINGLIT_414, Bootstrap.STRINGLIT_415);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2309, null, Bootstrap.STRINGLIT_416, 25, Bootstrap.STRINGLIT_407, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2310, null, Bootstrap.STRINGLIT_417, 25, Bootstrap.STRINGLIT_402, null);
				return _out;
			case 842:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2311, null, Bootstrap.STRINGLIT_418, 2, Bootstrap.STRINGLIT_235, null);
				return _out;
			case 844:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2312, null, Bootstrap.STRINGLIT_419, 9, Bootstrap.STRINGLIT_420, null);
				return _out;
			case 850:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2313, null, Bootstrap.STRINGLIT_232, 1, Bootstrap.STRINGLIT_233, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2314, null, Bootstrap.STRINGLIT_421, 2, Bootstrap.STRINGLIT_422, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2315, null, Bootstrap.STRINGLIT_423, 10, Bootstrap.STRINGLIT_424, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2316, null, Bootstrap.STRINGLIT_425, 2, Bootstrap.STRINGLIT_426, null);
				return _out;
			case 854:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2317, null, Bootstrap.STRINGLIT_427, 2, Bootstrap.STRINGLIT_277, Bootstrap.STRINGLIT_428);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2318, null, Bootstrap.STRINGLIT_388, 2, Bootstrap.STRINGLIT_426, null);
				return _out;
			case 855:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2319, null, Bootstrap.STRINGLIT_425, 10, Bootstrap.STRINGLIT_426, null);
				return _out;
			case 857:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2322, null, Bootstrap.STRINGLIT_429, 25, Bootstrap.STRINGLIT_255, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2323, null, Bootstrap.STRINGLIT_430, 25, Bootstrap.STRINGLIT_354, Bootstrap.STRINGLIT_355);
				return _out;
			case 859:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2324, null, Bootstrap.STRINGLIT_366, 1, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2325, null, Bootstrap.STRINGLIT_365, 1, Bootstrap.STRINGLIT_233, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2326, null, Bootstrap.STRINGLIT_364, 1, Bootstrap.STRINGLIT_233, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2327, null, Bootstrap.STRINGLIT_118, 1, Bootstrap.STRINGLIT_231, null);
				return _out;
			case 862:
				_out = new JA_L(6, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2329, null, Bootstrap.STRINGLIT_431, 16, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2330, null, Bootstrap.STRINGLIT_432, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2331, null, Bootstrap.STRINGLIT_433, 16, Bootstrap.STRINGLIT_434, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2332, null, Bootstrap.STRINGLIT_435, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2333, null, Bootstrap.STRINGLIT_436, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[5] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2334, null, Bootstrap.STRINGLIT_437, 16, Bootstrap.STRINGLIT_231, null);
				return _out;
			default:
				break;
		}
		return null;
	}
	static JA_L getFields2_I__Lj_MemberInfo_(int p0) {
		JA_L _out = null;
		switch (p0) {
			case 865:
				_out = new JA_L(7, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2337, null, Bootstrap.STRINGLIT_438, 16, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2338, null, Bootstrap.STRINGLIT_439, 17, Bootstrap.STRINGLIT_255, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2339, null, Bootstrap.STRINGLIT_440, 18, Bootstrap.STRINGLIT_235, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2352, null, Bootstrap.STRINGLIT_441, 0, Bootstrap.STRINGLIT_235, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2353, null, Bootstrap.STRINGLIT_442, 0, Bootstrap.STRINGLIT_235, null);
				_out.data[5] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2354, null, Bootstrap.STRINGLIT_443, 0, Bootstrap.STRINGLIT_444, null);
				_out.data[6] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2368, null, Bootstrap.STRINGLIT_445, 2, Bootstrap.STRINGLIT_235, null);
				return _out;
			case 872:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2344, null, Bootstrap.STRINGLIT_446, 16, Bootstrap.STRINGLIT_447, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2345, null, Bootstrap.STRINGLIT_448, 16, Bootstrap.STRINGLIT_255, null);
				return _out;
			case 874:
				_out = new JA_L(6, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2346, null, Bootstrap.STRINGLIT_232, 18, Bootstrap.STRINGLIT_233, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2347, null, Bootstrap.STRINGLIT_449, 16, Bootstrap.STRINGLIT_235, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2348, null, Bootstrap.STRINGLIT_37, 25, Bootstrap.STRINGLIT_444, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2349, null, Bootstrap.STRINGLIT_450, 26, Bootstrap.STRINGLIT_444, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2350, null, Bootstrap.STRINGLIT_36, 25, Bootstrap.STRINGLIT_444, null);
				_out.data[5] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2351, null, Bootstrap.STRINGLIT_441, 26, Bootstrap.STRINGLIT_235, null);
				return _out;
			case 877:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2355, null, Bootstrap.STRINGLIT_448, 16, Bootstrap.STRINGLIT_255, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2356, null, Bootstrap.STRINGLIT_446, 16, Bootstrap.STRINGLIT_447, null);
				return _out;
			case 881:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2357, null, Bootstrap.STRINGLIT_446, 16, Bootstrap.STRINGLIT_447, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2358, null, Bootstrap.STRINGLIT_448, 16, Bootstrap.STRINGLIT_255, null);
				return _out;
			case 885:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2359, null, Bootstrap.STRINGLIT_451, 10, Bootstrap.STRINGLIT_444, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2360, null, Bootstrap.STRINGLIT_452, 10, Bootstrap.STRINGLIT_444, null);
				return _out;
			case 886:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2361, null, Bootstrap.STRINGLIT_448, 16, Bootstrap.STRINGLIT_255, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2362, null, Bootstrap.STRINGLIT_446, 16, Bootstrap.STRINGLIT_447, null);
				return _out;
			case 890:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2363, null, Bootstrap.STRINGLIT_446, 16, Bootstrap.STRINGLIT_447, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2364, null, Bootstrap.STRINGLIT_448, 16, Bootstrap.STRINGLIT_255, null);
				return _out;
			case 894:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2365, null, Bootstrap.STRINGLIT_446, 16, Bootstrap.STRINGLIT_447, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2366, null, Bootstrap.STRINGLIT_448, 16, Bootstrap.STRINGLIT_255, null);
				return _out;
			case 897:
				_out = new JA_L(3, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2371, null, Bootstrap.STRINGLIT_386, 2, Bootstrap.STRINGLIT_453, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2372, null, Bootstrap.STRINGLIT_454, 0, Bootstrap.STRINGLIT_330, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2373, null, Bootstrap.STRINGLIT_455, 10, Bootstrap.STRINGLIT_255, null);
				return _out;
			case 906:
				_out = new JA_L(5, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2407, null, Bootstrap.STRINGLIT_456, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2408, null, Bootstrap.STRINGLIT_457, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2409, null, Bootstrap.STRINGLIT_458, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2410, null, Bootstrap.STRINGLIT_459, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2411, null, Bootstrap.STRINGLIT_460, 0, Bootstrap.STRINGLIT_231, null);
				return _out;
			case 910:
				_out = new JA_L(8, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2487, null, Bootstrap.STRINGLIT_386, 0, Bootstrap.STRINGLIT_461, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2488, null, Bootstrap.STRINGLIT_462, 2, Bootstrap.STRINGLIT_231, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2489, null, Bootstrap.STRINGLIT_463, 2, Bootstrap.STRINGLIT_231, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2490, null, Bootstrap.STRINGLIT_464, 2, Bootstrap.STRINGLIT_235, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2491, null, Bootstrap.STRINGLIT_465, 2, Bootstrap.STRINGLIT_231, null);
				_out.data[5] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2492, null, Bootstrap.STRINGLIT_466, 2, Bootstrap.STRINGLIT_330, null);
				_out.data[6] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2493, null, Bootstrap.STRINGLIT_467, 2, Bootstrap.STRINGLIT_231, null);
				_out.data[7] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2494, null, Bootstrap.STRINGLIT_468, 2, Bootstrap.STRINGLIT_231, null);
				return _out;
			case 919:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2535, null, Bootstrap.STRINGLIT_469, 4, Bootstrap.STRINGLIT_290, null);
				return _out;
			case 920:
				_out = new JA_L(9, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2514, null, Bootstrap.STRINGLIT_11, 130, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2515, null, Bootstrap.STRINGLIT_301, 130, Bootstrap.STRINGLIT_470, Bootstrap.STRINGLIT_471);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2516, null, Bootstrap.STRINGLIT_304, 26, Bootstrap.STRINGLIT_305, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2517, null, Bootstrap.STRINGLIT_306, 130, Bootstrap.STRINGLIT_231, null);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2518, null, Bootstrap.STRINGLIT_310, 130, Bootstrap.STRINGLIT_231, null);
				_out.data[5] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2526, null, Bootstrap.STRINGLIT_311, 130, Bootstrap.STRINGLIT_312, Bootstrap.STRINGLIT_313);
				_out.data[6] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2527, null, Bootstrap.STRINGLIT_316, 130, Bootstrap.STRINGLIT_312, Bootstrap.STRINGLIT_317);
				_out.data[7] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2528, null, Bootstrap.STRINGLIT_252, 130, Bootstrap.STRINGLIT_314, Bootstrap.STRINGLIT_315);
				_out.data[8] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2532, null, Bootstrap.STRINGLIT_472, 26, Bootstrap.STRINGLIT_473, null);
				return _out;
			case 922:
				_out = new JA_L(4, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2510, null, Bootstrap.STRINGLIT_251, 0, Bootstrap.STRINGLIT_320, Bootstrap.STRINGLIT_323);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2511, null, Bootstrap.STRINGLIT_322, 0, Bootstrap.STRINGLIT_474, Bootstrap.STRINGLIT_475);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2512, null, Bootstrap.STRINGLIT_319, 16, Bootstrap.STRINGLIT_320, Bootstrap.STRINGLIT_321);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2513, null, Bootstrap.STRINGLIT_230, 16, Bootstrap.STRINGLIT_231, null);
				return _out;
			case 924:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2519, null, Bootstrap.STRINGLIT_324, 4112, Bootstrap.STRINGLIT_476, null);
				return _out;
			case 925:
				_out = new JA_L(1, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2524, null, Bootstrap.STRINGLIT_324, 4112, Bootstrap.STRINGLIT_476, null);
				return _out;
			case 926:
				_out = new JA_L(5, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2520, null, Bootstrap.STRINGLIT_328, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2521, null, Bootstrap.STRINGLIT_327, 0, Bootstrap.STRINGLIT_231, null);
				_out.data[2] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2522, null, Bootstrap.STRINGLIT_324, 4112, Bootstrap.STRINGLIT_476, null);
				_out.data[3] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2523, null, Bootstrap.STRINGLIT_326, 0, Bootstrap.STRINGLIT_474, Bootstrap.STRINGLIT_475);
				_out.data[4] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2525, null, Bootstrap.STRINGLIT_329, 0, Bootstrap.STRINGLIT_474, Bootstrap.STRINGLIT_475);
				return _out;
			case 927:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2529, null, Bootstrap.STRINGLIT_232, 2, Bootstrap.STRINGLIT_233, null);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2530, null, Bootstrap.STRINGLIT_477, 2, Bootstrap.STRINGLIT_320, null);
				return _out;
			case 929:
				_out = new JA_L(2, "[Lj.MemberInfo;");
				_out.data[0] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2531, null, Bootstrap.STRINGLIT_478, 2, Bootstrap.STRINGLIT_320, Bootstrap.STRINGLIT_479);
				_out.data[1] = j_MemberInfo.create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2533, null, Bootstrap.STRINGLIT_480, 64, Bootstrap.STRINGLIT_481, Bootstrap.STRINGLIT_482);
				return _out;
			default:
				break;
		}
		return null;
	}
	j_ProgramReflection_AllFields([int CLASS_ID = 745]) : super(CLASS_ID) { }
	static void SI() { }
}
class j_MemberInfo extends java_lang_Object  {

	int _modifiers = 0;
	java_lang_String _desc = null;
	java_lang_String _internalName = null;
	int _id = 0;
	java_lang_String _name = null;
	java_lang_String _genericDesc = null;
	 j_MemberInfo j_MemberInfo_init__ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__V(int p0, java_lang_String p1, java_lang_String p2, int p3, java_lang_String p4, java_lang_String p5) {
		int G = 0;
		java_lang_String fA1 = null;
		while (true) {
			switch (G) {
				case 0:
					this.java_lang_Object_init___V();
					if (((p1 == null))) {
						G = 1;
						continue;
					}
					fA1 = p1;
					G = 2;
					continue;
				case 1:
					fA1 = p2;
					G = 2;
					continue;
				case 2:
					this._internalName = fA1;
					this._id = p0;
					this._name = p2;
					this._modifiers = p3;
					this._desc = p4;
					this._genericDesc = p5;
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	static j_MemberInfo create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(int p0, java_lang_String p1, java_lang_String p2, int p3, java_lang_String p4, java_lang_String p5) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new j_MemberInfo());
		fA0 = tA0;
		(tA0 as j_MemberInfo).j_MemberInfo_init__ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__V(p0, p1, p2, p3, p4, p5);
		return (fA0 as j_MemberInfo);
	}
	static JA_L createList_I_I_I_Ljava_lang_String__Ljava_lang_String__Ljava_lang_String__Ljava_lang_String___Lj_MemberInfo_(int p0, JA_I p1, JA_I p2, JA_L p3, JA_L p4, JA_L p5, JA_L p6) {
		int G = 0;
		java_lang_Object lA7 = null;
		int fI1 = 0;
		int lI8 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lA7 = new JA_L(p0, "[Lj.MemberInfo;");
					lI8 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI8 >= p0))) {
						G = 2;
						continue;
					}
					fA0 = lA7;
					fI1 = lI8;
					tA0 = (new j_MemberInfo());
					fA2 = tA0;
					(tA0 as j_MemberInfo).j_MemberInfo_init__ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__V((p1).data[lI8], (((p3 as JA_L)).data[lI8] as java_lang_String), (((p4 as JA_L)).data[lI8] as java_lang_String), (p2).data[lI8], (((p5 as JA_L)).data[lI8] as java_lang_String), (((p6 as JA_L)).data[lI8] as java_lang_String));
					(fA0 as JA_L).data[fI1] = fA2;
					lI8 = (N.I(lI8 + 1));
					G = 1;
					continue;
				case 2:
					return (lA7 as JA_L);
				default:
					break;
			}
		}
		return null;
	}
	j_MemberInfo([int CLASS_ID = 744]) : super(CLASS_ID) { }
	static void SI() { }
}
class j_ProgramReflection_DynamicGetSet extends java_lang_Object  {

	 j_ProgramReflection_DynamicGetSet j_ProgramReflection_DynamicGetSet_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void dynamicSet_IILjava_lang_Object_Ljava_lang_Object__V(int p0, int p1, java_lang_Object p2, java_lang_Object p3) {
		if (((p1 >= 2374))) {
			j_ProgramReflection_DynamicGetSet.dynamicSet2_IILjava_lang_Object_Ljava_lang_Object__V(p0, p1, p2, p3);
			return;
		}
		if (((p1 >= 2264))) {
			j_ProgramReflection_DynamicGetSet.dynamicSet1_IILjava_lang_Object_Ljava_lang_Object__V(p0, p1, p2, p3);
			return;
		}
		if (((p1 >= 2139))) {
			j_ProgramReflection_DynamicGetSet.dynamicSet0_IILjava_lang_Object_Ljava_lang_Object__V(p0, p1, p2, p3);
			return;
		}
	}
	static java_lang_Object dynamicGet_IILjava_lang_Object__Ljava_lang_Object_(int p0, int p1, java_lang_Object p2) {
		if (((p1 >= 2374))) {
			return j_ProgramReflection_DynamicGetSet.dynamicGet2_IILjava_lang_Object__Ljava_lang_Object_(p0, p1, p2);
		}
		if (((p1 >= 2264))) {
			return j_ProgramReflection_DynamicGetSet.dynamicGet1_IILjava_lang_Object__Ljava_lang_Object_(p0, p1, p2);
		}
		if (((p1 >= 2139))) {
			return j_ProgramReflection_DynamicGetSet.dynamicGet0_IILjava_lang_Object__Ljava_lang_Object_(p0, p1, p2);
		}
		return null;
	}
	static java_lang_Object dynamicGet0_IILjava_lang_Object__Ljava_lang_Object_(int p0, int p1, java_lang_Object p2) {
		switch (p1) {
			case 2139: return java_lang_String._CASE_INSENSITIVE_ORDER;
			case 2140: return (p2 as java_lang_Class)._name;
			case 2141: return N.boxBool((p2 as java_lang_Class)._primitive);
			case 2142: return N.boxInt((p2 as java_lang_Class)._modifiers);
			case 2143: return N.boxInt((p2 as java_lang_Integer)._value);
			case 2144: return java_lang_Integer._values;
			case 2145: return java_lang_Integer._NTZ_TABLE;
			case 2146: return java_lang_Integer._TYPE;
			case 2147: return (p2 as java_lang_reflect_Field)._clazz;
			case 2148: return (p2 as java_lang_reflect_Field)._name;
			case 2149: return N.boxInt((p2 as java_lang_reflect_Field)._modifiers);
			case 2150: return N.boxBool((p2 as java_lang_Throwable)._thrown);
			case 2151: return java_lang_Throwable._EMPTY_ARRAY;
			case 2152: return (p2 as java_lang_Throwable)._message;
			case 2153: return (p2 as java_lang_reflect_Field)._signature;
			case 2154: return java_lang_Void._TYPE;
			case 2155: return N.boxFloat((p2 as java_lang_Float)._value);
			case 2156: return java_lang_Float._TYPE;
			case 2157: return N.boxChar((p2 as java_lang_Character)._value);
			case 2158: return java_lang_Character._TYPE;
			case 2159: return N.boxInt((p2 as java_lang_String)._hash);
			case 2160: return (p2 as java_io_FilterOutputStream).__out;
			case 2161: return (p2 as java_io_PrintStream)._encoding;
			case 2162: return N.boxBool((p2 as java_lang_Throwable)._writableStackTrace);
			case 2163: return N.boxBool((p2 as java_lang_Throwable)._enableSuppression);
			case 2164: return (p2 as java_lang_Throwable)._cause;
			case 2165: return (p2 as java_lang_ClassNotFoundException)._ex;
			case 2166: return java_lang_System.__in;
			case 2167: return java_lang_System.__out;
			case 2168: return java_lang_System._err;
			case 2169: return (p2 as com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream)._sb;
			case 2170: return N.boxBool((p2 as com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream)._error);
			case 2171: return N.boxBool((p2 as com_jtransc_io_JTranscConsolePrintStream)._error);
			case 2172: return (p2 as com_jtransc_io_JTranscConsolePrintStream)._stream;
			case 2173: return N.boxDouble((p2 as java_lang_Double)._value);
			case 2174: return java_lang_Double._TYPE;
			case 2175: return (p2 as java_util_HashMap_HashMapEntry)._key;
			case 2176: return (p2 as java_util_HashMap_HashMapEntry)._next;
			case 2177: return (p2 as java_util_HashMap_HashMapEntry)._value;
			case 2178: return N.boxInt((p2 as java_util_HashMap_HashMapEntry)._hash);
			case 2179: return (p2 as java_util_HashMap)._table;
			case 2180: return N.boxInt((p2 as java_util_HashMap)._threshold);
			case 2181: return java_util_HashMap._EMPTY_TABLE;
			case 2182: return N.boxInt((p2 as java_util_HashMap)._size);
			case 2183: return (p2 as java_util_HashMap_EntrySet)._this_0;
			case 2184: return (p2 as java_util_HashMap_HashIterator)._nextEntry;
			case 2185: return (p2 as java_util_HashMap)._entryForNullKey;
			case 2186: return N.boxInt((p2 as java_util_HashMap)._modCount);
			case 2187: return N.boxInt((p2 as java_util_HashMap_HashIterator)._nextIndex);
			case 2188: return (p2 as java_util_HashMap_HashIterator)._this_0;
			case 2189: return N.boxInt((p2 as java_util_HashMap_HashIterator)._expectedModCount);
			case 2190: return (p2 as java_util_HashMap_EntryIterator)._this_0_;
			case 2191: return (p2 as java_util_HashMap_HashIterator)._lastEntryReturned;
			case 2192: return (p2 as java_util_HashMap)._entrySet;
			case 2194: return N.boxLong((p2 as java_lang_Long)._value);
			case 2195: return java_lang_Long._TYPE;
			case 2196: return N.boxShort((p2 as java_lang_Short)._value);
			case 2197: return java_lang_Short._TYPE;
			case 2198: return java_lang_Boolean._TYPE;
			case 2199: return java_lang_Boolean._TRUE;
			case 2200: return java_lang_Boolean._FALSE;
			case 2201: return N.boxBool((p2 as java_lang_Boolean)._value);
			case 2202: return java_lang_Byte._cache;
			case 2203: return N.boxByte((p2 as java_lang_Byte)._value);
			case 2204: return java_lang_Byte._TYPE;
			case 2205: return java_lang_Class.__classCache;
			case 2206: return N.boxInt(java_lang_SystemInt.___lastId);
			case 2215: return j_ProgramReflection.__classInfos;
			case 2216: return j_ProgramReflection.__classNames;
			case 2217: return j_ProgramReflection.__classInfosByName;
			case 2218: return (p2 as java_lang_Class)._enumConstants;
			case 2219: return (p2 as java_lang_Class).__allFields;
			case 2220: return (p2 as java_lang_Class).__accessibleFields;
			case 2221: return N.boxInt((p2 as java_lang_Class)._id);
			case 2222: return (p2 as java_lang_Class)._related;
			case 2223: return (p2 as java_lang_Class)._info;
			case 2224: return (p2 as java_lang_StackTraceElement)._fileName;
			case 2225: return N.boxInt((p2 as java_lang_StackTraceElement)._lineNumber);
			case 2226: return (p2 as java_lang_StackTraceElement)._methodName;
			case 2227: return (p2 as java_lang_StackTraceElement)._declaringClass;
			case 2228: return (p2 as java_lang_Throwable)._stackTrace;
			case 2229: return N.boxInt((p2 as java_lang_reflect_Field)._slot);
			case 2236: return (p2 as java_lang_reflect_Field)._genericSignature;
			case 2237: return (p2 as java_lang_reflect_AccessibleObject)._info;
			case 2238: return (p2 as java_lang_reflect_MethodConstructor)._clazz;
			case 2239: return (p2 as java_lang_reflect_MethodConstructor)._name;
			case 2240: return N.boxInt((p2 as java_lang_reflect_MethodConstructor)._id);
			case 2241: return (p2 as java_lang_reflect_MethodConstructor)._exceptionTypes;
			case 2242: return (p2 as java_lang_reflect_MethodConstructor)._genericSignature;
			case 2243: return N.boxInt((p2 as java_lang_reflect_MethodConstructor)._modifiers);
			case 2244: return N.boxInt((p2 as java_lang_reflect_MethodConstructor)._slot);
			case 2245: return (p2 as java_lang_reflect_MethodConstructor)._signature;
			case 2253: return N.boxInt((p2 as java_util_ArrayList)._length);
			case 2254: return (p2 as java_util_ArrayList)._buffer;
			case 2255: return N.boxInt((p2 as java_util_AbstractList_SimpleListIterator)._expectedModCount);
			case 2256: return N.boxInt((p2 as java_util_AbstractList)._modCount);
			case 2257: return (p2 as java_util_AbstractList_SimpleListIterator)._this_0;
			case 2258: return N.boxInt((p2 as java_util_AbstractList_SimpleListIterator)._pos);
			case 2259: return N.boxInt((p2 as java_util_AbstractList_SimpleListIterator)._lastPosition);
			case 2260: return (p2 as java_util_AbstractList_FullListIterator)._this_0_;
			default:
				break;
		}
		return null;
	}
	static java_lang_Object dynamicGet1_IILjava_lang_Object__Ljava_lang_Object_(int p0, int p1, java_lang_Object p2) {
		switch (p1) {
			case 2264: return (p2 as java_util_Arrays_ArrayList)._a;
			case 2265: return (p2 as java_util_AbstractMap)._valuesCollection;
			case 2266: return (p2 as java_util_AbstractMap)._keySet;
			case 2267: return (p2 as java_util_HashMap)._values;
			case 2268: return (p2 as java_util_HashMap)._keySet_;
			case 2269: return (p2 as java_lang_Throwable)._supressed;
			case 2270: return N.boxDouble(Benchmark._totalTime);
			case 2271: return N.boxInt((p2 as Benchmark_MyClass2)._a);
			case 2272: return (p2 as Benchmark_MyClass2)._c;
			case 2273: return (p2 as Benchmark_MyClass2)._d;
			case 2274: return N.boxInt((p2 as Benchmark_MyClass2)._b);
			case 2275: return (p2 as Benchmark_37)._val_objects;
			case 2276: return N.boxDouble(com_jtransc_JTranscSystem._start);
			case 2277: return (p2 as Benchmark_39)._val_hexData;
			case 2278: return (p2 as Benchmark_40)._val_hexData;
			case 2279: return (p2 as Benchmark_42)._val_bytes;
			case 2280: return (p2 as Benchmark_41)._val_bytes;
			case 2281: return (p2 as Benchmark_15)._val_srcI;
			case 2282: return (p2 as Benchmark_15)._val_dstI;
			case 2283: return (p2 as Benchmark_19)._val_iarray;
			case 2284: return (p2 as Benchmark_18)._val_carray;
			case 2285: return (p2 as Benchmark_17)._val_sarray;
			case 2286: return (p2 as Benchmark_16)._val_barray;
			case 2287: return (p2 as Benchmark_21)._val_darray;
			case 2288: return (p2 as Benchmark_20)._val_farray;
			case 2289: return (p2 as com_jtransc_time_JTranscClock_Impl)._parent;
			case 2290: return com_jtransc_time_JTranscClock._impl;
			case 2291: return java_lang_Runtime._current;
			case 2292: return N.boxInt((p2 as com_jtransc_charset_JTranscCharset)._max);
			case 2293: return N.boxInt((p2 as com_jtransc_charset_JTranscCharset)._min);
			case 2294: return N.boxFloat((p2 as com_jtransc_charset_JTranscCharset)._avg);
			case 2295: return (p2 as com_jtransc_charset_JTranscCharset)._names;
			case 2296: return com_jtransc_charset_JTranscCharset._charsets;
			case 2297: return N.boxBool(com_jtransc_charset_JTranscCharset._loadedCharsets);
			case 2298: return N.boxInt((p2 as java_io_ByteArrayOutputStream)._count);
			case 2299: return (p2 as java_io_ByteArrayOutputStream)._buf;
			case 2300: return (p2 as java_nio_charset_UnsupportedCharsetException)._charsetName;
			case 2301: return (p2 as java_util_ServiceLoader)._service;
			case 2302: return (p2 as java_util_ServiceLoader)._list;
			case 2303: return N.boxByte((p2 as com_jtransc_charset_JTranscCharsetSingleByte)._invalidChar);
			case 2304: return (p2 as com_jtransc_charset_JTranscCharsetSingleByte)._decode;
			case 2305: return (p2 as com_jtransc_charset_JTranscCharsetSingleByte)._encode;
			case 2306: return java_util_Collections._EMPTY_SET;
			case 2307: return java_util_Collections._EMPTY_ITERATOR;
			case 2308: return java_util_Collections._EMPTY_ENUMERATION;
			case 2309: return java_util_Collections._EMPTY_MAP;
			case 2310: return java_util_Collections._EMPTY_LIST;
			case 2311: return N.boxBool((p2 as com_jtransc_charset_charsets_JTranscCharsetUTF16Base)._littleEndian);
			case 2312: return com_jtransc_mix_JTranscProcessMulti._creator;
			case 2313: return (p2 as java_lang_Thread)._name;
			case 2314: return (p2 as java_lang_Thread)._group;
			case 2315: return java_lang_Thread.__currentThread;
			case 2316: return (p2 as java_lang_Thread)._classLoader;
			case 2317: return (p2 as java_lang_ClassLoader)._nativeLibs;
			case 2318: return (p2 as java_lang_ClassLoader)._parent;
			case 2319: return java_lang__ClassInternalUtils._classLoader;
			case 2320: return N.boxBool((p2 as java_io_PrintStream)._autoFlush);
			case 2321: return N.boxBool((p2 as java_io_PrintStream)._ioError);
			case 2322: return com_jtransc_JTranscArrays._EMPTY_BYTE;
			case 2323: return com_jtransc_JTranscArrays._EMPTY_CLASS;
			case 2324: return N.boxInt((p2 as Benchmark_MyClass)._b);
			case 2325: return (p2 as Benchmark_MyClass)._d;
			case 2326: return (p2 as Benchmark_MyClass)._c;
			case 2327: return N.boxInt((p2 as Benchmark_MyClass)._a);
			case 2329: return N.boxInt((p2 as java_nio_Buffer).__elementSizeShift);
			case 2330: return N.boxInt((p2 as java_nio_Buffer)._mark);
			case 2331: return (p2 as java_nio_Buffer)._block;
			case 2332: return N.boxInt((p2 as java_nio_Buffer)._position);
			case 2333: return N.boxInt((p2 as java_nio_Buffer)._limit);
			case 2334: return N.boxInt((p2 as java_nio_Buffer)._capacity);
			case 2337: return N.boxInt((p2 as java_nio_ByteBuffer)._arrayOffset);
			case 2338: return (p2 as java_nio_ByteBuffer)._backingArray;
			case 2339: return N.boxBool((p2 as java_nio_ByteBuffer)._isReadOnly);
			case 2344: return (p2 as java_nio_ByteBufferAsFloatBuffer)._byteBuffer;
			case 2345: return (p2 as java_nio_ByteBufferAsFloatBuffer)._bytes;
			case 2346: return (p2 as java_nio_ByteOrder)._name;
			case 2347: return N.boxBool((p2 as java_nio_ByteOrder)._needsSwap);
			case 2348: return java_nio_ByteOrder._LITTLE_ENDIAN;
			case 2349: return java_nio_ByteOrder._NATIVE_ORDER;
			case 2350: return java_nio_ByteOrder._BIG_ENDIAN;
			case 2351: return N.boxBool(java_nio_ByteOrder._isLittleEndian);
			case 2352: return N.boxBool((p2 as java_nio_ByteBuffer)._isLittleEndian);
			case 2353: return N.boxBool((p2 as java_nio_ByteBuffer)._isNativeOrder);
			case 2354: return (p2 as java_nio_ByteBuffer)._order;
			case 2355: return (p2 as java_nio_ByteBufferAsLongBuffer)._bytes;
			case 2356: return (p2 as java_nio_ByteBufferAsLongBuffer)._byteBuffer;
			case 2357: return (p2 as java_nio_ByteBufferAsDoubleBuffer)._byteBuffer;
			case 2358: return (p2 as java_nio_ByteBufferAsDoubleBuffer)._bytes;
			case 2359: return libcore_io_Memory._SWAPPED;
			case 2360: return libcore_io_Memory._NATIVE;
			case 2361: return (p2 as java_nio_ByteBufferAsCharBuffer)._bytes;
			case 2362: return (p2 as java_nio_ByteBufferAsCharBuffer)._byteBuffer;
			case 2363: return (p2 as java_nio_ByteBufferAsShortBuffer)._byteBuffer;
			case 2364: return (p2 as java_nio_ByteBufferAsShortBuffer)._bytes;
			case 2365: return (p2 as java_nio_ByteBufferAsIntBuffer)._byteBuffer;
			case 2366: return (p2 as java_nio_ByteBufferAsIntBuffer)._bytes;
			case 2368: return N.boxBool((p2 as java_nio_ByteBuffer)._isDirect);
			case 2371: return (p2 as java_util_zip_CRC32)._impl;
			case 2372: return N.boxLong((p2 as java_util_zip_CRC32)._tbytes);
			case 2373: return java_util_zip_CRC32._temp;
			default:
				break;
		}
		return null;
	}
	static java_lang_Object dynamicGet2_IILjava_lang_Object__Ljava_lang_Object_(int p0, int p1, java_lang_Object p2) {
		switch (p1) {
			case 2374: return N.boxLong((p2 as java_util_Random)._seed);
			case 2375: return N.boxBool((p2 as java_util_Random)._haveNextNextGaussian);
			case 2407: return N.boxInt((p2 as com_jtransc_compression_jzlib_Deflate_Config)._nice_length);
			case 2408: return N.boxInt((p2 as com_jtransc_compression_jzlib_Deflate_Config)._max_chain);
			case 2409: return N.boxInt((p2 as com_jtransc_compression_jzlib_Deflate_Config)._max_lazy);
			case 2410: return N.boxInt((p2 as com_jtransc_compression_jzlib_Deflate_Config)._good_length);
			case 2411: return N.boxInt((p2 as com_jtransc_compression_jzlib_Deflate_Config)._func);
			case 2487: return (p2 as java_util_zip_Deflater)._impl;
			case 2488: return N.boxInt((p2 as java_util_zip_Deflater)._inLength);
			case 2489: return N.boxInt((p2 as java_util_zip_Deflater)._inRead);
			case 2490: return N.boxBool((p2 as java_util_zip_Deflater)._noHeader);
			case 2491: return N.boxInt((p2 as java_util_zip_Deflater)._compressLevel);
			case 2492: return N.boxLong((p2 as java_util_zip_Deflater)._streamHandle);
			case 2493: return N.boxInt((p2 as java_util_zip_Deflater)._flushParm);
			case 2494: return N.boxInt((p2 as java_util_zip_Deflater)._strategy);
			case 2510: return (p2 as java_util_Hashtable_HashtableEntry)._value;
			case 2511: return (p2 as java_util_Hashtable_HashtableEntry)._next;
			case 2512: return (p2 as java_util_Hashtable_HashtableEntry)._key;
			case 2513: return N.boxInt((p2 as java_util_Hashtable_HashtableEntry)._hash);
			case 2514: return N.boxInt((p2 as java_util_Hashtable)._threshold);
			case 2515: return (p2 as java_util_Hashtable)._table;
			case 2516: return java_util_Hashtable._EMPTY_TABLE;
			case 2517: return N.boxInt((p2 as java_util_Hashtable)._size);
			case 2518: return N.boxInt((p2 as java_util_Hashtable)._modCount);
			case 2519: return (p2 as java_util_Hashtable_EntrySet)._this_0;
			case 2520: return N.boxInt((p2 as java_util_Hashtable_HashIterator)._expectedModCount);
			case 2521: return N.boxInt((p2 as java_util_Hashtable_HashIterator)._nextIndex);
			case 2522: return (p2 as java_util_Hashtable_HashIterator)._this_0;
			case 2523: return (p2 as java_util_Hashtable_HashIterator)._nextEntry;
			case 2524: return (p2 as java_util_Hashtable_EntryIterator)._this_0_;
			case 2525: return (p2 as java_util_Hashtable_HashIterator)._lastEntryReturned;
			case 2526: return (p2 as java_util_Hashtable)._entrySet;
			case 2527: return (p2 as java_util_Hashtable)._keySet;
			case 2528: return (p2 as java_util_Hashtable)._values;
			case 2529: return (p2 as java_io_ObjectStreamField)._name;
			case 2530: return (p2 as java_io_ObjectStreamField)._type;
			case 2531: return (p2 as java_lang_ref_Reference)._referent;
			case 2532: return java_util_Hashtable._serialPersistentFields;
			case 2533: return (p2 as java_lang_ref_Reference)._queue;
			case 2534: return java_lang_System.__props;
			case 2535: return (p2 as java_util_Properties)._defaults;
			default:
				break;
		}
		return null;
	}
	static void dynamicSet0_IILjava_lang_Object_Ljava_lang_Object__V(int p0, int p1, java_lang_Object p2, java_lang_Object p3) {
		switch (p1) {
			case 2139: java_lang_String._CASE_INSENSITIVE_ORDER = (p3 as java_util_Comparator); break;
			case 2140: (p2 as java_lang_Class)._name = (p3 as java_lang_String); break;
			case 2141: (p2 as java_lang_Class)._primitive = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2142: (p2 as java_lang_Class)._modifiers = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2143: (p2 as java_lang_Integer)._value = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2144: java_lang_Integer._values = (p3 as JA_L); break;
			case 2145: java_lang_Integer._NTZ_TABLE = (p3 as JA_B); break;
			case 2146: java_lang_Integer._TYPE = (p3 as java_lang_Class); break;
			case 2147: (p2 as java_lang_reflect_Field)._clazz = (p3 as java_lang_Class); break;
			case 2148: (p2 as java_lang_reflect_Field)._name = (p3 as java_lang_String); break;
			case 2149: (p2 as java_lang_reflect_Field)._modifiers = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2150: (p2 as java_lang_Throwable)._thrown = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2151: java_lang_Throwable._EMPTY_ARRAY = (p3 as JA_L); break;
			case 2152: (p2 as java_lang_Throwable)._message = (p3 as java_lang_String); break;
			case 2153: (p2 as java_lang_reflect_Field)._signature = (p3 as java_lang_String); break;
			case 2154: java_lang_Void._TYPE = (p3 as java_lang_Class); break;
			case 2155: (p2 as java_lang_Float)._value = N.unboxFloat((p3 as java_lang_Float)); break;
			case 2156: java_lang_Float._TYPE = (p3 as java_lang_Class); break;
			case 2157: (p2 as java_lang_Character)._value = N.unboxChar((p3 as java_lang_Character)); break;
			case 2158: java_lang_Character._TYPE = (p3 as java_lang_Class); break;
			case 2159: (p2 as java_lang_String)._hash = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2160: (p2 as java_io_FilterOutputStream).__out = (p3 as java_io_OutputStream); break;
			case 2161: (p2 as java_io_PrintStream)._encoding = (p3 as java_lang_String); break;
			case 2162: (p2 as java_lang_Throwable)._writableStackTrace = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2163: (p2 as java_lang_Throwable)._enableSuppression = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2164: (p2 as java_lang_Throwable)._cause = (p3 as java_lang_Throwable); break;
			case 2165: (p2 as java_lang_ClassNotFoundException)._ex = (p3 as java_lang_Throwable); break;
			case 2166: java_lang_System.__in = (p3 as java_io_InputStream); break;
			case 2167: java_lang_System.__out = (p3 as java_io_PrintStream); break;
			case 2168: java_lang_System._err = (p3 as java_io_PrintStream); break;
			case 2169: (p2 as com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream)._sb = (p3 as java_lang_StringBuilder); break;
			case 2170: (p2 as com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream)._error = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2171: (p2 as com_jtransc_io_JTranscConsolePrintStream)._error = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2172: (p2 as com_jtransc_io_JTranscConsolePrintStream)._stream = (p3 as com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream); break;
			case 2173: (p2 as java_lang_Double)._value = N.unboxDouble((p3 as java_lang_Double)); break;
			case 2174: java_lang_Double._TYPE = (p3 as java_lang_Class); break;
			case 2175: (p2 as java_util_HashMap_HashMapEntry)._key = p3; break;
			case 2176: (p2 as java_util_HashMap_HashMapEntry)._next = (p3 as java_util_HashMap_HashMapEntry); break;
			case 2177: (p2 as java_util_HashMap_HashMapEntry)._value = p3; break;
			case 2178: (p2 as java_util_HashMap_HashMapEntry)._hash = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2179: (p2 as java_util_HashMap)._table = (p3 as JA_L); break;
			case 2180: (p2 as java_util_HashMap)._threshold = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2181: java_util_HashMap._EMPTY_TABLE = (p3 as JA_L); break;
			case 2182: (p2 as java_util_HashMap)._size = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2183: (p2 as java_util_HashMap_EntrySet)._this_0 = (p3 as java_util_HashMap); break;
			case 2184: (p2 as java_util_HashMap_HashIterator)._nextEntry = (p3 as java_util_HashMap_HashMapEntry); break;
			case 2185: (p2 as java_util_HashMap)._entryForNullKey = (p3 as java_util_HashMap_HashMapEntry); break;
			case 2186: (p2 as java_util_HashMap)._modCount = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2187: (p2 as java_util_HashMap_HashIterator)._nextIndex = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2188: (p2 as java_util_HashMap_HashIterator)._this_0 = (p3 as java_util_HashMap); break;
			case 2189: (p2 as java_util_HashMap_HashIterator)._expectedModCount = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2190: (p2 as java_util_HashMap_EntryIterator)._this_0_ = (p3 as java_util_HashMap); break;
			case 2191: (p2 as java_util_HashMap_HashIterator)._lastEntryReturned = (p3 as java_util_HashMap_HashMapEntry); break;
			case 2192: (p2 as java_util_HashMap)._entrySet = (p3 as java_util_Set); break;
			case 2194: (p2 as java_lang_Long)._value = N.unboxLong((p3 as java_lang_Long)); break;
			case 2195: java_lang_Long._TYPE = (p3 as java_lang_Class); break;
			case 2196: (p2 as java_lang_Short)._value = N.unboxShort((p3 as java_lang_Short)); break;
			case 2197: java_lang_Short._TYPE = (p3 as java_lang_Class); break;
			case 2198: java_lang_Boolean._TYPE = (p3 as java_lang_Class); break;
			case 2199: java_lang_Boolean._TRUE = (p3 as java_lang_Boolean); break;
			case 2200: java_lang_Boolean._FALSE = (p3 as java_lang_Boolean); break;
			case 2201: (p2 as java_lang_Boolean)._value = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2202: java_lang_Byte._cache = (p3 as JA_L); break;
			case 2203: (p2 as java_lang_Byte)._value = N.unboxByte((p3 as java_lang_Byte)); break;
			case 2204: java_lang_Byte._TYPE = (p3 as java_lang_Class); break;
			case 2205: java_lang_Class.__classCache = (p3 as com_jtransc_ds_FastStringMap); break;
			case 2206: java_lang_SystemInt.___lastId = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2215: j_ProgramReflection.__classInfos = (p3 as JA_L); break;
			case 2216: j_ProgramReflection.__classNames = (p3 as JA_L); break;
			case 2217: j_ProgramReflection.__classInfosByName = (p3 as com_jtransc_ds_FastStringMap); break;
			case 2218: (p2 as java_lang_Class)._enumConstants = (p3 as JA_L); break;
			case 2219: (p2 as java_lang_Class).__allFields = (p3 as JA_L); break;
			case 2220: (p2 as java_lang_Class).__accessibleFields = (p3 as JA_L); break;
			case 2221: (p2 as java_lang_Class)._id = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2222: (p2 as java_lang_Class)._related = (p3 as JA_I); break;
			case 2223: (p2 as java_lang_Class)._info = (p3 as j_ClassInfo); break;
			case 2224: (p2 as java_lang_StackTraceElement)._fileName = (p3 as java_lang_String); break;
			case 2225: (p2 as java_lang_StackTraceElement)._lineNumber = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2226: (p2 as java_lang_StackTraceElement)._methodName = (p3 as java_lang_String); break;
			case 2227: (p2 as java_lang_StackTraceElement)._declaringClass = (p3 as java_lang_String); break;
			case 2228: (p2 as java_lang_Throwable)._stackTrace = (p3 as JA_L); break;
			case 2229: (p2 as java_lang_reflect_Field)._slot = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2236: (p2 as java_lang_reflect_Field)._genericSignature = (p3 as java_lang_String); break;
			case 2237: (p2 as java_lang_reflect_AccessibleObject)._info = (p3 as j_MemberInfo); break;
			case 2238: (p2 as java_lang_reflect_MethodConstructor)._clazz = (p3 as java_lang_Class); break;
			case 2239: (p2 as java_lang_reflect_MethodConstructor)._name = (p3 as java_lang_String); break;
			case 2240: (p2 as java_lang_reflect_MethodConstructor)._id = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2241: (p2 as java_lang_reflect_MethodConstructor)._exceptionTypes = (p3 as JA_L); break;
			case 2242: (p2 as java_lang_reflect_MethodConstructor)._genericSignature = (p3 as java_lang_String); break;
			case 2243: (p2 as java_lang_reflect_MethodConstructor)._modifiers = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2244: (p2 as java_lang_reflect_MethodConstructor)._slot = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2245: (p2 as java_lang_reflect_MethodConstructor)._signature = (p3 as java_lang_String); break;
			case 2253: (p2 as java_util_ArrayList)._length = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2254: (p2 as java_util_ArrayList)._buffer = (p3 as JA_L); break;
			case 2255: (p2 as java_util_AbstractList_SimpleListIterator)._expectedModCount = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2256: (p2 as java_util_AbstractList)._modCount = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2257: (p2 as java_util_AbstractList_SimpleListIterator)._this_0 = (p3 as java_util_AbstractList); break;
			case 2258: (p2 as java_util_AbstractList_SimpleListIterator)._pos = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2259: (p2 as java_util_AbstractList_SimpleListIterator)._lastPosition = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2260: (p2 as java_util_AbstractList_FullListIterator)._this_0_ = (p3 as java_util_AbstractList); break;
			default:
				break;
		}
	}
	static void dynamicSet1_IILjava_lang_Object_Ljava_lang_Object__V(int p0, int p1, java_lang_Object p2, java_lang_Object p3) {
		switch (p1) {
			case 2264: (p2 as java_util_Arrays_ArrayList)._a = (p3 as JA_L); break;
			case 2265: (p2 as java_util_AbstractMap)._valuesCollection = (p3 as java_util_Collection); break;
			case 2266: (p2 as java_util_AbstractMap)._keySet = (p3 as java_util_Set); break;
			case 2267: (p2 as java_util_HashMap)._values = (p3 as java_util_Collection); break;
			case 2268: (p2 as java_util_HashMap)._keySet_ = (p3 as java_util_Set); break;
			case 2269: (p2 as java_lang_Throwable)._supressed = (p3 as java_util_ArrayList); break;
			case 2270: Benchmark._totalTime = N.unboxDouble((p3 as java_lang_Double)); break;
			case 2271: (p2 as Benchmark_MyClass2)._a = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2272: (p2 as Benchmark_MyClass2)._c = (p3 as java_lang_String); break;
			case 2273: (p2 as Benchmark_MyClass2)._d = (p3 as java_lang_String); break;
			case 2274: (p2 as Benchmark_MyClass2)._b = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2275: (p2 as Benchmark_37)._val_objects = (p3 as JA_L); break;
			case 2276: com_jtransc_JTranscSystem._start = N.unboxDouble((p3 as java_lang_Double)); break;
			case 2277: (p2 as Benchmark_39)._val_hexData = (p3 as JA_B); break;
			case 2278: (p2 as Benchmark_40)._val_hexData = (p3 as JA_B); break;
			case 2279: (p2 as Benchmark_42)._val_bytes = (p3 as JA_B); break;
			case 2280: (p2 as Benchmark_41)._val_bytes = (p3 as JA_B); break;
			case 2281: (p2 as Benchmark_15)._val_srcI = (p3 as JA_I); break;
			case 2282: (p2 as Benchmark_15)._val_dstI = (p3 as JA_I); break;
			case 2283: (p2 as Benchmark_19)._val_iarray = (p3 as JA_I); break;
			case 2284: (p2 as Benchmark_18)._val_carray = (p3 as JA_C); break;
			case 2285: (p2 as Benchmark_17)._val_sarray = (p3 as JA_S); break;
			case 2286: (p2 as Benchmark_16)._val_barray = (p3 as JA_B); break;
			case 2287: (p2 as Benchmark_21)._val_darray = (p3 as JA_D); break;
			case 2288: (p2 as Benchmark_20)._val_farray = (p3 as JA_F); break;
			case 2289: (p2 as com_jtransc_time_JTranscClock_Impl)._parent = (p3 as com_jtransc_time_JTranscClock_Impl); break;
			case 2290: com_jtransc_time_JTranscClock._impl = (p3 as com_jtransc_time_JTranscClock_Impl); break;
			case 2291: java_lang_Runtime._current = (p3 as java_lang_Runtime); break;
			case 2292: (p2 as com_jtransc_charset_JTranscCharset)._max = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2293: (p2 as com_jtransc_charset_JTranscCharset)._min = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2294: (p2 as com_jtransc_charset_JTranscCharset)._avg = N.unboxFloat((p3 as java_lang_Float)); break;
			case 2295: (p2 as com_jtransc_charset_JTranscCharset)._names = (p3 as JA_L); break;
			case 2296: com_jtransc_charset_JTranscCharset._charsets = (p3 as com_jtransc_ds_FastStringMap); break;
			case 2297: com_jtransc_charset_JTranscCharset._loadedCharsets = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2298: (p2 as java_io_ByteArrayOutputStream)._count = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2299: (p2 as java_io_ByteArrayOutputStream)._buf = (p3 as JA_B); break;
			case 2300: (p2 as java_nio_charset_UnsupportedCharsetException)._charsetName = (p3 as java_lang_String); break;
			case 2301: (p2 as java_util_ServiceLoader)._service = (p3 as java_lang_Class); break;
			case 2302: (p2 as java_util_ServiceLoader)._list = (p3 as java_util_List); break;
			case 2303: (p2 as com_jtransc_charset_JTranscCharsetSingleByte)._invalidChar = N.unboxByte((p3 as java_lang_Byte)); break;
			case 2304: (p2 as com_jtransc_charset_JTranscCharsetSingleByte)._decode = (p3 as java_lang_String); break;
			case 2305: (p2 as com_jtransc_charset_JTranscCharsetSingleByte)._encode = (p3 as java_util_Map); break;
			case 2306: java_util_Collections._EMPTY_SET = (p3 as java_util_Set); break;
			case 2307: java_util_Collections._EMPTY_ITERATOR = (p3 as java_util_Iterator); break;
			case 2308: java_util_Collections._EMPTY_ENUMERATION = (p3 as java_util_Enumeration); break;
			case 2309: java_util_Collections._EMPTY_MAP = (p3 as java_util_Map); break;
			case 2310: java_util_Collections._EMPTY_LIST = (p3 as java_util_List); break;
			case 2311: (p2 as com_jtransc_charset_charsets_JTranscCharsetUTF16Base)._littleEndian = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2312: com_jtransc_mix_JTranscProcessMulti._creator = (p3 as com_jtransc_mix_JTranscProcessMulti_Creator); break;
			case 2313: (p2 as java_lang_Thread)._name = (p3 as java_lang_String); break;
			case 2314: (p2 as java_lang_Thread)._group = (p3 as java_lang_ThreadGroup); break;
			case 2315: java_lang_Thread.__currentThread = (p3 as java_lang_Thread); break;
			case 2316: (p2 as java_lang_Thread)._classLoader = (p3 as java_lang_ClassLoader); break;
			case 2317: (p2 as java_lang_ClassLoader)._nativeLibs = (p3 as java_util_ArrayList); break;
			case 2318: (p2 as java_lang_ClassLoader)._parent = (p3 as java_lang_ClassLoader); break;
			case 2319: java_lang__ClassInternalUtils._classLoader = (p3 as java_lang_ClassLoader); break;
			case 2320: (p2 as java_io_PrintStream)._autoFlush = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2321: (p2 as java_io_PrintStream)._ioError = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2322: com_jtransc_JTranscArrays._EMPTY_BYTE = (p3 as JA_B); break;
			case 2323: com_jtransc_JTranscArrays._EMPTY_CLASS = (p3 as JA_L); break;
			case 2324: (p2 as Benchmark_MyClass)._b = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2325: (p2 as Benchmark_MyClass)._d = (p3 as java_lang_String); break;
			case 2326: (p2 as Benchmark_MyClass)._c = (p3 as java_lang_String); break;
			case 2327: (p2 as Benchmark_MyClass)._a = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2329: (p2 as java_nio_Buffer).__elementSizeShift = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2330: (p2 as java_nio_Buffer)._mark = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2331: (p2 as java_nio_Buffer)._block = (p3 as java_nio_internal_MemoryBlock); break;
			case 2332: (p2 as java_nio_Buffer)._position = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2333: (p2 as java_nio_Buffer)._limit = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2334: (p2 as java_nio_Buffer)._capacity = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2337: (p2 as java_nio_ByteBuffer)._arrayOffset = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2338: (p2 as java_nio_ByteBuffer)._backingArray = (p3 as JA_B); break;
			case 2339: (p2 as java_nio_ByteBuffer)._isReadOnly = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2344: (p2 as java_nio_ByteBufferAsFloatBuffer)._byteBuffer = (p3 as java_nio_ByteBuffer); break;
			case 2345: (p2 as java_nio_ByteBufferAsFloatBuffer)._bytes = (p3 as JA_B); break;
			case 2346: (p2 as java_nio_ByteOrder)._name = (p3 as java_lang_String); break;
			case 2347: (p2 as java_nio_ByteOrder)._needsSwap = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2348: java_nio_ByteOrder._LITTLE_ENDIAN = (p3 as java_nio_ByteOrder); break;
			case 2349: java_nio_ByteOrder._NATIVE_ORDER = (p3 as java_nio_ByteOrder); break;
			case 2350: java_nio_ByteOrder._BIG_ENDIAN = (p3 as java_nio_ByteOrder); break;
			case 2351: java_nio_ByteOrder._isLittleEndian = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2352: (p2 as java_nio_ByteBuffer)._isLittleEndian = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2353: (p2 as java_nio_ByteBuffer)._isNativeOrder = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2354: (p2 as java_nio_ByteBuffer)._order = (p3 as java_nio_ByteOrder); break;
			case 2355: (p2 as java_nio_ByteBufferAsLongBuffer)._bytes = (p3 as JA_B); break;
			case 2356: (p2 as java_nio_ByteBufferAsLongBuffer)._byteBuffer = (p3 as java_nio_ByteBuffer); break;
			case 2357: (p2 as java_nio_ByteBufferAsDoubleBuffer)._byteBuffer = (p3 as java_nio_ByteBuffer); break;
			case 2358: (p2 as java_nio_ByteBufferAsDoubleBuffer)._bytes = (p3 as JA_B); break;
			case 2359: libcore_io_Memory._SWAPPED = (p3 as java_nio_ByteOrder); break;
			case 2360: libcore_io_Memory._NATIVE = (p3 as java_nio_ByteOrder); break;
			case 2361: (p2 as java_nio_ByteBufferAsCharBuffer)._bytes = (p3 as JA_B); break;
			case 2362: (p2 as java_nio_ByteBufferAsCharBuffer)._byteBuffer = (p3 as java_nio_ByteBuffer); break;
			case 2363: (p2 as java_nio_ByteBufferAsShortBuffer)._byteBuffer = (p3 as java_nio_ByteBuffer); break;
			case 2364: (p2 as java_nio_ByteBufferAsShortBuffer)._bytes = (p3 as JA_B); break;
			case 2365: (p2 as java_nio_ByteBufferAsIntBuffer)._byteBuffer = (p3 as java_nio_ByteBuffer); break;
			case 2366: (p2 as java_nio_ByteBufferAsIntBuffer)._bytes = (p3 as JA_B); break;
			case 2368: (p2 as java_nio_ByteBuffer)._isDirect = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2371: (p2 as java_util_zip_CRC32)._impl = (p3 as com_jtransc_compression_jzlib_CRC32); break;
			case 2372: (p2 as java_util_zip_CRC32)._tbytes = N.unboxLong((p3 as java_lang_Long)); break;
			case 2373: java_util_zip_CRC32._temp = (p3 as JA_B); break;
			default:
				break;
		}
	}
	static void dynamicSet2_IILjava_lang_Object_Ljava_lang_Object__V(int p0, int p1, java_lang_Object p2, java_lang_Object p3) {
		switch (p1) {
			case 2374: (p2 as java_util_Random)._seed = N.unboxLong((p3 as java_lang_Long)); break;
			case 2375: (p2 as java_util_Random)._haveNextNextGaussian = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2407: (p2 as com_jtransc_compression_jzlib_Deflate_Config)._nice_length = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2408: (p2 as com_jtransc_compression_jzlib_Deflate_Config)._max_chain = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2409: (p2 as com_jtransc_compression_jzlib_Deflate_Config)._max_lazy = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2410: (p2 as com_jtransc_compression_jzlib_Deflate_Config)._good_length = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2411: (p2 as com_jtransc_compression_jzlib_Deflate_Config)._func = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2487: (p2 as java_util_zip_Deflater)._impl = (p3 as com_jtransc_compression_jzlib_Deflater); break;
			case 2488: (p2 as java_util_zip_Deflater)._inLength = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2489: (p2 as java_util_zip_Deflater)._inRead = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2490: (p2 as java_util_zip_Deflater)._noHeader = N.unboxBool((p3 as java_lang_Boolean)); break;
			case 2491: (p2 as java_util_zip_Deflater)._compressLevel = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2492: (p2 as java_util_zip_Deflater)._streamHandle = N.unboxLong((p3 as java_lang_Long)); break;
			case 2493: (p2 as java_util_zip_Deflater)._flushParm = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2494: (p2 as java_util_zip_Deflater)._strategy = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2510: (p2 as java_util_Hashtable_HashtableEntry)._value = p3; break;
			case 2511: (p2 as java_util_Hashtable_HashtableEntry)._next = (p3 as java_util_Hashtable_HashtableEntry); break;
			case 2512: (p2 as java_util_Hashtable_HashtableEntry)._key = p3; break;
			case 2513: (p2 as java_util_Hashtable_HashtableEntry)._hash = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2514: (p2 as java_util_Hashtable)._threshold = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2515: (p2 as java_util_Hashtable)._table = (p3 as JA_L); break;
			case 2516: java_util_Hashtable._EMPTY_TABLE = (p3 as JA_L); break;
			case 2517: (p2 as java_util_Hashtable)._size = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2518: (p2 as java_util_Hashtable)._modCount = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2519: (p2 as java_util_Hashtable_EntrySet)._this_0 = (p3 as java_util_Hashtable); break;
			case 2520: (p2 as java_util_Hashtable_HashIterator)._expectedModCount = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2521: (p2 as java_util_Hashtable_HashIterator)._nextIndex = N.unboxInt((p3 as java_lang_Integer)); break;
			case 2522: (p2 as java_util_Hashtable_HashIterator)._this_0 = (p3 as java_util_Hashtable); break;
			case 2523: (p2 as java_util_Hashtable_HashIterator)._nextEntry = (p3 as java_util_Hashtable_HashtableEntry); break;
			case 2524: (p2 as java_util_Hashtable_EntryIterator)._this_0_ = (p3 as java_util_Hashtable); break;
			case 2525: (p2 as java_util_Hashtable_HashIterator)._lastEntryReturned = (p3 as java_util_Hashtable_HashtableEntry); break;
			case 2526: (p2 as java_util_Hashtable)._entrySet = (p3 as java_util_Set); break;
			case 2527: (p2 as java_util_Hashtable)._keySet = (p3 as java_util_Set); break;
			case 2528: (p2 as java_util_Hashtable)._values = (p3 as java_util_Collection); break;
			case 2529: (p2 as java_io_ObjectStreamField)._name = (p3 as java_lang_String); break;
			case 2530: (p2 as java_io_ObjectStreamField)._type = p3; break;
			case 2531: (p2 as java_lang_ref_Reference)._referent = p3; break;
			case 2532: java_util_Hashtable._serialPersistentFields = (p3 as JA_L); break;
			case 2533: (p2 as java_lang_ref_Reference)._queue = (p3 as java_lang_ref_ReferenceQueue); break;
			case 2534: java_lang_System.__props = (p3 as java_util_Properties); break;
			case 2535: (p2 as java_util_Properties)._defaults = (p3 as java_util_Properties); break;
			default:
				break;
		}
	}
	j_ProgramReflection_DynamicGetSet([int CLASS_ID = 743]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_CloneNotSupportedException extends java_lang_Exception  {

	 java_lang_CloneNotSupportedException java_lang_CloneNotSupportedException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_Exception_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	java_lang_CloneNotSupportedException([int CLASS_ID = 742]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_reflect_Array extends java_lang_Object  {

	 java_lang_reflect_Array java_lang_reflect_Array_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static int getLength_Ljava_lang_Object__I(java_lang_Object p0) {
		return (p0 as JA_0).length;
	}
	static java_lang_Object newInstance_Ljava_lang_Class_I_Ljava_lang_Object_(java_lang_Class p0, int p1) {
		int G = 0;
		int fI0 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		java_lang_Object tA3 = null;
		java_lang_Object tA4 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_NullPointerException());
					fA0 = tA0;
					(tA0 as java_lang_NullPointerException).java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_483);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					if (p0.isPrimitive__Z()) {
						G = 2;
						continue;
					}
					fI0 = p1;
					tA1 = (new java_lang_StringBuilder());
					fA1 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					return java_lang_reflect_Array.newObjectInstance_ILjava_lang_String__Ljava_lang_Object_(fI0, (fA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_484).append_Ljava_lang_String__Ljava_lang_StringBuilder_(p0.getName__Ljava_lang_String_()).toString__Ljava_lang_String_());
				case 2:
					if (((p0 != java_lang_Boolean._TYPE))) {
						G = 3;
						continue;
					}
					return new JA_Z(p1);
				case 3:
					if (((p0 != java_lang_Byte._TYPE))) {
						G = 4;
						continue;
					}
					return new JA_B(p1);
				case 4:
					if (((p0 != java_lang_Short._TYPE))) {
						G = 5;
						continue;
					}
					return new JA_S(p1);
				case 5:
					if (((p0 != java_lang_Character._TYPE))) {
						G = 6;
						continue;
					}
					return new JA_C(p1);
				case 6:
					if (((p0 != java_lang_Integer._TYPE))) {
						G = 7;
						continue;
					}
					return new JA_I(p1);
				case 7:
					if (((p0 != java_lang_Long._TYPE))) {
						G = 8;
						continue;
					}
					return new JA_J(p1);
				case 8:
					if (((p0 != java_lang_Float._TYPE))) {
						G = 9;
						continue;
					}
					return new JA_F(p1);
				case 9:
					if (((p0 != java_lang_Double._TYPE))) {
						G = 10;
						continue;
					}
					return new JA_D(p1);
				case 10:
					if (((p0 != java_lang_Void._TYPE))) {
						G = 11;
						continue;
					}
					tA2 = (new java_lang_RuntimeException());
					fA0 = tA2;
					(tA2 as java_lang_RuntimeException).java_lang_RuntimeException_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_485);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 11;
					continue;
				case 11:
					tA3 = (new java_lang_RuntimeException());
					fA0 = tA3;
					fA1 = tA3;
					tA4 = (new java_lang_StringBuilder());
					fA2 = tA4;
					(tA4 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_RuntimeException).java_lang_RuntimeException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_486).append_Ljava_lang_Object__Ljava_lang_StringBuilder_(p0).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					break;
				default:
					break;
			}
		}
		return null;
	}
	static java_lang_Object newObjectInstance_ILjava_lang_String__Ljava_lang_Object_(int p0, java_lang_String p1) {
		return new JA_L(p0, N.istr(p1));
	}
	java_lang_reflect_Array([int CLASS_ID = 741]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_StackTraceElement extends java_lang_Object implements java_io_Serializable {

	java_lang_String _fileName = null;
	int _lineNumber = 0;
	java_lang_String _methodName = null;
	java_lang_String _declaringClass = null;
	 java_lang_StackTraceElement java_lang_StackTraceElement_init__Ljava_lang_String_Ljava_lang_String_Ljava_lang_String_I_V(java_lang_String p0, java_lang_String p1, java_lang_String p2, int p3) {
		int G = 0;
		java_lang_StackTraceElement fA0 = null;
		java_lang_String fA1 = null;
		while (true) {
			switch (G) {
				case 0:
					this.java_lang_Object_init___V();
					fA0 = this;
					if (((p0 == null))) {
						G = 1;
						continue;
					}
					fA1 = p0;
					G = 2;
					continue;
				case 1:
					fA1 = Bootstrap.STRINGLIT_487;
					G = 2;
					continue;
				case 2:
					fA0._declaringClass = fA1;
					fA0 = this;
					if (((p1 == null))) {
						G = 3;
						continue;
					}
					fA1 = p1;
					G = 4;
					continue;
				case 3:
					fA1 = Bootstrap.STRINGLIT_487;
					G = 4;
					continue;
				case 4:
					fA0._methodName = fA1;
					fA0 = this;
					if (((p2 == null))) {
						G = 5;
						continue;
					}
					fA1 = p2;
					G = 6;
					continue;
				case 5:
					fA1 = Bootstrap.STRINGLIT_487;
					G = 6;
					continue;
				case 6:
					fA0._fileName = fA1;
					this._lineNumber = p3;
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					fA0 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getClassName__Ljava_lang_String_()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_223).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this._methodName);
					fA1 = this;
					if (!((fA1 as java_lang_StackTraceElement).isNativeMethod__Z())) {
						G = 1;
						continue;
					}
					fA1 = Bootstrap.STRINGLIT_488;
					G = 2;
					continue;
				case 1:
					if (((this._fileName == null))) {
						G = 3;
						continue;
					}
					if (((this._lineNumber < 0))) {
						G = 3;
						continue;
					}
					tA1 = (new java_lang_StringBuilder());
					fA1 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					fA1 = (fA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_224).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this._fileName).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_52).append_I_Ljava_lang_StringBuilder_(this._lineNumber).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_1).toString__Ljava_lang_String_();
					G = 2;
					continue;
				case 3:
					if (((this._fileName == null))) {
						G = 4;
						continue;
					}
					tA2 = (new java_lang_StringBuilder());
					fA1 = tA2;
					(tA2 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					fA1 = (fA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_224).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this._fileName).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_1).toString__Ljava_lang_String_();
					G = 2;
					continue;
				case 4:
					fA1 = Bootstrap.STRINGLIT_489;
					G = 2;
					continue;
				case 2:
					fA0 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_((fA1 as java_lang_String)).toString__Ljava_lang_String_();
					return (fA0 as java_lang_String);
				default:
					break;
			}
		}
		return null;
	}
	 java_lang_String getClassName__Ljava_lang_String_() {
		return this._declaringClass;
	}
	 bool isNativeMethod__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._lineNumber != -2))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 int hashCode__I() {
		int lI1 = 0;
		lI1 = (N.I((N.I(31 * this._declaringClass.hashCode__I())) + this._methodName.hashCode__I()));
		lI1 = (N.I((N.I(31 * lI1)) + java_util_Objects.hashCode_Ljava_lang_Object__I(this._fileName)));
		lI1 = (N.I((N.I(31 * lI1)) + this._lineNumber));
		return lI1;
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		java_lang_StackTraceElement lA2 = null;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != this))) {
						G = 1;
						continue;
					}
					return true;
				case 1:
					if (((p0) is java_lang_StackTraceElement)) {
						G = 2;
						continue;
					}
					return false;
				case 2:
					lA2 = ((p0) as java_lang_StackTraceElement);
					if (!(lA2._declaringClass.equals_Ljava_lang_Object__Z(this._declaringClass))) {
						G = 3;
						continue;
					}
					if (((lA2._lineNumber != this._lineNumber))) {
						G = 3;
						continue;
					}
					fA0 = this._methodName;
					fA1 = lA2._methodName;
					if (!(java_util_Objects.equals_Ljava_lang_Object_Ljava_lang_Object__Z(fA0, fA1))) {
						G = 3;
						continue;
					}
					fA0 = this._fileName;
					fA1 = lA2._fileName;
					if (!(java_util_Objects.equals_Ljava_lang_Object_Ljava_lang_Object__Z(fA0, fA1))) {
						G = 3;
						continue;
					}
					fI0 = 1;
					G = 4;
					continue;
				case 3:
					fI0 = 0;
					G = 4;
					continue;
				case 4:
					return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	java_lang_StackTraceElement([int CLASS_ID = 740]) : super(CLASS_ID) { }
	static void SI() { }
}
class j_ProgramReflection_AllClasses extends java_lang_Object  {

	 j_ProgramReflection_AllClasses j_ProgramReflection_AllClasses_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static JA_L getAllClasses___Lj_ClassInfo_() {
		JA_L _out = null;
		_out = new JA_L(933, "[Lj.ClassInfo;");
		_out.data[655] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(655, null, Bootstrap.STRINGLIT_490, 33, 656, JA_I.T([]), JA_I.T([655,656]));
		_out.data[656] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(656, null, Bootstrap.STRINGLIT_491, 33, -1, JA_I.T([]), JA_I.T([656]));
		_out.data[657] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(657, null, Bootstrap.STRINGLIT_492, 49, 656, JA_I.T([658,659,660]), JA_I.T([657,656,658,659,660]));
		_out.data[658] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(658, null, Bootstrap.STRINGLIT_493, 1537, -1, JA_I.T([]), JA_I.T([658]));
		_out.data[659] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(659, null, Bootstrap.STRINGLIT_494, 1537, -1, JA_I.T([]), JA_I.T([659]));
		_out.data[660] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(660, null, Bootstrap.STRINGLIT_495, 1537, -1, JA_I.T([]), JA_I.T([660]));
		_out.data[661] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(661, null, Bootstrap.STRINGLIT_496, 32, 656, JA_I.T([662,658]), JA_I.T([661,656,662,658]));
		_out.data[662] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(662, null, Bootstrap.STRINGLIT_497, 1537, -1, JA_I.T([]), JA_I.T([662]));
		_out.data[663] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(663, null, Bootstrap.STRINGLIT_498, 4128, 656, JA_I.T([]), JA_I.T([663,656]));
		_out.data[664] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(664, null, Bootstrap.STRINGLIT_499, 33, 656, JA_I.T([658,665,660]), JA_I.T([664,656,658,665,660]));
		_out.data[665] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(665, null, Bootstrap.STRINGLIT_500, 1537, -1, JA_I.T([]), JA_I.T([665]));
		_out.data[666] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(666, null, Bootstrap.STRINGLIT_501, 49, 656, JA_I.T([658,667,668,670]), JA_I.T([666,656,658,667,668,670,669]));
		_out.data[667] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(667, null, Bootstrap.STRINGLIT_502, 1537, -1, JA_I.T([]), JA_I.T([667]));
		_out.data[668] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(668, null, Bootstrap.STRINGLIT_503, 1537, -1, JA_I.T([669]), JA_I.T([668,669]));
		_out.data[669] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(669, null, Bootstrap.STRINGLIT_504, 1537, -1, JA_I.T([]), JA_I.T([669]));
		_out.data[670] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(670, null, Bootstrap.STRINGLIT_505, 1537, -1, JA_I.T([]), JA_I.T([670]));
		_out.data[671] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(671, null, Bootstrap.STRINGLIT_506, 33, 656, JA_I.T([]), JA_I.T([671,656]));
		_out.data[672] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(672, null, Bootstrap.STRINGLIT_507, 49, 673, JA_I.T([659]), JA_I.T([672,673,656,659]));
		_out.data[673] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(673, null, Bootstrap.STRINGLIT_508, 1057, 656, JA_I.T([]), JA_I.T([673,656]));
		_out.data[674] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(674, null, Bootstrap.STRINGLIT_509, 49, 676, JA_I.T([675]), JA_I.T([674,676,656,675,669]));
		_out.data[675] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(675, null, Bootstrap.STRINGLIT_510, 1537, -1, JA_I.T([]), JA_I.T([675]));
		_out.data[676] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(676, null, Bootstrap.STRINGLIT_511, 1057, 656, JA_I.T([669]), JA_I.T([676,656,669]));
		_out.data[678] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(678, null, Bootstrap.STRINGLIT_512, 33, 679, JA_I.T([]), JA_I.T([678,679,680,681,656,658]));
		_out.data[679] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(679, null, Bootstrap.STRINGLIT_513, 33, 680, JA_I.T([]), JA_I.T([679,680,681,656,658]));
		_out.data[680] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(680, null, Bootstrap.STRINGLIT_514, 33, 681, JA_I.T([]), JA_I.T([680,681,656,658]));
		_out.data[681] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(681, null, Bootstrap.STRINGLIT_515, 33, 656, JA_I.T([658]), JA_I.T([681,656,658]));
		_out.data[682] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(682, null, Bootstrap.STRINGLIT_516, 49, 656, JA_I.T([]), JA_I.T([682,656]));
		_out.data[683] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(683, null, Bootstrap.STRINGLIT_517, 49, 673, JA_I.T([659]), JA_I.T([683,673,656,659]));
		_out.data[684] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(684, null, Bootstrap.STRINGLIT_518, 33, 656, JA_I.T([]), JA_I.T([684,656]));
		_out.data[685] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(685, null, Bootstrap.STRINGLIT_519, 49, 656, JA_I.T([]), JA_I.T([685,656]));
		_out.data[686] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(686, null, Bootstrap.STRINGLIT_520, 49, 656, JA_I.T([658,659]), JA_I.T([686,656,658,659]));
		_out.data[687] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(687, null, Bootstrap.STRINGLIT_521, 33, 656, JA_I.T([]), JA_I.T([687,656]));
		_out.data[688] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(688, null, Bootstrap.STRINGLIT_522, 33, 656, JA_I.T([]), JA_I.T([688,656]));
		_out.data[689] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(689, null, Bootstrap.STRINGLIT_523, 33, 692, JA_I.T([665,690]), JA_I.T([689,692,693,656,665,690,691,694]));
		_out.data[690] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(690, null, Bootstrap.STRINGLIT_524, 1537, -1, JA_I.T([691]), JA_I.T([690,691]));
		_out.data[691] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(691, null, Bootstrap.STRINGLIT_525, 1537, -1, JA_I.T([]), JA_I.T([691]));
		_out.data[692] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(692, null, Bootstrap.STRINGLIT_526, 33, 693, JA_I.T([]), JA_I.T([692,693,656,690,694,691]));
		_out.data[693] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(693, null, Bootstrap.STRINGLIT_527, 1057, 656, JA_I.T([690,694]), JA_I.T([693,656,690,694,691]));
		_out.data[694] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(694, null, Bootstrap.STRINGLIT_528, 1537, -1, JA_I.T([]), JA_I.T([694]));
		_out.data[695] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(695, null, Bootstrap.STRINGLIT_529, 33, 696, JA_I.T([]), JA_I.T([695,696,680,681,656,658]));
		_out.data[696] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(696, null, Bootstrap.STRINGLIT_530, 33, 680, JA_I.T([]), JA_I.T([696,680,681,656,658]));
		_out.data[697] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(697, null, Bootstrap.STRINGLIT_531, 48, 698, JA_I.T([]), JA_I.T([697,698,656,690,691]));
		_out.data[698] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(698, null, Bootstrap.STRINGLIT_532, 1057, 656, JA_I.T([690]), JA_I.T([698,656,690,691]));
		_out.data[699] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(699, null, Bootstrap.STRINGLIT_533, 33, 689, JA_I.T([]), JA_I.T([699,689,692,693,656,665,690,691,694]));
		_out.data[700] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(700, null, Bootstrap.STRINGLIT_534, 32, 701, JA_I.T([]), JA_I.T([700,701,693,656,690,694,691]));
		_out.data[701] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(701, null, Bootstrap.STRINGLIT_535, 1056, 693, JA_I.T([]), JA_I.T([701,693,656,690,694,691]));
		_out.data[702] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(702, null, Bootstrap.STRINGLIT_536, 32, 701, JA_I.T([]), JA_I.T([702,701,693,656,690,694,691]));
		_out.data[703] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(703, null, Bootstrap.STRINGLIT_537, 33, 704, JA_I.T([]), JA_I.T([703,704,696,680,681,656,658]));
		_out.data[704] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(704, null, Bootstrap.STRINGLIT_538, 33, 696, JA_I.T([]), JA_I.T([704,696,680,681,656,658]));
		_out.data[705] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(705, null, Bootstrap.STRINGLIT_539, 33, 696, JA_I.T([]), JA_I.T([705,696,680,681,656,658]));
		_out.data[706] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(706, null, Bootstrap.STRINGLIT_540, 49, 673, JA_I.T([659]), JA_I.T([706,673,656,659]));
		_out.data[708] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(708, null, Bootstrap.STRINGLIT_541, 33, 710, JA_I.T([709,658]), JA_I.T([708,710,656,709,658,711]));
		_out.data[709] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(709, null, Bootstrap.STRINGLIT_542, 1537, -1, JA_I.T([]), JA_I.T([709]));
		_out.data[710] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(710, null, Bootstrap.STRINGLIT_543, 1057, 656, JA_I.T([711]), JA_I.T([710,656,711]));
		_out.data[711] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(711, null, Bootstrap.STRINGLIT_544, 1537, -1, JA_I.T([]), JA_I.T([711]));
		_out.data[712] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(712, null, Bootstrap.STRINGLIT_545, 1537, -1, JA_I.T([]), JA_I.T([712]));
		_out.data[713] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(713, null, Bootstrap.STRINGLIT_546, 32, 656, JA_I.T([712]), JA_I.T([713,656,712]));
		_out.data[714] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(714, null, Bootstrap.STRINGLIT_547, 1537, -1, JA_I.T([]), JA_I.T([714]));
		_out.data[715] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(715, null, Bootstrap.STRINGLIT_548, 1537, -1, JA_I.T([716]), JA_I.T([715,716,717]));
		_out.data[716] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(716, null, Bootstrap.STRINGLIT_549, 1537, -1, JA_I.T([717]), JA_I.T([716,717]));
		_out.data[717] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(717, null, Bootstrap.STRINGLIT_550, 1537, -1, JA_I.T([]), JA_I.T([717]));
		_out.data[718] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(718, null, Bootstrap.STRINGLIT_551, 4128, 656, JA_I.T([]), JA_I.T([718,656]));
		_out.data[719] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(719, null, Bootstrap.STRINGLIT_552, 48, 720, JA_I.T([]), JA_I.T([719,720,721,656,715,716,717]));
		_out.data[720] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(720, null, Bootstrap.STRINGLIT_553, 1057, 721, JA_I.T([715]), JA_I.T([720,721,656,715,716,717]));
		_out.data[721] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(721, null, Bootstrap.STRINGLIT_554, 1057, 656, JA_I.T([716]), JA_I.T([721,656,716,717]));
		_out.data[722] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(722, null, Bootstrap.STRINGLIT_555, 48, 723, JA_I.T([714]), JA_I.T([722,723,656,714]));
		_out.data[723] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(723, null, Bootstrap.STRINGLIT_556, 1056, 656, JA_I.T([]), JA_I.T([723,656]));
		_out.data[724] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(724, null, Bootstrap.STRINGLIT_557, 33, 696, JA_I.T([]), JA_I.T([724,696,680,681,656,658]));
		_out.data[725] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(725, null, Bootstrap.STRINGLIT_558, 33, 696, JA_I.T([]), JA_I.T([725,696,680,681,656,658]));
		_out.data[726] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(726, null, Bootstrap.STRINGLIT_559, 49, 673, JA_I.T([659]), JA_I.T([726,673,656,659]));
		_out.data[727] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(727, null, Bootstrap.STRINGLIT_560, 33, 656, JA_I.T([]), JA_I.T([727,656]));
		_out.data[728] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(728, null, Bootstrap.STRINGLIT_561, 49, 673, JA_I.T([659]), JA_I.T([728,673,656,659]));
		_out.data[729] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(729, null, Bootstrap.STRINGLIT_562, 33, 656, JA_I.T([]), JA_I.T([729,656]));
		_out.data[730] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(730, null, Bootstrap.STRINGLIT_563, 49, 656, JA_I.T([658,659]), JA_I.T([730,656,658,659]));
		_out.data[731] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(731, null, Bootstrap.STRINGLIT_564, 49, 673, JA_I.T([659]), JA_I.T([731,673,656,659]));
		_out.data[732] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(732, null, Bootstrap.STRINGLIT_565, 33, 656, JA_I.T([]), JA_I.T([732,656]));
		_out.data[733] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(733, null, Bootstrap.STRINGLIT_566, 33, 696, JA_I.T([]), JA_I.T([733,696,680,681,656,658]));
		_out.data[734] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(734, null, Bootstrap.STRINGLIT_567, 49, 656, JA_I.T([]), JA_I.T([734,656]));
		_out.data[735] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(735, null, Bootstrap.STRINGLIT_568, 33, 656, JA_I.T([]), JA_I.T([735,656]));
		_out.data[736] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(736, null, Bootstrap.STRINGLIT_569, 33, 656, JA_I.T([]), JA_I.T([736,656]));
		_out.data[737] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(737, null, Bootstrap.STRINGLIT_570, 33, 656, JA_I.T([]), JA_I.T([737,656]));
		_out.data[738] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(738, null, Bootstrap.STRINGLIT_571, 33, 656, JA_I.T([]), JA_I.T([738,656]));
		_out.data[739] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(739, null, Bootstrap.STRINGLIT_572, 33, 696, JA_I.T([]), JA_I.T([739,696,680,681,656,658]));
		_out.data[740] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(740, null, Bootstrap.STRINGLIT_573, 49, 656, JA_I.T([658]), JA_I.T([740,656,658]));
		_out.data[741] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(741, null, Bootstrap.STRINGLIT_574, 49, 656, JA_I.T([]), JA_I.T([741,656]));
		_out.data[742] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(742, null, Bootstrap.STRINGLIT_575, 33, 680, JA_I.T([]), JA_I.T([742,680,681,656,658]));
		_out.data[743] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(743, null, Bootstrap.STRINGLIT_576, 33, 656, JA_I.T([]), JA_I.T([743,656]));
		_out.data[744] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(744, null, Bootstrap.STRINGLIT_577, 33, 656, JA_I.T([]), JA_I.T([744,656]));
		_out.data[745] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(745, null, Bootstrap.STRINGLIT_578, 33, 656, JA_I.T([]), JA_I.T([745,656]));
		_out.data[746] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(746, null, Bootstrap.STRINGLIT_579, 33, 680, JA_I.T([]), JA_I.T([746,680,681,656,658]));
		_out.data[747] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(747, null, Bootstrap.STRINGLIT_580, 33, 679, JA_I.T([]), JA_I.T([747,679,680,681,656,658]));
		_out.data[748] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(748, null, Bootstrap.STRINGLIT_581, 49, 749, JA_I.T([675,668]), JA_I.T([748,749,676,656,675,668,669]));
		_out.data[749] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(749, null, Bootstrap.STRINGLIT_582, 1057, 676, JA_I.T([]), JA_I.T([749,676,656,669]));
		_out.data[752] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(752, null, Bootstrap.STRINGLIT_583, 33, 681, JA_I.T([]), JA_I.T([752,681,656,658]));
		_out.data[754] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(754, null, Bootstrap.STRINGLIT_584, 33, 757, JA_I.T([755,756,709,658]), JA_I.T([754,757,721,656,755,756,709,658,716,717]));
		_out.data[755] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(755, null, Bootstrap.STRINGLIT_585, 1537, -1, JA_I.T([716]), JA_I.T([755,716,717]));
		_out.data[756] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(756, null, Bootstrap.STRINGLIT_586, 1537, -1, JA_I.T([]), JA_I.T([756]));
		_out.data[757] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(757, null, Bootstrap.STRINGLIT_587, 1057, 721, JA_I.T([755]), JA_I.T([757,721,656,755,716,717]));
		_out.data[758] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(758, null, Bootstrap.STRINGLIT_588, 1537, -1, JA_I.T([714]), JA_I.T([758,714]));
		_out.data[759] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(759, null, Bootstrap.STRINGLIT_589, 48, 760, JA_I.T([758]), JA_I.T([759,760,656,758,714]));
		_out.data[760] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(760, null, Bootstrap.STRINGLIT_590, 32, 656, JA_I.T([714]), JA_I.T([760,656,714]));
		_out.data[761] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(761, null, Bootstrap.STRINGLIT_591, 33, 752, JA_I.T([]), JA_I.T([761,752,681,656,658]));
		_out.data[763] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(763, null, Bootstrap.STRINGLIT_592, 1537, -1, JA_I.T([667]), JA_I.T([763,667]));
		_out.data[764] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(764, null, Bootstrap.STRINGLIT_593, 33, 679, JA_I.T([]), JA_I.T([764,679,680,681,656,658]));
		_out.data[765] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(765, null, Bootstrap.STRINGLIT_594, 33, 656, JA_I.T([]), JA_I.T([765,656]));
		_out.data[766] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(766, null, Bootstrap.STRINGLIT_595, 33, 656, JA_I.T([]), JA_I.T([766,656]));
		_out.data[767] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(767, null, Bootstrap.STRINGLIT_596, 32, 757, JA_I.T([755,658,756]), JA_I.T([767,757,721,656,755,658,756,716,717]));
		_out.data[768] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(768, null, Bootstrap.STRINGLIT_597, 9729, -1, JA_I.T([769]), JA_I.T([768,769]));
		_out.data[769] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(769, null, Bootstrap.STRINGLIT_598, 1537, -1, JA_I.T([]), JA_I.T([769]));
		_out.data[770] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(770, null, Bootstrap.STRINGLIT_599, 33, 752, JA_I.T([]), JA_I.T([770,752,681,656,658]));
		_out.data[771] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(771, null, Bootstrap.STRINGLIT_600, 1537, -1, JA_I.T([]), JA_I.T([771]));
		_out.data[772] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(772, null, Bootstrap.STRINGLIT_601, 48, 656, JA_I.T([773]), JA_I.T([772,656,773]));
		_out.data[773] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(773, null, Bootstrap.STRINGLIT_602, 1536, -1, JA_I.T([]), JA_I.T([773]));
		_out.data[774] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(774, null, Bootstrap.STRINGLIT_603, 32, 656, JA_I.T([]), JA_I.T([774,656]));
		_out.data[775] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(775, null, Bootstrap.STRINGLIT_604, 48, 656, JA_I.T([773]), JA_I.T([775,656,773]));
		_out.data[776] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(776, null, Bootstrap.STRINGLIT_605, 48, 656, JA_I.T([773]), JA_I.T([776,656,773]));
		_out.data[777] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(777, null, Bootstrap.STRINGLIT_606, 48, 656, JA_I.T([773]), JA_I.T([777,656,773]));
		_out.data[778] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(778, null, Bootstrap.STRINGLIT_607, 33, 656, JA_I.T([]), JA_I.T([778,656]));
		_out.data[779] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(779, null, Bootstrap.STRINGLIT_608, 48, 656, JA_I.T([773]), JA_I.T([779,656,773]));
		_out.data[780] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(780, null, Bootstrap.STRINGLIT_609, 48, 656, JA_I.T([773]), JA_I.T([780,656,773]));
		_out.data[781] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(781, null, Bootstrap.STRINGLIT_610, 33, 656, JA_I.T([658]), JA_I.T([781,656,658]));
		_out.data[782] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(782, null, Bootstrap.STRINGLIT_611, 48, 656, JA_I.T([773]), JA_I.T([782,656,773]));
		_out.data[783] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(783, null, Bootstrap.STRINGLIT_612, 48, 656, JA_I.T([773]), JA_I.T([783,656,773]));
		_out.data[784] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(784, null, Bootstrap.STRINGLIT_613, 48, 656, JA_I.T([773]), JA_I.T([784,656,773]));
		_out.data[785] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(785, null, Bootstrap.STRINGLIT_614, 48, 656, JA_I.T([773]), JA_I.T([785,656,773]));
		_out.data[786] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(786, null, Bootstrap.STRINGLIT_615, 33, 656, JA_I.T([]), JA_I.T([786,656]));
		_out.data[787] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(787, null, Bootstrap.STRINGLIT_616, 48, 656, JA_I.T([773]), JA_I.T([787,656,773]));
		_out.data[788] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(788, null, Bootstrap.STRINGLIT_617, 48, 656, JA_I.T([773]), JA_I.T([788,656,773]));
		_out.data[789] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(789, null, Bootstrap.STRINGLIT_618, 48, 656, JA_I.T([773]), JA_I.T([789,656,773]));
		_out.data[790] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(790, null, Bootstrap.STRINGLIT_619, 48, 656, JA_I.T([773]), JA_I.T([790,656,773]));
		_out.data[791] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(791, null, Bootstrap.STRINGLIT_620, 48, 656, JA_I.T([773]), JA_I.T([791,656,773]));
		_out.data[792] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(792, null, Bootstrap.STRINGLIT_621, 48, 656, JA_I.T([773]), JA_I.T([792,656,773]));
		_out.data[793] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(793, null, Bootstrap.STRINGLIT_622, 48, 656, JA_I.T([773]), JA_I.T([793,656,773]));
		_out.data[794] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(794, null, Bootstrap.STRINGLIT_623, 48, 656, JA_I.T([773]), JA_I.T([794,656,773]));
		_out.data[795] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(795, null, Bootstrap.STRINGLIT_624, 48, 656, JA_I.T([773]), JA_I.T([795,656,773]));
		_out.data[796] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(796, null, Bootstrap.STRINGLIT_625, 48, 656, JA_I.T([773]), JA_I.T([796,656,773]));
		_out.data[797] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(797, null, Bootstrap.STRINGLIT_626, 48, 656, JA_I.T([773]), JA_I.T([797,656,773]));
		_out.data[798] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(798, null, Bootstrap.STRINGLIT_627, 48, 656, JA_I.T([773]), JA_I.T([798,656,773]));
		_out.data[799] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(799, null, Bootstrap.STRINGLIT_628, 48, 656, JA_I.T([773]), JA_I.T([799,656,773]));
		_out.data[800] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(800, null, Bootstrap.STRINGLIT_629, 48, 656, JA_I.T([773]), JA_I.T([800,656,773]));
		_out.data[801] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(801, null, Bootstrap.STRINGLIT_630, 48, 656, JA_I.T([773]), JA_I.T([801,656,773]));
		_out.data[802] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(802, null, Bootstrap.STRINGLIT_631, 48, 656, JA_I.T([773]), JA_I.T([802,656,773]));
		_out.data[803] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(803, null, Bootstrap.STRINGLIT_632, 48, 656, JA_I.T([773]), JA_I.T([803,656,773]));
		_out.data[804] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(804, null, Bootstrap.STRINGLIT_633, 48, 656, JA_I.T([773]), JA_I.T([804,656,773]));
		_out.data[805] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(805, null, Bootstrap.STRINGLIT_634, 48, 656, JA_I.T([773]), JA_I.T([805,656,773]));
		_out.data[806] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(806, null, Bootstrap.STRINGLIT_635, 48, 656, JA_I.T([773]), JA_I.T([806,656,773]));
		_out.data[807] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(807, null, Bootstrap.STRINGLIT_636, 48, 656, JA_I.T([773]), JA_I.T([807,656,773]));
		_out.data[808] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(808, null, Bootstrap.STRINGLIT_637, 48, 656, JA_I.T([773]), JA_I.T([808,656,773]));
		_out.data[809] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(809, null, Bootstrap.STRINGLIT_638, 48, 656, JA_I.T([773]), JA_I.T([809,656,773]));
		_out.data[810] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(810, null, Bootstrap.STRINGLIT_639, 48, 656, JA_I.T([773]), JA_I.T([810,656,773]));
		_out.data[811] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(811, null, Bootstrap.STRINGLIT_640, 48, 656, JA_I.T([773]), JA_I.T([811,656,773]));
		_out.data[812] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(812, null, Bootstrap.STRINGLIT_641, 48, 656, JA_I.T([773]), JA_I.T([812,656,773]));
		_out.data[813] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(813, null, Bootstrap.STRINGLIT_642, 48, 656, JA_I.T([773]), JA_I.T([813,656,773]));
		_out.data[814] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(814, null, Bootstrap.STRINGLIT_643, 48, 656, JA_I.T([773]), JA_I.T([814,656,773]));
		_out.data[815] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(815, null, Bootstrap.STRINGLIT_644, 48, 656, JA_I.T([773]), JA_I.T([815,656,773]));
		_out.data[816] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(816, null, Bootstrap.STRINGLIT_645, 48, 656, JA_I.T([773]), JA_I.T([816,656,773]));
		_out.data[817] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(817, null, Bootstrap.STRINGLIT_646, 48, 656, JA_I.T([773]), JA_I.T([817,656,773]));
		_out.data[818] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(818, null, Bootstrap.STRINGLIT_647, 48, 656, JA_I.T([773]), JA_I.T([818,656,773]));
		_out.data[819] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(819, null, Bootstrap.STRINGLIT_648, 48, 656, JA_I.T([773]), JA_I.T([819,656,773]));
		_out.data[820] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(820, null, Bootstrap.STRINGLIT_649, 48, 656, JA_I.T([773]), JA_I.T([820,656,773]));
		_out.data[821] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(821, null, Bootstrap.STRINGLIT_650, 33, 656, JA_I.T([]), JA_I.T([821,656]));
		_out.data[822] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(822, null, Bootstrap.STRINGLIT_651, 33, 656, JA_I.T([]), JA_I.T([822,656]));
		_out.data[823] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(823, null, Bootstrap.STRINGLIT_652, 48, 824, JA_I.T([]), JA_I.T([823,824,656]));
		_out.data[824] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(824, null, Bootstrap.STRINGLIT_653, 33, 656, JA_I.T([]), JA_I.T([824,656]));
		_out.data[825] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(825, null, Bootstrap.STRINGLIT_654, 33, 680, JA_I.T([]), JA_I.T([825,680,681,656,658]));
		_out.data[826] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(826, null, Bootstrap.STRINGLIT_112, 1057, 656, JA_I.T([]), JA_I.T([826,656]));
		_out.data[827] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(827, null, Bootstrap.STRINGLIT_655, 33, 693, JA_I.T([]), JA_I.T([827,693,656,690,694,691]));
		_out.data[828] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(828, null, Bootstrap.STRINGLIT_656, 33, 705, JA_I.T([]), JA_I.T([828,705,696,680,681,656,658]));
		_out.data[829] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(829, null, Bootstrap.STRINGLIT_657, 49, 656, JA_I.T([717]), JA_I.T([829,656,717]));
		_out.data[830] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(830, null, Bootstrap.STRINGLIT_658, 33, 831, JA_I.T([]), JA_I.T([830,831,826,656]));
		_out.data[831] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(831, null, Bootstrap.STRINGLIT_659, 33, 826, JA_I.T([]), JA_I.T([831,826,656]));
		_out.data[832] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(832, null, Bootstrap.STRINGLIT_660, 33, 656, JA_I.T([]), JA_I.T([832,656]));
		_out.data[833] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(833, null, Bootstrap.STRINGLIT_661, 48, 656, JA_I.T([714]), JA_I.T([833,656,714]));
		_out.data[834] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(834, null, Bootstrap.STRINGLIT_662, 48, 757, JA_I.T([756,658]), JA_I.T([834,757,721,656,756,658,755,716,717]));
		_out.data[835] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(835, null, Bootstrap.STRINGLIT_663, 48, 656, JA_I.T([836]), JA_I.T([835,656,836]));
		_out.data[836] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(836, null, Bootstrap.STRINGLIT_664, 1537, -1, JA_I.T([]), JA_I.T([836]));
		_out.data[837] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(837, null, Bootstrap.STRINGLIT_665, 48, 710, JA_I.T([658]), JA_I.T([837,710,656,658,711]));
		_out.data[838] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(838, null, Bootstrap.STRINGLIT_666, 48, 720, JA_I.T([658]), JA_I.T([838,720,721,656,658,715,716,717]));
		_out.data[839] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(839, null, Bootstrap.STRINGLIT_667, 33, 826, JA_I.T([]), JA_I.T([839,826,656]));
		_out.data[840] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(840, null, Bootstrap.STRINGLIT_668, 33, 831, JA_I.T([]), JA_I.T([840,831,826,656]));
		_out.data[841] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(841, null, Bootstrap.STRINGLIT_669, 33, 842, JA_I.T([]), JA_I.T([841,842,826,656]));
		_out.data[842] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(842, null, Bootstrap.STRINGLIT_670, 1056, 826, JA_I.T([]), JA_I.T([842,826,656]));
		_out.data[844] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(844, null, Bootstrap.STRINGLIT_671, 33, 845, JA_I.T([]), JA_I.T([844,845,846,656]));
		_out.data[845] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(845, null, Bootstrap.STRINGLIT_113, 1057, 846, JA_I.T([]), JA_I.T([845,846,656]));
		_out.data[846] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(846, null, Bootstrap.STRINGLIT_672, 1057, 656, JA_I.T([]), JA_I.T([846,656]));
		_out.data[847] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(847, null, Bootstrap.STRINGLIT_673, 33, 656, JA_I.T([]), JA_I.T([847,656]));
		_out.data[848] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(848, null, Bootstrap.STRINGLIT_674, 33, 831, JA_I.T([]), JA_I.T([848,831,826,656]));
		_out.data[849] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(849, null, Bootstrap.STRINGLIT_675, 33, 842, JA_I.T([]), JA_I.T([849,842,826,656]));
		_out.data[850] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(850, null, Bootstrap.STRINGLIT_676, 33, 656, JA_I.T([851]), JA_I.T([850,656,851]));
		_out.data[851] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(851, null, Bootstrap.STRINGLIT_677, 1537, -1, JA_I.T([]), JA_I.T([851]));
		_out.data[852] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(852, null, Bootstrap.STRINGLIT_678, 33, 656, JA_I.T([853]), JA_I.T([852,656,853]));
		_out.data[853] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(853, null, Bootstrap.STRINGLIT_679, 1537, -1, JA_I.T([]), JA_I.T([853]));
		_out.data[854] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(854, null, Bootstrap.STRINGLIT_680, 1057, 656, JA_I.T([]), JA_I.T([854,656]));
		_out.data[855] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(855, null, Bootstrap.STRINGLIT_681, 32, 656, JA_I.T([]), JA_I.T([855,656]));
		_out.data[856] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(856, null, Bootstrap.STRINGLIT_682, 48, 854, JA_I.T([]), JA_I.T([856,854,656]));
		_out.data[857] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(857, null, Bootstrap.STRINGLIT_683, 33, 656, JA_I.T([]), JA_I.T([857,656]));
		_out.data[858] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(858, null, Bootstrap.STRINGLIT_684, 33, 656, JA_I.T([]), JA_I.T([858,656]));
		_out.data[859] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(859, null, Bootstrap.STRINGLIT_685, 32, 656, JA_I.T([]), JA_I.T([859,656]));
		_out.data[861] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(861, null, Bootstrap.STRINGLIT_686, 1057, 862, JA_I.T([659]), JA_I.T([861,862,656,659]));
		_out.data[862] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(862, null, Bootstrap.STRINGLIT_687, 1057, 656, JA_I.T([]), JA_I.T([862,656]));
		_out.data[863] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(863, null, Bootstrap.STRINGLIT_688, 33, 656, JA_I.T([]), JA_I.T([863,656]));
		_out.data[864] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(864, null, Bootstrap.STRINGLIT_689, 1057, 862, JA_I.T([659]), JA_I.T([864,862,656,659]));
		_out.data[865] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(865, null, Bootstrap.STRINGLIT_690, 33, 862, JA_I.T([659]), JA_I.T([865,862,656,659]));
		_out.data[866] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(866, null, Bootstrap.STRINGLIT_691, 33, 739, JA_I.T([]), JA_I.T([866,739,696,680,681,656,658]));
		_out.data[867] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(867, null, Bootstrap.STRINGLIT_692, 1057, 862, JA_I.T([659,660,665,868]), JA_I.T([867,862,656,659,660,665,868]));
		_out.data[868] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(868, null, Bootstrap.STRINGLIT_693, 1537, -1, JA_I.T([]), JA_I.T([868]));
		_out.data[869] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(869, null, Bootstrap.STRINGLIT_694, 1057, 862, JA_I.T([659]), JA_I.T([869,862,656,659]));
		_out.data[870] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(870, null, Bootstrap.STRINGLIT_695, 1057, 862, JA_I.T([659]), JA_I.T([870,862,656,659]));
		_out.data[871] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(871, null, Bootstrap.STRINGLIT_696, 1057, 862, JA_I.T([659]), JA_I.T([871,862,656,659]));
		_out.data[872] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(872, null, Bootstrap.STRINGLIT_697, 32, 864, JA_I.T([873]), JA_I.T([872,864,862,656,873,659]));
		_out.data[873] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(873, null, Bootstrap.STRINGLIT_698, 1537, -1, JA_I.T([]), JA_I.T([873]));
		_out.data[874] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(874, null, Bootstrap.STRINGLIT_699, 49, 656, JA_I.T([]), JA_I.T([874,656]));
		_out.data[875] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(875, null, Bootstrap.STRINGLIT_700, 49, 872, JA_I.T([]), JA_I.T([875,872,864,862,656,873,659]));
		_out.data[876] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(876, null, Bootstrap.STRINGLIT_701, 49, 872, JA_I.T([]), JA_I.T([876,872,864,862,656,873,659]));
		_out.data[877] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(877, null, Bootstrap.STRINGLIT_702, 1056, 871, JA_I.T([873]), JA_I.T([877,871,862,656,873,659]));
		_out.data[878] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(878, null, Bootstrap.STRINGLIT_703, 49, 877, JA_I.T([]), JA_I.T([878,877,871,862,656,873,659]));
		_out.data[879] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(879, null, Bootstrap.STRINGLIT_704, 4128, 656, JA_I.T([]), JA_I.T([879,656]));
		_out.data[880] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(880, null, Bootstrap.STRINGLIT_705, 49, 877, JA_I.T([]), JA_I.T([880,877,871,862,656,873,659]));
		_out.data[881] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(881, null, Bootstrap.STRINGLIT_706, 1056, 869, JA_I.T([873]), JA_I.T([881,869,862,656,873,659]));
		_out.data[882] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(882, null, Bootstrap.STRINGLIT_707, 49, 881, JA_I.T([]), JA_I.T([882,881,869,862,656,873,659]));
		_out.data[883] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(883, null, Bootstrap.STRINGLIT_708, 4128, 656, JA_I.T([]), JA_I.T([883,656]));
		_out.data[884] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(884, null, Bootstrap.STRINGLIT_709, 49, 881, JA_I.T([]), JA_I.T([884,881,869,862,656,873,659]));
		_out.data[885] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(885, null, Bootstrap.STRINGLIT_710, 33, 656, JA_I.T([]), JA_I.T([885,656]));
		_out.data[886] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(886, null, Bootstrap.STRINGLIT_711, 1056, 867, JA_I.T([873]), JA_I.T([886,867,862,656,873,659,660,665,868]));
		_out.data[887] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(887, null, Bootstrap.STRINGLIT_712, 49, 886, JA_I.T([]), JA_I.T([887,886,867,862,656,873,659,660,665,868]));
		_out.data[888] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(888, null, Bootstrap.STRINGLIT_713, 4128, 656, JA_I.T([]), JA_I.T([888,656]));
		_out.data[889] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(889, null, Bootstrap.STRINGLIT_714, 49, 886, JA_I.T([]), JA_I.T([889,886,867,862,656,873,659,660,665,868]));
		_out.data[890] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(890, null, Bootstrap.STRINGLIT_715, 32, 870, JA_I.T([873]), JA_I.T([890,870,862,656,873,659]));
		_out.data[891] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(891, null, Bootstrap.STRINGLIT_716, 49, 890, JA_I.T([]), JA_I.T([891,890,870,862,656,873,659]));
		_out.data[892] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(892, null, Bootstrap.STRINGLIT_717, 4128, 656, JA_I.T([]), JA_I.T([892,656]));
		_out.data[893] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(893, null, Bootstrap.STRINGLIT_718, 49, 890, JA_I.T([]), JA_I.T([893,890,870,862,656,873,659]));
		_out.data[894] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(894, null, Bootstrap.STRINGLIT_719, 1056, 861, JA_I.T([873]), JA_I.T([894,861,862,656,873,659]));
		_out.data[895] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(895, null, Bootstrap.STRINGLIT_720, 49, 894, JA_I.T([]), JA_I.T([895,894,861,862,656,873,659]));
		_out.data[896] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(896, null, Bootstrap.STRINGLIT_721, 49, 894, JA_I.T([]), JA_I.T([896,894,861,862,656,873,659]));
		_out.data[897] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(897, null, Bootstrap.STRINGLIT_722, 33, 656, JA_I.T([898]), JA_I.T([897,656,898]));
		_out.data[898] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(898, null, Bootstrap.STRINGLIT_723, 1537, -1, JA_I.T([]), JA_I.T([898]));
		_out.data[906] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(906, null, Bootstrap.STRINGLIT_724, 32, 656, JA_I.T([]), JA_I.T([906,656]));
		_out.data[910] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(910, null, Bootstrap.STRINGLIT_725, 33, 656, JA_I.T([]), JA_I.T([910,656]));
		_out.data[911] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(911, null, Bootstrap.STRINGLIT_726, 32, 656, JA_I.T([]), JA_I.T([911,656]));
		_out.data[912] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(912, null, Bootstrap.STRINGLIT_727, 32, 656, JA_I.T([]), JA_I.T([912,656]));
		_out.data[919] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(919, null, Bootstrap.STRINGLIT_728, 33, 920, JA_I.T([]), JA_I.T([919,920,921,656,711,709,658]));
		_out.data[920] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(920, null, Bootstrap.STRINGLIT_729, 33, 921, JA_I.T([711,709,658]), JA_I.T([920,921,656,711,709,658]));
		_out.data[921] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(921, null, Bootstrap.STRINGLIT_730, 1057, 656, JA_I.T([]), JA_I.T([921,656]));
		_out.data[922] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(922, null, Bootstrap.STRINGLIT_731, 32, 656, JA_I.T([712]), JA_I.T([922,656,712]));
		_out.data[923] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(923, null, Bootstrap.STRINGLIT_732, 4128, 656, JA_I.T([]), JA_I.T([923,656]));
		_out.data[924] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(924, null, Bootstrap.STRINGLIT_733, 48, 720, JA_I.T([]), JA_I.T([924,720,721,656,715,716,717]));
		_out.data[925] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(925, null, Bootstrap.STRINGLIT_734, 48, 926, JA_I.T([714]), JA_I.T([925,926,656,714]));
		_out.data[926] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(926, null, Bootstrap.STRINGLIT_735, 1056, 656, JA_I.T([]), JA_I.T([926,656]));
		_out.data[927] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(927, null, Bootstrap.STRINGLIT_736, 33, 656, JA_I.T([659]), JA_I.T([927,656,659]));
		_out.data[928] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(928, null, Bootstrap.STRINGLIT_737, 33, 929, JA_I.T([]), JA_I.T([928,929,656]));
		_out.data[929] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(929, null, Bootstrap.STRINGLIT_738, 1057, 656, JA_I.T([]), JA_I.T([929,656]));
		_out.data[930] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(930, null, Bootstrap.STRINGLIT_739, 33, 656, JA_I.T([]), JA_I.T([930,656]));
		_out.data[931] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(931, null, Bootstrap.STRINGLIT_740, 1, 656, JA_I.T([768,769]), JA_I.T([931,656,768,769]));
		_out.data[932] = j_ClassInfo.create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(932, null, Bootstrap.STRINGLIT_741, 1, 656, JA_I.T([769,769]), JA_I.T([932,656,769]));
		return _out;
	}
	j_ProgramReflection_AllClasses([int CLASS_ID = 738]) : super(CLASS_ID) { }
	static void SI() { }
}
class j_ClassInfo extends java_lang_Object  {

	JA_I _interfaces = null;
	int _modifiers = 0;
	int _id = 0;
	java_lang_String _internalName = null;
	JA_I _related = null;
	java_lang_String _name = null;
	int _parent = 0;
	 j_ClassInfo j_ClassInfo_init__ILjava_lang_String_Ljava_lang_String_II_I_I_V(int p0, java_lang_String p1, java_lang_String p2, int p3, int p4, JA_I p5, JA_I p6) {
		int G = 0;
		java_lang_Object lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lA2 = p1;
					this.java_lang_Object_init___V();
					if (((lA2 != null))) {
						G = 1;
						continue;
					}
					lA2 = p2;
					G = 1;
					continue;
				case 1:
					this._id = p0;
					this._internalName = (lA2 as java_lang_String);
					this._name = p2;
					this._modifiers = p3;
					this._parent = p4;
					this._interfaces = p5;
					this._related = p6;
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	static j_ClassInfo create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(int p0, java_lang_String p1, java_lang_String p2, int p3, int p4, JA_I p5, JA_I p6) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new j_ClassInfo());
		fA0 = tA0;
		(tA0 as j_ClassInfo).j_ClassInfo_init__ILjava_lang_String_Ljava_lang_String_II_I_I_V(p0, p1, p2, p3, p4, p5, p6);
		return (fA0 as j_ClassInfo);
	}
	j_ClassInfo([int CLASS_ID = 737]) : super(CLASS_ID) { }
	static void SI() { }
}
class j_ProgramReflection extends java_lang_Object  {

	static JA_L __classInfos = null;
	static JA_L __classNames = null;
	static com_jtransc_ds_FastStringMap __classInfosByName = null;
	 j_ProgramReflection j_ProgramReflection_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void _ensure__V() {
		int G = 0;
		java_lang_Object lA0 = null;
		java_lang_Object lA3 = null;
		int lI1 = 0;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((j_ProgramReflection.__classInfos == null))) {
						G = 1;
						continue;
					}
					return;
					G = 1;
					continue;
				case 1:
					tA0 = (new com_jtransc_ds_FastStringMap());
					fA0 = tA0;
					(tA0 as com_jtransc_ds_FastStringMap).com_jtransc_ds_FastStringMap_init___V();
					j_ProgramReflection.__classInfosByName = (fA0 as com_jtransc_ds_FastStringMap);
					j_ProgramReflection.__classInfos = j_ProgramReflection.getAllClasses___Lj_ClassInfo_();
					if (((j_ProgramReflection.__classInfos == null))) {
						G = 2;
						continue;
					}
					j_ProgramReflection.__classNames = new JA_L((j_ProgramReflection.__classInfos as JA_0).length, "[Ljava.lang.String;");
					lA0 = j_ProgramReflection.__classInfos;
					lI1 = (lA0 as JA_0).length;
					lI2 = 0;
					G = 3;
					continue;
				case 3:
					if (((lI2 >= lI1))) {
						G = 2;
						continue;
					}
					lA3 = ((lA0 as JA_L)).data[lI2];
					if (((lA3 != null))) {
						G = 4;
						continue;
					}
					G = 5;
					continue;
				case 4:
					j_ProgramReflection.__classInfosByName.set_Ljava_lang_String_Ljava_lang_Object__V((lA3 as j_ClassInfo)._name, lA3);
					(j_ProgramReflection.__classNames as JA_L).data[(lA3 as j_ClassInfo)._id] = (lA3 as j_ClassInfo)._name;
					G = 5;
					continue;
				case 5:
					lI2 = (N.I(lI2 + 1));
					G = 3;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	static JA_L getAllClasses___Lj_ClassInfo_() {
		int G = 0;
		JA_L fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((j_ProgramReflection.__classInfos == null))) {
						G = 1;
						continue;
					}
					fA0 = j_ProgramReflection.__classInfos;
					G = 2;
					continue;
				case 1:
					fA0 = j_ProgramReflection_AllClasses.getAllClasses___Lj_ClassInfo_();
					G = 2;
					continue;
				case 2: return fA0;
				default:
					break;
			}
		}
		return null;
	}
	static j_ClassInfo getClassInfoWithName_Ljava_lang_String__Lj_ClassInfo_(java_lang_String p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					j_ProgramReflection._ensure__V();
					if (!(j_ProgramReflection.hasClassWithName_Ljava_lang_String__Z(p0))) {
						G = 1;
						continue;
					}
					fA0 = ((j_ProgramReflection.__classInfosByName.get_Ljava_lang_String__Ljava_lang_Object_(p0)) as j_ClassInfo);
					G = 2;
					continue;
				case 1:
					fA0 = null;
					G = 2;
					continue;
				case 2: return (fA0 as j_ClassInfo);
				default:
					break;
			}
		}
		return null;
	}
	static bool hasClassWithName_Ljava_lang_String__Z(java_lang_String p0) {
		j_ProgramReflection._ensure__V();
		return j_ProgramReflection.__classInfosByName.has_Ljava_lang_String__Z(p0);
	}
	static void dynamicSet_IILjava_lang_Object_Ljava_lang_Object__V(int p0, int p1, java_lang_Object p2, java_lang_Object p3) {
		j_ProgramReflection_DynamicGetSet.dynamicSet_IILjava_lang_Object_Ljava_lang_Object__V(p0, p1, p2, p3);
		return;
	}
	static java_lang_Object dynamicGet_IILjava_lang_Object__Ljava_lang_Object_(int p0, int p1, java_lang_Object p2) {
		return j_ProgramReflection_DynamicGetSet.dynamicGet_IILjava_lang_Object__Ljava_lang_Object_(p0, p1, p2);
	}
	static JA_L getFields_I__Lj_MemberInfo_(int p0) {
		return j_ProgramReflection_AllFields.getFields_I__Lj_MemberInfo_(p0);
	}
	static java_lang_Object dynamicNew_II_Ljava_lang_Object__Ljava_lang_Object_(int p0, int p1, JA_L p2) {
		return j_ProgramReflection_DynamicNewInvoke.dynamicNew_II_Ljava_lang_Object__Ljava_lang_Object_(p0, p1, p2);
	}
	static JA_L getConstructors_I__Lj_MemberInfo_(int p0) {
		return j_ProgramReflection_AllConstructors.getConstructors_I__Lj_MemberInfo_(p0);
	}
	static java_lang_Class getClassById_I_Ljava_lang_Class_(int p0) {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							j_ProgramReflection._ensure__V();
							G = 1;
							continue;
						case 1:
							fA0 = java_lang_Class.forName_Ljava_lang_String__Ljava_lang_Class_((((j_ProgramReflection.__classNames as JA_L)).data[p0] as java_lang_String));
							G = 2;
							continue;
						case 2: return (fA0 as java_lang_Class);
						case 3:
							fA0 = J__exception__;
							lA1 = fA0;
							(lA1 as java_lang_ClassNotFoundException).printStackTrace__V();
							return null;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_ClassNotFoundException)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	j_ProgramReflection([int CLASS_ID = 736]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_jtransc_JTranscCoreReflection extends java_lang_Object  {

	 java_lang_jtransc_JTranscCoreReflection java_lang_jtransc_JTranscCoreReflection_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static java_lang_Class getClassById_I_Ljava_lang_Class_(int p0) {
		j_ProgramReflection._ensure__V();
		return java_lang_jtransc_JTranscCoreReflection.getClassByName_Ljava_lang_String__Ljava_lang_Class_(java_lang_jtransc_JTranscCoreReflection.getClassNameById_I_Ljava_lang_String_(p0));
	}
	static java_lang_String getClassNameById_I_Ljava_lang_String_(int p0) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (java_lang_jtransc_JTranscCoreReflection.checkClassId_I_Z(p0)) {
						G = 1;
						continue;
					}
					return null;
				case 1:
					j_ProgramReflection._ensure__V();
					return (((j_ProgramReflection.__classInfos as JA_L)).data[p0] as j_ClassInfo)._name;
				default:
					break;
			}
		}
		return null;
	}
	static bool checkClassId_I_Z(int p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					j_ProgramReflection._ensure__V();
					if (((p0 < 0))) {
						G = 1;
						continue;
					}
					if (((p0 >= (j_ProgramReflection.__classInfos as JA_0).length))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	static java_lang_Class getClassByName_Ljava_lang_String__Ljava_lang_Class_(java_lang_String p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							G = 1;
							continue;
						case 1:
							fA0 = java_lang_Class.forName_Ljava_lang_String__Ljava_lang_Class_(p0);
							G = 2;
							continue;
						case 2: return (fA0 as java_lang_Class);
						case 3:
							fA0 = J__exception__;
							return null;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_ClassNotFoundException)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	static int getClassId_Ljava_lang_Object__I(java_lang_Object p0) {
		return p0.__DART__CLASS_ID;
	}
	static bool isArray_Ljava_lang_Object__Z(java_lang_Object p0) {
		return p0 is JA_0;
	}
	static java_lang_String getArrayDescriptor_Ljava_lang_Object__Ljava_lang_String_(java_lang_Object p0) {
		return N.str((p0 as JA_0).desc);
	}
	static j_ClassInfo getClassInfoWithName_Ljava_lang_String__Lj_ClassInfo_(java_lang_String p0) {
		return j_ProgramReflection.getClassInfoWithName_Ljava_lang_String__Lj_ClassInfo_(p0);
	}
	static int getModifiersWithId_I_I(int p0) {
		j_ProgramReflection._ensure__V();
		return (((j_ProgramReflection.__classInfos as JA_L)).data[p0] as j_ClassInfo)._modifiers;
	}
	static JA_L getDeclaredFields_Ljava_lang_Class___Ljava_lang_reflect_Field_(java_lang_Class p0) {
		int G = 0;
		java_lang_Object lA1 = null;
		int fI0 = 0;
		java_lang_Object lA3 = null;
		int fI1 = 0;
		int lI2 = 0;
		int lI4 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = j_ProgramReflection.getFields_I__Lj_MemberInfo_(java_lang_jtransc_JTranscCoreReflection.getClassId_Ljava_lang_Class__I(p0));
					if (((lA1 == null))) {
						G = 1;
						continue;
					}
					fI0 = (lA1 as JA_0).length;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2:
					lI2 = fI0;
					lA3 = new JA_L(lI2, "[Ljava.lang.reflect.Field;");
					lI4 = 0;
					G = 3;
					continue;
				case 3:
					if (((lI4 >= lI2))) {
						G = 4;
						continue;
					}
					fA0 = lA3;
					fI1 = lI4;
					tA0 = (new java_lang_reflect_Field());
					fA2 = tA0;
					(tA0 as java_lang_reflect_Field).java_lang_reflect_Field_init__Ljava_lang_Class_Lj_MemberInfo__V(p0, (((lA1 as JA_L)).data[lI4] as j_MemberInfo));
					(fA0 as JA_L).data[fI1] = fA2;
					lI4 = (N.I(lI4 + 1));
					G = 3;
					continue;
				case 4:
					return (lA3 as JA_L);
				default:
					break;
			}
		}
		return null;
	}
	static int getClassId_Ljava_lang_Class__I(java_lang_Class p0) {
		return p0._id;
	}
	static JA_L getDeclaredConstructors_Ljava_lang_Class___Ljava_lang_reflect_Constructor_(java_lang_Class p0) {
		int G = 0;
		java_lang_Object lA1 = null;
		int fI0 = 0;
		java_lang_Object lA3 = null;
		int fI1 = 0;
		int lI2 = 0;
		int lI4 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = j_ProgramReflection.getConstructors_I__Lj_MemberInfo_(java_lang_jtransc_JTranscCoreReflection.getClassId_Ljava_lang_Class__I(p0));
					if (((lA1 == null))) {
						G = 1;
						continue;
					}
					fI0 = (lA1 as JA_0).length;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2:
					lI2 = fI0;
					lA3 = new JA_L(lI2, "[Ljava.lang.reflect.Constructor;");
					lI4 = 0;
					G = 3;
					continue;
				case 3:
					if (((lI4 >= lI2))) {
						G = 4;
						continue;
					}
					fA0 = lA3;
					fI1 = lI4;
					tA0 = (new java_lang_reflect_Constructor());
					fA2 = tA0;
					(tA0 as java_lang_reflect_Constructor).java_lang_reflect_Constructor_init__Ljava_lang_Class_Lj_MemberInfo__V(p0, (((lA1 as JA_L)).data[lI4] as j_MemberInfo));
					(fA0 as JA_L).data[fI1] = fA2;
					lI4 = (N.I(lI4 + 1));
					G = 3;
					continue;
				case 4:
					return (lA3 as JA_L);
				default:
					break;
			}
		}
		return null;
	}
	static bool hasClassWithName_Ljava_lang_String__Z(java_lang_String p0) {
		return j_ProgramReflection.hasClassWithName_Ljava_lang_String__Z(p0);
	}
	java_lang_jtransc_JTranscCoreReflection([int CLASS_ID = 735]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_Objects extends java_lang_Object  {

	 java_util_Objects java_util_Objects_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static bool equals_Ljava_lang_Object_Ljava_lang_Object__Z(java_lang_Object p0, java_lang_Object p1) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					if (((p1 != null))) {
						G = 2;
						continue;
					}
					fI0 = 1;
					G = 3;
					continue;
				case 2:
					fI0 = 0;
					G = 3;
					continue;
				case 1:
					fI0 = (N.z2i(p0.equals_Ljava_lang_Object__Z(p1)));
					G = 3;
					continue;
				case 3: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	static int hashCode_Ljava_lang_Object__I(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					fI0 = 0;
					G = 2;
					continue;
				case 1:
					fI0 = p0.hashCode__I();
					G = 2;
					continue;
				case 2: return fI0;
				default:
					break;
			}
		}
		return 0;
	}
	static java_lang_Object requireNonNull_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_NullPointerException());
					fA0 = tA0;
					(tA0 as java_lang_NullPointerException).java_lang_NullPointerException_init___V();
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					return p0;
				default:
					break;
			}
		}
		return null;
	}
	java_util_Objects([int CLASS_ID = 734]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_ClassCastException extends java_lang_RuntimeException  {

	 java_lang_ClassCastException java_lang_ClassCastException_init___V() {
		this.java_lang_RuntimeException_init___V();
		return this;
		return this;
	}
	java_lang_ClassCastException([int CLASS_ID = 733]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_SystemInt extends java_lang_Object  {

	static int ___lastId = 0;
	 java_lang_SystemInt java_lang_SystemInt_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void java_lang_SystemInt_clinit___V() {
		java_lang_SystemInt.___lastId = 1;
		return;
	}
	static int identityHashCode_Ljava_lang_Object__I(java_lang_Object p0) {
		int G = 0;
		int fI1 = 0;
		int tI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 == null))) {
						G = 1;
						continue;
					}
					if (((p0.___id != 0))) {
						G = 2;
						continue;
					}
					tI0 = java_lang_SystemInt.___lastId;
					fI1 = tI0;
					java_lang_SystemInt.___lastId = (N.I(tI0 + 1));
					p0.___id = fI1;
					G = 2;
					continue;
				case 2:
					return p0.___id;
				case 1:
					return 0;
				default:
					break;
			}
		}
		return 0;
	}
	java_lang_SystemInt([int CLASS_ID = 732]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_SystemInt.___lastId = 0;
		java_lang_SystemInt.java_lang_SystemInt_clinit___V();
	}
}
abstract class java_lang_Number extends java_lang_Object  {

	 java_lang_Number java_lang_Number_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	 double doubleValue__D() {
		throw new Exception("Missing body java.lang.Number.doubleValue\u0028\u0029D");
	}
	 Int64 longValue__J() {
		throw new Exception("Missing body java.lang.Number.longValue\u0028\u0029J");
	}
	 int shortValue__S() {
		return N.i2s(this.intValue__I());
	}
	 int intValue__I() {
		throw new Exception("Missing body java.lang.Number.intValue\u0028\u0029I");
	}
	 int byteValue__B() {
		return N.i2b(this.intValue__I());
	}
	 double floatValue__F() {
		throw new Exception("Missing body java.lang.Number.floatValue\u0028\u0029F");
	}
	java_lang_Number([int CLASS_ID = 673]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_Byte extends java_lang_Number implements java_lang_Comparable {

	static JA_L _cache = null;
	int _value = 0;
	static java_lang_Class _TYPE = null;
	static java_lang_Byte valueOf_B_Ljava_lang_Byte_(int p0) {
		int G = 0;
		int fI1 = 0;
		int lI1 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = (N.I((p0) + 128));
					if (((((java_lang_Byte._cache as JA_L)).data[lI1] != null))) {
						G = 1;
						continue;
					}
					fA0 = java_lang_Byte._cache;
					fI1 = lI1;
					tA0 = (new java_lang_Byte());
					fA2 = tA0;
					(tA0 as java_lang_Byte).java_lang_Byte_init__B_V(p0);
					(fA0 as JA_L).data[fI1] = fA2;
					G = 1;
					continue;
				case 1:
					return (((java_lang_Byte._cache as JA_L)).data[lI1] as java_lang_Byte);
				default:
					break;
			}
		}
		return null;
	}
	 java_lang_Byte java_lang_Byte_init__B_V(int p0) {
		this.java_lang_Number_init___V();
		this._value = p0;
		return this;
		return this;
	}
	static void java_lang_Byte_clinit___V() {
		java_lang_Byte._TYPE = java_lang_Class.getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap.STRINGLIT_742);
		java_lang_Byte._cache = new JA_L(256, "[Ljava.lang.Byte;");
		return;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		return java_lang_Integer.toString_I_Ljava_lang_String_((this._value));
	}
	 double doubleValue__D() {
		return N.i2d(this._value);
	}
	 int hashCode__I() {
		return java_lang_Byte.hashCode_B_I(this._value);
	}
	static int hashCode_B_I(int p0) {
		return (p0);
	}
	 Int64 longValue__J() {
		return N.i2j(this._value);
	}
	 int intValue__I() {
		return (this._value);
	}
	 int shortValue__S() {
		return N.i2s(this._value);
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(((p0) is java_lang_Byte))) {
						G = 1;
						continue;
					}
					if (((this._value != ((p0) as java_lang_Byte).byteValue__B()))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 int byteValue__B() {
		return this._value;
	}
	 double floatValue__F() {
		return N.i2f(this._value);
	}
	java_lang_Byte([int CLASS_ID = 731]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_Byte._cache = null;
		java_lang_Byte._TYPE = null;
		java_lang_Byte.java_lang_Byte_clinit___V();
	}
}
class java_lang_Boolean extends java_lang_Object implements java_io_Serializable, java_lang_Comparable {

	static java_lang_Class _TYPE = null;
	static java_lang_Boolean _TRUE = null;
	static java_lang_Boolean _FALSE = null;
	bool _value = false;
	static void java_lang_Boolean_clinit___V() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		tA0 = (new java_lang_Boolean());
		fA0 = tA0;
		(tA0 as java_lang_Boolean).java_lang_Boolean_init__Z_V(true);
		java_lang_Boolean._TRUE = (fA0 as java_lang_Boolean);
		tA1 = (new java_lang_Boolean());
		fA0 = tA1;
		(tA1 as java_lang_Boolean).java_lang_Boolean_init__Z_V(false);
		java_lang_Boolean._FALSE = (fA0 as java_lang_Boolean);
		java_lang_Boolean._TYPE = java_lang_Class.getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap.STRINGLIT_743);
		return;
	}
	 java_lang_Boolean java_lang_Boolean_init__Z_V(bool p0) {
		this.java_lang_Object_init___V();
		this._value = p0;
		return this;
		return this;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		return java_lang_Boolean.toString_Z_Ljava_lang_String_(this._value);
	}
	static java_lang_String toString_Z_Ljava_lang_String_(bool p0) {
		int G = 0;
		java_lang_String fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(p0)) {
						G = 1;
						continue;
					}
					fA0 = Bootstrap.STRINGLIT_744;
					G = 2;
					continue;
				case 1:
					fA0 = Bootstrap.STRINGLIT_745;
					G = 2;
					continue;
				case 2: return fA0;
				default:
					break;
			}
		}
		return null;
	}
	 int hashCode__I() {
		return java_lang_Boolean.hashCode_Z_I(this._value);
	}
	static int hashCode_Z_I(bool p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(p0)) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return fI0;
				default:
					break;
			}
		}
		return 0;
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this != p0))) {
						G = 1;
						continue;
					}
					return true;
				case 1:
					if (((p0 != null))) {
						G = 2;
						continue;
					}
					return false;
				case 2:
					if (((this.getClass__Ljava_lang_Class_() == p0.getClass__Ljava_lang_Class_()))) {
						G = 3;
						continue;
					}
					return false;
				case 3:
					if (((this._value != ((p0) as java_lang_Boolean)._value))) {
						G = 4;
						continue;
					}
					fI0 = 1;
					G = 5;
					continue;
				case 4:
					fI0 = 0;
					G = 5;
					continue;
				case 5: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 bool booleanValue__Z() {
		return this._value;
	}
	static java_lang_Boolean valueOf_Z_Ljava_lang_Boolean_(bool p0) {
		int G = 0;
		java_lang_Boolean fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(p0)) {
						G = 1;
						continue;
					}
					fA0 = java_lang_Boolean._TRUE;
					G = 2;
					continue;
				case 1:
					fA0 = java_lang_Boolean._FALSE;
					G = 2;
					continue;
				case 2: return fA0;
				default:
					break;
			}
		}
		return null;
	}
	java_lang_Boolean([int CLASS_ID = 730]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_Boolean._TYPE = null;
		java_lang_Boolean._TRUE = null;
		java_lang_Boolean._FALSE = null;
		java_lang_Boolean.java_lang_Boolean_clinit___V();
	}
}
class com_jtransc_io_JTranscConsole extends java_lang_Object  {

	 com_jtransc_io_JTranscConsole com_jtransc_io_JTranscConsole_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void logString_Ljava_lang_String__V(java_lang_String p0) {
		com_jtransc_io_JTranscConsole.log_Ljava_lang_Object__V(p0);
		return;
	}
	static void log_Ljava_lang_Object__V(java_lang_Object p0) {
		print(p0);
	}
	static void error_Ljava_lang_Object__V(java_lang_Object p0) {
		print(p0);
	}
	static void logOrError_Ljava_lang_Object_Z_V(java_lang_Object p0, bool p1) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(p1)) {
						G = 1;
						continue;
					}
					com_jtransc_io_JTranscConsole.error_Ljava_lang_Object__V(p0);
					G = 2;
					continue;
				case 1:
					com_jtransc_io_JTranscConsole.log_Ljava_lang_Object__V(p0);
					G = 2;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	com_jtransc_io_JTranscConsole([int CLASS_ID = 729]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_Short extends java_lang_Number implements java_lang_Comparable {

	int _value = 0;
	static java_lang_Class _TYPE = null;
	static java_lang_Short valueOf_S_Ljava_lang_Short_(int p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_Short());
		fA0 = tA0;
		(tA0 as java_lang_Short).java_lang_Short_init__S_V(p0);
		return (fA0 as java_lang_Short);
	}
	 java_lang_Short java_lang_Short_init__S_V(int p0) {
		this.java_lang_Number_init___V();
		this._value = p0;
		return this;
		return this;
	}
	static void java_lang_Short_clinit___V() {
		java_lang_Short._TYPE = java_lang_Class.getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap.STRINGLIT_746);
		return;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		return java_lang_Integer.toString_I_Ljava_lang_String_((this._value));
	}
	 double doubleValue__D() {
		return N.i2d(this._value);
	}
	 int hashCode__I() {
		return java_lang_Short.hashCode_S_I(this._value);
	}
	static int hashCode_S_I(int p0) {
		return (p0);
	}
	 Int64 longValue__J() {
		return N.i2j(this._value);
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(((p0) is java_lang_Short))) {
						G = 1;
						continue;
					}
					if (((this._value != ((p0) as java_lang_Short).shortValue__S()))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 int shortValue__S() {
		return this._value;
	}
	 int intValue__I() {
		return (this._value);
	}
	 int byteValue__B() {
		return N.i2b(this._value);
	}
	 double floatValue__F() {
		return N.i2f(this._value);
	}
	static int reverseBytes_S_S(int p0) {
		return N.i2s((N.I((N.I((N.I((p0) & 65280)) >> 8)) | (N.I((N.I((p0) & 255)) << 8)))));
	}
	java_lang_Short([int CLASS_ID = 728]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_Short._TYPE = null;
		java_lang_Short.java_lang_Short_clinit___V();
	}
}
class com_jtransc_internal_JTranscCType extends java_lang_Object  {

	 com_jtransc_internal_JTranscCType com_jtransc_internal_JTranscCType_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static int encodeDigit_I_C(int p0) {
		return java_lang_Character.forDigit_II_C(p0, 36);
	}
	static bool isDigit_C_Z(int p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if ((((p0) < 48))) {
						G = 1;
						continue;
					}
					if ((((p0) <= 57))) {
						G = 2;
						continue;
					}
					G = 1;
					continue;
				case 1:
					if ((((p0) < 97))) {
						G = 3;
						continue;
					}
					if ((((p0) <= 122))) {
						G = 2;
						continue;
					}
					G = 3;
					continue;
				case 3:
					if ((((p0) < 65))) {
						G = 4;
						continue;
					}
					if ((((p0) > 90))) {
						G = 4;
						continue;
					}
					G = 2;
					continue;
				case 2:
					fI0 = 1;
					G = 5;
					continue;
				case 4:
					fI0 = 0;
					G = 5;
					continue;
				case 5: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	com_jtransc_internal_JTranscCType([int CLASS_ID = 727]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_Long extends java_lang_Number implements java_lang_Comparable {

	Int64 _value = N.lnew(0);
	static java_lang_Class _TYPE = null;
	static java_lang_Long valueOf_J_Ljava_lang_Long_(Int64 p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_Long());
		fA0 = tA0;
		(tA0 as java_lang_Long).java_lang_Long_init__J_V(p0);
		return (fA0 as java_lang_Long);
	}
	 java_lang_Long java_lang_Long_init__J_V(Int64 p0) {
		this.java_lang_Number_init___V();
		this._value = p0;
		return this;
		return this;
	}
	static void java_lang_Long_clinit___V() {
		java_lang_Long._TYPE = java_lang_Class.getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap.STRINGLIT_747);
		return;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		return java_lang_Long.toString_J_Ljava_lang_String_(this._value);
	}
	static java_lang_String toString_J_Ljava_lang_String_(Int64 p0) {
		return java_lang_Long.toString_JI_Ljava_lang_String_(p0, 10);
	}
	static java_lang_String toString_JI_Ljava_lang_String_(Int64 p0, int p1) {
		int G = 0;
		java_lang_Object lA3 = null;
		int fI0 = 0;
		int lI4 = 0;
		java_lang_Object fA0 = null;
		Int64 lJ0 = N.lnew(0);
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lJ0 = p0;
					if ((((N.lcmp(lJ0, N.lnew(0))) != 0))) {
						G = 1;
						continue;
					}
					return Bootstrap.STRINGLIT_748;
				case 1:
					if ((((N.lcmp(lJ0, N.lnew(-9223372036854775808))) != 0))) {
						G = 2;
						continue;
					}
					return Bootstrap.STRINGLIT_749;
				case 2:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA3 = fA0;
					if ((((N.lcmp(lJ0, N.lnew(0))) >= 0))) {
						G = 3;
						continue;
					}
					fI0 = 1;
					G = 4;
					continue;
				case 3:
					fI0 = 0;
					G = 4;
					continue;
				case 4:
					lI4 = fI0;
					if (((lI4 == 0))) {
						G = 5;
						continue;
					}
					lJ0 = (-(lJ0));
					G = 5;
					continue;
				case 5:
					if ((((N.lcmp(lJ0, N.lnew(0))) == 0))) {
						G = 6;
						continue;
					}
					(lA3 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(com_jtransc_internal_JTranscCType.encodeDigit_I_C(N.j2i((N.lrem((((N.lrem(lJ0, N.i2j(p1)))+N.i2j(p1))), N.i2j(p1))))));
					lJ0 = ((lJ0~/N.i2j(p1)));
					G = 5;
					continue;
				case 6:
					if (((lI4 == 0))) {
						G = 7;
						continue;
					}
					(lA3 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_750);
					G = 7;
					continue;
				case 7:
					(lA3 as java_lang_StringBuilder).reverse__Ljava_lang_StringBuilder_();
					return (lA3 as java_lang_StringBuilder).toString__Ljava_lang_String_();
				default:
					break;
			}
		}
		return null;
	}
	 double doubleValue__D() {
		return N.j2d(this._value);
	}
	 int hashCode__I() {
		return java_lang_Long.hashCode_J_I(this._value);
	}
	static int hashCode_J_I(Int64 p0) {
		return N.j2i(((p0^(N.lushr_opt(p0, 32)))));
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_lang_Long)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					if ((((N.lcmp(this._value, ((p0) as java_lang_Long).longValue__J())) != 0))) {
						G = 2;
						continue;
					}
					fI0 = 1;
					G = 3;
					continue;
				case 2:
					fI0 = 0;
					G = 3;
					continue;
				case 3: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 Int64 longValue__J() {
		return this._value;
	}
	 int intValue__I() {
		return N.j2i(this._value);
	}
	 int shortValue__S() {
		return N.i2s(N.j2i(this._value));
	}
	 int byteValue__B() {
		return N.i2b(N.j2i(this._value));
	}
	 double floatValue__F() {
		return N.j2f(this._value);
	}
	static Int64 reverseBytes_J_J(Int64 p0) {
		Int64 lJ0 = N.lnew(0);
		lJ0 = p0;
		lJ0 = (((((N.lushr_opt(lJ0, 8))&N.lnew(71777214294589695)))|((((lJ0&N.lnew(71777214294589695))) << 8))));
		lJ0 = (((((N.lushr_opt(lJ0, 16))&N.lnew(281470681808895)))|((((lJ0&N.lnew(281470681808895))) << 16))));
		return (((N.lushr_opt(lJ0, 32))|((lJ0 << 32))));
	}
	java_lang_Long([int CLASS_ID = 726]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_Long._TYPE = null;
		java_lang_Long.java_lang_Long_clinit___V();
	}
}
class java_util_ConcurrentModificationException extends java_lang_RuntimeException  {

	 java_util_ConcurrentModificationException java_util_ConcurrentModificationException_init___V() {
		this.java_lang_RuntimeException_init___V();
		return this;
		return this;
	}
	java_util_ConcurrentModificationException([int CLASS_ID = 725]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_NoSuchElementException extends java_lang_RuntimeException  {

	 java_util_NoSuchElementException java_util_NoSuchElementException_init___V() {
		this.java_lang_RuntimeException_init___V();
		return this;
		return this;
	}
	java_util_NoSuchElementException([int CLASS_ID = 724]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_HashMap_HashIterator extends java_lang_Object  {

	java_util_HashMap_HashMapEntry _nextEntry = null;
	int _nextIndex = 0;
	java_util_HashMap _this_0 = null;
	int _expectedModCount = 0;
	java_util_HashMap_HashMapEntry _lastEntryReturned = null;
	 java_util_HashMap_HashIterator java_util_HashMap_HashIterator_init__Ljava_util_HashMap__V(java_util_HashMap p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		java_lang_Object lA3 = null;
		int fI1 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		int tI1 = 0;
		java_lang_Object tA2 = null;
		while (true) {
			switch (G) {
				case 0:
					this._this_0 = p0;
					this.java_lang_Object_init___V();
					this._nextEntry = this._this_0._entryForNullKey;
					this._expectedModCount = this._this_0._modCount;
					if (((this._nextEntry != null))) {
						G = 1;
						continue;
					}
					lA2 = p0._table;
					lA3 = null;
					G = 2;
					continue;
				case 2:
					if (((lA3 != null))) {
						G = 3;
						continue;
					}
					if (((this._nextIndex >= (lA2 as JA_0).length))) {
						G = 3;
						continue;
					}
					fA0 = lA2;
					fA1 = this;
					tA2 = fA1;
					tI1 = this._nextIndex;
					fI1 = tI1;
					(tA2 as java_util_HashMap_HashIterator)._nextIndex = (N.I(tI1 + 1));
					lA3 = ((fA0 as JA_L)).data[fI1];
					G = 2;
					continue;
				case 3:
					this._nextEntry = (lA3 as java_util_HashMap_HashMapEntry);
					G = 1;
					continue;
				case 1:
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 java_util_HashMap_HashMapEntry nextEntry__Ljava_util_HashMap_HashMapEntry_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA2 = null;
		java_lang_Object lA3 = null;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA4 = null;
		java_lang_Object tA6 = null;
		java_lang_Object tA5 = null;
		int fI1 = 0;
		int tI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._this_0._modCount == this._expectedModCount))) {
						G = 1;
						continue;
					}
					tA0 = (new java_util_ConcurrentModificationException());
					fA0 = tA0;
					(tA0 as java_util_ConcurrentModificationException).java_util_ConcurrentModificationException_init___V();
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					if (((this._nextEntry != null))) {
						G = 2;
						continue;
					}
					tA1 = (new java_util_NoSuchElementException());
					fA0 = tA1;
					(tA1 as java_util_NoSuchElementException).java_util_NoSuchElementException_init___V();
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 2;
					continue;
				case 2:
					lA1 = this._nextEntry;
					lA2 = this._this_0._table;
					lA3 = (lA1 as java_util_HashMap_HashMapEntry)._next;
					G = 3;
					continue;
				case 3:
					if (((lA3 != null))) {
						G = 4;
						continue;
					}
					if (((this._nextIndex >= (lA2 as JA_0).length))) {
						G = 4;
						continue;
					}
					fA0 = lA2;
					fA1 = this;
					tA4 = fA1;
					tI3 = this._nextIndex;
					fI1 = tI3;
					(tA4 as java_util_HashMap_HashIterator)._nextIndex = (N.I(tI3 + 1));
					lA3 = ((fA0 as JA_L)).data[fI1];
					G = 3;
					continue;
				case 4:
					this._nextEntry = (lA3 as java_util_HashMap_HashMapEntry);
					fA0 = this;
					tA6 = fA0;
					tA5 = lA1;
					fA0 = tA5;
					(tA6 as java_util_HashMap_HashIterator)._lastEntryReturned = (tA5 as java_util_HashMap_HashMapEntry);
					return (fA0 as java_util_HashMap_HashMapEntry);
				default:
					break;
			}
		}
		return null;
	}
	 bool hasNext__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._nextEntry == null))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	java_util_HashMap_HashIterator([int CLASS_ID = 723]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_HashMap_EntryIterator extends java_util_HashMap_HashIterator implements java_util_Iterator {

	java_util_HashMap _this_0_ = null;
	 java_util_HashMap_EntryIterator java_util_HashMap_EntryIterator_init__Ljava_util_HashMap__V(java_util_HashMap p0) {
		this._this_0_ = p0;
		this.java_util_HashMap_HashIterator_init__Ljava_util_HashMap__V(p0);
		return this;
		return this;
	}
	 java_util_HashMap_EntryIterator java_util_HashMap_EntryIterator_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V(java_util_HashMap p0, java_util_HashMap_1 p1) {
		this.java_util_HashMap_EntryIterator_init__Ljava_util_HashMap__V(p0);
		return this;
		return this;
	}
	 java_lang_Object next__Ljava_lang_Object_() {
		return this.next__Ljava_util_Map_Entry_();
	}
	 java_util_Map_Entry next__Ljava_util_Map_Entry_() {
		return this.nextEntry__Ljava_util_HashMap_HashMapEntry_();
	}
	java_util_HashMap_EntryIterator([int CLASS_ID = 722]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_HashMap_EntrySet extends java_util_AbstractSet  {

	java_util_HashMap _this_0 = null;
	 int size__I() {
		return this._this_0._size;
	}
	 bool isEmpty__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._this_0._size != 0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 java_util_Iterator iterator__Ljava_util_Iterator_() {
		return this._this_0.newEntryIterator__Ljava_util_Iterator_();
	}
	 java_util_HashMap_EntrySet java_util_HashMap_EntrySet_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V(java_util_HashMap p0, java_util_HashMap_1 p1) {
		this.java_util_HashMap_EntrySet_init__Ljava_util_HashMap__V(p0);
		return this;
		return this;
	}
	 java_util_HashMap_EntrySet java_util_HashMap_EntrySet_init__Ljava_util_HashMap__V(java_util_HashMap p0) {
		this._this_0 = p0;
		this.java_util_AbstractSet_init___V();
		return this;
		return this;
	}
	 bool contains_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		java_util_Map_Entry lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_util_Map_Entry)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					lA2 = ((p0) as java_util_Map_Entry);
					return java_util_HashMap.access_600_Ljava_util_HashMap_Ljava_lang_Object_Ljava_lang_Object__Z(this._this_0, lA2.getKey__Ljava_lang_Object_(), lA2.getValue__Ljava_lang_Object_());
				default:
					break;
			}
		}
		return false;
	}
	java_util_HashMap_EntrySet([int CLASS_ID = 719]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_HashMap_1 extends java_lang_Object  {

	java_util_HashMap_1([int CLASS_ID = 718]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_HashMap_HashMapEntry extends java_lang_Object implements java_util_Map_Entry {

	java_lang_Object _key = null;
	java_util_HashMap_HashMapEntry _next = null;
	java_lang_Object _value = null;
	int _hash = 0;
	 java_util_HashMap_HashMapEntry java_util_HashMap_HashMapEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__V(java_lang_Object p0, java_lang_Object p1, int p2, java_util_HashMap_HashMapEntry p3) {
		this.java_lang_Object_init___V();
		this._key = p0;
		this._value = p1;
		this._hash = p2;
		this._next = p3;
		return this;
		return this;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		return (fA0 as java_lang_StringBuilder).append_Ljava_lang_Object__Ljava_lang_StringBuilder_(this._key).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_7).append_Ljava_lang_Object__Ljava_lang_StringBuilder_(this._value).toString__Ljava_lang_String_();
	}
	 java_lang_Object getKey__Ljava_lang_Object_() {
		return this._key;
	}
	 java_lang_Object getValue__Ljava_lang_Object_() {
		return this._value;
	}
	 int hashCode__I() {
		int G = 0;
		int fI0 = 0;
		int fI1 = 0;
		java_lang_Object fA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((this._key != null))) {
						G = 1;
						continue;
					}
					fI0 = 0;
					G = 2;
					continue;
				case 1:
					fI0 = this._key.hashCode__I();
					G = 2;
					continue;
				case 2:
					if (((this._value != null))) {
						G = 3;
						continue;
					}
					fI1 = 0;
					G = 4;
					continue;
				case 3:
					fA1 = this._value;
					fI1 = fA1.hashCode__I();
					G = 4;
					continue;
				case 4:
					fI0 = (N.I(fI0 ^ fI1));
					return fI0;
				default:
					break;
			}
		}
		return 0;
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		java_util_Map_Entry lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0) is java_util_Map_Entry)) {
						G = 1;
						continue;
					}
					return false;
				case 1:
					lA2 = ((p0) as java_util_Map_Entry);
					if (!(java_util_Objects.equals_Ljava_lang_Object_Ljava_lang_Object__Z(lA2.getKey__Ljava_lang_Object_(), this._key))) {
						G = 2;
						continue;
					}
					if (!(java_util_Objects.equals_Ljava_lang_Object_Ljava_lang_Object__Z(lA2.getValue__Ljava_lang_Object_(), this._value))) {
						G = 2;
						continue;
					}
					fI0 = 1;
					G = 3;
					continue;
				case 2:
					fI0 = 0;
					G = 3;
					continue;
				case 3:
					return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	java_util_HashMap_HashMapEntry([int CLASS_ID = 713]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_util_HashMap extends java_util_AbstractMap implements java_lang_Cloneable, java_io_Serializable {

	JA_L _table = null;
	int _threshold = 0;
	static JA_L _EMPTY_TABLE = null;
	int _size = 0;
	java_util_HashMap_HashMapEntry _entryForNullKey = null;
	int _modCount = 0;
	java_util_Set _entrySet = null;
	java_util_Collection _values = null;
	java_util_Set _keySet_ = null;
	 java_util_HashMap java_util_HashMap_init___V() {
		this.java_util_AbstractMap_init___V();
		this._table = ((java_util_HashMap._EMPTY_TABLE) as JA_L);
		this._threshold = -1;
		return this;
		return this;
	}
	 java_util_Set entrySet__Ljava_util_Set_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA2 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = this._entrySet;
					if (((lA1 == null))) {
						G = 1;
						continue;
					}
					fA0 = lA1;
					G = 2;
					continue;
				case 1:
					fA0 = this;
					tA0 = (new java_util_HashMap_EntrySet());
					fA1 = tA0;
					(tA0 as java_util_HashMap_EntrySet).java_util_HashMap_EntrySet_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V(this, null);
					tA2 = fA0;
					tA1 = fA1;
					fA0 = tA1;
					(tA2 as java_util_HashMap)._entrySet = (tA1 as java_util_Set);
					G = 2;
					continue;
				case 2: return (fA0 as java_util_Set);
				default:
					break;
			}
		}
		return null;
	}
	 java_util_Iterator newEntryIterator__Ljava_util_Iterator_() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_util_HashMap_EntryIterator());
		fA0 = tA0;
		(tA0 as java_util_HashMap_EntryIterator).java_util_HashMap_EntryIterator_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V(this, null);
		return (fA0 as java_util_Iterator);
	}
	 int size__I() {
		return this._size;
	}
	 bool isEmpty__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this._size != 0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	static void java_util_HashMap_clinit___V() {
		java_util_HashMap._EMPTY_TABLE = (new JA_L(2, "[Ljava.util.HashMap\$HashMapEntry;") as JA_L);
		return;
	}
	 bool containsKey_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		java_lang_Object lA3 = null;
		java_lang_Object lA4 = null;
		java_lang_Object lA5 = null;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					if (((this._entryForNullKey == null))) {
						G = 2;
						continue;
					}
					fI0 = 1;
					G = 3;
					continue;
				case 2:
					fI0 = 0;
					G = 3;
					continue;
				case 3: return ((fI0)!=0);
				case 1:
					lI2 = p0.hashCode__I();
					lI2 = (N.I(lI2 ^ (N.I((N.iushr_opt(lI2, 20)) ^ (N.iushr_opt(lI2, 12))))));
					lI2 = (N.I(lI2 ^ (N.I((N.iushr_opt(lI2, 7)) ^ (N.iushr_opt(lI2, 4))))));
					lA3 = this._table;
					lA4 = ((lA3 as JA_L)).data[(N.I(lI2 & (N.I((lA3 as JA_0).length - 1))))];
					G = 4;
					continue;
				case 4:
					if (((lA4 == null))) {
						G = 5;
						continue;
					}
					lA5 = (lA4 as java_util_HashMap_HashMapEntry)._key;
					if (((lA5 == p0))) {
						G = 6;
						continue;
					}
					if ((((lA4 as java_util_HashMap_HashMapEntry)._hash != lI2))) {
						G = 7;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z(lA5))) {
						G = 7;
						continue;
					}
					G = 6;
					continue;
				case 6:
					return true;
				case 7:
					lA4 = (lA4 as java_util_HashMap_HashMapEntry)._next;
					G = 4;
					continue;
				case 5:
					return false;
				default:
					break;
			}
		}
		return false;
	}
	 java_lang_Object get_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		java_lang_Object lA3 = null;
		java_lang_Object lA4 = null;
		java_lang_Object lA5 = null;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					lA2 = this._entryForNullKey;
					if (((lA2 != null))) {
						G = 2;
						continue;
					}
					fA0 = null;
					G = 3;
					continue;
				case 2:
					fA0 = (lA2 as java_util_HashMap_HashMapEntry)._value;
					G = 3;
					continue;
				case 3: return fA0;
				case 1:
					lI2 = p0.hashCode__I();
					lI2 = (N.I(lI2 ^ (N.I((N.iushr_opt(lI2, 20)) ^ (N.iushr_opt(lI2, 12))))));
					lI2 = (N.I(lI2 ^ (N.I((N.iushr_opt(lI2, 7)) ^ (N.iushr_opt(lI2, 4))))));
					lA3 = this._table;
					lA4 = ((lA3 as JA_L)).data[(N.I(lI2 & (N.I((lA3 as JA_0).length - 1))))];
					G = 4;
					continue;
				case 4:
					if (((lA4 == null))) {
						G = 5;
						continue;
					}
					lA5 = (lA4 as java_util_HashMap_HashMapEntry)._key;
					if (((lA5 == p0))) {
						G = 6;
						continue;
					}
					if ((((lA4 as java_util_HashMap_HashMapEntry)._hash != lI2))) {
						G = 7;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z(lA5))) {
						G = 7;
						continue;
					}
					G = 6;
					continue;
				case 6:
					return (lA4 as java_util_HashMap_HashMapEntry)._value;
				case 7:
					lA4 = (lA4 as java_util_HashMap_HashMapEntry)._next;
					G = 4;
					continue;
				case 5:
					return null;
				default:
					break;
			}
		}
		return null;
	}
	static bool access_600_Ljava_util_HashMap_Ljava_lang_Object_Ljava_lang_Object__Z(java_util_HashMap p0, java_lang_Object p1, java_lang_Object p2) {
		return p0.containsMapping_Ljava_lang_Object_Ljava_lang_Object__Z(p1, p2);
	}
	 bool containsMapping_Ljava_lang_Object_Ljava_lang_Object__Z(java_lang_Object p0, java_lang_Object p1) {
		int G = 0;
		java_lang_Object lA3 = null;
		int fI0 = 0;
		java_lang_Object lA4 = null;
		java_lang_Object lA6 = null;
		int lI3 = 0;
		int lI5 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					lA3 = this._entryForNullKey;
					if (((lA3 == null))) {
						G = 2;
						continue;
					}
					if (!(java_util_Objects.equals_Ljava_lang_Object_Ljava_lang_Object__Z(p1, (lA3 as java_util_HashMap_HashMapEntry)._value))) {
						G = 2;
						continue;
					}
					fI0 = 1;
					G = 3;
					continue;
				case 2:
					fI0 = 0;
					G = 3;
					continue;
				case 3: return ((fI0)!=0);
				case 1:
					lI3 = java_util_HashMap.secondaryHash_Ljava_lang_Object__I(p0);
					lA4 = this._table;
					lI5 = (N.I(lI3 & (N.I((lA4 as JA_0).length - 1))));
					lA6 = ((lA4 as JA_L)).data[lI5];
					G = 4;
					continue;
				case 4:
					if (((lA6 == null))) {
						G = 5;
						continue;
					}
					if ((((lA6 as java_util_HashMap_HashMapEntry)._hash != lI3))) {
						G = 6;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z((lA6 as java_util_HashMap_HashMapEntry)._key))) {
						G = 6;
						continue;
					}
					return java_util_Objects.equals_Ljava_lang_Object_Ljava_lang_Object__Z(p1, (lA6 as java_util_HashMap_HashMapEntry)._value);
				case 6:
					lA6 = (lA6 as java_util_HashMap_HashMapEntry)._next;
					G = 4;
					continue;
				case 5:
					return false;
				default:
					break;
			}
		}
		return false;
	}
	static int secondaryHash_Ljava_lang_Object__I(java_lang_Object p0) {
		int lI1 = 0;
		lI1 = p0.hashCode__I();
		lI1 = (N.I(lI1 ^ (N.I((N.iushr_opt(lI1, 20)) ^ (N.iushr_opt(lI1, 12))))));
		lI1 = (N.I(lI1 ^ (N.I((N.iushr_opt(lI1, 7)) ^ (N.iushr_opt(lI1, 4))))));
		return lI1;
	}
	 java_lang_Object put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0, java_lang_Object p1) {
		int G = 0;
		java_lang_Object lA4 = null;
		java_lang_Object lA6 = null;
		java_lang_Object lA7 = null;
		int fI0 = 0;
		int lI3 = 0;
		int lI5 = 0;
		java_lang_Object fA0 = null;
		int tI2 = 0;
		java_lang_Object tA3 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					return this.putValueForNullKey_Ljava_lang_Object__Ljava_lang_Object_(p1);
				case 1:
					lI3 = java_util_HashMap.secondaryHash_Ljava_lang_Object__I(p0);
					lA4 = this._table;
					lI5 = (N.I(lI3 & (N.I((lA4 as JA_0).length - 1))));
					lA6 = ((lA4 as JA_L)).data[lI5];
					G = 2;
					continue;
				case 2:
					if (((lA6 == null))) {
						G = 3;
						continue;
					}
					if ((((lA6 as java_util_HashMap_HashMapEntry)._hash != lI3))) {
						G = 4;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z((lA6 as java_util_HashMap_HashMapEntry)._key))) {
						G = 4;
						continue;
					}
					this.preModify_Ljava_util_HashMap_HashMapEntry__V((lA6 as java_util_HashMap_HashMapEntry));
					lA7 = (lA6 as java_util_HashMap_HashMapEntry)._value;
					(lA6 as java_util_HashMap_HashMapEntry)._value = p1;
					return lA7;
				case 4:
					lA6 = (lA6 as java_util_HashMap_HashMapEntry)._next;
					G = 2;
					continue;
				case 3:
					this._modCount = (N.I(this._modCount + 1));
					fA0 = this;
					tA3 = fA0;
					tI2 = this._size;
					fI0 = tI2;
					(tA3 as java_util_HashMap)._size = (N.I(tI2 + 1));
					if (((fI0 <= this._threshold))) {
						G = 5;
						continue;
					}
					lA4 = this.doubleCapacity___Ljava_util_HashMap_HashMapEntry_();
					lI5 = (N.I(lI3 & (N.I((lA4 as JA_0).length - 1))));
					G = 5;
					continue;
				case 5:
					this.addNewEntry_Ljava_lang_Object_Ljava_lang_Object_II_V(p0, p1, lI3, lI5);
					return null;
				default:
					break;
			}
		}
		return null;
	}
	 void addNewEntry_Ljava_lang_Object_Ljava_lang_Object_II_V(java_lang_Object p0, java_lang_Object p1, int p2, int p3) {
		int fI1 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		fA0 = this._table;
		fI1 = p3;
		tA0 = (new java_util_HashMap_HashMapEntry());
		fA2 = tA0;
		(tA0 as java_util_HashMap_HashMapEntry).java_util_HashMap_HashMapEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__V(p0, p1, p2, (((this._table as JA_L)).data[p3] as java_util_HashMap_HashMapEntry));
		(fA0 as JA_L).data[fI1] = fA2;
		return;
	}
	 void preModify_Ljava_util_HashMap_HashMapEntry__V(java_util_HashMap_HashMapEntry p0) {
		return;
	}
	 JA_L doubleCapacity___Ljava_util_HashMap_HashMapEntry_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA4 = null;
		java_lang_Object lA6 = null;
		java_lang_Object lA8 = null;
		java_lang_Object lA9 = null;
		int lI2 = 0;
		int lI3 = 0;
		int lI5 = 0;
		int lI7 = 0;
		int lI10 = 0;
		while (true) {
			switch (G) {
				case 0:
					lA1 = this._table;
					lI2 = (lA1 as JA_0).length;
					if (((lI2 != 1073741824))) {
						G = 1;
						continue;
					}
					return (lA1 as JA_L);
				case 1:
					lI3 = (N.I(lI2 * 2));
					lA4 = this.makeTable_I__Ljava_util_HashMap_HashMapEntry_(lI3);
					if (((this._size != 0))) {
						G = 2;
						continue;
					}
					return (lA4 as JA_L);
				case 2:
					lI5 = 0;
					G = 3;
					continue;
				case 3:
					if (((lI5 >= lI2))) {
						G = 4;
						continue;
					}
					lA6 = ((lA1 as JA_L)).data[lI5];
					if (((lA6 != null))) {
						G = 5;
						continue;
					}
					G = 6;
					continue;
				case 5:
					lI7 = (N.I((lA6 as java_util_HashMap_HashMapEntry)._hash & lI2));
					lA8 = null;
					(lA4 as JA_L).data[(N.I(lI5 | lI7))] = lA6;
					lA9 = (lA6 as java_util_HashMap_HashMapEntry)._next;
					G = 7;
					continue;
				case 7:
					if (((lA9 == null))) {
						G = 8;
						continue;
					}
					lI10 = (N.I((lA9 as java_util_HashMap_HashMapEntry)._hash & lI2));
					if (((lI10 == lI7))) {
						G = 9;
						continue;
					}
					if (((lA8 != null))) {
						G = 10;
						continue;
					}
					(lA4 as JA_L).data[(N.I(lI5 | lI10))] = lA9;
					G = 11;
					continue;
				case 10:
					(lA8 as java_util_HashMap_HashMapEntry)._next = (lA9 as java_util_HashMap_HashMapEntry);
					G = 11;
					continue;
				case 11:
					lA8 = lA6;
					lI7 = lI10;
					G = 9;
					continue;
				case 9:
					lA6 = lA9;
					lA9 = (lA9 as java_util_HashMap_HashMapEntry)._next;
					G = 7;
					continue;
				case 8:
					if (((lA8 == null))) {
						G = 6;
						continue;
					}
					(lA8 as java_util_HashMap_HashMapEntry)._next = null;
					G = 6;
					continue;
				case 6:
					lI5 = (N.I(lI5 + 1));
					G = 3;
					continue;
				case 4:
					return (lA4 as JA_L);
				default:
					break;
			}
		}
		return null;
	}
	 JA_L makeTable_I__Ljava_util_HashMap_HashMapEntry_(int p0) {
		JA_L lA2 = null;
		lA2 = new JA_L(p0, "[Ljava.util.HashMap\$HashMapEntry;");
		this._table = lA2;
		this._threshold = (N.I((N.I(p0 >> 1)) + (N.I(p0 >> 2))));
		return lA2;
	}
	 java_lang_Object putValueForNullKey_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		java_lang_Object lA3 = null;
		while (true) {
			switch (G) {
				case 0:
					lA2 = this._entryForNullKey;
					if (((lA2 != null))) {
						G = 1;
						continue;
					}
					this.addNewEntryForNullKey_Ljava_lang_Object__V(p0);
					this._size = (N.I(this._size + 1));
					this._modCount = (N.I(this._modCount + 1));
					return null;
				case 1:
					this.preModify_Ljava_util_HashMap_HashMapEntry__V((lA2 as java_util_HashMap_HashMapEntry));
					lA3 = (lA2 as java_util_HashMap_HashMapEntry)._value;
					(lA2 as java_util_HashMap_HashMapEntry)._value = p0;
					return lA3;
				default:
					break;
			}
		}
		return null;
	}
	 void addNewEntryForNullKey_Ljava_lang_Object__V(java_lang_Object p0) {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_util_HashMap_HashMapEntry());
		fA1 = tA0;
		(tA0 as java_util_HashMap_HashMapEntry).java_util_HashMap_HashMapEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__V(null, p0, 0, null);
		this._entryForNullKey = (fA1 as java_util_HashMap_HashMapEntry);
		return;
	}
	 java_lang_Object clone__Ljava_lang_Object_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object lA2 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							G = 1;
							continue;
						case 1:
							lA1 = ((super.clone__Ljava_lang_Object_()) as java_util_HashMap);
							G = 2;
							continue;
						case 2:
							G = 3;
							continue;
						case 4:
							fA0 = J__exception__;
							lA2 = fA0;
							tA1 = (new java_lang_AssertionError());
							fA0 = tA1;
							(tA1 as java_lang_AssertionError).java_lang_AssertionError_init__Ljava_lang_Object__V(lA2);
							throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
							G = 3;
							continue;
						case 3:
							(lA1 as java_util_HashMap).makeTable_I__Ljava_util_HashMap_HashMapEntry_((this._table as JA_0).length);
							(lA1 as java_util_HashMap)._entryForNullKey = null;
							(lA1 as java_util_HashMap)._size = 0;
							(lA1 as java_util_HashMap)._keySet_ = null;
							(lA1 as java_util_HashMap)._entrySet = null;
							(lA1 as java_util_HashMap)._values = null;
							(lA1 as java_util_HashMap).init__V();
							(lA1 as java_util_HashMap).constructorPutAll_Ljava_util_Map__V(this);
							return lA1;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_CloneNotSupportedException)))) {
					G = 4;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	 void init__V() {
		return;
	}
	 void constructorPutAll_Ljava_util_Map__V(java_util_Map p0) {
		int G = 0;
		java_util_Map_Entry lA3 = null;
		java_util_Iterator lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((this._table != java_util_HashMap._EMPTY_TABLE))) {
						G = 1;
						continue;
					}
					this.doubleCapacity___Ljava_util_HashMap_HashMapEntry_();
					G = 1;
					continue;
				case 1:
					lA2 = p0.entrySet__Ljava_util_Set_().iterator__Ljava_util_Iterator_();
					G = 2;
					continue;
				case 2:
					if (!(lA2.hasNext__Z())) {
						G = 3;
						continue;
					}
					lA3 = ((lA2.next__Ljava_lang_Object_()) as java_util_Map_Entry);
					this.constructorPut_Ljava_lang_Object_Ljava_lang_Object__V(lA3.getKey__Ljava_lang_Object_(), lA3.getValue__Ljava_lang_Object_());
					G = 2;
					continue;
				case 3:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void constructorPut_Ljava_lang_Object_Ljava_lang_Object__V(java_lang_Object p0, java_lang_Object p1) {
		int G = 0;
		java_lang_Object lA3 = null;
		java_lang_Object lA4 = null;
		java_lang_Object lA6 = null;
		java_lang_Object lA7 = null;
		int lI3 = 0;
		int lI5 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					lA3 = this._entryForNullKey;
					if (((lA3 != null))) {
						G = 2;
						continue;
					}
					this._entryForNullKey = this.constructorNewEntry_Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__Ljava_util_HashMap_HashMapEntry_(null, p1, 0, null);
					this._size = (N.I(this._size + 1));
					G = 3;
					continue;
				case 2:
					(lA3 as java_util_HashMap_HashMapEntry)._value = p1;
					G = 3;
					continue;
				case 3:
					return;
					G = 1;
					continue;
				case 1:
					lI3 = java_util_HashMap.secondaryHash_Ljava_lang_Object__I(p0);
					lA4 = this._table;
					lI5 = (N.I(lI3 & (N.I((lA4 as JA_0).length - 1))));
					lA6 = ((lA4 as JA_L)).data[lI5];
					lA7 = lA6;
					G = 4;
					continue;
				case 4:
					if (((lA7 == null))) {
						G = 5;
						continue;
					}
					if ((((lA7 as java_util_HashMap_HashMapEntry)._hash != lI3))) {
						G = 6;
						continue;
					}
					if (!(p0.equals_Ljava_lang_Object__Z((lA7 as java_util_HashMap_HashMapEntry)._key))) {
						G = 6;
						continue;
					}
					(lA7 as java_util_HashMap_HashMapEntry)._value = p1;
					return;
					G = 6;
					continue;
				case 6:
					lA7 = (lA7 as java_util_HashMap_HashMapEntry)._next;
					G = 4;
					continue;
				case 5:
					(lA4 as JA_L).data[lI5] = this.constructorNewEntry_Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__Ljava_util_HashMap_HashMapEntry_(p0, p1, lI3, (lA6 as java_util_HashMap_HashMapEntry));
					this._size = (N.I(this._size + 1));
					return;
				default:
					break;
			}
		}
		return;
	}
	 java_util_HashMap_HashMapEntry constructorNewEntry_Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__Ljava_util_HashMap_HashMapEntry_(java_lang_Object p0, java_lang_Object p1, int p2, java_util_HashMap_HashMapEntry p3) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_util_HashMap_HashMapEntry());
		fA0 = tA0;
		(tA0 as java_util_HashMap_HashMapEntry).java_util_HashMap_HashMapEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__V(p0, p1, p2, p3);
		return (fA0 as java_util_HashMap_HashMapEntry);
	}
	 java_util_HashMap java_util_HashMap_init__I_V(int p0) {
		int G = 0;
		int lI1 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		JA_L lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = p0;
					this.java_util_AbstractMap_init___V();
					if (((lI1 >= 0))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_IllegalArgumentException());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_IllegalArgumentException).java_lang_IllegalArgumentException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_751).append_I_Ljava_lang_StringBuilder_(lI1).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					if (((lI1 != 0))) {
						G = 2;
						continue;
					}
					lA2 = ((java_util_HashMap._EMPTY_TABLE) as JA_L);
					this._table = lA2;
					this._threshold = -1;
					return this;
					G = 2;
					continue;
				case 2:
					if (((lI1 >= 4))) {
						G = 3;
						continue;
					}
					lI1 = 4;
					G = 4;
					continue;
				case 3:
					if (((lI1 <= 1073741824))) {
						G = 5;
						continue;
					}
					lI1 = 1073741824;
					G = 4;
					continue;
				case 5:
					lI1 = java_util_Collections.roundUpToPowerOfTwo_I_I(lI1);
					G = 4;
					continue;
				case 4:
					this.makeTable_I__Ljava_util_HashMap_HashMapEntry_(lI1);
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	java_util_HashMap([int CLASS_ID = 708]) : super(CLASS_ID) { }
	static void SI() {
		java_util_HashMap._EMPTY_TABLE = null;
		java_util_HashMap.java_util_HashMap_clinit___V();
	}
}
class com_jtransc_ds_FastStringMap extends java_lang_Object  {

	java_util_HashMap _map = null;
	 com_jtransc_ds_FastStringMap com_jtransc_ds_FastStringMap_init___V() {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		this.java_lang_Object_init___V();
		tA0 = (new java_util_HashMap());
		fA1 = tA0;
		(tA0 as java_util_HashMap).java_util_HashMap_init___V();
		this._map = (fA1 as java_util_HashMap);
		return this;
		return this;
	}
	 bool has_Ljava_lang_String__Z(java_lang_String p0) {
		return this._map.containsKey_Ljava_lang_Object__Z(p0);
	}
	 void set_Ljava_lang_String_Ljava_lang_Object__V(java_lang_String p0, java_lang_Object p1) {
		this._map.put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(p0, p1);
		return;
	}
	 java_lang_Object get_Ljava_lang_String__Ljava_lang_Object_(java_lang_String p0) {
		return this._map.get_Ljava_lang_Object__Ljava_lang_Object_(p0);
	}
	com_jtransc_ds_FastStringMap([int CLASS_ID = 707]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_Double extends java_lang_Number implements java_lang_Comparable {

	double _value = 0.0;
	static java_lang_Class _TYPE = null;
	static java_lang_Double valueOf_D_Ljava_lang_Double_(double p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_Double());
		fA0 = tA0;
		(tA0 as java_lang_Double).java_lang_Double_init__D_V(p0);
		return (fA0 as java_lang_Double);
	}
	 java_lang_Double java_lang_Double_init__D_V(double p0) {
		this.java_lang_Number_init___V();
		this._value = p0;
		return this;
		return this;
	}
	static void java_lang_Double_clinit___V() {
		java_lang_Double._TYPE = java_lang_Class.getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap.STRINGLIT_752);
		return;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		return java_lang_Double.toString_D_Ljava_lang_String_(this._value);
	}
	static java_lang_String toString_D_Ljava_lang_String_(double p0) {
		return com_jtransc_text_JTranscStringTools.toString_D_Ljava_lang_String_(p0);
	}
	static bool isNaN_D_Z(double p0) {
		return p0.isNaN;
	}
	static Int64 doubleToRawLongBits_D_J(double p0) {
		return N.doubleToLongBits(p0);
	}
	static bool isInfinite_D_Z(double p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (java_lang_Double.isNaN_D_Z(p0)) {
						G = 1;
						continue;
					}
					if (java_lang_Double._isFinite_D_Z(p0)) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	static bool _isFinite_D_Z(double p0) {
		return p0.isFinite;
	}
	 int hashCode__I() {
		return java_lang_Double.hashCode_D_I(this.doubleValue__D());
	}
	 double doubleValue__D() {
		return this._value;
	}
	static int hashCode_D_I(double p0) {
		return N.j2i(java_lang_Double.doubleToLongBits_D_J(p0));
	}
	static Int64 doubleToLongBits_D_J(double p0) {
		return N.doubleToLongBits(p0);
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(((p0) is java_lang_Double))) {
						G = 1;
						continue;
					}
					if ((((N.lcmp(java_lang_Double.doubleToLongBits_D_J(((p0) as java_lang_Float).doubleValue__D()), java_lang_Double.doubleToLongBits_D_J(this.doubleValue__D()))) != 0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 Int64 longValue__J() {
		return N.d2j(this._value);
	}
	 int intValue__I() {
		return (N.d2i(this._value));
	}
	 int shortValue__S() {
		return N.i2s(N.d2i(this._value));
	}
	 int byteValue__B() {
		return N.i2b(N.d2i(this._value));
	}
	 double floatValue__F() {
		return ((this._value));
	}
	static double longBitsToDouble_J_D(Int64 p0) {
		return N.longBitsToDouble(p0);
	}
	java_lang_Double([int CLASS_ID = 706]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_Double._TYPE = null;
		java_lang_Double.java_lang_Double_clinit___V();
	}
}
class java_lang_IndexOutOfBoundsException extends java_lang_RuntimeException  {

	 java_lang_IndexOutOfBoundsException java_lang_IndexOutOfBoundsException_init___V() {
		this.java_lang_RuntimeException_init___V();
		return this;
		return this;
	}
	 java_lang_IndexOutOfBoundsException java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_RuntimeException_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	java_lang_IndexOutOfBoundsException([int CLASS_ID = 704]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_ArrayIndexOutOfBoundsException extends java_lang_IndexOutOfBoundsException  {

	 java_lang_ArrayIndexOutOfBoundsException java_lang_ArrayIndexOutOfBoundsException_init___V() {
		this.java_lang_IndexOutOfBoundsException_init___V();
		return this;
		return this;
	}
	 java_lang_ArrayIndexOutOfBoundsException java_lang_ArrayIndexOutOfBoundsException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	java_lang_ArrayIndexOutOfBoundsException([int CLASS_ID = 703]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream extends java_io_OutputStream  {

	java_lang_StringBuilder _sb = null;
	bool _error = false;
	 com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_init__Z_V(bool p0) {
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		this.java_io_OutputStream_init___V();
		tA0 = (new java_lang_StringBuilder());
		fA1 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		this._sb = (fA1 as java_lang_StringBuilder);
		this._error = p0;
		return this;
		return this;
	}
	com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream([int CLASS_ID = 701]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream extends com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream  {

	 com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream_init___V() {
		this.com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_init__Z_V(true);
		return this;
		return this;
	}
	 void write_I_V(int p0) {
		stderr.writeCharCode(p0);
	}
	com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream([int CLASS_ID = 702]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream extends com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream  {

	 com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream_init___V() {
		this.com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_init__Z_V(false);
		return this;
		return this;
	}
	 void write_I_V(int p0) {
		stdout.writeCharCode(p0);
	}
	com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream([int CLASS_ID = 700]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_io_FilterOutputStream extends java_io_OutputStream  {

	java_io_OutputStream __out = null;
	 java_io_FilterOutputStream java_io_FilterOutputStream_init__Ljava_io_OutputStream__V(java_io_OutputStream p0) {
		this.java_io_OutputStream_init___V();
		this.__out = p0;
		return this;
		return this;
	}
	 void write_I_V(int p0) {
		this.__out.write_I_V(p0);
		return;
	}
	 void flush__V() {
		this.__out.flush__V();
		return;
	}
	 void write__B_V(JA_B p0) {
		this.write__BII_V(p0, 0, (p0 as JA_0).length);
		return;
	}
	 void write__BII_V(JA_B p0, int p1, int p2) {
		int G = 0;
		int lI4 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI4 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI4 >= p2))) {
						G = 2;
						continue;
					}
					this.write_I_V(((p0).data[(N.I(p1 + lI4))]));
					lI4 = (N.I(lI4 + 1));
					G = 1;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	java_io_FilterOutputStream([int CLASS_ID = 692]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_io_PrintStream extends java_io_FilterOutputStream implements java_lang_Appendable, java_io_Closeable {

	java_lang_String _encoding = null;
	bool _autoFlush = false;
	bool _ioError = false;
	 java_io_PrintStream java_io_PrintStream_init__Ljava_io_OutputStream__V(java_io_OutputStream p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					this.java_io_FilterOutputStream_init__Ljava_io_OutputStream__V(p0);
					this._encoding = Bootstrap.STRINGLIT_51;
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_NullPointerException());
					fA0 = tA0;
					(tA0 as java_lang_NullPointerException).java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap.STRINGLIT_753);
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 void println_D_V(double p0) {
		this.println_Ljava_lang_String__V(java_lang_String.valueOf_D_Ljava_lang_String_(p0));
		return;
	}
	 void println_Ljava_lang_String__V(java_lang_String p0) {
		this.print_Ljava_lang_String__V(p0);
		this.newline__V();
		return;
	}
	 void newline__V() {
		this.print_Ljava_lang_String__V(java_lang_System.lineSeparator__Ljava_lang_String_());
		return;
	}
	 void print_Ljava_lang_String__V(java_lang_String p0) {
		int G = 0;
		java_io_IOException fA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							G = 1;
							continue;
						case 1:
							this.write__B_V(p0.getBytes_Ljava_lang_String___B(this._encoding));
							G = 2;
							continue;
						case 2:
							G = 3;
							continue;
						case 4:
							fA0 = J__exception__;
							this.setError__V();
							G = 3;
							continue;
						case 3:
							return;
						default:
							break;
					}
				}
				return;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_io_IOException)))) {
					G = 4;
					continue;
				}
				rethrow;
			}
		}
		return;
	}
	 void write_I_V(int p0) {
		int G = 0;
		int fI0 = 0;
		int lI2 = 0;
		int lI3 = 0;
		java_io_IOException fA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							if (((this.__out != null))) {
								G = 1;
								continue;
							}
							this.setError__V();
							return;
							G = 1;
							continue;
						case 1:
							this.__out.write_I_V(p0);
							lI2 = (N.I(p0 & 255));
							if (((lI2 == 10))) {
								G = 2;
								continue;
							}
							if (((lI2 != 21))) {
								G = 3;
								continue;
							}
							G = 2;
							continue;
						case 2:
							fI0 = 1;
							G = 4;
							continue;
						case 3:
							fI0 = 0;
							G = 4;
							continue;
						case 4:
							lI3 = fI0;
							if (!(this._autoFlush)) {
								G = 5;
								continue;
							}
							if (((lI3 == 0))) {
								G = 5;
								continue;
							}
							this.flush__V();
							G = 5;
							continue;
						case 5:
							G = 6;
							continue;
						case 7:
							fA0 = J__exception__;
							this.setError__V();
							G = 6;
							continue;
						case 6:
							return;
						default:
							break;
					}
				}
				return;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 5)))) && ((J__exception__) is java_io_IOException)))) {
					G = 7;
					continue;
				}
				rethrow;
			}
		}
		return;
	}
	 void flush__V() {
		int G = 0;
		java_io_IOException fA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							if (((this.__out == null))) {
								G = 1;
								continue;
							}
							G = 2;
							continue;
						case 2:
							this.__out.flush__V();
							G = 3;
							continue;
						case 3:
							return;
							G = 4;
							continue;
						case 4:
							fA0 = J__exception__;
							G = 1;
							continue;
						case 1:
							this.setError__V();
							return;
						default:
							break;
					}
				}
				return;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 2)) && ((G < 3)))) && ((J__exception__) is java_io_IOException)))) {
					G = 4;
					continue;
				}
				rethrow;
			}
		}
		return;
	}
	 void setError__V() {
		this._ioError = true;
		return;
	}
	 void write__BII_V(JA_B p0, int p1, int p2) {
		int G = 0;
		java_lang_Object lA4 = null;
		java_lang_Object lA6 = null;
		java_lang_Object fA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							com_jtransc_JTranscArrays.checkOffsetAndCount_III_V((p0 as JA_0).length, p1, p2);
							fA0 = this;
							lA4 = this;
							N.monitorEnter(fA0);
							G = 1;
							continue;
						case 1:
							if (((this.__out != null))) {
								G = 2;
								continue;
							}
							this.setError__V();
							N.monitorExit(lA4);
							G = 3;
							continue;
						case 3:
							return;
							G = 2;
							continue;
						case 2:
							this.__out.write__BII_V(p0, p1, p2);
							if (!(this._autoFlush)) {
								G = 4;
								continue;
							}
							this.flush__V();
							G = 4;
							continue;
						case 4:
							G = 5;
							continue;
						case 6:
							fA0 = J__exception__;
							this.setError__V();
							G = 5;
							continue;
						case 5:
							N.monitorExit(lA4);
							G = 7;
							continue;
						case 7:
							G = 8;
							continue;
						case 9:
							lA6 = J__exception__;
							N.monitorExit(lA4);
							G = 10;
							continue;
						case 10:
							throw (lA6).prepareThrow__Ljava_lang_Throwable_().dartError;
							G = 8;
							continue;
						case 8:
							return;
						default:
							break;
					}
				}
				return;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 2)) && ((G < 4)))) && ((J__exception__) is java_io_IOException)))) {
					G = 6;
					continue;
				}
				if (((((((G >= 1)) && ((G < 3)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 9;
					continue;
				}
				if (((((((G >= 2)) && ((G < 7)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 9;
					continue;
				}
				if (((((((G >= 9)) && ((G < 10)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 9;
					continue;
				}
				rethrow;
			}
		}
		return;
	}
	java_io_PrintStream([int CLASS_ID = 689]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_io_JTranscConsolePrintStream extends java_io_PrintStream  {

	bool _error = false;
	com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream _stream = null;
	 com_jtransc_io_JTranscConsolePrintStream com_jtransc_io_JTranscConsolePrintStream_init__Z_V(bool p0) {
		int G = 0;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(p0)) {
						G = 1;
						continue;
					}
					tA0 = (new com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream());
					fA1 = tA0;
					(tA0 as com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream).com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream_init___V();
					G = 2;
					continue;
				case 1:
					tA1 = (new com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream());
					fA1 = tA1;
					(tA1 as com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream).com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream_init___V();
					G = 2;
					continue;
				case 2:
					this.com_jtransc_io_JTranscConsolePrintStream_init__Lcom_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_Z_V((fA1 as com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream), p0);
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 com_jtransc_io_JTranscConsolePrintStream com_jtransc_io_JTranscConsolePrintStream_init__Lcom_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_Z_V(com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream p0, bool p1) {
		this.java_io_PrintStream_init__Ljava_io_OutputStream__V(p0);
		this._stream = p0;
		this._error = p1;
		return this;
		return this;
	}
	 void println_Ljava_lang_String__V(java_lang_String p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_StringBuilder());
		fA0 = tA0;
		(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
		com_jtransc_io_JTranscConsole.logOrError_Ljava_lang_Object_Z_V((fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this._stream._sb.toString__Ljava_lang_String_()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(p0).toString__Ljava_lang_String_(), this._error);
		this._stream._sb.setLength_I_V(0);
		return;
	}
	com_jtransc_io_JTranscConsolePrintStream([int CLASS_ID = 699]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_io_InputStream extends java_lang_Object implements java_io_Closeable {

	 java_io_InputStream java_io_InputStream_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	java_io_InputStream([int CLASS_ID = 698]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_System_1 extends java_io_InputStream  {

	 java_lang_System_1 java_lang_System_1_init___V() {
		this.java_io_InputStream_init___V();
		return this;
		return this;
	}
	java_lang_System_1([int CLASS_ID = 697]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_NullPointerException extends java_lang_RuntimeException  {

	 java_lang_NullPointerException java_lang_NullPointerException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_RuntimeException_init__Ljava_lang_String__V(p0);
		return this;
		return this;
	}
	 java_lang_NullPointerException java_lang_NullPointerException_init__Ljava_lang_String_Ljava_lang_Throwable__V(java_lang_String p0, java_lang_Throwable p1) {
		this.java_lang_RuntimeException_init__Ljava_lang_String_Ljava_lang_Throwable__V(p0, p1);
		return this;
		return this;
	}
	 java_lang_NullPointerException java_lang_NullPointerException_init__Ljava_lang_Throwable__V(java_lang_Throwable p0) {
		this.java_lang_RuntimeException_init__Ljava_lang_Throwable__V(p0);
		return this;
		return this;
	}
	 java_lang_NullPointerException java_lang_NullPointerException_init___V() {
		this.java_lang_RuntimeException_init___V();
		return this;
		return this;
	}
	java_lang_NullPointerException([int CLASS_ID = 695]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_System extends java_lang_Object  {

	static java_io_InputStream __in = null;
	static java_io_PrintStream __out = null;
	static java_io_PrintStream _err = null;
	static java_util_Properties __props = null;
	 java_lang_System java_lang_System_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void java_lang_System_clinit___V() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		tA0 = (new java_lang_System_1());
		fA0 = tA0;
		(tA0 as java_lang_System_1).java_lang_System_1_init___V();
		java_lang_System.__in = (fA0 as java_io_InputStream);
		tA1 = (new com_jtransc_io_JTranscConsolePrintStream());
		fA0 = tA1;
		(tA1 as com_jtransc_io_JTranscConsolePrintStream).com_jtransc_io_JTranscConsolePrintStream_init__Z_V(false);
		java_lang_System.__out = (fA0 as java_io_PrintStream);
		tA2 = (new com_jtransc_io_JTranscConsolePrintStream());
		fA0 = tA2;
		(tA2 as com_jtransc_io_JTranscConsolePrintStream).com_jtransc_io_JTranscConsolePrintStream_init__Z_V(true);
		java_lang_System._err = (fA0 as java_io_PrintStream);
		return;
	}
	static void arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(java_lang_Object p0, int p1, java_lang_Object p2, int p3, int p4) {
		N.arraycopy(p0, p1, p2, p3, p4);
	}
	static int identityHashCode_Ljava_lang_Object__I(java_lang_Object p0) {
		return java_lang_SystemInt.identityHashCode_Ljava_lang_Object__I(p0);
	}
	static void gc__V() {
		java_lang_Runtime.getRuntime__Ljava_lang_Runtime_().gc__V();
		return;
	}
	static java_lang_String lineSeparator__Ljava_lang_String_() {
		return com_jtransc_JTranscSystem.lineSeparator__Ljava_lang_String_();
	}
	static Int64 currentTimeMillis__J() {
		return N.d2j(com_jtransc_JTranscSystem.fastTime__D());
	}
	static java_lang_String getProperty_Ljava_lang_String__Ljava_lang_String_(java_lang_String p0) {
		return java_lang_System.getProps__Ljava_util_Properties_().getProperty_Ljava_lang_String__Ljava_lang_String_(p0);
	}
	static java_util_Properties getProps__Ljava_util_Properties_() {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((java_lang_System.__props != null))) {
						G = 1;
						continue;
					}
					tA0 = (new java_util_Properties());
					fA0 = tA0;
					(tA0 as java_util_Properties).java_util_Properties_init___V();
					java_lang_System.__props = (fA0 as java_util_Properties);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_144, com_jtransc_JTranscSystem.getArch__Ljava_lang_String_(), Bootstrap.STRINGLIT_134);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_754, com_jtransc_JTranscSystem.getOS__Ljava_lang_String_(), Bootstrap.STRINGLIT_134);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_755, Bootstrap.STRINGLIT_756);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_757, com_jtransc_JTranscSystem.getRuntimeName__Ljava_lang_String_(), Bootstrap.STRINGLIT_758);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_759, Bootstrap.STRINGLIT_760);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_761, Bootstrap.STRINGLIT_762);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_763, Bootstrap.STRINGLIT_764);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_765, com_jtransc_JTranscSystem.fileSeparator__Ljava_lang_String_(), Bootstrap.STRINGLIT_49);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_766, com_jtransc_JTranscSystem.lineSeparator__Ljava_lang_String_(), Bootstrap.STRINGLIT_42);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_767, com_jtransc_JTranscSystem.pathSeparator__Ljava_lang_String_(), Bootstrap.STRINGLIT_52);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_768, com_jtransc_JTranscSystemProperties.fileEncoding__Ljava_lang_String_());
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_143, com_jtransc_JTranscSystem.getJavaHome__Ljava_lang_String_(), Bootstrap.STRINGLIT_49);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_769, com_jtransc_JTranscSystem.getRuntimeName__Ljava_lang_String_(), Bootstrap.STRINGLIT_758);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_770, Bootstrap.STRINGLIT_771);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_772, Bootstrap.STRINGLIT_773);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_774, Bootstrap.STRINGLIT_771);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_775, Bootstrap.STRINGLIT_776);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_777, Bootstrap.STRINGLIT_125);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_778, Bootstrap.STRINGLIT_779);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_780, Bootstrap.STRINGLIT_771);
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_781, com_jtransc_JTranscVersion.getVersion__Ljava_lang_String_());
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_782, com_jtransc_JTranscSystemProperties.tmpdir__Ljava_lang_String_());
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_783, com_jtransc_JTranscSystemProperties.userHome__Ljava_lang_String_());
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_784, com_jtransc_JTranscSystemProperties.userDir__Ljava_lang_String_());
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_785, com_jtransc_JTranscSystemProperties.userName__Ljava_lang_String_());
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_786, com_jtransc_JTranscSystemProperties.userLanguage__Ljava_lang_String_());
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_787, com_jtransc_JTranscSystemProperties.userRegion__Ljava_lang_String_());
					java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap.STRINGLIT_788, com_jtransc_JTranscSystemProperties.userVariant__Ljava_lang_String_());
					G = 1;
					continue;
				case 1:
					return java_lang_System.__props;
				default:
					break;
			}
		}
		return null;
	}
	static java_lang_String getenv_Ljava_lang_String__Ljava_lang_String_(java_lang_String p0) {
		return N.str(Platform.environment[N.istr(p0)]);
	}
	static void _setProperty_Ljava_lang_String_Ljava_lang_String__V(java_lang_String p0, java_lang_String p1) {
		java_lang_System._setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(p0, p1, Bootstrap.STRINGLIT_20);
		return;
	}
	static void _setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(java_lang_String p0, java_lang_String p1, java_lang_String p2) {
		int G = 0;
		java_lang_Object lA0 = null;
		java_lang_Object lA1 = null;
		while (true) {
			switch (G) {
				case 0:
					lA0 = p0;
					lA1 = p1;
					if (((lA0 != null))) {
						G = 1;
						continue;
					}
					lA0 = Bootstrap.STRINGLIT_20;
					G = 1;
					continue;
				case 1:
					if (((lA1 != null))) {
						G = 2;
						continue;
					}
					lA1 = p2;
					G = 2;
					continue;
				case 2:
					java_lang_System.getProps__Ljava_util_Properties_().put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(lA0, lA1);
					return;
				default:
					break;
			}
		}
		return;
	}
	java_lang_System([int CLASS_ID = 688]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_System.__in = null;
		java_lang_System.__out = null;
		java_lang_System._err = null;
		java_lang_System.__props = null;
		java_lang_System.java_lang_System_clinit___V();
	}
}
class java_util_Arrays extends java_lang_Object  {

	 java_util_Arrays java_util_Arrays_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static JA_C copyOf__CI__C(JA_C p0, int p1) {
		return java_util_Arrays.copyOfRange__CII__C(p0, 0, p1);
	}
	static JA_C copyOfRange__CII__C(JA_C p0, int p1, int p2) {
		java_lang_Object lA6 = null;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		lI3 = java_util_Arrays.checkRange_III_I(p1, p2, (p0 as JA_0).length);
		lI4 = (N.I(p2 - p1));
		lI5 = java_lang_Math.min_II_I(lI4, (N.I(lI3 - p1)));
		lA6 = new JA_C(lI4);
		java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(p0, p1, lA6, 0, lI5);
		return (lA6 as JA_C);
	}
	static int checkRange_III_I(int p0, int p1, int p2) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 <= p1))) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_IllegalArgumentException());
					fA0 = tA0;
					(tA0 as java_lang_IllegalArgumentException).java_lang_IllegalArgumentException_init___V();
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					if (((p0 < 0))) {
						G = 2;
						continue;
					}
					if (((p0 <= p2))) {
						G = 3;
						continue;
					}
					G = 2;
					continue;
				case 2:
					tA1 = (new java_lang_ArrayIndexOutOfBoundsException());
					fA0 = tA1;
					(tA1 as java_lang_ArrayIndexOutOfBoundsException).java_lang_ArrayIndexOutOfBoundsException_init___V();
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 3;
					continue;
				case 3:
					return p2;
				default:
					break;
			}
		}
		return 0;
	}
	static JA_L copyOf__Ljava_lang_Object_I__Ljava_lang_Object_(JA_L p0, int p1) {
		return java_util_Arrays.copyOfRange__Ljava_lang_Object_II__Ljava_lang_Object_(p0, 0, p1);
	}
	static JA_L copyOfRange__Ljava_lang_Object_II__Ljava_lang_Object_(JA_L p0, int p1, int p2) {
		java_lang_Object lA6 = null;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		lI3 = java_util_Arrays.checkRange_III_I(p1, p2, (p0 as JA_0).length);
		lI4 = (N.I(p2 - p1));
		lI5 = java_lang_Math.min_II_I(lI4, (N.I(lI3 - p1)));
		lA6 = java_util_Arrays.newInstance__Ljava_lang_Object_I__Ljava_lang_Object_(p0, lI4);
		java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(p0, p1, lA6, 0, lI5);
		return (lA6 as JA_L);
	}
	static JA_L newInstance__Ljava_lang_Object_I__Ljava_lang_Object_(JA_L p0, int p1) {
		return ((java_lang_reflect_Array.newInstance_Ljava_lang_Class_I_Ljava_lang_Object_(p0.getClass__Ljava_lang_Class_().getComponentType__Ljava_lang_Class_(), p1)) as JA_L);
	}
	static JA_L copyOf__Ljava_lang_Object_ILjava_lang_Class___Ljava_lang_Object_(JA_L p0, int p1, java_lang_Class p2) {
		return java_util_Arrays.copyOfRange__Ljava_lang_Object_IILjava_lang_Class___Ljava_lang_Object_(p0, 0, p1, p2);
	}
	static JA_L copyOfRange__Ljava_lang_Object_IILjava_lang_Class___Ljava_lang_Object_(JA_L p0, int p1, int p2, java_lang_Class p3) {
		java_lang_Object lA7 = null;
		int lI4 = 0;
		int lI5 = 0;
		int lI6 = 0;
		lI4 = java_util_Arrays.checkRange_III_I(p1, p2, (p0 as JA_0).length);
		lI5 = (N.I(p2 - p1));
		lI6 = java_lang_Math.min_II_I(lI5, (N.I(lI4 - p1)));
		lA7 = ((java_lang_reflect_Array.newInstance_Ljava_lang_Class_I_Ljava_lang_Object_(p3.getComponentType__Ljava_lang_Class_(), lI5)) as JA_L);
		java_lang_System.arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(p0, p1, lA7, 0, lI6);
		return (lA7 as JA_L);
	}
	static bool equals__Ljava_lang_Object__Ljava_lang_Object__Z(JA_L p0, JA_L p1) {
		int G = 0;
		java_lang_Object lA3 = null;
		java_lang_Object lA4 = null;
		int lI2 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != p1))) {
						G = 1;
						continue;
					}
					return true;
				case 1:
					if (((p0 == null))) {
						G = 2;
						continue;
					}
					if (((p1 == null))) {
						G = 2;
						continue;
					}
					if ((((p0 as JA_0).length == (p1 as JA_0).length))) {
						G = 3;
						continue;
					}
					G = 2;
					continue;
				case 2: return false;
				case 3:
					lI2 = 0;
					G = 4;
					continue;
				case 4:
					if (((lI2 >= (p0 as JA_0).length))) {
						G = 5;
						continue;
					}
					lA3 = (p0).data[lI2];
					lA4 = (p1).data[lI2];
					if (((lA3 != null))) {
						G = 6;
						continue;
					}
					if (((lA4 != null))) {
						G = 7;
						continue;
					}
					G = 8;
					continue;
				case 6:
					if (lA3.equals_Ljava_lang_Object__Z(lA4)) {
						G = 8;
						continue;
					}
					G = 7;
					continue;
				case 7: return false;
				case 8:
					lI2 = (N.I(lI2 + 1));
					G = 4;
					continue;
				case 5:
					return true;
				default:
					break;
			}
		}
		return false;
	}
	static java_util_List asList__Ljava_lang_Object__Ljava_util_List_(JA_L p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_util_Arrays_ArrayList());
		fA0 = tA0;
		(tA0 as java_util_Arrays_ArrayList).java_util_Arrays_ArrayList_init___Ljava_lang_Object__V(p0);
		return (fA0 as java_util_List);
	}
	static JA_L access_000__Ljava_lang_Object_I__Ljava_lang_Object_(JA_L p0, int p1) {
		return java_util_Arrays.newInstance__Ljava_lang_Object_I__Ljava_lang_Object_(p0, p1);
	}
	java_util_Arrays([int CLASS_ID = 687]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_Character extends java_lang_Object implements java_io_Serializable, java_lang_Comparable {

	int _value = 0;
	static java_lang_Class _TYPE = null;
	 java_lang_Character java_lang_Character_init__C_V(int p0) {
		this.java_lang_Object_init___V();
		this._value = p0;
		return this;
		return this;
	}
	static java_lang_Character valueOf_C_Ljava_lang_Character_(int p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_Character());
		fA0 = tA0;
		(tA0 as java_lang_Character).java_lang_Character_init__C_V(p0);
		return (fA0 as java_lang_Character);
	}
	static void java_lang_Character_clinit___V() {
		java_lang_Character._TYPE = java_lang_Class.getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap.STRINGLIT_789);
		return;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		return java_lang_Character.toString_C_Ljava_lang_String_(this._value);
	}
	static java_lang_String toString_C_Ljava_lang_String_(int p0) {
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		tA0 = (new java_lang_String());
		fA0 = tA0;
		fA1 = tA0;
		tA1 = new JA_C(1);
		fA2 = tA1;
		(tA1 as JA_C).data[0] = p0;
		(fA1 as java_lang_String).java_lang_String_init___C_V((fA2 as JA_C));
		return (fA0 as java_lang_String);
	}
	static bool isDigit_C_Z(int p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if ((((p0) < 48))) {
						G = 1;
						continue;
					}
					if ((((p0) > 57))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	static int forDigit_II_C(int p0, int p1) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 < 0))) {
						G = 1;
						continue;
					}
					if (((p0 > 9))) {
						G = 1;
						continue;
					}
					return N.i2c((N.I(48 + (N.I(p0 - 0)))));
				case 1:
					if (((p0 < 10))) {
						G = 2;
						continue;
					}
					if (((p0 > 35))) {
						G = 2;
						continue;
					}
					return N.i2c((N.I(97 + (N.I(p0 - 10)))));
				case 2:
					return 0;
				default:
					break;
			}
		}
		return 0;
	}
	 int hashCode__I() {
		return (this._value);
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(((p0) is java_lang_Character))) {
						G = 1;
						continue;
					}
					if (((this._value != ((p0) as java_lang_Character)._value))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 int charValue__C() {
		return this._value;
	}
	static int reverseBytes_C_C(int p0) {
		return N.i2c((N.I((N.I((N.I((p0) & 65280)) >> 8)) | (N.I((p0) << 8)))));
	}
	java_lang_Character([int CLASS_ID = 686]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_Character._TYPE = null;
		java_lang_Character.java_lang_Character_clinit___V();
	}
}
class java_lang_Math extends java_lang_Object  {

	 java_lang_Math java_lang_Math_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static int min_II_I(int p0, int p1) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 >= p1))) {
						G = 1;
						continue;
					}
					fI0 = p0;
					G = 2;
					continue;
				case 1:
					fI0 = p1;
					G = 2;
					continue;
				case 2: return fI0;
				default:
					break;
			}
		}
		return 0;
	}
	static int max_II_I(int p0, int p1) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 <= p1))) {
						G = 1;
						continue;
					}
					fI0 = p0;
					G = 2;
					continue;
				case 1:
					fI0 = p1;
					G = 2;
					continue;
				case 2: return fI0;
				default:
					break;
			}
		}
		return 0;
	}
	java_lang_Math([int CLASS_ID = 685]) : super(CLASS_ID) { }
	static void SI() { }
}
class com_jtransc_text_JTranscStringTools extends java_lang_Object  {

	 com_jtransc_text_JTranscStringTools com_jtransc_text_JTranscStringTools_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static java_lang_String toString_F_Ljava_lang_String_(double p0) {
		int G = 0;
		int lI2 = 0;
		java_lang_String lA1 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = com_jtransc_text_JTranscStringTools.toString_D_Ljava_lang_String_(((p0)));
					lI2 = lA1.indexOf_I_I(46);
					if (((lI2 < 0))) {
						G = 1;
						continue;
					}
					return lA1.substring_II_Ljava_lang_String_(0, java_lang_Math.min_II_I(lA1.length__I(), (N.I(lI2 + 6))));
				case 1:
					return lA1;
				default:
					break;
			}
		}
		return null;
	}
	static java_lang_String toString_D_Ljava_lang_String_(double p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		int lI3 = 0;
		int lI4 = 0;
		int lI5 = 0;
		java_lang_Object fA0 = null;
		Int64 lJ2 = N.lnew(0);
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (!(java_lang_Double.isNaN_D_Z(p0))) {
						G = 1;
						continue;
					}
					return Bootstrap.STRINGLIT_790;
				case 1:
					if (!(java_lang_Double.isInfinite_D_Z(p0))) {
						G = 2;
						continue;
					}
					if (!(((p0 < 0.0)))) {
						G = 3;
						continue;
					}
					fA0 = Bootstrap.STRINGLIT_791;
					G = 4;
					continue;
				case 3:
					fA0 = Bootstrap.STRINGLIT_792;
					G = 4;
					continue;
				case 4: return (fA0 as java_lang_String);
				case 2:
					if ((((N.cmpl(p0, 0.0)) != 0))) {
						G = 5;
						continue;
					}
					lJ2 = java_lang_Double.doubleToRawLongBits_D_J(p0);
					if ((((N.lcmp((N.lushr_opt(lJ2, 63)), N.lnew(0))) == 0))) {
						G = 5;
						continue;
					}
					return Bootstrap.STRINGLIT_793;
				case 5:
					lA2 = com_jtransc_text_JTranscStringTools._toString_D_Ljava_lang_String_(p0);
					lI3 = 0;
					lI4 = 0;
					G = 6;
					continue;
				case 6:
					if (((lI4 >= (lA2 as java_lang_String).length__I()))) {
						G = 7;
						continue;
					}
					lI5 = ((lA2 as java_lang_String).charAt_I_C(lI4));
					if (java_lang_Character.isDigit_C_Z(N.i2c(lI5))) {
						G = 8;
						continue;
					}
					if (((lI5 == 45))) {
						G = 8;
						continue;
					}
					lI3 = 1;
					G = 7;
					continue;
				case 8:
					lI4 = (N.I(lI4 + 1));
					G = 6;
					continue;
				case 7:
					if ((((lA2 as java_lang_String).indexOf_Ljava_lang_String__I(Bootstrap.STRINGLIT_794) < 0))) {
						G = 9;
						continue;
					}
					lA2 = (lA2 as java_lang_String).replace_Ljava_lang_CharSequence_Ljava_lang_CharSequence__Ljava_lang_String_(Bootstrap.STRINGLIT_794, Bootstrap.STRINGLIT_795);
					G = 9;
					continue;
				case 9:
					if ((((lA2 as java_lang_String).indexOf_Ljava_lang_String__I(Bootstrap.STRINGLIT_796) < 0))) {
						G = 10;
						continue;
					}
					lA2 = (lA2 as java_lang_String).replace_Ljava_lang_CharSequence_Ljava_lang_CharSequence__Ljava_lang_String_(Bootstrap.STRINGLIT_796, Bootstrap.STRINGLIT_797);
					G = 10;
					continue;
				case 10:
					if (((lI3 == 0))) {
						G = 11;
						continue;
					}
					fA0 = lA2;
					G = 12;
					continue;
				case 11:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					fA0 = (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_((lA2 as java_lang_String)).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_798).toString__Ljava_lang_String_();
					G = 12;
					continue;
				case 12: return (fA0 as java_lang_String);
				default:
					break;
			}
		}
		return null;
	}
	static java_lang_String _toString_D_Ljava_lang_String_(double p0) {
		return N.str(p0.toString());
	}
	com_jtransc_text_JTranscStringTools([int CLASS_ID = 684]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_Float extends java_lang_Number implements java_lang_Comparable {

	double _value = 0.0;
	static java_lang_Class _TYPE = null;
	static java_lang_Float valueOf_F_Ljava_lang_Float_(double p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_Float());
		fA0 = tA0;
		(tA0 as java_lang_Float).java_lang_Float_init__F_V(p0);
		return (fA0 as java_lang_Float);
	}
	 java_lang_Float java_lang_Float_init__F_V(double p0) {
		this.java_lang_Number_init___V();
		this._value = p0;
		return this;
		return this;
	}
	static void java_lang_Float_clinit___V() {
		java_lang_Float._TYPE = java_lang_Class.getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap.STRINGLIT_799);
		return;
	}
	 java_lang_String toString__Ljava_lang_String_() {
		return java_lang_Float.toString_F_Ljava_lang_String_(this._value);
	}
	static java_lang_String toString_F_Ljava_lang_String_(double p0) {
		return com_jtransc_text_JTranscStringTools.toString_F_Ljava_lang_String_(p0);
	}
	 int hashCode__I() {
		return java_lang_Float.hashCode_F_I(this._value);
	}
	static int hashCode_F_I(double p0) {
		return java_lang_Float.floatToIntBits_F_I(p0);
	}
	static int floatToIntBits_F_I(double p0) {
		return N.floatToIntBits(p0);
	}
	 double doubleValue__D() {
		return ((this._value));
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(((p0) is java_lang_Float))) {
						G = 1;
						continue;
					}
					if (((java_lang_Float.floatToIntBits_F_I(((p0) as java_lang_Float)._value) != java_lang_Float.floatToIntBits_F_I(this._value)))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 Int64 longValue__J() {
		return N.f2j(this._value);
	}
	 int intValue__I() {
		return (N.f2i(this._value));
	}
	 int shortValue__S() {
		return N.i2s(N.f2i(this._value));
	}
	 int byteValue__B() {
		return N.i2b(N.f2i(this._value));
	}
	 double floatValue__F() {
		return this._value;
	}
	static double intBitsToFloat_I_F(int p0) {
		return N.intBitsToFloat(p0);
	}
	static int floatToRawIntBits_F_I(double p0) {
		return N.floatToIntBits(p0);
	}
	java_lang_Float([int CLASS_ID = 683]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_Float._TYPE = null;
		java_lang_Float.java_lang_Float_clinit___V();
	}
}
class java_lang_Void extends java_lang_Object  {

	static java_lang_Class _TYPE = null;
	 java_lang_Void java_lang_Void_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void java_lang_Void_clinit___V() {
		java_lang_Void._TYPE = java_lang_Class.getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap.STRINGLIT_800);
		return;
	}
	java_lang_Void([int CLASS_ID = 682]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_Void._TYPE = null;
		java_lang_Void.java_lang_Void_clinit___V();
	}
}
class java_lang_ClassNotFoundException extends java_lang_ReflectiveOperationException  {

	java_lang_Throwable _ex = null;
	 java_lang_ClassNotFoundException java_lang_ClassNotFoundException_init__Ljava_lang_String__V(java_lang_String p0) {
		this.java_lang_ReflectiveOperationException_init__Ljava_lang_String_Ljava_lang_Throwable__V(p0, null);
		return this;
		return this;
	}
	 java_lang_ClassNotFoundException java_lang_ClassNotFoundException_init__Ljava_lang_String_Ljava_lang_Throwable__V(java_lang_String p0, java_lang_Throwable p1) {
		this.java_lang_ReflectiveOperationException_init__Ljava_lang_String_Ljava_lang_Throwable__V(p0, null);
		this._ex = p1;
		return this;
		return this;
	}
	 java_lang_ClassNotFoundException java_lang_ClassNotFoundException_init___V() {
		this.java_lang_ReflectiveOperationException_init__Ljava_lang_Throwable__V(null);
		return this;
		return this;
	}
	java_lang_ClassNotFoundException([int CLASS_ID = 678]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_reflect__InternalUtils extends java_lang_Object  {

	 java_lang_reflect__InternalUtils java_lang_reflect__InternalUtils_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static java_lang_String getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(java_lang_reflect_Type p0) {
		int G = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(((p0) is java_lang_Class))) {
						G = 1;
						continue;
					}
					return ((p0) as java_lang_Class).getName__Ljava_lang_String_();
				case 1:
					return p0.toString__Ljava_lang_String_();
				default:
					break;
			}
		}
		return null;
	}
	static java_lang_reflect_MethodTypeImpl parseMethodType_Ljava_lang_String_Ljava_lang_reflect_Type__Ljava_lang_reflect_MethodTypeImpl_(java_lang_String p0, java_lang_reflect_Type p1) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new com_jtransc_text_MStringReader());
		fA0 = tA0;
		(tA0 as com_jtransc_text_MStringReader).com_jtransc_text_MStringReader_init__Ljava_lang_String__V(p0);
		return ((java_lang_reflect__InternalUtils.parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_((fA0 as com_jtransc_text_MStringReader), p1)) as java_lang_reflect_MethodTypeImpl);
	}
	static java_lang_reflect_Type parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_(com_jtransc_text_MStringReader p0, java_lang_reflect_Type p1) {
		int G = 0;
		java_lang_Object lA6 = null;
		int lI2 = 0;
		int lI5 = 0;
		int lI7 = 0;
		int lI8 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA2 = null;
		java_lang_Object tA3 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object lA10 = null;
		int fI0 = 0;
		JA_L lA3 = null;
		java_lang_reflect_Type lA4 = null;
		java_lang_String lA9 = null;
		while (true) {
			switch (G) {
				case 0:
					lI2 = (p0.read__C());
					switch (lI2) {
						case 40:
							G = 2;
							continue;
						case 41:
							G = 1;
							continue;
						case 42:
							G = 1;
							continue;
						case 43:
							G = 3;
							continue;
						case 44:
							G = 1;
							continue;
						case 45:
							G = 1;
							continue;
						case 46:
							G = 1;
							continue;
						case 47:
							G = 1;
							continue;
						case 48:
							G = 1;
							continue;
						case 49:
							G = 1;
							continue;
						case 50:
							G = 1;
							continue;
						case 51:
							G = 1;
							continue;
						case 52:
							G = 1;
							continue;
						case 53:
							G = 1;
							continue;
						case 54:
							G = 1;
							continue;
						case 55:
							G = 1;
							continue;
						case 56:
							G = 1;
							continue;
						case 57:
							G = 1;
							continue;
						case 58:
							G = 1;
							continue;
						case 59:
							G = 1;
							continue;
						case 60:
							G = 1;
							continue;
						case 61:
							G = 1;
							continue;
						case 62:
							G = 1;
							continue;
						case 63:
							G = 1;
							continue;
						case 64:
							G = 1;
							continue;
						case 65:
							G = 1;
							continue;
						case 66:
							G = 4;
							continue;
						case 67:
							G = 5;
							continue;
						case 68:
							G = 6;
							continue;
						case 69:
							G = 1;
							continue;
						case 70:
							G = 7;
							continue;
						case 71:
							G = 1;
							continue;
						case 72:
							G = 1;
							continue;
						case 73:
							G = 8;
							continue;
						case 74:
							G = 9;
							continue;
						case 75:
							G = 1;
							continue;
						case 76:
							G = 10;
							continue;
						case 77:
							G = 1;
							continue;
						case 78:
							G = 1;
							continue;
						case 79:
							G = 1;
							continue;
						case 80:
							G = 1;
							continue;
						case 81:
							G = 1;
							continue;
						case 82:
							G = 1;
							continue;
						case 83:
							G = 11;
							continue;
						case 84:
							G = 10;
							continue;
						case 85:
							G = 1;
							continue;
						case 86:
							G = 12;
							continue;
						case 87:
							G = 1;
							continue;
						case 88:
							G = 1;
							continue;
						case 89:
							G = 1;
							continue;
						case 90:
							G = 13;
							continue;
						case 91:
							G = 14;
							continue;
						default:
							G = 1;
							continue;
					}
					G = 2;
					continue;
				case 2:
					lA3 = java_lang_reflect__InternalUtils.parseTypes_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type___Ljava_lang_reflect_Type_(p0, p1);
					p0.expect_C_V(41);
					lA4 = java_lang_reflect__InternalUtils.parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_(p0, p1);
					tA2 = (new java_lang_reflect_MethodTypeImpl());
					fA0 = tA2;
					(tA2 as java_lang_reflect_MethodTypeImpl).java_lang_reflect_MethodTypeImpl_init___Ljava_lang_reflect_Type_Ljava_lang_reflect_Type__V(lA3, lA4);
					return (fA0 as java_lang_reflect_Type);
				case 3:
					return java_lang_reflect__InternalUtils.parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_(p0, p1);
				case 12:
					return java_lang_Void._TYPE;
				case 13:
					return java_lang_Boolean._TYPE;
				case 4:
					return java_lang_Byte._TYPE;
				case 5:
					return java_lang_Character._TYPE;
				case 11:
					return java_lang_Short._TYPE;
				case 6:
					return java_lang_Double._TYPE;
				case 7:
					return java_lang_Float._TYPE;
				case 8:
					return java_lang_Integer._TYPE;
				case 9:
					return java_lang_Long._TYPE;
				case 14:
					lI5 = (N.I(p0._offset - 1));
					lA6 = java_lang_reflect__InternalUtils.parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_(p0, p1);
					lI7 = p0._offset;
					if (!(((lA6) is java_lang_Class))) {
						G = 15;
						continue;
					}
					return java_lang_reflect__InternalUtils.Class_forName0_Ljava_lang_String__Ljava_lang_Class_(p0._str.substring_II_Ljava_lang_String_(lI5, lI7));
				case 15:
					tA3 = (new java_lang_reflect_ArrayType());
					fA0 = tA3;
					(tA3 as java_lang_reflect_ArrayType).java_lang_reflect_ArrayType_init__Ljava_lang_reflect_Type__V((lA6 as java_lang_reflect_Type));
					return (fA0 as java_lang_reflect_Type);
				case 10:
					if (((lI2 != 84))) {
						G = 16;
						continue;
					}
					fI0 = 1;
					G = 17;
					continue;
				case 16:
					fI0 = 0;
					G = 17;
					continue;
				case 17:
					lI8 = fI0;
					lA9 = p0.readUntil_CCZ_Ljava_lang_String_(59, 60, false).replace_CC_Ljava_lang_String_(47, 46);
					if (((lI8 == 0))) {
						G = 18;
						continue;
					}
					fA0 = java_lang_reflect__InternalUtils.Class_forName0_Ljava_lang_String__Ljava_lang_Class_(Bootstrap.STRINGLIT_491);
					G = 19;
					continue;
				case 18:
					fA0 = java_lang_reflect__InternalUtils.Class_forName0_Ljava_lang_String__Ljava_lang_Class_(lA9);
					G = 19;
					continue;
				case 19:
					lA10 = fA0;
					if (((p0.peek__C() != 60))) {
						G = 20;
						continue;
					}
					lA10 = java_lang_reflect__InternalUtils.parseTypeGeneric_Ljava_lang_reflect_Type_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_((lA10 as java_lang_reflect_Type), p0, p1);
					G = 20;
					continue;
				case 20:
					p0.expect_C_V(59);
					return (lA10 as java_lang_reflect_Type);
				case 1:
					tA0 = (new java_lang_Error());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_Error).java_lang_Error_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_802).append_C_Ljava_lang_StringBuilder_(N.i2c(lI2)).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_801).append_Ljava_lang_String__Ljava_lang_StringBuilder_(p0._str).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					break;
				default:
					break;
			}
		}
		return null;
	}
	static JA_L parseTypes_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type___Ljava_lang_reflect_Type_(com_jtransc_text_MStringReader p0, java_lang_reflect_Type p1) {
		int G = 0;
		java_lang_Object lA2 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_util_ArrayList());
					fA0 = tA0;
					(tA0 as java_util_ArrayList).java_util_ArrayList_init___V();
					lA2 = fA0;
					G = 1;
					continue;
				case 1:
					if (!(p0.hasMore__Z())) {
						G = 2;
						continue;
					}
					if (((p0.peek__C() != 41))) {
						G = 3;
						continue;
					}
					G = 2;
					continue;
				case 3:
					(lA2 as java_util_ArrayList).add_Ljava_lang_Object__Z(java_lang_reflect__InternalUtils.parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_(p0, p1));
					G = 1;
					continue;
				case 2:
					return (((lA2 as java_util_ArrayList).toArray__Ljava_lang_Object___Ljava_lang_Object_((new JA_L((lA2 as java_util_ArrayList).size__I(), "[Ljava.lang.reflect.Type;") as JA_L))) as JA_L);
				default:
					break;
			}
		}
		return null;
	}
	static java_lang_Class Class_forName0_Ljava_lang_String__Ljava_lang_Class_(java_lang_String p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							G = 1;
							continue;
						case 1:
							fA0 = java_lang_Class.forName_Ljava_lang_String__Ljava_lang_Class_(p0);
							G = 2;
							continue;
						case 2: return (fA0 as java_lang_Class);
						case 3:
							fA0 = J__exception__;
							tA0 = (new java_lang_StringBuilder());
							fA0 = tA0;
							(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
							com_jtransc_io_JTranscConsole.error_Ljava_lang_Object__V((fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_804).append_Ljava_lang_String__Ljava_lang_StringBuilder_(p0).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_803).toString__Ljava_lang_String_());
							return null;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_ClassNotFoundException)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	static java_lang_reflect_Type parseTypeGeneric_Ljava_lang_reflect_Type_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_(java_lang_reflect_Type p0, com_jtransc_text_MStringReader p1, java_lang_reflect_Type p2) {
		int G = 0;
		java_lang_Object lA3 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA2 = null;
		while (true) {
			switch (G) {
				case 0:
					p1.expect_C_V(60);
					tA0 = (new java_util_ArrayList());
					fA0 = tA0;
					(tA0 as java_util_ArrayList).java_util_ArrayList_init___V();
					lA3 = fA0;
					G = 1;
					continue;
				case 1:
					if (!(p1.hasMore__Z())) {
						G = 2;
						continue;
					}
					if (((p1.peek__C() == 62))) {
						G = 2;
						continue;
					}
					(lA3 as java_util_ArrayList).add_Ljava_lang_Object__Z(java_lang_reflect__InternalUtils.parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_(p1, p2));
					G = 1;
					continue;
				case 2:
					p1.expect_C_V(62);
					tA2 = (new java_lang_reflect_ParameterizedTypeImpl());
					fA0 = tA2;
					(tA2 as java_lang_reflect_ParameterizedTypeImpl).java_lang_reflect_ParameterizedTypeImpl_init___Ljava_lang_reflect_Type_Ljava_lang_reflect_Type_Ljava_lang_reflect_Type__V((((lA3 as java_util_ArrayList).toArray__Ljava_lang_Object___Ljava_lang_Object_((new JA_L((lA3 as java_util_ArrayList).size__I(), "[Ljava.lang.reflect.Type;") as JA_L))) as JA_L), p0, p2);
					return (fA0 as java_lang_reflect_Type);
				default:
					break;
			}
		}
		return null;
	}
	java_lang_reflect__InternalUtils([int CLASS_ID = 677]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_reflect_Field extends java_lang_reflect_AccessibleObject implements java_lang_reflect_Member {

	java_lang_Class _clazz = null;
	java_lang_String _name = null;
	int _modifiers = 0;
	java_lang_String _signature = null;
	int _slot = 0;
	java_lang_String _genericSignature = null;
	 java_lang_String toString__Ljava_lang_String_() {
		int G = 0;
		int lI1 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					lI1 = this.getModifiers__I();
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					if (((lI1 != 0))) {
						G = 1;
						continue;
					}
					fA1 = Bootstrap.STRINGLIT_20;
					G = 2;
					continue;
				case 1:
					tA1 = (new java_lang_StringBuilder());
					fA1 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					fA1 = (fA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect_Modifier.toString_I_Ljava_lang_String_(lI1)).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_222).toString__Ljava_lang_String_();
					G = 2;
					continue;
				case 2: return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_((fA1 as java_lang_String)).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils.getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(this.getType__Ljava_lang_Class_())).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_222).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils.getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(this.getDeclaringClass__Ljava_lang_Class_())).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_223).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getName__Ljava_lang_String_()).toString__Ljava_lang_String_();
				default:
					break;
			}
		}
		return null;
	}
	 java_lang_Class getDeclaringClass__Ljava_lang_Class_() {
		return this._clazz;
	}
	 java_lang_String getName__Ljava_lang_String_() {
		return this._name;
	}
	 int getModifiers__I() {
		return this._modifiers;
	}
	 java_lang_Class getType__Ljava_lang_Class_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							G = 1;
							continue;
						case 1:
							fA0 = java_lang_Class.forName_Ljava_lang_String__Ljava_lang_Class_(this._signature);
							G = 2;
							continue;
						case 2: return (fA0 as java_lang_Class);
						case 3:
							fA0 = J__exception__;
							lA1 = fA0;
							(lA1 as java_lang_ClassNotFoundException).printStackTrace__V();
							return null;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_ClassNotFoundException)))) {
					G = 3;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	 int hashCode__I() {
		return (N.I(this.getDeclaringClass__Ljava_lang_Class_().getName__Ljava_lang_String_().hashCode__I() ^ this.getName__Ljava_lang_String_().hashCode__I()));
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this != p0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 void set_Ljava_lang_Object_Ljava_lang_Object__V(java_lang_Object p0, java_lang_Object p1) {
		int G = 0;
		java_lang_Object lA3 = null;
		while (true) {
			switch (G) {
				case 0:
					lA3 = this.getType__Ljava_lang_Class_();
					if (((lA3 != null))) {
						G = 1;
						continue;
					}
					G = 2;
					continue;
				case 1:
					this._setObject_Ljava_lang_Object_Ljava_lang_Object__V(p0, p1);
					G = 2;
					continue;
				case 2:
					return;
				default:
					break;
			}
		}
		return;
	}
	 void _setObject_Ljava_lang_Object_Ljava_lang_Object__V(java_lang_Object p0, java_lang_Object p1) {
		j_ProgramReflection.dynamicSet_IILjava_lang_Object_Ljava_lang_Object__V(this._clazz._id, this._slot, p0, p1);
		return;
	}
	 java_lang_Object get_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		while (true) {
			switch (G) {
				case 0:
					lA2 = this.getType__Ljava_lang_Class_();
					if (((lA2 != null))) {
						G = 1;
						continue;
					}
					return null;
				case 1:
					return this._getObject_Ljava_lang_Object__Ljava_lang_Object_(p0);
				default:
					break;
			}
		}
		return null;
	}
	 java_lang_Object _getObject_Ljava_lang_Object__Ljava_lang_Object_(java_lang_Object p0) {
		return j_ProgramReflection.dynamicGet_IILjava_lang_Object__Ljava_lang_Object_(this._clazz._id, this._slot, p0);
	}
	 java_lang_reflect_Field java_lang_reflect_Field_init__Ljava_lang_Class_Lj_MemberInfo__V(java_lang_Class p0, j_MemberInfo p1) {
		this.java_lang_reflect_AccessibleObject_init__Lj_MemberInfo__V(p1);
		this._clazz = p0;
		this._slot = p1._id;
		this._name = p1._name;
		this._signature = p1._desc;
		this._genericSignature = p1._genericDesc;
		this._modifiers = p1._modifiers;
		return this;
		return this;
	}
	java_lang_reflect_Field([int CLASS_ID = 674]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_Integer extends java_lang_Number implements java_lang_Comparable {

	int _value = 0;
	static JA_L _values = null;
	static JA_B _NTZ_TABLE = null;
	static java_lang_Class _TYPE = null;
	 java_lang_String toString__Ljava_lang_String_() {
		return java_lang_Integer.toString_I_Ljava_lang_String_(this._value);
	}
	static java_lang_String toString_I_Ljava_lang_String_(int p0) {
		return java_lang_Integer.toString_II_Ljava_lang_String_(p0, 10);
	}
	static java_lang_String toString_II_Ljava_lang_String_(int p0, int p1) {
		return N.str(p0.toRadixString(p1));
	}
	static java_lang_Integer valueOf_I_Ljava_lang_Integer_(int p0) {
		int G = 0;
		int fI1 = 0;
		int lI1 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((java_lang_Integer._values != null))) {
						G = 1;
						continue;
					}
					java_lang_Integer._values = new JA_L(256, "[Ljava.lang.Integer;");
					lI1 = -128;
					G = 2;
					continue;
				case 2:
					if (((lI1 >= 128))) {
						G = 1;
						continue;
					}
					fA0 = java_lang_Integer._values;
					fI1 = (N.I(lI1 - -128));
					tA0 = (new java_lang_Integer());
					fA2 = tA0;
					(tA0 as java_lang_Integer).java_lang_Integer_init__I_V(lI1);
					(fA0 as JA_L).data[fI1] = fA2;
					lI1 = (N.I(lI1 + 1));
					G = 2;
					continue;
				case 1:
					if (((p0 < -128))) {
						G = 3;
						continue;
					}
					if (((p0 >= 128))) {
						G = 3;
						continue;
					}
					return (((java_lang_Integer._values as JA_L)).data[(N.I(p0 - -128))] as java_lang_Integer);
				case 3:
					tA1 = (new java_lang_Integer());
					fA0 = tA1;
					(tA1 as java_lang_Integer).java_lang_Integer_init__I_V(p0);
					return (fA0 as java_lang_Integer);
				default:
					break;
			}
		}
		return null;
	}
	 java_lang_Integer java_lang_Integer_init__I_V(int p0) {
		this.java_lang_Number_init___V();
		this._value = p0;
		return this;
		return this;
	}
	static void java_lang_Integer_clinit___V() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Integer._TYPE = java_lang_Class.getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap.STRINGLIT_805);
		tA0 = new JA_B(64);
		fA0 = tA0;
		(tA0 as JA_B).data[0] = 32;
		(fA0 as JA_B).setArraySlice(1, [ 0, 1, 12, 2, 6, -1, 13, 3, -1, 7, -1, -1, -1, -1, 14, 10, 4, -1, -1, 8, -1, -1, 25, -1, -1, -1, -1, -1, 21, 27, 15, 31, 11, 5, -1, -1, -1, -1, -1, 9, -1, -1, 24, -1, -1, 20, 26, 30, -1, -1, -1, -1, 23, -1, 19, 29, -1, 22, 18, 28, 17, 16, -1 ]);
		java_lang_Integer._NTZ_TABLE = (fA0 as JA_B);
		return;
	}
	 int hashCode__I() {
		return this._value;
	}
	 double doubleValue__D() {
		return N.i2d(this._value);
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (!(((p0) is java_lang_Integer))) {
						G = 1;
						continue;
					}
					if (((((p0) as java_lang_Integer)._value != this._value))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 Int64 longValue__J() {
		return N.i2j(this._value);
	}
	 int intValue__I() {
		return this._value;
	}
	 int shortValue__S() {
		return N.i2s(this._value);
	}
	 int byteValue__B() {
		return N.i2b(this._value);
	}
	static java_lang_String toHexString_I_Ljava_lang_String_(int p0) {
		return java_lang_Integer.toUnsignedString_II_Ljava_lang_String_(p0, 16);
	}
	static java_lang_String toUnsignedString_II_Ljava_lang_String_(int p0, int p1) {
		int G = 0;
		java_lang_Object lA2 = null;
		int lI0 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lI0 = p0;
					if (((lI0 != 0))) {
						G = 1;
						continue;
					}
					return Bootstrap.STRINGLIT_748;
				case 1:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA2 = fA0;
					G = 2;
					continue;
				case 2:
					if (((lI0 == 0))) {
						G = 3;
						continue;
					}
					(lA2 as java_lang_StringBuilder).append_C_Ljava_lang_StringBuilder_(com_jtransc_internal_JTranscCType.encodeDigit_I_C(java_lang_Integer.remainderUnsigned_II_I(lI0, p1)));
					lI0 = java_lang_Integer.divideUnsigned_II_I(lI0, p1);
					G = 2;
					continue;
				case 3:
					(lA2 as java_lang_StringBuilder).reverse__Ljava_lang_StringBuilder_();
					return (lA2 as java_lang_StringBuilder).toString__Ljava_lang_String_();
				default:
					break;
			}
		}
		return null;
	}
	static int divideUnsigned_II_I(int p0, int p1) {
		int G = 0;
		int fI0 = 0;
		int fI1 = 0;
		int lI2 = 0;
		int lI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p1 >= 0))) {
						G = 1;
						continue;
					}
					if (((java_lang_Integer.compareUnsigned_II_I(p0, p1) >= 0))) {
						G = 2;
						continue;
					}
					fI0 = 0;
					G = 3;
					continue;
				case 2:
					fI0 = 1;
					G = 3;
					continue;
				case 3: return fI0;
				case 1:
					if (((p0 < 0))) {
						G = 4;
						continue;
					}
					return (N.I(p0 ~/ p1));
				case 4:
					lI2 = (N.I((N.I((N.iushr_opt(p0, 1)) ~/ p1)) << 1));
					lI3 = (N.I(p0 - (N.I(lI2 * p1))));
					fI0 = lI2;
					if (((java_lang_Integer.compareUnsigned_II_I(lI3, p1) < 0))) {
						G = 5;
						continue;
					}
					fI1 = 1;
					G = 6;
					continue;
				case 5:
					fI1 = 0;
					G = 6;
					continue;
				case 6: return (N.I(fI0 + fI1));
				default:
					break;
			}
		}
		return 0;
	}
	static int compareUnsigned_II_I(int p0, int p1) {
		return java_lang_Integer.compare_II_I((N.I(p0 ^ N.MIN_INT32)), (N.I(p1 ^ N.MIN_INT32)));
	}
	static int compare_II_I(int p0, int p1) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 >= p1))) {
						G = 1;
						continue;
					}
					fI0 = -1;
					G = 2;
					continue;
				case 1:
					if (((p0 <= p1))) {
						G = 3;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 3:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return fI0;
				default:
					break;
			}
		}
		return 0;
	}
	static int remainderUnsigned_II_I(int p0, int p1) {
		int G = 0;
		int fI0 = 0;
		int fI1 = 0;
		int lI2 = 0;
		int lI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((p1 >= 0))) {
						G = 1;
						continue;
					}
					if (((java_lang_Integer.compareUnsigned_II_I(p0, p1) >= 0))) {
						G = 2;
						continue;
					}
					fI0 = p0;
					G = 3;
					continue;
				case 2:
					fI0 = (N.I(p0 - p1));
					G = 3;
					continue;
				case 3: return fI0;
				case 1:
					if (((p0 < 0))) {
						G = 4;
						continue;
					}
					return (N.I(p0.remainder(p1)));
				case 4:
					lI2 = (N.I((N.I((N.iushr_opt(p0, 1)) ~/ p1)) << 1));
					lI3 = (N.I(p0 - (N.I(lI2 * p1))));
					fI0 = lI3;
					if (((java_lang_Integer.compareUnsigned_II_I(lI3, p1) < 0))) {
						G = 5;
						continue;
					}
					fI1 = p1;
					G = 6;
					continue;
				case 5:
					fI1 = 0;
					G = 6;
					continue;
				case 6: return (N.I(fI0 - fI1));
				default:
					break;
			}
		}
		return 0;
	}
	 double floatValue__F() {
		return N.i2f(this._value);
	}
	static int min_II_I(int p0, int p1) {
		return java_lang_Math.min_II_I(p0, p1);
	}
	static int max_II_I(int p0, int p1) {
		return java_lang_Math.max_II_I(p0, p1);
	}
	static int reverseBytes_I_I(int p0) {
		return (N.I((N.I((N.I((N.iushr_opt(p0, 24)) | (N.I((N.I(p0 >> 8)) & 65280)))) | (N.I((N.I(p0 << 8)) & 16711680)))) | (N.I(p0 << 24))));
	}
	java_lang_Integer([int CLASS_ID = 672]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_Integer._values = null;
		java_lang_Integer._NTZ_TABLE = null;
		java_lang_Integer._TYPE = null;
		java_lang_Integer.java_lang_Integer_clinit___V();
	}
}
class java_lang_reflect_Modifier extends java_lang_Object  {

	 java_lang_reflect_Modifier java_lang_reflect_Modifier_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static bool isInterface_I_Z(int p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if ((((N.I(p0 & 512)) == 0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	static java_lang_String toString_I_Ljava_lang_String_(int p0) {
		int G = 0;
		java_lang_Object lA1 = null;
		int fI0 = 0;
		int lI2 = 0;
		java_lang_Object fA0 = null;
		int tI13 = 0;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					lA1 = fA0;
					if ((((N.I(p0 & 1)) == 0))) {
						G = 1;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_806);
					G = 1;
					continue;
				case 1:
					if ((((N.I(p0 & 4)) == 0))) {
						G = 2;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_807);
					G = 2;
					continue;
				case 2:
					if ((((N.I(p0 & 2)) == 0))) {
						G = 3;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_808);
					G = 3;
					continue;
				case 3:
					if ((((N.I(p0 & 1024)) == 0))) {
						G = 4;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_809);
					G = 4;
					continue;
				case 4:
					if ((((N.I(p0 & 8)) == 0))) {
						G = 5;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_810);
					G = 5;
					continue;
				case 5:
					if ((((N.I(p0 & 16)) == 0))) {
						G = 6;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_811);
					G = 6;
					continue;
				case 6:
					if ((((N.I(p0 & 128)) == 0))) {
						G = 7;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_812);
					G = 7;
					continue;
				case 7:
					if ((((N.I(p0 & 64)) == 0))) {
						G = 8;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_813);
					G = 8;
					continue;
				case 8:
					if ((((N.I(p0 & 32)) == 0))) {
						G = 9;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_814);
					G = 9;
					continue;
				case 9:
					if ((((N.I(p0 & 256)) == 0))) {
						G = 10;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_815);
					G = 10;
					continue;
				case 10:
					if ((((N.I(p0 & 2048)) == 0))) {
						G = 11;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_816);
					G = 11;
					continue;
				case 11:
					if ((((N.I(p0 & 512)) == 0))) {
						G = 12;
						continue;
					}
					(lA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_817);
					G = 12;
					continue;
				case 12:
					tI13 = (lA1 as java_lang_StringBuilder).length__I();
					fI0 = tI13;
					lI2 = tI13;
					if (((fI0 <= 0))) {
						G = 13;
						continue;
					}
					return (lA1 as java_lang_StringBuilder).toString__Ljava_lang_String_().substring_II_Ljava_lang_String_(0, (N.I(lI2 - 1)));
				case 13:
					return Bootstrap.STRINGLIT_20;
				default:
					break;
			}
		}
		return null;
	}
	java_lang_reflect_Modifier([int CLASS_ID = 671]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_lang_AnnotatedElement   {

}
class java_lang_AnnotatedElement_IFields {

	static void SI() { }
}
class java_lang_Class extends java_lang_Object implements java_io_Serializable, java_lang_reflect_Type, java_lang_reflect_GenericDeclaration, java_lang_AnnotatedElement {

	java_lang_String _name = null;
	bool _primitive = false;
	int _modifiers = 0;
	static com_jtransc_ds_FastStringMap __classCache = null;
	JA_L _enumConstants = null;
	JA_L __allFields = null;
	JA_L __accessibleFields = null;
	int _id = 0;
	JA_I _related = null;
	j_ClassInfo _info = null;
	 java_lang_String toString__Ljava_lang_String_() {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_String fA1 = null;
		java_lang_Object tA0 = null;
		while (true) {
			switch (G) {
				case 0:
					tA0 = (new java_lang_StringBuilder());
					fA0 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					if (!(this.isInterface__Z())) {
						G = 1;
						continue;
					}
					fA1 = Bootstrap.STRINGLIT_817;
					G = 2;
					continue;
				case 1:
					if (!(this.isPrimitive__Z())) {
						G = 3;
						continue;
					}
					fA1 = Bootstrap.STRINGLIT_20;
					G = 2;
					continue;
				case 3:
					fA1 = Bootstrap.STRINGLIT_818;
					G = 2;
					continue;
				case 2: return (fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(fA1).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this._name).toString__Ljava_lang_String_();
				default:
					break;
			}
		}
		return null;
	}
	 bool isPrimitive__Z() {
		return this._primitive;
	}
	 bool isInterface__Z() {
		return java_lang_reflect_Modifier.isInterface_I_Z(this.getModifiers__I());
	}
	 int getModifiers__I() {
		return (N.I(this._modifiers & -33));
	}
	static java_lang_Class getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(java_lang_String p0) {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_Class());
		fA0 = tA0;
		(tA0 as java_lang_Class).java_lang_Class_init__Ljava_lang_String_Z_V(p0, true);
		return (fA0 as java_lang_Class);
	}
	 java_lang_Class java_lang_Class_init__Ljava_lang_String_Z_V(java_lang_String p0, bool p1) {
		this.java_lang_Object_init___V();
		this._primitive = false;
		this._enumConstants = null;
		this.__allFields = null;
		this.__accessibleFields = null;
		this._name = p0;
		this._primitive = p1;
		this._id = -1;
		return this;
		return this;
	}
	 java_lang_String getName__Ljava_lang_String_() {
		return this._name;
	}
	static java_lang_Class forName_Ljava_lang_String__Ljava_lang_Class_(java_lang_String p0) {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA2 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 != null))) {
						G = 1;
						continue;
					}
					return null;
				case 1:
					if (((p0.length__I() != 1))) {
						G = 2;
						continue;
					}
					switch (p0.charAt_I_C(0)) {
						case 66:
							G = 3;
							continue;
						case 67:
							G = 4;
							continue;
						case 68:
							G = 5;
							continue;
						case 69:
							G = 2;
							continue;
						case 70:
							G = 6;
							continue;
						case 71:
							G = 2;
							continue;
						case 72:
							G = 2;
							continue;
						case 73:
							G = 7;
							continue;
						case 74:
							G = 8;
							continue;
						case 75:
							G = 2;
							continue;
						case 76:
							G = 2;
							continue;
						case 77:
							G = 2;
							continue;
						case 78:
							G = 2;
							continue;
						case 79:
							G = 2;
							continue;
						case 80:
							G = 2;
							continue;
						case 81:
							G = 2;
							continue;
						case 82:
							G = 2;
							continue;
						case 83:
							G = 9;
							continue;
						case 84:
							G = 2;
							continue;
						case 85:
							G = 2;
							continue;
						case 86:
							G = 10;
							continue;
						case 87:
							G = 2;
							continue;
						case 88:
							G = 2;
							continue;
						case 89:
							G = 2;
							continue;
						case 90:
							G = 11;
							continue;
						default:
							G = 2;
							continue;
					}
					G = 10;
					continue;
				case 10:
					return java_lang_Void._TYPE;
				case 11:
					return java_lang_Boolean._TYPE;
				case 3:
					return java_lang_Byte._TYPE;
				case 4:
					return java_lang_Character._TYPE;
				case 9:
					return java_lang_Short._TYPE;
				case 5:
					return java_lang_Double._TYPE;
				case 6:
					return java_lang_Float._TYPE;
				case 7:
					return java_lang_Integer._TYPE;
				case 8:
					return java_lang_Long._TYPE;
				case 2:
					if (!(p0.startsWith_Ljava_lang_String__Z(Bootstrap.STRINGLIT_819))) {
						G = 12;
						continue;
					}
					if (!(p0.endsWith_Ljava_lang_String__Z(Bootstrap.STRINGLIT_820))) {
						G = 12;
						continue;
					}
					return java_lang_Class.forName_Ljava_lang_String__Ljava_lang_Class_(p0.substring_II_Ljava_lang_String_(1, (N.I(p0.length__I() - 1))).replace_CC_Ljava_lang_String_(47, 46));
				case 12:
					if (((java_lang_Class.__classCache != null))) {
						G = 13;
						continue;
					}
					tA0 = (new com_jtransc_ds_FastStringMap());
					fA0 = tA0;
					(tA0 as com_jtransc_ds_FastStringMap).com_jtransc_ds_FastStringMap_init___V();
					java_lang_Class.__classCache = (fA0 as com_jtransc_ds_FastStringMap);
					G = 13;
					continue;
				case 13:
					if (java_lang_Class.__classCache.has_Ljava_lang_String__Z(p0)) {
						G = 14;
						continue;
					}
					fA0 = java_lang_Class.__classCache;
					tA1 = (new java_lang_Class());
					fA2 = tA1;
					(tA1 as java_lang_Class).java_lang_Class_init__Ljava_lang_String__V(p0);
					(fA0 as com_jtransc_ds_FastStringMap).set_Ljava_lang_String_Ljava_lang_Object__V(p0, fA2);
					G = 14;
					continue;
				case 14:
					lA1 = ((java_lang_Class.__classCache.get_Ljava_lang_String__Ljava_lang_Object_(p0)) as java_lang_Class);
					if (((lA1 != null))) {
						G = 15;
						continue;
					}
					tA2 = (new java_lang_StringBuilder());
					fA0 = tA2;
					(tA2 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					com_jtransc_io_JTranscConsole.error_Ljava_lang_Object__V((fA0 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_821).append_Ljava_lang_String__Ljava_lang_StringBuilder_(p0).toString__Ljava_lang_String_());
					G = 15;
					continue;
				case 15:
					return (lA1 as java_lang_Class);
				default:
					break;
			}
		}
		return null;
	}
	 int hashCode__I() {
		return this._name.hashCode__I();
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this != p0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 java_lang_Class java_lang_Class_init__Ljava_lang_String__V(java_lang_String p0) {
		int G = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					this.java_lang_Object_init___V();
					this._primitive = false;
					this._enumConstants = null;
					this.__allFields = null;
					this.__accessibleFields = null;
					this._name = p0;
					this._primitive = false;
					if (this._check__Z()) {
						G = 1;
						continue;
					}
					tA0 = (new java_lang_ClassNotFoundException());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_ClassNotFoundException).java_lang_ClassNotFoundException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_822).append_Ljava_lang_String__Ljava_lang_StringBuilder_(p0).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_803).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					G = 1;
					continue;
				case 1:
					return this;
				default:
					break;
			}
		}
		return this;
		return this;
	}
	 bool _check__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					this._info = java_lang_jtransc_JTranscCoreReflection.getClassInfoWithName_Ljava_lang_String__Lj_ClassInfo_(this._name);
					if (((this._info == null))) {
						G = 1;
						continue;
					}
					this._id = this._info._id;
					this._related = this._info._related;
					this._modifiers = java_lang_jtransc_JTranscCoreReflection.getModifiersWithId_I_I(this._id);
					G = 2;
					continue;
				case 1:
					this._id = -1;
					this._related = new JA_I(0);
					this._modifiers = 0;
					G = 2;
					continue;
				case 2:
					if (this.isArray__Z()) {
						G = 3;
						continue;
					}
					if (((this._id < 0))) {
						G = 4;
						continue;
					}
					G = 3;
					continue;
				case 3:
					fI0 = 1;
					G = 5;
					continue;
				case 4:
					fI0 = 0;
					G = 5;
					continue;
				case 5: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	 bool isArray__Z() {
		return this._name.startsWith_Ljava_lang_String__Z(Bootstrap.STRINGLIT_484);
	}
	 JA_L getDeclaredFields___Ljava_lang_reflect_Field_() {
		return java_lang_jtransc_JTranscCoreReflection.getDeclaredFields_Ljava_lang_Class___Ljava_lang_reflect_Field_(this);
	}
	 java_lang_Class getComponentType__Ljava_lang_Class_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							if (!(this.isArray__Z())) {
								G = 1;
								continue;
							}
							G = 2;
							continue;
						case 2:
							lA1 = java_lang_Class.forName_Ljava_lang_String__Ljava_lang_Class_(this.getName__Ljava_lang_String_().substring_I_Ljava_lang_String_(1));
							fA0 = lA1;
							G = 3;
							continue;
						case 3: return (fA0 as java_lang_Class);
						case 4:
							fA0 = J__exception__;
							lA1 = fA0;
							tA0 = (new java_lang_RuntimeException());
							fA0 = tA0;
							(tA0 as java_lang_RuntimeException).java_lang_RuntimeException_init__Ljava_lang_Throwable__V((lA1 as java_lang_Throwable));
							throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
							G = 1;
							continue;
						case 1:
							return null;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 2)) && ((G < 3)))) && ((J__exception__) is java_lang_ClassNotFoundException)))) {
					G = 4;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	 java_lang_Object newInstance__Ljava_lang_Object_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							G = 1;
							continue;
						case 1:
							lA1 = this.getDeclaredConstructor__Ljava_lang_Class__Ljava_lang_reflect_Constructor_(new JA_L(0, "[Ljava.lang.Class;"));
							fA0 = (lA1 as java_lang_reflect_Constructor).newInstance__Ljava_lang_Object__Ljava_lang_Object_(new JA_L(0, "[Ljava.lang.Object;"));
							G = 2;
							continue;
						case 2: return fA0;
						case 3:
							fA0 = J__exception__;
							lA1 = fA0;
							tA0 = (new java_lang_InstantiationException());
							fA0 = tA0;
							(tA0 as java_lang_InstantiationException).java_lang_InstantiationException_init__Ljava_lang_String__V((lA1 as java_lang_NoSuchMethodException).getMessage__Ljava_lang_String_());
							throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
							G = 4;
							continue;
						case 4:
							fA0 = J__exception__;
							lA1 = fA0;
							tA1 = (new java_lang_InstantiationException());
							fA0 = tA1;
							(tA1 as java_lang_InstantiationException).java_lang_InstantiationException_init__Ljava_lang_String__V((lA1 as java_lang_reflect_InvocationTargetException).getMessage__Ljava_lang_String_());
							throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
							break;
						default:
							break;
					}
				}
				return null;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_NoSuchMethodException)))) {
					G = 3;
					continue;
				}
				if (((((((G >= 1)) && ((G < 2)))) && ((J__exception__) is java_lang_reflect_InvocationTargetException)))) {
					G = 4;
					continue;
				}
				rethrow;
			}
		}
		return null;
	}
	 java_lang_reflect_Constructor getDeclaredConstructor__Ljava_lang_Class__Ljava_lang_reflect_Constructor_(JA_L p0) {
		int G = 0;
		java_lang_Object lA2 = null;
		java_lang_Object lA3 = null;
		java_lang_Object lA6 = null;
		int lI4 = 0;
		int lI5 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		java_lang_Object fA2 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA1 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 == null))) {
						G = 1;
						continue;
					}
					fA0 = p0;
					G = 2;
					continue;
				case 1:
					fA0 = new JA_L(0, "[Ljava.lang.Class;");
					G = 2;
					continue;
				case 2:
					lA2 = fA0;
					lA3 = this.getDeclaredConstructors___Ljava_lang_reflect_Constructor_();
					lI4 = (lA3 as JA_0).length;
					lI5 = 0;
					G = 3;
					continue;
				case 3:
					if (((lI5 >= lI4))) {
						G = 4;
						continue;
					}
					lA6 = ((lA3 as JA_L)).data[lI5];
					if (!(java_util_Arrays.equals__Ljava_lang_Object__Ljava_lang_Object__Z(((lA6 as java_lang_reflect_Constructor).getParameterTypes___Ljava_lang_Class_() as JA_L), (lA2 as JA_L)))) {
						G = 5;
						continue;
					}
					return (lA6 as java_lang_reflect_Constructor);
				case 5:
					lI5 = (N.I(lI5 + 1));
					G = 3;
					continue;
				case 4:
					tA0 = (new java_lang_NoSuchMethodException());
					fA0 = tA0;
					fA1 = tA0;
					tA1 = (new java_lang_StringBuilder());
					fA2 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA1 as java_lang_NoSuchMethodException).java_lang_NoSuchMethodException_init__Ljava_lang_String__V((fA2 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_824).append_Ljava_lang_String__Ljava_lang_StringBuilder_(this.getName__Ljava_lang_String_()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_823).append_Ljava_lang_Object__Ljava_lang_StringBuilder_(java_util_Arrays.asList__Ljava_lang_Object__Ljava_util_List_((p0 as JA_L))).toString__Ljava_lang_String_());
					throw (fA0).prepareThrow__Ljava_lang_Throwable_().dartError;
					break;
				default:
					break;
			}
		}
		return null;
	}
	 JA_L getDeclaredConstructors___Ljava_lang_reflect_Constructor_() {
		int G = 0;
		java_lang_Object lA1 = null;
		java_lang_Object fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = this._getDeclaredConstructors___Ljava_lang_reflect_Constructor_();
					if (((lA1 == null))) {
						G = 1;
						continue;
					}
					fA0 = lA1;
					G = 2;
					continue;
				case 1:
					fA0 = new JA_L(0, "[Ljava.lang.reflect.Constructor;");
					G = 2;
					continue;
				case 2: return (fA0 as JA_L);
				default:
					break;
			}
		}
		return null;
	}
	 JA_L _getDeclaredConstructors___Ljava_lang_reflect_Constructor_() {
		return java_lang_jtransc_JTranscCoreReflection.getDeclaredConstructors_Ljava_lang_Class___Ljava_lang_reflect_Constructor_(this);
	}
	 java_lang_Class getDeclaringClass__Ljava_lang_Class_() {
		throw new Exception("Missing body java.lang.Class.getDeclaringClass\u0028\u0029Ljava/lang/Class;");
	}
	java_lang_Class([int CLASS_ID = 666]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_StringBuilder extends java_lang_Object implements java_io_Serializable, java_lang_Appendable, java_lang_CharSequence {
	StringBuffer _buffer = new StringBuffer();
	 java_lang_String toString__Ljava_lang_String_() {
		return N.str(this._buffer.toString());
	}
	 java_lang_StringBuilder java_lang_StringBuilder_init___V() {
		this.java_lang_StringBuilder_init__I_V(16);
		return this;
		return this;
	}
	 java_lang_StringBuilder java_lang_StringBuilder_init__I_V(int p0) {
		this._buffer = new StringBuffer(); return this;
	}
	 java_lang_StringBuilder append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_String p0) {
		this._buffer.write((p0 != null) ? N.istr(p0) : 'null'); return this;
	}
	 int length__I() {
		return this._buffer.length;
	}
	 int charAt_I_C(int p0) {
		return this._buffer.toString().codeUnitAt(p0) & 0xFFFF;
	}
	 java_lang_StringBuilder append_Ljava_lang_Object__Ljava_lang_StringBuilder_(java_lang_Object p0) {
		return this.append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_String.valueOf_Ljava_lang_Object__Ljava_lang_String_(p0));
	}
	 java_lang_StringBuilder append_C_Ljava_lang_StringBuilder_(int p0) {
		this._buffer.write(N.ichar(p0)); return this;
	}
	 java_lang_StringBuilder reverse__Ljava_lang_StringBuilder_() {
		var str = this._buffer.toString(); int len = str.length; this._buffer = new StringBuffer(); for (int n = 0; n < len; n++) this._buffer.write(str[len - n - 1]); return this;
	}
	 java_lang_StringBuilder append_I_Ljava_lang_StringBuilder_(int p0) {
		this._buffer.write(p0); return this;
	}
	 java_lang_String substring_II_Ljava_lang_String_(int p0, int p1) {
		return this.toString__Ljava_lang_String_().substring_II_Ljava_lang_String_(p0, p1);
	}
	 int indexOf_Ljava_lang_String__I(java_lang_String p0) {
		return this._buffer.toString().indexOf(N.istr(p0));
	}
	 java_lang_String substring_I_Ljava_lang_String_(int p0) {
		return this.toString__Ljava_lang_String_().substring_I_Ljava_lang_String_(p0);
	}
	 void setLength_I_V(int p0) {
		this.delete_II_Ljava_lang_StringBuilder_(p0, this.length__I());
		return;
	}
	 java_lang_StringBuilder delete_II_Ljava_lang_StringBuilder_(int p0, int p1) {
		var str = this._buffer.toString(); this._buffer = new StringBuffer()..write(str.substring(0, p0))..write(str.substring(p1)); return this;
	}
	 java_lang_StringBuilder append_F_Ljava_lang_StringBuilder_(double p0) {
		return this.append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_String.valueOf_F_Ljava_lang_String_(p0));
	}
	 void ensureCapacity_I_V(int p0) {

	}
	 java_lang_StringBuilder append_J_Ljava_lang_StringBuilder_(Int64 p0) {
		return this.append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_String.valueOf_J_Ljava_lang_String_(p0));
	}
	 java_lang_StringBuilder append_D_Ljava_lang_StringBuilder_(double p0) {
		return this.append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_String.valueOf_D_Ljava_lang_String_(p0));
	}
	java_lang_StringBuilder([int CLASS_ID = 664]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_String_1 extends java_lang_Object  {

	java_lang_String_1([int CLASS_ID = 663]) : super(CLASS_ID) { }
	static void SI() { }
}
abstract class java_util_Comparator   {

	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0);
}
class java_util_Comparator_IFields {

	static void SI() { }
}
class java_lang_String_CaseInsensitiveComparator extends java_lang_Object implements java_util_Comparator, java_io_Serializable {

	 java_lang_String_CaseInsensitiveComparator java_lang_String_CaseInsensitiveComparator_init__Ljava_lang_String_1__V(java_lang_String_1 p0) {
		this.java_lang_String_CaseInsensitiveComparator_init___V();
		return this;
		return this;
	}
	 java_lang_String_CaseInsensitiveComparator java_lang_String_CaseInsensitiveComparator_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	java_lang_String_CaseInsensitiveComparator([int CLASS_ID = 661]) : super(CLASS_ID) { }
	static void SI() { }
}
class java_lang_String extends java_lang_Object implements java_io_Serializable, java_lang_Comparable, java_lang_CharSequence {
	String _str = null;
JA_C _arr = null;
	static java_util_Comparator _CASE_INSENSITIVE_ORDER = null;
	int _hash = 0;
	 java_lang_String toString__Ljava_lang_String_() {
		return this;
	}
	static void java_lang_String_clinit___V() {
		java_lang_Object fA0 = null;
		java_lang_Object tA0 = null;
		tA0 = (new java_lang_String_CaseInsensitiveComparator());
		fA0 = tA0;
		(tA0 as java_lang_String_CaseInsensitiveComparator).java_lang_String_CaseInsensitiveComparator_init__Ljava_lang_String_1__V(null);
		java_lang_String._CASE_INSENSITIVE_ORDER = (fA0 as java_util_Comparator);
		return;
	}
	 java_lang_String substring_II_Ljava_lang_String_(int p0, int p1) {
		return N.str(this._str.substring(p0, p1));
	}
	 int length__I() {
		return this._str.length;
	}
	 java_lang_String java_lang_String_init___C_V(JA_C p0) {
		this.java_lang_Object_init___V();
		this._hash = 0;
		this.setChars__C_V(java_util_Arrays.copyOf__CI__C(p0, (p0 as JA_0).length));
		return this;
		return this;
	}
	 void setChars__C_V(JA_C p0) {
		this._str = N.charArrayToString(p0);
	}
	 int charAt_I_C(int p0) {
		return this._str.codeUnitAt(p0) & 0xFFFF;
	}
	 java_lang_String replace_Ljava_lang_CharSequence_Ljava_lang_CharSequence__Ljava_lang_String_(java_lang_CharSequence p0, java_lang_CharSequence p1) {
		return this._replace_Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_(p0.toString__Ljava_lang_String_(), p1.toString__Ljava_lang_String_());
	}
	 java_lang_String _replace_Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_(java_lang_String p0, java_lang_String p1) {
		return N.str(N.istr(this).split(N.istr(p0)).join(N.istr(p1)));
	}
	 int indexOf_Ljava_lang_String__I(java_lang_String p0) {
		return this._str.indexOf(N.istr(p0));
	}
	 int indexOf_I_I(int p0) {
		return this._str.indexOf(N.ichar(p0));
	}
	static java_lang_String valueOf_Ljava_lang_Object__Ljava_lang_String_(java_lang_Object p0) {
		int G = 0;
		java_lang_String fA0 = null;
		while (true) {
			switch (G) {
				case 0:
					if (((p0 == null))) {
						G = 1;
						continue;
					}
					fA0 = p0.toString__Ljava_lang_String_();
					G = 2;
					continue;
				case 1:
					fA0 = Bootstrap.STRINGLIT_825;
					G = 2;
					continue;
				case 2: return fA0;
				default:
					break;
			}
		}
		return null;
	}
	 int hashCode__I() {
		int G = 0;
		int lI1 = 0;
		int lI2 = 0;
		int lI3 = 0;
		while (true) {
			switch (G) {
				case 0:
					lI1 = this._hash;
					lI2 = this.length__I();
					if (((lI1 != 0))) {
						G = 1;
						continue;
					}
					if (((lI2 <= 0))) {
						G = 1;
						continue;
					}
					lI3 = 0;
					G = 2;
					continue;
				case 2:
					if (((lI3 >= lI2))) {
						G = 3;
						continue;
					}
					lI1 = (N.I((N.I(31 * lI1)) + this.charAt_I_C(lI3)));
					lI3 = (N.I(lI3 + 1));
					G = 2;
					continue;
				case 3:
					this._hash = lI1;
					G = 1;
					continue;
				case 1:
					return lI1;
				default:
					break;
			}
		}
		return 0;
	}
	 bool equals_Ljava_lang_Object__Z(java_lang_Object p0) {
		return (p0 is java_lang_String) && N.istr(this) == N.istr(p0);
	}
	 bool startsWith_Ljava_lang_String__Z(java_lang_String p0) {
		return this._str.substring(0, Math.min(this._str.length, p0._str.length)) == p0._str;
	}
	 java_lang_String replace_CC_Ljava_lang_String_(int p0, int p1) {
		return N.str(N.istr(this).split(N.ichar(p0)).join(N.ichar(p1)));
	}
	 bool endsWith_Ljava_lang_String__Z(java_lang_String p0) {
		return this._str.substring(this._str.length-p0._str.length) == p0._str;
	}
	 java_lang_String substring_I_Ljava_lang_String_(int p0) {
		return N.str(this._str.substring(p0));
	}
	 bool isEmpty__Z() {
		int G = 0;
		int fI0 = 0;
		while (true) {
			switch (G) {
				case 0:
					if (((this.length__I() != 0))) {
						G = 1;
						continue;
					}
					fI0 = 1;
					G = 2;
					continue;
				case 1:
					fI0 = 0;
					G = 2;
					continue;
				case 2: return ((fI0)!=0);
				default:
					break;
			}
		}
		return false;
	}
	static java_lang_String valueOf_D_Ljava_lang_String_(double p0) {
		return java_lang_Double.toString_D_Ljava_lang_String_(p0);
	}
	 JA_B getBytes_Ljava_lang_String___B(java_lang_String p0) {
		return com_jtransc_charset_JTranscCharset.forName_Ljava_lang_String__Lcom_jtransc_charset_JTranscCharset_(p0).encode_Ljava_lang_String___B(this);
	}
	 java_lang_String java_lang_String_init___BII_V(JA_B p0, int p1, int p2) {
		this.java_lang_String_init___BIILjava_lang_String_Z_V(p0, p1, p2, Bootstrap.STRINGLIT_51, false);
		return this;
		return this;
	}
	 java_lang_String java_lang_String_init___BIILjava_lang_String_Z_V(JA_B p0, int p1, int p2, java_lang_String p3, bool p4) {
		this.java_lang_Object_init___V();
		this._hash = 0;
		this.setChars__C_V(com_jtransc_charset_JTranscCharset.forName_Ljava_lang_String__Lcom_jtransc_charset_JTranscCharset_(p3).decodeChars__BII__C(p0, p1, p2));
		return this;
		return this;
	}
	 JA_C toCharArray___C() {
		if (this._arr == null) this._arr = N.stringToCharArray(this._str); return this._arr;
	}
	 java_lang_String trim__Ljava_lang_String_() {
		return N.str(this._str.trim());
	}
	 java_lang_String toUpperCase__Ljava_lang_String_() {
		return N.str(this._str.toUpperCase());
	}
	static java_lang_String valueOf_F_Ljava_lang_String_(double p0) {
		return java_lang_Float.toString_F_Ljava_lang_String_(p0);
	}
	static java_lang_String valueOf_J_Ljava_lang_String_(Int64 p0) {
		return java_lang_Long.toString_J_Ljava_lang_String_(p0);
	}
	 java_lang_String toLowerCase__Ljava_lang_String_() {
		return N.str(this._str.toLowerCase());
	}
	java_lang_String([int CLASS_ID = 657]) : super(CLASS_ID) { }
	static void SI() {
		java_lang_String._CASE_INSENSITIVE_ORDER = null;
		java_lang_String.java_lang_String_clinit___V();
	}
}
class Benchmark extends java_lang_Object  {

	static double _totalTime = 0.0;
	 Benchmark Benchmark_init___V() {
		this.java_lang_Object_init___V();
		return this;
		return this;
	}
	static void Benchmark_clinit___V() {
		Benchmark._totalTime = 0.0;
		return;
	}
	static void main__Ljava_lang_String__V(JA_L p0) {
		java_lang_Object fA1 = null;
		java_lang_Object tA39 = null;
		java_lang_Object tA31 = null;
		java_lang_Object tA35 = null;
		java_lang_Object tA3 = null;
		java_lang_Object tA7 = null;
		java_lang_Object lA14 = null;
		java_lang_Object tA296 = null;
		JA_B lA4 = null;
		java_lang_Object tA22 = null;
		java_lang_Object tA26 = null;
		JA_F lA8 = null;
		java_lang_Object tA10 = null;
		java_lang_Object tA14 = null;
		java_lang_Object tA18 = null;
		java_lang_Runtime lA1 = null;
		int G = 0;
		int lI13 = 0;
		java_lang_Object fA0 = null;
		JA_S lA5 = null;
		java_lang_Object tA32 = null;
		java_lang_Object tA36 = null;
		java_lang_Object tA2 = null;
		java_lang_Object tA6 = null;
		java_lang_Object lA11 = null;
		java_lang_Object tA295 = null;
		java_lang_Object tA299 = null;
		java_lang_Object tA23 = null;
		java_lang_Object tA27 = null;
		java_lang_Object tA40 = null;
		java_lang_Object tA11 = null;
		java_lang_Object tA15 = null;
		JA_D lA9 = null;
		java_lang_Object tA19 = null;
		JA_B lA12 = null;
		java_lang_Object tA33 = null;
		java_lang_Object tA37 = null;
		java_lang_Object tA1 = null;
		java_lang_Object tA5 = null;
		java_lang_Object tA9 = null;
		java_lang_Object tA294 = null;
		java_lang_Object tA298 = null;
		java_lang_Object tA301 = null;
		java_lang_Object tA20 = null;
		java_lang_Object tA24 = null;
		java_lang_Object tA28 = null;
		java_lang_Object tA41 = null;
		JA_C lA6 = null;
		java_lang_Object tA12 = null;
		java_lang_Object tA16 = null;
		JA_I lA2 = null;
		int lI15 = 0;
		java_lang_Object tA38 = null;
		java_lang_Object tA30 = null;
		java_lang_Object tA34 = null;
		java_lang_Object tA0 = null;
		java_lang_Object tA4 = null;
		java_lang_Object tA8 = null;
		java_lang_Object lA13 = null;
		java_lang_Object tA297 = null;
		java_lang_Object tA300 = null;
		JA_L lA10 = null;
		java_lang_Object tA21 = null;
		java_lang_Object tA25 = null;
		java_lang_Object tA29 = null;
		java_lang_Object tA13 = null;
		java_lang_Object tA17 = null;
		JA_I lA3 = null;
		JA_I lA7 = null;
		while (true) {
			switch (G) {
				case 0:
					lA1 = java_lang_Runtime.getRuntime__Ljava_lang_Runtime_();
					fA0 = java_lang_System.__out;
					tA0 = (new java_lang_StringBuilder());
					fA1 = tA0;
					(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA0 as java_io_PrintStream).println_Ljava_lang_String__V((fA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_827).append_Ljava_lang_String__Ljava_lang_StringBuilder_(com_jtransc_JTranscVersion.getVersion__Ljava_lang_String_()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_826).append_Ljava_lang_String__Ljava_lang_StringBuilder_(com_jtransc_JTranscSystem.getRuntimeKind__Ljava_lang_String_()).toString__Ljava_lang_String_());
					fA0 = java_lang_System.__out;
					tA1 = (new java_lang_StringBuilder());
					fA1 = tA1;
					(tA1 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA0 as java_io_PrintStream).println_Ljava_lang_String__V((fA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_828).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_System.getProperty_Ljava_lang_String__Ljava_lang_String_(Bootstrap.STRINGLIT_759)).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_826).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_System.getProperty_Ljava_lang_String__Ljava_lang_String_(Bootstrap.STRINGLIT_761)).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_826).append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_System.getProperty_Ljava_lang_String__Ljava_lang_String_(Bootstrap.STRINGLIT_763)).toString__Ljava_lang_String_());
					fA0 = java_lang_System.__out;
					tA2 = (new java_lang_StringBuilder());
					fA1 = tA2;
					(tA2 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA0 as java_io_PrintStream).println_Ljava_lang_String__V((fA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_831).append_J_Ljava_lang_StringBuilder_(lA1.freeMemory__J()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_830).append_J_Ljava_lang_StringBuilder_(lA1.maxMemory__J()).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_829).append_J_Ljava_lang_StringBuilder_(lA1.totalMemory__J()).toString__Ljava_lang_String_());
					java_lang_System.__out.println_Ljava_lang_String__V(Bootstrap.STRINGLIT_832);
					fA0 = Bootstrap.STRINGLIT_833;
					tA3 = (new Benchmark_1());
					fA1 = tA3;
					(tA3 as Benchmark_1).Benchmark_1_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_834;
					tA4 = (new Benchmark_2());
					fA1 = tA4;
					(tA4 as Benchmark_2).Benchmark_2_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_835;
					tA5 = (new Benchmark_3());
					fA1 = tA5;
					(tA5 as Benchmark_3).Benchmark_3_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_836;
					tA6 = (new Benchmark_4());
					fA1 = tA6;
					(tA6 as Benchmark_4).Benchmark_4_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_837;
					tA7 = (new Benchmark_5());
					fA1 = tA7;
					(tA7 as Benchmark_5).Benchmark_5_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_838;
					tA8 = (new Benchmark_6());
					fA1 = tA8;
					(tA8 as Benchmark_6).Benchmark_6_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_839;
					tA9 = (new Benchmark_7());
					fA1 = tA9;
					(tA9 as Benchmark_7).Benchmark_7_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_840;
					tA10 = (new Benchmark_8());
					fA1 = tA10;
					(tA10 as Benchmark_8).Benchmark_8_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_841;
					tA11 = (new Benchmark_9());
					fA1 = tA11;
					(tA11 as Benchmark_9).Benchmark_9_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_842;
					tA12 = (new Benchmark_10());
					fA1 = tA12;
					(tA12 as Benchmark_10).Benchmark_10_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_843;
					tA13 = (new Benchmark_11());
					fA1 = tA13;
					(tA13 as Benchmark_11).Benchmark_11_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_844;
					tA14 = (new Benchmark_12());
					fA1 = tA14;
					(tA14 as Benchmark_12).Benchmark_12_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_845;
					tA15 = (new Benchmark_13());
					fA1 = tA15;
					(tA15 as Benchmark_13).Benchmark_13_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_846;
					tA16 = (new Benchmark_14());
					fA1 = tA16;
					(tA16 as Benchmark_14).Benchmark_14_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					lA2 = new JA_I(16384);
					lA3 = new JA_I(16384);
					fA0 = Bootstrap.STRINGLIT_847;
					tA17 = (new Benchmark_15());
					fA1 = tA17;
					(tA17 as Benchmark_15).Benchmark_15_init___I_I_V(lA2, lA3);
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					lA4 = new JA_B(1000000);
					lA5 = new JA_S(1000000);
					lA6 = new JA_C(1000000);
					lA7 = new JA_I(1000000);
					lA8 = new JA_F(1000000);
					lA9 = new JA_D(1000000);
					fA0 = Bootstrap.STRINGLIT_848;
					tA18 = (new Benchmark_16());
					fA1 = tA18;
					(tA18 as Benchmark_16).Benchmark_16_init___B_V(lA4);
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_849;
					tA19 = (new Benchmark_17());
					fA1 = tA19;
					(tA19 as Benchmark_17).Benchmark_17_init___S_V(lA5);
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_850;
					tA20 = (new Benchmark_18());
					fA1 = tA20;
					(tA20 as Benchmark_18).Benchmark_18_init___C_V(lA6);
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_851;
					tA21 = (new Benchmark_19());
					fA1 = tA21;
					(tA21 as Benchmark_19).Benchmark_19_init___I_V(lA7);
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_852;
					tA22 = (new Benchmark_20());
					fA1 = tA22;
					(tA22 as Benchmark_20).Benchmark_20_init___F_V(lA8);
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_853;
					tA23 = (new Benchmark_21());
					fA1 = tA23;
					(tA23 as Benchmark_21).Benchmark_21_init___D_V(lA9);
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_854;
					tA24 = (new Benchmark_22());
					fA1 = tA24;
					(tA24 as Benchmark_22).Benchmark_22_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_855;
					tA25 = (new Benchmark_23());
					fA1 = tA25;
					(tA25 as Benchmark_23).Benchmark_23_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_856;
					tA26 = (new Benchmark_24());
					fA1 = tA26;
					(tA26 as Benchmark_24).Benchmark_24_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_857;
					tA27 = (new Benchmark_25());
					fA1 = tA27;
					(tA27 as Benchmark_25).Benchmark_25_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_858;
					tA28 = (new Benchmark_26());
					fA1 = tA28;
					(tA28 as Benchmark_26).Benchmark_26_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_859;
					tA29 = (new Benchmark_27());
					fA1 = tA29;
					(tA29 as Benchmark_27).Benchmark_27_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_860;
					tA30 = (new Benchmark_28());
					fA1 = tA30;
					(tA30 as Benchmark_28).Benchmark_28_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_861;
					tA31 = (new Benchmark_29());
					fA1 = tA31;
					(tA31 as Benchmark_29).Benchmark_29_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_862;
					tA32 = (new Benchmark_30());
					fA1 = tA32;
					(tA32 as Benchmark_30).Benchmark_30_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_863;
					tA33 = (new Benchmark_31());
					fA1 = tA33;
					(tA33 as Benchmark_31).Benchmark_31_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_864;
					tA34 = (new Benchmark_32());
					fA1 = tA34;
					(tA34 as Benchmark_32).Benchmark_32_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_865;
					tA35 = (new Benchmark_33());
					fA1 = tA35;
					(tA35 as Benchmark_33).Benchmark_33_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_866;
					tA36 = (new Benchmark_34());
					fA1 = tA36;
					(tA36 as Benchmark_34).Benchmark_34_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_867;
					tA37 = (new Benchmark_35());
					fA1 = tA37;
					(tA37 as Benchmark_35).Benchmark_35_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					java_lang_System.gc__V();
					fA0 = Bootstrap.STRINGLIT_868;
					tA38 = (new Benchmark_36());
					fA1 = tA38;
					(tA38 as Benchmark_36).Benchmark_36_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					lA10 = new JA_L(100000, "[LBenchmark\$MyClass2;");
					fA0 = Bootstrap.STRINGLIT_869;
					tA39 = (new Benchmark_37());
					fA1 = tA39;
					(tA39 as Benchmark_37).Benchmark_37_init___LBenchmark_MyClass2__V(lA10);
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_870;
					tA40 = (new Benchmark_38());
					fA1 = tA40;
					(tA40 as Benchmark_38).Benchmark_38_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					tA41 = new JA_C(253);
					fA0 = tA41;
					(tA41 as JA_C).data[0] = 80;
					(fA0 as JA_C).setArraySlice(1, [ 75, 3, 4, 10, 3, 0, 0, 0, 0, 73, 158, 116, 72, 163, 28, 41, 28, 12, 0, 0, 0, 12, 0, 0, 0, 9, 0, 0, 0, 104, 101, 108, 108, 111, 46, 116, 120, 116, 72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33, 80, 75, 3, 4, 20, 3, 0, 0, 8, 0, 53, 181, 116, 72, 170, 192, 105, 58, 29, 0, 0, 0, 56, 7, 0, 0, 10, 0, 0, 0, 104, 101, 108, 108, 111, 50, 46, 116, 120, 116, 243, 72, 205, 201, 201, 87, 8, 207, 47, 202, 73, 81, 28, 101, 143, 178, 71, 217, 163, 236, 81, 246, 40, 123, 148, 141, 159, 13, 0, 80, 75, 1, 2, 63, 3, 10, 3, 0, 0, 0, 0, 73, 158, 116, 72, 163, 28, 41, 28, 12, 0, 0, 0, 12, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 128, 164, 129, 0, 0, 0, 0, 104, 101, 108, 108, 111, 46, 116, 120, 116, 80, 75, 1, 2, 63, 3, 20, 3, 0, 0, 8, 0, 53, 181, 116, 72, 170, 192, 105, 58, 29, 0, 0, 0, 56, 7, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 128, 164, 129, 51, 0, 0, 0, 104, 101, 108, 108, 111, 50, 46, 116, 120, 116, 80, 75, 5, 6, 0, 0, 0, 0, 2, 0, 2, 0, 111, 0, 0, 0, 120, 0, 0, 0, 0, 0 ]);
					lA11 = fA0;
					lA12 = new JA_B((lA11 as JA_0).length);
					lI13 = 0;
					G = 1;
					continue;
				case 1:
					if (((lI13 >= (lA11 as JA_0).length))) {
						G = 2;
						continue;
					}
					lA12.data[lI13] = N.i2b(((lA11 as JA_C)).data[lI13]);
					lI13 = (N.I(lI13 + 1));
					G = 1;
					continue;
				case 2:
					fA0 = Bootstrap.STRINGLIT_871;
					tA294 = (new Benchmark_39());
					fA1 = tA294;
					(tA294 as Benchmark_39).Benchmark_39_init___B_V(lA12);
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_872;
					tA295 = (new Benchmark_40());
					fA1 = tA295;
					(tA295 as Benchmark_40).Benchmark_40_init___B_V(lA12);
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					tA296 = (new java_util_Random());
					fA0 = tA296;
					(tA296 as java_util_Random).java_util_Random_init__J_V(N.lnew(0));
					lA13 = fA0;
					lA14 = new JA_B(65536);
					lI15 = 0;
					G = 3;
					continue;
				case 3:
					if (((lI15 >= (lA14 as JA_0).length))) {
						G = 4;
						continue;
					}
					(lA14 as JA_B).data[lI15] = N.i2b((lA13 as java_util_Random).nextInt__I());
					lI15 = (N.I(lI15 + 1));
					G = 3;
					continue;
				case 4:
					fA0 = Bootstrap.STRINGLIT_873;
					tA297 = (new Benchmark_41());
					fA1 = tA297;
					(tA297 as Benchmark_41).Benchmark_41_init___B_V((lA14 as JA_B));
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_874;
					tA298 = (new Benchmark_42());
					fA1 = tA298;
					(tA298 as Benchmark_42).Benchmark_42_init___B_V((lA14 as JA_B));
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_875;
					tA299 = (new Benchmark_43());
					fA1 = tA299;
					(tA299 as Benchmark_43).Benchmark_43_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = Bootstrap.STRINGLIT_876;
					tA300 = (new Benchmark_44());
					fA1 = tA300;
					(tA300 as Benchmark_44).Benchmark_44_init___V();
					Benchmark.benchmark_Ljava_lang_String_LBenchmark_Task__V((fA0 as java_lang_String), (fA1 as Benchmark_Task));
					fA0 = java_lang_System.__out;
					tA301 = (new java_lang_StringBuilder());
					fA1 = tA301;
					(tA301 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
					(fA0 as java_io_PrintStream).println_Ljava_lang_String__V((fA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_877).append_D_Ljava_lang_StringBuilder_(Benchmark._totalTime).toString__Ljava_lang_String_());
					return;
				default:
					break;
			}
		}
		return;
	}
	static void benchmark_Ljava_lang_String_LBenchmark_Task__V(java_lang_String p0, Benchmark_Task p1) {
		int G = 0;
		int lI4 = 0;
		int lI6 = 0;
		java_lang_Object fA0 = null;
		java_lang_Object fA1 = null;
		double lD4 = 0.0;
		double lD6 = 0.0;
		double lD8 = 0.0;
		java_lang_Object tA0 = null;
		java_lang_Throwable J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch (G) {
						case 0:
							fA0 = java_lang_System.__out;
							tA0 = (new java_lang_StringBuilder());
							fA1 = tA0;
							(tA0 as java_lang_StringBuilder).java_lang_StringBuilder_init___V();
							(fA0 as java_io_PrintStream).print_Ljava_lang_String__V((fA1 as java_lang_StringBuilder).append_Ljava_lang_String__Ljava_lang_StringBuilder_(p0).append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap.STRINGLIT_878).toString__Ljava_lang_String_());
							java_lang_System.__out.flush__V();
							G = 1;
							continue;
						case 1:
							com_jtransc_JTranscSystem.stamp__D();
							lI4 = 0;
							G = 2;
							continue;
						case 2:
							if (((lI4 >= 10))) {
								G = 3;
								continue;
							}
							p1.run__I();
							lI4 = (N.I(lI4 + 1));
							G = 2;
							continue;
						case 3:
							java_lang_System.gc__V();
							lD4 = com_jtransc_JTranscSystem.stamp__D();
							lI6 = 0;
							G = 4;
							continue;
						case 4:
							if (((lI6 >= 10))) {
								G = 5;
								continue;
							}
							p1.run__I();
							lI6 = (N.I(lI6 + 1));
							G = 4;
							continue;
						case 5:
							lD6 = com_jtransc_JTranscSystem.stamp__D();
							lD8 = com_jtransc_JTranscSystem.elapsedTime_DD_D(lD4, lD6);
							java_lang_System.__out.println_D_V(lD8);
							Benchmark._totalTime = ((Benchmark._totalTime + lD8));
							G = 6;
							continue;
						case 6:
							G = 7;
							continue;
						case 8:
							fA0 = J__exception__;
							com_jtransc_io_JTranscConsole.log_Ljava_lang_Object__V(Bootstrap.STRINGLIT_879);
							G = 7;
							continue;
						case 7:
							return;
						default:
							break;
					}
				}
				return;
			}
			catch (J__i__exception__) {
				J__exception__ = N.getJavaException(J__i__exception__);
				if (((((((G >= 1)) && ((G < 6)))) && ((J__exception__) is java_lang_Throwable)))) {
					G = 8;
					continue;
				}
				rethrow;
			}
		}
		return;
	}
	static int calc_II_I(int p0, int p1) {
		return (N.I((N.I(p0 + p1)) * (N.I(p0 + p1))));
	}
	Benchmark([int CLASS_ID = 655]) : super(CLASS_ID) { }
	static void SI() {
		Benchmark._totalTime = 0.0;
		Benchmark.Benchmark_clinit___V();
	}
}



class Bootstrap {
	static java_lang_String STRINGLIT_0 = N.strLitEscape("\u0040");
	static java_lang_String STRINGLIT_1 = N.strLitEscape("\u0029");
	static java_lang_String STRINGLIT_2 = N.strLitEscape("name == null");
	static java_lang_String STRINGLIT_3 = N.strLitEscape("cl == null");
	static java_lang_String STRINGLIT_4 = N.strLitEscape("[]");
	static java_lang_String STRINGLIT_5 = N.strLitEscape("\u0028this Collection\u0029");
	static java_lang_String STRINGLIT_6 = N.strLitEscape(", ");
	static java_lang_String STRINGLIT_7 = N.strLitEscape("=");
	static java_lang_String STRINGLIT_8 = N.strLitEscape("key == null");
	static java_lang_String STRINGLIT_9 = N.strLitEscape("value == null");
	static java_lang_String STRINGLIT_10 = N.strLitEscape("\u0028this Map\u0029");
	static java_lang_String STRINGLIT_11 = N.strLitEscape("threshold");
	static java_lang_String STRINGLIT_12 = N.strLitEscape("loadFactor");
	static java_lang_String STRINGLIT_13 = N.strLitEscape("Simd.MutableFloat32x4\u0028");
	static java_lang_String STRINGLIT_14 = N.strLitEscape("Exception:");
	static java_lang_String STRINGLIT_15 = N.strLitEscape("\tat ");
	static java_lang_String STRINGLIT_16 = N.strLitEscape("Supressed:");
	static java_lang_String STRINGLIT_17 = N.strLitEscape("Cause:");
	static java_lang_String STRINGLIT_18 = N.strLitEscape("need dictionary");
	static java_lang_String STRINGLIT_19 = N.strLitEscape("stream end");
	static java_lang_String STRINGLIT_20 = N.strLitEscape("");
	static java_lang_String STRINGLIT_21 = N.strLitEscape("file error");
	static java_lang_String STRINGLIT_22 = N.strLitEscape("stream error");
	static java_lang_String STRINGLIT_23 = N.strLitEscape("data error");
	static java_lang_String STRINGLIT_24 = N.strLitEscape("insufficient memory");
	static java_lang_String STRINGLIT_25 = N.strLitEscape("buffer error");
	static java_lang_String STRINGLIT_26 = N.strLitEscape("incompatible version");
	static java_lang_String STRINGLIT_27 = N.strLitEscape(": ");
	static java_lang_String STRINGLIT_28 = N.strLitEscape("capacity < 0: ");
	static java_lang_String STRINGLIT_29 = N.strLitEscape("]");
	static java_lang_String STRINGLIT_30 = N.strLitEscape(",capacity=");
	static java_lang_String STRINGLIT_31 = N.strLitEscape(",limit=");
	static java_lang_String STRINGLIT_32 = N.strLitEscape("[position=");
	static java_lang_String STRINGLIT_33 = N.strLitEscape(", limit=");
	static java_lang_String STRINGLIT_34 = N.strLitEscape("index=");
	static java_lang_String STRINGLIT_35 = N.strLitEscape(", remaining\u0028\u0029=");
	static java_lang_String STRINGLIT_36 = N.strLitEscape("BIG_ENDIAN");
	static java_lang_String STRINGLIT_37 = N.strLitEscape("LITTLE_ENDIAN");
	static java_lang_String STRINGLIT_38 = N.strLitEscape(", arrayOffset=");
	static java_lang_String STRINGLIT_39 = N.strLitEscape(", capacity=");
	static java_lang_String STRINGLIT_40 = N.strLitEscape("backingArray.length=");
	static java_lang_String STRINGLIT_41 = N.strLitEscape("hello");
	static java_lang_String STRINGLIT_42 = N.strLitEscape("\n");
	static java_lang_String STRINGLIT_43 = N.strLitEscape("HOME");
	static java_lang_String STRINGLIT_44 = N.strLitEscape("/tmp");
	static java_lang_String STRINGLIT_45 = N.strLitEscape("en");
	static java_lang_String STRINGLIT_46 = N.strLitEscape("TMPDIR");
	static java_lang_String STRINGLIT_47 = N.strLitEscape("TEMP");
	static java_lang_String STRINGLIT_48 = N.strLitEscape("TMP");
	static java_lang_String STRINGLIT_49 = N.strLitEscape("/");
	static java_lang_String STRINGLIT_50 = N.strLitEscape("\\");
	static java_lang_String STRINGLIT_51 = N.strLitEscape("UTF\u002d8");
	static java_lang_String STRINGLIT_52 = N.strLitEscape(":");
	static java_lang_String STRINGLIT_53 = N.strLitEscape("USERNAME");
	static java_lang_String STRINGLIT_54 = N.strLitEscape("USER");
	static java_lang_String STRINGLIT_55 = N.strLitEscape("username");
	static java_lang_String STRINGLIT_56 = N.strLitEscape("US");
	static java_lang_String STRINGLIT_57 = N.strLitEscape("; regionLength=");
	static java_lang_String STRINGLIT_58 = N.strLitEscape("; regionStart=");
	static java_lang_String STRINGLIT_59 = N.strLitEscape("length=");
	static java_lang_String STRINGLIT_60 = N.strLitEscape(",maxpri=");
	static java_lang_String STRINGLIT_61 = N.strLitEscape("[name=");
	static java_lang_String STRINGLIT_62 = N.strLitEscape(",");
	static java_lang_String STRINGLIT_63 = N.strLitEscape("Thread[");
	static java_lang_String STRINGLIT_64 = N.strLitEscape(",]");
	static java_lang_String STRINGLIT_65 = N.strLitEscape("UTF\u002d16BE");
	static java_lang_String STRINGLIT_66 = N.strLitEscape("UnicodeBigUnmarked");
	static java_lang_String STRINGLIT_67 = N.strLitEscape("X\u002dUTF\u002d16BE");
	static java_lang_String STRINGLIT_68 = N.strLitEscape("ISO\u002d10646\u002dUCS\u002d2");
	static java_lang_String STRINGLIT_69 = N.strLitEscape("ISO\u002d8859\u002d1");
	static java_lang_String STRINGLIT_70 = N.strLitEscape("819");
	static java_lang_String STRINGLIT_71 = N.strLitEscape("ISO8859\u002d1");
	static java_lang_String STRINGLIT_72 = N.strLitEscape("L1");
	static java_lang_String STRINGLIT_73 = N.strLitEscape("ISO_8859\u002d1:1987");
	static java_lang_String STRINGLIT_74 = N.strLitEscape("ISO_8859\u002d1");
	static java_lang_String STRINGLIT_75 = N.strLitEscape("8859_1");
	static java_lang_String STRINGLIT_76 = N.strLitEscape("ISO\u002dIR\u002d100");
	static java_lang_String STRINGLIT_77 = N.strLitEscape("LATIN1");
	static java_lang_String STRINGLIT_78 = N.strLitEscape("CP819");
	static java_lang_String STRINGLIT_79 = N.strLitEscape("ISO8859_1");
	static java_lang_String STRINGLIT_80 = N.strLitEscape("IBM819");
	static java_lang_String STRINGLIT_81 = N.strLitEscape("ISO_8859_1");
	static java_lang_String STRINGLIT_82 = N.strLitEscape("IBM\u002d819");
	static java_lang_String STRINGLIT_83 = N.strLitEscape("CSISOLATIN1");
	static java_lang_String STRINGLIT_84 = N.strLitEscape("\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\t\n\u000b\u000c\r\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f !\"\u0023\$%&\'\u0028\u0029\u002a\u002b,\u002d./0123456789:;<=>\u003f\u0040ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]\u005e_\u0060abcdefghijklmnopqrstuvwxyz{\u007c}\u007e\u007f\u0080\u0081\u0082\u0083\u0084\u0085\u0086\u0087\u0088\u0089\u008a\u008b\u008c\u008d\u008e\u008f\u0090\u0091\u0092\u0093\u0094\u0095\u0096\u0097\u0098\u0099\u009a\u009b\u009c\u009d\u009e\u009f\u00a0\u00a1\u00a2\u00a3\u00a4\u00a5\u00a6\u00a7\u00a8\u00a9\u00aa\u00ab\u00ac\u00ad\u00ae\u00af\u00b0\u00b1\u00b2\u00b3\u00b4\u00b5\u00b6\u00b7\u00b8\u00b9\u00ba\u00bb\u00bc\u00bd\u00be\u00bf\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u00c6\u00c7\u00c8\u00c9\u00ca\u00cb\u00cc\u00cd\u00ce\u00cf\u00d0\u00d1\u00d2\u00d3\u00d4\u00d5\u00d6\u00d7\u00d8\u00d9\u00da\u00db\u00dc\u00dd\u00de\u00df\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e6\u00e7\u00e8\u00e9\u00ea\u00eb\u00ec\u00ed\u00ee\u00ef\u00f0\u00f1\u00f2\u00f3\u00f4\u00f5\u00f6\u00f7\u00f8\u00f9\u00fa\u00fb\u00fc\u00fd\u00fe\u00ff");
	static java_lang_String STRINGLIT_85 = N.strLitEscape("UTF\u002d16LE");
	static java_lang_String STRINGLIT_86 = N.strLitEscape("UTF\u002d16");
	static java_lang_String STRINGLIT_87 = N.strLitEscape("UnicodeLittleUnmarked");
	static java_lang_String STRINGLIT_88 = N.strLitEscape("X\u002dUTF\u002d16LE");
	static java_lang_String STRINGLIT_89 = N.strLitEscape("US\u002dASCII");
	static java_lang_String STRINGLIT_90 = N.strLitEscape("ANSI_X3.4\u002d1968");
	static java_lang_String STRINGLIT_91 = N.strLitEscape("CP367");
	static java_lang_String STRINGLIT_92 = N.strLitEscape("CSASCII");
	static java_lang_String STRINGLIT_93 = N.strLitEscape("ISO\u002dIR\u002d6");
	static java_lang_String STRINGLIT_94 = N.strLitEscape("ASCII");
	static java_lang_String STRINGLIT_95 = N.strLitEscape("ISO_646.IRV:1983");
	static java_lang_String STRINGLIT_96 = N.strLitEscape("ANSI_X3.4\u002d1986");
	static java_lang_String STRINGLIT_97 = N.strLitEscape("ASCII7");
	static java_lang_String STRINGLIT_98 = N.strLitEscape("DEFAULT");
	static java_lang_String STRINGLIT_99 = N.strLitEscape("ISO_646.IRV:1991");
	static java_lang_String STRINGLIT_100 = N.strLitEscape("ISO646\u002dUS");
	static java_lang_String STRINGLIT_101 = N.strLitEscape("IBM367");
	static java_lang_String STRINGLIT_102 = N.strLitEscape("646");
	static java_lang_String STRINGLIT_103 = N.strLitEscape("\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\t\n\u000b\u000c\r\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f !\"\u0023\$%&\'\u0028\u0029\u002a\u002b,\u002d./0123456789:;<=>\u003f\u0040ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]\u005e_\u0060abcdefghijklmnopqrstuvwxyz{\u007c}\u007e\u007f\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd");
	static java_lang_String STRINGLIT_104 = N.strLitEscape("UTF8");
	static java_lang_String STRINGLIT_105 = N.strLitEscape("{}");
	static java_lang_String STRINGLIT_106 = N.strLitEscape("IBM866");
	static java_lang_String STRINGLIT_107 = N.strLitEscape("866");
	static java_lang_String STRINGLIT_108 = N.strLitEscape("IBM\u002d866");
	static java_lang_String STRINGLIT_109 = N.strLitEscape("CSIBM866");
	static java_lang_String STRINGLIT_110 = N.strLitEscape("CP866");
	static java_lang_String STRINGLIT_111 = N.strLitEscape("\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\t\n\u000b\u000c\r\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f !\"\u0023\$%&\'\u0028\u0029\u002a\u002b,\u002d./0123456789:;<=>\u003f\u0040ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]\u005e_\u0060abcdefghijklmnopqrstuvwxyz{\u007c}\u007e\u007f\u0410\u0411\u0412\u0413\u0414\u0415\u0416\u0417\u0418\u0419\u041a\u041b\u041c\u041d\u041e\u041f\u0420\u0421\u0422\u0423\u0424\u0425\u0426\u0427\u0428\u0429\u042a\u042b\u042c\u042d\u042e\u042f\u0430\u0431\u0432\u0433\u0434\u0435\u0436\u0437\u0438\u0439\u043a\u043b\u043c\u043d\u043e\u043f\u2591\u2592\u2593\u2502\u2524\u2561\u2562\u2556\u2555\u2563\u2551\u2557\u255d\u255c\u255b\u2510\u2514\u2534\u252c\u251c\u2500\u253c\u255e\u255f\u255a\u2554\u2569\u2566\u2560\u2550\u256c\u2567\u2568\u2564\u2565\u2559\u2558\u2552\u2553\u256b\u256a\u2518\u250c\u2588\u2584\u258c\u2590\u2580\u0440\u0441\u0442\u0443\u0444\u0445\u0446\u0447\u0448\u0449\u044a\u044b\u044c\u044d\u044e\u044f\u0401\u0451\u0404\u0454\u0407\u0457\u040e\u045e\u00b0\u2219\u00b7\u221a\u2116\u00a4\u25a0\u00a0");
	static java_lang_String STRINGLIT_112 = N.strLitEscape("com.jtransc.charset.JTranscCharset");
	static java_lang_String STRINGLIT_113 = N.strLitEscape("com.jtransc.JTranscProcess");
	static java_lang_String STRINGLIT_114 = N.strLitEscape("ServiceLoader for ");
	static java_lang_String STRINGLIT_115 = N.strLitEscape("size < 0");
	static java_lang_String STRINGLIT_116 = N.strLitEscape("0.6.1");
	static java_lang_String STRINGLIT_117 = N.strLitEscape("orld");
	static java_lang_String STRINGLIT_118 = N.strLitEscape("a");
	static java_lang_String STRINGLIT_119 = N.strLitEscape("test");
	static java_lang_String STRINGLIT_120 = N.strLitEscape("java");
	static java_lang_String STRINGLIT_121 = N.strLitEscape("haxe\u002djs");
	static java_lang_String STRINGLIT_122 = N.strLitEscape("haxe\u002das3");
	static java_lang_String STRINGLIT_123 = N.strLitEscape("haxe\u002dneko");
	static java_lang_String STRINGLIT_124 = N.strLitEscape("haxe\u002dcpp");
	static java_lang_String STRINGLIT_125 = N.strLitEscape("haxe");
	static java_lang_String STRINGLIT_126 = N.strLitEscape("swf");
	static java_lang_String STRINGLIT_127 = N.strLitEscape("csharp");
	static java_lang_String STRINGLIT_128 = N.strLitEscape("neko");
	static java_lang_String STRINGLIT_129 = N.strLitEscape("php");
	static java_lang_String STRINGLIT_130 = N.strLitEscape("python");
	static java_lang_String STRINGLIT_131 = N.strLitEscape("as3");
	static java_lang_String STRINGLIT_132 = N.strLitEscape("dart");
	static java_lang_String STRINGLIT_133 = N.strLitEscape("cpp");
	static java_lang_String STRINGLIT_134 = N.strLitEscape("unknown");
	static java_lang_String STRINGLIT_135 = N.strLitEscape("win");
	static java_lang_String STRINGLIT_136 = N.strLitEscape("windows");
	static java_lang_String STRINGLIT_137 = N.strLitEscape("lin");
	static java_lang_String STRINGLIT_138 = N.strLitEscape("linux");
	static java_lang_String STRINGLIT_139 = N.strLitEscape("mac");
	static java_lang_String STRINGLIT_140 = N.strLitEscape("osx");
	static java_lang_String STRINGLIT_141 = N.strLitEscape("fuch");
	static java_lang_String STRINGLIT_142 = N.strLitEscape("fuchsia");
	static java_lang_String STRINGLIT_143 = N.strLitEscape("java.home");
	static java_lang_String STRINGLIT_144 = N.strLitEscape("os.arch");
	static java_lang_String STRINGLIT_145 = N.strLitEscape("storage == null");
	static java_lang_String STRINGLIT_146 = N.strLitEscape("<init>");
	static java_lang_String STRINGLIT_147 = N.strLitEscape("\u0028\u0029V");
	static java_lang_String STRINGLIT_148 = N.strLitEscape("\u0028[C\u0029V");
	static java_lang_String STRINGLIT_149 = N.strLitEscape("\u0028[BII\u0029V");
	static java_lang_String STRINGLIT_150 = N.strLitEscape("\u0028[BIILjava/lang/String;Z\u0029V");
	static java_lang_String STRINGLIT_151 = N.strLitEscape("\u0028Ljava/lang/String\$1;\u0029V");
	static java_lang_String STRINGLIT_152 = N.strLitEscape("\u0028I\u0029V");
	static java_lang_String STRINGLIT_153 = N.strLitEscape("\u0028Ljava/lang/String;Z\u0029V");
	static java_lang_String STRINGLIT_154 = N.strLitEscape("\u0028Ljava/lang/String;\u0029V");
	static java_lang_String STRINGLIT_155 = N.strLitEscape("\u0028Ljava/lang/Class;Lj/MemberInfo;\u0029V");
	static java_lang_String STRINGLIT_156 = N.strLitEscape("\u0028Ljava/lang/Class<\u002a>;Lj/MemberInfo;\u0029V");
	static java_lang_String STRINGLIT_157 = N.strLitEscape("\u0028Lj/MemberInfo;\u0029V");
	static java_lang_String STRINGLIT_158 = N.strLitEscape("\u0028Ljava/lang/String;Ljava/lang/Throwable;\u0029V");
	static java_lang_String STRINGLIT_159 = N.strLitEscape("\u0028Ljava/lang/Throwable;\u0029V");
	static java_lang_String STRINGLIT_160 = N.strLitEscape("\u0028F\u0029V");
	static java_lang_String STRINGLIT_161 = N.strLitEscape("\u0028C\u0029V");
	static java_lang_String STRINGLIT_162 = N.strLitEscape("\u0028Ljava/io/OutputStream;\u0029V");
	static java_lang_String STRINGLIT_163 = N.strLitEscape("\u0028Z\u0029V");
	static java_lang_String STRINGLIT_164 = N.strLitEscape("\u0028Lcom/jtransc/io/JTranscConsolePrintStream\$ConsoleBaseStream;Z\u0029V");
	static java_lang_String STRINGLIT_165 = N.strLitEscape("\u0028D\u0029V");
	static java_lang_String STRINGLIT_166 = N.strLitEscape("\u0028Ljava/lang/Object;Ljava/lang/Object;ILjava/util/HashMap\$HashMapEntry;\u0029V");
	static java_lang_String STRINGLIT_167 = N.strLitEscape("\u0028TK;TV;ILjava/util/HashMap\$HashMapEntry<TK;TV;>;\u0029V");
	static java_lang_String STRINGLIT_168 = N.strLitEscape("\u0028Ljava/util/HashMap;Ljava/util/HashMap\$1;\u0029V");
	static java_lang_String STRINGLIT_169 = N.strLitEscape("\u0028Ljava/util/HashMap;\u0029V");
	static java_lang_String STRINGLIT_170 = N.strLitEscape("\u0028J\u0029V");
	static java_lang_String STRINGLIT_171 = N.strLitEscape("\u0028S\u0029V");
	static java_lang_String STRINGLIT_172 = N.strLitEscape("\u0028B\u0029V");
	static java_lang_String STRINGLIT_173 = N.strLitEscape("\u0028ILjava/lang/String;Ljava/lang/String;II[I[I\u0029V");
	static java_lang_String STRINGLIT_174 = N.strLitEscape("\u0028Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;I\u0029V");
	static java_lang_String STRINGLIT_175 = N.strLitEscape("\u0028ILjava/lang/String;Ljava/lang/String;ILjava/lang/String;Ljava/lang/String;\u0029V");
	static java_lang_String STRINGLIT_176 = N.strLitEscape("\u0028Ljava/util/AbstractList;I\u0029V");
	static java_lang_String STRINGLIT_177 = N.strLitEscape("\u0028Ljava/util/AbstractList;\u0029V");
	static java_lang_String STRINGLIT_178 = N.strLitEscape("\u0028[Ljava/lang/Object;\u0029V");
	static java_lang_String STRINGLIT_179 = N.strLitEscape("\u0028[TE;\u0029V");
	static java_lang_String STRINGLIT_180 = N.strLitEscape("\u0028Ljava/lang/Object;\u0029V");
	static java_lang_String STRINGLIT_181 = N.strLitEscape("\u0028[LBenchmark\$MyClass2;\u0029V");
	static java_lang_String STRINGLIT_182 = N.strLitEscape("\u0028Ljava/lang/String;I\u0029V");
	static java_lang_String STRINGLIT_183 = N.strLitEscape("\u0028[B\u0029V");
	static java_lang_String STRINGLIT_184 = N.strLitEscape("\u0028[I[I\u0029V");
	static java_lang_String STRINGLIT_185 = N.strLitEscape("\u0028[I\u0029V");
	static java_lang_String STRINGLIT_186 = N.strLitEscape("\u0028[S\u0029V");
	static java_lang_String STRINGLIT_187 = N.strLitEscape("\u0028[D\u0029V");
	static java_lang_String STRINGLIT_188 = N.strLitEscape("\u0028[F\u0029V");
	static java_lang_String STRINGLIT_189 = N.strLitEscape("\u0028Lcom/jtransc/time/JTranscClock\$Impl;\u0029V");
	static java_lang_String STRINGLIT_190 = N.strLitEscape("\u0028[Ljava/lang/String;IFI\u0029V");
	static java_lang_String STRINGLIT_191 = N.strLitEscape("\u0028Ljava/lang/Class;\u0029V");
	static java_lang_String STRINGLIT_192 = N.strLitEscape("\u0028Ljava/lang/Class<TS;>;\u0029V");
	static java_lang_String STRINGLIT_193 = N.strLitEscape("\u0028[Ljava/lang/String;Ljava/lang/String;\u0029V");
	static java_lang_String STRINGLIT_194 = N.strLitEscape("\u0028Ljava/util/Collections\$1;\u0029V");
	static java_lang_String STRINGLIT_195 = N.strLitEscape("\u0028[Ljava/lang/String;Z\u0029V");
	static java_lang_String STRINGLIT_196 = N.strLitEscape("\u0028Ljava/lang/ClassLoader;\u0029V");
	static java_lang_String STRINGLIT_197 = N.strLitEscape("\u0028IILjava/nio/internal/MemoryBlock;\u0029V");
	static java_lang_String STRINGLIT_198 = N.strLitEscape("\u0028I[BIZ\u0029V");
	static java_lang_String STRINGLIT_199 = N.strLitEscape("\u0028[BZ\u0029V");
	static java_lang_String STRINGLIT_200 = N.strLitEscape("\u0028Ljava/nio/ByteBuffer;\u0029V");
	static java_lang_String STRINGLIT_201 = N.strLitEscape("\u0028Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsLongBuffer\$1;\u0029V");
	static java_lang_String STRINGLIT_202 = N.strLitEscape("\u0028Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsDoubleBuffer\$1;\u0029V");
	static java_lang_String STRINGLIT_203 = N.strLitEscape("\u0028Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsCharBuffer\$1;\u0029V");
	static java_lang_String STRINGLIT_204 = N.strLitEscape("\u0028Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsShortBuffer\$1;\u0029V");
	static java_lang_String STRINGLIT_205 = N.strLitEscape("\u0028IIIII\u0029V");
	static java_lang_String STRINGLIT_206 = N.strLitEscape("\u0028IZ\u0029V");
	static java_lang_String STRINGLIT_207 = N.strLitEscape("\u0028LBenchmark\$1;\u0029V");
	static java_lang_String STRINGLIT_208 = N.strLitEscape("\u0028Ljava/lang/Object;Ljava/lang/Object;ILjava/util/Hashtable\$HashtableEntry;\u0029V");
	static java_lang_String STRINGLIT_209 = N.strLitEscape("\u0028TK;TV;ILjava/util/Hashtable\$HashtableEntry<TK;TV;>;\u0029V");
	static java_lang_String STRINGLIT_210 = N.strLitEscape("\u0028Ljava/util/Hashtable;Ljava/util/Hashtable\$1;\u0029V");
	static java_lang_String STRINGLIT_211 = N.strLitEscape("\u0028Ljava/util/Hashtable;\u0029V");
	static java_lang_String STRINGLIT_212 = N.strLitEscape("\u0028Ljava/lang/String;Ljava/lang/Class;\u0029V");
	static java_lang_String STRINGLIT_213 = N.strLitEscape("\u0028Ljava/lang/String;Ljava/lang/Class<\u002a>;\u0029V");
	static java_lang_String STRINGLIT_214 = N.strLitEscape("\u0028TT;\u0029V");
	static java_lang_String STRINGLIT_215 = N.strLitEscape("\u0028Ljava/lang/Object;Ljava/lang/ref/ReferenceQueue;\u0029V");
	static java_lang_String STRINGLIT_216 = N.strLitEscape("\u0028TT;Ljava/lang/ref/ReferenceQueue<\u002dTT;>;\u0029V");
	static java_lang_String STRINGLIT_217 = N.strLitEscape(", Size: ");
	static java_lang_String STRINGLIT_218 = N.strLitEscape("Index: ");
	static java_lang_String STRINGLIT_219 = N.strLitEscape("Can\'t read more");
	static java_lang_String STRINGLIT_220 = N.strLitEscape(" but found end");
	static java_lang_String STRINGLIT_221 = N.strLitEscape("Expected ");
	static java_lang_String STRINGLIT_222 = N.strLitEscape(" ");
	static java_lang_String STRINGLIT_223 = N.strLitEscape(".");
	static java_lang_String STRINGLIT_224 = N.strLitEscape("\u0028");
	static java_lang_String STRINGLIT_225 = N.strLitEscape("totalTime");
	static java_lang_String STRINGLIT_226 = N.strLitEscape("D");
	static java_lang_String STRINGLIT_227 = N.strLitEscape("CASE_INSENSITIVE_ORDER");
	static java_lang_String STRINGLIT_228 = N.strLitEscape("Ljava/util/Comparator;");
	static java_lang_String STRINGLIT_229 = N.strLitEscape("Ljava/util/Comparator<Ljava/lang/String;>;");
	static java_lang_String STRINGLIT_230 = N.strLitEscape("hash");
	static java_lang_String STRINGLIT_231 = N.strLitEscape("I");
	static java_lang_String STRINGLIT_232 = N.strLitEscape("name");
	static java_lang_String STRINGLIT_233 = N.strLitEscape("Ljava/lang/String;");
	static java_lang_String STRINGLIT_234 = N.strLitEscape("primitive");
	static java_lang_String STRINGLIT_235 = N.strLitEscape("Z");
	static java_lang_String STRINGLIT_236 = N.strLitEscape("modifiers");
	static java_lang_String STRINGLIT_237 = N.strLitEscape("_classCache");
	static java_lang_String STRINGLIT_238 = N.strLitEscape("Lcom/jtransc/ds/FastStringMap;");
	static java_lang_String STRINGLIT_239 = N.strLitEscape("Lcom/jtransc/ds/FastStringMap<Ljava/lang/Class<\u002a>;>;");
	static java_lang_String STRINGLIT_240 = N.strLitEscape("enumConstants");
	static java_lang_String STRINGLIT_241 = N.strLitEscape("[Ljava/lang/Object;");
	static java_lang_String STRINGLIT_242 = N.strLitEscape("[TT;");
	static java_lang_String STRINGLIT_243 = N.strLitEscape("_allFields");
	static java_lang_String STRINGLIT_244 = N.strLitEscape("[Ljava/lang/reflect/Field;");
	static java_lang_String STRINGLIT_245 = N.strLitEscape("_accessibleFields");
	static java_lang_String STRINGLIT_246 = N.strLitEscape("id");
	static java_lang_String STRINGLIT_247 = N.strLitEscape("related");
	static java_lang_String STRINGLIT_248 = N.strLitEscape("[I");
	static java_lang_String STRINGLIT_249 = N.strLitEscape("info");
	static java_lang_String STRINGLIT_250 = N.strLitEscape("Lj/ClassInfo;");
	static java_lang_String STRINGLIT_251 = N.strLitEscape("value");
	static java_lang_String STRINGLIT_252 = N.strLitEscape("values");
	static java_lang_String STRINGLIT_253 = N.strLitEscape("[Ljava/lang/Integer;");
	static java_lang_String STRINGLIT_254 = N.strLitEscape("NTZ_TABLE");
	static java_lang_String STRINGLIT_255 = N.strLitEscape("[B");
	static java_lang_String STRINGLIT_256 = N.strLitEscape("TYPE");
	static java_lang_String STRINGLIT_257 = N.strLitEscape("Ljava/lang/Class;");
	static java_lang_String STRINGLIT_258 = N.strLitEscape("Ljava/lang/Class<Ljava/lang/Integer;>;");
	static java_lang_String STRINGLIT_259 = N.strLitEscape("clazz");
	static java_lang_String STRINGLIT_260 = N.strLitEscape("Ljava/lang/Class<\u002a>;");
	static java_lang_String STRINGLIT_261 = N.strLitEscape("signature");
	static java_lang_String STRINGLIT_262 = N.strLitEscape("slot");
	static java_lang_String STRINGLIT_263 = N.strLitEscape("genericSignature");
	static java_lang_String STRINGLIT_264 = N.strLitEscape("Lj/MemberInfo;");
	static java_lang_String STRINGLIT_265 = N.strLitEscape("ex");
	static java_lang_String STRINGLIT_266 = N.strLitEscape("Ljava/lang/Throwable;");
	static java_lang_String STRINGLIT_267 = N.strLitEscape("thrown");
	static java_lang_String STRINGLIT_268 = N.strLitEscape("EMPTY_ARRAY");
	static java_lang_String STRINGLIT_269 = N.strLitEscape("[Ljava/lang/Throwable;");
	static java_lang_String STRINGLIT_270 = N.strLitEscape("message");
	static java_lang_String STRINGLIT_271 = N.strLitEscape("writableStackTrace");
	static java_lang_String STRINGLIT_272 = N.strLitEscape("enableSuppression");
	static java_lang_String STRINGLIT_273 = N.strLitEscape("cause");
	static java_lang_String STRINGLIT_274 = N.strLitEscape("stackTrace");
	static java_lang_String STRINGLIT_275 = N.strLitEscape("[Ljava/lang/StackTraceElement;");
	static java_lang_String STRINGLIT_276 = N.strLitEscape("supressed");
	static java_lang_String STRINGLIT_277 = N.strLitEscape("Ljava/util/ArrayList;");
	static java_lang_String STRINGLIT_278 = N.strLitEscape("Ljava/util/ArrayList<Ljava/lang/Throwable;>;");
	static java_lang_String STRINGLIT_279 = N.strLitEscape("Ljava/lang/Class<Ljava/lang/Void;>;");
	static java_lang_String STRINGLIT_280 = N.strLitEscape("F");
	static java_lang_String STRINGLIT_281 = N.strLitEscape("Ljava/lang/Class<Ljava/lang/Float;>;");
	static java_lang_String STRINGLIT_282 = N.strLitEscape("C");
	static java_lang_String STRINGLIT_283 = N.strLitEscape("Ljava/lang/Class<Ljava/lang/Character;>;");
	static java_lang_String STRINGLIT_284 = N.strLitEscape("in");
	static java_lang_String STRINGLIT_285 = N.strLitEscape("Ljava/io/InputStream;");
	static java_lang_String STRINGLIT_286 = N.strLitEscape("out");
	static java_lang_String STRINGLIT_287 = N.strLitEscape("Ljava/io/PrintStream;");
	static java_lang_String STRINGLIT_288 = N.strLitEscape("err");
	static java_lang_String STRINGLIT_289 = N.strLitEscape("_props");
	static java_lang_String STRINGLIT_290 = N.strLitEscape("Ljava/util/Properties;");
	static java_lang_String STRINGLIT_291 = N.strLitEscape("encoding");
	static java_lang_String STRINGLIT_292 = N.strLitEscape("autoFlush");
	static java_lang_String STRINGLIT_293 = N.strLitEscape("ioError");
	static java_lang_String STRINGLIT_294 = N.strLitEscape("Ljava/io/OutputStream;");
	static java_lang_String STRINGLIT_295 = N.strLitEscape("error");
	static java_lang_String STRINGLIT_296 = N.strLitEscape("stream");
	static java_lang_String STRINGLIT_297 = N.strLitEscape("Lcom/jtransc/io/JTranscConsolePrintStream\$ConsoleBaseStream;");
	static java_lang_String STRINGLIT_298 = N.strLitEscape("sb");
	static java_lang_String STRINGLIT_299 = N.strLitEscape("Ljava/lang/StringBuilder;");
	static java_lang_String STRINGLIT_300 = N.strLitEscape("Ljava/lang/Class<Ljava/lang/Double;>;");
	static java_lang_String STRINGLIT_301 = N.strLitEscape("table");
	static java_lang_String STRINGLIT_302 = N.strLitEscape("[Ljava/util/HashMap\$HashMapEntry;");
	static java_lang_String STRINGLIT_303 = N.strLitEscape("[Ljava/util/HashMap\$HashMapEntry<TK;TV;>;");
	static java_lang_String STRINGLIT_304 = N.strLitEscape("EMPTY_TABLE");
	static java_lang_String STRINGLIT_305 = N.strLitEscape("[Ljava/util/Map\$Entry;");
	static java_lang_String STRINGLIT_306 = N.strLitEscape("size");
	static java_lang_String STRINGLIT_307 = N.strLitEscape("entryForNullKey");
	static java_lang_String STRINGLIT_308 = N.strLitEscape("Ljava/util/HashMap\$HashMapEntry;");
	static java_lang_String STRINGLIT_309 = N.strLitEscape("Ljava/util/HashMap\$HashMapEntry<TK;TV;>;");
	static java_lang_String STRINGLIT_310 = N.strLitEscape("modCount");
	static java_lang_String STRINGLIT_311 = N.strLitEscape("entrySet");
	static java_lang_String STRINGLIT_312 = N.strLitEscape("Ljava/util/Set;");
	static java_lang_String STRINGLIT_313 = N.strLitEscape("Ljava/util/Set<Ljava/util/Map\$Entry<TK;TV;>;>;");
	static java_lang_String STRINGLIT_314 = N.strLitEscape("Ljava/util/Collection;");
	static java_lang_String STRINGLIT_315 = N.strLitEscape("Ljava/util/Collection<TV;>;");
	static java_lang_String STRINGLIT_316 = N.strLitEscape("keySet");
	static java_lang_String STRINGLIT_317 = N.strLitEscape("Ljava/util/Set<TK;>;");
	static java_lang_String STRINGLIT_318 = N.strLitEscape("valuesCollection");
	static java_lang_String STRINGLIT_319 = N.strLitEscape("key");
	static java_lang_String STRINGLIT_320 = N.strLitEscape("Ljava/lang/Object;");
	static java_lang_String STRINGLIT_321 = N.strLitEscape("TK;");
	static java_lang_String STRINGLIT_322 = N.strLitEscape("next");
	static java_lang_String STRINGLIT_323 = N.strLitEscape("TV;");
	static java_lang_String STRINGLIT_324 = N.strLitEscape("this\$0");
	static java_lang_String STRINGLIT_325 = N.strLitEscape("Ljava/util/HashMap;");
	static java_lang_String STRINGLIT_326 = N.strLitEscape("nextEntry");
	static java_lang_String STRINGLIT_327 = N.strLitEscape("nextIndex");
	static java_lang_String STRINGLIT_328 = N.strLitEscape("expectedModCount");
	static java_lang_String STRINGLIT_329 = N.strLitEscape("lastEntryReturned");
	static java_lang_String STRINGLIT_330 = N.strLitEscape("J");
	static java_lang_String STRINGLIT_331 = N.strLitEscape("Ljava/lang/Class<Ljava/lang/Long;>;");
	static java_lang_String STRINGLIT_332 = N.strLitEscape("S");
	static java_lang_String STRINGLIT_333 = N.strLitEscape("Ljava/lang/Class<Ljava/lang/Short;>;");
	static java_lang_String STRINGLIT_334 = N.strLitEscape("Ljava/lang/Class<Ljava/lang/Boolean;>;");
	static java_lang_String STRINGLIT_335 = N.strLitEscape("TRUE");
	static java_lang_String STRINGLIT_336 = N.strLitEscape("Ljava/lang/Boolean;");
	static java_lang_String STRINGLIT_337 = N.strLitEscape("FALSE");
	static java_lang_String STRINGLIT_338 = N.strLitEscape("cache");
	static java_lang_String STRINGLIT_339 = N.strLitEscape("[Ljava/lang/Byte;");
	static java_lang_String STRINGLIT_340 = N.strLitEscape("B");
	static java_lang_String STRINGLIT_341 = N.strLitEscape("Ljava/lang/Class<Ljava/lang/Byte;>;");
	static java_lang_String STRINGLIT_342 = N.strLitEscape("\$\$lastId");
	static java_lang_String STRINGLIT_343 = N.strLitEscape("_classInfos");
	static java_lang_String STRINGLIT_344 = N.strLitEscape("[Lj/ClassInfo;");
	static java_lang_String STRINGLIT_345 = N.strLitEscape("_classNames");
	static java_lang_String STRINGLIT_346 = N.strLitEscape("[Ljava/lang/String;");
	static java_lang_String STRINGLIT_347 = N.strLitEscape("_classInfosByName");
	static java_lang_String STRINGLIT_348 = N.strLitEscape("Lcom/jtransc/ds/FastStringMap<Lj/ClassInfo;>;");
	static java_lang_String STRINGLIT_349 = N.strLitEscape("fileName");
	static java_lang_String STRINGLIT_350 = N.strLitEscape("lineNumber");
	static java_lang_String STRINGLIT_351 = N.strLitEscape("methodName");
	static java_lang_String STRINGLIT_352 = N.strLitEscape("declaringClass");
	static java_lang_String STRINGLIT_353 = N.strLitEscape("exceptionTypes");
	static java_lang_String STRINGLIT_354 = N.strLitEscape("[Ljava/lang/Class;");
	static java_lang_String STRINGLIT_355 = N.strLitEscape("[Ljava/lang/Class<\u002a>;");
	static java_lang_String STRINGLIT_356 = N.strLitEscape("length");
	static java_lang_String STRINGLIT_357 = N.strLitEscape("buffer");
	static java_lang_String STRINGLIT_358 = N.strLitEscape("Ljava/util/AbstractList;");
	static java_lang_String STRINGLIT_359 = N.strLitEscape("pos");
	static java_lang_String STRINGLIT_360 = N.strLitEscape("lastPosition");
	static java_lang_String STRINGLIT_361 = N.strLitEscape("[TE;");
	static java_lang_String STRINGLIT_362 = N.strLitEscape("val\$objects");
	static java_lang_String STRINGLIT_363 = N.strLitEscape("[LBenchmark\$MyClass2;");
	static java_lang_String STRINGLIT_364 = N.strLitEscape("c");
	static java_lang_String STRINGLIT_365 = N.strLitEscape("d");
	static java_lang_String STRINGLIT_366 = N.strLitEscape("b");
	static java_lang_String STRINGLIT_367 = N.strLitEscape("start");
	static java_lang_String STRINGLIT_368 = N.strLitEscape("val\$hexData");
	static java_lang_String STRINGLIT_369 = N.strLitEscape("seed");
	static java_lang_String STRINGLIT_370 = N.strLitEscape("haveNextNextGaussian");
	static java_lang_String STRINGLIT_371 = N.strLitEscape("current");
	static java_lang_String STRINGLIT_372 = N.strLitEscape("Ljava/lang/Runtime;");
	static java_lang_String STRINGLIT_373 = N.strLitEscape("val\$bytes");
	static java_lang_String STRINGLIT_374 = N.strLitEscape("val\$srcI");
	static java_lang_String STRINGLIT_375 = N.strLitEscape("val\$dstI");
	static java_lang_String STRINGLIT_376 = N.strLitEscape("val\$iarray");
	static java_lang_String STRINGLIT_377 = N.strLitEscape("val\$carray");
	static java_lang_String STRINGLIT_378 = N.strLitEscape("[C");
	static java_lang_String STRINGLIT_379 = N.strLitEscape("val\$sarray");
	static java_lang_String STRINGLIT_380 = N.strLitEscape("[S");
	static java_lang_String STRINGLIT_381 = N.strLitEscape("val\$barray");
	static java_lang_String STRINGLIT_382 = N.strLitEscape("val\$darray");
	static java_lang_String STRINGLIT_383 = N.strLitEscape("[D");
	static java_lang_String STRINGLIT_384 = N.strLitEscape("val\$farray");
	static java_lang_String STRINGLIT_385 = N.strLitEscape("[F");
	static java_lang_String STRINGLIT_386 = N.strLitEscape("impl");
	static java_lang_String STRINGLIT_387 = N.strLitEscape("Lcom/jtransc/time/JTranscClock\$Impl;");
	static java_lang_String STRINGLIT_388 = N.strLitEscape("parent");
	static java_lang_String STRINGLIT_389 = N.strLitEscape("max");
	static java_lang_String STRINGLIT_390 = N.strLitEscape("min");
	static java_lang_String STRINGLIT_391 = N.strLitEscape("avg");
	static java_lang_String STRINGLIT_392 = N.strLitEscape("names");
	static java_lang_String STRINGLIT_393 = N.strLitEscape("charsets");
	static java_lang_String STRINGLIT_394 = N.strLitEscape("Lcom/jtransc/ds/FastStringMap<Lcom/jtransc/charset/JTranscCharset;>;");
	static java_lang_String STRINGLIT_395 = N.strLitEscape("loadedCharsets");
	static java_lang_String STRINGLIT_396 = N.strLitEscape("count");
	static java_lang_String STRINGLIT_397 = N.strLitEscape("buf");
	static java_lang_String STRINGLIT_398 = N.strLitEscape("charsetName");
	static java_lang_String STRINGLIT_399 = N.strLitEscape("service");
	static java_lang_String STRINGLIT_400 = N.strLitEscape("Ljava/lang/Class<TS;>;");
	static java_lang_String STRINGLIT_401 = N.strLitEscape("list");
	static java_lang_String STRINGLIT_402 = N.strLitEscape("Ljava/util/List;");
	static java_lang_String STRINGLIT_403 = N.strLitEscape("Ljava/util/List<TS;>;");
	static java_lang_String STRINGLIT_404 = N.strLitEscape("invalidChar");
	static java_lang_String STRINGLIT_405 = N.strLitEscape("decode");
	static java_lang_String STRINGLIT_406 = N.strLitEscape("encode");
	static java_lang_String STRINGLIT_407 = N.strLitEscape("Ljava/util/Map;");
	static java_lang_String STRINGLIT_408 = N.strLitEscape("Ljava/util/Map<Ljava/lang/Character;Ljava/lang/Byte;>;");
	static java_lang_String STRINGLIT_409 = N.strLitEscape("EMPTY_SET");
	static java_lang_String STRINGLIT_410 = N.strLitEscape("EMPTY_ITERATOR");
	static java_lang_String STRINGLIT_411 = N.strLitEscape("Ljava/util/Iterator;");
	static java_lang_String STRINGLIT_412 = N.strLitEscape("Ljava/util/Iterator<\u002a>;");
	static java_lang_String STRINGLIT_413 = N.strLitEscape("EMPTY_ENUMERATION");
	static java_lang_String STRINGLIT_414 = N.strLitEscape("Ljava/util/Enumeration;");
	static java_lang_String STRINGLIT_415 = N.strLitEscape("Ljava/util/Enumeration<\u002a>;");
	static java_lang_String STRINGLIT_416 = N.strLitEscape("EMPTY_MAP");
	static java_lang_String STRINGLIT_417 = N.strLitEscape("EMPTY_LIST");
	static java_lang_String STRINGLIT_418 = N.strLitEscape("littleEndian");
	static java_lang_String STRINGLIT_419 = N.strLitEscape("creator");
	static java_lang_String STRINGLIT_420 = N.strLitEscape("Lcom/jtransc/mix/JTranscProcessMulti\$Creator;");
	static java_lang_String STRINGLIT_421 = N.strLitEscape("group");
	static java_lang_String STRINGLIT_422 = N.strLitEscape("Ljava/lang/ThreadGroup;");
	static java_lang_String STRINGLIT_423 = N.strLitEscape("_currentThread");
	static java_lang_String STRINGLIT_424 = N.strLitEscape("Ljava/lang/Thread;");
	static java_lang_String STRINGLIT_425 = N.strLitEscape("classLoader");
	static java_lang_String STRINGLIT_426 = N.strLitEscape("Ljava/lang/ClassLoader;");
	static java_lang_String STRINGLIT_427 = N.strLitEscape("nativeLibs");
	static java_lang_String STRINGLIT_428 = N.strLitEscape("Ljava/util/ArrayList<Ljava/lang/ClassLoader\$NativeLib;>;");
	static java_lang_String STRINGLIT_429 = N.strLitEscape("EMPTY_BYTE");
	static java_lang_String STRINGLIT_430 = N.strLitEscape("EMPTY_CLASS");
	static java_lang_String STRINGLIT_431 = N.strLitEscape("_elementSizeShift");
	static java_lang_String STRINGLIT_432 = N.strLitEscape("mark");
	static java_lang_String STRINGLIT_433 = N.strLitEscape("block");
	static java_lang_String STRINGLIT_434 = N.strLitEscape("Ljava/nio/internal/MemoryBlock;");
	static java_lang_String STRINGLIT_435 = N.strLitEscape("position");
	static java_lang_String STRINGLIT_436 = N.strLitEscape("limit");
	static java_lang_String STRINGLIT_437 = N.strLitEscape("capacity");
	static java_lang_String STRINGLIT_438 = N.strLitEscape("arrayOffset");
	static java_lang_String STRINGLIT_439 = N.strLitEscape("backingArray");
	static java_lang_String STRINGLIT_440 = N.strLitEscape("isReadOnly");
	static java_lang_String STRINGLIT_441 = N.strLitEscape("isLittleEndian");
	static java_lang_String STRINGLIT_442 = N.strLitEscape("isNativeOrder");
	static java_lang_String STRINGLIT_443 = N.strLitEscape("order");
	static java_lang_String STRINGLIT_444 = N.strLitEscape("Ljava/nio/ByteOrder;");
	static java_lang_String STRINGLIT_445 = N.strLitEscape("isDirect");
	static java_lang_String STRINGLIT_446 = N.strLitEscape("byteBuffer");
	static java_lang_String STRINGLIT_447 = N.strLitEscape("Ljava/nio/ByteBuffer;");
	static java_lang_String STRINGLIT_448 = N.strLitEscape("bytes");
	static java_lang_String STRINGLIT_449 = N.strLitEscape("needsSwap");
	static java_lang_String STRINGLIT_450 = N.strLitEscape("NATIVE_ORDER");
	static java_lang_String STRINGLIT_451 = N.strLitEscape("SWAPPED");
	static java_lang_String STRINGLIT_452 = N.strLitEscape("NATIVE");
	static java_lang_String STRINGLIT_453 = N.strLitEscape("Lcom/jtransc/compression/jzlib/CRC32;");
	static java_lang_String STRINGLIT_454 = N.strLitEscape("tbytes");
	static java_lang_String STRINGLIT_455 = N.strLitEscape("temp");
	static java_lang_String STRINGLIT_456 = N.strLitEscape("nice_length");
	static java_lang_String STRINGLIT_457 = N.strLitEscape("max_chain");
	static java_lang_String STRINGLIT_458 = N.strLitEscape("max_lazy");
	static java_lang_String STRINGLIT_459 = N.strLitEscape("good_length");
	static java_lang_String STRINGLIT_460 = N.strLitEscape("func");
	static java_lang_String STRINGLIT_461 = N.strLitEscape("Lcom/jtransc/compression/jzlib/Deflater;");
	static java_lang_String STRINGLIT_462 = N.strLitEscape("inLength");
	static java_lang_String STRINGLIT_463 = N.strLitEscape("inRead");
	static java_lang_String STRINGLIT_464 = N.strLitEscape("noHeader");
	static java_lang_String STRINGLIT_465 = N.strLitEscape("compressLevel");
	static java_lang_String STRINGLIT_466 = N.strLitEscape("streamHandle");
	static java_lang_String STRINGLIT_467 = N.strLitEscape("flushParm");
	static java_lang_String STRINGLIT_468 = N.strLitEscape("strategy");
	static java_lang_String STRINGLIT_469 = N.strLitEscape("defaults");
	static java_lang_String STRINGLIT_470 = N.strLitEscape("[Ljava/util/Hashtable\$HashtableEntry;");
	static java_lang_String STRINGLIT_471 = N.strLitEscape("[Ljava/util/Hashtable\$HashtableEntry<TK;TV;>;");
	static java_lang_String STRINGLIT_472 = N.strLitEscape("serialPersistentFields");
	static java_lang_String STRINGLIT_473 = N.strLitEscape("[Ljava/io/ObjectStreamField;");
	static java_lang_String STRINGLIT_474 = N.strLitEscape("Ljava/util/Hashtable\$HashtableEntry;");
	static java_lang_String STRINGLIT_475 = N.strLitEscape("Ljava/util/Hashtable\$HashtableEntry<TK;TV;>;");
	static java_lang_String STRINGLIT_476 = N.strLitEscape("Ljava/util/Hashtable;");
	static java_lang_String STRINGLIT_477 = N.strLitEscape("type");
	static java_lang_String STRINGLIT_478 = N.strLitEscape("referent");
	static java_lang_String STRINGLIT_479 = N.strLitEscape("TT;");
	static java_lang_String STRINGLIT_480 = N.strLitEscape("queue");
	static java_lang_String STRINGLIT_481 = N.strLitEscape("Ljava/lang/ref/ReferenceQueue;");
	static java_lang_String STRINGLIT_482 = N.strLitEscape("Ljava/lang/ref/ReferenceQueue<\u002dTT;>;");
	static java_lang_String STRINGLIT_483 = N.strLitEscape("Array.newInstance");
	static java_lang_String STRINGLIT_484 = N.strLitEscape("[");
	static java_lang_String STRINGLIT_485 = N.strLitEscape("Invalid Array of void type");
	static java_lang_String STRINGLIT_486 = N.strLitEscape("Invalid Array.newInstance with ");
	static java_lang_String STRINGLIT_487 = N.strLitEscape("UNKNOWN");
	static java_lang_String STRINGLIT_488 = N.strLitEscape("\u0028Native Method\u0029");
	static java_lang_String STRINGLIT_489 = N.strLitEscape("\u0028Unknown Source\u0029");
	static java_lang_String STRINGLIT_490 = N.strLitEscape("Benchmark");
	static java_lang_String STRINGLIT_491 = N.strLitEscape("java.lang.Object");
	static java_lang_String STRINGLIT_492 = N.strLitEscape("java.lang.String");
	static java_lang_String STRINGLIT_493 = N.strLitEscape("java.io.Serializable");
	static java_lang_String STRINGLIT_494 = N.strLitEscape("java.lang.Comparable");
	static java_lang_String STRINGLIT_495 = N.strLitEscape("java.lang.CharSequence");
	static java_lang_String STRINGLIT_496 = N.strLitEscape("java.lang.String\$CaseInsensitiveComparator");
	static java_lang_String STRINGLIT_497 = N.strLitEscape("java.util.Comparator");
	static java_lang_String STRINGLIT_498 = N.strLitEscape("java.lang.String\$1");
	static java_lang_String STRINGLIT_499 = N.strLitEscape("java.lang.StringBuilder");
	static java_lang_String STRINGLIT_500 = N.strLitEscape("java.lang.Appendable");
	static java_lang_String STRINGLIT_501 = N.strLitEscape("java.lang.Class");
	static java_lang_String STRINGLIT_502 = N.strLitEscape("java.lang.reflect.Type");
	static java_lang_String STRINGLIT_503 = N.strLitEscape("java.lang.reflect.GenericDeclaration");
	static java_lang_String STRINGLIT_504 = N.strLitEscape("java.lang.reflect.AnnotatedElement");
	static java_lang_String STRINGLIT_505 = N.strLitEscape("java.lang.AnnotatedElement");
	static java_lang_String STRINGLIT_506 = N.strLitEscape("java.lang.reflect.Modifier");
	static java_lang_String STRINGLIT_507 = N.strLitEscape("java.lang.Integer");
	static java_lang_String STRINGLIT_508 = N.strLitEscape("java.lang.Number");
	static java_lang_String STRINGLIT_509 = N.strLitEscape("java.lang.reflect.Field");
	static java_lang_String STRINGLIT_510 = N.strLitEscape("java.lang.reflect.Member");
	static java_lang_String STRINGLIT_511 = N.strLitEscape("java.lang.reflect.AccessibleObject");
	static java_lang_String STRINGLIT_512 = N.strLitEscape("java.lang.ClassNotFoundException");
	static java_lang_String STRINGLIT_513 = N.strLitEscape("java.lang.ReflectiveOperationException");
	static java_lang_String STRINGLIT_514 = N.strLitEscape("java.lang.Exception");
	static java_lang_String STRINGLIT_515 = N.strLitEscape("java.lang.Throwable");
	static java_lang_String STRINGLIT_516 = N.strLitEscape("java.lang.Void");
	static java_lang_String STRINGLIT_517 = N.strLitEscape("java.lang.Float");
	static java_lang_String STRINGLIT_518 = N.strLitEscape("com.jtransc.text.JTranscStringTools");
	static java_lang_String STRINGLIT_519 = N.strLitEscape("java.lang.Math");
	static java_lang_String STRINGLIT_520 = N.strLitEscape("java.lang.Character");
	static java_lang_String STRINGLIT_521 = N.strLitEscape("java.util.Arrays");
	static java_lang_String STRINGLIT_522 = N.strLitEscape("java.lang.System");
	static java_lang_String STRINGLIT_523 = N.strLitEscape("java.io.PrintStream");
	static java_lang_String STRINGLIT_524 = N.strLitEscape("java.io.Closeable");
	static java_lang_String STRINGLIT_525 = N.strLitEscape("java.lang.AutoCloseable");
	static java_lang_String STRINGLIT_526 = N.strLitEscape("java.io.FilterOutputStream");
	static java_lang_String STRINGLIT_527 = N.strLitEscape("java.io.OutputStream");
	static java_lang_String STRINGLIT_528 = N.strLitEscape("java.io.Flushable");
	static java_lang_String STRINGLIT_529 = N.strLitEscape("java.lang.NullPointerException");
	static java_lang_String STRINGLIT_530 = N.strLitEscape("java.lang.RuntimeException");
	static java_lang_String STRINGLIT_531 = N.strLitEscape("java.lang.System\$1");
	static java_lang_String STRINGLIT_532 = N.strLitEscape("java.io.InputStream");
	static java_lang_String STRINGLIT_533 = N.strLitEscape("com.jtransc.io.JTranscConsolePrintStream");
	static java_lang_String STRINGLIT_534 = N.strLitEscape("com.jtransc.io.JTranscConsolePrintStream\$ConsoleOutputStream");
	static java_lang_String STRINGLIT_535 = N.strLitEscape("com.jtransc.io.JTranscConsolePrintStream\$ConsoleBaseStream");
	static java_lang_String STRINGLIT_536 = N.strLitEscape("com.jtransc.io.JTranscConsolePrintStream\$ConsoleErrorStream");
	static java_lang_String STRINGLIT_537 = N.strLitEscape("java.lang.ArrayIndexOutOfBoundsException");
	static java_lang_String STRINGLIT_538 = N.strLitEscape("java.lang.IndexOutOfBoundsException");
	static java_lang_String STRINGLIT_539 = N.strLitEscape("java.lang.IllegalArgumentException");
	static java_lang_String STRINGLIT_540 = N.strLitEscape("java.lang.Double");
	static java_lang_String STRINGLIT_541 = N.strLitEscape("java.util.HashMap");
	static java_lang_String STRINGLIT_542 = N.strLitEscape("java.lang.Cloneable");
	static java_lang_String STRINGLIT_543 = N.strLitEscape("java.util.AbstractMap");
	static java_lang_String STRINGLIT_544 = N.strLitEscape("java.util.Map");
	static java_lang_String STRINGLIT_545 = N.strLitEscape("java.util.Map\$Entry");
	static java_lang_String STRINGLIT_546 = N.strLitEscape("java.util.HashMap\$HashMapEntry");
	static java_lang_String STRINGLIT_547 = N.strLitEscape("java.util.Iterator");
	static java_lang_String STRINGLIT_548 = N.strLitEscape("java.util.Set");
	static java_lang_String STRINGLIT_549 = N.strLitEscape("java.util.Collection");
	static java_lang_String STRINGLIT_550 = N.strLitEscape("java.lang.Iterable");
	static java_lang_String STRINGLIT_551 = N.strLitEscape("java.util.HashMap\$1");
	static java_lang_String STRINGLIT_552 = N.strLitEscape("java.util.HashMap\$EntrySet");
	static java_lang_String STRINGLIT_553 = N.strLitEscape("java.util.AbstractSet");
	static java_lang_String STRINGLIT_554 = N.strLitEscape("java.util.AbstractCollection");
	static java_lang_String STRINGLIT_555 = N.strLitEscape("java.util.HashMap\$EntryIterator");
	static java_lang_String STRINGLIT_556 = N.strLitEscape("java.util.HashMap\$HashIterator");
	static java_lang_String STRINGLIT_557 = N.strLitEscape("java.util.NoSuchElementException");
	static java_lang_String STRINGLIT_558 = N.strLitEscape("java.util.ConcurrentModificationException");
	static java_lang_String STRINGLIT_559 = N.strLitEscape("java.lang.Long");
	static java_lang_String STRINGLIT_560 = N.strLitEscape("com.jtransc.internal.JTranscCType");
	static java_lang_String STRINGLIT_561 = N.strLitEscape("java.lang.Short");
	static java_lang_String STRINGLIT_562 = N.strLitEscape("com.jtransc.io.JTranscConsole");
	static java_lang_String STRINGLIT_563 = N.strLitEscape("java.lang.Boolean");
	static java_lang_String STRINGLIT_564 = N.strLitEscape("java.lang.Byte");
	static java_lang_String STRINGLIT_565 = N.strLitEscape("java.lang.SystemInt");
	static java_lang_String STRINGLIT_566 = N.strLitEscape("java.lang.ClassCastException");
	static java_lang_String STRINGLIT_567 = N.strLitEscape("java.util.Objects");
	static java_lang_String STRINGLIT_568 = N.strLitEscape("java.lang.jtransc.JTranscCoreReflection");
	static java_lang_String STRINGLIT_569 = N.strLitEscape("j.ProgramReflection");
	static java_lang_String STRINGLIT_570 = N.strLitEscape("j.ClassInfo");
	static java_lang_String STRINGLIT_571 = N.strLitEscape("j.ProgramReflection\$AllClasses");
	static java_lang_String STRINGLIT_572 = N.strLitEscape("java.lang.UnsupportedOperationException");
	static java_lang_String STRINGLIT_573 = N.strLitEscape("java.lang.StackTraceElement");
	static java_lang_String STRINGLIT_574 = N.strLitEscape("java.lang.reflect.Array");
	static java_lang_String STRINGLIT_575 = N.strLitEscape("java.lang.CloneNotSupportedException");
	static java_lang_String STRINGLIT_576 = N.strLitEscape("j.ProgramReflection\$DynamicGetSet");
	static java_lang_String STRINGLIT_577 = N.strLitEscape("j.MemberInfo");
	static java_lang_String STRINGLIT_578 = N.strLitEscape("j.ProgramReflection\$AllFields");
	static java_lang_String STRINGLIT_579 = N.strLitEscape("java.lang.NoSuchMethodException");
	static java_lang_String STRINGLIT_580 = N.strLitEscape("java.lang.InstantiationException");
	static java_lang_String STRINGLIT_581 = N.strLitEscape("java.lang.reflect.Constructor");
	static java_lang_String STRINGLIT_582 = N.strLitEscape("java.lang.reflect.MethodConstructor");
	static java_lang_String STRINGLIT_583 = N.strLitEscape("java.lang.Error");
	static java_lang_String STRINGLIT_584 = N.strLitEscape("java.util.ArrayList");
	static java_lang_String STRINGLIT_585 = N.strLitEscape("java.util.List");
	static java_lang_String STRINGLIT_586 = N.strLitEscape("java.util.RandomAccess");
	static java_lang_String STRINGLIT_587 = N.strLitEscape("java.util.AbstractList");
	static java_lang_String STRINGLIT_588 = N.strLitEscape("java.util.ListIterator");
	static java_lang_String STRINGLIT_589 = N.strLitEscape("java.util.AbstractList\$FullListIterator");
	static java_lang_String STRINGLIT_590 = N.strLitEscape("java.util.AbstractList\$SimpleListIterator");
	static java_lang_String STRINGLIT_591 = N.strLitEscape("java.lang.InternalError");
	static java_lang_String STRINGLIT_592 = N.strLitEscape("java.lang.reflect.ParameterizedType");
	static java_lang_String STRINGLIT_593 = N.strLitEscape("java.lang.reflect.InvocationTargetException");
	static java_lang_String STRINGLIT_594 = N.strLitEscape("j.ProgramReflection\$DynamicNewInvoke");
	static java_lang_String STRINGLIT_595 = N.strLitEscape("j.ProgramReflection\$AllConstructors");
	static java_lang_String STRINGLIT_596 = N.strLitEscape("java.util.Arrays\$ArrayList");
	static java_lang_String STRINGLIT_597 = N.strLitEscape("java.lang.SafeVarargs");
	static java_lang_String STRINGLIT_598 = N.strLitEscape("java.lang.annotation.Annotation");
	static java_lang_String STRINGLIT_599 = N.strLitEscape("java.lang.AssertionError");
	static java_lang_String STRINGLIT_600 = N.strLitEscape("com.jtransc.async.JTranscAsyncHandler");
	static java_lang_String STRINGLIT_601 = N.strLitEscape("Benchmark\$37");
	static java_lang_String STRINGLIT_602 = N.strLitEscape("Benchmark\$Task");
	static java_lang_String STRINGLIT_603 = N.strLitEscape("Benchmark\$MyClass2");
	static java_lang_String STRINGLIT_604 = N.strLitEscape("Benchmark\$36");
	static java_lang_String STRINGLIT_605 = N.strLitEscape("Benchmark\$35");
	static java_lang_String STRINGLIT_606 = N.strLitEscape("Benchmark\$34");
	static java_lang_String STRINGLIT_607 = N.strLitEscape("com.jtransc.JTranscSystem");
	static java_lang_String STRINGLIT_608 = N.strLitEscape("Benchmark\$39");
	static java_lang_String STRINGLIT_609 = N.strLitEscape("Benchmark\$38");
	static java_lang_String STRINGLIT_610 = N.strLitEscape("java.util.Random");
	static java_lang_String STRINGLIT_611 = N.strLitEscape("Benchmark\$33");
	static java_lang_String STRINGLIT_612 = N.strLitEscape("Benchmark\$32");
	static java_lang_String STRINGLIT_613 = N.strLitEscape("Benchmark\$31");
	static java_lang_String STRINGLIT_614 = N.strLitEscape("Benchmark\$30");
	static java_lang_String STRINGLIT_615 = N.strLitEscape("java.lang.Runtime");
	static java_lang_String STRINGLIT_616 = N.strLitEscape("Benchmark\$40");
	static java_lang_String STRINGLIT_617 = N.strLitEscape("Benchmark\$1");
	static java_lang_String STRINGLIT_618 = N.strLitEscape("Benchmark\$2");
	static java_lang_String STRINGLIT_619 = N.strLitEscape("Benchmark\$3");
	static java_lang_String STRINGLIT_620 = N.strLitEscape("Benchmark\$44");
	static java_lang_String STRINGLIT_621 = N.strLitEscape("Benchmark\$4");
	static java_lang_String STRINGLIT_622 = N.strLitEscape("Benchmark\$43");
	static java_lang_String STRINGLIT_623 = N.strLitEscape("Benchmark\$5");
	static java_lang_String STRINGLIT_624 = N.strLitEscape("Benchmark\$42");
	static java_lang_String STRINGLIT_625 = N.strLitEscape("Benchmark\$6");
	static java_lang_String STRINGLIT_626 = N.strLitEscape("Benchmark\$41");
	static java_lang_String STRINGLIT_627 = N.strLitEscape("Benchmark\$7");
	static java_lang_String STRINGLIT_628 = N.strLitEscape("Benchmark\$8");
	static java_lang_String STRINGLIT_629 = N.strLitEscape("Benchmark\$9");
	static java_lang_String STRINGLIT_630 = N.strLitEscape("Benchmark\$15");
	static java_lang_String STRINGLIT_631 = N.strLitEscape("Benchmark\$14");
	static java_lang_String STRINGLIT_632 = N.strLitEscape("Benchmark\$13");
	static java_lang_String STRINGLIT_633 = N.strLitEscape("Benchmark\$12");
	static java_lang_String STRINGLIT_634 = N.strLitEscape("Benchmark\$19");
	static java_lang_String STRINGLIT_635 = N.strLitEscape("Benchmark\$18");
	static java_lang_String STRINGLIT_636 = N.strLitEscape("Benchmark\$17");
	static java_lang_String STRINGLIT_637 = N.strLitEscape("Benchmark\$16");
	static java_lang_String STRINGLIT_638 = N.strLitEscape("Benchmark\$11");
	static java_lang_String STRINGLIT_639 = N.strLitEscape("Benchmark\$10");
	static java_lang_String STRINGLIT_640 = N.strLitEscape("Benchmark\$26");
	static java_lang_String STRINGLIT_641 = N.strLitEscape("Benchmark\$25");
	static java_lang_String STRINGLIT_642 = N.strLitEscape("Benchmark\$24");
	static java_lang_String STRINGLIT_643 = N.strLitEscape("Benchmark\$23");
	static java_lang_String STRINGLIT_644 = N.strLitEscape("Benchmark\$29");
	static java_lang_String STRINGLIT_645 = N.strLitEscape("Benchmark\$28");
	static java_lang_String STRINGLIT_646 = N.strLitEscape("Benchmark\$27");
	static java_lang_String STRINGLIT_647 = N.strLitEscape("Benchmark\$22");
	static java_lang_String STRINGLIT_648 = N.strLitEscape("Benchmark\$21");
	static java_lang_String STRINGLIT_649 = N.strLitEscape("Benchmark\$20");
	static java_lang_String STRINGLIT_650 = N.strLitEscape("com.jtransc.JTranscVersion");
	static java_lang_String STRINGLIT_651 = N.strLitEscape("com.jtransc.time.JTranscClock");
	static java_lang_String STRINGLIT_652 = N.strLitEscape("com.jtransc.time.JTranscClock\$1");
	static java_lang_String STRINGLIT_653 = N.strLitEscape("com.jtransc.time.JTranscClock\$Impl");
	static java_lang_String STRINGLIT_654 = N.strLitEscape("java.io.IOException");
	static java_lang_String STRINGLIT_655 = N.strLitEscape("java.io.ByteArrayOutputStream");
	static java_lang_String STRINGLIT_656 = N.strLitEscape("java.nio.charset.UnsupportedCharsetException");
	static java_lang_String STRINGLIT_657 = N.strLitEscape("java.util.ServiceLoader");
	static java_lang_String STRINGLIT_658 = N.strLitEscape("com.jtransc.charset.charsets.JTranscCharsetIBM866");
	static java_lang_String STRINGLIT_659 = N.strLitEscape("com.jtransc.charset.JTranscCharsetSingleByte");
	static java_lang_String STRINGLIT_660 = N.strLitEscape("java.util.Collections");
	static java_lang_String STRINGLIT_661 = N.strLitEscape("java.util.Collections\$1");
	static java_lang_String STRINGLIT_662 = N.strLitEscape("java.util.Collections\$EmptyList");
	static java_lang_String STRINGLIT_663 = N.strLitEscape("java.util.Collections\$2");
	static java_lang_String STRINGLIT_664 = N.strLitEscape("java.util.Enumeration");
	static java_lang_String STRINGLIT_665 = N.strLitEscape("java.util.Collections\$EmptyMap");
	static java_lang_String STRINGLIT_666 = N.strLitEscape("java.util.Collections\$EmptySet");
	static java_lang_String STRINGLIT_667 = N.strLitEscape("com.jtransc.charset.charsets.JTranscCharsetUTF8");
	static java_lang_String STRINGLIT_668 = N.strLitEscape("com.jtransc.charset.charsets.JTranscCharsetUSASCII");
	static java_lang_String STRINGLIT_669 = N.strLitEscape("com.jtransc.charset.charsets.JTranscCharsetUTF16LE");
	static java_lang_String STRINGLIT_670 = N.strLitEscape("com.jtransc.charset.charsets.JTranscCharsetUTF16Base");
	static java_lang_String STRINGLIT_671 = N.strLitEscape("com.jtransc.mix.JTranscProcessMulti");
	static java_lang_String STRINGLIT_672 = N.strLitEscape("java.lang.Process");
	static java_lang_String STRINGLIT_673 = N.strLitEscape("com.jtransc.mix.JTranscProcessMulti\$Creator");
	static java_lang_String STRINGLIT_674 = N.strLitEscape("com.jtransc.charset.charsets.JTranscCharsetLatin1");
	static java_lang_String STRINGLIT_675 = N.strLitEscape("com.jtransc.charset.charsets.JTranscCharsetUTF16BE");
	static java_lang_String STRINGLIT_676 = N.strLitEscape("java.lang.Thread");
	static java_lang_String STRINGLIT_677 = N.strLitEscape("java.lang.Runnable");
	static java_lang_String STRINGLIT_678 = N.strLitEscape("java.lang.ThreadGroup");
	static java_lang_String STRINGLIT_679 = N.strLitEscape("java.lang.Thread\$UncaughtExceptionHandler");
	static java_lang_String STRINGLIT_680 = N.strLitEscape("java.lang.ClassLoader");
	static java_lang_String STRINGLIT_681 = N.strLitEscape("java.lang._ClassInternalUtils");
	static java_lang_String STRINGLIT_682 = N.strLitEscape("java.lang._ClassInternalUtils\$1");
	static java_lang_String STRINGLIT_683 = N.strLitEscape("com.jtransc.JTranscArrays");
	static java_lang_String STRINGLIT_684 = N.strLitEscape("com.jtransc.JTranscSystemProperties");
	static java_lang_String STRINGLIT_685 = N.strLitEscape("Benchmark\$MyClass");
	static java_lang_String STRINGLIT_686 = N.strLitEscape("java.nio.IntBuffer");
	static java_lang_String STRINGLIT_687 = N.strLitEscape("java.nio.Buffer");
	static java_lang_String STRINGLIT_688 = N.strLitEscape("java.nio.internal.MemoryBlock");
	static java_lang_String STRINGLIT_689 = N.strLitEscape("java.nio.FloatBuffer");
	static java_lang_String STRINGLIT_690 = N.strLitEscape("java.nio.ByteBuffer");
	static java_lang_String STRINGLIT_691 = N.strLitEscape("java.nio.ReadOnlyBufferException");
	static java_lang_String STRINGLIT_692 = N.strLitEscape("java.nio.CharBuffer");
	static java_lang_String STRINGLIT_693 = N.strLitEscape("java.lang.Readable");
	static java_lang_String STRINGLIT_694 = N.strLitEscape("java.nio.DoubleBuffer");
	static java_lang_String STRINGLIT_695 = N.strLitEscape("java.nio.ShortBuffer");
	static java_lang_String STRINGLIT_696 = N.strLitEscape("java.nio.LongBuffer");
	static java_lang_String STRINGLIT_697 = N.strLitEscape("java.nio.ByteBufferAsFloatBuffer");
	static java_lang_String STRINGLIT_698 = N.strLitEscape("java.nio.internal.ByteBufferAs");
	static java_lang_String STRINGLIT_699 = N.strLitEscape("java.nio.ByteOrder");
	static java_lang_String STRINGLIT_700 = N.strLitEscape("java.nio.ByteBufferAsFloatBuffer\$BE");
	static java_lang_String STRINGLIT_701 = N.strLitEscape("java.nio.ByteBufferAsFloatBuffer\$LE");
	static java_lang_String STRINGLIT_702 = N.strLitEscape("java.nio.ByteBufferAsLongBuffer");
	static java_lang_String STRINGLIT_703 = N.strLitEscape("java.nio.ByteBufferAsLongBuffer\$LE");
	static java_lang_String STRINGLIT_704 = N.strLitEscape("java.nio.ByteBufferAsLongBuffer\$1");
	static java_lang_String STRINGLIT_705 = N.strLitEscape("java.nio.ByteBufferAsLongBuffer\$BE");
	static java_lang_String STRINGLIT_706 = N.strLitEscape("java.nio.ByteBufferAsDoubleBuffer");
	static java_lang_String STRINGLIT_707 = N.strLitEscape("java.nio.ByteBufferAsDoubleBuffer\$LE");
	static java_lang_String STRINGLIT_708 = N.strLitEscape("java.nio.ByteBufferAsDoubleBuffer\$1");
	static java_lang_String STRINGLIT_709 = N.strLitEscape("java.nio.ByteBufferAsDoubleBuffer\$BE");
	static java_lang_String STRINGLIT_710 = N.strLitEscape("libcore.io.Memory");
	static java_lang_String STRINGLIT_711 = N.strLitEscape("java.nio.ByteBufferAsCharBuffer");
	static java_lang_String STRINGLIT_712 = N.strLitEscape("java.nio.ByteBufferAsCharBuffer\$LE");
	static java_lang_String STRINGLIT_713 = N.strLitEscape("java.nio.ByteBufferAsCharBuffer\$1");
	static java_lang_String STRINGLIT_714 = N.strLitEscape("java.nio.ByteBufferAsCharBuffer\$BE");
	static java_lang_String STRINGLIT_715 = N.strLitEscape("java.nio.ByteBufferAsShortBuffer");
	static java_lang_String STRINGLIT_716 = N.strLitEscape("java.nio.ByteBufferAsShortBuffer\$LE");
	static java_lang_String STRINGLIT_717 = N.strLitEscape("java.nio.ByteBufferAsShortBuffer\$1");
	static java_lang_String STRINGLIT_718 = N.strLitEscape("java.nio.ByteBufferAsShortBuffer\$BE");
	static java_lang_String STRINGLIT_719 = N.strLitEscape("java.nio.ByteBufferAsIntBuffer");
	static java_lang_String STRINGLIT_720 = N.strLitEscape("java.nio.ByteBufferAsIntBuffer\$BE");
	static java_lang_String STRINGLIT_721 = N.strLitEscape("java.nio.ByteBufferAsIntBuffer\$LE");
	static java_lang_String STRINGLIT_722 = N.strLitEscape("java.util.zip.CRC32");
	static java_lang_String STRINGLIT_723 = N.strLitEscape("java.util.zip.Checksum");
	static java_lang_String STRINGLIT_724 = N.strLitEscape("com.jtransc.compression.jzlib.Deflate\$Config");
	static java_lang_String STRINGLIT_725 = N.strLitEscape("java.util.zip.Deflater");
	static java_lang_String STRINGLIT_726 = N.strLitEscape("Benchmark\$Test1");
	static java_lang_String STRINGLIT_727 = N.strLitEscape("Benchmark\$Test2");
	static java_lang_String STRINGLIT_728 = N.strLitEscape("java.util.Properties");
	static java_lang_String STRINGLIT_729 = N.strLitEscape("java.util.Hashtable");
	static java_lang_String STRINGLIT_730 = N.strLitEscape("java.util.Dictionary");
	static java_lang_String STRINGLIT_731 = N.strLitEscape("java.util.Hashtable\$HashtableEntry");
	static java_lang_String STRINGLIT_732 = N.strLitEscape("java.util.Hashtable\$1");
	static java_lang_String STRINGLIT_733 = N.strLitEscape("java.util.Hashtable\$EntrySet");
	static java_lang_String STRINGLIT_734 = N.strLitEscape("java.util.Hashtable\$EntryIterator");
	static java_lang_String STRINGLIT_735 = N.strLitEscape("java.util.Hashtable\$HashIterator");
	static java_lang_String STRINGLIT_736 = N.strLitEscape("java.io.ObjectStreamField");
	static java_lang_String STRINGLIT_737 = N.strLitEscape("java.lang.ref.WeakReference");
	static java_lang_String STRINGLIT_738 = N.strLitEscape("java.lang.ref.Reference");
	static java_lang_String STRINGLIT_739 = N.strLitEscape("java.lang.ref.ReferenceQueue");
	static java_lang_String STRINGLIT_740 = N.strLitEscape("java.lang.SafeVarargs\$Impl");
	static java_lang_String STRINGLIT_741 = N.strLitEscape("java.lang.annotation.Annotation\$Impl");
	static java_lang_String STRINGLIT_742 = N.strLitEscape("byte");
	static java_lang_String STRINGLIT_743 = N.strLitEscape("boolean");
	static java_lang_String STRINGLIT_744 = N.strLitEscape("true");
	static java_lang_String STRINGLIT_745 = N.strLitEscape("false");
	static java_lang_String STRINGLIT_746 = N.strLitEscape("short");
	static java_lang_String STRINGLIT_747 = N.strLitEscape("long");
	static java_lang_String STRINGLIT_748 = N.strLitEscape("0");
	static java_lang_String STRINGLIT_749 = N.strLitEscape("\u002d9223372036854775808");
	static java_lang_String STRINGLIT_750 = N.strLitEscape("\u002d");
	static java_lang_String STRINGLIT_751 = N.strLitEscape("Capacity: ");
	static java_lang_String STRINGLIT_752 = N.strLitEscape("double");
	static java_lang_String STRINGLIT_753 = N.strLitEscape("out == null");
	static java_lang_String STRINGLIT_754 = N.strLitEscape("os.name");
	static java_lang_String STRINGLIT_755 = N.strLitEscape("os.version");
	static java_lang_String STRINGLIT_756 = N.strLitEscape("0.1");
	static java_lang_String STRINGLIT_757 = N.strLitEscape("java.runtime.name");
	static java_lang_String STRINGLIT_758 = N.strLitEscape("jtransc\u002dunknown");
	static java_lang_String STRINGLIT_759 = N.strLitEscape("java.version");
	static java_lang_String STRINGLIT_760 = N.strLitEscape("1.8.0_51");
	static java_lang_String STRINGLIT_761 = N.strLitEscape("java.vm.version");
	static java_lang_String STRINGLIT_762 = N.strLitEscape("25.51\u002db03");
	static java_lang_String STRINGLIT_763 = N.strLitEscape("java.runtime.version");
	static java_lang_String STRINGLIT_764 = N.strLitEscape("1.8.0_51\u002db16");
	static java_lang_String STRINGLIT_765 = N.strLitEscape("file.separator");
	static java_lang_String STRINGLIT_766 = N.strLitEscape("line.separator");
	static java_lang_String STRINGLIT_767 = N.strLitEscape("path.separator");
	static java_lang_String STRINGLIT_768 = N.strLitEscape("file.encoding");
	static java_lang_String STRINGLIT_769 = N.strLitEscape("java.specification.name");
	static java_lang_String STRINGLIT_770 = N.strLitEscape("java.specification.vendor");
	static java_lang_String STRINGLIT_771 = N.strLitEscape("jtransc");
	static java_lang_String STRINGLIT_772 = N.strLitEscape("java.specification.version");
	static java_lang_String STRINGLIT_773 = N.strLitEscape("1.7");
	static java_lang_String STRINGLIT_774 = N.strLitEscape("java.vendor");
	static java_lang_String STRINGLIT_775 = N.strLitEscape("java.vendor.url");
	static java_lang_String STRINGLIT_776 = N.strLitEscape("http://github.com/jtransc/jtransc");
	static java_lang_String STRINGLIT_777 = N.strLitEscape("java.vm.name");
	static java_lang_String STRINGLIT_778 = N.strLitEscape("java.vm.specification.name");
	static java_lang_String STRINGLIT_779 = N.strLitEscape("Jtransc JVM emulator");
	static java_lang_String STRINGLIT_780 = N.strLitEscape("java.vm.specification.vendor");
	static java_lang_String STRINGLIT_781 = N.strLitEscape("java.vm.specification.version");
	static java_lang_String STRINGLIT_782 = N.strLitEscape("java.io.tmpdir");
	static java_lang_String STRINGLIT_783 = N.strLitEscape("user.home");
	static java_lang_String STRINGLIT_784 = N.strLitEscape("user.dir");
	static java_lang_String STRINGLIT_785 = N.strLitEscape("user.name");
	static java_lang_String STRINGLIT_786 = N.strLitEscape("user.language");
	static java_lang_String STRINGLIT_787 = N.strLitEscape("user.region");
	static java_lang_String STRINGLIT_788 = N.strLitEscape("user.variant");
	static java_lang_String STRINGLIT_789 = N.strLitEscape("char");
	static java_lang_String STRINGLIT_790 = N.strLitEscape("NaN");
	static java_lang_String STRINGLIT_791 = N.strLitEscape("\u002dInfinity");
	static java_lang_String STRINGLIT_792 = N.strLitEscape("Infinity");
	static java_lang_String STRINGLIT_793 = N.strLitEscape("\u002d0.0");
	static java_lang_String STRINGLIT_794 = N.strLitEscape("e\u002b");
	static java_lang_String STRINGLIT_795 = N.strLitEscape("E");
	static java_lang_String STRINGLIT_796 = N.strLitEscape("e\u002d");
	static java_lang_String STRINGLIT_797 = N.strLitEscape("E\u002d");
	static java_lang_String STRINGLIT_798 = N.strLitEscape(".0");
	static java_lang_String STRINGLIT_799 = N.strLitEscape("float");
	static java_lang_String STRINGLIT_800 = N.strLitEscape("void");
	static java_lang_String STRINGLIT_801 = N.strLitEscape("\' of ");
	static java_lang_String STRINGLIT_802 = N.strLitEscape("Can\'t parse type \'");
	static java_lang_String STRINGLIT_803 = N.strLitEscape("\'");
	static java_lang_String STRINGLIT_804 = N.strLitEscape("Class_forName0: Can\'t find class \'");
	static java_lang_String STRINGLIT_805 = N.strLitEscape("int");
	static java_lang_String STRINGLIT_806 = N.strLitEscape("public ");
	static java_lang_String STRINGLIT_807 = N.strLitEscape("protected ");
	static java_lang_String STRINGLIT_808 = N.strLitEscape("private ");
	static java_lang_String STRINGLIT_809 = N.strLitEscape("abstract ");
	static java_lang_String STRINGLIT_810 = N.strLitEscape("static ");
	static java_lang_String STRINGLIT_811 = N.strLitEscape("final ");
	static java_lang_String STRINGLIT_812 = N.strLitEscape("transient ");
	static java_lang_String STRINGLIT_813 = N.strLitEscape("volatile ");
	static java_lang_String STRINGLIT_814 = N.strLitEscape("synchronized ");
	static java_lang_String STRINGLIT_815 = N.strLitEscape("native ");
	static java_lang_String STRINGLIT_816 = N.strLitEscape("strictfp ");
	static java_lang_String STRINGLIT_817 = N.strLitEscape("interface ");
	static java_lang_String STRINGLIT_818 = N.strLitEscape("class ");
	static java_lang_String STRINGLIT_819 = N.strLitEscape("L");
	static java_lang_String STRINGLIT_820 = N.strLitEscape(";");
	static java_lang_String STRINGLIT_821 = N.strLitEscape("Couldn\'t find class ");
	static java_lang_String STRINGLIT_822 = N.strLitEscape("Class constructor: Can\'t find class \'");
	static java_lang_String STRINGLIT_823 = N.strLitEscape(" with parameters ");
	static java_lang_String STRINGLIT_824 = N.strLitEscape("Can\'t find constructor of class ");
	static java_lang_String STRINGLIT_825 = N.strLitEscape("null");
	static java_lang_String STRINGLIT_826 = N.strLitEscape(" \u002d ");
	static java_lang_String STRINGLIT_827 = N.strLitEscape("JTransc ");
	static java_lang_String STRINGLIT_828 = N.strLitEscape("Java ");
	static java_lang_String STRINGLIT_829 = N.strLitEscape(", totalMemory: ");
	static java_lang_String STRINGLIT_830 = N.strLitEscape(", maxMemory: ");
	static java_lang_String STRINGLIT_831 = N.strLitEscape("freeMemory: ");
	static java_lang_String STRINGLIT_832 = N.strLitEscape("Benchmarking:");
	static java_lang_String STRINGLIT_833 = N.strLitEscape("plain loops");
	static java_lang_String STRINGLIT_834 = N.strLitEscape("shift left constant");
	static java_lang_String STRINGLIT_835 = N.strLitEscape("shift right constant");
	static java_lang_String STRINGLIT_836 = N.strLitEscape("shift unsigned right constant");
	static java_lang_String STRINGLIT_837 = N.strLitEscape("shift left constant long");
	static java_lang_String STRINGLIT_838 = N.strLitEscape("shift right constant long");
	static java_lang_String STRINGLIT_839 = N.strLitEscape("shift unsigned right constant long");
	static java_lang_String STRINGLIT_840 = N.strLitEscape("left shift");
	static java_lang_String STRINGLIT_841 = N.strLitEscape("right shift");
	static java_lang_String STRINGLIT_842 = N.strLitEscape("right unsigned shift");
	static java_lang_String STRINGLIT_843 = N.strLitEscape("call static mult");
	static java_lang_String STRINGLIT_844 = N.strLitEscape("call instance mult");
	static java_lang_String STRINGLIT_845 = N.strLitEscape("call instance div");
	static java_lang_String STRINGLIT_846 = N.strLitEscape("instanceof classes");
	static java_lang_String STRINGLIT_847 = N.strLitEscape("arraycopy int");
	static java_lang_String STRINGLIT_848 = N.strLitEscape("write byte[]");
	static java_lang_String STRINGLIT_849 = N.strLitEscape("write short[]");
	static java_lang_String STRINGLIT_850 = N.strLitEscape("write char[]");
	static java_lang_String STRINGLIT_851 = N.strLitEscape("write int[]");
	static java_lang_String STRINGLIT_852 = N.strLitEscape("write float[]");
	static java_lang_String STRINGLIT_853 = N.strLitEscape("write double[]");
	static java_lang_String STRINGLIT_854 = N.strLitEscape("String Builder 1");
	static java_lang_String STRINGLIT_855 = N.strLitEscape("String Builder 2");
	static java_lang_String STRINGLIT_856 = N.strLitEscape("long arithmetic");
	static java_lang_String STRINGLIT_857 = N.strLitEscape("simd mutable");
	static java_lang_String STRINGLIT_858 = N.strLitEscape("simd immutable");
	static java_lang_String STRINGLIT_859 = N.strLitEscape("simd mutable matrix mult");
	static java_lang_String STRINGLIT_860 = N.strLitEscape("StringBuilder1");
	static java_lang_String STRINGLIT_861 = N.strLitEscape("StringBuilder2");
	static java_lang_String STRINGLIT_862 = N.strLitEscape("Non Direct Buffer");
	static java_lang_String STRINGLIT_863 = N.strLitEscape("Direct Buffer Int/float");
	static java_lang_String STRINGLIT_864 = N.strLitEscape("Direct Buffer Short/Char");
	static java_lang_String STRINGLIT_865 = N.strLitEscape("Direct Buffer Double/Long");
	static java_lang_String STRINGLIT_866 = N.strLitEscape("FastMemory");
	static java_lang_String STRINGLIT_867 = N.strLitEscape("Create Instances1 local");
	static java_lang_String STRINGLIT_868 = N.strLitEscape("Create Instances2 local");
	static java_lang_String STRINGLIT_869 = N.strLitEscape("Create Instances2 global");
	static java_lang_String STRINGLIT_870 = N.strLitEscape("Create Instances with builder");
	static java_lang_String STRINGLIT_871 = N.strLitEscape("Java\'s CRC32");
	static java_lang_String STRINGLIT_872 = N.strLitEscape("jzlib\'s CRC32");
	static java_lang_String STRINGLIT_873 = N.strLitEscape("compress java\'s Deflate");
	static java_lang_String STRINGLIT_874 = N.strLitEscape("compress jzlib");
	static java_lang_String STRINGLIT_875 = N.strLitEscape("random");
	static java_lang_String STRINGLIT_876 = N.strLitEscape("exception");
	static java_lang_String STRINGLIT_877 = N.strLitEscape("TOTAL time: ");
	static java_lang_String STRINGLIT_878 = N.strLitEscape("...");
	static java_lang_String STRINGLIT_879 = N.strLitEscape("ERROR");
	static void Main(List<String> args) {
		N.init();
		java_lang_Object.SI();
		Benchmark.SI();
		java_lang_String_CaseInsensitiveComparator.SI();
		java_lang_String.SI();
		java_io_Serializable_IFields.SI();
		java_lang_Comparable_IFields.SI();
		java_lang_CharSequence_IFields.SI();
		java_util_Comparator_IFields.SI();
		java_lang_String_1.SI();
		java_lang_StringBuilder.SI();
		java_lang_Appendable_IFields.SI();
		java_lang_Class.SI();
		java_lang_reflect_Type_IFields.SI();
		java_lang_reflect_GenericDeclaration_IFields.SI();
		java_lang_reflect_AnnotatedElement_IFields.SI();
		java_lang_AnnotatedElement_IFields.SI();
		java_lang_reflect_Modifier.SI();
		java_lang_Number.SI();
		java_lang_Integer.SI();
		java_lang_reflect_AccessibleObject.SI();
		java_lang_reflect_Field.SI();
		java_lang_reflect_Member_IFields.SI();
		java_lang_reflect__InternalUtils.SI();
		java_lang_Throwable.SI();
		java_lang_Exception.SI();
		java_lang_ReflectiveOperationException.SI();
		java_lang_ClassNotFoundException.SI();
		java_lang_Void.SI();
		java_lang_Float.SI();
		com_jtransc_text_JTranscStringTools.SI();
		java_lang_Math.SI();
		java_lang_Character.SI();
		java_util_Arrays.SI();
		java_io_InputStream.SI();
		java_lang_System_1.SI();
		java_io_OutputStream.SI();
		com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream.SI();
		com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream.SI();
		com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream.SI();
		java_io_FilterOutputStream.SI();
		java_lang_RuntimeException.SI();
		java_lang_NullPointerException.SI();
		java_io_PrintStream.SI();
		com_jtransc_io_JTranscConsolePrintStream.SI();
		java_lang_System.SI();
		java_io_Closeable_IFields.SI();
		java_lang_AutoCloseable_IFields.SI();
		java_io_Flushable_IFields.SI();
		java_lang_IndexOutOfBoundsException.SI();
		java_lang_ArrayIndexOutOfBoundsException.SI();
		java_lang_IllegalArgumentException.SI();
		java_lang_Double.SI();
		com_jtransc_ds_FastStringMap.SI();
		java_util_AbstractMap.SI();
		java_util_HashMap.SI();
		java_lang_Cloneable_IFields.SI();
		java_util_Map_IFields.SI();
		java_util_Map_Entry_IFields.SI();
		java_util_HashMap_HashMapEntry.SI();
		java_util_Iterator_IFields.SI();
		java_util_Set_IFields.SI();
		java_util_Collection_IFields.SI();
		java_lang_Iterable_IFields.SI();
		java_util_HashMap_1.SI();
		java_util_AbstractCollection.SI();
		java_util_AbstractSet.SI();
		java_util_HashMap_EntrySet.SI();
		java_util_HashMap_HashIterator.SI();
		java_util_HashMap_EntryIterator.SI();
		java_util_NoSuchElementException.SI();
		java_util_ConcurrentModificationException.SI();
		java_lang_Long.SI();
		com_jtransc_internal_JTranscCType.SI();
		java_lang_Short.SI();
		com_jtransc_io_JTranscConsole.SI();
		java_lang_Boolean.SI();
		java_lang_Byte.SI();
		java_lang_SystemInt.SI();
		java_lang_ClassCastException.SI();
		java_util_Objects.SI();
		java_lang_jtransc_JTranscCoreReflection.SI();
		j_ProgramReflection.SI();
		j_ClassInfo.SI();
		j_ProgramReflection_AllClasses.SI();
		java_lang_UnsupportedOperationException.SI();
		java_lang_StackTraceElement.SI();
		java_lang_reflect_Array.SI();
		java_lang_CloneNotSupportedException.SI();
		j_ProgramReflection_DynamicGetSet.SI();
		j_MemberInfo.SI();
		j_ProgramReflection_AllFields.SI();
		java_lang_NoSuchMethodException.SI();
		java_lang_InstantiationException.SI();
		java_lang_reflect_MethodConstructor.SI();
		java_lang_reflect_Constructor.SI();
		java_lang_reflect_MethodTypeImpl.SI();
		com_jtransc_text_MStringReader.SI();
		java_lang_Error.SI();
		java_lang_reflect_ArrayType.SI();
		java_util_AbstractList.SI();
		java_util_ArrayList.SI();
		java_util_List_IFields.SI();
		java_util_RandomAccess_IFields.SI();
		java_util_ListIterator_IFields.SI();
		java_util_AbstractList_SimpleListIterator.SI();
		java_util_AbstractList_FullListIterator.SI();
		java_lang_InternalError.SI();
		java_lang_reflect_ParameterizedTypeImpl.SI();
		java_lang_reflect_ParameterizedType_IFields.SI();
		java_lang_reflect_InvocationTargetException.SI();
		j_ProgramReflection_DynamicNewInvoke.SI();
		j_ProgramReflection_AllConstructors.SI();
		java_util_Arrays_ArrayList.SI();
		java_lang_SafeVarargs_IFields.SI();
		java_lang_annotation_Annotation_IFields.SI();
		java_lang_AssertionError.SI();
		com_jtransc_async_JTranscAsyncHandler_IFields.SI();
		Benchmark_37.SI();
		Benchmark_Task_IFields.SI();
		Benchmark_MyClass2.SI();
		Benchmark_36.SI();
		Benchmark_35.SI();
		Benchmark_34.SI();
		com_jtransc_JTranscSystem.SI();
		Benchmark_39.SI();
		Benchmark_38.SI();
		java_util_Random.SI();
		Benchmark_33.SI();
		Benchmark_32.SI();
		Benchmark_31.SI();
		Benchmark_30.SI();
		java_lang_Runtime.SI();
		Benchmark_40.SI();
		Benchmark_1.SI();
		Benchmark_2.SI();
		Benchmark_3.SI();
		Benchmark_44.SI();
		Benchmark_4.SI();
		Benchmark_43.SI();
		Benchmark_5.SI();
		Benchmark_42.SI();
		Benchmark_6.SI();
		Benchmark_41.SI();
		Benchmark_7.SI();
		Benchmark_8.SI();
		Benchmark_9.SI();
		Benchmark_15.SI();
		Benchmark_14.SI();
		Benchmark_13.SI();
		Benchmark_12.SI();
		Benchmark_19.SI();
		Benchmark_18.SI();
		Benchmark_17.SI();
		Benchmark_16.SI();
		Benchmark_11.SI();
		Benchmark_10.SI();
		Benchmark_26.SI();
		Benchmark_25.SI();
		Benchmark_24.SI();
		Benchmark_23.SI();
		Benchmark_29.SI();
		Benchmark_28.SI();
		Benchmark_27.SI();
		Benchmark_22.SI();
		Benchmark_21.SI();
		Benchmark_20.SI();
		com_jtransc_JTranscVersion.SI();
		com_jtransc_time_JTranscClock_Impl.SI();
		com_jtransc_time_JTranscClock_1.SI();
		com_jtransc_time_JTranscClock.SI();
		java_io_IOException.SI();
		com_jtransc_charset_JTranscCharset.SI();
		java_io_ByteArrayOutputStream.SI();
		java_nio_charset_UnsupportedCharsetException.SI();
		java_util_ServiceLoader.SI();
		com_jtransc_charset_JTranscCharsetSingleByte.SI();
		com_jtransc_charset_charsets_JTranscCharsetIBM866.SI();
		java_util_Collections_1.SI();
		java_util_Collections_2.SI();
		java_util_Collections_EmptyList.SI();
		java_util_Collections_EmptySet.SI();
		java_util_Collections_EmptyMap.SI();
		java_util_Collections.SI();
		java_util_Enumeration_IFields.SI();
		com_jtransc_charset_charsets_JTranscCharsetUTF8.SI();
		com_jtransc_charset_charsets_JTranscCharsetUSASCII.SI();
		com_jtransc_charset_charsets_JTranscCharsetUTF16Base.SI();
		com_jtransc_charset_charsets_JTranscCharsetUTF16LE.SI();
		com_jtransc_JTranscBits.SI();
		java_lang_Process.SI();
		com_jtransc_JTranscProcess.SI();
		com_jtransc_mix_JTranscProcessMulti_Creator.SI();
		com_jtransc_mix_JTranscProcessMulti.SI();
		com_jtransc_charset_charsets_JTranscCharsetLatin1.SI();
		com_jtransc_charset_charsets_JTranscCharsetUTF16BE.SI();
		java_lang_Thread.SI();
		java_lang_Runnable_IFields.SI();
		java_lang_ThreadGroup.SI();
		java_lang_Thread_UncaughtExceptionHandler_IFields.SI();
		java_lang_ClassLoader.SI();
		java_lang__ClassInternalUtils.SI();
		java_lang__ClassInternalUtils_1.SI();
		com_jtransc_JTranscArrays.SI();
		com_jtransc_JTranscSystemProperties.SI();
		Benchmark_MyClass.SI();
		com_jtransc_FastMemory.SI();
		java_nio_Buffer.SI();
		java_nio_IntBuffer.SI();
		java_nio_internal_MemoryBlock.SI();
		java_nio_FloatBuffer.SI();
		java_nio_ByteBuffer.SI();
		java_nio_ReadOnlyBufferException.SI();
		java_nio_CharBuffer.SI();
		java_lang_Readable_IFields.SI();
		java_nio_DoubleBuffer.SI();
		java_nio_ShortBuffer.SI();
		java_nio_LongBuffer.SI();
		java_nio_ByteBufferAsFloatBuffer.SI();
		java_nio_internal_ByteBufferAs_IFields.SI();
		java_nio_ByteOrder.SI();
		java_nio_ByteBufferAsFloatBuffer_BE.SI();
		java_nio_ByteBufferAsFloatBuffer_LE.SI();
		java_nio_ByteBufferAsLongBuffer.SI();
		java_nio_ByteBufferAsLongBuffer_LE.SI();
		java_nio_ByteBufferAsLongBuffer_1.SI();
		java_nio_ByteBufferAsLongBuffer_BE.SI();
		java_nio_ByteBufferAsDoubleBuffer.SI();
		java_nio_ByteBufferAsDoubleBuffer_LE.SI();
		java_nio_ByteBufferAsDoubleBuffer_1.SI();
		java_nio_ByteBufferAsDoubleBuffer_BE.SI();
		libcore_io_Memory.SI();
		java_nio_ByteBufferAsCharBuffer.SI();
		java_nio_ByteBufferAsCharBuffer_LE.SI();
		java_nio_ByteBufferAsCharBuffer_1.SI();
		java_nio_ByteBufferAsCharBuffer_BE.SI();
		java_nio_ByteBufferAsShortBuffer.SI();
		java_nio_ByteBufferAsShortBuffer_LE.SI();
		java_nio_ByteBufferAsShortBuffer_1.SI();
		java_nio_ByteBufferAsShortBuffer_BE.SI();
		java_nio_ByteBufferAsIntBuffer.SI();
		java_nio_ByteBufferAsIntBuffer_BE.SI();
		java_nio_ByteBufferAsIntBuffer_LE.SI();
		java_util_zip_CRC32.SI();
		java_util_zip_Checksum_IFields.SI();
		com_jtransc_compression_jzlib_CRC32.SI();
		com_jtransc_compression_jzlib_Checksum_IFields.SI();
		com_jtransc_compression_jzlib_ZStream.SI();
		com_jtransc_compression_jzlib_Deflater.SI();
		com_jtransc_compression_jzlib_Deflate_Config.SI();
		com_jtransc_compression_jzlib_Deflate.SI();
		com_jtransc_compression_jzlib_Tree.SI();
		com_jtransc_compression_jzlib_GZIPHeader.SI();
		com_jtransc_compression_jzlib_StaticTree.SI();
		com_jtransc_compression_jzlib_GZIPException.SI();
		com_jtransc_compression_jzlib_Adler32.SI();
		java_util_zip_Deflater.SI();
		Benchmark_Test1.SI();
		Benchmark_Test2.SI();
		com_jtransc_simd_Simd.SI();
		com_jtransc_simd_MutableFloat32x4Utils.SI();
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils.SI();
		java_util_Dictionary.SI();
		java_lang_ref_Reference.SI();
		java_lang_ref_WeakReference.SI();
		java_io_ObjectStreamField.SI();
		java_util_Hashtable.SI();
		java_util_Properties.SI();
		java_util_Hashtable_HashtableEntry.SI();
		java_util_Hashtable_1.SI();
		java_util_Hashtable_EntrySet.SI();
		java_util_Hashtable_HashIterator.SI();
		java_util_Hashtable_EntryIterator.SI();
		java_lang_ref_ReferenceQueue.SI();
		java_lang_SafeVarargs_Impl.SI();
		java_lang_annotation_Annotation_Impl.SI();
		Benchmark.main__Ljava_lang_String__V(N.strArray(args));
	}
}
