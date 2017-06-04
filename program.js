var _global = (typeof window !== "undefined") ? window : global;

var hasSIMD = typeof SIMD !== "undefined";


var _global = (typeof window !== "undefined") ? window : global;

if ('á'.charCodeAt(0) != 225) {
	throw new Error('Encoding must be UTF-8. Please add <META http-equiv="Content-Type" content="text/html; charset=utf-8" /> to the html');
}

// Polyfills
Array.prototype.includes = Array.prototype.includes || (function(searchElement /*, fromIndex*/ ) {
	var O = Object(this);
	var len = parseInt(O.length, 10) || 0;
	if (len === 0) return false;
	var n = parseInt(arguments[1], 10) || 0;
	var k;
	if (n >= 0) {
		k = n;
	} else {
		k = len + n;
		if (k < 0) k = 0;
	}
	for (;k < len; ++k) if (searchElement === O[k]) return true;
	return false;
});

Array.prototype.map = Array.prototype.map || (function(callback, thisArg) {
	var T, A, k;
	var O = Object(this);
	var len = O.length >>> 0;
	if (arguments.length > 1) T = thisArg;
	A = new Array(len);
	k = 0;
	while (k < len) {
		var kValue, mappedValue;
		if (k in O) {
			kValue = O[k];
			mappedValue = callback.call(T, kValue, k, O);
			A[k] = mappedValue;
		}
		++k;
	}
	return A;
});

Array.prototype.contains = Array.prototype.contains || (function(searchElement) { return this.indexOf(searchElement) >= 0; });
Map.prototype.remove = Map.prototype.remove || (function(key) { this.delete(key); });

Math.imul = Math.imul || function(a, b) {
	var ah = (a >>> 16) & 0xffff;
	var al = a & 0xffff;
	var bh = (b >>> 16) & 0xffff;
	var bl = b & 0xffff;
	return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0)|0);
};

Math.clz32 = Math.clz32 || (function (x) { return (x >>>= 0) ? 31 - Math.floor(Math.log(x + 0.5) * Math.LOG2E) : 32; });
Math.fround = Math.fround || (function (array) { return function(x) { return array[0] = x, array[0]; }; })(new Float32Array(1));

String.prototype.reverse = String.prototype.reverse || (function() { return this.split("").reverse().join(""); });

String.prototype.startsWith = String.prototype.startsWith || (function(searchString, position){
	position = position || 0;
	return this.substr(position, searchString.length) === searchString;
});

String.prototype.endsWith = String.prototype.endsWith || (function(searchString, position) {
	if (position === undefined) position = subjectString.length;
	var subjectString = this.toString();
	position -= searchString.length;
	var lastIndex = subjectString.indexOf(searchString, position);
	return lastIndex !== -1 && lastIndex === position;
});

String.prototype.replaceAll = String.prototype.replaceAll || (function(search, replacement) {
	var target = this;
	return target.split(search).join(replacement);
});

String.prototype.trim = String.prototype.trim || (function () { return this.replace(/^[\s\uFEFF\xA0]+|[\s\uFEFF\xA0]+$/g, ''); });
String.prototype.quote = String.prototype.quote || (function () { return JSON.stringify(this); });

var onBrowser = typeof window != "undefined";
var onNodeJs = typeof window == "undefined";

(function(_global) { "use strict";

////////////////////////////////////////////////////////////////////////////

var Int32 = function(value) { this.value = value | 0; };
Int32.compare = function(a, b) {
	a |= 0;
	b |= 0;
	if(a == b) {
		return 0;
	} else if(a > b) {
		return 1;
	} else {
		return -1;
	}
};

Int32.ucompare = function(a, b) {
	if(a < 0) {
		if(b < 0) {
			return ~b - ~a | 0;
		} else {
			return 1;
		}
	}
	if(b < 0) {
		return -1;
	} else {
		return a - b | 0;
	}
};

Int32.mul = function(a, b) { return Math.imul(a, b); }

var Int64 = function(high, low) {
	this.high = high | 0;
	this.low = low | 0;
};

// Building
var M2P32_DBL = Math.pow(2, 32);
var MAX_INT64 = new Int64(0x7FFFFFFF, 0xFFFFFFFF);
var MIN_INT64 = new Int64(0x80000000, 0x00000000);
Int64.zero = new Int64(0, 0);
Int64.one = new Int64(0, 1);
Int64.MIN_VALUE = MIN_INT64;
Int64.MAX_VALUE = MAX_INT64;
Int64.is = function(value) { return value instanceof Int64; };
Int64.make = function(high, low) {
	if (high == 0) {
		if (low == 0) return Int64.zero;
		if (low == 1) return Int64.one;
	}
	return new Int64(high, low);
};
Int64.ofInt = function(value) { return Int64.make(value >> 31, value | 0); };
Int64.ofFloat = function(f) {
	if (isNaN(f) || !isFinite(f)) throw "Number is NaN or Infinite";
	var noFractions = f - (f % 1);
	// 2^53-1 and -2^53: these are parseable without loss of precision
	if (noFractions > 9007199254740991) throw "Conversion overflow";
	if (noFractions < -9007199254740991) throw "Conversion underflow";

	var result = Int64.ofInt(0);
	var neg = noFractions < 0;
	var rest = neg ? -noFractions : noFractions;

	var i = 0;
	while (rest >= 1) {
		var curr = rest % 2;
		rest = rest / 2;
		if (curr >= 1) result = Int64.add(result, Int64.shl(Int64.ofInt(1), i));
		++i;
	}

	return neg ? Int64.neg(result) : result;
};

Int64.ofString = function(sParam) {
	var base = Int64.ofInt(10);
	var current = Int64.ofInt(0);
	var multiplier = Int64.ofInt(1);
	var sIsNegative = false;

	var s = String(sParam).trim();
	if (s.charAt(0) == "-") {
		sIsNegative = true;
		s = s.substring(1, s.length);
	}
	var len = s.length;

	for (var i = 0; i < len; ++i) {
		var digitInt = s.charCodeAt(len - 1 - i) - '0'.code;

		if (digitInt < 0 || digitInt > 9) throw "NumberFormatError";

		var digit = Int64.ofInt(digitInt);
		if (sIsNegative) {
			current = Int64.sub(current, Int64.mul(multiplier, digit));
			if (!Int64.isNeg(current)) throw "NumberFormatError: Underflow";
		} else {
			current = Int64.add(current, Int64.mul(multiplier, digit));
			if (Int64.isNeg(current)) throw "NumberFormatError: Overflow";
		}
		multiplier = Int64.mul(multiplier, base);
	}
	return current;
};

Int64.prototype.toString = function() {
	var i = this;
	if (Int64.isZero(i)) return "0";
	var str = "";
	var neg = false;
	if(Int64.isNeg(i)) {
		neg = true;
		// i = -i; cannot negate here as --9223372036854775808 = -9223372036854775808
	}
	var ten = Int64.ofInt(10);
	while (Int64.isNotZero(i)) {
		var r = Int64.divMod(i, ten);
		if (Int64.isNeg(r.modulus)) {
			str = Int64.neg(r.modulus).low + str;
			i = Int64.neg(r.quotient);
		} else {
			str = r.modulus.low + str;
			i = r.quotient;
		}
	}
	if( neg ) str = "-" + str;
	return str;
};

Int64.toInt = function(a) { return a.low; };
Int64.toFloat = function(v) {
	if (Int64.isNeg(v)) {
		return Int64.eq(v, MIN_INT64) ? Int64.ofFloat(-9223372036854775808.0) : -Int64.toFloat(Int64.neg(v));
	} else {
		var lowf = v.low;
		var highf = v.high;
		return lowf + highf * M2P32_DBL;
	}
};

Int64.isNeg = function(a) { return a.high < 0; };
Int64.isZero = function(a) { return a.high == 0 && a.low == 0; };
Int64.isNotZero = function(a) { return a.high != 0 || a.low != 0; };

// Comparisons

Int64.compare = function(a, b) {
	var v = a.high - b.high | 0;
	if (v == 0) v = Int32.ucompare(a.low, b.low);
	return (a.high < 0) ? ((b.high < 0) ? v : -1) : ((b.high >= 0) ? v : 1);
};

Int64.ucompare = function(a, b) {
	var v = Int32.ucompare(a.high, b.high);
	return (v != 0) ? v : Int32.ucompare(a.low, b.low);
};

Int64.eq  = function(a, b) { return (a.high == b.high) && (a.low == b.low); };
Int64.ne  = function(a, b) { return (a.high != b.high) || (a.low != b.low); };
Int64.neq = function(a, b) { return (a.high != b.high) || (a.low != b.low); };
Int64.lt  = function(a, b) { return Int64.compare(a, b) < 0; };
Int64.le  = function(a, b) { return Int64.compare(a, b) <= 0; };
Int64.gt  = function(a, b) { return Int64.compare(a, b) > 0; };
Int64.ge  = function(a, b) { return Int64.compare(a, b) >= 0; };

// Strings

Int64.prototype.toString = function() {
	var i = this;
	if(Int64.eq(i,Int64.ofInt(0))) {
		return "0";
	}
	var str = "";
	var neg = false;
	if(Int64.isNeg(i)) {
		neg = true;
	}
	var ten = Int64.ofInt(10);
	while(Int64.neq(i,Int64.ofInt(0))) {
		var r = Int64.divMod(i,ten);
		if(Int64.isNeg(r.modulus)) {
			str = Int64.neg(r.modulus).low + str;
			i = Int64.neg(r.quotient);
		} else {
			str = r.modulus.low + str;
			i = r.quotient;
		}
	}
	if(neg) {
		str = "-" + str;
	}
	return str;
};



// Arithmetic

Int64.divMod = function(dividend, divisor) {
	if(divisor.high == 0) {
		switch(divisor.low) {
		case 0:
			throw new Error("divide by zero");
			break;
		case 1:
			return { quotient : Int64.make(dividend.high,dividend.low), modulus : Int64.ofInt(0)};
		}
	}
	var divSign = Int64.isNeg(dividend) != Int64.isNeg(divisor);
	var modulus = Int64.isNeg(dividend)?Int64.neg(dividend):Int64.make(dividend.high,dividend.low);
	if(Int64.isNeg(divisor)) {
		divisor = Int64.neg(divisor);
	} else {
		divisor = divisor;
	}
	var quotient = Int64.ofInt(0);
	var mask = Int64.ofInt(1);
	while(!Int64.isNeg(divisor)) {
		var cmp = Int64.ucompare(divisor,modulus);
		divisor = Int64.shl(divisor,1);
		mask = Int64.shl(mask,1);
		if(cmp >= 0) {
			break;
		}
	}
	while(Int64.neq(mask,Int64.ofInt(0))) {
		if(Int64.ucompare(modulus,divisor) >= 0) {
			quotient = Int64.or(quotient,mask);
			modulus = Int64.sub(modulus,divisor);
		}
		mask = Int64.ushr(mask,1);
		divisor = Int64.ushr(divisor,1);
	}
	if(divSign) quotient = Int64.neg(quotient);
	if(Int64.isNeg(dividend)) modulus = Int64.neg(modulus);
	return { quotient : quotient, modulus : modulus};
};

Int64.neg = function(x) {
	var high = ~x.high | 0;
	var low = -x.low | 0;
	if(low == 0) high = high + 1 | 0;
	return Int64.make(high,low);
};

Int64.add = function(a, b) {
	var high = a.high + b.high | 0;
	var low = a.low + b.low | 0;
	if(Int32.ucompare(low,a.low) < 0) {
		high = high + 1 | 0;
	}
	return Int64.make(high,low);
};

Int64.sub = function(a, b) {
	var high = a.high - b.high | 0;
	var low = a.low - b.low | 0;
	if(Int32.ucompare(a.low,b.low) < 0) {
		high = high - 1 | 0;
	}
	return Int64.make(high,low);
};

Int64.mul = function(a, b) {
	var al = a.low & 65535;
	var ah = a.low >>> 16;
	var bl = b.low & 65535;
	var bh = b.low >>> 16;
	var p00 = Int32.mul(al,bl);
	var p10 = Int32.mul(ah,bl);
	var p01 = Int32.mul(al,bh);
	var p11 = Int32.mul(ah,bh);
	var low = p00;
	var high = (p11 + (p01 >>> 16) | 0) + (p10 >>> 16) | 0;
	p01 = p01 << 16;
	low = p00 + p01 | 0;
	if(Int32.ucompare(low,p01) < 0) high = high + 1 | 0;
	p10 = p10 << 16;
	low = low + p10 | 0;
	if(Int32.ucompare(low,p10) < 0) high = high + 1 | 0;
	high = high + (Int32.mul(a.low,b.high) + Int32.mul(a.high,b.low) | 0) | 0;
	return Int64.make(high,low);
};

Int64.div = function(a, b) { return Int64.divMod(a, b).quotient; };
Int64.mod = function(a, b) { return Int64.divMod(a, b).modulus; };
Int64.rem = function(a, b) { return Int64.divMod(a, b).modulus; };

// BIT-WISE
Int64.not = function(x) { return Int64.make(~x.high, ~x.low); }
Int64.and = function(a, b) { return Int64.make(a.high & b.high, a.low & b.low); }
Int64.or = function(a, b) { return Int64.make(a.high | b.high, a.low | b.low); }
Int64.xor = function(a, b) { return Int64.make(a.high ^ b.high, a.low ^ b.low); }
Int64.shl = function(a, b) {
	b &= 63;
	if(b == 0) {
		return Int64.make(a.high,a.low);
	} else if(b < 32) {
		return Int64.make(a.high << b | a.low >>> 32 - b,a.low << b);
	} else {
		return Int64.make(a.low << b - 32,0);
	}
}
Int64.shr = function(a, b) {
	b &= 63;
	if(b == 0) {
		return Int64.make(a.high,a.low);
	} else if(b < 32) {
		return Int64.make(a.high >> b,a.high << 32 - b | a.low >>> b);
	} else {
		return Int64.make(a.high >> 31,a.high >> b - 32);
	}
}
Int64.ushr = function(a, b) {
	b &= 63;
	if(b == 0) {
		return Int64.make(a.high,a.low);
	} else if(b < 32) {
		return Int64.make(a.high >>> b,a.high << 32 - b | a.low >>> b);
	} else {
		return Int64.make(0,a.high >>> b - 32);
	}
}

Int64.sign = function(a) {
	if (Int64.isNeg(a)) return -1;
	if (Int64.isNotZero(a)) return +1;
	return 0;
};

Int64.abs = function(a) {
	return (Int64.sign(a) < 0) ? Int64.neg(a) : a;
};

////////////////////////////////////////////////////////////////////////////

var S = [];
var SS = [];

function __buildStrings() {
	var len = SS.length
	S.length = len;
	for (var n = 0; n < len; ++n) S[n] = N.str(SS[n]);
}

var JA_0, JA_Z, JA_B, JA_C, JA_S, JA_I, JA_J, JA_F, JA_D, JA_L;

function __createJavaArrayBaseType() {
	var ARRAY = function() {
	};

	ARRAY.prototype = Object.create(java_lang_Object.prototype);
	ARRAY.prototype.constructor = ARRAY;

	ARRAY.prototype["getClass()Ljava/lang/Class;"] = function() {
		return N.resolveClass(this.desc);
	};

	ARRAY.prototype['setArraySlice'] = function(startIndex, array) {
		var len = array.length;
		for (var n = 0; n < len; ++n) this.data[startIndex + n] = array[n];
	};


	return ARRAY;
}

function __addArrayJavaMethods(ARRAY) {
	ARRAY.prototype["clone()Ljava/lang/Object;"] = ARRAY.prototype.clone;

	ARRAY.prototype["getClass()Ljava/lang/Class;"] = function() {
		return N.resolveClass(this.desc);
	};

	ARRAY.prototype["toString()Ljava/lang/String;"] = function() {
		return N.str('ARRAY(' + this.desc + ')');
	};
}

function __createJavaArrayType(desc, type, elementBytesSize) {
	var ARRAY;
	ARRAY = function(size) {
		this.desc = desc;
		this.memorySize = size * elementBytesSize;
		this.data = new type((((this.memorySize + 7) & ~7) / elementBytesSize)|0);
		this.length = size;
		this.init();

		//console.log('Created array instance: [' + desc + ":" + type.name + ":" + size + "]");
	};

	ARRAY.prototype = Object.create(JA_0.prototype);
	ARRAY.prototype.constructor = ARRAY;

	if (desc == '[J') {
		ARRAY.prototype.init = function() {
			var zero = N.lnew(0, 0);
			for (var n = 0; n < this.length; ++n) this.set(n, zero);
		};
		ARRAY.prototype.clone = function() {
			var out = new ARRAY(this.length);
			for (var n = 0; n < this.length; ++n) out.set(n, this.get(n));
			return out;
		};
	} else {
		ARRAY.prototype.init = function() { };
		ARRAY.prototype.clone = function() {
			var out = new ARRAY(this.length);
			out.data.set(this.data);
			return out;
		};
	}

	ARRAY.fromTypedArray = function(typedArray) {
		var out = new ARRAY(typedArray.length);
		out.data.set(typedArray);
		return out;
	};

	ARRAY.T = ARRAY.fromTypedArray;

	ARRAY.wrapBuffer = function(arrayBuffer) {
		var out = new ARRAY(0);
		out.data = new type(arrayBuffer);
		out.length = out.data.length;
		return out;
	};

	ARRAY.prototype.get = function(index) { return this.data[index]; };
	ARRAY.prototype.set = function(index, value) { this.data[index] = value; };

	ARRAY.prototype.getBuffer = function() {
		return this.data.buffer;
	};

	ARRAY.prototype.toArray = function() {
    	var out = new Array(this.length);
    	for (var n = 0; n < out.length; ++n) out[n] = this.get(n);
    	return out;
    };

	//override fun genStmSetArrayLiterals(stm: AstStm.SET_ARRAY_LITERALS) // Use typedarrays
	//ARRAY.prototype['setTypedArraySlice'] = function(startIndex, array) {
	//	this.data.set(array, startIndex);
	//};

	// @TODO: Check performance
	//ARRAY.prototype['setArraySlice'] = function(startIndex, array) {
	//	this.data.set(new type(array), startIndex);
	//};

	//JA_0 dest, int srcPos, int destPos, int length
	if (desc == '[J') {
		ARRAY.prototype.copyTo = function(dest, srcPos, destPos, length, overlapping) {
			var srcData = this.data;
			var destData = dest.data;
			if (overlapping) {
				for (var n = length - 1; n >= 0; n--) destData[destPos + n] = srcData[srcPos + n];
			} else {
				for (var n = 0; n < length; ++n) destData[destPos + n] = srcData[srcPos + n];
			}
		};
	} else {
		ARRAY.prototype.copyTo = function(dest, srcPos, destPos, length, overlapping) {
			dest.data.set(new type(this.data.buffer, srcPos * elementBytesSize, length), destPos);
		};
	}

	__addArrayJavaMethods(ARRAY);

	return ARRAY;
}

function __createGenericArrayType() {
	var ARRAY = function(size, desc) {
		this.desc = desc;
		this.data = new Array(size);
		this.length = size;
		for (var n = 0; n < size; ++n) this.data[n] = null;
	};

	ARRAY.prototype = Object.create(JA_0.prototype);
	ARRAY.prototype.constructor = ARRAY;

	ARRAY.copyOfRange = function(jarray, start, end, desc) {
		if (desc === undefined) desc = jarray.desc;
		var size = end - start;
		var out = new ARRAY(size, desc);
		var outData = out.data;
		var jarrayData = jarray.data;
		for (var n = 0; n < size; ++n) outData[n] = jarrayData[start + n];
		return out;
	};

	ARRAY.fromArray = function(array, desc) {
		if (array == null) return null;
		var out = new JA_L(array.length, desc);
		for (var n = 0; n < out.length; ++n) out.set(n, array[n]);
		return out;
	};

	ARRAY.fromArrayOrEmpty = function(array, desc) {
		return ARRAY.fromArray(array ? array : [], desc);
	};

	ARRAY.prototype.get = function(index) {
		return this.data[index];
	};

	ARRAY.prototype.set = function(index, value) {
		this.data[index] = value;
	};

	ARRAY.prototype.clone = function() {
		var out = new JA_L(this.length, this.desc);
		for (var n = 0; n < this.length; ++n) out.set(n, this.get(n));
		return out;
	};

	ARRAY.prototype.toArray = function() {
		return this.data;
	};

	//JA_0 dest, int srcPos, int destPos, int length
	ARRAY.prototype.copyTo = function(dest, srcPos, destPos, length, overlapping) {
		var srcData = this.data;
		var destData = dest.data;
		if (overlapping) {
			for (var n = length - 1; n >= 0; n--) destData[destPos + n] = srcData[srcPos + n];
		} else {
			for (var n = 0; n < length; ++n) destData[destPos + n] = srcData[srcPos + n];
		}
	};

	__addArrayJavaMethods(ARRAY);

	return ARRAY;
}

function __createJavaArrays() {
	JA_0 = __createJavaArrayBaseType();
	JA_Z = __createJavaArrayType('[Z', Int8Array, 1);    // Bool Array
	JA_B = __createJavaArrayType('[B', Int8Array, 1);    // Byte Array
	JA_C = __createJavaArrayType('[C', Uint16Array, 2);  // Character Array
	JA_S = __createJavaArrayType('[S', Int16Array, 2);   // Short Array
	JA_I = __createJavaArrayType('[I', Int32Array, 4);   // Int Array
	JA_F = __createJavaArrayType('[F', Float32Array, 4); // Float Array
	JA_D = __createJavaArrayType('[D', Float64Array, 8); // Double Array

	// Specially handled
	JA_J = __createJavaArrayType('[J', Array, 1);        // Long Array

	JA_L =__createGenericArrayType(); // Generic Array

	JA_L.createMultiSure = function(sizes, desc) {
		if (!desc.startsWith('[')) return null;
		if (sizes.length == 1) return JA_L.create(sizes[0], desc);
		var out = new JA_L(sizes[0], desc);
		var sizes2 = sizes.slice(1);
		var desc2 = desc.substr(1);
		for (var n = 0; n < out.length; ++n) {
			out.set(n, JA_L.createMultiSure(sizes2, desc2));
		}
		return out;
	};

	 JA_L.create = function(size, desc) {
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
	};

	JA_L.fromArray1 = function(items, desc) {
		if (items == null) return null;
		var out = JA_L.create(items.length, desc);
		for (var n = 0; n < items.length; ++n) out.set(n, items[n]);
		return out;
	}

	JA_L.fromArray2 = function(items, desc) {
		if (items == null) return null;
		var out = new JA_L(items.length, desc);
		for (var n = 0; n < items.length; ++n) out.set(n, JA_L.fromArray1(items[n], desc.substr(1)));
		return out;
	};
}

var N = function() {
};

var __reints = (function() {
	var buffer = new ArrayBuffer(8);
	var doubleArray = new Float64Array(buffer);
	var floatArray = new Float32Array(buffer);
    var intArray = new Int32Array(buffer);
    return {
    	intBitsToFloat: function(v) {
    		intArray[0] = v;
    		return floatArray[0];
    	},
    	floatToIntBits: function(v) {
    		floatArray[0] = v;
    		return intArray[0];
    	},
    	doubleToLongBits: function(v) {
    		doubleArray[0] = v;
    		return N.lnew(intArray[1], intArray[0]);
    	},
		longBitsToDouble: function(v) {
			intArray[0] = N.llow(v);
			intArray[1] = N.lhigh(v);
			return doubleArray[0];
		},
		isLittleEndian: function() {
           return new Int16Array(new Uint8Array([1,0]).buffer)[0] == 1;
		}
    };
})();

N.MIN_INT32 = -2147483648;

N.isLittleEndian = __reints.isLittleEndian();
N.intBitsToFloat = __reints.intBitsToFloat;
N.floatToIntBits = __reints.floatToIntBits;
N.doubleToLongBits = __reints.doubleToLongBits;
N.longBitsToDouble = __reints.longBitsToDouble;

N.i = function(v) { return v | 0; }

N.z2i = function(v) { return v | 0; }

///////////////////////
// Conversions
///////////////////////
N.i2z = function(v) { return v != 0; }
N.i2b = function(v) { return ((v << 24) >> 24); }
N.i2s = function(v) { return ((v << 16) >> 16); }
N.i2c = function(v) { return v & 0xFFFF; }
N.i2i = function(v) { return v | 0; }
N.i2j = function(v) { return Int64.ofInt(v); }
N.i2f = function(v) { return +v; }
N.i2d = function(v) { return +v; }
N.f2j = function(v) { return Int64.ofFloat(v); }
N.d2j = function(v) { return Int64.ofFloat(v); }


///////////////////////
// Integer
///////////////////////
N.ishl = function(a, b) { return (a << b) | 0; };
N.ishr = function(a, b) { return (a >> b) | 0; };
N.iushr = function(a, b) { return (a >>> b) | 0; };

N.idiv = function(a, b) { return Math.floor(a / b) | 0; };
N.irem = function(a, b) { return (a % b) | 0; };

///////////////////////
// Long
///////////////////////
N.linit = function() {
};
N.lnew = function(high, low) { return Int64.make(high, low); };
N.lnewFloat = function(v) { return Int64.ofFloat(v); };
N.ltoFloat = function(v) { return Int64.toFloat(v); };
N.llow  = function(v) { return v.low; }
N.lhigh = function(v) { return v.high; }
N.ladd  = function(a, b) { return Int64.add(a, b); }
N.lsub  = function(a, b) { return Int64.sub(a, b); }
N.lmul  = function(a, b) { return Int64.mul(a, b); }
N.ldiv  = function(a, b) { return Int64.div(a, b); }
N.lrem  = function(a, b) { return Int64.rem(a, b); }
N.llcmp = function(a, b) { return Int64.compare(a, b); } // Deprecated
N.lcmp  = function(a, b) { return Int64.compare(a, b); }
N.lxor  = function(a, b) { return Int64.xor(a, b); }
N.land  = function(a, b) { return Int64.and(a, b); }
N.lor   = function(a, b) { return Int64.or(a, b); }
N.lshl  = function(a, b) { return Int64.shl(a, b); }
N.lshr  = function(a, b) { return Int64.shr(a, b); }
N.lushr = function(a, b) { return Int64.ushr(a, b); }
N.lneg  = function(a) { return Int64.neg(a); }
N.linv  = function(a) { return Int64.not(a); }

N.j2i   = function(v) { return Int64.toInt(v); }
N.j2f   = function(v) { return Int64.toFloat(v); }
N.j2d   = function(v) { return Int64.toFloat(v); }

N.cmp  = function(a, b) { return (a < b) ? -1 : ((a > b) ? 1 : 0); }
N.cmpl = function(a, b) { return (isNaN(a) || isNaN(b)) ? -1 : N.cmp(a, b); }
N.cmpg = function(a, b) { return (isNaN(a) || isNaN(b)) ? 1 : N.cmp(a, b); }


N.getTime = function() { return Date.now(); };
N.hrtime = function() {
	if (onBrowser) {
		if (typeof performance != 'undefined') {
			return N.lnewFloat(performance.now() * 1000000.0);
		} else {
			return N.lmul(N.lnewFloat(Date.now()), N.i2j(1000000));
		}
	} else if (onNodeJs) {
		var hr = process.hrtime()
		return N.ladd(N.lmul(N.i2j(hr[0]), N.i2j(1000000000)), N.i2j(hr[1]));
	} else {
		throw 'Unsupported high resolution time';
	}
};

// @TODO: optimize this again!
N.is = function(i, clazz) {
	if (i instanceof clazz) return true;
	if (i == null) return false;
	if (typeof i.__JT__CLASS_IDS === 'undefined') return false;
	return i.__JT__CLASS_IDS.indexOf(clazz.__JT__CLASS_ID) >= 0;
};

N.checkCast = function(i, clazz) {
	if (i == null) return null;
	if (clazz === null) throw new Error('Internal error N.checkCast');
	if (!N.is(i, clazz)) {
		throw new WrappedError((new java_lang_ClassCastException())["java.lang.ClassCastException<init>(Ljava/lang/String;)V"](N.str('Invalid conversion')));
	}
	return i;
};

N.isClassId = function(i, classId) {
	if (i == null) return false;
	if (!i.__JT__CLASS_IDS) return false;
	return i.__JT__CLASS_IDS.indexOf(classId) >= 0;
};

N.istr = function(str) {
	if (str == null) return null;
	if (str instanceof java_lang_String) return str._str;
	return '' + str;
}

N.ichar = function(i) {
	return String.fromCharCode(i);
}

N.str = function(str) {
	if (str == null) return null;
	if (str instanceof java_lang_String) return str;
	var out = new java_lang_String();
	out._str = '' + str;
	return out;
}

N.strLit = function(str) {
	// Check cache!
	return N.str(str);
};

N.strLitEscape = function(str) {
	// Check cache!
	return str;
};

N.strArray = function(strs) {
	if (strs == null) return null;
	var out = new JA_L(strs.length, '[Ljava/lang/String;');
	for (var n = 0; n < strs.length; ++n) {
		out.set(n, N.str(strs[n]));
	}
	return out;
};

N.strArrayOrEmpty = function(strs) {
	var out = N.strArray(strs);
	return out ? out : [];
};

N.istrArray = function(strs) {
	if (strs == null) return null;
	return strs.data.map(function(s) { return N.istr(s); });
};

N.iteratorToArray = function(it) {
	if (it == null) return null;
	var out = [];
	while (it["hasNext()Z"]()) {
		out.push(it["next()Ljava/lang/Object;"]());
	}
	return out;
};

N.imap = function(map) {
	if (map == null) return null;
	var obj = {};
	N.iteratorToArray(map["entrySet()Ljava/util/Set;"]()["iterator()Ljava/util/Iterator;"]()).forEach(function(item) {
		var key = item["getKey()Ljava/lang/Object;"]();
		var value = item["getValue()Ljava/lang/Object;"]();
		obj[N.unbox(key)] = N.unbox(value);
	});
	return obj;
};

N.args = function() {
	return onNodeJs ? process.argv.slice(2) : [];
};

N.byteArrayToString = N.intArrayToString = N.charArrayToString = function(array, offset, length, encoding) {
	if (offset === undefined) offset = 0;
	if (length === undefined) length = array.length - offset;
	if (encoding === undefined) encoding = 'UTF-8';
	// @TODO: Handle encodings!
	var out = '';
	for (var n = offset; n < offset + length; ++n) {
		out += String.fromCharCode(array.get(n));
	}
	return out;
};

N.stringToCharArray = function(str) {
	var out = new JA_C(str.length);
	for (var n = 0; n < str.length; ++n) out.set(n, str.charCodeAt(n));
	return out;
};

N.resolveClass = function(name) {
	return java_lang_Class["forName(Ljava/lang/String;)Ljava/lang/Class;"](N.str(name));
};

N.createStackTraceElement = function(declaringClass, methodName, fileName, lineNumber) {
	var out = (new java_lang_StackTraceElement())["java.lang.StackTraceElement<init>(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;I)V"](
		N.str(declaringClass),
		N.str(methodName),
		N.str(fileName),
		lineNumber | 0
	);
	return out;
};

function stackTrace() {
    var err = new Error();
    return err.stack.split('\n').slice(3);
}

N.getStackTrace = function(error, count) {
	//var traces = stackTrace()
	var traces = error.stack.split('\n').slice(count);
	var out = new JA_L(traces.length, '[Ljava/lang/StackTraceElement;');
	for (var n = 0; n < traces.length; ++n) {
		out.set(n, N.createStackTraceElement('JS', 'js', traces[n], 0));
	}
	return out;
};

N.arraycopy = function(src, srcPos, dest, destPos, length) {
	//if (length < 0 || srcPos < 0 || destPos < 0 || srcPos + length > src.length || destPos + length > dest.length) N.throwRuntimeException('N.arraycopy out of bounds');
	var overlapping = src == dest && (destPos > srcPos);
	src.copyTo(dest, srcPos, destPos, length, overlapping);
};

N.isInstanceOfClass = function(obj, javaClass) {
	if (obj == null) return false;
	if (javaClass == null) return false;
	var clazz = jtranscClasses[N.istr(javaClass._name)];
	if (clazz == null) return false;
	return N.is(obj, clazz);
};

N.identityHashCode = function(p0) {
	return (p0 != null) ? p0.$JS$ID$ : 0;
};

N.fillSecureRandomBytes = function(array) {
	var buf;

	if (onNodeJs) {
		buf = require('crypto').randomBytes(256);
	} else {
		buf = new Uint8Array(array.length);
		window.crypto.getRandomValues(buf);
	}

	for (var n = 0; n < array.length; ++n) array.set(n, buf[n]);
};

N.boxVoid = function(value) { return null; }
N.boxBool = function(value) { return java_lang_Boolean["valueOf(Z)Ljava/lang/Boolean;"](value); }
N.boxByte = function(value) { return java_lang_Byte["valueOf(B)Ljava/lang/Byte;"](value); }
N.boxShort = function(value) { return java_lang_Short["valueOf(S)Ljava/lang/Short;"](value); }
N.boxChar = function(value) { return java_lang_Character["valueOf(C)Ljava/lang/Character;"](value); }
N.boxInt = function(value) { return java_lang_Integer["valueOf(I)Ljava/lang/Integer;"](value); }
N.boxLong = function(value) { return java_lang_Long["valueOf(J)Ljava/lang/Long;"](value); }
N.boxFloat = function(value) { return java_lang_Float["valueOf(F)Ljava/lang/Float;"](value); }
N.boxDouble = function(value) { return java_lang_Double["valueOf(D)Ljava/lang/Double;"](value); }
N.boxString = function(value) { return (value != null) ? N.str(value) : null; }
N.boxWrapped = function(value) { return N.wrap(value); }

N.unboxVoid      = function(value) { return null; }
N.unboxBool      = function(value) { return value["_value"]; }
N.unboxByte      = function(value) { return value["_value"]; }
N.unboxShort     = function(value) { return value["_value"]; }
N.unboxChar      = function(value) { return value["_value"]; }
N.unboxInt       = function(value) { return value["_value"]; }
N.unboxLong      = function(value) { return value["_value"]; }
N.unboxFloat     = function(value) { return value["_value"]; }
N.unboxDouble    = function(value) { return value["_value"]; }
N.unboxString    = function(value) { return N.istr(value); }
N.unboxWrapped   = function(value) { return value._wrapped; }

N.unboxByteArray = function(value) {
	return value.data;
};

N.unbox = function(value, throwOnInvalid) {
	if (N.is(value, java_lang_Boolean)) return N.unboxBool(value);
	if (N.is(value, java_lang_Byte)) return N.unboxByte(value);
	if (N.is(value, java_lang_Short)) return N.unboxShort(value);
	if (N.is(value, java_lang_Character)) return N.unboxChar(value);
	if (N.is(value, java_lang_Integer)) return N.unboxInt(value);
	if (N.is(value, java_lang_Long)) return N.unboxLong(value);
	if (N.is(value, java_lang_Float)) return N.unboxFloat(value);
	if (N.is(value, java_lang_Double)) return N.unboxDouble(value);
	if (N.is(value, java_lang_String)) return N.unboxString(value);
	if (value instanceof JA_B) return N.unboxByteArray(value);
	if (N.is(value, com_jtransc_JTranscWrapped)) return N.unboxWrapped(value);
	if (throwOnInvalid) throw 'Was not able to unbox "' + value + '"';
	return value;
}

N.wrap = function(value) {
	var out = new com_jtransc_JTranscWrapped();
	out._wrapped = value;
	return out;
}

N.createRuntimeException = function(msg) {
	return (new java_lang_RuntimeException())["java.lang.RuntimeException<init>(Ljava/lang/String;)V"](N.str(msg));
};

N.throwRuntimeException = function(msg) {
	throw N.createRuntimeException(msg);
	//throw msg;
};

N.boxWithType = function(clazz, value) {
	if (value instanceof JA_0) return value;
	if (value instanceof java_lang_Object) return value;

	var clazzName = N.istr(clazz.__name);

	switch (clazzName) {
		case 'void'   : return N.boxVoid();
		case 'boolean': return N.boxBool(value);
		case 'byte'   : return N.boxByte(value);
		case 'short'  : return N.boxShort(value);
		case 'char'   : return N.boxChar(value);
		case 'int'    : return N.boxInt(value);
		case 'long'   : return N.boxLong(value);
		case 'float'  : return N.boxFloat(value);
		case 'double' : return N.boxDouble(value);
	}

	console.log("WARNING: Don't know how to unbox class '" + clazzName + "' with value '" + value + "'", value);
	return value;
};

N.unboxWithTypeWhenRequired = function(clazz, value) {
	var clazzName = N.istr(clazz.__name);

	switch (clazzName) {
		case 'void'   :
		case 'boolean':
		case 'byte'   :
		case 'short'  :
		case 'char'   :
		case 'int'    :
		case 'long'   :
		case 'float'  :
		case 'double' :
			return N.unbox(value);
	}

	return value;
};

N.unboxArray = function(array) {
	return array.map(function(it) { return N.unbox(it); });
};

N.boxArray = function(array) {
	return JA_L.fromArray(array.map(function(it) { return N.box(it); }));
};

N.box = function(v) {
	if (v instanceof java_lang_Object) return v; // already boxed!
	if (v instanceof Int64) return N.boxLong(v);
	if (typeof v == 'string') return N.str(v);
	if ((v|0) == v) return N.boxInt(v);
	if (+(v) == v) return N.boxFloat(v);
	if ((v == null) || N.is(v, java_lang_Object)) return v;
	return N.wrap(v);
};

N.isNegativeZero = function(x) {
	return x === 0 && 1 / x === -Infinity;
};

N.sort = function(array, start, end, comparator) {
	var slice = array.slice(start, end);
	if (comparator === undefined) {
		slice.sort();
	} else {
		slice.sort(function(a, b) {
			return comparator["compare(Ljava/lang/Object;Ljava/lang/Object;)I"](a, b);
		});
	}
	for (var n = 0; n < slice.length; ++n) array[start + n] = slice[n];
};

N.getByteArray = function(v) {
	if (v instanceof JA_B) return v;
	var length = v.byteLength || v.length
	if (v.buffer) v = v.buffer;
	var out = new JA_B(length);
	out.data = new Int8Array(v);
	return out;
};

N.clone = function(obj) {
	if (obj == null) return null;
	var temp = Object.create(obj);
	temp.___id = 0;
	return temp;
};

N.methodWithoutBody = function(name) {
	throw 'Method not implemented: native or abstract: ' + name;
};

N.EMPTY_FUNCTION = function() { }

var java_lang_Object_base = function() { };
java_lang_Object_base.prototype.toString = function() {
	return this ? N.istr(this["toString()Ljava/lang/String;"]()) : null;
};

function WrappedError(javaThrowable) {
	this.constructor.prototype.__proto__ = Error.prototype;
	Error.captureStackTrace(this, this.constructor);
	this.name = this.constructor.name;
	this.javaThrowable = javaThrowable;
	try {
		this.message = (javaThrowable != null) ? (('' + javaThrowable) || 'JavaError') : 'JavaError';
	} catch (e) {
		this.message = 'JavaErrorWithoutValidMessage';
	}
}

//process.on('uncaughtException', function (exception) { console.error(exception); });


SS = new Array(528);
SS[0] = "@";
SS[1] = ")";
SS[2] = "name == null";
SS[3] = "cl == null";
SS[4] = "[]";
SS[5] = "(this Collection)";
SS[6] = ", ";
SS[7] = "=";
SS[8] = "threshold";
SS[9] = "loadFactor";
SS[10] = "key == null";
SS[11] = "value == null";
SS[12] = "(this Map)";
SS[13] = "Simd.MutableFloat32x4(";
SS[14] = "Exception:";
SS[15] = "\tat ";
SS[16] = "Supressed:";
SS[17] = "Cause:";
SS[18] = "need dictionary";
SS[19] = "stream end";
SS[20] = "";
SS[21] = "file error";
SS[22] = "stream error";
SS[23] = "data error";
SS[24] = "insufficient memory";
SS[25] = "buffer error";
SS[26] = "incompatible version";
SS[27] = ": ";
SS[28] = "capacity < 0: ";
SS[29] = "]";
SS[30] = ",capacity=";
SS[31] = ",limit=";
SS[32] = "[position=";
SS[33] = ", limit=";
SS[34] = "index=";
SS[35] = ", remaining()=";
SS[36] = "BIG_ENDIAN";
SS[37] = "LITTLE_ENDIAN";
SS[38] = ", arrayOffset=";
SS[39] = ", capacity=";
SS[40] = "backingArray.length=";
SS[41] = "hello";
SS[42] = "\n";
SS[43] = "HOME";
SS[44] = "/tmp";
SS[45] = "en";
SS[46] = "TMPDIR";
SS[47] = "TEMP";
SS[48] = "TMP";
SS[49] = "/";
SS[50] = "\\";
SS[51] = "UTF-8";
SS[52] = ":";
SS[53] = "USERNAME";
SS[54] = "USER";
SS[55] = "username";
SS[56] = "US";
SS[57] = "; regionLength=";
SS[58] = "; regionStart=";
SS[59] = "length=";
SS[60] = ",maxpri=";
SS[61] = "[name=";
SS[62] = ",";
SS[63] = "Thread[";
SS[64] = ",]";
SS[65] = "Array.newInstance";
SS[66] = "[";
SS[67] = "Invalid Array of void type";
SS[68] = "Invalid Array.newInstance with ";
SS[69] = "storage == null";
SS[70] = "UTF-16BE";
SS[71] = "UnicodeBigUnmarked";
SS[72] = "X-UTF-16BE";
SS[73] = "ISO-10646-UCS-2";
SS[74] = "ISO-8859-1";
SS[75] = "819";
SS[76] = "ISO8859-1";
SS[77] = "L1";
SS[78] = "ISO_8859-1:1987";
SS[79] = "ISO_8859-1";
SS[80] = "8859_1";
SS[81] = "ISO-IR-100";
SS[82] = "LATIN1";
SS[83] = "CP819";
SS[84] = "ISO8859_1";
SS[85] = "IBM819";
SS[86] = "ISO_8859_1";
SS[87] = "IBM-819";
SS[88] = "CSISOLATIN1";
SS[89] = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0b\x0c\r\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ";
SS[90] = "UTF-16LE";
SS[91] = "UTF-16";
SS[92] = "UnicodeLittleUnmarked";
SS[93] = "X-UTF-16LE";
SS[94] = "US-ASCII";
SS[95] = "ANSI_X3.4-1968";
SS[96] = "CP367";
SS[97] = "CSASCII";
SS[98] = "ISO-IR-6";
SS[99] = "ASCII";
SS[100] = "ISO_646.IRV:1983";
SS[101] = "ANSI_X3.4-1986";
SS[102] = "ASCII7";
SS[103] = "DEFAULT";
SS[104] = "ISO_646.IRV:1991";
SS[105] = "ISO646-US";
SS[106] = "IBM367";
SS[107] = "646";
SS[108] = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0b\x0c\r\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~��������������������������������������������������������������������������������������������������������������������������������";
SS[109] = "UTF8";
SS[110] = "{}";
SS[111] = "Capacity: ";
SS[112] = "IBM866";
SS[113] = "866";
SS[114] = "IBM-866";
SS[115] = "CSIBM866";
SS[116] = "CP866";
SS[117] = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0b\x0c\r\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдежзийклмноп░▒▓│┤╡╢╖╕╣║╗╝╜╛┐└┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪┘┌█▄▌▐▀рстуфхцчшщъыьэюяЁёЄєЇїЎў°∙·√№¤■ ";
SS[118] = "com.jtransc.charset.JTranscCharset";
SS[119] = "com.jtransc.JTranscProcess";
SS[120] = "ServiceLoader for ";
SS[121] = "size < 0";
SS[122] = "0.6.3-snapshot";
SS[123] = "orld";
SS[124] = "a";
SS[125] = "test";
SS[126] = "win";
SS[127] = "windows";
SS[128] = "lin";
SS[129] = "linux";
SS[130] = "mac";
SS[131] = "osx";
SS[132] = "fuch";
SS[133] = "fuchsia";
SS[134] = "UNKNOWN";
SS[135] = ".";
SS[136] = "(Native Method)";
SS[137] = "(";
SS[138] = "(Unknown Source)";
SS[139] = "Benchmark";
SS[140] = "java.lang.Object";
SS[141] = "java.lang.String";
SS[142] = "java.io.Serializable";
SS[143] = "java.lang.Comparable";
SS[144] = "java.lang.CharSequence";
SS[145] = "java.lang.String$CaseInsensitiveComparator";
SS[146] = "java.util.Comparator";
SS[147] = "java.lang.String$1";
SS[148] = "java.lang.StringBuilder";
SS[149] = "java.lang.Appendable";
SS[150] = "java.lang.Class";
SS[151] = "java.lang.reflect.Type";
SS[152] = "java.lang.reflect.GenericDeclaration";
SS[153] = "java.lang.reflect.AnnotatedElement";
SS[154] = "java.lang.AnnotatedElement";
SS[155] = "java.lang.reflect.Modifier";
SS[156] = "java.lang.Integer";
SS[157] = "java.lang.Number";
SS[158] = "java.lang.reflect.Field";
SS[159] = "java.lang.reflect.Member";
SS[160] = "java.lang.reflect.AccessibleObject";
SS[161] = "java.lang.ClassNotFoundException";
SS[162] = "java.lang.ReflectiveOperationException";
SS[163] = "java.lang.Exception";
SS[164] = "java.lang.Throwable";
SS[165] = "java.lang.Void";
SS[166] = "java.lang.Float";
SS[167] = "com.jtransc.text.JTranscStringTools";
SS[168] = "java.lang.Math";
SS[169] = "java.lang.Character";
SS[170] = "java.util.Arrays";
SS[171] = "java.lang.System";
SS[172] = "java.io.PrintStream";
SS[173] = "java.io.Closeable";
SS[174] = "java.lang.AutoCloseable";
SS[175] = "java.io.FilterOutputStream";
SS[176] = "java.io.OutputStream";
SS[177] = "java.io.Flushable";
SS[178] = "java.lang.NullPointerException";
SS[179] = "java.lang.RuntimeException";
SS[180] = "java.lang.System$1";
SS[181] = "java.io.InputStream";
SS[182] = "com.jtransc.io.JTranscConsolePrintStream";
SS[183] = "com.jtransc.io.JTranscConsolePrintStream$ConsoleOutputStream";
SS[184] = "com.jtransc.io.JTranscConsolePrintStream$ConsoleBaseStream";
SS[185] = "com.jtransc.io.JTranscConsolePrintStream$ConsoleErrorStream";
SS[186] = "java.lang.ArrayIndexOutOfBoundsException";
SS[187] = "java.lang.IndexOutOfBoundsException";
SS[188] = "java.lang.IllegalArgumentException";
SS[189] = "java.lang.Double";
SS[190] = "java.lang.Long";
SS[191] = "com.jtransc.internal.JTranscCType";
SS[192] = "java.lang.Short";
SS[193] = "com.jtransc.io.JTranscConsole";
SS[194] = "java.lang.Boolean";
SS[195] = "java.lang.Byte";
SS[196] = "java.lang.reflect.Method";
SS[197] = "java.lang.reflect.MethodConstructor";
SS[198] = "j.MemberInfo";
SS[199] = "java.lang.Error";
SS[200] = "java.util.ArrayList";
SS[201] = "java.util.List";
SS[202] = "java.util.Collection";
SS[203] = "java.lang.Iterable";
SS[204] = "java.util.RandomAccess";
SS[205] = "java.lang.Cloneable";
SS[206] = "java.util.AbstractList";
SS[207] = "java.util.AbstractCollection";
SS[208] = "java.util.Iterator";
SS[209] = "java.util.AbstractList$SimpleListIterator";
SS[210] = "java.util.ListIterator";
SS[211] = "java.util.AbstractList$FullListIterator";
SS[212] = "java.util.NoSuchElementException";
SS[213] = "java.util.ConcurrentModificationException";
SS[214] = "java.lang.UnsupportedOperationException";
SS[215] = "java.lang.jtransc.JTranscCoreReflection";
SS[216] = "j.ProgramReflection";
SS[217] = "j.ClassInfo";
SS[218] = "j.ProgramReflection$AllClasses";
SS[219] = "java.lang.reflect.ParameterizedType";
SS[220] = "java.lang.StackTraceElement";
SS[221] = "java.lang.SystemInt";
SS[222] = "java.util.Objects";
SS[223] = "java.lang.ClassCastException";
SS[224] = "java.util.Map";
SS[225] = "java.util.Set";
SS[226] = "java.util.Map$Entry";
SS[227] = "com.jtransc.JTranscWrapped";
SS[228] = "Benchmark$37";
SS[229] = "Benchmark$Task";
SS[230] = "Benchmark$MyClass2";
SS[231] = "Benchmark$36";
SS[232] = "Benchmark$35";
SS[233] = "Benchmark$34";
SS[234] = "com.jtransc.JTranscSystem";
SS[235] = "Benchmark$39";
SS[236] = "Benchmark$38";
SS[237] = "java.util.Random";
SS[238] = "Benchmark$33";
SS[239] = "Benchmark$32";
SS[240] = "Benchmark$31";
SS[241] = "Benchmark$30";
SS[242] = "java.lang.Runtime";
SS[243] = "Benchmark$40";
SS[244] = "Benchmark$1";
SS[245] = "Benchmark$2";
SS[246] = "Benchmark$3";
SS[247] = "Benchmark$44";
SS[248] = "Benchmark$4";
SS[249] = "Benchmark$43";
SS[250] = "Benchmark$5";
SS[251] = "Benchmark$42";
SS[252] = "Benchmark$6";
SS[253] = "Benchmark$41";
SS[254] = "Benchmark$7";
SS[255] = "Benchmark$8";
SS[256] = "Benchmark$9";
SS[257] = "Benchmark$15";
SS[258] = "Benchmark$14";
SS[259] = "Benchmark$13";
SS[260] = "Benchmark$12";
SS[261] = "Benchmark$19";
SS[262] = "Benchmark$18";
SS[263] = "Benchmark$17";
SS[264] = "Benchmark$16";
SS[265] = "Benchmark$11";
SS[266] = "Benchmark$10";
SS[267] = "Benchmark$26";
SS[268] = "Benchmark$25";
SS[269] = "Benchmark$24";
SS[270] = "Benchmark$23";
SS[271] = "Benchmark$29";
SS[272] = "Benchmark$28";
SS[273] = "Benchmark$27";
SS[274] = "Benchmark$22";
SS[275] = "Benchmark$21";
SS[276] = "Benchmark$20";
SS[277] = "com.jtransc.JTranscVersion";
SS[278] = "com.jtransc.time.JTranscClock";
SS[279] = "com.jtransc.time.JTranscClock$1";
SS[280] = "com.jtransc.time.JTranscClock$Impl";
SS[281] = "java.io.IOException";
SS[282] = "java.io.ByteArrayOutputStream";
SS[283] = "com.jtransc.charset.JTranscCharBuffer";
SS[284] = "java.nio.charset.UnsupportedCharsetException";
SS[285] = "java.util.ServiceLoader";
SS[286] = "com.jtransc.charset.charsets.JTranscCharsetIBM866";
SS[287] = "com.jtransc.charset.JTranscCharsetSingleByte";
SS[288] = "java.util.HashMap";
SS[289] = "java.util.AbstractMap";
SS[290] = "java.util.HashMap$HashMapEntry";
SS[291] = "java.util.HashMap$1";
SS[292] = "java.util.HashMap$EntrySet";
SS[293] = "java.util.AbstractSet";
SS[294] = "java.util.HashMap$EntryIterator";
SS[295] = "java.util.HashMap$HashIterator";
SS[296] = "java.lang.AssertionError";
SS[297] = "java.lang.CloneNotSupportedException";
SS[298] = "java.util.Collections";
SS[299] = "java.util.Collections$1";
SS[300] = "java.util.Collections$EmptyList";
SS[301] = "java.util.Collections$2";
SS[302] = "java.util.Enumeration";
SS[303] = "java.util.Collections$EmptyMap";
SS[304] = "java.util.Collections$EmptySet";
SS[305] = "com.jtransc.charset.charsets.JTranscCharsetUTF8";
SS[306] = "com.jtransc.charset.charsets.JTranscCharsetUSASCII";
SS[307] = "com.jtransc.charset.charsets.JTranscCharsetUTF16LE";
SS[308] = "com.jtransc.charset.charsets.JTranscCharsetUTF16Base";
SS[309] = "com.jtransc.mix.JTranscProcessMulti";
SS[310] = "java.lang.Process";
SS[311] = "com.jtransc.mix.JTranscProcessMulti$Creator";
SS[312] = "com.jtransc.charset.charsets.JTranscCharsetLatin1";
SS[313] = "com.jtransc.charset.charsets.JTranscCharsetUTF16BE";
SS[314] = "java.util.Arrays$ArrayList";
SS[315] = "java.lang.reflect.Array";
SS[316] = "java.lang.SafeVarargs";
SS[317] = "java.lang.annotation.Annotation";
SS[318] = "java.lang.Thread";
SS[319] = "java.lang.Runnable";
SS[320] = "java.lang.ThreadGroup";
SS[321] = "java.lang.Thread$UncaughtExceptionHandler";
SS[322] = "java.lang.ClassLoader";
SS[323] = "java.lang._ClassInternalUtils";
SS[324] = "java.lang._ClassInternalUtils$1";
SS[325] = "com.jtransc.JTranscArrays";
SS[326] = "com.jtransc.JTranscSystemProperties";
SS[327] = "Benchmark$MyClass";
SS[328] = "java.nio.ByteBuffer";
SS[329] = "java.nio.Buffer";
SS[330] = "java.nio.internal.MemoryBlock";
SS[331] = "java.nio.ReadOnlyBufferException";
SS[332] = "java.util.zip.CRC32";
SS[333] = "java.util.zip.Checksum";
SS[334] = "java.nio.ByteOrder";
SS[335] = "java.nio.DoubleBuffer";
SS[336] = "java.nio.LongBuffer";
SS[337] = "java.nio.ByteBufferAsLongBuffer";
SS[338] = "java.nio.internal.ByteBufferAs";
SS[339] = "java.nio.ByteBufferAsLongBuffer$LE";
SS[340] = "java.nio.ByteBufferAsLongBuffer$1";
SS[341] = "java.nio.ByteBufferAsLongBuffer$BE";
SS[342] = "java.nio.ByteBufferAsDoubleBuffer";
SS[343] = "java.nio.ByteBufferAsDoubleBuffer$LE";
SS[344] = "java.nio.ByteBufferAsDoubleBuffer$1";
SS[345] = "java.nio.ByteBufferAsDoubleBuffer$BE";
SS[346] = "libcore.io.Memory";
SS[347] = "java.nio.CharBuffer";
SS[348] = "java.lang.Readable";
SS[349] = "java.nio.ShortBuffer";
SS[350] = "java.nio.ByteBufferAsShortBuffer";
SS[351] = "java.nio.ByteBufferAsShortBuffer$LE";
SS[352] = "java.nio.ByteBufferAsShortBuffer$1";
SS[353] = "java.nio.ByteBufferAsShortBuffer$BE";
SS[354] = "java.nio.ByteBufferAsCharBuffer";
SS[355] = "java.nio.ByteBufferAsCharBuffer$LE";
SS[356] = "java.nio.ByteBufferAsCharBuffer$1";
SS[357] = "java.nio.ByteBufferAsCharBuffer$BE";
SS[358] = "java.nio.IntBuffer";
SS[359] = "java.nio.FloatBuffer";
SS[360] = "java.nio.ByteBufferAsFloatBuffer";
SS[361] = "java.nio.ByteBufferAsFloatBuffer$BE";
SS[362] = "java.nio.ByteBufferAsFloatBuffer$LE";
SS[363] = "java.nio.ByteBufferAsIntBuffer";
SS[364] = "java.nio.ByteBufferAsIntBuffer$BE";
SS[365] = "java.nio.ByteBufferAsIntBuffer$LE";
SS[366] = "com.jtransc.compression.jzlib.Deflate$Config";
SS[367] = "java.util.zip.Deflater";
SS[368] = "Benchmark$Test1";
SS[369] = "Benchmark$Test2";
SS[370] = "java.util.Properties";
SS[371] = "java.util.Hashtable";
SS[372] = "java.util.Dictionary";
SS[373] = "java.util.Hashtable$HashtableEntry";
SS[374] = "java.util.Hashtable$1";
SS[375] = "java.util.Hashtable$EntrySet";
SS[376] = "java.util.Hashtable$EntryIterator";
SS[377] = "java.util.Hashtable$HashIterator";
SS[378] = "java.io.ObjectStreamField";
SS[379] = "java.lang.ref.WeakReference";
SS[380] = "java.lang.ref.Reference";
SS[381] = "java.lang.ref.ReferenceQueue";
SS[382] = "java.lang.SafeVarargs$Impl";
SS[383] = "java.lang.annotation.Annotation$Impl";
SS[384] = ", Size: ";
SS[385] = "Index: ";
SS[386] = "Can't read more";
SS[387] = " but found end";
SS[388] = "Expected ";
SS[389] = " ";
SS[390] = "byte";
SS[391] = "boolean";
SS[392] = "true";
SS[393] = "false";
SS[394] = "short";
SS[395] = "long";
SS[396] = "0";
SS[397] = "-9223372036854775808";
SS[398] = "-";
SS[399] = "double";
SS[400] = "out == null";
SS[401] = "os.arch";
SS[402] = "unknown";
SS[403] = "os.name";
SS[404] = "os.version";
SS[405] = "0.1";
SS[406] = "java.runtime.name";
SS[407] = "jtransc-unknown";
SS[408] = "java.version";
SS[409] = "1.8.0_51";
SS[410] = "java.vm.version";
SS[411] = "25.51-b03";
SS[412] = "java.runtime.version";
SS[413] = "1.8.0_51-b16";
SS[414] = "file.separator";
SS[415] = "line.separator";
SS[416] = "path.separator";
SS[417] = "file.encoding";
SS[418] = "java.home";
SS[419] = "java.specification.name";
SS[420] = "java.specification.vendor";
SS[421] = "jtransc";
SS[422] = "java.specification.version";
SS[423] = "1.7";
SS[424] = "java.vendor";
SS[425] = "java.vendor.url";
SS[426] = "http://github.com/jtransc/jtransc";
SS[427] = "java.vm.name";
SS[428] = "haxe";
SS[429] = "java.vm.specification.name";
SS[430] = "Jtransc JVM emulator";
SS[431] = "java.vm.specification.vendor";
SS[432] = "java.vm.specification.version";
SS[433] = "java.io.tmpdir";
SS[434] = "user.home";
SS[435] = "user.dir";
SS[436] = "user.name";
SS[437] = "user.language";
SS[438] = "user.region";
SS[439] = "user.variant";
SS[440] = "char";
SS[441] = "NaN";
SS[442] = "-Infinity";
SS[443] = "Infinity";
SS[444] = "-0.0";
SS[445] = "e+";
SS[446] = "E";
SS[447] = "e-";
SS[448] = "E-";
SS[449] = ".0";
SS[450] = "float";
SS[451] = "void";
SS[452] = "' of ";
SS[453] = "Can't parse type '";
SS[454] = "'";
SS[455] = "Class_forName0: Can't find class '";
SS[456] = "int";
SS[457] = "public ";
SS[458] = "protected ";
SS[459] = "private ";
SS[460] = "abstract ";
SS[461] = "static ";
SS[462] = "final ";
SS[463] = "transient ";
SS[464] = "volatile ";
SS[465] = "synchronized ";
SS[466] = "native ";
SS[467] = "strictfp ";
SS[468] = "interface ";
SS[469] = "Class constructor: Can't find class '";
SS[470] = "class ";
SS[471] = "L";
SS[472] = ";";
SS[473] = "Couldn't find class ";
SS[474] = "null";
SS[475] = " - ";
SS[476] = "JTransc ";
SS[477] = "Java ";
SS[478] = ", totalMemory: ";
SS[479] = ", maxMemory: ";
SS[480] = "freeMemory: ";
SS[481] = "Benchmarking:";
SS[482] = "plain loops";
SS[483] = "shift left constant";
SS[484] = "shift right constant";
SS[485] = "shift unsigned right constant";
SS[486] = "shift left constant long";
SS[487] = "shift right constant long";
SS[488] = "shift unsigned right constant long";
SS[489] = "left shift";
SS[490] = "right shift";
SS[491] = "right unsigned shift";
SS[492] = "call static mult";
SS[493] = "call instance mult";
SS[494] = "call instance div";
SS[495] = "instanceof classes";
SS[496] = "arraycopy int";
SS[497] = "write byte[]";
SS[498] = "write short[]";
SS[499] = "write char[]";
SS[500] = "write int[]";
SS[501] = "write float[]";
SS[502] = "write double[]";
SS[503] = "String Builder 1";
SS[504] = "String Builder 2";
SS[505] = "long arithmetic";
SS[506] = "simd mutable";
SS[507] = "simd immutable";
SS[508] = "simd mutable matrix mult";
SS[509] = "StringBuilder1";
SS[510] = "StringBuilder2";
SS[511] = "Non Direct Buffer";
SS[512] = "Direct Buffer Int/float";
SS[513] = "Direct Buffer Short/Char";
SS[514] = "Direct Buffer Double/Long";
SS[515] = "FastMemory";
SS[516] = "Create Instances1 local";
SS[517] = "Create Instances2 local";
SS[518] = "Create Instances2 global";
SS[519] = "Create Instances with builder";
SS[520] = "Java's CRC32";
SS[521] = "jzlib's CRC32";
SS[522] = "compress java's Deflate";
SS[523] = "compress jzlib";
SS[524] = "random";
SS[525] = "exception";
SS[526] = "TOTAL time: ";
SS[527] = "...";
SS[528] = "ERROR";

function java_lang_annotation_Annotation() {
}
java_lang_annotation_Annotation.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_annotation_Annotation.prototype.constructor = java_lang_annotation_Annotation;
java_lang_annotation_Annotation.SI = function(){};
java_lang_annotation_Annotation.prototype.__JT__CLASS_ID = java_lang_annotation_Annotation.__JT__CLASS_ID = 842;
java_lang_annotation_Annotation.prototype.__JT__CLASS_IDS = java_lang_annotation_Annotation.__JT__CLASS_IDS = [842,656];
function java_lang_Object() {
}
java_lang_Object.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_Object.prototype.constructor = java_lang_Object;
java_lang_Object.prototype.___id = 0;
java_lang_Object.SI = function(){};
java_lang_Object.prototype.__JT__CLASS_ID = java_lang_Object.__JT__CLASS_ID = 656;
java_lang_Object.prototype.__JT__CLASS_IDS = java_lang_Object.__JT__CLASS_IDS = [656];
java_lang_Object.prototype["java.lang.Object<init>()V"] = function() { 
	return this;
	return this;
};
java_lang_Object.prototype["toString()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_StringBuilder()));
	fA0 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this["getClass()Ljava/lang/Class;"]()["getName()Ljava/lang/String;"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[0])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_Integer["toHexString(I)Ljava/lang/String;"](this["hashCode()I"]()))["toString()Ljava/lang/String;"]();
};
java_lang_Object.prototype["getClass()Ljava/lang/Class;"] = function() { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!(java_lang_jtransc_JTranscCoreReflection["isArray(Ljava/lang/Object;)Z"](this))) {
					_G = 1;
					continue;
				}
				return java_lang_jtransc_JTranscCoreReflection["getClassByName(Ljava/lang/String;)Ljava/lang/Class;"](java_lang_jtransc_JTranscCoreReflection["getArrayDescriptor(Ljava/lang/Object;)Ljava/lang/String;"](this));
			case 1:
				return java_lang_jtransc_JTranscCoreReflection["getClassById(I)Ljava/lang/Class;"](java_lang_jtransc_JTranscCoreReflection["getClassId(Ljava/lang/Object;)I"](this));
			default:
				break;
		}
	}
	return null;
};
java_lang_Object.prototype["clone()Ljava/lang/Object;"] = function() { 
	return N.clone(this);
};
java_lang_Object.prototype["hashCode()I"] = function() { 
	return java_lang_SystemInt["identityHashCode(Ljava/lang/Object;)I"](this);
};
java_lang_Object.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this != p0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
function java_lang_annotation_Annotation$Impl() {
}
java_lang_annotation_Annotation$Impl.prototype = Object.create(java_lang_Object.prototype);
java_lang_annotation_Annotation$Impl.prototype.constructor = java_lang_annotation_Annotation$Impl;
java_lang_annotation_Annotation$Impl.prototype.___id = 0;
java_lang_annotation_Annotation$Impl.SI = function(){};
java_lang_annotation_Annotation$Impl.prototype.__JT__CLASS_ID = java_lang_annotation_Annotation$Impl.__JT__CLASS_ID = 925;
java_lang_annotation_Annotation$Impl.prototype.__JT__CLASS_IDS = java_lang_annotation_Annotation$Impl.__JT__CLASS_IDS = [925,656,842];
java_lang_annotation_Annotation$Impl.prototype["java.lang.annotation.Annotation$Impl<init>()V"] = function() { 
	return this;
};
java_lang_annotation_Annotation$Impl.prototype["annotationType()Ljava/lang/Class;"] = function() { 
	return N.resolveClass("Ljava/lang/annotation/Annotation;");
};
java_lang_annotation_Annotation$Impl.prototype["getClass()Ljava/lang/Class;"] = function() { 
	return N.resolveClass("Ljava/lang/annotation/Annotation;");
};
java_lang_annotation_Annotation$Impl.prototype["toString()Ljava/lang/String;"] = function() { 
	return (new java_lang_StringBuilder())["java.lang.StringBuilder<init>()V"]()["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"](S[1])["toString()Ljava/lang/String;"]();
};
function java_lang_SafeVarargs() {
}
java_lang_SafeVarargs.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_SafeVarargs.prototype.constructor = java_lang_SafeVarargs;
java_lang_SafeVarargs.SI = function(){};
java_lang_SafeVarargs.prototype.__JT__CLASS_ID = java_lang_SafeVarargs.__JT__CLASS_ID = 841;
java_lang_SafeVarargs.prototype.__JT__CLASS_IDS = java_lang_SafeVarargs.__JT__CLASS_IDS = [841,842,656];
function java_lang_SafeVarargs$Impl() {
}
java_lang_SafeVarargs$Impl.prototype = Object.create(java_lang_Object.prototype);
java_lang_SafeVarargs$Impl.prototype.constructor = java_lang_SafeVarargs$Impl;
java_lang_SafeVarargs$Impl.prototype.___id = 0;
java_lang_SafeVarargs$Impl.SI = function(){};
java_lang_SafeVarargs$Impl.prototype.__JT__CLASS_ID = java_lang_SafeVarargs$Impl.__JT__CLASS_ID = 924;
java_lang_SafeVarargs$Impl.prototype.__JT__CLASS_IDS = java_lang_SafeVarargs$Impl.__JT__CLASS_IDS = [924,656,841,842];
java_lang_SafeVarargs$Impl.prototype["java.lang.SafeVarargs$Impl<init>()V"] = function() { 
	return this;
};
java_lang_SafeVarargs$Impl.prototype["annotationType()Ljava/lang/Class;"] = function() { 
	return N.resolveClass("Ljava/lang/SafeVarargs;");
};
java_lang_SafeVarargs$Impl.prototype["getClass()Ljava/lang/Class;"] = function() { 
	return N.resolveClass("Ljava/lang/SafeVarargs;");
};
java_lang_SafeVarargs$Impl.prototype["toString()Ljava/lang/String;"] = function() { 
	return (new java_lang_StringBuilder())["java.lang.StringBuilder<init>()V"]()["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"](S[1])["toString()Ljava/lang/String;"]();
};
function java_lang_ref_ReferenceQueue() {
}
java_lang_ref_ReferenceQueue.prototype = Object.create(java_lang_Object.prototype);
java_lang_ref_ReferenceQueue.prototype.constructor = java_lang_ref_ReferenceQueue;
java_lang_ref_ReferenceQueue.prototype.___id = 0;
java_lang_ref_ReferenceQueue.SI = function(){};
java_lang_ref_ReferenceQueue.prototype.__JT__CLASS_ID = java_lang_ref_ReferenceQueue.__JT__CLASS_ID = 923;
java_lang_ref_ReferenceQueue.prototype.__JT__CLASS_IDS = java_lang_ref_ReferenceQueue.__JT__CLASS_IDS = [923,656];
java_lang_ref_ReferenceQueue.prototype["java.lang.ref.ReferenceQueue<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
// ABSTRACT
function java_lang_ref_Reference() {
}
java_lang_ref_Reference.prototype = Object.create(java_lang_Object.prototype);
java_lang_ref_Reference.prototype.constructor = java_lang_ref_Reference;
java_lang_ref_Reference.prototype._referent = null;
java_lang_ref_Reference.prototype._queue = null;
java_lang_ref_Reference.prototype.___id = 0;
java_lang_ref_Reference.SI = function(){};
java_lang_ref_Reference.prototype.__JT__CLASS_ID = java_lang_ref_Reference.__JT__CLASS_ID = 922;
java_lang_ref_Reference.prototype.__JT__CLASS_IDS = java_lang_ref_Reference.__JT__CLASS_IDS = [922,656];
java_lang_ref_Reference.prototype["java.lang.ref.Reference<init>(Ljava/lang/Object;)V"] = function(p0) { 
	this["java.lang.ref.Reference<init>(Ljava/lang/Object;Ljava/lang/ref/ReferenceQueue;)V"](p0, null);
	return this;
	return this;
};
java_lang_ref_Reference.prototype["java.lang.ref.Reference<init>(Ljava/lang/Object;Ljava/lang/ref/ReferenceQueue;)V"] = function(p0, p1) { 
	(this)["java.lang.Object<init>()V"]();
	this._referent = p0;
	this._queue = p1;
	return this;
	return this;
};
java_lang_ref_Reference.prototype["get()Ljava/lang/Object;"] = function() { 
	return this._referent;
};
function java_lang_ref_WeakReference() {
}
java_lang_ref_WeakReference.prototype = Object.create(java_lang_ref_Reference.prototype);
java_lang_ref_WeakReference.prototype.constructor = java_lang_ref_WeakReference;
java_lang_ref_WeakReference.prototype._referent = null;
java_lang_ref_WeakReference.prototype._queue = null;
java_lang_ref_WeakReference.SI = function(){};
java_lang_ref_WeakReference.prototype.__JT__CLASS_ID = java_lang_ref_WeakReference.__JT__CLASS_ID = 921;
java_lang_ref_WeakReference.prototype.__JT__CLASS_IDS = java_lang_ref_WeakReference.__JT__CLASS_IDS = [921,922,656];
java_lang_ref_WeakReference.prototype["java.lang.ref.WeakReference<init>(Ljava/lang/Object;)V"] = function(p0) { 
	(this)["java.lang.ref.Reference<init>(Ljava/lang/Object;)V"](p0);
	return this;
	return this;
};
java_lang_ref_WeakReference.prototype["java.lang.ref.WeakReference<init>(Ljava/lang/Object;Ljava/lang/ref/ReferenceQueue;)V"] = function(p0, p1) { 
	(this)["java.lang.ref.Reference<init>(Ljava/lang/Object;Ljava/lang/ref/ReferenceQueue;)V"](p0, p1);
	return this;
	return this;
};
function java_lang_Comparable() {
}
java_lang_Comparable.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_Comparable.prototype.constructor = java_lang_Comparable;
java_lang_Comparable.SI = function(){};
java_lang_Comparable.prototype.__JT__CLASS_ID = java_lang_Comparable.__JT__CLASS_ID = 659;
java_lang_Comparable.prototype.__JT__CLASS_IDS = java_lang_Comparable.__JT__CLASS_IDS = [659,656];
function java_io_ObjectStreamField() {
}
java_io_ObjectStreamField.prototype = Object.create(java_lang_Object.prototype);
java_io_ObjectStreamField.prototype.constructor = java_io_ObjectStreamField;
java_io_ObjectStreamField.prototype.__name = null;
java_io_ObjectStreamField.prototype._type = null;
java_io_ObjectStreamField.prototype.___id = 0;
java_io_ObjectStreamField.SI = function(){};
java_io_ObjectStreamField.prototype.__JT__CLASS_ID = java_io_ObjectStreamField.__JT__CLASS_ID = 920;
java_io_ObjectStreamField.prototype.__JT__CLASS_IDS = java_io_ObjectStreamField.__JT__CLASS_IDS = [920,656,659];
java_io_ObjectStreamField.prototype["java.io.ObjectStreamField<init>(Ljava/lang/String;Ljava/lang/Class;)V"] = function(p0, p1) { 
	var _G = 0, fA0 = null, fA1 = null, tA0 = null, tA1 = null, tA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.lang.Object<init>()V"]();
				if ((((p0) != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_NullPointerException()));
				fA0 = tA0;
				(tA0)["java.lang.NullPointerException<init>(Ljava/lang/String;)V"](S[2]);
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				if ((((p1) != null))) {
					_G = 2;
					continue;
				}
				tA1 = ((new java_lang_NullPointerException()));
				fA0 = tA1;
				(tA1)["java.lang.NullPointerException<init>(Ljava/lang/String;)V"](S[3]);
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				this.__name = p0;
				fA0 = (this);
				tA2 = ((new java_lang_ref_WeakReference()));
				fA1 = tA2;
				(tA2)["java.lang.ref.WeakReference<init>(Ljava/lang/Object;)V"]((p1));
				(fA0)._type = fA1;
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
java_io_ObjectStreamField.prototype["toString()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_StringBuilder()));
	fA0 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"]((this)["getClass()Ljava/lang/Class;"]()["getName()Ljava/lang/String;"]())["append(C)Ljava/lang/StringBuilder;"](40)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this["getName()Ljava/lang/String;"]())["append(C)Ljava/lang/StringBuilder;"](58)["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"]((this["getTypeInternal()Ljava/lang/Class;"]()))["append(C)Ljava/lang/StringBuilder;"](41)["toString()Ljava/lang/String;"]();
};
java_io_ObjectStreamField.prototype["getName()Ljava/lang/String;"] = function() { 
	return this.__name;
};
java_io_ObjectStreamField.prototype["getTypeInternal()Ljava/lang/Class;"] = function() { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!((this._type instanceof java_lang_ref_WeakReference))) {
					_G = 1;
					continue;
				}
				return N.checkCast(N.checkCast(this._type, java_lang_ref_WeakReference)["get()Ljava/lang/Object;"](), java_lang_Class);
			case 1:
				return N.checkCast(this._type, java_lang_Class);
			default:
				break;
		}
	}
	return null;
};
// ABSTRACT
function java_util_Hashtable$HashIterator() {
}
java_util_Hashtable$HashIterator.prototype = Object.create(java_lang_Object.prototype);
java_util_Hashtable$HashIterator.prototype.constructor = java_util_Hashtable$HashIterator;
java_util_Hashtable$HashIterator.prototype._expectedModCount = 0;
java_util_Hashtable$HashIterator.prototype._nextIndex = 0;
java_util_Hashtable$HashIterator.prototype._this_0 = null;
java_util_Hashtable$HashIterator.prototype._nextEntry = null;
java_util_Hashtable$HashIterator.prototype._lastEntryReturned = null;
java_util_Hashtable$HashIterator.prototype.___id = 0;
java_util_Hashtable$HashIterator.SI = function(){};
java_util_Hashtable$HashIterator.prototype.__JT__CLASS_ID = java_util_Hashtable$HashIterator.__JT__CLASS_ID = 919;
java_util_Hashtable$HashIterator.prototype.__JT__CLASS_IDS = java_util_Hashtable$HashIterator.__JT__CLASS_IDS = [919,656];
java_util_Hashtable$HashIterator.prototype["java.util.Hashtable$HashIterator<init>(Ljava/util/Hashtable;)V"] = function(p0) { 
	var _G = 0, lA2 = null, lA3 = null, fI1 = 0, fA0 = null, fA1 = null, tI1 = 0, tA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				this._this_0 = p0;
				(this)["java.lang.Object<init>()V"]();
				this._expectedModCount = java_util_Hashtable["access$500(Ljava/util/Hashtable;)I"](this._this_0);
				lA2 = (java_util_Hashtable["access$600(Ljava/util/Hashtable;)[Ljava/util/Hashtable$HashtableEntry;"](p0));
				lA3 = null;
				_G = 1;
				continue;
			case 1:
				if (((lA3 != null))) {
					_G = 2;
					continue;
				}
				if (((this._nextIndex >= lA2.length))) {
					_G = 2;
					continue;
				}
				fA0 = lA2;
				fA1 = (this);
				tA2 = fA1;
				tI1 = this._nextIndex;
				fI1 = tI1;
				(tA2)._nextIndex = (((tI1 + 1))|0);
				lA3 = ((fA0).data[fI1]);
				_G = 1;
				continue;
			case 2:
				this._nextEntry = (lA3);
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
java_util_Hashtable$HashIterator.prototype["hasNext()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._nextEntry == null))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_util_Hashtable$HashIterator.prototype["nextEntry()Ljava/util/Hashtable$HashtableEntry;"] = function() { 
	var _G = 0, lA1 = null, lA2 = null, lA3 = null, fA0 = null, fA1 = null, tA0 = null, tA1 = null, tA4 = null, tA6 = null, tA5 = null, fI1 = 0, tI3 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((java_util_Hashtable["access$500(Ljava/util/Hashtable;)I"](this._this_0) == this._expectedModCount))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_util_ConcurrentModificationException()));
				fA0 = tA0;
				(tA0)["java.util.ConcurrentModificationException<init>()V"]();
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				if (((this._nextEntry != null))) {
					_G = 2;
					continue;
				}
				tA1 = ((new java_util_NoSuchElementException()));
				fA0 = tA1;
				(tA1)["java.util.NoSuchElementException<init>()V"]();
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				lA1 = (this._nextEntry);
				lA2 = (java_util_Hashtable["access$600(Ljava/util/Hashtable;)[Ljava/util/Hashtable$HashtableEntry;"](this._this_0));
				lA3 = ((lA1)._next);
				_G = 3;
				continue;
			case 3:
				if (((lA3 != null))) {
					_G = 4;
					continue;
				}
				if (((this._nextIndex >= lA2.length))) {
					_G = 4;
					continue;
				}
				fA0 = lA2;
				fA1 = (this);
				tA4 = fA1;
				tI3 = this._nextIndex;
				fI1 = tI3;
				(tA4)._nextIndex = (((tI3 + 1))|0);
				lA3 = ((fA0).data[fI1]);
				_G = 3;
				continue;
			case 4:
				this._nextEntry = (lA3);
				fA0 = (this);
				tA6 = fA0;
				tA5 = lA1;
				fA0 = tA5;
				(tA6)._lastEntryReturned = (tA5);
				return (fA0);
			default:
				break;
		}
	}
	return null;
};
function java_util_Iterator() {
}
java_util_Iterator.prototype = Object.create(java_lang_Object_base.prototype);
java_util_Iterator.prototype.constructor = java_util_Iterator;
java_util_Iterator.SI = function(){};
java_util_Iterator.prototype.__JT__CLASS_ID = java_util_Iterator.__JT__CLASS_ID = 729;
java_util_Iterator.prototype.__JT__CLASS_IDS = java_util_Iterator.__JT__CLASS_IDS = [729,656];
java_util_Iterator.prototype["next()Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.Iterator.next') };
java_util_Iterator.prototype["hasNext()Z"] = function() { N.methodWithoutBody('java.util.Iterator.hasNext') };
function java_util_Hashtable$EntryIterator() {
}
java_util_Hashtable$EntryIterator.prototype = Object.create(java_util_Hashtable$HashIterator.prototype);
java_util_Hashtable$EntryIterator.prototype.constructor = java_util_Hashtable$EntryIterator;
java_util_Hashtable$EntryIterator.prototype._this_0_ = null;
java_util_Hashtable$EntryIterator.prototype._expectedModCount = 0;
java_util_Hashtable$EntryIterator.prototype._nextIndex = 0;
java_util_Hashtable$EntryIterator.prototype._this_0 = null;
java_util_Hashtable$EntryIterator.prototype._nextEntry = null;
java_util_Hashtable$EntryIterator.prototype._lastEntryReturned = null;
java_util_Hashtable$EntryIterator.SI = function(){};
java_util_Hashtable$EntryIterator.prototype.__JT__CLASS_ID = java_util_Hashtable$EntryIterator.__JT__CLASS_ID = 918;
java_util_Hashtable$EntryIterator.prototype.__JT__CLASS_IDS = java_util_Hashtable$EntryIterator.__JT__CLASS_IDS = [918,919,656,729];
java_util_Hashtable$EntryIterator.prototype["java.util.Hashtable$EntryIterator<init>(Ljava/util/Hashtable;)V"] = function(p0) { 
	this._this_0_ = p0;
	(this)["java.util.Hashtable$HashIterator<init>(Ljava/util/Hashtable;)V"](p0);
	return this;
	return this;
};
java_util_Hashtable$EntryIterator.prototype["java.util.Hashtable$EntryIterator<init>(Ljava/util/Hashtable;Ljava/util/Hashtable$1;)V"] = function(p0, p1) { 
	this["java.util.Hashtable$EntryIterator<init>(Ljava/util/Hashtable;)V"](p0);
	return this;
	return this;
};
java_util_Hashtable$EntryIterator.prototype["next()Ljava/lang/Object;"] = function() { 
	return (this["next()Ljava/util/Map$Entry;"]());
};
java_util_Hashtable$EntryIterator.prototype["next()Ljava/util/Map$Entry;"] = function() { 
	return (this["nextEntry()Ljava/util/Hashtable$HashtableEntry;"]());
};
function java_lang_Iterable() {
}
java_lang_Iterable.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_Iterable.prototype.constructor = java_lang_Iterable;
java_lang_Iterable.SI = function(){};
java_lang_Iterable.prototype.__JT__CLASS_ID = java_lang_Iterable.__JT__CLASS_ID = 724;
java_lang_Iterable.prototype.__JT__CLASS_IDS = java_lang_Iterable.__JT__CLASS_IDS = [724,656];
java_lang_Iterable.prototype["iterator()Ljava/util/Iterator;"] = function() { N.methodWithoutBody('java.lang.Iterable.iterator') };
function java_util_Collection() {
}
java_util_Collection.prototype = Object.create(java_lang_Object_base.prototype);
java_util_Collection.prototype.constructor = java_util_Collection;
java_util_Collection.SI = function(){};
java_util_Collection.prototype.__JT__CLASS_ID = java_util_Collection.__JT__CLASS_ID = 723;
java_util_Collection.prototype.__JT__CLASS_IDS = java_util_Collection.__JT__CLASS_IDS = [723,724,656];
java_util_Collection.prototype["size()I"] = function() { N.methodWithoutBody('java.util.Collection.size') };
java_util_Collection.prototype["isEmpty()Z"] = function() { N.methodWithoutBody('java.util.Collection.isEmpty') };
java_util_Collection.prototype["iterator()Ljava/util/Iterator;"] = function() { N.methodWithoutBody('java.util.Collection.iterator') };
java_util_Collection.prototype["add(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.Collection.add') };
java_util_Collection.prototype["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.Collection.toArray') };
java_util_Collection.prototype["hashCode()I"] = function() { N.methodWithoutBody('java.util.Collection.hashCode') };
java_util_Collection.prototype["equals(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.Collection.equals') };
java_util_Collection.prototype["contains(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.Collection.contains') };
java_util_Collection.prototype["containsAll(Ljava/util/Collection;)Z"] = function() { N.methodWithoutBody('java.util.Collection.containsAll') };
function java_util_Set() {
}
java_util_Set.prototype = Object.create(java_lang_Object_base.prototype);
java_util_Set.prototype.constructor = java_util_Set;
java_util_Set.SI = function(){};
java_util_Set.prototype.__JT__CLASS_ID = java_util_Set.__JT__CLASS_ID = 747;
java_util_Set.prototype.__JT__CLASS_IDS = java_util_Set.__JT__CLASS_IDS = [747,723,724,656];
java_util_Set.prototype["size()I"] = function() { N.methodWithoutBody('java.util.Set.size') };
java_util_Set.prototype["isEmpty()Z"] = function() { N.methodWithoutBody('java.util.Set.isEmpty') };
java_util_Set.prototype["iterator()Ljava/util/Iterator;"] = function() { N.methodWithoutBody('java.util.Set.iterator') };
java_util_Set.prototype["add(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.Set.add') };
java_util_Set.prototype["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.Set.toArray') };
java_util_Set.prototype["hashCode()I"] = function() { N.methodWithoutBody('java.util.Set.hashCode') };
java_util_Set.prototype["equals(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.Set.equals') };
java_util_Set.prototype["contains(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.Set.contains') };
java_util_Set.prototype["containsAll(Ljava/util/Collection;)Z"] = function() { N.methodWithoutBody('java.util.Set.containsAll') };
// ABSTRACT
function java_util_AbstractCollection() {
}
java_util_AbstractCollection.prototype = Object.create(java_lang_Object.prototype);
java_util_AbstractCollection.prototype.constructor = java_util_AbstractCollection;
java_util_AbstractCollection.prototype.___id = 0;
java_util_AbstractCollection.SI = function(){};
java_util_AbstractCollection.prototype.__JT__CLASS_ID = java_util_AbstractCollection.__JT__CLASS_ID = 728;
java_util_AbstractCollection.prototype.__JT__CLASS_IDS = java_util_AbstractCollection.__JT__CLASS_IDS = [728,656,723,724];
java_util_AbstractCollection.prototype["java.util.AbstractCollection<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_util_AbstractCollection.prototype["toString()Ljava/lang/String;"] = function() { 
	var _G = 0, lA1 = null, lA3 = null, fA0 = null, tA0 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(this["isEmpty()Z"]())) {
					_G = 1;
					continue;
				}
				return S[4];
			case 1:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>(I)V"](((Math.imul(this["size()I"](), 16))|0));
				lA1 = fA0;
				(lA1)["append(C)Ljava/lang/StringBuilder;"](91);
				lA2 = this["iterator()Ljava/util/Iterator;"]();
				_G = 2;
				continue;
			case 2:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				lA3 = lA2["next()Ljava/lang/Object;"]();
				if (((lA3 == (this)))) {
					_G = 4;
					continue;
				}
				(lA1)["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"](lA3);
				_G = 5;
				continue;
			case 4:
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[5]);
				_G = 5;
				continue;
			case 5:
				if (!(lA2["hasNext()Z"]())) {
					_G = 6;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[6]);
				_G = 6;
				continue;
			case 6:
				_G = 2;
				continue;
			case 3:
				(lA1)["append(C)Ljava/lang/StringBuilder;"](93);
				return (lA1)["toString()Ljava/lang/String;"]();
			default:
				break;
		}
	}
	return null;
};
java_util_AbstractCollection.prototype["size()I"] = function() { N.methodWithoutBody('java.util.AbstractCollection.size') };
java_util_AbstractCollection.prototype["isEmpty()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this["size()I"]() != 0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_util_AbstractCollection.prototype["iterator()Ljava/util/Iterator;"] = function() { N.methodWithoutBody('java.util.AbstractCollection.iterator') };
java_util_AbstractCollection.prototype["add(Ljava/lang/Object;)Z"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_UnsupportedOperationException()));
	fA0 = tA0;
	(tA0)["java.lang.UnsupportedOperationException<init>()V"]();
	throw new WrappedError(fA0);
};
java_util_AbstractCollection.prototype["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"] = function(p0) { 
	return this["toArrayList()Ljava/util/ArrayList;"]()["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"](p0);
};
java_util_AbstractCollection.prototype["toArrayList()Ljava/util/ArrayList;"] = function() { 
	var _G = 0, lA1 = null, lA3 = null, fA0 = null, tA0 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_util_ArrayList()));
				fA0 = tA0;
				(tA0)["java.util.ArrayList<init>(I)V"](this["size()I"]());
				lA1 = fA0;
				lA2 = this["iterator()Ljava/util/Iterator;"]();
				_G = 1;
				continue;
			case 1:
				if (!(lA2["hasNext()Z"]())) {
					_G = 2;
					continue;
				}
				lA3 = lA2["next()Ljava/lang/Object;"]();
				(lA1)["add(Ljava/lang/Object;)Z"](lA3);
				_G = 1;
				continue;
			case 2:
				return (lA1);
			default:
				break;
		}
	}
	return null;
};
java_util_AbstractCollection.prototype["containsAll(Ljava/util/Collection;)Z"] = function(p0) { 
	var _G = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA2 = p0["iterator()Ljava/util/Iterator;"]();
				_G = 1;
				continue;
			case 1:
				if (!(lA2["hasNext()Z"]())) {
					_G = 2;
					continue;
				}
				if (this["contains(Ljava/lang/Object;)Z"](lA2["next()Ljava/lang/Object;"]())) {
					_G = 1;
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
};
java_util_AbstractCollection.prototype["contains(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA2 = this["iterator()Ljava/util/Iterator;"]();
				if (((p0 == null))) {
					_G = 1;
					continue;
				}
				_G = 2;
				continue;
			case 2:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"](lA2["next()Ljava/lang/Object;"]()))) {
					_G = 2;
					continue;
				}
				return true;
			case 1:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				if (((lA2["next()Ljava/lang/Object;"]() != null))) {
					_G = 1;
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
};
// ABSTRACT
function java_util_AbstractSet() {
}
java_util_AbstractSet.prototype = Object.create(java_util_AbstractCollection.prototype);
java_util_AbstractSet.prototype.constructor = java_util_AbstractSet;
java_util_AbstractSet.SI = function(){};
java_util_AbstractSet.prototype.__JT__CLASS_ID = java_util_AbstractSet.__JT__CLASS_ID = 816;
java_util_AbstractSet.prototype.__JT__CLASS_IDS = java_util_AbstractSet.__JT__CLASS_IDS = [816,728,656,747,723,724];
java_util_AbstractSet.prototype["java.util.AbstractSet<init>()V"] = function() { 
	(this)["java.util.AbstractCollection<init>()V"]();
	return this;
	return this;
};
java_util_AbstractSet.prototype["hashCode()I"] = function() { 
	var _G = 0, lA3 = null, fI0 = 0, fI1 = 0, lI1 = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				lA2 = this["iterator()Ljava/util/Iterator;"]();
				_G = 1;
				continue;
			case 1:
				if (!(lA2["hasNext()Z"]())) {
					_G = 2;
					continue;
				}
				lA3 = lA2["next()Ljava/lang/Object;"]();
				fI0 = lI1;
				if (((lA3 != null))) {
					_G = 3;
					continue;
				}
				fI1 = 0;
				_G = 4;
				continue;
			case 3:
				fI1 = lA3["hashCode()I"]();
				_G = 4;
				continue;
			case 4:
				lI1 = (((fI0 + fI1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
java_util_AbstractSet.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null, fI0 = 0, lA3 = null, fA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						if ((((this) != p0))) {
							_G = 1;
							continue;
						}
						return true;
					case 1:
						if (!(N.isClassId(p0, 747))) {
							_G = 2;
							continue;
						}
						lA2 = (N.checkCast(p0, java_util_Set));
						_G = 3;
						continue;
					case 3:
						if (((this["size()I"]() != (lA2)["size()I"]()))) {
							_G = 4;
							continue;
						}
						if (!(this["containsAll(Ljava/util/Collection;)Z"]((lA2)))) {
							_G = 4;
							continue;
						}
						fI0 = 1;
						_G = 5;
						continue;
					case 4:
						fI0 = 0;
						_G = 5;
						continue;
					case 5:return ((fI0)!=0); 
					case 6:
						fA0 = (J__exception__);
						lA3 = fA0;
						return false;
					case 7:
						fA0 = (J__exception__);
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
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 3)) && ((_G < 5)))) && (J__exception__ instanceof java_lang_NullPointerException)))) {
				_G = 6;
				continue;
			}
			if (((((((_G >= 3)) && ((_G < 5)))) && (J__exception__ instanceof java_lang_ClassCastException)))) {
				_G = 7;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return false;
};
function java_util_Hashtable$EntrySet() {
}
java_util_Hashtable$EntrySet.prototype = Object.create(java_util_AbstractSet.prototype);
java_util_Hashtable$EntrySet.prototype.constructor = java_util_Hashtable$EntrySet;
java_util_Hashtable$EntrySet.prototype._this_0 = null;
java_util_Hashtable$EntrySet.SI = function(){};
java_util_Hashtable$EntrySet.prototype.__JT__CLASS_ID = java_util_Hashtable$EntrySet.__JT__CLASS_ID = 917;
java_util_Hashtable$EntrySet.prototype.__JT__CLASS_IDS = java_util_Hashtable$EntrySet.__JT__CLASS_IDS = [917,816,728,656,747,723,724];
java_util_Hashtable$EntrySet.prototype["java.util.Hashtable$EntrySet<init>(Ljava/util/Hashtable;Ljava/util/Hashtable$1;)V"] = function(p0, p1) { 
	this["java.util.Hashtable$EntrySet<init>(Ljava/util/Hashtable;)V"](p0);
	return this;
	return this;
};
java_util_Hashtable$EntrySet.prototype["java.util.Hashtable$EntrySet<init>(Ljava/util/Hashtable;)V"] = function(p0) { 
	this._this_0 = p0;
	(this)["java.util.AbstractSet<init>()V"]();
	return this;
	return this;
};
java_util_Hashtable$EntrySet.prototype["hashCode()I"] = function() { 
	return this._this_0["hashCode()I"]();
};
java_util_Hashtable$EntrySet.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null, fI0 = 0, lA3 = null, fA0 = null, tA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						tA0 = (this._this_0);
						fA0 = tA0;
						lA2 = tA0;
						// MONITOR_ENTER
						_G = 1;
						continue;
					case 1:
						fI0 = ((N.z2i(java_util_AbstractSet.prototype["equals(Ljava/lang/Object;)Z"].call((this), p0)))|0);
						// MONITOR_EXIT
						_G = 2;
						continue;
					case 2:return ((fI0)!=0); 
					case 3:
						fA0 = (J__exception__);
						lA3 = fA0;
						// MONITOR_EXIT
						_G = 4;
						continue;
					case 4:throw new WrappedError(lA3); break;
					default:
						break;
				}
			}
			return false;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 3;
				continue;
			}
			if (((((((_G >= 3)) && ((_G < 4)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 3;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return false;
};
java_util_Hashtable$EntrySet.prototype["toString()Ljava/lang/String;"] = function() { 
	var _G = 0, lA1 = null, lA2 = null, fA0 = null, tA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						tA0 = (this._this_0);
						fA0 = tA0;
						lA1 = tA0;
						// MONITOR_ENTER
						_G = 1;
						continue;
					case 1:
						fA0 = (java_util_AbstractCollection.prototype["toString()Ljava/lang/String;"].call((this)));
						// MONITOR_EXIT
						_G = 2;
						continue;
					case 2:return (fA0); 
					case 3:
						fA0 = (J__exception__);
						lA2 = fA0;
						// MONITOR_EXIT
						_G = 4;
						continue;
					case 4:throw new WrappedError(lA2); break;
					default:
						break;
				}
			}
			return null;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 3;
				continue;
			}
			if (((((((_G >= 3)) && ((_G < 4)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 3;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return null;
};
java_util_Hashtable$EntrySet.prototype["size()I"] = function() { 
	return this._this_0["size()I"]();
};
java_util_Hashtable$EntrySet.prototype["iterator()Ljava/util/Iterator;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_util_Hashtable$EntryIterator()));
	fA0 = tA0;
	(tA0)["java.util.Hashtable$EntryIterator<init>(Ljava/util/Hashtable;Ljava/util/Hashtable$1;)V"](this._this_0, null);
	return (fA0);
};
java_util_Hashtable$EntrySet.prototype["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"] = function(p0) { 
	var _G = 0, lA2 = null, lA3 = null, fA0 = null, tA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						tA0 = (this._this_0);
						fA0 = tA0;
						lA2 = tA0;
						// MONITOR_ENTER
						_G = 1;
						continue;
					case 1:
						fA0 = (java_util_AbstractCollection.prototype["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"].call((this), p0));
						// MONITOR_EXIT
						_G = 2;
						continue;
					case 2:return (fA0); 
					case 3:
						fA0 = (J__exception__);
						lA3 = fA0;
						// MONITOR_EXIT
						_G = 4;
						continue;
					case 4:throw new WrappedError(lA3); break;
					default:
						break;
				}
			}
			return null;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 3;
				continue;
			}
			if (((((((_G >= 3)) && ((_G < 4)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 3;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return null;
};
java_util_Hashtable$EntrySet.prototype["containsAll(Ljava/util/Collection;)Z"] = function(p0) { 
	var _G = 0, lA2 = null, fI0 = 0, lA3 = null, fA0 = null, tA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						tA0 = (this._this_0);
						fA0 = tA0;
						lA2 = tA0;
						// MONITOR_ENTER
						_G = 1;
						continue;
					case 1:
						fI0 = ((N.z2i(java_util_AbstractCollection.prototype["containsAll(Ljava/util/Collection;)Z"].call((this), p0)))|0);
						// MONITOR_EXIT
						_G = 2;
						continue;
					case 2:return ((fI0)!=0); 
					case 3:
						fA0 = (J__exception__);
						lA3 = fA0;
						// MONITOR_EXIT
						_G = 4;
						continue;
					case 4:throw new WrappedError(lA3); break;
					default:
						break;
				}
			}
			return false;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 3;
				continue;
			}
			if (((((((_G >= 3)) && ((_G < 4)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 3;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return false;
};
java_util_Hashtable$EntrySet.prototype["contains(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (N.isClassId(p0, 748)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				lA2 = N.checkCast(p0, java_util_Map$Entry);
				return java_util_Hashtable["access$1100(Ljava/util/Hashtable;Ljava/lang/Object;Ljava/lang/Object;)Z"](this._this_0, lA2["getKey()Ljava/lang/Object;"](), lA2["getValue()Ljava/lang/Object;"]());
			default:
				break;
		}
	}
	return false;
};
function java_util_Hashtable$1() {
}
java_util_Hashtable$1.prototype = Object.create(java_lang_Object.prototype);
java_util_Hashtable$1.prototype.constructor = java_util_Hashtable$1;
java_util_Hashtable$1.prototype.___id = 0;
java_util_Hashtable$1.SI = function(){};
java_util_Hashtable$1.prototype.__JT__CLASS_ID = java_util_Hashtable$1.__JT__CLASS_ID = 916;
java_util_Hashtable$1.prototype.__JT__CLASS_IDS = java_util_Hashtable$1.__JT__CLASS_IDS = [916,656];
function java_util_Map$Entry() {
}
java_util_Map$Entry.prototype = Object.create(java_lang_Object_base.prototype);
java_util_Map$Entry.prototype.constructor = java_util_Map$Entry;
java_util_Map$Entry.SI = function(){};
java_util_Map$Entry.prototype.__JT__CLASS_ID = java_util_Map$Entry.__JT__CLASS_ID = 748;
java_util_Map$Entry.prototype.__JT__CLASS_IDS = java_util_Map$Entry.__JT__CLASS_IDS = [748,656];
java_util_Map$Entry.prototype["getKey()Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.Map$Entry.getKey') };
java_util_Map$Entry.prototype["getValue()Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.Map$Entry.getValue') };
java_util_Map$Entry.prototype["hashCode()I"] = function() { N.methodWithoutBody('java.util.Map$Entry.hashCode') };
java_util_Map$Entry.prototype["equals(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.Map$Entry.equals') };
function java_util_Hashtable$HashtableEntry() {
}
java_util_Hashtable$HashtableEntry.prototype = Object.create(java_lang_Object.prototype);
java_util_Hashtable$HashtableEntry.prototype.constructor = java_util_Hashtable$HashtableEntry;
java_util_Hashtable$HashtableEntry.prototype._value = null;
java_util_Hashtable$HashtableEntry.prototype._next = null;
java_util_Hashtable$HashtableEntry.prototype._key = null;
java_util_Hashtable$HashtableEntry.prototype._hash = 0;
java_util_Hashtable$HashtableEntry.prototype.___id = 0;
java_util_Hashtable$HashtableEntry.SI = function(){};
java_util_Hashtable$HashtableEntry.prototype.__JT__CLASS_ID = java_util_Hashtable$HashtableEntry.__JT__CLASS_ID = 915;
java_util_Hashtable$HashtableEntry.prototype.__JT__CLASS_IDS = java_util_Hashtable$HashtableEntry.__JT__CLASS_IDS = [915,656,748];
java_util_Hashtable$HashtableEntry.prototype["java.util.Hashtable$HashtableEntry<init>(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/Hashtable$HashtableEntry;)V"] = function(p0, p1, p2, p3) { 
	(this)["java.lang.Object<init>()V"]();
	this._key = p0;
	this._value = p1;
	this._hash = p2;
	this._next = p3;
	return this;
	return this;
};
java_util_Hashtable$HashtableEntry.prototype["toString()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_StringBuilder()));
	fA0 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	return (fA0)["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"](this._key)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[7])["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"](this._value)["toString()Ljava/lang/String;"]();
};
java_util_Hashtable$HashtableEntry.prototype["hashCode()I"] = function() { 
	return (((this._key["hashCode()I"]() ^ this._value["hashCode()I"]()))|0);
};
java_util_Hashtable$HashtableEntry.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (N.isClassId(p0, 748)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				lA2 = N.checkCast(p0, java_util_Map$Entry);
				if (!(this._key["equals(Ljava/lang/Object;)Z"](lA2["getKey()Ljava/lang/Object;"]()))) {
					_G = 2;
					continue;
				}
				if (!(this._value["equals(Ljava/lang/Object;)Z"](lA2["getValue()Ljava/lang/Object;"]()))) {
					_G = 2;
					continue;
				}
				fI0 = 1;
				_G = 3;
				continue;
			case 2:
				fI0 = 0;
				_G = 3;
				continue;
			case 3:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_util_Hashtable$HashtableEntry.prototype["getKey()Ljava/lang/Object;"] = function() { 
	return this._key;
};
java_util_Hashtable$HashtableEntry.prototype["getValue()Ljava/lang/Object;"] = function() { 
	return this._value;
};
// ABSTRACT
function java_util_Dictionary() {
}
java_util_Dictionary.prototype = Object.create(java_lang_Object.prototype);
java_util_Dictionary.prototype.constructor = java_util_Dictionary;
java_util_Dictionary.prototype.___id = 0;
java_util_Dictionary.SI = function(){};
java_util_Dictionary.prototype.__JT__CLASS_ID = java_util_Dictionary.__JT__CLASS_ID = 914;
java_util_Dictionary.prototype.__JT__CLASS_IDS = java_util_Dictionary.__JT__CLASS_IDS = [914,656];
java_util_Dictionary.prototype["java.util.Dictionary<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_util_Dictionary.prototype["size()I"] = function() { N.methodWithoutBody('java.util.Dictionary.size') };
java_util_Dictionary.prototype["isEmpty()Z"] = function() { N.methodWithoutBody('java.util.Dictionary.isEmpty') };
java_util_Dictionary.prototype["get(Ljava/lang/Object;)Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.Dictionary.get') };
java_util_Dictionary.prototype["put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.Dictionary.put') };
function java_util_Map() {
}
java_util_Map.prototype = Object.create(java_lang_Object_base.prototype);
java_util_Map.prototype.constructor = java_util_Map;
java_util_Map.SI = function(){};
java_util_Map.prototype.__JT__CLASS_ID = java_util_Map.__JT__CLASS_ID = 746;
java_util_Map.prototype.__JT__CLASS_IDS = java_util_Map.__JT__CLASS_IDS = [746,656];
java_util_Map.prototype["entrySet()Ljava/util/Set;"] = function() { N.methodWithoutBody('java.util.Map.entrySet') };
java_util_Map.prototype["size()I"] = function() { N.methodWithoutBody('java.util.Map.size') };
java_util_Map.prototype["isEmpty()Z"] = function() { N.methodWithoutBody('java.util.Map.isEmpty') };
java_util_Map.prototype["get(Ljava/lang/Object;)Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.Map.get') };
java_util_Map.prototype["containsKey(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.Map.containsKey') };
java_util_Map.prototype["equals(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.Map.equals') };
java_util_Map.prototype["hashCode()I"] = function() { N.methodWithoutBody('java.util.Map.hashCode') };
java_util_Map.prototype["put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.Map.put') };
function java_lang_Cloneable() {
}
java_lang_Cloneable.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_Cloneable.prototype.constructor = java_lang_Cloneable;
java_lang_Cloneable.SI = function(){};
java_lang_Cloneable.prototype.__JT__CLASS_ID = java_lang_Cloneable.__JT__CLASS_ID = 726;
java_lang_Cloneable.prototype.__JT__CLASS_IDS = java_lang_Cloneable.__JT__CLASS_IDS = [726,656];
function java_io_Serializable() {
}
java_io_Serializable.prototype = Object.create(java_lang_Object_base.prototype);
java_io_Serializable.prototype.constructor = java_io_Serializable;
java_io_Serializable.SI = function(){};
java_io_Serializable.prototype.__JT__CLASS_ID = java_io_Serializable.__JT__CLASS_ID = 658;
java_io_Serializable.prototype.__JT__CLASS_IDS = java_io_Serializable.__JT__CLASS_IDS = [658,656];
function java_util_Hashtable() {
}
java_util_Hashtable.prototype = Object.create(java_util_Dictionary.prototype);
java_util_Hashtable.prototype.constructor = java_util_Hashtable;
java_util_Hashtable.prototype._threshold = 0;
java_util_Hashtable.prototype._table = null;
java_util_Hashtable.prototype._size = 0;
java_util_Hashtable.prototype._modCount = 0;
java_util_Hashtable.prototype._entrySet = null;
java_util_Hashtable.prototype._keySet = null;
java_util_Hashtable.prototype._values = null;
java_util_Hashtable.SI = function() { 
	java_util_Hashtable._EMPTY_TABLE = null;
	java_util_Hashtable._serialPersistentFields = null;
	java_util_Hashtable["java.util.Hashtable<clinit>()V"]();
};
java_util_Hashtable.prototype.__JT__CLASS_ID = java_util_Hashtable.__JT__CLASS_ID = 913;
java_util_Hashtable.prototype.__JT__CLASS_IDS = java_util_Hashtable.__JT__CLASS_IDS = [913,914,656,746,726,658];
java_util_Hashtable.prototype["java.util.Hashtable<init>()V"] = function() { 
	(this)["java.util.Dictionary<init>()V"]();
	this._table = N.checkCast(N.checkCast(java_util_Hashtable._EMPTY_TABLE, JA_L), JA_L);
	this._threshold = -1;
	return this;
	return this;
};
java_util_Hashtable["java.util.Hashtable<clinit>()V"] = function() { 
	var fI2 = 0, fA0 = null, fA1 = null, fA3 = null, tA0 = null, tA1 = null, tA3 = null;
	java_util_Hashtable._EMPTY_TABLE = (new JA_L(2, "[Ljava.util.Hashtable$HashtableEntry;"));
	tA0 = (new JA_L(2, "[Ljava.io.ObjectStreamField;"));
	fA0 = tA0;
	fA1 = tA0;
	fI2 = 0;
	tA1 = ((new java_io_ObjectStreamField()));
	fA3 = tA1;
	(tA1)["java.io.ObjectStreamField<init>(Ljava/lang/String;Ljava/lang/Class;)V"](S[8], java_lang_Integer._TYPE);
	(fA1).data[fI2] = fA3;
	fA1 = fA0;
	fI2 = 1;
	tA3 = ((new java_io_ObjectStreamField()));
	fA3 = tA3;
	(tA3)["java.io.ObjectStreamField<init>(Ljava/lang/String;Ljava/lang/Class;)V"](S[9], java_lang_Float._TYPE);
	(fA1).data[fI2] = fA3;
	java_util_Hashtable._serialPersistentFields = (fA0);
	return;
};
java_util_Hashtable.prototype["size()I"] = function() { 
	return this._size;
};
java_util_Hashtable.prototype["isEmpty()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._size != 0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_util_Hashtable.prototype["get(Ljava/lang/Object;)Ljava/lang/Object;"] = function(p0) { 
	var _G = 0, lA3 = null, lA4 = null, lA5 = null, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI2 = java_util_Collections["secondaryHash(Ljava/lang/Object;)I"](p0);
				lA3 = (this._table);
				lA4 = ((lA3).data[(((lI2 & (((lA3.length - 1))|0)))|0)]);
				_G = 1;
				continue;
			case 1:
				if (((lA4 == null))) {
					_G = 2;
					continue;
				}
				lA5 = (lA4)._key;
				if (((lA5 == p0))) {
					_G = 3;
					continue;
				}
				if ((((lA4)._hash != lI2))) {
					_G = 4;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"](lA5))) {
					_G = 4;
					continue;
				}
				_G = 3;
				continue;
			case 3:
				return (lA4)._value;
			case 4:
				lA4 = ((lA4)._next);
				_G = 1;
				continue;
			case 2:
				return null;
			default:
				break;
		}
	}
	return null;
};
java_util_Hashtable.prototype["put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"] = function(p0, p1) { 
	var _G = 0, lA4 = null, lA6 = null, lA7 = null, lA8 = null, lI3 = 0, lI5 = 0, fA0 = null, fA2 = null, tA0 = null, tA1 = null, tA5 = null, tA6 = null, fI0 = 0, fI1 = 0, tI4 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_NullPointerException()));
				fA0 = tA0;
				(tA0)["java.lang.NullPointerException<init>(Ljava/lang/String;)V"](S[10]);
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				if (((p1 != null))) {
					_G = 2;
					continue;
				}
				tA1 = ((new java_lang_NullPointerException()));
				fA0 = tA1;
				(tA1)["java.lang.NullPointerException<init>(Ljava/lang/String;)V"](S[11]);
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				lI3 = java_util_Collections["secondaryHash(Ljava/lang/Object;)I"](p0);
				lA4 = (this._table);
				lI5 = (((lI3 & (((lA4.length - 1))|0)))|0);
				lA6 = ((lA4).data[lI5]);
				lA7 = lA6;
				_G = 3;
				continue;
			case 3:
				if (((lA7 == null))) {
					_G = 4;
					continue;
				}
				if ((((lA7)._hash != lI3))) {
					_G = 5;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"]((lA7)._key))) {
					_G = 5;
					continue;
				}
				lA8 = (lA7)._value;
				(lA7)._value = p1;
				return lA8;
			case 5:
				lA7 = ((lA7)._next);
				_G = 3;
				continue;
			case 4:
				this._modCount = (((this._modCount + 1))|0);
				fA0 = (this);
				tA5 = fA0;
				tI4 = this._size;
				fI0 = tI4;
				(tA5)._size = (((tI4 + 1))|0);
				if (((fI0 <= this._threshold))) {
					_G = 6;
					continue;
				}
				this["rehash()V"]();
				lA4 = (this["doubleCapacity()[Ljava/util/Hashtable$HashtableEntry;"]());
				lI5 = (((lI3 & (((lA4.length - 1))|0)))|0);
				lA6 = ((lA4).data[lI5]);
				_G = 6;
				continue;
			case 6:
				fA0 = lA4;
				fI1 = lI5;
				tA6 = ((new java_util_Hashtable$HashtableEntry()));
				fA2 = tA6;
				(tA6)["java.util.Hashtable$HashtableEntry<init>(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/Hashtable$HashtableEntry;)V"](p0, p1, lI3, (lA6));
				(fA0).data[fI1] = fA2;
				return null;
			default:
				break;
		}
	}
	return null;
};
java_util_Hashtable.prototype["rehash()V"] = function() { 
	return;
};
java_util_Hashtable.prototype["doubleCapacity()[Ljava/util/Hashtable$HashtableEntry;"] = function() { 
	var _G = 0, lA1 = null, lA4 = null, lA6 = null, lA8 = null, lA9 = null, lI2 = 0, lI3 = 0, lI5 = 0, lI7 = 0, lI10 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = (this._table);
				lI2 = lA1.length;
				if (((lI2 != 1073741824))) {
					_G = 1;
					continue;
				}
				return (lA1);
			case 1:
				lI3 = ((Math.imul(lI2, 2))|0);
				lA4 = (this["makeTable(I)[Ljava/util/Hashtable$HashtableEntry;"](lI3));
				if (((this._size != 0))) {
					_G = 2;
					continue;
				}
				return (lA4);
			case 2:
				lI5 = 0;
				_G = 3;
				continue;
			case 3:
				if (((lI5 >= lI2))) {
					_G = 4;
					continue;
				}
				lA6 = ((lA1).data[lI5]);
				if (((lA6 != null))) {
					_G = 5;
					continue;
				}
				_G = 6;
				continue;
			case 5:
				lI7 = ((((lA6)._hash & lI2))|0);
				lA8 = null;
				(lA4).data[(((lI5 | lI7))|0)] = lA6;
				lA9 = ((lA6)._next);
				_G = 7;
				continue;
			case 7:
				if (((lA9 == null))) {
					_G = 8;
					continue;
				}
				lI10 = ((((lA9)._hash & lI2))|0);
				if (((lI10 == lI7))) {
					_G = 9;
					continue;
				}
				if (((lA8 != null))) {
					_G = 10;
					continue;
				}
				(lA4).data[(((lI5 | lI10))|0)] = lA9;
				_G = 11;
				continue;
			case 10:
				(lA8)._next = (lA9);
				_G = 11;
				continue;
			case 11:
				lA8 = lA6;
				lI7 = lI10;
				_G = 9;
				continue;
			case 9:
				lA6 = lA9;
				lA9 = ((lA9)._next);
				_G = 7;
				continue;
			case 8:
				if (((lA8 == null))) {
					_G = 6;
					continue;
				}
				(lA8)._next = null;
				_G = 6;
				continue;
			case 6:
				lI5 = (((lI5 + 1))|0);
				_G = 3;
				continue;
			case 4:
				return (lA4);
			default:
				break;
		}
	}
	return null;
};
java_util_Hashtable.prototype["makeTable(I)[Ljava/util/Hashtable$HashtableEntry;"] = function(p0) { 
	var lA2 = null;
	lA2 = N.checkCast(new JA_L(p0, "[Ljava.util.Hashtable$HashtableEntry;"), JA_L);
	this._table = lA2;
	this._threshold = ((((((p0 >> 1))|0) + (((p0 >> 2))|0)))|0);
	return lA2;
};
java_util_Hashtable.prototype["toString()Ljava/lang/String;"] = function() { 
	var _G = 0, lA1 = null, lA5 = null, lA6 = null, fI0 = 0, lI3 = 0, lA4 = null, fA0 = null, tI5 = 0, fA1 = null, tA0 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>(I)V"](((Math.imul(15, this._size))|0));
				lA1 = fA0;
				(lA1)["append(C)Ljava/lang/StringBuilder;"](123);
				lA2 = this["entrySet()Ljava/util/Set;"]()["iterator()Ljava/util/Iterator;"]();
				lI3 = ((N.z2i(lA2["hasNext()Z"]()))|0);
				_G = 1;
				continue;
			case 1:
				if (((lI3 == 0))) {
					_G = 2;
					continue;
				}
				lA4 = N.checkCast(lA2["next()Ljava/lang/Object;"](), java_util_Map$Entry);
				lA5 = lA4["getKey()Ljava/lang/Object;"]();
				fA0 = lA1;
				if (((lA5 != (this)))) {
					_G = 3;
					continue;
				}
				fA1 = S[12];
				_G = 4;
				continue;
			case 3:
				fA1 = lA5["toString()Ljava/lang/String;"]();
				_G = 4;
				continue;
			case 4:
				(fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](fA1);
				(lA1)["append(C)Ljava/lang/StringBuilder;"](61);
				lA6 = lA4["getValue()Ljava/lang/Object;"]();
				fA0 = lA1;
				if (((lA6 != (this)))) {
					_G = 5;
					continue;
				}
				fA1 = S[12];
				_G = 6;
				continue;
			case 5:
				fA1 = lA6["toString()Ljava/lang/String;"]();
				_G = 6;
				continue;
			case 6:
				(fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](fA1);
				tI5 = ((N.z2i(lA2["hasNext()Z"]()))|0);
				fI0 = tI5;
				lI3 = tI5;
				if (((fI0 == 0))) {
					_G = 7;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[6]);
				_G = 7;
				continue;
			case 7:
				_G = 1;
				continue;
			case 2:
				(lA1)["append(C)Ljava/lang/StringBuilder;"](125);
				return (lA1)["toString()Ljava/lang/String;"]();
			default:
				break;
		}
	}
	return null;
};
java_util_Hashtable.prototype["entrySet()Ljava/util/Set;"] = function() { 
	var _G = 0, lA1 = null, fA0 = null, fA1 = null, tA0 = null, tA2 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = (this._entrySet);
				if (((lA1 == null))) {
					_G = 1;
					continue;
				}
				fA0 = lA1;
				_G = 2;
				continue;
			case 1:
				fA0 = (this);
				tA0 = ((new java_util_Hashtable$EntrySet()));
				fA1 = tA0;
				(tA0)["java.util.Hashtable$EntrySet<init>(Ljava/util/Hashtable;Ljava/util/Hashtable$1;)V"](this, null);
				tA2 = fA0;
				tA1 = fA1;
				fA0 = tA1;
				(tA2)._entrySet = (tA1);
				_G = 2;
				continue;
			case 2:return (fA0); 
			default:
				break;
		}
	}
	return null;
};
java_util_Hashtable.prototype["hashCode()I"] = function() { 
	var _G = 0, lA4 = null, lA5 = null, fI0 = 0, fI1 = 0, fI2 = 0, lI1 = 0, lA3 = null, fA2 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				lA2 = this["entrySet()Ljava/util/Set;"]()["iterator()Ljava/util/Iterator;"]();
				_G = 1;
				continue;
			case 1:
				if (!(lA2["hasNext()Z"]())) {
					_G = 2;
					continue;
				}
				lA3 = N.checkCast(lA2["next()Ljava/lang/Object;"](), java_util_Map$Entry);
				lA4 = lA3["getKey()Ljava/lang/Object;"]();
				lA5 = lA3["getValue()Ljava/lang/Object;"]();
				if (((lA4 == (this)))) {
					_G = 1;
					continue;
				}
				if (((lA5 != (this)))) {
					_G = 3;
					continue;
				}
				_G = 1;
				continue;
			case 3:
				fI0 = lI1;
				if (((lA4 == null))) {
					_G = 4;
					continue;
				}
				fI1 = lA4["hashCode()I"]();
				_G = 5;
				continue;
			case 4:
				fI1 = 0;
				_G = 5;
				continue;
			case 5:
				if (((lA5 == null))) {
					_G = 6;
					continue;
				}
				fA2 = lA5;
				fI2 = fA2["hashCode()I"]();
				_G = 7;
				continue;
			case 6:
				fI2 = 0;
				_G = 7;
				continue;
			case 7:
				lI1 = (((fI0 + (((fI1 ^ fI2))|0)))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
java_util_Hashtable["access$500(Ljava/util/Hashtable;)I"] = function(p0) { 
	return p0._modCount;
};
java_util_Hashtable["access$600(Ljava/util/Hashtable;)[Ljava/util/Hashtable$HashtableEntry;"] = function(p0) { 
	return p0._table;
};
java_util_Hashtable["access$1100(Ljava/util/Hashtable;Ljava/lang/Object;Ljava/lang/Object;)Z"] = function(p0, p1, p2) { 
	return p0["containsMapping(Ljava/lang/Object;Ljava/lang/Object;)Z"](p1, p2);
};
java_util_Hashtable.prototype["containsMapping(Ljava/lang/Object;Ljava/lang/Object;)Z"] = function(p0, p1) { 
	var _G = 0, lA4 = null, lA6 = null, lI3 = 0, lI5 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI3 = java_util_Collections["secondaryHash(Ljava/lang/Object;)I"](p0);
				lA4 = (this._table);
				lI5 = (((lI3 & (((lA4.length - 1))|0)))|0);
				lA6 = ((lA4).data[lI5]);
				_G = 1;
				continue;
			case 1:
				if (((lA6 == null))) {
					_G = 2;
					continue;
				}
				if ((((lA6)._hash != lI3))) {
					_G = 3;
					continue;
				}
				if (!((lA6)._key["equals(Ljava/lang/Object;)Z"](p0))) {
					_G = 3;
					continue;
				}
				return (lA6)._value["equals(Ljava/lang/Object;)Z"](p1);
			case 3:
				lA6 = ((lA6)._next);
				_G = 1;
				continue;
			case 2:
				return false;
			default:
				break;
		}
	}
	return false;
};
java_util_Hashtable.prototype["clone()Ljava/lang/Object;"] = function() { 
	var _G = 0, lA1 = null, lA2 = null, fA0 = null, tA1 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						_G = 1;
						continue;
					case 1:
						lA1 = (N.checkCast(java_lang_Object.prototype["clone()Ljava/lang/Object;"].call((this)), java_util_Hashtable));
						_G = 2;
						continue;
					case 2:
						_G = 3;
						continue;
					case 4:
						fA0 = (J__exception__);
						lA2 = fA0;
						tA1 = ((new java_lang_AssertionError()));
						fA0 = tA1;
						(tA1)["java.lang.AssertionError<init>(Ljava/lang/Object;)V"](lA2);
						throw new WrappedError(fA0);
						_G = 3;
						continue;
					case 3:
						(lA1)["makeTable(I)[Ljava/util/Hashtable$HashtableEntry;"](this._table.length);
						(lA1)._size = 0;
						(lA1)._keySet = null;
						(lA1)._entrySet = null;
						(lA1)._values = null;
						(lA1)["constructorPutAll(Ljava/util/Map;)V"]((this));
						return lA1;
					default:
						break;
				}
			}
			return null;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_CloneNotSupportedException)))) {
				_G = 4;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return null;
};
java_util_Hashtable.prototype["constructorPutAll(Ljava/util/Map;)V"] = function(p0) { 
	var _G = 0, lA3 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._table != java_util_Hashtable._EMPTY_TABLE))) {
					_G = 1;
					continue;
				}
				this["doubleCapacity()[Ljava/util/Hashtable$HashtableEntry;"]();
				_G = 1;
				continue;
			case 1:
				lA2 = p0["entrySet()Ljava/util/Set;"]()["iterator()Ljava/util/Iterator;"]();
				_G = 2;
				continue;
			case 2:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				lA3 = N.checkCast(lA2["next()Ljava/lang/Object;"](), java_util_Map$Entry);
				this["constructorPut(Ljava/lang/Object;Ljava/lang/Object;)V"](lA3["getKey()Ljava/lang/Object;"](), lA3["getValue()Ljava/lang/Object;"]());
				_G = 2;
				continue;
			case 3:
				return;
			default:
				break;
		}
	}
	return;
};
java_util_Hashtable.prototype["constructorPut(Ljava/lang/Object;Ljava/lang/Object;)V"] = function(p0, p1) { 
	var _G = 0, lA4 = null, lA6 = null, lA7 = null, fI1 = 0, lI3 = 0, lI5 = 0, fA0 = null, fA2 = null, tA0 = null, tA1 = null, tA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_NullPointerException()));
				fA0 = tA0;
				(tA0)["java.lang.NullPointerException<init>(Ljava/lang/String;)V"](S[10]);
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				if (((p1 != null))) {
					_G = 2;
					continue;
				}
				tA1 = ((new java_lang_NullPointerException()));
				fA0 = tA1;
				(tA1)["java.lang.NullPointerException<init>(Ljava/lang/String;)V"](S[11]);
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				lI3 = java_util_Collections["secondaryHash(Ljava/lang/Object;)I"](p0);
				lA4 = (this._table);
				lI5 = (((lI3 & (((lA4.length - 1))|0)))|0);
				lA6 = ((lA4).data[lI5]);
				lA7 = lA6;
				_G = 3;
				continue;
			case 3:
				if (((lA7 == null))) {
					_G = 4;
					continue;
				}
				if ((((lA7)._hash != lI3))) {
					_G = 5;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"]((lA7)._key))) {
					_G = 5;
					continue;
				}
				(lA7)._value = p1;
				return;
				_G = 5;
				continue;
			case 5:
				lA7 = ((lA7)._next);
				_G = 3;
				continue;
			case 4:
				fA0 = lA4;
				fI1 = lI5;
				tA2 = ((new java_util_Hashtable$HashtableEntry()));
				fA2 = tA2;
				(tA2)["java.util.Hashtable$HashtableEntry<init>(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/Hashtable$HashtableEntry;)V"](p0, p1, lI3, (lA6));
				(fA0).data[fI1] = fA2;
				this._size = (((this._size + 1))|0);
				return;
			default:
				break;
		}
	}
	return;
};
java_util_Hashtable.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!(N.isClassId(p0, 746))) {
					_G = 1;
					continue;
				}
				if (!(this["entrySet()Ljava/util/Set;"]()["equals(Ljava/lang/Object;)Z"]((N.checkCast(p0, java_util_Map)["entrySet()Ljava/util/Set;"]())))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:
				return ((fI0)!=0);
			default:
				break;
		}
	}
	return false;
};
java_util_Hashtable.prototype["containsKey(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA3 = null, lA4 = null, lA5 = null, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI2 = java_util_Collections["secondaryHash(Ljava/lang/Object;)I"](p0);
				lA3 = (this._table);
				lA4 = ((lA3).data[(((lI2 & (((lA3.length - 1))|0)))|0)]);
				_G = 1;
				continue;
			case 1:
				if (((lA4 == null))) {
					_G = 2;
					continue;
				}
				lA5 = (lA4)._key;
				if (((lA5 == p0))) {
					_G = 3;
					continue;
				}
				if ((((lA4)._hash != lI2))) {
					_G = 4;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"](lA5))) {
					_G = 4;
					continue;
				}
				_G = 3;
				continue;
			case 3:
				return true;
			case 4:
				lA4 = ((lA4)._next);
				_G = 1;
				continue;
			case 2:
				return false;
			default:
				break;
		}
	}
	return false;
};
function java_util_Properties() {
}
java_util_Properties.prototype = Object.create(java_util_Hashtable.prototype);
java_util_Properties.prototype.constructor = java_util_Properties;
java_util_Properties.prototype._defaults = null;
java_util_Properties.prototype._threshold = 0;
java_util_Properties.prototype._table = null;
java_util_Properties.prototype._size = 0;
java_util_Properties.prototype._modCount = 0;
java_util_Properties.prototype._entrySet = null;
java_util_Properties.prototype._keySet = null;
java_util_Properties.prototype._values = null;
java_util_Properties.SI = function(){};
java_util_Properties.prototype.__JT__CLASS_ID = java_util_Properties.__JT__CLASS_ID = 912;
java_util_Properties.prototype.__JT__CLASS_IDS = java_util_Properties.__JT__CLASS_IDS = [912,913,914,656,746,726,658];
java_util_Properties.prototype["java.util.Properties<init>()V"] = function() { 
	(this)["java.util.Hashtable<init>()V"]();
	return this;
	return this;
};
java_util_Properties.prototype["getProperty(Ljava/lang/String;)Ljava/lang/String;"] = function(p0) { 
	var _G = 0, lA2 = null, lA3 = null, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA2 = java_util_Hashtable.prototype["get(Ljava/lang/Object;)Ljava/lang/Object;"].call((this), (p0));
				if (!((lA2 instanceof java_lang_String))) {
					_G = 1;
					continue;
				}
				fA0 = (N.checkCast(lA2, java_lang_String));
				_G = 2;
				continue;
			case 1:
				fA0 = null;
				_G = 2;
				continue;
			case 2:
				lA3 = fA0;
				if (((lA3 != null))) {
					_G = 3;
					continue;
				}
				if (((this._defaults == null))) {
					_G = 3;
					continue;
				}
				lA3 = (this._defaults["getProperty(Ljava/lang/String;)Ljava/lang/String;"](p0));
				_G = 3;
				continue;
			case 3:
				return (lA3);
			default:
				break;
		}
	}
	return null;
};
function com_jtransc_simd_MutableMatrixFloat32x4x4Utils() {
}
com_jtransc_simd_MutableMatrixFloat32x4x4Utils.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_simd_MutableMatrixFloat32x4x4Utils.prototype.constructor = com_jtransc_simd_MutableMatrixFloat32x4x4Utils;
com_jtransc_simd_MutableMatrixFloat32x4x4Utils.prototype.___id = 0;
com_jtransc_simd_MutableMatrixFloat32x4x4Utils.SI = function() { 
	com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp2 = null;
	com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp1 = null;
	com_jtransc_simd_MutableMatrixFloat32x4x4Utils["com.jtransc.simd.MutableMatrixFloat32x4x4Utils<clinit>()V"]();
};
com_jtransc_simd_MutableMatrixFloat32x4x4Utils.prototype.__JT__CLASS_ID = com_jtransc_simd_MutableMatrixFloat32x4x4Utils.__JT__CLASS_ID = 911;
com_jtransc_simd_MutableMatrixFloat32x4x4Utils.prototype.__JT__CLASS_IDS = com_jtransc_simd_MutableMatrixFloat32x4x4Utils.__JT__CLASS_IDS = [911,656];
com_jtransc_simd_MutableMatrixFloat32x4x4Utils.prototype["com.jtransc.simd.MutableMatrixFloat32x4x4Utils<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_simd_MutableMatrixFloat32x4x4Utils["com.jtransc.simd.MutableMatrixFloat32x4x4Utils<clinit>()V"] = function() { 
	com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp1 = com_jtransc_simd_MutableFloat32x4["create()Lcom/jtransc/simd/MutableFloat32x4;"]();
	com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp2 = com_jtransc_simd_MutableFloat32x4["create()Lcom/jtransc/simd/MutableFloat32x4;"]();
	return;
};
com_jtransc_simd_MutableMatrixFloat32x4x4Utils["_setToMul44(Lcom/jtransc/simd/MutableMatrixFloat32x4x4;Lcom/jtransc/simd/MutableMatrixFloat32x4x4;Lcom/jtransc/simd/MutableMatrixFloat32x4x4;)Lcom/jtransc/simd/MutableMatrixFloat32x4x4;"] = function(p0, p1, p2) { 
	var lA10 = null, lA11 = null, lA12 = null, lA3 = null, lA4 = null, lA5 = null, lA6 = null, lA7 = null, lA8 = null, lA9 = null;
	lA3 = p1["getX()Lcom/jtransc/simd/MutableFloat32x4;"]();
	lA4 = p2["getX()Lcom/jtransc/simd/MutableFloat32x4;"]();
	lA5 = p1["getY()Lcom/jtransc/simd/MutableFloat32x4;"]();
	lA6 = p2["getY()Lcom/jtransc/simd/MutableFloat32x4;"]();
	lA7 = p1["getZ()Lcom/jtransc/simd/MutableFloat32x4;"]();
	lA8 = p2["getZ()Lcom/jtransc/simd/MutableFloat32x4;"]();
	lA9 = p1["getW()Lcom/jtransc/simd/MutableFloat32x4;"]();
	lA10 = p2["getW()Lcom/jtransc/simd/MutableFloat32x4;"]();
	lA11 = com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp1;
	lA12 = com_jtransc_simd_MutableMatrixFloat32x4x4Utils._vtemp2;
	lA12["setToZero()V"]();
	lA11["setToXXXX(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA4);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA3);
	lA11["setToYYYY(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA4);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA5);
	lA11["setToZZZZ(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA4);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA7);
	lA11["setToWWWW(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA4);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA9);
	p0["setX(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12);
	lA12["setToZero()V"]();
	lA11["setToXXXX(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA6);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA3);
	lA11["setToYYYY(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA6);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA5);
	lA11["setToZZZZ(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA6);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA7);
	lA11["setToWWWW(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA6);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA9);
	p0["setY(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12);
	lA12["setToZero()V"]();
	lA11["setToXXXX(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA8);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA3);
	lA11["setToYYYY(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA8);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA5);
	lA11["setToZZZZ(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA8);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA7);
	lA11["setToWWWW(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA8);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA9);
	p0["setZ(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12);
	lA12["setToZero()V"]();
	lA11["setToXXXX(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA10);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA3);
	lA11["setToYYYY(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA10);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA5);
	lA11["setToZZZZ(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA10);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA7);
	lA11["setToWWWW(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA10);
	lA12["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12, lA11, lA9);
	p0["setW(Lcom/jtransc/simd/MutableFloat32x4;)V"](lA12);
	return p0;
};
com_jtransc_simd_MutableMatrixFloat32x4x4Utils["_getSumAll(Lcom/jtransc/simd/MutableMatrixFloat32x4x4;)F"] = function(p0) { 
	return Math.fround((Math.fround((Math.fround((p0["getX()Lcom/jtransc/simd/MutableFloat32x4;"]()["getSumAll()F"]() + p0["getY()Lcom/jtransc/simd/MutableFloat32x4;"]()["getSumAll()F"]())) + p0["getZ()Lcom/jtransc/simd/MutableFloat32x4;"]()["getSumAll()F"]())) + p0["getW()Lcom/jtransc/simd/MutableFloat32x4;"]()["getSumAll()F"]()));
};
function com_jtransc_simd_MutableMatrixFloat32x4x4() {
}
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype.constructor = com_jtransc_simd_MutableMatrixFloat32x4x4;
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype._w = null;
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype._x = null;
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype._y = null;
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype._z = null;
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype.___id = 0;
com_jtransc_simd_MutableMatrixFloat32x4x4.SI = function() { 
	com_jtransc_simd_MutableMatrixFloat32x4x4["com.jtransc.simd.MutableMatrixFloat32x4x4<clinit>()V"]();
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype.__JT__CLASS_ID = com_jtransc_simd_MutableMatrixFloat32x4x4.__JT__CLASS_ID = 910;
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype.__JT__CLASS_IDS = com_jtransc_simd_MutableMatrixFloat32x4x4.__JT__CLASS_IDS = [910,656];
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["com.jtransc.simd.MutableMatrixFloat32x4x4<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	this._x = com_jtransc_simd_MutableFloat32x4["create()Lcom/jtransc/simd/MutableFloat32x4;"]();
	this._y = com_jtransc_simd_MutableFloat32x4["create()Lcom/jtransc/simd/MutableFloat32x4;"]();
	this._z = com_jtransc_simd_MutableFloat32x4["create()Lcom/jtransc/simd/MutableFloat32x4;"]();
	this._w = com_jtransc_simd_MutableFloat32x4["create()Lcom/jtransc/simd/MutableFloat32x4;"]();
	return this;
	return this;
};
com_jtransc_simd_MutableMatrixFloat32x4x4["com.jtransc.simd.MutableMatrixFloat32x4x4<clinit>()V"] = function() { 
	com_jtransc_simd_Simd["ref()V"]();
	return;
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["setTo(FFFFFFFFFFFFFFFF)V"] = function(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15) { 
	this["getX()Lcom/jtransc/simd/MutableFloat32x4;"]()["setTo(FFFF)V"](p0, p1, p2, p3);
	this["getY()Lcom/jtransc/simd/MutableFloat32x4;"]()["setTo(FFFF)V"](p4, p5, p6, p7);
	this["getZ()Lcom/jtransc/simd/MutableFloat32x4;"]()["setTo(FFFF)V"](p8, p9, p10, p11);
	this["getW()Lcom/jtransc/simd/MutableFloat32x4;"]()["setTo(FFFF)V"](p12, p13, p14, p15);
	return;
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["getZ()Lcom/jtransc/simd/MutableFloat32x4;"] = function() { 
	return this._z;
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["getY()Lcom/jtransc/simd/MutableFloat32x4;"] = function() { 
	return this._y;
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["getX()Lcom/jtransc/simd/MutableFloat32x4;"] = function() { 
	return this._x;
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["getW()Lcom/jtransc/simd/MutableFloat32x4;"] = function() { 
	return this._w;
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["setToMul44(Lcom/jtransc/simd/MutableMatrixFloat32x4x4;Lcom/jtransc/simd/MutableMatrixFloat32x4x4;)V"] = function(p0, p1) { 
	com_jtransc_simd_MutableMatrixFloat32x4x4Utils["_setToMul44(Lcom/jtransc/simd/MutableMatrixFloat32x4x4;Lcom/jtransc/simd/MutableMatrixFloat32x4x4;Lcom/jtransc/simd/MutableMatrixFloat32x4x4;)Lcom/jtransc/simd/MutableMatrixFloat32x4x4;"](this, p0, p1);
	return;
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["setW(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
	this["getW()Lcom/jtransc/simd/MutableFloat32x4;"]()["setTo(Lcom/jtransc/simd/MutableFloat32x4;)V"](p0);
	return;
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["setX(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
	this["getX()Lcom/jtransc/simd/MutableFloat32x4;"]()["setTo(Lcom/jtransc/simd/MutableFloat32x4;)V"](p0);
	return;
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["setY(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
	this["getY()Lcom/jtransc/simd/MutableFloat32x4;"]()["setTo(Lcom/jtransc/simd/MutableFloat32x4;)V"](p0);
	return;
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["setZ(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
	this["getZ()Lcom/jtransc/simd/MutableFloat32x4;"]()["setTo(Lcom/jtransc/simd/MutableFloat32x4;)V"](p0);
	return;
};
com_jtransc_simd_MutableMatrixFloat32x4x4.prototype["getSumAll()F"] = function() { 
	return com_jtransc_simd_MutableMatrixFloat32x4x4Utils["_getSumAll(Lcom/jtransc/simd/MutableMatrixFloat32x4x4;)F"](this);
};
com_jtransc_simd_MutableMatrixFloat32x4x4["create()Lcom/jtransc/simd/MutableMatrixFloat32x4x4;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new com_jtransc_simd_MutableMatrixFloat32x4x4()));
	fA0 = tA0;
	(tA0)["com.jtransc.simd.MutableMatrixFloat32x4x4<init>()V"]();
	return (fA0);
};
function com_jtransc_simd_MutableFloat32x4Utils() {
}
com_jtransc_simd_MutableFloat32x4Utils.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_simd_MutableFloat32x4Utils.prototype.constructor = com_jtransc_simd_MutableFloat32x4Utils;
com_jtransc_simd_MutableFloat32x4Utils.prototype.___id = 0;
com_jtransc_simd_MutableFloat32x4Utils.SI = function() { 
	com_jtransc_simd_MutableFloat32x4Utils._temp = null;
	com_jtransc_simd_MutableFloat32x4Utils["com.jtransc.simd.MutableFloat32x4Utils<clinit>()V"]();
};
com_jtransc_simd_MutableFloat32x4Utils.prototype.__JT__CLASS_ID = com_jtransc_simd_MutableFloat32x4Utils.__JT__CLASS_ID = 909;
com_jtransc_simd_MutableFloat32x4Utils.prototype.__JT__CLASS_IDS = com_jtransc_simd_MutableFloat32x4Utils.__JT__CLASS_IDS = [909,656];
com_jtransc_simd_MutableFloat32x4Utils.prototype["com.jtransc.simd.MutableFloat32x4Utils<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_simd_MutableFloat32x4Utils["com.jtransc.simd.MutableFloat32x4Utils<clinit>()V"] = function() { 
	com_jtransc_simd_MutableFloat32x4Utils._temp = com_jtransc_simd_MutableFloat32x4["create()Lcom/jtransc/simd/MutableFloat32x4;"]();
	return;
};
com_jtransc_simd_MutableFloat32x4Utils["toStringInternal(Lcom/jtransc/simd/MutableFloat32x4;)Ljava/lang/String;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_StringBuilder()));
	fA0 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[13])["append(F)Ljava/lang/StringBuilder;"](p0["getX()F"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[6])["append(F)Ljava/lang/StringBuilder;"](p0["getY()F"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[6])["append(F)Ljava/lang/StringBuilder;"](p0["getZ()F"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[6])["append(F)Ljava/lang/StringBuilder;"](p0["getW()F"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[1])["toString()Ljava/lang/String;"]();
};
function com_jtransc_simd_MutableFloat32x4() {
}
com_jtransc_simd_MutableFloat32x4.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_simd_MutableFloat32x4.prototype.constructor = com_jtransc_simd_MutableFloat32x4;
com_jtransc_simd_MutableFloat32x4.prototype._z = 0.0;
com_jtransc_simd_MutableFloat32x4.prototype._x = 0.0;
com_jtransc_simd_MutableFloat32x4.prototype._y = 0.0;
com_jtransc_simd_MutableFloat32x4.prototype._w = 0.0;
com_jtransc_simd_MutableFloat32x4.prototype.___id = 0;
com_jtransc_simd_MutableFloat32x4.SI = function() { 
	com_jtransc_simd_MutableFloat32x4["com.jtransc.simd.MutableFloat32x4<clinit>()V"]();
};
com_jtransc_simd_MutableFloat32x4.prototype.__JT__CLASS_ID = com_jtransc_simd_MutableFloat32x4.__JT__CLASS_ID = 908;
com_jtransc_simd_MutableFloat32x4.prototype.__JT__CLASS_IDS = com_jtransc_simd_MutableFloat32x4.__JT__CLASS_IDS = [908,656];
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["com.jtransc.simd.MutableFloat32x4<init>(FFFF)V"] = function(p0, p1, p2, p3) { 
		this.simd = SIMD.Float32x4(+p0, +p1, +p2, +p3);
		return this;
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["com.jtransc.simd.MutableFloat32x4<init>(FFFF)V"] = function(p0, p1, p2, p3) { 
		(this)["java.lang.Object<init>()V"]();
		this["setTo(FFFF)V"](p0, p1, p2, p3);
		return this;
		return this;
	};
}
com_jtransc_simd_MutableFloat32x4["com.jtransc.simd.MutableFloat32x4<clinit>()V"] = function() { 
	com_jtransc_simd_Simd["ref()V"]();
	return;
};
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["setTo(FFFF)V"] = function(p0, p1, p2, p3) { 
		this.simd = SIMD.Float32x4(+p0, +p1, +p2, +p3);
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["setTo(FFFF)V"] = function(p0, p1, p2, p3) { 
		this._x = p0;
		this._y = p1;
		this._z = p2;
		this._w = p3;
		return;
	};
}
com_jtransc_simd_MutableFloat32x4.prototype["toString()Ljava/lang/String;"] = function() { 
	return com_jtransc_simd_MutableFloat32x4Utils["toStringInternal(Lcom/jtransc/simd/MutableFloat32x4;)Ljava/lang/String;"](this);
};
com_jtransc_simd_MutableFloat32x4["create()Lcom/jtransc/simd/MutableFloat32x4;"] = function() { 
	return com_jtransc_simd_MutableFloat32x4["create(FFFF)Lcom/jtransc/simd/MutableFloat32x4;"](0.0, 0.0, 0.0, 0.0);
};
com_jtransc_simd_MutableFloat32x4["create(FFFF)Lcom/jtransc/simd/MutableFloat32x4;"] = function(p0, p1, p2, p3) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new com_jtransc_simd_MutableFloat32x4()));
	fA0 = tA0;
	(tA0)["com.jtransc.simd.MutableFloat32x4<init>(FFFF)V"](p0, p1, p2, p3);
	return (fA0);
};
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["getZ()F"] = function() { 
		return SIMD.Float32x4.extractLane(this.simd, 2);
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["getZ()F"] = function() { 
		return this._z;
	};
}
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["getY()F"] = function() { 
		return SIMD.Float32x4.extractLane(this.simd, 1);
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["getY()F"] = function() { 
		return this._y;
	};
}
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["getX()F"] = function() { 
		return SIMD.Float32x4.extractLane(this.simd, 0);
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["getX()F"] = function() { 
		return this._x;
	};
}
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["getW()F"] = function() { 
		return SIMD.Float32x4.extractLane(this.simd, 3);
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["getW()F"] = function() { 
		return this._w;
	};
}
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["setToAdd(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0, p1) { 
		this.simd = SIMD.Float32x4.add(p0.simd, p1.simd);
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["setToAdd(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0, p1) { 
		this["setTo(FFFF)V"](Math.fround((p0._x + p1._x)), Math.fround((p0._y + p1._y)), Math.fround((p0._z + p1._z)), Math.fround((p0._w + p1._w)));
		return;
	};
}
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["setTo(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
		this.simd = p0.simd;
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["setTo(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
		this._x = p0._x;
		this._y = p0._y;
		this._z = p0._z;
		this._w = p0._w;
		return;
	};
}
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["setToZZZZ(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
		this.simd = SIMD.Float32x4.swizzle(p0.simd, 2, 2, 2, 2);
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["setToZZZZ(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
		p0["setTo(FFFF)V"](p0._z, p0._z, p0._z, p0._z);
		return;
	};
}
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0, p1, p2) { 
		this.simd = SIMD.Float32x4.add(p0.simd, SIMD.Float32x4.mul(p1.simd, p2.simd));
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["setToAddMul(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0, p1, p2) { 
		this["setTo(FFFF)V"](Math.fround((p0._x + Math.fround((p1._x * p2._x)))), Math.fround((p0._y + Math.fround((p1._y * p2._y)))), Math.fround((p0._z + Math.fround((p1._z * p2._z)))), Math.fround((p0._w + Math.fround((p1._w * p2._w)))));
		return;
	};
}
com_jtransc_simd_MutableFloat32x4.prototype["setToZero()V"] = function() { 
	this["setTo(FFFF)V"](0.0, 0.0, 0.0, 0.0);
	return;
};
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["setToWWWW(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
		this.simd = SIMD.Float32x4.swizzle(p0.simd, 3, 3, 3, 3);
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["setToWWWW(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
		p0["setTo(FFFF)V"](p0._w, p0._w, p0._w, p0._w);
		return;
	};
}
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["setToXXXX(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
		this.simd = SIMD.Float32x4.swizzle(p0.simd, 0, 0, 0, 0);
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["setToXXXX(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
		p0["setTo(FFFF)V"](p0._x, p0._x, p0._x, p0._x);
		return;
	};
}
if (hasSIMD) {
	com_jtransc_simd_MutableFloat32x4.prototype["setToYYYY(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
		this.simd = SIMD.Float32x4.swizzle(p0.simd, 1, 1, 1, 1);
	};
}
else {
	com_jtransc_simd_MutableFloat32x4.prototype["setToYYYY(Lcom/jtransc/simd/MutableFloat32x4;)V"] = function(p0) { 
		p0["setTo(FFFF)V"](p0._y, p0._y, p0._y, p0._y);
		return;
	};
}
com_jtransc_simd_MutableFloat32x4.prototype["getSumAll()F"] = function() { 
	return Math.fround((Math.fround((Math.fround((this["getX()F"]() + this["getY()F"]())) + this["getZ()F"]())) + this["getW()F"]()));
};
function com_jtransc_simd_Simd() {
}
com_jtransc_simd_Simd.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_simd_Simd.prototype.constructor = com_jtransc_simd_Simd;
com_jtransc_simd_Simd.prototype.___id = 0;
com_jtransc_simd_Simd.SI = function(){};
com_jtransc_simd_Simd.prototype.__JT__CLASS_ID = com_jtransc_simd_Simd.__JT__CLASS_ID = 907;
com_jtransc_simd_Simd.prototype.__JT__CLASS_IDS = com_jtransc_simd_Simd.__JT__CLASS_IDS = [907,656];
com_jtransc_simd_Simd.prototype["com.jtransc.simd.Simd<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_simd_Simd["ref()V"] = function() { 
	return;
};
function com_jtransc_simd_Float32x4() {
}
com_jtransc_simd_Float32x4.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_simd_Float32x4.prototype.constructor = com_jtransc_simd_Float32x4;
com_jtransc_simd_Float32x4.prototype._z = 0.0;
com_jtransc_simd_Float32x4.prototype._y = 0.0;
com_jtransc_simd_Float32x4.prototype._x = 0.0;
com_jtransc_simd_Float32x4.prototype._w = 0.0;
com_jtransc_simd_Float32x4.prototype.___id = 0;
com_jtransc_simd_Float32x4.SI = function() { 
	com_jtransc_simd_Float32x4["com.jtransc.simd.Float32x4<clinit>()V"]();
};
com_jtransc_simd_Float32x4.prototype.__JT__CLASS_ID = com_jtransc_simd_Float32x4.__JT__CLASS_ID = 906;
com_jtransc_simd_Float32x4.prototype.__JT__CLASS_IDS = com_jtransc_simd_Float32x4.__JT__CLASS_IDS = [906,656];
com_jtransc_simd_Float32x4.prototype["com.jtransc.simd.Float32x4<init>(FFFF)V"] = function(p0, p1, p2, p3) { 
	(this)["java.lang.Object<init>()V"]();
	this._x = p0;
	this._y = p1;
	this._z = p2;
	this._w = p3;
	return this;
	return this;
};
com_jtransc_simd_Float32x4["com.jtransc.simd.Float32x4<clinit>()V"] = function() { 
	com_jtransc_simd_Simd["ref()V"]();
	return;
};
if (hasSIMD) {
	com_jtransc_simd_Float32x4["getW(Lcom/jtransc/simd/Float32x4;)F"] = function(p0) { 
		return SIMD.Float32x4.extractLane(p0, 3);
	};
}
else {
	com_jtransc_simd_Float32x4["getW(Lcom/jtransc/simd/Float32x4;)F"] = function(p0) { 
		return p0._w;
	};
}
if (hasSIMD) {
	com_jtransc_simd_Float32x4["getX(Lcom/jtransc/simd/Float32x4;)F"] = function(p0) { 
		return SIMD.Float32x4.extractLane(p0, 0);
	};
}
else {
	com_jtransc_simd_Float32x4["getX(Lcom/jtransc/simd/Float32x4;)F"] = function(p0) { 
		return p0._x;
	};
}
if (hasSIMD) {
	com_jtransc_simd_Float32x4["getY(Lcom/jtransc/simd/Float32x4;)F"] = function(p0) { 
		return SIMD.Float32x4.extractLane(p0, 1);
	};
}
else {
	com_jtransc_simd_Float32x4["getY(Lcom/jtransc/simd/Float32x4;)F"] = function(p0) { 
		return p0._y;
	};
}
if (hasSIMD) {
	com_jtransc_simd_Float32x4["create(FFFF)Lcom/jtransc/simd/Float32x4;"] = function(p0, p1, p2, p3) { 
		return SIMD.Float32x4(+p0, +p1, +p2, +p3);
	};
}
else {
	com_jtransc_simd_Float32x4["create(FFFF)Lcom/jtransc/simd/Float32x4;"] = function(p0, p1, p2, p3) { 
		var fA0 = null, tA0 = null;
		tA0 = ((new com_jtransc_simd_Float32x4()));
		fA0 = tA0;
		(tA0)["com.jtransc.simd.Float32x4<init>(FFFF)V"](p0, p1, p2, p3);
		return (fA0);
	};
}
if (hasSIMD) {
	com_jtransc_simd_Float32x4["getZ(Lcom/jtransc/simd/Float32x4;)F"] = function(p0) { 
		return SIMD.Float32x4.extractLane(p0, 2);
	};
}
else {
	com_jtransc_simd_Float32x4["getZ(Lcom/jtransc/simd/Float32x4;)F"] = function(p0) { 
		return p0._z;
	};
}
if (hasSIMD) {
	com_jtransc_simd_Float32x4["add(Lcom/jtransc/simd/Float32x4;Lcom/jtransc/simd/Float32x4;)Lcom/jtransc/simd/Float32x4;"] = function(p0, p1) { 
		return SIMD.Float32x4.add(p0, p1);
	};
}
else {
	com_jtransc_simd_Float32x4["add(Lcom/jtransc/simd/Float32x4;Lcom/jtransc/simd/Float32x4;)Lcom/jtransc/simd/Float32x4;"] = function(p0, p1) { 
		var fA0 = null, tA0 = null;
		tA0 = ((new com_jtransc_simd_Float32x4()));
		fA0 = tA0;
		(tA0)["com.jtransc.simd.Float32x4<init>(FFFF)V"](Math.fround((p0._x + p1._x)), Math.fround((p0._y + p1._y)), Math.fround((p0._z + p1._z)), Math.fround((p0._w + p1._w)));
		return (fA0);
	};
}
function Benchmark$Test2() {
}
Benchmark$Test2.prototype = Object.create(java_lang_Object.prototype);
Benchmark$Test2.prototype.constructor = Benchmark$Test2;
Benchmark$Test2.prototype.___id = 0;
Benchmark$Test2.SI = function(){};
Benchmark$Test2.prototype.__JT__CLASS_ID = Benchmark$Test2.__JT__CLASS_ID = 905;
Benchmark$Test2.prototype.__JT__CLASS_IDS = Benchmark$Test2.__JT__CLASS_IDS = [905,656];
Benchmark$Test2.prototype["Benchmark$Test2<init>(LBenchmark$1;)V"] = function(p0) { 
	this["Benchmark$Test2<init>()V"]();
	return this;
	return this;
};
Benchmark$Test2.prototype["Benchmark$Test2<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
function Benchmark$Test1() {
}
Benchmark$Test1.prototype = Object.create(java_lang_Object.prototype);
Benchmark$Test1.prototype.constructor = Benchmark$Test1;
Benchmark$Test1.prototype.___id = 0;
Benchmark$Test1.SI = function(){};
Benchmark$Test1.prototype.__JT__CLASS_ID = Benchmark$Test1.__JT__CLASS_ID = 904;
Benchmark$Test1.prototype.__JT__CLASS_IDS = Benchmark$Test1.__JT__CLASS_IDS = [904,656];
Benchmark$Test1.prototype["Benchmark$Test1<init>(LBenchmark$1;)V"] = function(p0) { 
	this["Benchmark$Test1<init>()V"]();
	return this;
	return this;
};
Benchmark$Test1.prototype["Benchmark$Test1<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
function java_util_zip_Deflater() {
}
java_util_zip_Deflater.prototype = Object.create(java_lang_Object.prototype);
java_util_zip_Deflater.prototype.constructor = java_util_zip_Deflater;
java_util_zip_Deflater.prototype._impl = null;
java_util_zip_Deflater.prototype._inLength = 0;
java_util_zip_Deflater.prototype._inRead = 0;
java_util_zip_Deflater.prototype._noHeader = false;
java_util_zip_Deflater.prototype._compressLevel = 0;
java_util_zip_Deflater.prototype._streamHandle = N.lnew(0, 0);
java_util_zip_Deflater.prototype._flushParm = 0;
java_util_zip_Deflater.prototype._strategy = 0;
java_util_zip_Deflater.prototype.___id = 0;
java_util_zip_Deflater.SI = function(){};
java_util_zip_Deflater.prototype.__JT__CLASS_ID = java_util_zip_Deflater.__JT__CLASS_ID = 903;
java_util_zip_Deflater.prototype.__JT__CLASS_IDS = java_util_zip_Deflater.__JT__CLASS_IDS = [903,656];
java_util_zip_Deflater.prototype["java.util.zip.Deflater<init>(IZ)V"] = function(p0, p1) { 
	var fA1 = null, tA0 = null;
	(this)["java.lang.Object<init>()V"]();
	this._flushParm = 0;
	this._compressLevel = -1;
	this._strategy = 0;
	this._streamHandle = N.lnew(-1, -1);
	this._compressLevel = p0;
	this._noHeader = p1;
	tA0 = ((new com_jtransc_compression_jzlib_Deflater()));
	fA1 = tA0;
	(tA0)["com.jtransc.compression.jzlib.Deflater<init>(IZ)V"](p0, p1);
	this._impl = (fA1);
	return this;
	return this;
};
java_util_zip_Deflater.prototype["setInput([BII)V"] = function(p0, p1, p2) { 
	this._inLength = p2;
	this._inRead = 0;
	this._impl["setInput([BIIZ)V"](p0, p1, p2, false);
	return;
};
java_util_zip_Deflater.prototype["deflate([BIII)I"] = function(p0, p1, p2, p3) { 
	return this["deflateImpl([BIII)I"](p0, p1, p2, p3);
};
java_util_zip_Deflater.prototype["deflateImpl([BIII)I"] = function(p0, p1, p2, p3) { 
	var lJ5 = N.lnew(0, 0), lJ8 = N.lnew(0, 0);
	this._impl["setOutput([BII)V"](p0, p1, p2);
	lJ5 = this._impl["getTotalOut()J"]();
	this._impl["deflate(I)I"](p3);
	lJ8 = this._impl["getTotalOut()J"]();
	return N.j2i(N.lsub(lJ8, lJ5));
};
function com_jtransc_compression_jzlib_Checksum() {
}
com_jtransc_compression_jzlib_Checksum.prototype = Object.create(java_lang_Object_base.prototype);
com_jtransc_compression_jzlib_Checksum.prototype.constructor = com_jtransc_compression_jzlib_Checksum;
com_jtransc_compression_jzlib_Checksum.SI = function(){};
com_jtransc_compression_jzlib_Checksum.prototype.__JT__CLASS_ID = com_jtransc_compression_jzlib_Checksum.__JT__CLASS_ID = 861;
com_jtransc_compression_jzlib_Checksum.prototype.__JT__CLASS_IDS = com_jtransc_compression_jzlib_Checksum.__JT__CLASS_IDS = [861,656];
com_jtransc_compression_jzlib_Checksum.prototype["getValue()I"] = function() { N.methodWithoutBody('com.jtransc.compression.jzlib.Checksum.getValue') };
com_jtransc_compression_jzlib_Checksum.prototype["reset()V"] = function() { N.methodWithoutBody('com.jtransc.compression.jzlib.Checksum.reset') };
com_jtransc_compression_jzlib_Checksum.prototype["update([BII)V"] = function() { N.methodWithoutBody('com.jtransc.compression.jzlib.Checksum.update') };
function com_jtransc_compression_jzlib_Adler32() {
}
com_jtransc_compression_jzlib_Adler32.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_compression_jzlib_Adler32.prototype.constructor = com_jtransc_compression_jzlib_Adler32;
com_jtransc_compression_jzlib_Adler32.prototype._s1 = 0;
com_jtransc_compression_jzlib_Adler32.prototype._s2 = 0;
com_jtransc_compression_jzlib_Adler32.prototype.___id = 0;
com_jtransc_compression_jzlib_Adler32.SI = function(){};
com_jtransc_compression_jzlib_Adler32.prototype.__JT__CLASS_ID = com_jtransc_compression_jzlib_Adler32.__JT__CLASS_ID = 902;
com_jtransc_compression_jzlib_Adler32.prototype.__JT__CLASS_IDS = com_jtransc_compression_jzlib_Adler32.__JT__CLASS_IDS = [902,656,861];
com_jtransc_compression_jzlib_Adler32.prototype["com.jtransc.compression.jzlib.Adler32<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	this._s1 = 1;
	this._s2 = 0;
	return this;
	return this;
};
com_jtransc_compression_jzlib_Adler32.prototype["getValue()I"] = function() { 
	return ((((((this._s2 << 16))|0) | this._s1))|0);
};
com_jtransc_compression_jzlib_Adler32.prototype["reset()V"] = function() { 
	this._s1 = 1;
	this._s2 = 0;
	return;
};
com_jtransc_compression_jzlib_Adler32.prototype["update([BII)V"] = function(p0, p1, p2) { 
	var _G = 0, fA2 = null, lI2 = 0, lI3 = 0, lI4 = 0, lI5 = 0, lI6 = 0, tA10 = null, fI1 = 0, fI3 = 0, fI0 = 0, fA0 = null, tA5 = null, tA7 = null, tA9 = null, tA1 = null, tA3 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI2 = p1;
				lI3 = p2;
				if (((lI3 != 1))) {
					_G = 1;
					continue;
				}
				fA0 = this;
				fI1 = this._s1;
				fA2 = p0;
				fI3 = lI2;
				lI2 = (((lI2 + 1))|0);
				fA0._s1 = (((fI1 + ((((fA2.data[fI3]) & 255))|0)))|0);
				tA1 = this;
				tA1._s2 = (((tA1._s2 + this._s1))|0);
				this._s1 = (((this._s1 % 65521))|0);
				tA3 = this;
				tA3._s2 = (((tA3._s2 % 65521))|0);
				return;
				_G = 1;
				continue;
			case 1:
				lI4 = (((lI3 / 5552))|0);
				lI5 = (((lI3 % 5552))|0);
				_G = 2;
				continue;
			case 2:
				fI0 = lI4;
				lI4 = (((lI4 + -1))|0);
				if (((fI0 <= 0))) {
					_G = 3;
					continue;
				}
				lI6 = 5552;
				lI3 = (((lI3 - lI6))|0);
				_G = 4;
				continue;
			case 4:
				fI0 = lI6;
				lI6 = (((lI6 + -1))|0);
				if (((fI0 <= 0))) {
					_G = 5;
					continue;
				}
				fA0 = this;
				fI1 = this._s1;
				fA2 = p0;
				fI3 = lI2;
				lI2 = (((lI2 + 1))|0);
				fA0._s1 = (((fI1 + ((((fA2.data[fI3]) & 255))|0)))|0);
				tA5 = this;
				tA5._s2 = (((tA5._s2 + this._s1))|0);
				_G = 4;
				continue;
			case 5:
				this._s1 = (((this._s1 % 65521))|0);
				tA7 = this;
				tA7._s2 = (((tA7._s2 % 65521))|0);
				_G = 2;
				continue;
			case 3:
				lI6 = lI5;
				lI3 = (((lI3 - lI6))|0);
				_G = 6;
				continue;
			case 6:
				fI0 = lI6;
				lI6 = (((lI6 + -1))|0);
				if (((fI0 <= 0))) {
					_G = 7;
					continue;
				}
				fA0 = this;
				fI1 = this._s1;
				fA2 = p0;
				fI3 = lI2;
				lI2 = (((lI2 + 1))|0);
				fA0._s1 = (((fI1 + ((((fA2.data[fI3]) & 255))|0)))|0);
				tA9 = this;
				tA9._s2 = (((tA9._s2 + this._s1))|0);
				_G = 6;
				continue;
			case 7:
				tA10 = this;
				tA10._s1 = (((tA10._s1 % 65521))|0);
				this._s2 = (((this._s2 % 65521))|0);
				return;
			default:
				break;
		}
	}
	return;
};
function java_lang_Throwable() {
}
java_lang_Throwable.prototype = Object.create(java_lang_Object.prototype);
java_lang_Throwable.prototype.constructor = java_lang_Throwable;
java_lang_Throwable.prototype._thrown = false;
java_lang_Throwable.prototype._message = null;
java_lang_Throwable.prototype._writableStackTrace = false;
java_lang_Throwable.prototype._enableSuppression = false;
java_lang_Throwable.prototype._cause = null;
java_lang_Throwable.prototype._stackTrace = null;
java_lang_Throwable.prototype._supressed = null;
java_lang_Throwable.prototype.___id = 0;
java_lang_Throwable.SI = function() { 
	java_lang_Throwable._EMPTY_ARRAY = null;
	java_lang_Throwable["java.lang.Throwable<clinit>()V"]();
};
java_lang_Throwable.prototype.__JT__CLASS_ID = java_lang_Throwable.__JT__CLASS_ID = 681;
java_lang_Throwable.prototype.__JT__CLASS_IDS = java_lang_Throwable.__JT__CLASS_IDS = [681,656,658];
java_lang_Throwable["java.lang.Throwable<clinit>()V"] = function() { 
	java_lang_Throwable._EMPTY_ARRAY = new JA_L(0, "[Ljava.lang.Throwable;");
	return;
};
java_lang_Throwable.prototype["java.lang.Throwable<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	this._enableSuppression = false;
	this._writableStackTrace = false;
	this._thrown = false;
	this["init(Ljava/lang/String;Ljava/lang/Throwable;ZZ)V"](p0, null, false, false);
	return this;
	return this;
};
java_lang_Throwable.prototype["java.lang.Throwable<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"] = function(p0, p1) { 
	(this)["java.lang.Object<init>()V"]();
	this._enableSuppression = false;
	this._writableStackTrace = false;
	this._thrown = false;
	this["init(Ljava/lang/String;Ljava/lang/Throwable;ZZ)V"](p0, p1, false, false);
	return this;
	return this;
};
java_lang_Throwable.prototype["java.lang.Throwable<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	this._enableSuppression = false;
	this._writableStackTrace = false;
	this._thrown = false;
	this["init(Ljava/lang/String;Ljava/lang/Throwable;ZZ)V"](null, null, false, false);
	return this;
	return this;
};
java_lang_Throwable.prototype["java.lang.Throwable<init>(Ljava/lang/Throwable;)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	this._enableSuppression = false;
	this._writableStackTrace = false;
	this._thrown = false;
	this["init(Ljava/lang/String;Ljava/lang/Throwable;ZZ)V"](null, p0, false, false);
	return this;
	return this;
};
java_lang_Throwable.prototype["prepareThrow()Ljava/lang/Throwable;"] = function() { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (this._thrown) {
					_G = 1;
					continue;
				}
				this["init_exception()V"]();
				_G = 1;
				continue;
			case 1:
				this._thrown = true;
				return this;
			default:
				break;
		}
	}
	return null;
};
java_lang_Throwable.prototype["init_exception()V"] = function() { 
	this.error = new Error();
};
java_lang_Throwable.prototype["toString()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_StringBuilder()));
	fA0 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[14])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this._message)["toString()Ljava/lang/String;"]();
};
java_lang_Throwable.prototype["init(Ljava/lang/String;Ljava/lang/Throwable;ZZ)V"] = function(p0, p1, p2, p3) { 
	this._message = p0;
	this._cause = p1;
	this._enableSuppression = p2;
	this._writableStackTrace = p3;
	return;
};
java_lang_Throwable.prototype["printStackTrace()V"] = function() { 
	var _G = 0, lA1 = null, lA2 = null, lA5 = null, lI3 = 0, lI4 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				com_jtransc_io_JTranscConsole["error(Ljava/lang/Object;)V"]((this));
				lA1 = (this["getStackTraceLazy()[Ljava/lang/StackTraceElement;"]());
				lA2 = lA1;
				lI3 = lA2.length;
				lI4 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI4 >= lI3))) {
					_G = 2;
					continue;
				}
				lA5 = ((lA2).data[lI4]);
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				com_jtransc_io_JTranscConsole["error(Ljava/lang/Object;)V"](((fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[15])["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"](lA5)["toString()Ljava/lang/String;"]()));
				lI4 = (((lI4 + 1))|0);
				_G = 1;
				continue;
			case 2:
				lA2 = (this["getSuppressed()[Ljava/lang/Throwable;"]());
				lI3 = lA2.length;
				lI4 = 0;
				_G = 3;
				continue;
			case 3:
				if (((lI4 >= lI3))) {
					_G = 4;
					continue;
				}
				lA5 = ((lA2).data[lI4]);
				com_jtransc_io_JTranscConsole["error(Ljava/lang/Object;)V"]((S[16]));
				(lA5)["printStackTrace()V"]();
				lI4 = (((lI4 + 1))|0);
				_G = 3;
				continue;
			case 4:
				lA2 = (this["getCause()Ljava/lang/Throwable;"]());
				if (((lA2 == null))) {
					_G = 5;
					continue;
				}
				com_jtransc_io_JTranscConsole["error(Ljava/lang/Object;)V"]((S[17]));
				(lA2)["printStackTrace()V"]();
				_G = 5;
				continue;
			case 5:
				return;
			default:
				break;
		}
	}
	return;
};
java_lang_Throwable.prototype["getStackTraceLazy()[Ljava/lang/StackTraceElement;"] = function() { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._stackTrace != null))) {
					_G = 1;
					continue;
				}
				this["fillInStackTrace(I)V"](0);
				_G = 1;
				continue;
			case 1:
				return this._stackTrace;
			default:
				break;
		}
	}
	return null;
};
java_lang_Throwable.prototype["fillInStackTrace(I)V"] = function(p0) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!(this._thrown)) {
					_G = 1;
					continue;
				}
				this["genStackTraceFromError()V"]();
				_G = 2;
				continue;
			case 1:
				this["genStackTrace()V"]();
				_G = 2;
				continue;
			case 2:
				this["setStackTrace([Ljava/lang/StackTraceElement;)V"](this["getStackTraceInternal()[Ljava/lang/StackTraceElement;"]());
				return;
			default:
				break;
		}
	}
	return;
};
java_lang_Throwable.prototype["genStackTrace()V"] = function() { 
	this.error = new Error();
};
java_lang_Throwable.prototype["getStackTraceInternal()[Ljava/lang/StackTraceElement;"] = function() { 
	return N.getStackTrace(this.error, 0);
};
java_lang_Throwable.prototype["setStackTrace([Ljava/lang/StackTraceElement;)V"] = function(p0) { 
	this._stackTrace = N.checkCast((p0)["clone()Ljava/lang/Object;"](), JA_L);
	return;
};
java_lang_Throwable.prototype["genStackTraceFromError()V"] = function() { 
	return;
};
java_lang_Throwable.prototype["getSuppressed()[Ljava/lang/Throwable;"] = function() { 
	var _G = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._supressed == null))) {
					_G = 1;
					continue;
				}
				fA0 = N.checkCast(this._supressed["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"]((java_lang_Throwable._EMPTY_ARRAY)), JA_L);
				_G = 2;
				continue;
			case 1:
				fA0 = java_lang_Throwable._EMPTY_ARRAY;
				_G = 2;
				continue;
			case 2:return fA0; 
			default:
				break;
		}
	}
	return null;
};
java_lang_Throwable.prototype["getCause()Ljava/lang/Throwable;"] = function() { 
	return this._cause;
};
java_lang_Throwable.prototype["initCause(Ljava/lang/Throwable;)Ljava/lang/Throwable;"] = function(p0) { 
	this._cause = p0;
	return this._cause;
};
function java_lang_Exception() {
}
java_lang_Exception.prototype = Object.create(java_lang_Throwable.prototype);
java_lang_Exception.prototype.constructor = java_lang_Exception;
java_lang_Exception.prototype._thrown = false;
java_lang_Exception.prototype._message = null;
java_lang_Exception.prototype._writableStackTrace = false;
java_lang_Exception.prototype._enableSuppression = false;
java_lang_Exception.prototype._cause = null;
java_lang_Exception.prototype._stackTrace = null;
java_lang_Exception.prototype._supressed = null;
java_lang_Exception.SI = function(){};
java_lang_Exception.prototype.__JT__CLASS_ID = java_lang_Exception.__JT__CLASS_ID = 680;
java_lang_Exception.prototype.__JT__CLASS_IDS = java_lang_Exception.__JT__CLASS_IDS = [680,681,656,658];
java_lang_Exception.prototype["java.lang.Exception<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.Throwable<init>(Ljava/lang/String;)V"](p0);
	return this;
	return this;
};
java_lang_Exception.prototype["java.lang.Exception<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"] = function(p0, p1) { 
	(this)["java.lang.Throwable<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"](p0, p1);
	return this;
	return this;
};
java_lang_Exception.prototype["java.lang.Exception<init>()V"] = function() { 
	(this)["java.lang.Throwable<init>()V"]();
	return this;
	return this;
};
java_lang_Exception.prototype["java.lang.Exception<init>(Ljava/lang/Throwable;)V"] = function(p0) { 
	(this)["java.lang.Throwable<init>(Ljava/lang/Throwable;)V"](p0);
	return this;
	return this;
};
function java_lang_RuntimeException() {
}
java_lang_RuntimeException.prototype = Object.create(java_lang_Exception.prototype);
java_lang_RuntimeException.prototype.constructor = java_lang_RuntimeException;
java_lang_RuntimeException.SI = function(){};
java_lang_RuntimeException.prototype.__JT__CLASS_ID = java_lang_RuntimeException.__JT__CLASS_ID = 696;
java_lang_RuntimeException.prototype.__JT__CLASS_IDS = java_lang_RuntimeException.__JT__CLASS_IDS = [696,680,681,656,658];
java_lang_RuntimeException.prototype["java.lang.RuntimeException<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.Exception<init>(Ljava/lang/String;)V"](p0);
	return this;
	return this;
};
java_lang_RuntimeException.prototype["java.lang.RuntimeException<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"] = function(p0, p1) { 
	(this)["java.lang.Exception<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"](p0, p1);
	return this;
	return this;
};
java_lang_RuntimeException.prototype["java.lang.RuntimeException<init>()V"] = function() { 
	(this)["java.lang.Exception<init>()V"]();
	return this;
	return this;
};
java_lang_RuntimeException.prototype["java.lang.RuntimeException<init>(Ljava/lang/Throwable;)V"] = function(p0) { 
	(this)["java.lang.Exception<init>(Ljava/lang/Throwable;)V"](p0);
	return this;
	return this;
};
function com_jtransc_compression_jzlib_GZIPException() {
}
com_jtransc_compression_jzlib_GZIPException.prototype = Object.create(java_lang_RuntimeException.prototype);
com_jtransc_compression_jzlib_GZIPException.prototype.constructor = com_jtransc_compression_jzlib_GZIPException;
com_jtransc_compression_jzlib_GZIPException.SI = function(){};
com_jtransc_compression_jzlib_GZIPException.prototype.__JT__CLASS_ID = com_jtransc_compression_jzlib_GZIPException.__JT__CLASS_ID = 901;
com_jtransc_compression_jzlib_GZIPException.prototype.__JT__CLASS_IDS = com_jtransc_compression_jzlib_GZIPException.__JT__CLASS_IDS = [901,696,680,681,656,658];
com_jtransc_compression_jzlib_GZIPException.prototype["com.jtransc.compression.jzlib.GZIPException<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.RuntimeException<init>(Ljava/lang/String;)V"](p0);
	return this;
	return this;
};
function com_jtransc_compression_jzlib_StaticTree() {
}
com_jtransc_compression_jzlib_StaticTree.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_compression_jzlib_StaticTree.prototype.constructor = com_jtransc_compression_jzlib_StaticTree;
com_jtransc_compression_jzlib_StaticTree.prototype._elems = 0;
com_jtransc_compression_jzlib_StaticTree.prototype._max_length = 0;
com_jtransc_compression_jzlib_StaticTree.prototype._extra_base = 0;
com_jtransc_compression_jzlib_StaticTree.prototype._static_tree = null;
com_jtransc_compression_jzlib_StaticTree.prototype._extra_bits = null;
com_jtransc_compression_jzlib_StaticTree.prototype.___id = 0;
com_jtransc_compression_jzlib_StaticTree.SI = function() { 
	com_jtransc_compression_jzlib_StaticTree._static_ltree = null;
	com_jtransc_compression_jzlib_StaticTree._static_dtree = null;
	com_jtransc_compression_jzlib_StaticTree._static_bl_desc = null;
	com_jtransc_compression_jzlib_StaticTree._static_d_desc = null;
	com_jtransc_compression_jzlib_StaticTree._static_l_desc = null;
	com_jtransc_compression_jzlib_StaticTree["com.jtransc.compression.jzlib.StaticTree<clinit>()V"]();
};
com_jtransc_compression_jzlib_StaticTree.prototype.__JT__CLASS_ID = com_jtransc_compression_jzlib_StaticTree.__JT__CLASS_ID = 900;
com_jtransc_compression_jzlib_StaticTree.prototype.__JT__CLASS_IDS = com_jtransc_compression_jzlib_StaticTree.__JT__CLASS_IDS = [900,656];
com_jtransc_compression_jzlib_StaticTree.prototype["com.jtransc.compression.jzlib.StaticTree<init>([S[IIII)V"] = function(p0, p1, p2, p3, p4) { 
	(this)["java.lang.Object<init>()V"]();
	this._static_tree = p0;
	this._extra_bits = p1;
	this._extra_base = p2;
	this._elems = p3;
	this._max_length = p4;
	return this;
	return this;
};
com_jtransc_compression_jzlib_StaticTree["com.jtransc.compression.jzlib.StaticTree<clinit>()V"] = function() { 
	var tA636 = null, tA637 = null, tA638 = null, fA0 = null, tA576 = null, tA0 = null;
	tA0 = (new JA_S(576));
	fA0 = tA0;
	(tA0).data[0] = 12;
	(fA0).setArraySlice(1, [8, 140, 8, 76, 8, 204, 8, 44, 8, 172, 8, 108, 8, 236, 8, 28, 8, 156, 8, 92, 8, 220, 8, 60, 8, 188, 8, 124, 8, 252, 8, 2, 8, 130, 8, 66, 8, 194, 8, 34, 8, 162, 8, 98, 8, 226, 8, 18, 8, 146, 8, 82, 8, 210, 8, 50, 8, 178, 8, 114, 8, 242, 8, 10, 8, 138, 8, 74, 8, 202, 8, 42, 8, 170, 8, 106, 8, 234, 8, 26, 8, 154, 8, 90, 8, 218, 8, 58, 8, 186, 8, 122, 8, 250, 8, 6, 8, 134, 8, 70, 8, 198, 8, 38, 8, 166, 8, 102, 8, 230, 8, 22, 8, 150, 8, 86, 8, 214, 8, 54, 8, 182, 8, 118, 8, 246, 8, 14, 8, 142, 8, 78, 8, 206, 8, 46, 8, 174, 8, 110, 8, 238, 8, 30, 8, 158, 8, 94, 8, 222, 8, 62, 8, 190, 8, 126, 8, 254, 8, 1, 8, 129, 8, 65, 8, 193, 8, 33, 8, 161, 8, 97, 8, 225, 8, 17, 8, 145, 8, 81, 8, 209, 8, 49, 8, 177, 8, 113, 8, 241, 8, 9, 8, 137, 8, 73, 8, 201, 8, 41, 8, 169, 8, 105, 8, 233, 8, 25, 8, 153, 8, 89, 8, 217, 8, 57, 8, 185, 8, 121, 8, 249, 8, 5, 8, 133, 8, 69, 8, 197, 8, 37, 8, 165, 8, 101, 8, 229, 8, 21, 8, 149, 8, 85, 8, 213, 8, 53, 8, 181, 8, 117, 8, 245, 8, 13, 8, 141, 8, 77, 8, 205, 8, 45, 8, 173, 8, 109, 8, 237, 8, 29, 8, 157, 8, 93, 8, 221, 8, 61, 8, 189, 8, 125, 8, 253, 8, 19, 9, 275, 9, 147, 9, 403, 9, 83, 9, 339, 9, 211, 9, 467, 9, 51, 9, 307, 9, 179, 9, 435, 9, 115, 9, 371, 9, 243, 9, 499, 9, 11, 9, 267, 9, 139, 9, 395, 9, 75, 9, 331, 9, 203, 9, 459, 9, 43, 9, 299, 9, 171, 9, 427, 9, 107, 9, 363, 9, 235, 9, 491, 9, 27, 9, 283, 9, 155, 9, 411, 9, 91, 9, 347, 9, 219, 9, 475, 9, 59, 9, 315, 9, 187, 9, 443, 9, 123, 9, 379, 9, 251, 9, 507, 9, 7, 9, 263, 9, 135, 9, 391, 9, 71, 9, 327, 9, 199, 9, 455, 9, 39, 9, 295, 9, 167, 9, 423, 9, 103, 9, 359, 9, 231, 9, 487, 9, 23, 9, 279, 9, 151, 9, 407, 9, 87, 9, 343, 9, 215, 9, 471, 9, 55, 9, 311, 9, 183, 9, 439, 9, 119, 9, 375, 9, 247, 9, 503, 9, 15, 9, 271, 9, 143, 9, 399, 9, 79, 9, 335, 9, 207, 9, 463, 9, 47, 9, 303, 9, 175, 9, 431, 9, 111, 9, 367, 9, 239, 9, 495, 9, 31, 9, 287, 9, 159, 9, 415, 9, 95, 9, 351, 9, 223, 9, 479, 9, 63, 9, 319, 9, 191, 9, 447, 9, 127, 9, 383, 9, 255, 9, 511, 9, 0, 7, 64, 7, 32, 7, 96, 7, 16, 7, 80, 7, 48, 7, 112, 7, 8, 7, 72, 7, 40, 7, 104, 7, 24, 7, 88, 7, 56, 7, 120, 7, 4, 7, 68, 7, 36, 7, 100, 7, 20, 7, 84, 7, 52, 7, 116, 7, 3, 8, 131, 8, 67, 8, 195, 8, 35, 8, 163, 8, 99, 8, 227, 8]);
	com_jtransc_compression_jzlib_StaticTree._static_ltree = (fA0);
	tA576 = (new JA_S(60));
	fA0 = tA576;
	(tA576).data[0] = 0;
	(fA0).setArraySlice(1, [5, 16, 5, 8, 5, 24, 5, 4, 5, 20, 5, 12, 5, 28, 5, 2, 5, 18, 5, 10, 5, 26, 5, 6, 5, 22, 5, 14, 5, 30, 5, 1, 5, 17, 5, 9, 5, 25, 5, 5, 5, 21, 5, 13, 5, 29, 5, 3, 5, 19, 5, 11, 5, 27, 5, 7, 5, 23, 5]);
	com_jtransc_compression_jzlib_StaticTree._static_dtree = (fA0);
	tA636 = ((new com_jtransc_compression_jzlib_StaticTree()));
	fA0 = tA636;
	(tA636)["com.jtransc.compression.jzlib.StaticTree<init>([S[IIII)V"](com_jtransc_compression_jzlib_StaticTree._static_ltree, com_jtransc_compression_jzlib_Tree._extra_lbits, 257, 286, 15);
	com_jtransc_compression_jzlib_StaticTree._static_l_desc = (fA0);
	tA637 = ((new com_jtransc_compression_jzlib_StaticTree()));
	fA0 = tA637;
	(tA637)["com.jtransc.compression.jzlib.StaticTree<init>([S[IIII)V"](com_jtransc_compression_jzlib_StaticTree._static_dtree, com_jtransc_compression_jzlib_Tree._extra_dbits, 0, 30, 15);
	com_jtransc_compression_jzlib_StaticTree._static_d_desc = (fA0);
	tA638 = ((new com_jtransc_compression_jzlib_StaticTree()));
	fA0 = tA638;
	(tA638)["com.jtransc.compression.jzlib.StaticTree<init>([S[IIII)V"](null, com_jtransc_compression_jzlib_Tree._extra_blbits, 0, 19, 7);
	com_jtransc_compression_jzlib_StaticTree._static_bl_desc = (fA0);
	return;
};
function com_jtransc_compression_jzlib_Deflate$Config() {
}
com_jtransc_compression_jzlib_Deflate$Config.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_compression_jzlib_Deflate$Config.prototype.constructor = com_jtransc_compression_jzlib_Deflate$Config;
com_jtransc_compression_jzlib_Deflate$Config.prototype._nice_length = 0;
com_jtransc_compression_jzlib_Deflate$Config.prototype._max_chain = 0;
com_jtransc_compression_jzlib_Deflate$Config.prototype._max_lazy = 0;
com_jtransc_compression_jzlib_Deflate$Config.prototype._good_length = 0;
com_jtransc_compression_jzlib_Deflate$Config.prototype._func = 0;
com_jtransc_compression_jzlib_Deflate$Config.prototype.___id = 0;
com_jtransc_compression_jzlib_Deflate$Config.SI = function(){};
com_jtransc_compression_jzlib_Deflate$Config.prototype.__JT__CLASS_ID = com_jtransc_compression_jzlib_Deflate$Config.__JT__CLASS_ID = 899;
com_jtransc_compression_jzlib_Deflate$Config.prototype.__JT__CLASS_IDS = com_jtransc_compression_jzlib_Deflate$Config.__JT__CLASS_IDS = [899,656];
com_jtransc_compression_jzlib_Deflate$Config.prototype["com.jtransc.compression.jzlib.Deflate$Config<init>(IIIII)V"] = function(p0, p1, p2, p3, p4) { 
	(this)["java.lang.Object<init>()V"]();
	this._good_length = p0;
	this._max_lazy = p1;
	this._nice_length = p2;
	this._max_chain = p3;
	this._func = p4;
	return this;
	return this;
};
function com_jtransc_compression_jzlib_GZIPHeader() {
}
com_jtransc_compression_jzlib_GZIPHeader.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_compression_jzlib_GZIPHeader.prototype.constructor = com_jtransc_compression_jzlib_GZIPHeader;
com_jtransc_compression_jzlib_GZIPHeader.prototype._text = false;
com_jtransc_compression_jzlib_GZIPHeader.prototype._fhcrc = false;
com_jtransc_compression_jzlib_GZIPHeader.prototype._mtime = N.lnew(0, 0);
com_jtransc_compression_jzlib_GZIPHeader.prototype._os = 0;
com_jtransc_compression_jzlib_GZIPHeader.prototype._done = false;
com_jtransc_compression_jzlib_GZIPHeader.prototype._comment = null;
com_jtransc_compression_jzlib_GZIPHeader.prototype._extra = null;
com_jtransc_compression_jzlib_GZIPHeader.prototype.__name = null;
com_jtransc_compression_jzlib_GZIPHeader.prototype._crc = N.lnew(0, 0);
com_jtransc_compression_jzlib_GZIPHeader.prototype.___id = 0;
com_jtransc_compression_jzlib_GZIPHeader.SI = function(){};
com_jtransc_compression_jzlib_GZIPHeader.prototype.__JT__CLASS_ID = com_jtransc_compression_jzlib_GZIPHeader.__JT__CLASS_ID = 898;
com_jtransc_compression_jzlib_GZIPHeader.prototype.__JT__CLASS_IDS = com_jtransc_compression_jzlib_GZIPHeader.__JT__CLASS_IDS = [898,656,726];
com_jtransc_compression_jzlib_GZIPHeader.prototype["com.jtransc.compression.jzlib.GZIPHeader<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	this._text = false;
	this._fhcrc = false;
	this._os = 255;
	this._done = false;
	this._mtime = N.lnew(0, 0);
	return this;
	return this;
};
com_jtransc_compression_jzlib_GZIPHeader.prototype["clone()Ljava/lang/Object;"] = function() { 
	var _G = 0, lA1 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = (N.checkCast(java_lang_Object.prototype["clone()Ljava/lang/Object;"].call((this)), com_jtransc_compression_jzlib_GZIPHeader));
				if ((((lA1)._extra == null))) {
					_G = 1;
					continue;
				}
				lA2 = (new JA_B((lA1)._extra.length));
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"](((lA1)._extra), 0, lA2, 0, lA2.length);
				(lA1)._extra = (lA2);
				_G = 1;
				continue;
			case 1:
				if ((((lA1).__name == null))) {
					_G = 2;
					continue;
				}
				lA2 = (new JA_B((lA1).__name.length));
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"](((lA1).__name), 0, lA2, 0, lA2.length);
				(lA1).__name = (lA2);
				_G = 2;
				continue;
			case 2:
				if ((((lA1)._comment == null))) {
					_G = 3;
					continue;
				}
				lA2 = (new JA_B((lA1)._comment.length));
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"](((lA1)._comment), 0, lA2, 0, lA2.length);
				(lA1)._comment = (lA2);
				_G = 3;
				continue;
			case 3:
				return lA1;
			default:
				break;
		}
	}
	return null;
};
com_jtransc_compression_jzlib_GZIPHeader.prototype["put(Lcom/jtransc/compression/jzlib/Deflate;)V"] = function(p0) { 
	var _G = 0, lI2 = 0, lI3 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI2 = 0;
				if (!(this._text)) {
					_G = 1;
					continue;
				}
				lI2 = (((lI2 | 1))|0);
				_G = 1;
				continue;
			case 1:
				if (!(this._fhcrc)) {
					_G = 2;
					continue;
				}
				lI2 = (((lI2 | 2))|0);
				_G = 2;
				continue;
			case 2:
				if (((this._extra == null))) {
					_G = 3;
					continue;
				}
				lI2 = (((lI2 | 4))|0);
				_G = 3;
				continue;
			case 3:
				if (((this.__name == null))) {
					_G = 4;
					continue;
				}
				lI2 = (((lI2 | 8))|0);
				_G = 4;
				continue;
			case 4:
				if (((this._comment == null))) {
					_G = 5;
					continue;
				}
				lI2 = (((lI2 | 16))|0);
				_G = 5;
				continue;
			case 5:
				lI3 = 0;
				if (((p0._level != 1))) {
					_G = 6;
					continue;
				}
				lI3 = (((lI3 | 4))|0);
				_G = 7;
				continue;
			case 6:
				if (((p0._level != 9))) {
					_G = 7;
					continue;
				}
				lI3 = (((lI3 | 2))|0);
				_G = 7;
				continue;
			case 7:
				p0["put_short(I)V"](-29921);
				p0["put_byte(B)V"](8);
				p0["put_byte(B)V"](((lI2)<<24>>24));
				p0["put_byte(B)V"](((N.j2i(this._mtime))<<24>>24));
				p0["put_byte(B)V"](((N.j2i(N.lshr(this._mtime, 8)))<<24>>24));
				p0["put_byte(B)V"](((N.j2i(N.lshr(this._mtime, 16)))<<24>>24));
				p0["put_byte(B)V"](((N.j2i(N.lshr(this._mtime, 24)))<<24>>24));
				p0["put_byte(B)V"](((lI3)<<24>>24));
				p0["put_byte(B)V"](((this._os)<<24>>24));
				if (((this._extra == null))) {
					_G = 8;
					continue;
				}
				p0["put_byte(B)V"](((this._extra.length)<<24>>24));
				p0["put_byte(B)V"]((((((this._extra.length >> 8))|0))<<24>>24));
				p0["put_byte([BII)V"](this._extra, 0, this._extra.length);
				_G = 8;
				continue;
			case 8:
				if (((this.__name == null))) {
					_G = 9;
					continue;
				}
				p0["put_byte([BII)V"](this.__name, 0, this.__name.length);
				p0["put_byte(B)V"](0);
				_G = 9;
				continue;
			case 9:
				if (((this._comment == null))) {
					_G = 10;
					continue;
				}
				p0["put_byte([BII)V"](this._comment, 0, this._comment.length);
				p0["put_byte(B)V"](0);
				_G = 10;
				continue;
			case 10:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_GZIPHeader.prototype["setCRC(J)V"] = function(p0) { 
	this._crc = p0;
	return;
};
function com_jtransc_compression_jzlib_Tree() {
}
com_jtransc_compression_jzlib_Tree.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_compression_jzlib_Tree.prototype.constructor = com_jtransc_compression_jzlib_Tree;
com_jtransc_compression_jzlib_Tree.prototype._dyn_tree = null;
com_jtransc_compression_jzlib_Tree.prototype._max_code = 0;
com_jtransc_compression_jzlib_Tree.prototype._stat_desc = null;
com_jtransc_compression_jzlib_Tree.prototype.___id = 0;
com_jtransc_compression_jzlib_Tree.SI = function() { 
	com_jtransc_compression_jzlib_Tree._base_dist = null;
	com_jtransc_compression_jzlib_Tree._extra_lbits = null;
	com_jtransc_compression_jzlib_Tree.__dist_code = null;
	com_jtransc_compression_jzlib_Tree._extra_dbits = null;
	com_jtransc_compression_jzlib_Tree.__length_code = null;
	com_jtransc_compression_jzlib_Tree._base_length = null;
	com_jtransc_compression_jzlib_Tree._extra_blbits = null;
	com_jtransc_compression_jzlib_Tree._bl_order = null;
	com_jtransc_compression_jzlib_Tree["com.jtransc.compression.jzlib.Tree<clinit>()V"]();
};
com_jtransc_compression_jzlib_Tree.prototype.__JT__CLASS_ID = com_jtransc_compression_jzlib_Tree.__JT__CLASS_ID = 897;
com_jtransc_compression_jzlib_Tree.prototype.__JT__CLASS_IDS = com_jtransc_compression_jzlib_Tree.__JT__CLASS_IDS = [897,656];
com_jtransc_compression_jzlib_Tree.prototype["com.jtransc.compression.jzlib.Tree<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_compression_jzlib_Tree["com.jtransc.compression.jzlib.Tree<clinit>()V"] = function() { 
	var tA97 = null, tA609 = null, fA0 = null, tA29 = null, tA59 = null, tA78 = null, tA865 = null, tA894 = null, tA0 = null;
	tA0 = (new JA_I(29));
	fA0 = tA0;
	(tA0).data[0] = 0;
	(fA0).setArraySlice(1, [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0]);
	com_jtransc_compression_jzlib_Tree._extra_lbits = (fA0);
	tA29 = (new JA_I(30));
	fA0 = tA29;
	(tA29).data[0] = 0;
	(fA0).setArraySlice(1, [0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13]);
	com_jtransc_compression_jzlib_Tree._extra_dbits = (fA0);
	tA59 = (new JA_I(19));
	fA0 = tA59;
	(tA59).data[0] = 0;
	(fA0).setArraySlice(1, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 7]);
	com_jtransc_compression_jzlib_Tree._extra_blbits = (fA0);
	tA78 = (new JA_B(19));
	fA0 = tA78;
	(tA78).data[0] = 16;
	(fA0).setArraySlice(1, [17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]);
	com_jtransc_compression_jzlib_Tree._bl_order = (fA0);
	tA97 = (new JA_B(512));
	fA0 = tA97;
	(tA97).data[0] = 0;
	(fA0).setArraySlice(1, [1, 2, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 0, 0, 16, 17, 18, 18, 19, 19, 20, 20, 20, 20, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29]);
	com_jtransc_compression_jzlib_Tree.__dist_code = (fA0);
	tA609 = (new JA_B(256));
	fA0 = tA609;
	(tA609).data[0] = 0;
	(fA0).setArraySlice(1, [1, 2, 3, 4, 5, 6, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 12, 12, 13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28]);
	com_jtransc_compression_jzlib_Tree.__length_code = (fA0);
	tA865 = (new JA_I(29));
	fA0 = tA865;
	(tA865).data[0] = 0;
	(fA0).setArraySlice(1, [1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 20, 24, 28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 0]);
	com_jtransc_compression_jzlib_Tree._base_length = (fA0);
	tA894 = (new JA_I(30));
	fA0 = tA894;
	(tA894).data[0] = 0;
	(fA0).setArraySlice(1, [1, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 64, 96, 128, 192, 256, 384, 512, 768, 1024, 1536, 2048, 3072, 4096, 6144, 8192, 12288, 16384, 24576]);
	com_jtransc_compression_jzlib_Tree._base_dist = (fA0);
	return;
};
com_jtransc_compression_jzlib_Tree.prototype["build_tree(Lcom/jtransc/compression/jzlib/Deflate;)V"] = function(p0) { 
	var lA3 = null, lI4 = 0, lI8 = 0, lI6 = 0, fA1 = null, fA3 = null, tI21 = 0, tI25 = 0, tA9 = null, fI2 = 0, fI0 = 0, tA20 = null, tA26 = null, tI8 = 0, tA14 = null, tI16 = 0, _G = 0, lA2 = null, lI7 = 0, lI5 = 0, fA0 = null, fA2 = null, tI22 = 0, tA2 = null, tA6 = null, fI1 = 0, fI3 = 0, tA23 = null, tI1 = 0, tI3 = 0, tI5 = 0, tI7 = 0, tI13 = 0, tA17 = null, tI19 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lA2 = (this._dyn_tree);
				lA3 = (this._stat_desc._static_tree);
				lI4 = this._stat_desc._elems;
				lI7 = -1;
				p0._heap_len = 0;
				p0._heap_max = 573;
				lI5 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= lI4))) {
					_G = 2;
					continue;
				}
				if (((((lA2).data[((Math.imul(lI5, 2))|0)]) == 0))) {
					_G = 3;
					continue;
				}
				fA0 = (p0._heap);
				fA1 = (p0);
				tA2 = fA1;
				tI1 = (((p0._heap_len + 1))|0);
				fI1 = tI1;
				(tA2)._heap_len = tI1;
				tI3 = lI5;
				fI2 = tI3;
				lI7 = tI3;
				(fA0).data[fI1] = fI2;
				p0._depth.data[lI5] = 0;
				_G = 4;
				continue;
			case 3:
				(lA2).data[(((((Math.imul(lI5, 2))|0) + 1))|0)] = 0;
				_G = 4;
				continue;
			case 4:
				lI5 = (((lI5 + 1))|0);
				_G = 1;
				continue;
			case 2:
				if (((p0._heap_len >= 2))) {
					_G = 5;
					continue;
				}
				fA0 = (p0._heap);
				fA1 = (p0);
				tA6 = fA1;
				tI5 = (((p0._heap_len + 1))|0);
				fI1 = tI5;
				(tA6)._heap_len = tI5;
				if (((lI7 >= 2))) {
					_G = 6;
					continue;
				}
				lI7 = (((lI7 + 1))|0);
				fI2 = lI7;
				_G = 7;
				continue;
			case 6:
				fI2 = 0;
				_G = 7;
				continue;
			case 7:
				tA9 = fA0;
				tI8 = fI1;
				tI7 = fI2;
				fI0 = tI7;
				(tA9).data[tI8] = tI7;
				lI8 = fI0;
				(lA2).data[((Math.imul(lI8, 2))|0)] = 1;
				p0._depth.data[lI8] = 0;
				p0._opt_len = (((p0._opt_len - 1))|0);
				if (((lA3 == null))) {
					_G = 2;
					continue;
				}
				p0._static_len = (((p0._static_len - ((lA3).data[(((((Math.imul(lI8, 2))|0) + 1))|0)])))|0);
				_G = 2;
				continue;
			case 5:
				this._max_code = lI7;
				lI5 = (((p0._heap_len / 2))|0);
				_G = 8;
				continue;
			case 8:
				if (((lI5 < 1))) {
					_G = 9;
					continue;
				}
				p0["pqdownheap([SI)V"]((lA2), lI5);
				lI5 = (((lI5 + -1))|0);
				_G = 8;
				continue;
			case 9:
				lI8 = lI4;
				_G = 10;
				continue;
			case 10:
				lI5 = (p0._heap.data[1]);
				fA0 = (p0._heap);
				fI1 = 1;
				fA2 = (p0._heap);
				fA3 = (p0);
				tA14 = fA3;
				tI13 = p0._heap_len;
				fI3 = tI13;
				(tA14)._heap_len = (((tI13 - 1))|0);
				(fA0).data[fI1] = ((fA2).data[fI3]);
				p0["pqdownheap([SI)V"]((lA2), 1);
				lI6 = (p0._heap.data[1]);
				fA0 = (p0._heap);
				fA1 = (p0);
				tA17 = fA1;
				tI16 = (((p0._heap_max - 1))|0);
				fI1 = tI16;
				(tA17)._heap_max = tI16;
				(fA0).data[fI1] = lI5;
				fA0 = (p0._heap);
				fA1 = (p0);
				tA20 = fA1;
				tI19 = (((p0._heap_max - 1))|0);
				fI1 = tI19;
				(tA20)._heap_max = tI19;
				(fA0).data[fI1] = lI6;
				(lA2).data[((Math.imul(lI8, 2))|0)] = (((((((lA2).data[((Math.imul(lI5, 2))|0)]) + ((lA2).data[((Math.imul(lI6, 2))|0)])))|0))<<16>>16);
				p0._depth.data[lI8] = (((((java_lang_Math["max(II)I"]((((p0._depth.data[lI5]))|0), (((p0._depth.data[lI6]))|0)) + 1))|0))<<24>>24);
				fA0 = lA2;
				fI1 = (((((Math.imul(lI5, 2))|0) + 1))|0);
				fA2 = lA2;
				fI3 = (((((Math.imul(lI6, 2))|0) + 1))|0);
				tA23 = fA2;
				tI22 = fI3;
				tI21 = ((((lI8)<<16>>16))|0);
				fI2 = tI21;
				(tA23).data[tI22] = ((tI21)<<16>>16);
				(fA0).data[fI1] = ((fI2)<<16>>16);
				fA0 = (p0._heap);
				fI1 = 1;
				fI2 = lI8;
				lI8 = (((lI8 + 1))|0);
				(fA0).data[fI1] = fI2;
				p0["pqdownheap([SI)V"]((lA2), 1);
				if (((p0._heap_len >= 2))) {
					_G = 10;
					continue;
				}
				fA0 = (p0._heap);
				fA1 = (p0);
				tA26 = fA1;
				tI25 = (((p0._heap_max - 1))|0);
				fI1 = tI25;
				(tA26)._heap_max = tI25;
				(fA0).data[fI1] = (p0._heap.data[1]);
				this["gen_bitlen(Lcom/jtransc/compression/jzlib/Deflate;)V"](p0);
				com_jtransc_compression_jzlib_Tree["gen_codes([SI[S[S)V"]((lA2), lI7, p0._bl_count, p0._next_code);
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Tree.prototype["gen_bitlen(Lcom/jtransc/compression/jzlib/Deflate;)V"] = function(p0) { 
	var _G = 0, lA3 = null, lI13 = 0, lI11 = 0, lI12 = 0, lI5 = 0, lI6 = 0, lI10 = 0, lI7 = 0, lI8 = 0, lI9 = 0, tA1 = null, tA5 = null, tA7 = null, tA9 = null, lA2 = null, tI0 = 0, tI4 = 0, tI6 = 0, tI8 = 0, fA0 = null, lA4 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA2 = this._dyn_tree;
				lA3 = (this._stat_desc._static_tree);
				lA4 = this._stat_desc._extra_bits;
				lI5 = this._stat_desc._extra_base;
				lI6 = this._stat_desc._max_length;
				lI13 = 0;
				lI10 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI10 > 15))) {
					_G = 2;
					continue;
				}
				p0._bl_count.data[lI10] = 0;
				lI10 = (((lI10 + 1))|0);
				_G = 1;
				continue;
			case 2:
				lA2.data[(((((Math.imul((p0._heap.data[p0._heap_max]), 2))|0) + 1))|0)] = 0;
				lI7 = (((p0._heap_max + 1))|0);
				_G = 3;
				continue;
			case 3:
				if (((lI7 >= 573))) {
					_G = 4;
					continue;
				}
				lI8 = (p0._heap.data[lI7]);
				lI10 = ((((lA2.data[(((((Math.imul((lA2.data[(((((Math.imul(lI8, 2))|0) + 1))|0)]), 2))|0) + 1))|0)]) + 1))|0);
				if (((lI10 <= lI6))) {
					_G = 5;
					continue;
				}
				lI10 = lI6;
				lI13 = (((lI13 + 1))|0);
				_G = 5;
				continue;
			case 5:
				lA2.data[(((((Math.imul(lI8, 2))|0) + 1))|0)] = ((lI10)<<16>>16);
				if (((lI8 <= this._max_code))) {
					_G = 6;
					continue;
				}
				_G = 7;
				continue;
			case 6:
				tA1 = p0._bl_count;
				tI0 = lI10;
				tA1.data[tI0] = ((((((tA1.data[tI0]) + 1))|0))<<16>>16);
				lI11 = 0;
				if (((lI8 < lI5))) {
					_G = 8;
					continue;
				}
				lI11 = (lA4.data[(((lI8 - lI5))|0)]);
				_G = 8;
				continue;
			case 8:
				lI12 = (((lA2.data[((Math.imul(lI8, 2))|0)]))|0);
				p0._opt_len = (((p0._opt_len + ((Math.imul(lI12, (((lI10 + lI11))|0)))|0)))|0);
				if (((lA3 == null))) {
					_G = 7;
					continue;
				}
				p0._static_len = (((p0._static_len + ((Math.imul(lI12, (((((lA3).data[(((((Math.imul(lI8, 2))|0) + 1))|0)]) + lI11))|0)))|0)))|0);
				_G = 7;
				continue;
			case 7:
				lI7 = (((lI7 + 1))|0);
				_G = 3;
				continue;
			case 4:
				if (((lI13 != 0))) {
					_G = 9;
					continue;
				}
				return;
				_G = 9;
				continue;
			case 9:
				lI10 = (((lI6 - 1))|0);
				_G = 10;
				continue;
			case 10:
				if ((((p0._bl_count.data[lI10]) != 0))) {
					_G = 11;
					continue;
				}
				lI10 = (((lI10 + -1))|0);
				_G = 10;
				continue;
			case 11:
				tA5 = p0._bl_count;
				tI4 = lI10;
				tA5.data[tI4] = ((((((tA5.data[tI4]) - 1))|0))<<16>>16);
				tA7 = p0._bl_count;
				tI6 = (((lI10 + 1))|0);
				tA7.data[tI6] = ((((((tA7.data[tI6]) + 2))|0))<<16>>16);
				tA9 = p0._bl_count;
				tI8 = lI6;
				tA9.data[tI8] = ((((((tA9.data[tI8]) - 1))|0))<<16>>16);
				lI13 = (((lI13 + -2))|0);
				if (((lI13 > 0))) {
					_G = 9;
					continue;
				}
				lI10 = lI6;
				_G = 12;
				continue;
			case 12:
				if (((lI10 == 0))) {
					_G = 13;
					continue;
				}
				lI8 = (((p0._bl_count.data[lI10]))|0);
				_G = 14;
				continue;
			case 14:
				if (((lI8 == 0))) {
					_G = 15;
					continue;
				}
				fA0 = p0._heap;
				lI7 = (((lI7 + -1))|0);
				lI9 = (fA0.data[lI7]);
				if (((lI9 <= this._max_code))) {
					_G = 16;
					continue;
				}
				_G = 14;
				continue;
			case 16:
				if ((((lA2.data[(((((Math.imul(lI9, 2))|0) + 1))|0)]) == lI10))) {
					_G = 17;
					continue;
				}
				p0._opt_len = N.j2i(N.ladd(N.i2j(p0._opt_len), N.lmul(N.lsub(N.i2j(lI10), N.i2j((lA2.data[(((((Math.imul(lI9, 2))|0) + 1))|0)]))), N.i2j((lA2.data[((Math.imul(lI9, 2))|0)])))));
				lA2.data[(((((Math.imul(lI9, 2))|0) + 1))|0)] = ((lI10)<<16>>16);
				_G = 17;
				continue;
			case 17:
				lI8 = (((lI8 + -1))|0);
				_G = 14;
				continue;
			case 15:
				lI10 = (((lI10 + -1))|0);
				_G = 12;
				continue;
			case 13:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Tree["gen_codes([SI[S[S)V"] = function(p0, p1, p2, p3) { 
	var _G = 0, lI4 = 0, lI5 = 0, lI6 = 0, lI7 = 0, fA2 = null, tA5 = null, fA0 = null, fI1 = 0, fI2 = 0, fI3 = 0, tI0 = 0, tI1 = 0, tI4 = 0, tI3 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI4 = 0;
				p3.data[0] = 0;
				lI5 = 1;
				_G = 1;
				continue;
			case 1:
				if (((lI5 > 15))) {
					_G = 2;
					continue;
				}
				fA0 = p3;
				fI1 = lI5;
				tI0 = ((((((((((lI4 + (p2.data[(((lI5 - 1))|0)])))|0) << 1))|0))<<16>>16))|0);
				fI2 = tI0;
				lI4 = tI0;
				fA0.data[fI1] = ((fI2)<<16>>16);
				lI5 = (((lI5 + 1))|0);
				_G = 1;
				continue;
			case 2:
				lI6 = 0;
				_G = 3;
				continue;
			case 3:
				if (((lI6 > p1))) {
					_G = 4;
					continue;
				}
				lI7 = (((p0.data[(((((Math.imul(lI6, 2))|0) + 1))|0)]))|0);
				if (((lI7 != 0))) {
					_G = 5;
					continue;
				}
				_G = 6;
				continue;
			case 5:
				fA0 = p0;
				fI1 = ((Math.imul(lI6, 2))|0);
				tI1 = lI7;
				fA2 = (p3);
				fI3 = tI1;
				tA5 = fA2;
				tI4 = fI3;
				tI3 = (((p3.data[tI1]))|0);
				fI2 = tI3;
				(tA5).data[tI4] = (((((tI3 + 1))|0))<<16>>16);
				fA0.data[fI1] = ((com_jtransc_compression_jzlib_Tree["bi_reverse(II)I"](fI2, lI7))<<16>>16);
				_G = 6;
				continue;
			case 6:
				lI6 = (((lI6 + 1))|0);
				_G = 3;
				continue;
			case 4:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Tree["bi_reverse(II)I"] = function(p0, p1) { 
	var _G = 0, lI0 = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI0 = p0;
				lI1 = p1;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				lI2 = (((lI2 | (((lI0 & 1))|0)))|0);
				lI0 = (((lI0 >>> 1))|0);
				lI2 = (((lI2 << 1))|0);
				lI1 = (((lI1 + -1))|0);
				if (((lI1 > 0))) {
					_G = 1;
					continue;
				}
				return (((lI2 >>> 1))|0);
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_compression_jzlib_Tree["d_code(I)I"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 >= 256))) {
					_G = 1;
					continue;
				}
				fI0 = (((com_jtransc_compression_jzlib_Tree.__dist_code.data[p0]))|0);
				_G = 2;
				continue;
			case 1:
				fI0 = (((com_jtransc_compression_jzlib_Tree.__dist_code.data[(((256 + (((p0 >>> 7))|0)))|0)]))|0);
				_G = 2;
				continue;
			case 2:return fI0; 
			default:
				break;
		}
	}
	return 0;
};
function com_jtransc_compression_jzlib_Deflate() {
}
com_jtransc_compression_jzlib_Deflate.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_compression_jzlib_Deflate.prototype.constructor = com_jtransc_compression_jzlib_Deflate;
com_jtransc_compression_jzlib_Deflate.prototype._bl_desc = null;
com_jtransc_compression_jzlib_Deflate.prototype._l_desc = null;
com_jtransc_compression_jzlib_Deflate.prototype._dyn_ltree = null;
com_jtransc_compression_jzlib_Deflate.prototype._next_code = null;
com_jtransc_compression_jzlib_Deflate.prototype._depth = null;
com_jtransc_compression_jzlib_Deflate.prototype._strm = null;
com_jtransc_compression_jzlib_Deflate.prototype._dyn_dtree = null;
com_jtransc_compression_jzlib_Deflate.prototype._d_desc = null;
com_jtransc_compression_jzlib_Deflate.prototype._gheader = null;
com_jtransc_compression_jzlib_Deflate.prototype._wrap = 0;
com_jtransc_compression_jzlib_Deflate.prototype._bl_count = null;
com_jtransc_compression_jzlib_Deflate.prototype._heap = null;
com_jtransc_compression_jzlib_Deflate.prototype._bl_tree = null;
com_jtransc_compression_jzlib_Deflate.prototype._prev = null;
com_jtransc_compression_jzlib_Deflate.prototype._d_buf = 0;
com_jtransc_compression_jzlib_Deflate.prototype._pending_buf = null;
com_jtransc_compression_jzlib_Deflate.prototype._head = null;
com_jtransc_compression_jzlib_Deflate.prototype._l_buf = null;
com_jtransc_compression_jzlib_Deflate.prototype._window = null;
com_jtransc_compression_jzlib_Deflate.prototype._pending = 0;
com_jtransc_compression_jzlib_Deflate.prototype._level = 0;
com_jtransc_compression_jzlib_Deflate.prototype._lookahead = 0;
com_jtransc_compression_jzlib_Deflate.prototype._last_flush = 0;
com_jtransc_compression_jzlib_Deflate.prototype._w_bits = 0;
com_jtransc_compression_jzlib_Deflate.prototype._hash_size = 0;
com_jtransc_compression_jzlib_Deflate.prototype._status = 0;
com_jtransc_compression_jzlib_Deflate.prototype._strstart = 0;
com_jtransc_compression_jzlib_Deflate.prototype._pending_out = 0;
com_jtransc_compression_jzlib_Deflate.prototype._max_lazy_match = 0;
com_jtransc_compression_jzlib_Deflate.prototype._w_size = 0;
com_jtransc_compression_jzlib_Deflate.prototype._match_start = 0;
com_jtransc_compression_jzlib_Deflate.prototype._strategy = 0;
com_jtransc_compression_jzlib_Deflate.prototype._hash_mask = 0;
com_jtransc_compression_jzlib_Deflate.prototype._match_available = 0;
com_jtransc_compression_jzlib_Deflate.prototype._ins_h = 0;
com_jtransc_compression_jzlib_Deflate.prototype._hash_shift = 0;
com_jtransc_compression_jzlib_Deflate.prototype._w_mask = 0;
com_jtransc_compression_jzlib_Deflate.prototype._prev_match = 0;
com_jtransc_compression_jzlib_Deflate.prototype._match_length = 0;
com_jtransc_compression_jzlib_Deflate.prototype._prev_length = 0;
com_jtransc_compression_jzlib_Deflate.prototype._block_start = 0;
com_jtransc_compression_jzlib_Deflate.prototype._static_len = 0;
com_jtransc_compression_jzlib_Deflate.prototype._data_type = 0;
com_jtransc_compression_jzlib_Deflate.prototype._opt_len = 0;
com_jtransc_compression_jzlib_Deflate.prototype._bi_valid = 0;
com_jtransc_compression_jzlib_Deflate.prototype._bi_buf = 0;
com_jtransc_compression_jzlib_Deflate.prototype._matches = 0;
com_jtransc_compression_jzlib_Deflate.prototype._last_lit = 0;
com_jtransc_compression_jzlib_Deflate.prototype._heap_len = 0;
com_jtransc_compression_jzlib_Deflate.prototype._heap_max = 0;
com_jtransc_compression_jzlib_Deflate.prototype._last_eob_len = 0;
com_jtransc_compression_jzlib_Deflate.prototype._max_chain_length = 0;
com_jtransc_compression_jzlib_Deflate.prototype._nice_match = 0;
com_jtransc_compression_jzlib_Deflate.prototype._good_match = 0;
com_jtransc_compression_jzlib_Deflate.prototype._window_size = 0;
com_jtransc_compression_jzlib_Deflate.prototype._lit_bufsize = 0;
com_jtransc_compression_jzlib_Deflate.prototype._pending_buf_size = 0;
com_jtransc_compression_jzlib_Deflate.prototype._hash_bits = 0;
com_jtransc_compression_jzlib_Deflate.prototype._method = 0;
com_jtransc_compression_jzlib_Deflate.prototype.___id = 0;
com_jtransc_compression_jzlib_Deflate.SI = function() { 
	com_jtransc_compression_jzlib_Deflate._z_errmsg = null;
	com_jtransc_compression_jzlib_Deflate._config_table = null;
	com_jtransc_compression_jzlib_Deflate["com.jtransc.compression.jzlib.Deflate<clinit>()V"]();
};
com_jtransc_compression_jzlib_Deflate.prototype.__JT__CLASS_ID = com_jtransc_compression_jzlib_Deflate.__JT__CLASS_ID = 896;
com_jtransc_compression_jzlib_Deflate.prototype.__JT__CLASS_IDS = com_jtransc_compression_jzlib_Deflate.__JT__CLASS_IDS = [896,656,726];
com_jtransc_compression_jzlib_Deflate.prototype["com.jtransc.compression.jzlib.Deflate<init>(Lcom/jtransc/compression/jzlib/ZStream;)V"] = function(p0) { 
	var fA1 = null, fA0 = null, tA0 = null, tA1 = null, tA2 = null;
	(this)["java.lang.Object<init>()V"]();
	this._wrap = 1;
	fA0 = this;
	tA0 = ((new com_jtransc_compression_jzlib_Tree()));
	fA1 = tA0;
	(tA0)["com.jtransc.compression.jzlib.Tree<init>()V"]();
	fA0._l_desc = (fA1);
	fA0 = this;
	tA1 = ((new com_jtransc_compression_jzlib_Tree()));
	fA1 = tA1;
	(tA1)["com.jtransc.compression.jzlib.Tree<init>()V"]();
	fA0._d_desc = (fA1);
	fA0 = this;
	tA2 = ((new com_jtransc_compression_jzlib_Tree()));
	fA1 = tA2;
	(tA2)["com.jtransc.compression.jzlib.Tree<init>()V"]();
	fA0._bl_desc = (fA1);
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
};
com_jtransc_compression_jzlib_Deflate["com.jtransc.compression.jzlib.Deflate<clinit>()V"] = function() { 
	var tA1 = null, tA3 = null, tA5 = null, tA7 = null, tA9 = null, tA10 = null, fA0 = null, fA2 = null, tA0 = null, tA2 = null, tA4 = null, tA6 = null, tA8 = null, fI1 = 0;
	com_jtransc_compression_jzlib_Deflate._config_table = new JA_L(10, "[Lcom.jtransc.compression.jzlib.Deflate$Config;");
	fA0 = (com_jtransc_compression_jzlib_Deflate._config_table);
	fI1 = 0;
	tA0 = ((new com_jtransc_compression_jzlib_Deflate$Config()));
	fA2 = tA0;
	(tA0)["com.jtransc.compression.jzlib.Deflate$Config<init>(IIIII)V"](0, 0, 0, 0, 0);
	(fA0).data[fI1] = fA2;
	fA0 = (com_jtransc_compression_jzlib_Deflate._config_table);
	fI1 = 1;
	tA1 = ((new com_jtransc_compression_jzlib_Deflate$Config()));
	fA2 = tA1;
	(tA1)["com.jtransc.compression.jzlib.Deflate$Config<init>(IIIII)V"](4, 4, 8, 4, 1);
	(fA0).data[fI1] = fA2;
	fA0 = (com_jtransc_compression_jzlib_Deflate._config_table);
	fI1 = 2;
	tA2 = ((new com_jtransc_compression_jzlib_Deflate$Config()));
	fA2 = tA2;
	(tA2)["com.jtransc.compression.jzlib.Deflate$Config<init>(IIIII)V"](4, 5, 16, 8, 1);
	(fA0).data[fI1] = fA2;
	fA0 = (com_jtransc_compression_jzlib_Deflate._config_table);
	fI1 = 3;
	tA3 = ((new com_jtransc_compression_jzlib_Deflate$Config()));
	fA2 = tA3;
	(tA3)["com.jtransc.compression.jzlib.Deflate$Config<init>(IIIII)V"](4, 6, 32, 32, 1);
	(fA0).data[fI1] = fA2;
	fA0 = (com_jtransc_compression_jzlib_Deflate._config_table);
	fI1 = 4;
	tA4 = ((new com_jtransc_compression_jzlib_Deflate$Config()));
	fA2 = tA4;
	(tA4)["com.jtransc.compression.jzlib.Deflate$Config<init>(IIIII)V"](4, 4, 16, 16, 2);
	(fA0).data[fI1] = fA2;
	fA0 = (com_jtransc_compression_jzlib_Deflate._config_table);
	fI1 = 5;
	tA5 = ((new com_jtransc_compression_jzlib_Deflate$Config()));
	fA2 = tA5;
	(tA5)["com.jtransc.compression.jzlib.Deflate$Config<init>(IIIII)V"](8, 16, 32, 32, 2);
	(fA0).data[fI1] = fA2;
	fA0 = (com_jtransc_compression_jzlib_Deflate._config_table);
	fI1 = 6;
	tA6 = ((new com_jtransc_compression_jzlib_Deflate$Config()));
	fA2 = tA6;
	(tA6)["com.jtransc.compression.jzlib.Deflate$Config<init>(IIIII)V"](8, 16, 128, 128, 2);
	(fA0).data[fI1] = fA2;
	fA0 = (com_jtransc_compression_jzlib_Deflate._config_table);
	fI1 = 7;
	tA7 = ((new com_jtransc_compression_jzlib_Deflate$Config()));
	fA2 = tA7;
	(tA7)["com.jtransc.compression.jzlib.Deflate$Config<init>(IIIII)V"](8, 32, 128, 256, 2);
	(fA0).data[fI1] = fA2;
	fA0 = (com_jtransc_compression_jzlib_Deflate._config_table);
	fI1 = 8;
	tA8 = ((new com_jtransc_compression_jzlib_Deflate$Config()));
	fA2 = tA8;
	(tA8)["com.jtransc.compression.jzlib.Deflate$Config<init>(IIIII)V"](32, 128, 258, 1024, 2);
	(fA0).data[fI1] = fA2;
	fA0 = (com_jtransc_compression_jzlib_Deflate._config_table);
	fI1 = 9;
	tA9 = ((new com_jtransc_compression_jzlib_Deflate$Config()));
	fA2 = tA9;
	(tA9)["com.jtransc.compression.jzlib.Deflate$Config<init>(IIIII)V"](32, 258, 258, 4096, 2);
	(fA0).data[fI1] = fA2;
	tA10 = (new JA_L(10, "[Ljava.lang.String;"));
	fA0 = tA10;
	(tA10).data[0] = (S[18]);
	(fA0).setArraySlice(1, [(S[19]), (S[20]), (S[21]), (S[22]), (S[23]), (S[24]), (S[25]), (S[26]), (S[20])]);
	com_jtransc_compression_jzlib_Deflate._z_errmsg = (fA0);
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["clone()Ljava/lang/Object;"] = function() { 
	var _G = 0, lA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = (N.checkCast(java_lang_Object.prototype["clone()Ljava/lang/Object;"].call((this)), com_jtransc_compression_jzlib_Deflate));
				(lA1)._pending_buf = this["dup([B)[B"]((lA1)._pending_buf);
				(lA1)._l_buf = this["dup([B)[B"]((lA1)._l_buf);
				(lA1)._window = this["dup([B)[B"]((lA1)._window);
				(lA1)._prev = this["dup([S)[S"]((lA1)._prev);
				(lA1)._head = this["dup([S)[S"]((lA1)._head);
				(lA1)._dyn_ltree = this["dup([S)[S"]((lA1)._dyn_ltree);
				(lA1)._dyn_dtree = this["dup([S)[S"]((lA1)._dyn_dtree);
				(lA1)._bl_tree = this["dup([S)[S"]((lA1)._bl_tree);
				(lA1)._bl_count = this["dup([S)[S"]((lA1)._bl_count);
				(lA1)._next_code = this["dup([S)[S"]((lA1)._next_code);
				(lA1)._heap = this["dup([I)[I"]((lA1)._heap);
				(lA1)._depth = this["dup([B)[B"]((lA1)._depth);
				(lA1)._l_desc._dyn_tree = (lA1)._dyn_ltree;
				(lA1)._d_desc._dyn_tree = (lA1)._dyn_dtree;
				(lA1)._bl_desc._dyn_tree = (lA1)._bl_tree;
				if ((((lA1)._gheader == null))) {
					_G = 1;
					continue;
				}
				(lA1)._gheader = N.checkCast((lA1)._gheader["clone()Ljava/lang/Object;"](), com_jtransc_compression_jzlib_GZIPHeader);
				_G = 1;
				continue;
			case 1:
				return lA1;
			default:
				break;
		}
	}
	return null;
};
com_jtransc_compression_jzlib_Deflate.prototype["dup([S)[S"] = function(p0) { 
	var lA2 = null;
	lA2 = (new JA_S((p0).length));
	java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((p0), 0, lA2, 0, lA2.length);
	return (lA2);
};
com_jtransc_compression_jzlib_Deflate.prototype["dup([I)[I"] = function(p0) { 
	var lA2 = null;
	lA2 = (new JA_I((p0).length));
	java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((p0), 0, lA2, 0, lA2.length);
	return (lA2);
};
com_jtransc_compression_jzlib_Deflate.prototype["dup([B)[B"] = function(p0) { 
	var lA2 = null;
	lA2 = (new JA_B((p0).length));
	java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((p0), 0, lA2, 0, lA2.length);
	return (lA2);
};
com_jtransc_compression_jzlib_Deflate.prototype["deflate(I)I"] = function(p0) { 
	var _G = 0, fI0 = 0, lI2 = 0, lI3 = 0, lI4 = 0, lI5 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 > 4))) {
					_G = 1;
					continue;
				}
				if (((p0 >= 0))) {
					_G = 2;
					continue;
				}
				_G = 1;
				continue;
			case 1:
				return -2;
			case 2:
				if (((this._strm._next_out == null))) {
					_G = 3;
					continue;
				}
				if (((this._strm._next_in != null))) {
					_G = 4;
					continue;
				}
				if (((this._strm._avail_in != 0))) {
					_G = 3;
					continue;
				}
				_G = 4;
				continue;
			case 4:
				if (((this._status != 666))) {
					_G = 5;
					continue;
				}
				if (((p0 == 4))) {
					_G = 5;
					continue;
				}
				_G = 3;
				continue;
			case 3:
				this._strm._msg = (((com_jtransc_compression_jzlib_Deflate._z_errmsg).data[4]));
				return -2;
			case 5:
				if (((this._strm._avail_out != 0))) {
					_G = 6;
					continue;
				}
				this._strm._msg = (((com_jtransc_compression_jzlib_Deflate._z_errmsg).data[7]));
				return -5;
			case 6:
				lI2 = this._last_flush;
				this._last_flush = p0;
				if (((this._status != 42))) {
					_G = 7;
					continue;
				}
				if (((this._wrap != 2))) {
					_G = 8;
					continue;
				}
				this["getGZIPHeader()Lcom/jtransc/compression/jzlib/GZIPHeader;"]()["put(Lcom/jtransc/compression/jzlib/Deflate;)V"](this);
				this._status = 113;
				this._strm._adler["reset()V"]();
				_G = 7;
				continue;
			case 8:
				lI3 = ((((((8 + ((((((this._w_bits - 8))|0) << 4))|0)))|0) << 8))|0);
				lI4 = (((((((((this._level - 1))|0) & 255))|0) >> 1))|0);
				if (((lI4 <= 3))) {
					_G = 9;
					continue;
				}
				lI4 = 3;
				_G = 9;
				continue;
			case 9:
				lI3 = (((lI3 | (((lI4 << 6))|0)))|0);
				if (((this._strstart == 0))) {
					_G = 10;
					continue;
				}
				lI3 = (((lI3 | 32))|0);
				_G = 10;
				continue;
			case 10:
				lI3 = (((lI3 + (((31 - (((lI3 % 31))|0)))|0)))|0);
				this._status = 113;
				this["putShortMSB(I)V"](lI3);
				if (((this._strstart == 0))) {
					_G = 11;
					continue;
				}
				lI5 = this._strm._adler["getValue()I"]();
				this["putShortMSB(I)V"]((((lI5 >>> 16))|0));
				this["putShortMSB(I)V"]((((lI5 & 65535))|0));
				_G = 11;
				continue;
			case 11:
				this._strm._adler["reset()V"]();
				_G = 7;
				continue;
			case 7:
				if (((this._pending == 0))) {
					_G = 12;
					continue;
				}
				this._strm["flush_pending()V"]();
				if (((this._strm._avail_out != 0))) {
					_G = 13;
					continue;
				}
				this._last_flush = -1;
				return 0;
			case 12:
				if (((this._strm._avail_in != 0))) {
					_G = 13;
					continue;
				}
				if (((p0 > lI2))) {
					_G = 13;
					continue;
				}
				if (((p0 == 4))) {
					_G = 13;
					continue;
				}
				this._strm._msg = (((com_jtransc_compression_jzlib_Deflate._z_errmsg).data[7]));
				return -5;
			case 13:
				if (((this._status != 666))) {
					_G = 14;
					continue;
				}
				if (((this._strm._avail_in == 0))) {
					_G = 14;
					continue;
				}
				this._strm._msg = (((com_jtransc_compression_jzlib_Deflate._z_errmsg).data[7]));
				return -5;
			case 14:
				if (((this._strm._avail_in != 0))) {
					_G = 15;
					continue;
				}
				if (((this._lookahead != 0))) {
					_G = 15;
					continue;
				}
				if (((p0 == 0))) {
					_G = 16;
					continue;
				}
				if (((this._status == 666))) {
					_G = 16;
					continue;
				}
				_G = 15;
				continue;
			case 15:
				lI3 = -1;
				switch ((((com_jtransc_compression_jzlib_Deflate._config_table).data[this._level]))._func) {
					case 0:
						_G = 18;
						continue;
					case 1:
						_G = 19;
						continue;
					case 2:
						_G = 20;
						continue;
					default:
						_G = 17;
						continue;
				}
				_G = 18;
				continue;
			case 18:
				lI3 = this["deflate_stored(I)I"](p0);
				_G = 17;
				continue;
			case 19:
				lI3 = this["deflate_fast(I)I"](p0);
				_G = 17;
				continue;
			case 20:
				lI3 = this["deflate_slow(I)I"](p0);
				_G = 17;
				continue;
			case 17:
				if (((lI3 == 2))) {
					_G = 21;
					continue;
				}
				if (((lI3 != 3))) {
					_G = 22;
					continue;
				}
				_G = 21;
				continue;
			case 21:
				this._status = 666;
				_G = 22;
				continue;
			case 22:
				if (((lI3 == 0))) {
					_G = 23;
					continue;
				}
				if (((lI3 != 2))) {
					_G = 24;
					continue;
				}
				_G = 23;
				continue;
			case 23:
				if (((this._strm._avail_out != 0))) {
					_G = 25;
					continue;
				}
				this._last_flush = -1;
				_G = 25;
				continue;
			case 25:
				return 0;
			case 24:
				if (((lI3 != 1))) {
					_G = 16;
					continue;
				}
				if (((p0 != 1))) {
					_G = 26;
					continue;
				}
				this["_tr_align()V"]();
				_G = 27;
				continue;
			case 26:
				this["_tr_stored_block(IIZ)V"](0, 0, false);
				if (((p0 != 3))) {
					_G = 27;
					continue;
				}
				lI4 = 0;
				_G = 28;
				continue;
			case 28:
				if (((lI4 >= this._hash_size))) {
					_G = 27;
					continue;
				}
				this._head.data[lI4] = 0;
				lI4 = (((lI4 + 1))|0);
				_G = 28;
				continue;
			case 27:
				this._strm["flush_pending()V"]();
				if (((this._strm._avail_out != 0))) {
					_G = 16;
					continue;
				}
				this._last_flush = -1;
				return 0;
			case 16:
				if (((p0 == 4))) {
					_G = 29;
					continue;
				}
				return 0;
			case 29:
				if (((this._wrap > 0))) {
					_G = 30;
					continue;
				}
				return 1;
			case 30:
				if (((this._wrap != 2))) {
					_G = 31;
					continue;
				}
				lI3 = this._strm._adler["getValue()I"]();
				this["put_byte(B)V"]((((((lI3 & 255))|0))<<24>>24));
				this["put_byte(B)V"](((((((((lI3 >> 8))|0) & 255))|0))<<24>>24));
				this["put_byte(B)V"](((((((((lI3 >> 16))|0) & 255))|0))<<24>>24));
				this["put_byte(B)V"](((((((((lI3 >> 24))|0) & 255))|0))<<24>>24));
				this["put_byte(B)V"](((N.j2i(N.land(this._strm._total_in, N.lnew(0, 255))))<<24>>24));
				this["put_byte(B)V"](((N.j2i(N.land(N.lshr(this._strm._total_in, 8), N.lnew(0, 255))))<<24>>24));
				this["put_byte(B)V"](((N.j2i(N.land(N.lshr(this._strm._total_in, 16), N.lnew(0, 255))))<<24>>24));
				this["put_byte(B)V"](((N.j2i(N.land(N.lshr(this._strm._total_in, 24), N.lnew(0, 255))))<<24>>24));
				this["getGZIPHeader()Lcom/jtransc/compression/jzlib/GZIPHeader;"]()["setCRC(J)V"](N.i2j(lI3));
				_G = 32;
				continue;
			case 31:
				lI3 = this._strm._adler["getValue()I"]();
				this["putShortMSB(I)V"]((((lI3 >>> 16))|0));
				this["putShortMSB(I)V"]((((lI3 & 65535))|0));
				_G = 32;
				continue;
			case 32:
				this._strm["flush_pending()V"]();
				if (((this._wrap <= 0))) {
					_G = 33;
					continue;
				}
				this._wrap = ((-(this._wrap))|0);
				_G = 33;
				continue;
			case 33:
				if (((this._pending == 0))) {
					_G = 34;
					continue;
				}
				fI0 = 0;
				_G = 35;
				continue;
			case 34:
				fI0 = 1;
				_G = 35;
				continue;
			case 35:return fI0; 
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_compression_jzlib_Deflate.prototype["put_byte(B)V"] = function(p0) { 
	var fA0 = null, fI1 = 0, fA1 = null, tI1 = 0, tA2 = null;
	fA0 = this._pending_buf;
	fA1 = (this);
	tA2 = fA1;
	tI1 = this._pending;
	fI1 = tI1;
	(tA2)._pending = (((tI1 + 1))|0);
	fA0.data[fI1] = p0;
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["put_short(I)V"] = function(p0) { 
	this["put_byte(B)V"](((p0)<<24>>24));
	this["put_byte(B)V"]((((((p0 >>> 8))|0))<<24>>24));
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["put_byte([BII)V"] = function(p0, p1, p2) { 
	java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((p0), p1, (this._pending_buf), this._pending, p2);
	this._pending = (((this._pending + p2))|0);
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["deflate_slow(I)I"] = function(p0) { 
	var _G = 0, fI0 = 0, fI1 = 0, lI2 = 0, lI4 = 0, lI3 = 0, fA0 = null, tI3 = 0, tI6 = 0, tA4 = null, tA7 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((this._lookahead >= 262))) {
					_G = 2;
					continue;
				}
				this["fill_window()V"]();
				if (((this._lookahead >= 262))) {
					_G = 3;
					continue;
				}
				if (((p0 != 0))) {
					_G = 3;
					continue;
				}
				return 0;
			case 3:
				if (((this._lookahead != 0))) {
					_G = 2;
					continue;
				}
				_G = 4;
				continue;
			case 2:
				if (((this._lookahead < 3))) {
					_G = 5;
					continue;
				}
				this._ins_h = (((((((((this._ins_h << this._hash_shift))|0) ^ ((((this._window.data[(((this._strstart + 2))|0)]) & 255))|0)))|0) & this._hash_mask))|0);
				lI2 = ((((this._head.data[this._ins_h]) & 65535))|0);
				this._prev.data[(((this._strstart & this._w_mask))|0)] = (this._head.data[this._ins_h]);
				this._head.data[this._ins_h] = ((this._strstart)<<16>>16);
				_G = 5;
				continue;
			case 5:
				this._prev_length = this._match_length;
				this._prev_match = this._match_start;
				this._match_length = 2;
				if (((lI2 == 0))) {
					_G = 6;
					continue;
				}
				if (((this._prev_length >= this._max_lazy_match))) {
					_G = 6;
					continue;
				}
				if (((((((((this._strstart - lI2))|0) & 65535))|0) > (((this._w_size - 262))|0)))) {
					_G = 6;
					continue;
				}
				if (((this._strategy == 2))) {
					_G = 7;
					continue;
				}
				this._match_length = this["longest_match(I)I"](lI2);
				_G = 7;
				continue;
			case 7:
				if (((this._match_length > 5))) {
					_G = 6;
					continue;
				}
				if (((this._strategy == 1))) {
					_G = 8;
					continue;
				}
				if (((this._match_length != 3))) {
					_G = 6;
					continue;
				}
				if ((((((this._strstart - this._match_start))|0) <= 4096))) {
					_G = 6;
					continue;
				}
				_G = 8;
				continue;
			case 8:
				this._match_length = 2;
				_G = 6;
				continue;
			case 6:
				if (((this._prev_length < 3))) {
					_G = 9;
					continue;
				}
				if (((this._match_length > this._prev_length))) {
					_G = 9;
					continue;
				}
				lI4 = ((((((this._strstart + this._lookahead))|0) - 3))|0);
				lI3 = ((N.z2i(this["_tr_tally(II)Z"](((((((this._strstart - 1))|0) - this._prev_match))|0), (((this._prev_length - 3))|0))))|0);
				this._lookahead = (((this._lookahead - (((this._prev_length - 1))|0)))|0);
				this._prev_length = (((this._prev_length - 2))|0);
				_G = 10;
				continue;
			case 10:
				fA0 = (this);
				tA4 = fA0;
				tI3 = (((this._strstart + 1))|0);
				fI0 = tI3;
				(tA4)._strstart = tI3;
				if (((fI0 > lI4))) {
					_G = 11;
					continue;
				}
				this._ins_h = (((((((((this._ins_h << this._hash_shift))|0) ^ ((((this._window.data[(((this._strstart + 2))|0)]) & 255))|0)))|0) & this._hash_mask))|0);
				lI2 = ((((this._head.data[this._ins_h]) & 65535))|0);
				this._prev.data[(((this._strstart & this._w_mask))|0)] = (this._head.data[this._ins_h]);
				this._head.data[this._ins_h] = ((this._strstart)<<16>>16);
				_G = 11;
				continue;
			case 11:
				fA0 = (this);
				tA7 = fA0;
				tI6 = (((this._prev_length - 1))|0);
				fI0 = tI6;
				(tA7)._prev_length = tI6;
				if (((fI0 != 0))) {
					_G = 10;
					continue;
				}
				this._match_available = 0;
				this._match_length = 2;
				this._strstart = (((this._strstart + 1))|0);
				if (((lI3 == 0))) {
					_G = 12;
					continue;
				}
				this["flush_block_only(Z)V"](false);
				if (((this._strm._avail_out != 0))) {
					_G = 12;
					continue;
				}
				return 0;
			case 12:
				_G = 1;
				continue;
			case 9:
				if (((this._match_available == 0))) {
					_G = 13;
					continue;
				}
				lI3 = ((N.z2i(this["_tr_tally(II)Z"](0, ((((this._window.data[(((this._strstart - 1))|0)]) & 255))|0))))|0);
				if (((lI3 == 0))) {
					_G = 14;
					continue;
				}
				this["flush_block_only(Z)V"](false);
				_G = 14;
				continue;
			case 14:
				this._strstart = (((this._strstart + 1))|0);
				this._lookahead = (((this._lookahead - 1))|0);
				if (((this._strm._avail_out != 0))) {
					_G = 1;
					continue;
				}
				return 0;
			case 13:
				this._match_available = 1;
				this._strstart = (((this._strstart + 1))|0);
				this._lookahead = (((this._lookahead - 1))|0);
				_G = 1;
				continue;
			case 4:
				if (((this._match_available == 0))) {
					_G = 15;
					continue;
				}
				lI3 = ((N.z2i(this["_tr_tally(II)Z"](0, ((((this._window.data[(((this._strstart - 1))|0)]) & 255))|0))))|0);
				this._match_available = 0;
				_G = 15;
				continue;
			case 15:
				fA0 = (this);
				if (((p0 != 4))) {
					_G = 16;
					continue;
				}
				fI1 = 1;
				_G = 17;
				continue;
			case 16:
				fI1 = 0;
				_G = 17;
				continue;
			case 17:
				(fA0)["flush_block_only(Z)V"](((fI1)!=0));
				if (((this._strm._avail_out != 0))) {
					_G = 18;
					continue;
				}
				if (((p0 != 4))) {
					_G = 19;
					continue;
				}
				return 2;
			case 19:
				return 0;
			case 18:
				if (((p0 != 4))) {
					_G = 20;
					continue;
				}
				fI0 = 3;
				_G = 21;
				continue;
			case 20:
				fI0 = 1;
				_G = 21;
				continue;
			case 21:return fI0; 
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_compression_jzlib_Deflate.prototype["flush_block_only(Z)V"] = function(p0) { 
	var _G = 0, fI1 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._block_start < 0))) {
					_G = 1;
					continue;
				}
				fI1 = this._block_start;
				_G = 2;
				continue;
			case 1:
				fI1 = -1;
				_G = 2;
				continue;
			case 2:
				this["_tr_flush_block(IIZ)V"](fI1, (((this._strstart - this._block_start))|0), p0);
				this._block_start = this._strstart;
				this._strm["flush_pending()V"]();
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["_tr_flush_block(IIZ)V"] = function(p0, p1, p2) { 
	var _G = 0, fI0 = 0, fI1 = 0, fI2 = 0, lI6 = 0, lI4 = 0, lI5 = 0, fA0 = null, tI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI6 = 0;
				if (((this._level <= 0))) {
					_G = 1;
					continue;
				}
				if (((this._data_type != 2))) {
					_G = 2;
					continue;
				}
				this["set_data_type()V"]();
				_G = 2;
				continue;
			case 2:
				this._l_desc["build_tree(Lcom/jtransc/compression/jzlib/Deflate;)V"](this);
				this._d_desc["build_tree(Lcom/jtransc/compression/jzlib/Deflate;)V"](this);
				lI6 = this["build_bl_tree()I"]();
				lI4 = (((((((((this._opt_len + 3))|0) + 7))|0) >>> 3))|0);
				lI5 = (((((((((this._static_len + 3))|0) + 7))|0) >>> 3))|0);
				if (((lI5 > lI4))) {
					_G = 3;
					continue;
				}
				lI4 = lI5;
				_G = 3;
				continue;
			case 1:
				tI0 = (((p1 + 5))|0);
				fI0 = tI0;
				lI5 = tI0;
				lI4 = fI0;
				_G = 3;
				continue;
			case 3:
				if ((((((p1 + 4))|0) > lI4))) {
					_G = 4;
					continue;
				}
				if (((p0 == -1))) {
					_G = 4;
					continue;
				}
				this["_tr_stored_block(IIZ)V"](p0, p1, p2);
				_G = 5;
				continue;
			case 4:
				if (((lI5 != lI4))) {
					_G = 6;
					continue;
				}
				fA0 = this;
				fI1 = 2;
				if (!(p2)) {
					_G = 7;
					continue;
				}
				fI2 = 1;
				_G = 8;
				continue;
			case 7:
				fI2 = 0;
				_G = 8;
				continue;
			case 8:
				fA0["send_bits(II)V"]((((fI1 + fI2))|0), 3);
				this["compress_block([S[S)V"](com_jtransc_compression_jzlib_StaticTree._static_ltree, com_jtransc_compression_jzlib_StaticTree._static_dtree);
				_G = 5;
				continue;
			case 6:
				fA0 = this;
				fI1 = 4;
				if (!(p2)) {
					_G = 9;
					continue;
				}
				fI2 = 1;
				_G = 10;
				continue;
			case 9:
				fI2 = 0;
				_G = 10;
				continue;
			case 10:
				fA0["send_bits(II)V"]((((fI1 + fI2))|0), 3);
				this["send_all_trees(III)V"]((((this._l_desc._max_code + 1))|0), (((this._d_desc._max_code + 1))|0), (((lI6 + 1))|0));
				this["compress_block([S[S)V"](this._dyn_ltree, this._dyn_dtree);
				_G = 5;
				continue;
			case 5:
				this["init_block()V"]();
				if (!(p2)) {
					_G = 11;
					continue;
				}
				this["bi_windup()V"]();
				_G = 11;
				continue;
			case 11:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["send_all_trees(III)V"] = function(p0, p1, p2) { 
	var _G = 0, lI4 = 0;
	while (true) {
		switch (_G) {
			case 0:
				this["send_bits(II)V"]((((p0 - 257))|0), 5);
				this["send_bits(II)V"]((((p1 - 1))|0), 5);
				this["send_bits(II)V"]((((p2 - 4))|0), 4);
				lI4 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI4 >= p2))) {
					_G = 2;
					continue;
				}
				this["send_bits(II)V"]((((this._bl_tree.data[(((((Math.imul((com_jtransc_compression_jzlib_Tree._bl_order.data[lI4]), 2))|0) + 1))|0)]))|0), 3);
				lI4 = (((lI4 + 1))|0);
				_G = 1;
				continue;
			case 2:
				this["send_tree([SI)V"](this._dyn_ltree, (((p0 - 1))|0));
				this["send_tree([SI)V"](this._dyn_dtree, (((p1 - 1))|0));
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["send_tree([SI)V"] = function(p0, p1) { 
	var _G = 0, lI4 = 0, lI6 = 0, lI7 = 0, lI8 = 0, lI9 = 0, lI3 = 0, lI5 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI4 = -1;
				lI6 = (((p0.data[1]))|0);
				lI7 = 0;
				lI8 = 7;
				lI9 = 4;
				if (((lI6 != 0))) {
					_G = 1;
					continue;
				}
				lI8 = 138;
				lI9 = 3;
				_G = 1;
				continue;
			case 1:
				lI3 = 0;
				_G = 2;
				continue;
			case 2:
				if (((lI3 > p1))) {
					_G = 3;
					continue;
				}
				lI5 = lI6;
				lI6 = (((p0.data[(((((Math.imul((((lI3 + 1))|0), 2))|0) + 1))|0)]))|0);
				lI7 = (((lI7 + 1))|0);
				if (((lI7 >= lI8))) {
					_G = 4;
					continue;
				}
				if (((lI5 != lI6))) {
					_G = 4;
					continue;
				}
				_G = 5;
				continue;
			case 4:
				if (((lI7 >= lI9))) {
					_G = 6;
					continue;
				}
				_G = 7;
				continue;
			case 7:
				this["send_code(I[S)V"](lI5, this._bl_tree);
				lI7 = (((lI7 + -1))|0);
				if (((lI7 != 0))) {
					_G = 7;
					continue;
				}
				_G = 8;
				continue;
			case 6:
				if (((lI5 == 0))) {
					_G = 9;
					continue;
				}
				if (((lI5 == lI4))) {
					_G = 10;
					continue;
				}
				this["send_code(I[S)V"](lI5, this._bl_tree);
				lI7 = (((lI7 + -1))|0);
				_G = 10;
				continue;
			case 10:
				this["send_code(I[S)V"](16, this._bl_tree);
				this["send_bits(II)V"]((((lI7 - 3))|0), 2);
				_G = 8;
				continue;
			case 9:
				if (((lI7 > 10))) {
					_G = 11;
					continue;
				}
				this["send_code(I[S)V"](17, this._bl_tree);
				this["send_bits(II)V"]((((lI7 - 3))|0), 3);
				_G = 8;
				continue;
			case 11:
				this["send_code(I[S)V"](18, this._bl_tree);
				this["send_bits(II)V"]((((lI7 - 11))|0), 7);
				_G = 8;
				continue;
			case 8:
				lI7 = 0;
				lI4 = lI5;
				if (((lI6 != 0))) {
					_G = 12;
					continue;
				}
				lI8 = 138;
				lI9 = 3;
				_G = 5;
				continue;
			case 12:
				if (((lI5 != lI6))) {
					_G = 13;
					continue;
				}
				lI8 = 6;
				lI9 = 3;
				_G = 5;
				continue;
			case 13:
				lI8 = 7;
				lI9 = 4;
				_G = 5;
				continue;
			case 5:
				lI3 = (((lI3 + 1))|0);
				_G = 2;
				continue;
			case 3:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["send_code(I[S)V"] = function(p0, p1) { 
	var lI3 = 0;
	lI3 = ((Math.imul(p0, 2))|0);
	this["send_bits(II)V"](((((p1.data[lI3]) & 65535))|0), ((((p1.data[(((lI3 + 1))|0)]) & 65535))|0));
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["send_bits(II)V"] = function(p0, p1) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._bi_valid <= (((16 - p1))|0)))) {
					_G = 1;
					continue;
				}
				this._bi_buf = (((((this._bi_buf | ((((((p0 << this._bi_valid))|0) & 65535))|0)))|0))<<16>>16);
				this["put_short(I)V"](((this._bi_buf)|0));
				this._bi_buf = (((((p0 >>> (((16 - this._bi_valid))|0)))|0))<<16>>16);
				this._bi_valid = (((this._bi_valid + (((p1 - 16))|0)))|0);
				_G = 2;
				continue;
			case 1:
				this._bi_buf = (((((this._bi_buf | ((((((p0 << this._bi_valid))|0) & 65535))|0)))|0))<<16>>16);
				this._bi_valid = (((this._bi_valid + p1))|0);
				_G = 2;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["bi_windup()V"] = function() { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._bi_valid <= 8))) {
					_G = 1;
					continue;
				}
				this["put_short(I)V"](((this._bi_buf)|0));
				_G = 2;
				continue;
			case 1:
				if (((this._bi_valid <= 0))) {
					_G = 2;
					continue;
				}
				this["put_byte(B)V"](((this._bi_buf)<<24>>24));
				_G = 2;
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
};
com_jtransc_compression_jzlib_Deflate.prototype["init_block()V"] = function() { 
	var _G = 0, fI1 = 0, lI1 = 0, fA1 = null, fA0 = null, tI0 = 0, tI2 = 0, tA1 = null, tA3 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= 286))) {
					_G = 2;
					continue;
				}
				this._dyn_ltree.data[((Math.imul(lI1, 2))|0)] = 0;
				lI1 = (((lI1 + 1))|0);
				_G = 1;
				continue;
			case 2:
				lI1 = 0;
				_G = 3;
				continue;
			case 3:
				if (((lI1 >= 30))) {
					_G = 4;
					continue;
				}
				this._dyn_dtree.data[((Math.imul(lI1, 2))|0)] = 0;
				lI1 = (((lI1 + 1))|0);
				_G = 3;
				continue;
			case 4:
				lI1 = 0;
				_G = 5;
				continue;
			case 5:
				if (((lI1 >= 19))) {
					_G = 6;
					continue;
				}
				this._bl_tree.data[((Math.imul(lI1, 2))|0)] = 0;
				lI1 = (((lI1 + 1))|0);
				_G = 5;
				continue;
			case 6:
				this._dyn_ltree.data[512] = 1;
				fA0 = this;
				fA1 = (this);
				tA1 = fA1;
				tI0 = 0;
				fI1 = tI0;
				(tA1)._static_len = tI0;
				fA0._opt_len = fI1;
				fA0 = this;
				fA1 = (this);
				tA3 = fA1;
				tI2 = 0;
				fI1 = tI2;
				(tA3)._matches = tI2;
				fA0._last_lit = fI1;
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["set_data_type()V"] = function() { 
	var _G = 0, fI1 = 0, lI1 = 0, lI2 = 0, lI3 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				lI2 = 0;
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= 7))) {
					_G = 2;
					continue;
				}
				lI3 = (((lI3 + (this._dyn_ltree.data[((Math.imul(lI1, 2))|0)])))|0);
				lI1 = (((lI1 + 1))|0);
				_G = 1;
				continue;
			case 2:
				if (((lI1 >= 128))) {
					_G = 3;
					continue;
				}
				lI2 = (((lI2 + (this._dyn_ltree.data[((Math.imul(lI1, 2))|0)])))|0);
				lI1 = (((lI1 + 1))|0);
				_G = 2;
				continue;
			case 3:
				if (((lI1 >= 256))) {
					_G = 4;
					continue;
				}
				lI3 = (((lI3 + (this._dyn_ltree.data[((Math.imul(lI1, 2))|0)])))|0);
				lI1 = (((lI1 + 1))|0);
				_G = 3;
				continue;
			case 4:
				if (((lI3 <= (((lI2 >>> 2))|0)))) {
					_G = 5;
					continue;
				}
				fI1 = 0;
				_G = 6;
				continue;
			case 5:
				fI1 = 1;
				_G = 6;
				continue;
			case 6:
				this._data_type = ((fI1)<<24>>24);
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["pqdownheap([SI)V"] = function(p0, p1) { 
	var _G = 0, fA3 = null, fI1 = 0, fI2 = 0, lI2 = 0, lI3 = 0, lI4 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI2 = p1;
				lI3 = (this._heap.data[lI2]);
				lI4 = (((lI2 << 1))|0);
				_G = 1;
				continue;
			case 1:
				if (((lI4 > this._heap_len))) {
					_G = 2;
					continue;
				}
				if (((lI4 >= this._heap_len))) {
					_G = 3;
					continue;
				}
				fI1 = (this._heap.data[(((lI4 + 1))|0)]);
				fI2 = (this._heap.data[lI4]);
				fA3 = this._depth;
				if (!(com_jtransc_compression_jzlib_Deflate["smaller([SII[B)Z"](p0, fI1, fI2, fA3))) {
					_G = 3;
					continue;
				}
				lI4 = (((lI4 + 1))|0);
				_G = 3;
				continue;
			case 3:
				if (!(com_jtransc_compression_jzlib_Deflate["smaller([SII[B)Z"](p0, lI3, (this._heap.data[lI4]), this._depth))) {
					_G = 4;
					continue;
				}
				_G = 2;
				continue;
			case 4:
				this._heap.data[lI2] = (this._heap.data[lI4]);
				lI2 = lI4;
				lI4 = (((lI4 << 1))|0);
				_G = 1;
				continue;
			case 2:
				this._heap.data[lI2] = lI3;
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate["smaller([SII[B)Z"] = function(p0, p1, p2, p3) { 
	var _G = 0, fI0 = 0, lI4 = 0, lI5 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI4 = (((p0.data[((Math.imul(p1, 2))|0)]))|0);
				lI5 = (((p0.data[((Math.imul(p2, 2))|0)]))|0);
				if (((lI4 < lI5))) {
					_G = 1;
					continue;
				}
				if (((lI4 != lI5))) {
					_G = 2;
					continue;
				}
				if ((((p3.data[p1]) > (p3.data[p2])))) {
					_G = 2;
					continue;
				}
				_G = 1;
				continue;
			case 1:
				fI0 = 1;
				_G = 3;
				continue;
			case 2:
				fI0 = 0;
				_G = 3;
				continue;
			case 3:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
com_jtransc_compression_jzlib_Deflate.prototype["build_bl_tree()I"] = function() { 
	var _G = 0, lI1 = 0;
	while (true) {
		switch (_G) {
			case 0:
				this["scan_tree([SI)V"](this._dyn_ltree, this._l_desc._max_code);
				this["scan_tree([SI)V"](this._dyn_dtree, this._d_desc._max_code);
				this._bl_desc["build_tree(Lcom/jtransc/compression/jzlib/Deflate;)V"](this);
				lI1 = 18;
				_G = 1;
				continue;
			case 1:
				if (((lI1 < 3))) {
					_G = 2;
					continue;
				}
				if ((((this._bl_tree.data[(((((Math.imul((com_jtransc_compression_jzlib_Tree._bl_order.data[lI1]), 2))|0) + 1))|0)]) == 0))) {
					_G = 3;
					continue;
				}
				_G = 2;
				continue;
			case 3:
				lI1 = (((lI1 + -1))|0);
				_G = 1;
				continue;
			case 2:
				this._opt_len = (((this._opt_len + (((((((((((Math.imul(3, (((lI1 + 1))|0)))|0) + 5))|0) + 5))|0) + 4))|0)))|0);
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_compression_jzlib_Deflate.prototype["scan_tree([SI)V"] = function(p0, p1) { 
	var _G = 0, lI4 = 0, lI6 = 0, lI7 = 0, lI8 = 0, lI9 = 0, lI3 = 0, lI5 = 0, tA1 = null, tA3 = null, tA5 = null, tA7 = null, tA9 = null, tI0 = 0, tI2 = 0, tI4 = 0, tI6 = 0, tI8 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI4 = -1;
				lI6 = (((p0.data[1]))|0);
				lI7 = 0;
				lI8 = 7;
				lI9 = 4;
				if (((lI6 != 0))) {
					_G = 1;
					continue;
				}
				lI8 = 138;
				lI9 = 3;
				_G = 1;
				continue;
			case 1:
				p0.data[(((((Math.imul((((p1 + 1))|0), 2))|0) + 1))|0)] = -1;
				lI3 = 0;
				_G = 2;
				continue;
			case 2:
				if (((lI3 > p1))) {
					_G = 3;
					continue;
				}
				lI5 = lI6;
				lI6 = (((p0.data[(((((Math.imul((((lI3 + 1))|0), 2))|0) + 1))|0)]))|0);
				lI7 = (((lI7 + 1))|0);
				if (((lI7 >= lI8))) {
					_G = 4;
					continue;
				}
				if (((lI5 != lI6))) {
					_G = 4;
					continue;
				}
				_G = 5;
				continue;
			case 4:
				if (((lI7 >= lI9))) {
					_G = 6;
					continue;
				}
				tA1 = this._bl_tree;
				tI0 = ((Math.imul(lI5, 2))|0);
				tA1.data[tI0] = ((((((tA1.data[tI0]) + lI7))|0))<<16>>16);
				_G = 7;
				continue;
			case 6:
				if (((lI5 == 0))) {
					_G = 8;
					continue;
				}
				if (((lI5 == lI4))) {
					_G = 9;
					continue;
				}
				tA3 = this._bl_tree;
				tI2 = ((Math.imul(lI5, 2))|0);
				tA3.data[tI2] = ((((((tA3.data[tI2]) + 1))|0))<<16>>16);
				_G = 9;
				continue;
			case 9:
				tA5 = this._bl_tree;
				tI4 = 32;
				tA5.data[tI4] = ((((((tA5.data[tI4]) + 1))|0))<<16>>16);
				_G = 7;
				continue;
			case 8:
				if (((lI7 > 10))) {
					_G = 10;
					continue;
				}
				tA7 = this._bl_tree;
				tI6 = 34;
				tA7.data[tI6] = ((((((tA7.data[tI6]) + 1))|0))<<16>>16);
				_G = 7;
				continue;
			case 10:
				tA9 = this._bl_tree;
				tI8 = 36;
				tA9.data[tI8] = ((((((tA9.data[tI8]) + 1))|0))<<16>>16);
				_G = 7;
				continue;
			case 7:
				lI7 = 0;
				lI4 = lI5;
				if (((lI6 != 0))) {
					_G = 11;
					continue;
				}
				lI8 = 138;
				lI9 = 3;
				_G = 5;
				continue;
			case 11:
				if (((lI5 != lI6))) {
					_G = 12;
					continue;
				}
				lI8 = 6;
				lI9 = 3;
				_G = 5;
				continue;
			case 12:
				lI8 = 7;
				lI9 = 4;
				_G = 5;
				continue;
			case 5:
				lI3 = (((lI3 + 1))|0);
				_G = 2;
				continue;
			case 3:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["compress_block([S[S)V"] = function(p0, p1) { 
	var _G = 0, lI5 = 0, lI3 = 0, lI4 = 0, lI6 = 0, lI7 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI5 = 0;
				if (((this._last_lit == 0))) {
					_G = 1;
					continue;
				}
				_G = 2;
				continue;
			case 2:
				lI3 = ((((((((((this._pending_buf.data[(((this._d_buf + ((Math.imul(lI5, 2))|0)))|0)]) << 8))|0) & 65280))|0) | ((((this._pending_buf.data[((((((this._d_buf + ((Math.imul(lI5, 2))|0)))|0) + 1))|0)]) & 255))|0)))|0);
				lI4 = ((((this._l_buf.data[lI5]) & 255))|0);
				lI5 = (((lI5 + 1))|0);
				if (((lI3 != 0))) {
					_G = 3;
					continue;
				}
				this["send_code(I[S)V"](lI4, p0);
				_G = 4;
				continue;
			case 3:
				lI6 = (((com_jtransc_compression_jzlib_Tree.__length_code.data[lI4]))|0);
				this["send_code(I[S)V"](((((((lI6 + 256))|0) + 1))|0), p0);
				lI7 = (com_jtransc_compression_jzlib_Tree._extra_lbits.data[lI6]);
				if (((lI7 == 0))) {
					_G = 5;
					continue;
				}
				lI4 = (((lI4 - (com_jtransc_compression_jzlib_Tree._base_length.data[lI6])))|0);
				this["send_bits(II)V"](lI4, lI7);
				_G = 5;
				continue;
			case 5:
				lI3 = (((lI3 + -1))|0);
				lI6 = com_jtransc_compression_jzlib_Tree["d_code(I)I"](lI3);
				this["send_code(I[S)V"](lI6, p1);
				lI7 = (com_jtransc_compression_jzlib_Tree._extra_dbits.data[lI6]);
				if (((lI7 == 0))) {
					_G = 4;
					continue;
				}
				lI3 = (((lI3 - (com_jtransc_compression_jzlib_Tree._base_dist.data[lI6])))|0);
				this["send_bits(II)V"](lI3, lI7);
				_G = 4;
				continue;
			case 4:
				if (((lI5 < this._last_lit))) {
					_G = 2;
					continue;
				}
				_G = 1;
				continue;
			case 1:
				this["send_code(I[S)V"](256, p0);
				this._last_eob_len = (((p0.data[513]))|0);
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["_tr_stored_block(IIZ)V"] = function(p0, p1, p2) { 
	var _G = 0, fI1 = 0, fI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				fI1 = 0;
				if (!(p2)) {
					_G = 1;
					continue;
				}
				fI2 = 1;
				_G = 2;
				continue;
			case 1:
				fI2 = 0;
				_G = 2;
				continue;
			case 2:
				this["send_bits(II)V"]((((fI1 + fI2))|0), 3);
				this["copy_block(IIZ)V"](p0, p1, true);
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["copy_block(IIZ)V"] = function(p0, p1, p2) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				this["bi_windup()V"]();
				this._last_eob_len = 8;
				if (!(p2)) {
					_G = 1;
					continue;
				}
				this["put_short(I)V"](((((p1)<<16>>16))|0));
				this["put_short(I)V"]((((((((p1 ^ -1))|0))<<16>>16))|0));
				_G = 1;
				continue;
			case 1:
				this["put_byte([BII)V"](this._window, p0, p1);
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["longest_match(I)I"] = function(p0) { 
	var fA0 = null, lI12 = 0, lI2 = 0, lI6 = 0, lI8 = 0, lI10 = 0, lI4 = 0, fI0 = 0, tI0 = 0, _G = 0, fA1 = null, lI11 = 0, lI1 = 0, lI3 = 0, lI7 = 0, lI9 = 0, lI5 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = p0;
				lI2 = this._max_chain_length;
				lI3 = this._strstart;
				lI6 = this._prev_length;
				if (((this._strstart <= (((this._w_size - 262))|0)))) {
					_G = 1;
					continue;
				}
				fI0 = (((this._strstart - (((this._w_size - 262))|0)))|0);
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:
				lI7 = fI0;
				lI8 = this._nice_match;
				lI9 = this._w_mask;
				lI10 = (((this._strstart + 258))|0);
				lI11 = (((this._window.data[((((((lI3 + lI6))|0) - 1))|0)]))|0);
				lI12 = (((this._window.data[(((lI3 + lI6))|0)]))|0);
				if (((this._prev_length < this._good_match))) {
					_G = 3;
					continue;
				}
				lI2 = (((lI2 >> 2))|0);
				_G = 3;
				continue;
			case 3:
				if (((lI8 <= this._lookahead))) {
					_G = 4;
					continue;
				}
				lI8 = this._lookahead;
				_G = 4;
				continue;
			case 4:
				lI4 = lI1;
				if ((((this._window.data[(((lI4 + lI6))|0)]) != lI12))) {
					_G = 5;
					continue;
				}
				if ((((this._window.data[((((((lI4 + lI6))|0) - 1))|0)]) != lI11))) {
					_G = 5;
					continue;
				}
				if ((((this._window.data[lI4]) != (this._window.data[lI3])))) {
					_G = 5;
					continue;
				}
				fA0 = this._window;
				lI4 = (((lI4 + 1))|0);
				if ((((fA0.data[lI4]) == (this._window.data[(((lI3 + 1))|0)])))) {
					_G = 6;
					continue;
				}
				_G = 5;
				continue;
			case 6:
				lI3 = (((lI3 + 2))|0);
				lI4 = (((lI4 + 1))|0);
				_G = 7;
				continue;
			case 7:
				fA0 = this._window;
				lI3 = (((lI3 + 1))|0);
				fI0 = (((fA0.data[lI3]))|0);
				fA1 = this._window;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1.data[lI4])))) {
					_G = 8;
					continue;
				}
				fA0 = this._window;
				lI3 = (((lI3 + 1))|0);
				fI0 = (((fA0.data[lI3]))|0);
				fA1 = this._window;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1.data[lI4])))) {
					_G = 8;
					continue;
				}
				fA0 = this._window;
				lI3 = (((lI3 + 1))|0);
				fI0 = (((fA0.data[lI3]))|0);
				fA1 = this._window;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1.data[lI4])))) {
					_G = 8;
					continue;
				}
				fA0 = this._window;
				lI3 = (((lI3 + 1))|0);
				fI0 = (((fA0.data[lI3]))|0);
				fA1 = this._window;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1.data[lI4])))) {
					_G = 8;
					continue;
				}
				fA0 = this._window;
				lI3 = (((lI3 + 1))|0);
				fI0 = (((fA0.data[lI3]))|0);
				fA1 = this._window;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1.data[lI4])))) {
					_G = 8;
					continue;
				}
				fA0 = this._window;
				lI3 = (((lI3 + 1))|0);
				fI0 = (((fA0.data[lI3]))|0);
				fA1 = this._window;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1.data[lI4])))) {
					_G = 8;
					continue;
				}
				fA0 = this._window;
				lI3 = (((lI3 + 1))|0);
				fI0 = (((fA0.data[lI3]))|0);
				fA1 = this._window;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1.data[lI4])))) {
					_G = 8;
					continue;
				}
				fA0 = this._window;
				lI3 = (((lI3 + 1))|0);
				fI0 = (((fA0.data[lI3]))|0);
				fA1 = this._window;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1.data[lI4])))) {
					_G = 8;
					continue;
				}
				if (((lI3 < lI10))) {
					_G = 7;
					continue;
				}
				_G = 8;
				continue;
			case 8:
				lI5 = (((258 - (((lI10 - lI3))|0)))|0);
				lI3 = (((lI10 - 258))|0);
				if (((lI5 <= lI6))) {
					_G = 5;
					continue;
				}
				this._match_start = lI1;
				lI6 = lI5;
				if (((lI5 < lI8))) {
					_G = 9;
					continue;
				}
				_G = 10;
				continue;
			case 9:
				lI11 = (((this._window.data[((((((lI3 + lI6))|0) - 1))|0)]))|0);
				lI12 = (((this._window.data[(((lI3 + lI6))|0)]))|0);
				_G = 5;
				continue;
			case 5:
				tI0 = ((((this._prev.data[(((lI1 & lI9))|0)]) & 65535))|0);
				fI0 = tI0;
				lI1 = tI0;
				if (((fI0 <= lI7))) {
					_G = 10;
					continue;
				}
				lI2 = (((lI2 + -1))|0);
				if (((lI2 != 0))) {
					_G = 4;
					continue;
				}
				_G = 10;
				continue;
			case 10:
				if (((lI6 > this._lookahead))) {
					_G = 11;
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
};
com_jtransc_compression_jzlib_Deflate.prototype["fill_window()V"] = function() { 
	var _G = 0, fA0 = null, fI1 = 0, fI2 = 0, lI4 = 0, lI1 = 0, lI3 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				_G = 1;
				continue;
			case 1:
				lI4 = ((((((this._window_size - this._lookahead))|0) - this._strstart))|0);
				if (((lI4 != 0))) {
					_G = 2;
					continue;
				}
				if (((this._strstart != 0))) {
					_G = 2;
					continue;
				}
				if (((this._lookahead != 0))) {
					_G = 2;
					continue;
				}
				lI4 = this._w_size;
				_G = 3;
				continue;
			case 2:
				if (((lI4 != -1))) {
					_G = 4;
					continue;
				}
				lI4 = (((lI4 + -1))|0);
				_G = 3;
				continue;
			case 4:
				if (((this._strstart < ((((((this._w_size + this._w_size))|0) - 262))|0)))) {
					_G = 3;
					continue;
				}
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((this._window), this._w_size, (this._window), 0, this._w_size);
				this._match_start = (((this._match_start - this._w_size))|0);
				this._strstart = (((this._strstart - this._w_size))|0);
				this._block_start = (((this._block_start - this._w_size))|0);
				lI1 = this._hash_size;
				lI3 = lI1;
				_G = 5;
				continue;
			case 5:
				fA0 = this._head;
				lI3 = (((lI3 + -1))|0);
				lI2 = ((((fA0.data[lI3]) & 65535))|0);
				fA0 = this._head;
				fI1 = lI3;
				if (((lI2 < this._w_size))) {
					_G = 6;
					continue;
				}
				fI2 = (((((((lI2 - this._w_size))|0))<<16>>16))|0);
				_G = 7;
				continue;
			case 6:
				fI2 = 0;
				_G = 7;
				continue;
			case 7:
				fA0.data[fI1] = ((fI2)<<16>>16);
				lI1 = (((lI1 + -1))|0);
				if (((lI1 != 0))) {
					_G = 5;
					continue;
				}
				lI1 = this._w_size;
				lI3 = lI1;
				_G = 8;
				continue;
			case 8:
				fA0 = this._prev;
				lI3 = (((lI3 + -1))|0);
				lI2 = ((((fA0.data[lI3]) & 65535))|0);
				fA0 = this._prev;
				fI1 = lI3;
				if (((lI2 < this._w_size))) {
					_G = 9;
					continue;
				}
				fI2 = (((((((lI2 - this._w_size))|0))<<16>>16))|0);
				_G = 10;
				continue;
			case 9:
				fI2 = 0;
				_G = 10;
				continue;
			case 10:
				fA0.data[fI1] = ((fI2)<<16>>16);
				lI1 = (((lI1 + -1))|0);
				if (((lI1 != 0))) {
					_G = 8;
					continue;
				}
				lI4 = (((lI4 + this._w_size))|0);
				_G = 3;
				continue;
			case 3:
				if (((this._strm._avail_in != 0))) {
					_G = 11;
					continue;
				}
				return;
				_G = 11;
				continue;
			case 11:
				lI1 = this._strm["read_buf([BII)I"](this._window, (((this._strstart + this._lookahead))|0), lI4);
				this._lookahead = (((this._lookahead + lI1))|0);
				if (((this._lookahead < 3))) {
					_G = 12;
					continue;
				}
				this._ins_h = ((((this._window.data[this._strstart]) & 255))|0);
				this._ins_h = (((((((((this._ins_h << this._hash_shift))|0) ^ ((((this._window.data[(((this._strstart + 1))|0)]) & 255))|0)))|0) & this._hash_mask))|0);
				_G = 12;
				continue;
			case 12:
				if (((this._lookahead >= 262))) {
					_G = 13;
					continue;
				}
				if (((this._strm._avail_in != 0))) {
					_G = 1;
					continue;
				}
				_G = 13;
				continue;
			case 13:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["_tr_tally(II)Z"] = function(p0, p1) { 
	var _G = 0, fI0 = 0, lI1 = 0, lI3 = 0, lI4 = 0, lI5 = 0, tA2 = null, tA5 = null, tA7 = null, tI1 = 0, tI4 = 0, tI6 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = p0;
				this._pending_buf.data[(((this._d_buf + ((Math.imul(this._last_lit, 2))|0)))|0)] = (((((lI1 >>> 8))|0))<<24>>24);
				this._pending_buf.data[((((((this._d_buf + ((Math.imul(this._last_lit, 2))|0)))|0) + 1))|0)] = ((lI1)<<24>>24);
				this._l_buf.data[this._last_lit] = ((p1)<<24>>24);
				this._last_lit = (((this._last_lit + 1))|0);
				if (((lI1 != 0))) {
					_G = 1;
					continue;
				}
				tA2 = this._dyn_ltree;
				tI1 = ((Math.imul(p1, 2))|0);
				tA2.data[tI1] = ((((((tA2.data[tI1]) + 1))|0))<<16>>16);
				_G = 2;
				continue;
			case 1:
				this._matches = (((this._matches + 1))|0);
				lI1 = (((lI1 + -1))|0);
				tA5 = this._dyn_ltree;
				tI4 = ((Math.imul((((((((com_jtransc_compression_jzlib_Tree.__length_code.data[p1]) + 256))|0) + 1))|0), 2))|0);
				tA5.data[tI4] = ((((((tA5.data[tI4]) + 1))|0))<<16>>16);
				tA7 = this._dyn_dtree;
				tI6 = ((Math.imul(com_jtransc_compression_jzlib_Tree["d_code(I)I"](lI1), 2))|0);
				tA7.data[tI6] = ((((((tA7.data[tI6]) + 1))|0))<<16>>16);
				_G = 2;
				continue;
			case 2:
				if ((((((this._last_lit & 8191))|0) != 0))) {
					_G = 3;
					continue;
				}
				if (((this._level <= 2))) {
					_G = 3;
					continue;
				}
				lI3 = ((Math.imul(this._last_lit, 8))|0);
				lI4 = (((this._strstart - this._block_start))|0);
				lI5 = 0;
				_G = 4;
				continue;
			case 4:
				if (((lI5 >= 30))) {
					_G = 5;
					continue;
				}
				lI3 = N.j2i(N.ladd(N.i2j(lI3), N.lmul(N.i2j((this._dyn_dtree.data[((Math.imul(lI5, 2))|0)])), N.ladd(N.lnew(0, 5), N.i2j((com_jtransc_compression_jzlib_Tree._extra_dbits.data[lI5]))))));
				lI5 = (((lI5 + 1))|0);
				_G = 4;
				continue;
			case 5:
				lI3 = (((lI3 >>> 3))|0);
				if (((this._matches >= (((this._last_lit / 2))|0)))) {
					_G = 3;
					continue;
				}
				if (((lI3 >= (((lI4 / 2))|0)))) {
					_G = 3;
					continue;
				}
				return true;
			case 3:
				if (((this._last_lit != (((this._lit_bufsize - 1))|0)))) {
					_G = 6;
					continue;
				}
				fI0 = 1;
				_G = 7;
				continue;
			case 6:
				fI0 = 0;
				_G = 7;
				continue;
			case 7:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
com_jtransc_compression_jzlib_Deflate.prototype["deflate_fast(I)I"] = function(p0) { 
	var _G = 0, fI0 = 0, fI1 = 0, lI2 = 0, lI3 = 0, fA0 = null, tA1 = null, tI4 = 0, tA7 = null, tA9 = null, tA3 = null, tA5 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((this._lookahead >= 262))) {
					_G = 2;
					continue;
				}
				this["fill_window()V"]();
				if (((this._lookahead >= 262))) {
					_G = 3;
					continue;
				}
				if (((p0 != 0))) {
					_G = 3;
					continue;
				}
				return 0;
			case 3:
				if (((this._lookahead != 0))) {
					_G = 2;
					continue;
				}
				_G = 4;
				continue;
			case 2:
				if (((this._lookahead < 3))) {
					_G = 5;
					continue;
				}
				this._ins_h = (((((((((this._ins_h << this._hash_shift))|0) ^ ((((this._window.data[(((this._strstart + 2))|0)]) & 255))|0)))|0) & this._hash_mask))|0);
				lI2 = ((((this._head.data[this._ins_h]) & 65535))|0);
				this._prev.data[(((this._strstart & this._w_mask))|0)] = (this._head.data[this._ins_h]);
				this._head.data[this._ins_h] = ((this._strstart)<<16>>16);
				_G = 5;
				continue;
			case 5:
				if (((((N.lcmp(N.i2j(lI2), N.lnew(0, 0)))|0) == 0))) {
					_G = 6;
					continue;
				}
				if (((((((((this._strstart - lI2))|0) & 65535))|0) > (((this._w_size - 262))|0)))) {
					_G = 6;
					continue;
				}
				if (((this._strategy == 2))) {
					_G = 6;
					continue;
				}
				this._match_length = this["longest_match(I)I"](lI2);
				_G = 6;
				continue;
			case 6:
				if (((this._match_length < 3))) {
					_G = 7;
					continue;
				}
				lI3 = ((N.z2i(this["_tr_tally(II)Z"]((((this._strstart - this._match_start))|0), (((this._match_length - 3))|0))))|0);
				this._lookahead = (((this._lookahead - this._match_length))|0);
				if (((this._match_length > this._max_lazy_match))) {
					_G = 8;
					continue;
				}
				if (((this._lookahead < 3))) {
					_G = 8;
					continue;
				}
				tA1 = this;
				tA1._match_length = (((tA1._match_length - 1))|0);
				_G = 9;
				continue;
			case 9:
				this._strstart = (((this._strstart + 1))|0);
				this._ins_h = (((((((((this._ins_h << this._hash_shift))|0) ^ ((((this._window.data[(((this._strstart + 2))|0)]) & 255))|0)))|0) & this._hash_mask))|0);
				lI2 = ((((this._head.data[this._ins_h]) & 65535))|0);
				this._prev.data[(((this._strstart & this._w_mask))|0)] = (this._head.data[this._ins_h]);
				this._head.data[this._ins_h] = ((this._strstart)<<16>>16);
				tA3 = (this);
				fA0 = tA3;
				tA5 = fA0;
				tI4 = ((((tA3)._match_length - 1))|0);
				fI0 = tI4;
				(tA5)._match_length = tI4;
				if (((fI0 != 0))) {
					_G = 9;
					continue;
				}
				this._strstart = (((this._strstart + 1))|0);
				_G = 10;
				continue;
			case 8:
				tA7 = this;
				tA7._strstart = (((tA7._strstart + this._match_length))|0);
				this._match_length = 0;
				this._ins_h = ((((this._window.data[this._strstart]) & 255))|0);
				this._ins_h = (((((((((this._ins_h << this._hash_shift))|0) ^ ((((this._window.data[(((this._strstart + 1))|0)]) & 255))|0)))|0) & this._hash_mask))|0);
				_G = 10;
				continue;
			case 7:
				lI3 = ((N.z2i(this["_tr_tally(II)Z"](0, ((((this._window.data[this._strstart]) & 255))|0))))|0);
				this._lookahead = (((this._lookahead - 1))|0);
				tA9 = this;
				tA9._strstart = (((tA9._strstart + 1))|0);
				_G = 10;
				continue;
			case 10:
				if (((lI3 == 0))) {
					_G = 1;
					continue;
				}
				this["flush_block_only(Z)V"](false);
				if (((this._strm._avail_out != 0))) {
					_G = 1;
					continue;
				}
				return 0;
			case 4:
				fA0 = (this);
				if (((p0 != 4))) {
					_G = 11;
					continue;
				}
				fI1 = 1;
				_G = 12;
				continue;
			case 11:
				fI1 = 0;
				_G = 12;
				continue;
			case 12:
				(fA0)["flush_block_only(Z)V"](((fI1)!=0));
				if (((this._strm._avail_out != 0))) {
					_G = 13;
					continue;
				}
				if (((p0 != 4))) {
					_G = 14;
					continue;
				}
				return 2;
			case 14:
				return 0;
			case 13:
				if (((p0 != 4))) {
					_G = 15;
					continue;
				}
				fI0 = 3;
				_G = 16;
				continue;
			case 15:
				fI0 = 1;
				_G = 16;
				continue;
			case 16:return fI0; 
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_compression_jzlib_Deflate.prototype["_tr_align()V"] = function() { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				this["send_bits(II)V"](2, 3);
				this["send_code(I[S)V"](256, com_jtransc_compression_jzlib_StaticTree._static_ltree);
				this["bi_flush()V"]();
				if ((((((((((((1 + this._last_eob_len))|0) + 10))|0) - this._bi_valid))|0) >= 9))) {
					_G = 1;
					continue;
				}
				this["send_bits(II)V"](2, 3);
				this["send_code(I[S)V"](256, com_jtransc_compression_jzlib_StaticTree._static_ltree);
				this["bi_flush()V"]();
				_G = 1;
				continue;
			case 1:
				this._last_eob_len = 7;
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["bi_flush()V"] = function() { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._bi_valid != 16))) {
					_G = 1;
					continue;
				}
				this["put_short(I)V"](((this._bi_buf)|0));
				this._bi_buf = 0;
				this._bi_valid = 0;
				_G = 2;
				continue;
			case 1:
				if (((this._bi_valid < 8))) {
					_G = 2;
					continue;
				}
				this["put_byte(B)V"](((this._bi_buf)<<24>>24));
				this._bi_buf = (((((this._bi_buf >>> 8))|0))<<16>>16);
				this._bi_valid = (((this._bi_valid - 8))|0);
				_G = 2;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["deflate_stored(I)I"] = function(p0) { 
	var _G = 0, fI1 = 0, fI0 = 0, lI2 = 0, lI3 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI2 = 65535;
				if (((lI2 <= (((this._pending_buf_size - 5))|0)))) {
					_G = 1;
					continue;
				}
				lI2 = (((this._pending_buf_size - 5))|0);
				_G = 1;
				continue;
			case 1:
				if (((this._lookahead > 1))) {
					_G = 2;
					continue;
				}
				this["fill_window()V"]();
				if (((this._lookahead != 0))) {
					_G = 3;
					continue;
				}
				if (((p0 != 0))) {
					_G = 3;
					continue;
				}
				return 0;
			case 3:
				if (((this._lookahead != 0))) {
					_G = 2;
					continue;
				}
				_G = 4;
				continue;
			case 2:
				this._strstart = (((this._strstart + this._lookahead))|0);
				this._lookahead = 0;
				lI3 = (((this._block_start + lI2))|0);
				if (((this._strstart == 0))) {
					_G = 5;
					continue;
				}
				if (((this._strstart < lI3))) {
					_G = 6;
					continue;
				}
				_G = 5;
				continue;
			case 5:
				this._lookahead = (((this._strstart - lI3))|0);
				this._strstart = lI3;
				this["flush_block_only(Z)V"](false);
				if (((this._strm._avail_out != 0))) {
					_G = 6;
					continue;
				}
				return 0;
			case 6:
				if ((((((this._strstart - this._block_start))|0) < (((this._w_size - 262))|0)))) {
					_G = 1;
					continue;
				}
				this["flush_block_only(Z)V"](false);
				if (((this._strm._avail_out != 0))) {
					_G = 1;
					continue;
				}
				return 0;
			case 4:
				if (((p0 != 4))) {
					_G = 7;
					continue;
				}
				fI1 = 1;
				_G = 8;
				continue;
			case 7:
				fI1 = 0;
				_G = 8;
				continue;
			case 8:
				this["flush_block_only(Z)V"](((fI1)!=0));
				if (((this._strm._avail_out != 0))) {
					_G = 9;
					continue;
				}
				if (((p0 != 4))) {
					_G = 10;
					continue;
				}
				fI0 = 2;
				_G = 11;
				continue;
			case 10:
				fI0 = 0;
				_G = 11;
				continue;
			case 11:return fI0; 
			case 9:
				if (((p0 != 4))) {
					_G = 12;
					continue;
				}
				fI0 = 3;
				_G = 13;
				continue;
			case 12:
				fI0 = 1;
				_G = 13;
				continue;
			case 13:return fI0; 
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_compression_jzlib_Deflate.prototype["getGZIPHeader()Lcom/jtransc/compression/jzlib/GZIPHeader;"] = function() { 
	var _G = 0, fA1 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._gheader != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new com_jtransc_compression_jzlib_GZIPHeader()));
				fA1 = tA0;
				(tA0)["com.jtransc.compression.jzlib.GZIPHeader<init>()V"]();
				this._gheader = (fA1);
				_G = 1;
				continue;
			case 1:
				return this._gheader;
			default:
				break;
		}
	}
	return null;
};
com_jtransc_compression_jzlib_Deflate.prototype["putShortMSB(I)V"] = function(p0) { 
	this["put_byte(B)V"]((((((p0 >> 8))|0))<<24>>24));
	this["put_byte(B)V"](((p0)<<24>>24));
	return;
};
com_jtransc_compression_jzlib_Deflate.prototype["deflateInit(II)I"] = function(p0, p1) { 
	return this["deflateInit(IIIII)I"](p0, 8, p1, 8, 0);
};
com_jtransc_compression_jzlib_Deflate.prototype["deflateInit(IIIII)I"] = function(p0, p1, p2, p3, p4) { 
	var _G = 0, fA0 = null, lI1 = 0, lI3 = 0, lI6 = 0, fA1 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = p0;
				lI3 = p2;
				lI6 = 1;
				this._strm._msg = null;
				if (((lI1 != -1))) {
					_G = 1;
					continue;
				}
				lI1 = 6;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 0))) {
					_G = 2;
					continue;
				}
				lI6 = 0;
				lI3 = ((-(lI3))|0);
				_G = 3;
				continue;
			case 2:
				if (((lI3 <= 15))) {
					_G = 3;
					continue;
				}
				lI6 = 2;
				lI3 = (((lI3 + -16))|0);
				fA0 = this._strm;
				tA0 = ((new com_jtransc_compression_jzlib_CRC32()));
				fA1 = tA0;
				(tA0)["com.jtransc.compression.jzlib.CRC32<init>()V"]();
				fA0._adler = (fA1);
				_G = 3;
				continue;
			case 3:
				if (((p3 < 1))) {
					_G = 4;
					continue;
				}
				if (((p3 > 9))) {
					_G = 4;
					continue;
				}
				if (((p1 != 8))) {
					_G = 4;
					continue;
				}
				if (((lI3 < 9))) {
					_G = 4;
					continue;
				}
				if (((lI3 > 15))) {
					_G = 4;
					continue;
				}
				if (((lI1 < 0))) {
					_G = 4;
					continue;
				}
				if (((lI1 > 9))) {
					_G = 4;
					continue;
				}
				if (((p4 < 0))) {
					_G = 4;
					continue;
				}
				if (((p4 <= 2))) {
					_G = 5;
					continue;
				}
				_G = 4;
				continue;
			case 4:
				return -2;
			case 5:
				this._strm._dstate = this;
				this._wrap = lI6;
				this._w_bits = lI3;
				this._w_size = (((1 << this._w_bits))|0);
				this._w_mask = (((this._w_size - 1))|0);
				this._hash_bits = (((p3 + 7))|0);
				this._hash_size = (((1 << this._hash_bits))|0);
				this._hash_mask = (((this._hash_size - 1))|0);
				this._hash_shift = (((((((((this._hash_bits + 3))|0) - 1))|0) / 3))|0);
				this._window = new JA_B(((Math.imul(this._w_size, 2))|0));
				this._prev = new JA_S(this._w_size);
				this._head = new JA_S(this._hash_size);
				this._lit_bufsize = (((1 << (((p3 + 6))|0)))|0);
				this._pending_buf = new JA_B(((Math.imul(this._lit_bufsize, 3))|0));
				this._pending_buf_size = ((Math.imul(this._lit_bufsize, 3))|0);
				this._d_buf = this._lit_bufsize;
				this._l_buf = new JA_B(this._lit_bufsize);
				this._level = lI1;
				this._strategy = p4;
				this._method = ((p1)<<24>>24);
				return this["deflateReset()I"]();
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_compression_jzlib_Deflate.prototype["deflateReset()I"] = function() { 
	var _G = 0, fJ1 = N.lnew(0, 0), fI1 = 0, tJ0 = N.lnew(0, 0), fA0 = null, fA1 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				fA0 = (this._strm);
				fA1 = (this._strm);
				tA1 = fA1;
				tJ0 = N.lnew(0, 0);
				fJ1 = tJ0;
				(tA1)._total_out = tJ0;
				(fA0)._total_in = fJ1;
				this._strm._msg = null;
				this._strm._data_type = 2;
				this._pending = 0;
				this._pending_out = 0;
				if (((this._wrap >= 0))) {
					_G = 1;
					continue;
				}
				this._wrap = ((-(this._wrap))|0);
				_G = 1;
				continue;
			case 1:
				fA0 = (this);
				if (((this._wrap != 0))) {
					_G = 2;
					continue;
				}
				fI1 = 113;
				_G = 3;
				continue;
			case 2:
				fI1 = 42;
				_G = 3;
				continue;
			case 3:
				(fA0)._status = fI1;
				this._strm._adler["reset()V"]();
				this._last_flush = 0;
				this["tr_init()V"]();
				this["lm_init()V"]();
				return 0;
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_compression_jzlib_Deflate.prototype["lm_init()V"] = function() { 
	var _G = 0, fI1 = 0, lI1 = 0, tI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				this._window_size = ((Math.imul(2, this._w_size))|0);
				this._head.data[(((this._hash_size - 1))|0)] = 0;
				lI1 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= (((this._hash_size - 1))|0)))) {
					_G = 2;
					continue;
				}
				this._head.data[lI1] = 0;
				lI1 = (((lI1 + 1))|0);
				_G = 1;
				continue;
			case 2:
				this._max_lazy_match = (((com_jtransc_compression_jzlib_Deflate._config_table).data[this._level]))._max_lazy;
				this._good_match = (((com_jtransc_compression_jzlib_Deflate._config_table).data[this._level]))._good_length;
				this._nice_match = (((com_jtransc_compression_jzlib_Deflate._config_table).data[this._level]))._nice_length;
				this._max_chain_length = (((com_jtransc_compression_jzlib_Deflate._config_table).data[this._level]))._max_chain;
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
};
com_jtransc_compression_jzlib_Deflate.prototype["tr_init()V"] = function() { 
	this._l_desc._dyn_tree = this._dyn_ltree;
	this._l_desc._stat_desc = com_jtransc_compression_jzlib_StaticTree._static_l_desc;
	this._d_desc._dyn_tree = this._dyn_dtree;
	this._d_desc._stat_desc = com_jtransc_compression_jzlib_StaticTree._static_d_desc;
	this._bl_desc._dyn_tree = this._bl_tree;
	this._bl_desc._stat_desc = com_jtransc_compression_jzlib_StaticTree._static_bl_desc;
	this._bi_buf = 0;
	this._bi_valid = 0;
	this._last_eob_len = 8;
	this["init_block()V"]();
	return;
};
function com_jtransc_compression_jzlib_ZStream() {
}
com_jtransc_compression_jzlib_ZStream.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_compression_jzlib_ZStream.prototype.constructor = com_jtransc_compression_jzlib_ZStream;
com_jtransc_compression_jzlib_ZStream.prototype._dstate = null;
com_jtransc_compression_jzlib_ZStream.prototype._avail_out = 0;
com_jtransc_compression_jzlib_ZStream.prototype._next_out = null;
com_jtransc_compression_jzlib_ZStream.prototype._next_in = null;
com_jtransc_compression_jzlib_ZStream.prototype._avail_in = 0;
com_jtransc_compression_jzlib_ZStream.prototype._msg = null;
com_jtransc_compression_jzlib_ZStream.prototype._total_in = N.lnew(0, 0);
com_jtransc_compression_jzlib_ZStream.prototype._adler = null;
com_jtransc_compression_jzlib_ZStream.prototype._next_out_index = 0;
com_jtransc_compression_jzlib_ZStream.prototype._total_out = N.lnew(0, 0);
com_jtransc_compression_jzlib_ZStream.prototype._next_in_index = 0;
com_jtransc_compression_jzlib_ZStream.prototype._data_type = 0;
com_jtransc_compression_jzlib_ZStream.prototype.___id = 0;
com_jtransc_compression_jzlib_ZStream.SI = function(){};
com_jtransc_compression_jzlib_ZStream.prototype.__JT__CLASS_ID = com_jtransc_compression_jzlib_ZStream.__JT__CLASS_ID = 895;
com_jtransc_compression_jzlib_ZStream.prototype.__JT__CLASS_IDS = com_jtransc_compression_jzlib_ZStream.__JT__CLASS_IDS = [895,656];
com_jtransc_compression_jzlib_ZStream.prototype["com.jtransc.compression.jzlib.ZStream<init>()V"] = function() { 
	var fA1 = null, tA0 = null;
	tA0 = ((new com_jtransc_compression_jzlib_Adler32()));
	fA1 = tA0;
	(tA0)["com.jtransc.compression.jzlib.Adler32<init>()V"]();
	this["com.jtransc.compression.jzlib.ZStream<init>(Lcom/jtransc/compression/jzlib/Checksum;)V"]((fA1));
	return this;
	return this;
};
com_jtransc_compression_jzlib_ZStream.prototype["com.jtransc.compression.jzlib.ZStream<init>(Lcom/jtransc/compression/jzlib/Checksum;)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	this._adler = p0;
	return this;
	return this;
};
com_jtransc_compression_jzlib_ZStream.prototype["flush_pending()V"] = function() { 
	var _G = 0, lI1 = 0, tA1 = null, tA4 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = this._dstate._pending;
				if (((lI1 <= this._avail_out))) {
					_G = 1;
					continue;
				}
				lI1 = this._avail_out;
				_G = 1;
				continue;
			case 1:
				if (((lI1 != 0))) {
					_G = 2;
					continue;
				}
				return;
				_G = 2;
				continue;
			case 2:
				if (((this._dstate._pending_buf.length <= this._dstate._pending_out))) {
					_G = 3;
					continue;
				}
				if (((this._next_out.length <= this._next_out_index))) {
					_G = 3;
					continue;
				}
				if (((this._dstate._pending_buf.length < (((this._dstate._pending_out + lI1))|0)))) {
					_G = 3;
					continue;
				}
				if (((this._next_out.length >= (((this._next_out_index + lI1))|0)))) {
					_G = 3;
					continue;
				}
				_G = 3;
				continue;
			case 3:
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((this._dstate._pending_buf), this._dstate._pending_out, (this._next_out), this._next_out_index, lI1);
				this._next_out_index = (((this._next_out_index + lI1))|0);
				tA1 = this._dstate;
				tA1._pending_out = (((tA1._pending_out + lI1))|0);
				this._total_out = N.ladd(this._total_out, N.i2j(lI1));
				this._avail_out = (((this._avail_out - lI1))|0);
				tA4 = this._dstate;
				tA4._pending = (((tA4._pending - lI1))|0);
				if (((this._dstate._pending != 0))) {
					_G = 4;
					continue;
				}
				this._dstate._pending_out = 0;
				_G = 4;
				continue;
			case 4:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_ZStream.prototype["read_buf([BII)I"] = function(p0, p1, p2) { 
	var _G = 0, lI4 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI4 = this._avail_in;
				if (((lI4 <= p2))) {
					_G = 1;
					continue;
				}
				lI4 = p2;
				_G = 1;
				continue;
			case 1:
				if (((lI4 != 0))) {
					_G = 2;
					continue;
				}
				return 0;
			case 2:
				this._avail_in = (((this._avail_in - lI4))|0);
				if (((this._dstate._wrap == 0))) {
					_G = 3;
					continue;
				}
				this._adler["update([BII)V"](this._next_in, this._next_in_index, lI4);
				_G = 3;
				continue;
			case 3:
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((this._next_in), this._next_in_index, (p0), p1, lI4);
				this._next_in_index = (((this._next_in_index + lI4))|0);
				this._total_in = N.ladd(this._total_in, N.i2j(lI4));
				return lI4;
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_compression_jzlib_ZStream.prototype["deflate(I)I"] = function(p0) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._dstate != null))) {
					_G = 1;
					continue;
				}
				return -2;
			case 1:
				return this._dstate["deflate(I)I"](p0);
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_compression_jzlib_ZStream.prototype["setOutput([BII)V"] = function(p0, p1, p2) { 
	this._next_out = p0;
	this._next_out_index = p1;
	this._avail_out = p2;
	return;
};
com_jtransc_compression_jzlib_ZStream.prototype["setInput([BIIZ)V"] = function(p0, p1, p2, p3) { 
	var _G = 0, lA5 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p2 > 0))) {
					_G = 1;
					continue;
				}
				if (!(p3)) {
					_G = 1;
					continue;
				}
				if (((this._next_in == null))) {
					_G = 1;
					continue;
				}
				return;
				_G = 1;
				continue;
			case 1:
				if (((this._avail_in <= 0))) {
					_G = 2;
					continue;
				}
				if (!(p3)) {
					_G = 2;
					continue;
				}
				lA5 = (new JA_B((((this._avail_in + p2))|0)));
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((this._next_in), this._next_in_index, lA5, 0, this._avail_in);
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((p0), p1, lA5, this._avail_in, p2);
				this._next_in = (lA5);
				this._next_in_index = 0;
				this._avail_in = (((this._avail_in + p2))|0);
				_G = 3;
				continue;
			case 2:
				this._next_in = p0;
				this._next_in_index = p1;
				this._avail_in = p2;
				_G = 3;
				continue;
			case 3:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_ZStream.prototype["getTotalOut()J"] = function() { 
	return this._total_out;
};
function com_jtransc_compression_jzlib_Deflater() {
}
com_jtransc_compression_jzlib_Deflater.prototype = Object.create(com_jtransc_compression_jzlib_ZStream.prototype);
com_jtransc_compression_jzlib_Deflater.prototype.constructor = com_jtransc_compression_jzlib_Deflater;
com_jtransc_compression_jzlib_Deflater.prototype._finished = false;
com_jtransc_compression_jzlib_Deflater.prototype._dstate = null;
com_jtransc_compression_jzlib_Deflater.prototype._avail_out = 0;
com_jtransc_compression_jzlib_Deflater.prototype._next_out = null;
com_jtransc_compression_jzlib_Deflater.prototype._next_in = null;
com_jtransc_compression_jzlib_Deflater.prototype._avail_in = 0;
com_jtransc_compression_jzlib_Deflater.prototype._msg = null;
com_jtransc_compression_jzlib_Deflater.prototype._total_in = N.lnew(0, 0);
com_jtransc_compression_jzlib_Deflater.prototype._adler = null;
com_jtransc_compression_jzlib_Deflater.prototype._next_out_index = 0;
com_jtransc_compression_jzlib_Deflater.prototype._total_out = N.lnew(0, 0);
com_jtransc_compression_jzlib_Deflater.prototype._next_in_index = 0;
com_jtransc_compression_jzlib_Deflater.prototype._data_type = 0;
com_jtransc_compression_jzlib_Deflater.SI = function(){};
com_jtransc_compression_jzlib_Deflater.prototype.__JT__CLASS_ID = com_jtransc_compression_jzlib_Deflater.__JT__CLASS_ID = 894;
com_jtransc_compression_jzlib_Deflater.prototype.__JT__CLASS_IDS = com_jtransc_compression_jzlib_Deflater.__JT__CLASS_IDS = [894,895,656];
com_jtransc_compression_jzlib_Deflater.prototype["com.jtransc.compression.jzlib.Deflater<init>(IZ)V"] = function(p0, p1) { 
	this["com.jtransc.compression.jzlib.Deflater<init>(IIZ)V"](p0, 15, p1);
	return this;
	return this;
};
com_jtransc_compression_jzlib_Deflater.prototype["com.jtransc.compression.jzlib.Deflater<init>(IIZ)V"] = function(p0, p1, p2) { 
	var _G = 0, lI4 = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				(this)["com.jtransc.compression.jzlib.ZStream<init>()V"]();
				this._finished = false;
				lI4 = this["init(IIZ)I"](p0, p1, p2);
				if (((lI4 == 0))) {
					_G = 1;
					continue;
				}
				tA0 = ((new com_jtransc_compression_jzlib_GZIPException()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["com.jtransc.compression.jzlib.GZIPException<init>(Ljava/lang/String;)V"]((fA2)["append(I)Ljava/lang/StringBuilder;"](lI4)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[27])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this._msg)["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
com_jtransc_compression_jzlib_Deflater.prototype["com.jtransc.compression.jzlib.Deflater<init>()V"] = function() { 
	(this)["com.jtransc.compression.jzlib.ZStream<init>()V"]();
	this._finished = false;
	return this;
	return this;
};
com_jtransc_compression_jzlib_Deflater.prototype["deflate(I)I"] = function(p0) { 
	var _G = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._dstate != null))) {
					_G = 1;
					continue;
				}
				return -2;
			case 1:
				lI2 = this._dstate["deflate(I)I"](p0);
				if (((lI2 != 1))) {
					_G = 2;
					continue;
				}
				this._finished = true;
				_G = 2;
				continue;
			case 2:
				return lI2;
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_compression_jzlib_Deflater.prototype["init(IIZ)I"] = function(p0, p1, p2) { 
	var _G = 0, fI1 = 0, fI2 = 0, fA0 = null, fA1 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				this._finished = false;
				fA0 = (this);
				tA0 = ((new com_jtransc_compression_jzlib_Deflate()));
				fA1 = tA0;
				(tA0)["com.jtransc.compression.jzlib.Deflate<init>(Lcom/jtransc/compression/jzlib/ZStream;)V"]((this));
				(fA0)._dstate = (fA1);
				fA0 = (this._dstate);
				fI1 = p0;
				if (!(p2)) {
					_G = 1;
					continue;
				}
				fI2 = ((-(p1))|0);
				_G = 2;
				continue;
			case 1:
				fI2 = p1;
				_G = 2;
				continue;
			case 2:return (fA0)["deflateInit(II)I"](fI1, fI2); 
			default:
				break;
		}
	}
	return 0;
};
// ABSTRACT
function java_nio_Buffer() {
}
java_nio_Buffer.prototype = Object.create(java_lang_Object.prototype);
java_nio_Buffer.prototype.constructor = java_nio_Buffer;
java_nio_Buffer.prototype.__elementSizeShift = 0;
java_nio_Buffer.prototype._mark = 0;
java_nio_Buffer.prototype._block = null;
java_nio_Buffer.prototype._position = 0;
java_nio_Buffer.prototype._limit = 0;
java_nio_Buffer.prototype._capacity = 0;
java_nio_Buffer.prototype.___id = 0;
java_nio_Buffer.SI = function(){};
java_nio_Buffer.prototype.__JT__CLASS_ID = java_nio_Buffer.__JT__CLASS_ID = 855;
java_nio_Buffer.prototype.__JT__CLASS_IDS = java_nio_Buffer.__JT__CLASS_IDS = [855,656];
java_nio_Buffer.prototype["java.nio.Buffer<init>(IILjava/nio/internal/MemoryBlock;)V"] = function(p0, p1, p2) { 
	var _G = 0, fI1 = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null, tA3 = null;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.lang.Object<init>()V"]();
				this._mark = -1;
				this._position = 0;
				this.__elementSizeShift = p0;
				if (((p1 >= 0))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_IllegalArgumentException()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.IllegalArgumentException<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[28])["append(I)Ljava/lang/StringBuilder;"](p1)["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				fA0 = (this);
				fA1 = (this);
				tA3 = fA1;
				fI1 = p1;
				(tA3)._limit = p1;
				(fA0)._capacity = fI1;
				this._block = p2;
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
java_nio_Buffer.prototype["toString()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_StringBuilder()));
	fA0 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"]((this)["getClass()Ljava/lang/Class;"]()["getName()Ljava/lang/String;"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[32])["append(I)Ljava/lang/StringBuilder;"](this._position)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[31])["append(I)Ljava/lang/StringBuilder;"](this._limit)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[30])["append(I)Ljava/lang/StringBuilder;"](this._capacity)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[29])["toString()Ljava/lang/String;"]();
};
java_nio_Buffer.prototype["checkIndex(I)V"] = function(p0) { 
	var _G = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 < 0))) {
					_G = 1;
					continue;
				}
				if (((p0 < this._limit))) {
					_G = 2;
					continue;
				}
				_G = 1;
				continue;
			case 1:
				tA0 = ((new java_lang_IndexOutOfBoundsException()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.IndexOutOfBoundsException<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[34])["append(I)Ljava/lang/StringBuilder;"](p0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[33])["append(I)Ljava/lang/StringBuilder;"](this._limit)["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
java_nio_Buffer.prototype["remaining()I"] = function() { 
	return (((this._limit - this._position))|0);
};
java_nio_Buffer.prototype["capacity()I"] = function() { 
	return this._capacity;
};
java_nio_Buffer.prototype["clear()Ljava/nio/Buffer;"] = function() { 
	this._position = 0;
	this._mark = -1;
	this._limit = this._capacity;
	return this;
};
// ABSTRACT
function java_nio_IntBuffer() {
}
java_nio_IntBuffer.prototype = Object.create(java_nio_Buffer.prototype);
java_nio_IntBuffer.prototype.constructor = java_nio_IntBuffer;
java_nio_IntBuffer.prototype.__elementSizeShift = 0;
java_nio_IntBuffer.prototype._mark = 0;
java_nio_IntBuffer.prototype._block = null;
java_nio_IntBuffer.prototype._position = 0;
java_nio_IntBuffer.prototype._limit = 0;
java_nio_IntBuffer.prototype._capacity = 0;
java_nio_IntBuffer.SI = function(){};
java_nio_IntBuffer.prototype.__JT__CLASS_ID = java_nio_IntBuffer.__JT__CLASS_ID = 886;
java_nio_IntBuffer.prototype.__JT__CLASS_IDS = java_nio_IntBuffer.__JT__CLASS_IDS = [886,855,656,659];
java_nio_IntBuffer.prototype["java.nio.IntBuffer<init>(I)V"] = function(p0) { 
	(this)["java.nio.Buffer<init>(IILjava/nio/internal/MemoryBlock;)V"](2, p0, null);
	return this;
	return this;
};
java_nio_IntBuffer.prototype["hashCode()I"] = function() { 
	var _G = 0, fI0 = 0, fI2 = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = this._position;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= this._limit))) {
					_G = 2;
					continue;
				}
				fI0 = lI2;
				fI2 = lI1;
				lI1 = (((lI1 + 1))|0);
				lI2 = (((fI0 + this["get(I)I"](fI2)))|0);
				_G = 1;
				continue;
			case 2:
				return lI2;
			default:
				break;
		}
	}
	return 0;
};
java_nio_IntBuffer.prototype["get(I)I"] = function() { N.methodWithoutBody('java.nio.IntBuffer.get') };
java_nio_IntBuffer.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null, fI1 = 0, fI0 = 0, fI2 = 0, lI3 = 0, lI4 = 0, lI5 = 0, fA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if ((p0 instanceof java_nio_IntBuffer)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				lA2 = (N.checkCast(p0, java_nio_IntBuffer));
				if (((this["remaining()I"]() == (lA2)["remaining()I"]()))) {
					_G = 2;
					continue;
				}
				return false;
			case 2:
				lI3 = this._position;
				lI4 = (lA2)._position;
				lI5 = 1;
				_G = 3;
				continue;
			case 3:
				if (((lI5 == 0))) {
					_G = 4;
					continue;
				}
				if (((lI3 >= this._limit))) {
					_G = 4;
					continue;
				}
				fI1 = lI3;
				lI3 = (((lI3 + 1))|0);
				fI0 = this["get(I)I"](fI1);
				fA1 = lA2;
				fI2 = lI4;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1)["get(I)I"](fI2)))) {
					_G = 5;
					continue;
				}
				fI0 = 1;
				_G = 6;
				continue;
			case 5:
				fI0 = 0;
				_G = 6;
				continue;
			case 6:
				lI5 = fI0;
				_G = 3;
				continue;
			case 4:
				return ((lI5)!=0);
			default:
				break;
		}
	}
	return false;
};
function java_nio_internal_ByteBufferAs() {
}
java_nio_internal_ByteBufferAs.prototype = Object.create(java_lang_Object_base.prototype);
java_nio_internal_ByteBufferAs.prototype.constructor = java_nio_internal_ByteBufferAs;
java_nio_internal_ByteBufferAs.SI = function(){};
java_nio_internal_ByteBufferAs.prototype.__JT__CLASS_ID = java_nio_internal_ByteBufferAs.__JT__CLASS_ID = 866;
java_nio_internal_ByteBufferAs.prototype.__JT__CLASS_IDS = java_nio_internal_ByteBufferAs.__JT__CLASS_IDS = [866,656];
// ABSTRACT
function java_nio_ByteBufferAsIntBuffer() {
}
java_nio_ByteBufferAsIntBuffer.prototype = Object.create(java_nio_IntBuffer.prototype);
java_nio_ByteBufferAsIntBuffer.prototype.constructor = java_nio_ByteBufferAsIntBuffer;
java_nio_ByteBufferAsIntBuffer.prototype._byteBuffer = null;
java_nio_ByteBufferAsIntBuffer.prototype._bytes = null;
java_nio_ByteBufferAsIntBuffer.SI = function(){};
java_nio_ByteBufferAsIntBuffer.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsIntBuffer.__JT__CLASS_ID = 891;
java_nio_ByteBufferAsIntBuffer.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsIntBuffer.__JT__CLASS_IDS = [891,886,855,656,866,659];
java_nio_ByteBufferAsIntBuffer.prototype["java.nio.ByteBufferAsIntBuffer<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.IntBuffer<init>(I)V"]((((p0["capacity()I"]() / 4))|0));
	this._byteBuffer = p0;
	this._byteBuffer["clear()Ljava/nio/Buffer;"]();
	this._bytes = p0["array()[B"]();
	this["init([B)V"](p0["array()[B"]());
	return this;
	return this;
};
java_nio_ByteBufferAsIntBuffer.prototype["init([B)V"] = function(p0) { 
	this.tarray = new Int32Array(p0.data.buffer);
};
java_nio_ByteBufferAsIntBuffer.prototype["get(I)I"] = function(p0) { 
	return this.tarray[p0];
};
java_nio_ByteBufferAsIntBuffer["asIntBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/IntBuffer;"] = function(p0) { 
	var lA1 = null;
	lA1 = p0["slice()Ljava/nio/ByteBuffer;"]();
	lA1["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"](p0["order()Ljava/nio/ByteOrder;"]());
	return (java_nio_ByteBufferAsIntBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsIntBuffer;"](lA1, p0._isLittleEndian));
};
java_nio_ByteBufferAsIntBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsIntBuffer;"] = function(p0, p1) { 
	var _G = 0, fA0 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p1)) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_nio_ByteBufferAsIntBuffer$LE()));
				fA0 = tA0;
				(tA0)["java.nio.ByteBufferAsIntBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 1:
				tA1 = ((new java_nio_ByteBufferAsIntBuffer$BE()));
				fA0 = tA1;
				(tA1)["java.nio.ByteBufferAsIntBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 2:return (fA0); 
			default:
				break;
		}
	}
	return null;
};
function java_nio_ByteBufferAsIntBuffer$LE() {
}
java_nio_ByteBufferAsIntBuffer$LE.prototype = Object.create(java_nio_ByteBufferAsIntBuffer.prototype);
java_nio_ByteBufferAsIntBuffer$LE.prototype.constructor = java_nio_ByteBufferAsIntBuffer$LE;
java_nio_ByteBufferAsIntBuffer$LE.prototype._byteBuffer = null;
java_nio_ByteBufferAsIntBuffer$LE.prototype._bytes = null;
java_nio_ByteBufferAsIntBuffer$LE.SI = function(){};
java_nio_ByteBufferAsIntBuffer$LE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsIntBuffer$LE.__JT__CLASS_ID = 893;
java_nio_ByteBufferAsIntBuffer$LE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsIntBuffer$LE.__JT__CLASS_IDS = [893,891,886,855,656,866,659];
java_nio_ByteBufferAsIntBuffer$LE.prototype["java.nio.ByteBufferAsIntBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsIntBuffer<init>(Ljava/nio/ByteBuffer;)V"](p0);
	return this;
	return this;
};
java_nio_ByteBufferAsIntBuffer$LE.prototype["get(I)I"] = function(p0) { 
	return this.tarray[p0];
};
function java_nio_ByteBufferAsIntBuffer$BE() {
}
java_nio_ByteBufferAsIntBuffer$BE.prototype = Object.create(java_nio_ByteBufferAsIntBuffer.prototype);
java_nio_ByteBufferAsIntBuffer$BE.prototype.constructor = java_nio_ByteBufferAsIntBuffer$BE;
java_nio_ByteBufferAsIntBuffer$BE.prototype._byteBuffer = null;
java_nio_ByteBufferAsIntBuffer$BE.prototype._bytes = null;
java_nio_ByteBufferAsIntBuffer$BE.SI = function(){};
java_nio_ByteBufferAsIntBuffer$BE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsIntBuffer$BE.__JT__CLASS_ID = 892;
java_nio_ByteBufferAsIntBuffer$BE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsIntBuffer$BE.__JT__CLASS_IDS = [892,891,886,855,656,866,659];
java_nio_ByteBufferAsIntBuffer$BE.prototype["java.nio.ByteBufferAsIntBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsIntBuffer<init>(Ljava/nio/ByteBuffer;)V"](p0);
	return this;
	return this;
};
java_nio_ByteBufferAsIntBuffer$BE.prototype["get(I)I"] = function(p0) { 
	return libcore_io_Memory["peekAlignedIntBE([BI)I"](this._bytes, p0);
};
// ABSTRACT
function java_nio_FloatBuffer() {
}
java_nio_FloatBuffer.prototype = Object.create(java_nio_Buffer.prototype);
java_nio_FloatBuffer.prototype.constructor = java_nio_FloatBuffer;
java_nio_FloatBuffer.prototype.__elementSizeShift = 0;
java_nio_FloatBuffer.prototype._mark = 0;
java_nio_FloatBuffer.prototype._block = null;
java_nio_FloatBuffer.prototype._position = 0;
java_nio_FloatBuffer.prototype._limit = 0;
java_nio_FloatBuffer.prototype._capacity = 0;
java_nio_FloatBuffer.SI = function(){};
java_nio_FloatBuffer.prototype.__JT__CLASS_ID = java_nio_FloatBuffer.__JT__CLASS_ID = 887;
java_nio_FloatBuffer.prototype.__JT__CLASS_IDS = java_nio_FloatBuffer.__JT__CLASS_IDS = [887,855,656,659];
java_nio_FloatBuffer.prototype["java.nio.FloatBuffer<init>(I)V"] = function(p0) { 
	(this)["java.nio.Buffer<init>(IILjava/nio/internal/MemoryBlock;)V"](2, p0, null);
	return this;
	return this;
};
java_nio_FloatBuffer.prototype["hashCode()I"] = function() { 
	var _G = 0, fI0 = 0, fI2 = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = this._position;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= this._limit))) {
					_G = 2;
					continue;
				}
				fI0 = lI2;
				fI2 = lI1;
				lI1 = (((lI1 + 1))|0);
				lI2 = (((fI0 + java_lang_Float["floatToIntBits(F)I"](this["get(I)F"](fI2))))|0);
				_G = 1;
				continue;
			case 2:
				return lI2;
			default:
				break;
		}
	}
	return 0;
};
java_nio_FloatBuffer.prototype["get(I)F"] = function() { N.methodWithoutBody('java.nio.FloatBuffer.get') };
java_nio_FloatBuffer.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lF6 = 0.0, lF7 = 0.0, lA2 = null, fI1 = 0, fI0 = 0, lI3 = 0, lI4 = 0, lI5 = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if ((p0 instanceof java_nio_FloatBuffer)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				lA2 = (N.checkCast(p0, java_nio_FloatBuffer));
				if (((this["remaining()I"]() == (lA2)["remaining()I"]()))) {
					_G = 2;
					continue;
				}
				return false;
			case 2:
				lI3 = this._position;
				lI4 = (lA2)._position;
				lI5 = 1;
				_G = 3;
				continue;
			case 3:
				if (((lI5 == 0))) {
					_G = 4;
					continue;
				}
				if (((lI3 >= this._limit))) {
					_G = 4;
					continue;
				}
				fA0 = (this);
				fI1 = lI3;
				lI3 = (((lI3 + 1))|0);
				lF6 = (fA0)["get(I)F"](fI1);
				fA0 = lA2;
				fI1 = lI4;
				lI4 = (((lI4 + 1))|0);
				lF7 = (fA0)["get(I)F"](fI1);
				if (((Math.fround(N.cmpl(lF6, lF7)) == 0))) {
					_G = 5;
					continue;
				}
				if (((Math.fround(N.cmpl(lF6, lF6)) == 0))) {
					_G = 6;
					continue;
				}
				if (((Math.fround(N.cmpl(lF7, lF7)) == 0))) {
					_G = 6;
					continue;
				}
				_G = 5;
				continue;
			case 5:
				fI0 = 1;
				_G = 7;
				continue;
			case 6:
				fI0 = 0;
				_G = 7;
				continue;
			case 7:
				lI5 = fI0;
				_G = 3;
				continue;
			case 4:
				return ((lI5)!=0);
			default:
				break;
		}
	}
	return false;
};
java_nio_FloatBuffer.prototype["put(IF)Ljava/nio/FloatBuffer;"] = function() { N.methodWithoutBody('java.nio.FloatBuffer.put') };
function java_nio_ByteBufferAsFloatBuffer() {
}
java_nio_ByteBufferAsFloatBuffer.prototype = Object.create(java_nio_FloatBuffer.prototype);
java_nio_ByteBufferAsFloatBuffer.prototype.constructor = java_nio_ByteBufferAsFloatBuffer;
java_nio_ByteBufferAsFloatBuffer.prototype._byteBuffer = null;
java_nio_ByteBufferAsFloatBuffer.prototype._bytes = null;
java_nio_ByteBufferAsFloatBuffer.SI = function(){};
java_nio_ByteBufferAsFloatBuffer.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsFloatBuffer.__JT__CLASS_ID = 888;
java_nio_ByteBufferAsFloatBuffer.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsFloatBuffer.__JT__CLASS_IDS = [888,887,855,656,866,659];
java_nio_ByteBufferAsFloatBuffer.prototype["java.nio.ByteBufferAsFloatBuffer<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.FloatBuffer<init>(I)V"]((((p0["capacity()I"]() / 4))|0));
	this._byteBuffer = p0;
	this._byteBuffer["clear()Ljava/nio/Buffer;"]();
	this._bytes = p0["array()[B"]();
	this["init([B)V"](p0["array()[B"]());
	return this;
	return this;
};
java_nio_ByteBufferAsFloatBuffer.prototype["init([B)V"] = function(p0) { 
	this.iarray = new Int32Array(p0.data.buffer); this.farray = new Float32Array(p0.data.buffer);
};
java_nio_ByteBufferAsFloatBuffer.prototype["get(I)F"] = function(p0) { 
	return this.farray[p0];
};
java_nio_ByteBufferAsFloatBuffer["asFloatBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/FloatBuffer;"] = function(p0) { 
	var lA1 = null;
	lA1 = p0["slice()Ljava/nio/ByteBuffer;"]();
	lA1["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"](p0["order()Ljava/nio/ByteOrder;"]());
	return (java_nio_ByteBufferAsFloatBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsFloatBuffer;"](lA1, p0._isLittleEndian));
};
java_nio_ByteBufferAsFloatBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsFloatBuffer;"] = function(p0, p1) { 
	var _G = 0, fA0 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p1)) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_nio_ByteBufferAsFloatBuffer$LE()));
				fA0 = tA0;
				(tA0)["java.nio.ByteBufferAsFloatBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 1:
				tA1 = ((new java_nio_ByteBufferAsFloatBuffer$BE()));
				fA0 = tA1;
				(tA1)["java.nio.ByteBufferAsFloatBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 2:return (fA0); 
			default:
				break;
		}
	}
	return null;
};
java_nio_ByteBufferAsFloatBuffer.prototype["getInt(I)I"] = function(p0) { 
	return this.iarray[p0];
};
java_nio_ByteBufferAsFloatBuffer.prototype["put(IF)Ljava/nio/FloatBuffer;"] = function(p0, p1) { 
	this.farray[p0] = p1; return this;
};
java_nio_ByteBufferAsFloatBuffer.prototype["putInt(II)Ljava/nio/FloatBuffer;"] = function(p0, p1) { 
	this.iarray[p0] = p1; return this;
};
function java_nio_ByteBufferAsFloatBuffer$LE() {
}
java_nio_ByteBufferAsFloatBuffer$LE.prototype = Object.create(java_nio_ByteBufferAsFloatBuffer.prototype);
java_nio_ByteBufferAsFloatBuffer$LE.prototype.constructor = java_nio_ByteBufferAsFloatBuffer$LE;
java_nio_ByteBufferAsFloatBuffer$LE.prototype._byteBuffer = null;
java_nio_ByteBufferAsFloatBuffer$LE.prototype._bytes = null;
java_nio_ByteBufferAsFloatBuffer$LE.SI = function(){};
java_nio_ByteBufferAsFloatBuffer$LE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsFloatBuffer$LE.__JT__CLASS_ID = 890;
java_nio_ByteBufferAsFloatBuffer$LE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsFloatBuffer$LE.__JT__CLASS_IDS = [890,888,887,855,656,866,659];
java_nio_ByteBufferAsFloatBuffer$LE.prototype["java.nio.ByteBufferAsFloatBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsFloatBuffer<init>(Ljava/nio/ByteBuffer;)V"](p0);
	return this;
	return this;
};
java_nio_ByteBufferAsFloatBuffer$LE.prototype["get(I)F"] = function(p0) { 
	return this.farray[p0];
};
java_nio_ByteBufferAsFloatBuffer$LE.prototype["getInt(I)I"] = function(p0) { 
	return this.iarray[p0];
};
java_nio_ByteBufferAsFloatBuffer$LE.prototype["putInt(II)Ljava/nio/FloatBuffer;"] = function(p0, p1) { 
	this.iarray[p0] = p1; return this;
};
java_nio_ByteBufferAsFloatBuffer$LE.prototype["put(IF)Ljava/nio/FloatBuffer;"] = function(p0, p1) { 
	this.farray[p0] = p1; return this;
};
function java_nio_ByteBufferAsFloatBuffer$BE() {
}
java_nio_ByteBufferAsFloatBuffer$BE.prototype = Object.create(java_nio_ByteBufferAsFloatBuffer.prototype);
java_nio_ByteBufferAsFloatBuffer$BE.prototype.constructor = java_nio_ByteBufferAsFloatBuffer$BE;
java_nio_ByteBufferAsFloatBuffer$BE.prototype._byteBuffer = null;
java_nio_ByteBufferAsFloatBuffer$BE.prototype._bytes = null;
java_nio_ByteBufferAsFloatBuffer$BE.SI = function(){};
java_nio_ByteBufferAsFloatBuffer$BE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsFloatBuffer$BE.__JT__CLASS_ID = 889;
java_nio_ByteBufferAsFloatBuffer$BE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsFloatBuffer$BE.__JT__CLASS_IDS = [889,888,887,855,656,866,659];
java_nio_ByteBufferAsFloatBuffer$BE.prototype["java.nio.ByteBufferAsFloatBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsFloatBuffer<init>(Ljava/nio/ByteBuffer;)V"](p0);
	return this;
	return this;
};
java_nio_ByteBufferAsFloatBuffer$BE.prototype["get(I)F"] = function(p0) { 
	return java_lang_Float["intBitsToFloat(I)F"](java_lang_Integer["reverseBytes(I)I"](this["getInt(I)I"](p0)));
};
java_nio_ByteBufferAsFloatBuffer$BE.prototype["getInt(I)I"] = function(p0) { 
	return this.iarray[p0];
};
java_nio_ByteBufferAsFloatBuffer$BE.prototype["put(IF)Ljava/nio/FloatBuffer;"] = function(p0, p1) { 
	return this["putInt(II)Ljava/nio/FloatBuffer;"](p0, java_lang_Integer["reverseBytes(I)I"](java_lang_Float["floatToRawIntBits(F)I"](p1)));
};
java_nio_ByteBufferAsFloatBuffer$BE.prototype["putInt(II)Ljava/nio/FloatBuffer;"] = function(p0, p1) { 
	this.iarray[p0] = p1; return this;
};
function java_lang_Readable() {
}
java_lang_Readable.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_Readable.prototype.constructor = java_lang_Readable;
java_lang_Readable.SI = function(){};
java_lang_Readable.prototype.__JT__CLASS_ID = java_lang_Readable.__JT__CLASS_ID = 876;
java_lang_Readable.prototype.__JT__CLASS_IDS = java_lang_Readable.__JT__CLASS_IDS = [876,656];
function java_lang_Appendable() {
}
java_lang_Appendable.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_Appendable.prototype.constructor = java_lang_Appendable;
java_lang_Appendable.SI = function(){};
java_lang_Appendable.prototype.__JT__CLASS_ID = java_lang_Appendable.__JT__CLASS_ID = 665;
java_lang_Appendable.prototype.__JT__CLASS_IDS = java_lang_Appendable.__JT__CLASS_IDS = [665,656];
function java_lang_CharSequence() {
}
java_lang_CharSequence.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_CharSequence.prototype.constructor = java_lang_CharSequence;
java_lang_CharSequence.SI = function(){};
java_lang_CharSequence.prototype.__JT__CLASS_ID = java_lang_CharSequence.__JT__CLASS_ID = 660;
java_lang_CharSequence.prototype.__JT__CLASS_IDS = java_lang_CharSequence.__JT__CLASS_IDS = [660,656];
java_lang_CharSequence.prototype["toString()Ljava/lang/String;"] = function() { N.methodWithoutBody('java.lang.CharSequence.toString') };
java_lang_CharSequence.prototype["length()I"] = function() { N.methodWithoutBody('java.lang.CharSequence.length') };
java_lang_CharSequence.prototype["charAt(I)C"] = function() { N.methodWithoutBody('java.lang.CharSequence.charAt') };
// ABSTRACT
function java_nio_CharBuffer() {
}
java_nio_CharBuffer.prototype = Object.create(java_nio_Buffer.prototype);
java_nio_CharBuffer.prototype.constructor = java_nio_CharBuffer;
java_nio_CharBuffer.prototype.__elementSizeShift = 0;
java_nio_CharBuffer.prototype._mark = 0;
java_nio_CharBuffer.prototype._block = null;
java_nio_CharBuffer.prototype._position = 0;
java_nio_CharBuffer.prototype._limit = 0;
java_nio_CharBuffer.prototype._capacity = 0;
java_nio_CharBuffer.SI = function(){};
java_nio_CharBuffer.prototype.__JT__CLASS_ID = java_nio_CharBuffer.__JT__CLASS_ID = 875;
java_nio_CharBuffer.prototype.__JT__CLASS_IDS = java_nio_CharBuffer.__JT__CLASS_IDS = [875,855,656,659,660,665,876];
java_nio_CharBuffer.prototype["java.nio.CharBuffer<init>(I)V"] = function(p0) { 
	(this)["java.nio.Buffer<init>(IILjava/nio/internal/MemoryBlock;)V"](1, p0, null);
	return this;
	return this;
};
java_nio_CharBuffer.prototype["toString()Ljava/lang/String;"] = function() { 
	var _G = 0, lA1 = null, lI2 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>(I)V"]((((this._limit - this._position))|0));
				lA1 = fA0;
				lI2 = this._position;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= this._limit))) {
					_G = 2;
					continue;
				}
				(lA1)["append(C)Ljava/lang/StringBuilder;"](this["get(I)C"](lI2));
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (lA1)["toString()Ljava/lang/String;"]();
			default:
				break;
		}
	}
	return null;
};
java_nio_CharBuffer.prototype["get(I)C"] = function() { N.methodWithoutBody('java.nio.CharBuffer.get') };
java_nio_CharBuffer.prototype["hashCode()I"] = function() { 
	var _G = 0, fI0 = 0, fI2 = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = this._position;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= this._limit))) {
					_G = 2;
					continue;
				}
				fI0 = lI2;
				fI2 = lI1;
				lI1 = (((lI1 + 1))|0);
				lI2 = (((fI0 + this["get(I)C"](fI2)))|0);
				_G = 1;
				continue;
			case 2:
				return lI2;
			default:
				break;
		}
	}
	return 0;
};
java_nio_CharBuffer.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null, fI1 = 0, fI0 = 0, fI2 = 0, lI3 = 0, lI4 = 0, lI5 = 0, fA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if ((p0 instanceof java_nio_CharBuffer)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				lA2 = (N.checkCast(p0, java_nio_CharBuffer));
				if (((this["remaining()I"]() == (lA2)["remaining()I"]()))) {
					_G = 2;
					continue;
				}
				return false;
			case 2:
				lI3 = this._position;
				lI4 = (lA2)._position;
				lI5 = 1;
				_G = 3;
				continue;
			case 3:
				if (((lI5 == 0))) {
					_G = 4;
					continue;
				}
				if (((lI3 >= this._limit))) {
					_G = 4;
					continue;
				}
				fI1 = lI3;
				lI3 = (((lI3 + 1))|0);
				fI0 = ((this["get(I)C"](fI1))|0);
				fA1 = lA2;
				fI2 = lI4;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1)["get(I)C"](fI2)))) {
					_G = 5;
					continue;
				}
				fI0 = 1;
				_G = 6;
				continue;
			case 5:
				fI0 = 0;
				_G = 6;
				continue;
			case 6:
				lI5 = fI0;
				_G = 3;
				continue;
			case 4:
				return ((lI5)!=0);
			default:
				break;
		}
	}
	return false;
};
java_nio_CharBuffer.prototype["length()I"] = function() { 
	return this["remaining()I"]();
};
java_nio_CharBuffer.prototype["charAt(I)C"] = function(p0) { 
	var _G = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 < 0))) {
					_G = 1;
					continue;
				}
				if (((p0 < this["remaining()I"]()))) {
					_G = 2;
					continue;
				}
				_G = 1;
				continue;
			case 1:
				tA0 = ((new java_lang_IndexOutOfBoundsException()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.IndexOutOfBoundsException<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[34])["append(I)Ljava/lang/StringBuilder;"](p0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[35])["append(I)Ljava/lang/StringBuilder;"](this["remaining()I"]())["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				return this["get(I)C"]((((this._position + p0))|0));
			default:
				break;
		}
	}
	return 0;
};
java_nio_CharBuffer.prototype["put(IC)Ljava/nio/CharBuffer;"] = function() { N.methodWithoutBody('java.nio.CharBuffer.put') };
// ABSTRACT
function java_nio_ByteBufferAsCharBuffer() {
}
java_nio_ByteBufferAsCharBuffer.prototype = Object.create(java_nio_CharBuffer.prototype);
java_nio_ByteBufferAsCharBuffer.prototype.constructor = java_nio_ByteBufferAsCharBuffer;
java_nio_ByteBufferAsCharBuffer.prototype._bytes = null;
java_nio_ByteBufferAsCharBuffer.prototype._byteBuffer = null;
java_nio_ByteBufferAsCharBuffer.SI = function(){};
java_nio_ByteBufferAsCharBuffer.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsCharBuffer.__JT__CLASS_ID = 882;
java_nio_ByteBufferAsCharBuffer.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsCharBuffer.__JT__CLASS_IDS = [882,875,855,656,866,659,660,665,876];
java_nio_ByteBufferAsCharBuffer.prototype["java.nio.ByteBufferAsCharBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsCharBuffer$1;)V"] = function(p0, p1) { 
	this["java.nio.ByteBufferAsCharBuffer<init>(Ljava/nio/ByteBuffer;)V"](p0);
	return this;
	return this;
};
java_nio_ByteBufferAsCharBuffer.prototype["java.nio.ByteBufferAsCharBuffer<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.CharBuffer<init>(I)V"]((((p0["capacity()I"]() / 2))|0));
	this._byteBuffer = p0;
	this._byteBuffer["clear()Ljava/nio/Buffer;"]();
	this._bytes = p0["array()[B"]();
	this["init([B)V"](p0["array()[B"]());
	return this;
	return this;
};
java_nio_ByteBufferAsCharBuffer.prototype["get(I)C"] = function(p0) { 
	return this.tarray[p0];
};
java_nio_ByteBufferAsCharBuffer.prototype["put(IC)Ljava/nio/CharBuffer;"] = function(p0, p1) { 
	this.tarray[p0] = p1; return this;
};
java_nio_ByteBufferAsCharBuffer["asCharBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/CharBuffer;"] = function(p0) { 
	var lA1 = null;
	lA1 = p0["slice()Ljava/nio/ByteBuffer;"]();
	lA1["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"](p0["order()Ljava/nio/ByteOrder;"]());
	return (java_nio_ByteBufferAsCharBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsCharBuffer;"](lA1, p0._isLittleEndian));
};
java_nio_ByteBufferAsCharBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsCharBuffer;"] = function(p0, p1) { 
	var _G = 0, fA0 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p1)) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_nio_ByteBufferAsCharBuffer$LE()));
				fA0 = tA0;
				(tA0)["java.nio.ByteBufferAsCharBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 1:
				tA1 = ((new java_nio_ByteBufferAsCharBuffer$BE()));
				fA0 = tA1;
				(tA1)["java.nio.ByteBufferAsCharBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 2:return (fA0); 
			default:
				break;
		}
	}
	return null;
};
java_nio_ByteBufferAsCharBuffer.prototype["init([B)V"] = function(p0) { 
	this.tarray = new Uint16Array(p0.data.buffer);
};
function java_nio_ByteBufferAsCharBuffer$BE() {
}
java_nio_ByteBufferAsCharBuffer$BE.prototype = Object.create(java_nio_ByteBufferAsCharBuffer.prototype);
java_nio_ByteBufferAsCharBuffer$BE.prototype.constructor = java_nio_ByteBufferAsCharBuffer$BE;
java_nio_ByteBufferAsCharBuffer$BE.prototype._bytes = null;
java_nio_ByteBufferAsCharBuffer$BE.prototype._byteBuffer = null;
java_nio_ByteBufferAsCharBuffer$BE.SI = function(){};
java_nio_ByteBufferAsCharBuffer$BE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsCharBuffer$BE.__JT__CLASS_ID = 885;
java_nio_ByteBufferAsCharBuffer$BE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsCharBuffer$BE.__JT__CLASS_IDS = [885,882,875,855,656,866,659,660,665,876];
java_nio_ByteBufferAsCharBuffer$BE.prototype["java.nio.ByteBufferAsCharBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsCharBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsCharBuffer$1;)V"](p0, null);
	return this;
	return this;
};
java_nio_ByteBufferAsCharBuffer$BE.prototype["get(I)C"] = function(p0) { 
	return java_lang_Character["reverseBytes(C)C"](java_nio_ByteBufferAsCharBuffer.prototype["get(I)C"].call((this), p0));
};
java_nio_ByteBufferAsCharBuffer$BE.prototype["put(IC)Ljava/nio/CharBuffer;"] = function(p0, p1) { 
	return java_nio_ByteBufferAsCharBuffer.prototype["put(IC)Ljava/nio/CharBuffer;"].call((this), p0, java_lang_Character["reverseBytes(C)C"](p1));
};
function java_nio_ByteBufferAsCharBuffer$1() {
}
java_nio_ByteBufferAsCharBuffer$1.prototype = Object.create(java_lang_Object.prototype);
java_nio_ByteBufferAsCharBuffer$1.prototype.constructor = java_nio_ByteBufferAsCharBuffer$1;
java_nio_ByteBufferAsCharBuffer$1.prototype.___id = 0;
java_nio_ByteBufferAsCharBuffer$1.SI = function(){};
java_nio_ByteBufferAsCharBuffer$1.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsCharBuffer$1.__JT__CLASS_ID = 884;
java_nio_ByteBufferAsCharBuffer$1.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsCharBuffer$1.__JT__CLASS_IDS = [884,656];
function java_nio_ByteBufferAsCharBuffer$LE() {
}
java_nio_ByteBufferAsCharBuffer$LE.prototype = Object.create(java_nio_ByteBufferAsCharBuffer.prototype);
java_nio_ByteBufferAsCharBuffer$LE.prototype.constructor = java_nio_ByteBufferAsCharBuffer$LE;
java_nio_ByteBufferAsCharBuffer$LE.prototype._bytes = null;
java_nio_ByteBufferAsCharBuffer$LE.prototype._byteBuffer = null;
java_nio_ByteBufferAsCharBuffer$LE.SI = function(){};
java_nio_ByteBufferAsCharBuffer$LE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsCharBuffer$LE.__JT__CLASS_ID = 883;
java_nio_ByteBufferAsCharBuffer$LE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsCharBuffer$LE.__JT__CLASS_IDS = [883,882,875,855,656,866,659,660,665,876];
java_nio_ByteBufferAsCharBuffer$LE.prototype["java.nio.ByteBufferAsCharBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsCharBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsCharBuffer$1;)V"](p0, null);
	return this;
	return this;
};
java_nio_ByteBufferAsCharBuffer$LE.prototype["get(I)C"] = function(p0) { 
	return this.tarray[p0];
};
java_nio_ByteBufferAsCharBuffer$LE.prototype["put(IC)Ljava/nio/CharBuffer;"] = function(p0, p1) { 
	this.tarray[p0] = p1; return this;
};
// ABSTRACT
function java_nio_ShortBuffer() {
}
java_nio_ShortBuffer.prototype = Object.create(java_nio_Buffer.prototype);
java_nio_ShortBuffer.prototype.constructor = java_nio_ShortBuffer;
java_nio_ShortBuffer.prototype.__elementSizeShift = 0;
java_nio_ShortBuffer.prototype._mark = 0;
java_nio_ShortBuffer.prototype._block = null;
java_nio_ShortBuffer.prototype._position = 0;
java_nio_ShortBuffer.prototype._limit = 0;
java_nio_ShortBuffer.prototype._capacity = 0;
java_nio_ShortBuffer.SI = function(){};
java_nio_ShortBuffer.prototype.__JT__CLASS_ID = java_nio_ShortBuffer.__JT__CLASS_ID = 877;
java_nio_ShortBuffer.prototype.__JT__CLASS_IDS = java_nio_ShortBuffer.__JT__CLASS_IDS = [877,855,656,659];
java_nio_ShortBuffer.prototype["java.nio.ShortBuffer<init>(I)V"] = function(p0) { 
	(this)["java.nio.Buffer<init>(IILjava/nio/internal/MemoryBlock;)V"](1, p0, null);
	return this;
	return this;
};
java_nio_ShortBuffer.prototype["hashCode()I"] = function() { 
	var _G = 0, fI0 = 0, fI2 = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = this._position;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= this._limit))) {
					_G = 2;
					continue;
				}
				fI0 = lI2;
				fI2 = lI1;
				lI1 = (((lI1 + 1))|0);
				lI2 = (((fI0 + this["get(I)S"](fI2)))|0);
				_G = 1;
				continue;
			case 2:
				return lI2;
			default:
				break;
		}
	}
	return 0;
};
java_nio_ShortBuffer.prototype["get(I)S"] = function() { N.methodWithoutBody('java.nio.ShortBuffer.get') };
java_nio_ShortBuffer.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null, fI1 = 0, fI0 = 0, fI2 = 0, lI3 = 0, lI4 = 0, lI5 = 0, fA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if ((p0 instanceof java_nio_ShortBuffer)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				lA2 = (N.checkCast(p0, java_nio_ShortBuffer));
				if (((this["remaining()I"]() == (lA2)["remaining()I"]()))) {
					_G = 2;
					continue;
				}
				return false;
			case 2:
				lI3 = this._position;
				lI4 = (lA2)._position;
				lI5 = 1;
				_G = 3;
				continue;
			case 3:
				if (((lI5 == 0))) {
					_G = 4;
					continue;
				}
				if (((lI3 >= this._limit))) {
					_G = 4;
					continue;
				}
				fI1 = lI3;
				lI3 = (((lI3 + 1))|0);
				fI0 = ((this["get(I)S"](fI1))|0);
				fA1 = lA2;
				fI2 = lI4;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1)["get(I)S"](fI2)))) {
					_G = 5;
					continue;
				}
				fI0 = 1;
				_G = 6;
				continue;
			case 5:
				fI0 = 0;
				_G = 6;
				continue;
			case 6:
				lI5 = fI0;
				_G = 3;
				continue;
			case 4:
				return ((lI5)!=0);
			default:
				break;
		}
	}
	return false;
};
function java_nio_ByteBufferAsShortBuffer() {
}
java_nio_ByteBufferAsShortBuffer.prototype = Object.create(java_nio_ShortBuffer.prototype);
java_nio_ByteBufferAsShortBuffer.prototype.constructor = java_nio_ByteBufferAsShortBuffer;
java_nio_ByteBufferAsShortBuffer.prototype._byteBuffer = null;
java_nio_ByteBufferAsShortBuffer.prototype._bytes = null;
java_nio_ByteBufferAsShortBuffer.SI = function(){};
java_nio_ByteBufferAsShortBuffer.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsShortBuffer.__JT__CLASS_ID = 878;
java_nio_ByteBufferAsShortBuffer.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsShortBuffer.__JT__CLASS_IDS = [878,877,855,656,866,659];
java_nio_ByteBufferAsShortBuffer.prototype["java.nio.ByteBufferAsShortBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsShortBuffer$1;)V"] = function(p0, p1) { 
	this["java.nio.ByteBufferAsShortBuffer<init>(Ljava/nio/ByteBuffer;)V"](p0);
	return this;
	return this;
};
java_nio_ByteBufferAsShortBuffer.prototype["java.nio.ByteBufferAsShortBuffer<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ShortBuffer<init>(I)V"]((((p0["capacity()I"]() / 2))|0));
	this._byteBuffer = p0;
	this._byteBuffer["clear()Ljava/nio/Buffer;"]();
	this._bytes = p0["array()[B"]();
	this["init([B)V"](p0["array()[B"]());
	return this;
	return this;
};
java_nio_ByteBufferAsShortBuffer.prototype["get(I)S"] = function(p0) { 
	return this.tarray[p0];
};
java_nio_ByteBufferAsShortBuffer["asShortBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/ShortBuffer;"] = function(p0) { 
	var lA1 = null;
	lA1 = p0["slice()Ljava/nio/ByteBuffer;"]();
	lA1["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"](p0["order()Ljava/nio/ByteOrder;"]());
	return (java_nio_ByteBufferAsShortBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsShortBuffer;"](lA1, p0._isLittleEndian));
};
java_nio_ByteBufferAsShortBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsShortBuffer;"] = function(p0, p1) { 
	var _G = 0, fA0 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p1)) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_nio_ByteBufferAsShortBuffer$LE()));
				fA0 = tA0;
				(tA0)["java.nio.ByteBufferAsShortBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 1:
				tA1 = ((new java_nio_ByteBufferAsShortBuffer$BE()));
				fA0 = tA1;
				(tA1)["java.nio.ByteBufferAsShortBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 2:return (fA0); 
			default:
				break;
		}
	}
	return null;
};
java_nio_ByteBufferAsShortBuffer.prototype["init([B)V"] = function(p0) { 
	this.tarray = new Int16Array(p0.data.buffer);
};
function java_nio_ByteBufferAsShortBuffer$BE() {
}
java_nio_ByteBufferAsShortBuffer$BE.prototype = Object.create(java_nio_ByteBufferAsShortBuffer.prototype);
java_nio_ByteBufferAsShortBuffer$BE.prototype.constructor = java_nio_ByteBufferAsShortBuffer$BE;
java_nio_ByteBufferAsShortBuffer$BE.prototype._byteBuffer = null;
java_nio_ByteBufferAsShortBuffer$BE.prototype._bytes = null;
java_nio_ByteBufferAsShortBuffer$BE.SI = function(){};
java_nio_ByteBufferAsShortBuffer$BE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsShortBuffer$BE.__JT__CLASS_ID = 881;
java_nio_ByteBufferAsShortBuffer$BE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsShortBuffer$BE.__JT__CLASS_IDS = [881,878,877,855,656,866,659];
java_nio_ByteBufferAsShortBuffer$BE.prototype["java.nio.ByteBufferAsShortBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsShortBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsShortBuffer$1;)V"](p0, null);
	return this;
	return this;
};
java_nio_ByteBufferAsShortBuffer$BE.prototype["get(I)S"] = function(p0) { 
	return java_lang_Short["reverseBytes(S)S"](java_nio_ByteBufferAsShortBuffer.prototype["get(I)S"].call((this), p0));
};
function java_nio_ByteBufferAsShortBuffer$1() {
}
java_nio_ByteBufferAsShortBuffer$1.prototype = Object.create(java_lang_Object.prototype);
java_nio_ByteBufferAsShortBuffer$1.prototype.constructor = java_nio_ByteBufferAsShortBuffer$1;
java_nio_ByteBufferAsShortBuffer$1.prototype.___id = 0;
java_nio_ByteBufferAsShortBuffer$1.SI = function(){};
java_nio_ByteBufferAsShortBuffer$1.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsShortBuffer$1.__JT__CLASS_ID = 880;
java_nio_ByteBufferAsShortBuffer$1.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsShortBuffer$1.__JT__CLASS_IDS = [880,656];
function java_nio_ByteBufferAsShortBuffer$LE() {
}
java_nio_ByteBufferAsShortBuffer$LE.prototype = Object.create(java_nio_ByteBufferAsShortBuffer.prototype);
java_nio_ByteBufferAsShortBuffer$LE.prototype.constructor = java_nio_ByteBufferAsShortBuffer$LE;
java_nio_ByteBufferAsShortBuffer$LE.prototype._byteBuffer = null;
java_nio_ByteBufferAsShortBuffer$LE.prototype._bytes = null;
java_nio_ByteBufferAsShortBuffer$LE.SI = function(){};
java_nio_ByteBufferAsShortBuffer$LE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsShortBuffer$LE.__JT__CLASS_ID = 879;
java_nio_ByteBufferAsShortBuffer$LE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsShortBuffer$LE.__JT__CLASS_IDS = [879,878,877,855,656,866,659];
java_nio_ByteBufferAsShortBuffer$LE.prototype["java.nio.ByteBufferAsShortBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsShortBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsShortBuffer$1;)V"](p0, null);
	return this;
	return this;
};
java_nio_ByteBufferAsShortBuffer$LE.prototype["get(I)S"] = function(p0) { 
	return this.tarray[p0];
};
function libcore_io_Memory() {
}
libcore_io_Memory.prototype = Object.create(java_lang_Object.prototype);
libcore_io_Memory.prototype.constructor = libcore_io_Memory;
libcore_io_Memory.prototype.___id = 0;
libcore_io_Memory.SI = function() { 
	libcore_io_Memory._SWAPPED = null;
	libcore_io_Memory._NATIVE = null;
	libcore_io_Memory["libcore.io.Memory<clinit>()V"]();
};
libcore_io_Memory.prototype.__JT__CLASS_ID = libcore_io_Memory.__JT__CLASS_ID = 874;
libcore_io_Memory.prototype.__JT__CLASS_IDS = libcore_io_Memory.__JT__CLASS_IDS = [874,656];
libcore_io_Memory.prototype["libcore.io.Memory<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
libcore_io_Memory["libcore.io.Memory<clinit>()V"] = function() { 
	var _G = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				libcore_io_Memory._NATIVE = java_nio_ByteOrder["nativeOrder()Ljava/nio/ByteOrder;"]();
				if (((libcore_io_Memory._NATIVE != java_nio_ByteOrder._LITTLE_ENDIAN))) {
					_G = 1;
					continue;
				}
				fA0 = java_nio_ByteOrder._BIG_ENDIAN;
				_G = 2;
				continue;
			case 1:
				fA0 = java_nio_ByteOrder._LITTLE_ENDIAN;
				_G = 2;
				continue;
			case 2:
				libcore_io_Memory._SWAPPED = fA0;
				return;
			default:
				break;
		}
	}
	return;
};
libcore_io_Memory["peekAlignedLongLE([BI)J"] = function(p0, p1) { 
	return libcore_io_Memory["peekLongLE([BI)J"](p0, ((Math.imul(p1, 8))|0));
};
libcore_io_Memory["peekLongLE([BI)J"] = function(p0, p1) { 
	var fA0 = null, fA1 = null, fI1 = 0, fI0 = 0, fI2 = 0, lI1 = 0, lI2 = 0, lI3 = 0;
	lI1 = p1;
	fA0 = p0;
	fI1 = lI1;
	lI1 = (((lI1 + 1))|0);
	fI0 = (((((((fA0.data[fI1]) & 255))|0) << 0))|0);
	fA1 = p0;
	fI2 = lI1;
	lI1 = (((lI1 + 1))|0);
	fI0 = (((fI0 | (((((((fA1.data[fI2]) & 255))|0) << 8))|0)))|0);
	fA1 = p0;
	fI2 = lI1;
	lI1 = (((lI1 + 1))|0);
	fI0 = (((fI0 | (((((((fA1.data[fI2]) & 255))|0) << 16))|0)))|0);
	fA1 = p0;
	fI2 = lI1;
	lI1 = (((lI1 + 1))|0);
	lI2 = (((fI0 | (((((((fA1.data[fI2]) & 255))|0) << 24))|0)))|0);
	fA0 = p0;
	fI1 = lI1;
	lI1 = (((lI1 + 1))|0);
	fI0 = (((((((fA0.data[fI1]) & 255))|0) << 0))|0);
	fA1 = p0;
	fI2 = lI1;
	lI1 = (((lI1 + 1))|0);
	fI0 = (((fI0 | (((((((fA1.data[fI2]) & 255))|0) << 8))|0)))|0);
	fA1 = p0;
	fI2 = lI1;
	lI1 = (((lI1 + 1))|0);
	lI3 = ((((((fI0 | (((((((fA1.data[fI2]) & 255))|0) << 16))|0)))|0) | (((((((p0.data[lI1]) & 255))|0) << 24))|0)))|0);
	return N.lor(N.lshl(N.i2j(lI3), 32), N.land(N.i2j(lI2), N.lnew(0, -1)));
};
libcore_io_Memory["pokeAlignedLongLE([BIJ)V"] = function(p0, p1, p2) { 
	com_jtransc_JTranscBits["writeLongLE([BIJ)V"](p0, ((Math.imul(p1, 8))|0), p2);
	return;
};
libcore_io_Memory["peekAlignedIntBE([BI)I"] = function(p0, p1) { 
	return java_lang_Integer["reverseBytes(I)I"](libcore_io_Memory["peekAlignedIntLE([BI)I"](p0, p1));
};
libcore_io_Memory["peekAlignedIntLE([BI)I"] = function(p0, p1) { 
	return libcore_io_Memory["peekIntLE([BI)I"](p0, ((Math.imul(p1, 4))|0));
};
libcore_io_Memory["peekIntLE([BI)I"] = function(p0, p1) { 
	var fA1 = null, fI1 = 0, fI0 = 0, fI2 = 0, lI1 = 0;
	lI1 = p1;
	fI1 = lI1;
	lI1 = (((lI1 + 1))|0);
	fI0 = (((((((p0.data[fI1]) & 255))|0) << 0))|0);
	fA1 = p0;
	fI2 = lI1;
	lI1 = (((lI1 + 1))|0);
	fI0 = (((fI0 | (((((((fA1.data[fI2]) & 255))|0) << 8))|0)))|0);
	fA1 = p0;
	fI2 = lI1;
	lI1 = (((lI1 + 1))|0);
	return ((((((fI0 | (((((((fA1.data[fI2]) & 255))|0) << 16))|0)))|0) | (((((((p0.data[lI1]) & 255))|0) << 24))|0)))|0);
};
// ABSTRACT
function java_nio_DoubleBuffer() {
}
java_nio_DoubleBuffer.prototype = Object.create(java_nio_Buffer.prototype);
java_nio_DoubleBuffer.prototype.constructor = java_nio_DoubleBuffer;
java_nio_DoubleBuffer.prototype.__elementSizeShift = 0;
java_nio_DoubleBuffer.prototype._mark = 0;
java_nio_DoubleBuffer.prototype._block = null;
java_nio_DoubleBuffer.prototype._position = 0;
java_nio_DoubleBuffer.prototype._limit = 0;
java_nio_DoubleBuffer.prototype._capacity = 0;
java_nio_DoubleBuffer.SI = function(){};
java_nio_DoubleBuffer.prototype.__JT__CLASS_ID = java_nio_DoubleBuffer.__JT__CLASS_ID = 863;
java_nio_DoubleBuffer.prototype.__JT__CLASS_IDS = java_nio_DoubleBuffer.__JT__CLASS_IDS = [863,855,656,659];
java_nio_DoubleBuffer.prototype["java.nio.DoubleBuffer<init>(I)V"] = function(p0) { 
	(this)["java.nio.Buffer<init>(IILjava/nio/internal/MemoryBlock;)V"](3, p0, null);
	return this;
	return this;
};
java_nio_DoubleBuffer.prototype["hashCode()I"] = function() { 
	var _G = 0, fI1 = 0, lI1 = 0, lI2 = 0, lJ3 = N.lnew(0, 0);
	while (true) {
		switch (_G) {
			case 0:
				lI1 = this._position;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= this._limit))) {
					_G = 2;
					continue;
				}
				fI1 = lI1;
				lI1 = (((lI1 + 1))|0);
				lJ3 = java_lang_Double["doubleToLongBits(D)J"](this["get(I)D"](fI1));
				lI2 = ((((((lI2 + N.j2i(lJ3)))|0) ^ N.j2i(N.lshr(lJ3, 32))))|0);
				_G = 1;
				continue;
			case 2:
				return lI2;
			default:
				break;
		}
	}
	return 0;
};
java_nio_DoubleBuffer.prototype["get(I)D"] = function() { N.methodWithoutBody('java.nio.DoubleBuffer.get') };
java_nio_DoubleBuffer.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null, fI1 = 0, fI0 = 0, lI3 = 0, lI4 = 0, lI5 = 0, fA0 = null, lD6 = 0.0, lD8 = 0.0;
	while (true) {
		switch (_G) {
			case 0:
				if ((p0 instanceof java_nio_DoubleBuffer)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				lA2 = (N.checkCast(p0, java_nio_DoubleBuffer));
				if (((this["remaining()I"]() == (lA2)["remaining()I"]()))) {
					_G = 2;
					continue;
				}
				return false;
			case 2:
				lI3 = this._position;
				lI4 = (lA2)._position;
				lI5 = 1;
				_G = 3;
				continue;
			case 3:
				if (((lI5 == 0))) {
					_G = 4;
					continue;
				}
				if (((lI3 >= this._limit))) {
					_G = 4;
					continue;
				}
				fA0 = (this);
				fI1 = lI3;
				lI3 = (((lI3 + 1))|0);
				lD6 = (fA0)["get(I)D"](fI1);
				fA0 = lA2;
				fI1 = lI4;
				lI4 = (((lI4 + 1))|0);
				lD8 = (fA0)["get(I)D"](fI1);
				if (((+(N.cmpl(lD6, lD8)) == 0))) {
					_G = 5;
					continue;
				}
				if (((+(N.cmpl(lD6, lD6)) == 0))) {
					_G = 6;
					continue;
				}
				if (((+(N.cmpl(lD8, lD8)) == 0))) {
					_G = 6;
					continue;
				}
				_G = 5;
				continue;
			case 5:
				fI0 = 1;
				_G = 7;
				continue;
			case 6:
				fI0 = 0;
				_G = 7;
				continue;
			case 7:
				lI5 = fI0;
				_G = 3;
				continue;
			case 4:
				return ((lI5)!=0);
			default:
				break;
		}
	}
	return false;
};
java_nio_DoubleBuffer.prototype["put(ID)Ljava/nio/DoubleBuffer;"] = function() { N.methodWithoutBody('java.nio.DoubleBuffer.put') };
// ABSTRACT
function java_nio_ByteBufferAsDoubleBuffer() {
}
java_nio_ByteBufferAsDoubleBuffer.prototype = Object.create(java_nio_DoubleBuffer.prototype);
java_nio_ByteBufferAsDoubleBuffer.prototype.constructor = java_nio_ByteBufferAsDoubleBuffer;
java_nio_ByteBufferAsDoubleBuffer.prototype._byteBuffer = null;
java_nio_ByteBufferAsDoubleBuffer.prototype._bytes = null;
java_nio_ByteBufferAsDoubleBuffer.SI = function(){};
java_nio_ByteBufferAsDoubleBuffer.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsDoubleBuffer.__JT__CLASS_ID = 870;
java_nio_ByteBufferAsDoubleBuffer.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsDoubleBuffer.__JT__CLASS_IDS = [870,863,855,656,866,659];
java_nio_ByteBufferAsDoubleBuffer.prototype["java.nio.ByteBufferAsDoubleBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsDoubleBuffer$1;)V"] = function(p0, p1) { 
	this["java.nio.ByteBufferAsDoubleBuffer<init>(Ljava/nio/ByteBuffer;)V"](p0);
	return this;
	return this;
};
java_nio_ByteBufferAsDoubleBuffer.prototype["java.nio.ByteBufferAsDoubleBuffer<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.DoubleBuffer<init>(I)V"]((((p0["capacity()I"]() / 8))|0));
	this._byteBuffer = p0;
	this._byteBuffer["clear()Ljava/nio/Buffer;"]();
	this._bytes = p0["array()[B"]();
	this["init([B)V"](p0["array()[B"]());
	return this;
	return this;
};
java_nio_ByteBufferAsDoubleBuffer.prototype["get(I)D"] = function(p0) { 
	return this.tarray[p0];
};
java_nio_ByteBufferAsDoubleBuffer["asDoubleBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/DoubleBuffer;"] = function(p0) { 
	var lA1 = null;
	lA1 = p0["slice()Ljava/nio/ByteBuffer;"]();
	lA1["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"](p0["order()Ljava/nio/ByteOrder;"]());
	return (java_nio_ByteBufferAsDoubleBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsDoubleBuffer;"](lA1, p0._isLittleEndian));
};
java_nio_ByteBufferAsDoubleBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsDoubleBuffer;"] = function(p0, p1) { 
	var _G = 0, fA0 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p1)) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_nio_ByteBufferAsDoubleBuffer$LE()));
				fA0 = tA0;
				(tA0)["java.nio.ByteBufferAsDoubleBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 1:
				tA1 = ((new java_nio_ByteBufferAsDoubleBuffer$BE()));
				fA0 = tA1;
				(tA1)["java.nio.ByteBufferAsDoubleBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 2:return (fA0); 
			default:
				break;
		}
	}
	return null;
};
java_nio_ByteBufferAsDoubleBuffer.prototype["init([B)V"] = function(p0) { 
	this.tarray = new Float64Array(p0.data.buffer);
};
java_nio_ByteBufferAsDoubleBuffer.prototype["getLong(I)J"] = function(p0) { 
	return libcore_io_Memory["peekAlignedLongLE([BI)J"](this._bytes, p0);
};
java_nio_ByteBufferAsDoubleBuffer.prototype["put(ID)Ljava/nio/DoubleBuffer;"] = function(p0, p1) { 
	this.tarray[p0] = p1; return this;
};
java_nio_ByteBufferAsDoubleBuffer.prototype["putLong(IJ)Ljava/nio/DoubleBuffer;"] = function(p0, p1) { 
	libcore_io_Memory["pokeAlignedLongLE([BIJ)V"](this._bytes, p0, p1);
	return (this);
};
function java_nio_ByteBufferAsDoubleBuffer$BE() {
}
java_nio_ByteBufferAsDoubleBuffer$BE.prototype = Object.create(java_nio_ByteBufferAsDoubleBuffer.prototype);
java_nio_ByteBufferAsDoubleBuffer$BE.prototype.constructor = java_nio_ByteBufferAsDoubleBuffer$BE;
java_nio_ByteBufferAsDoubleBuffer$BE.prototype._byteBuffer = null;
java_nio_ByteBufferAsDoubleBuffer$BE.prototype._bytes = null;
java_nio_ByteBufferAsDoubleBuffer$BE.SI = function(){};
java_nio_ByteBufferAsDoubleBuffer$BE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsDoubleBuffer$BE.__JT__CLASS_ID = 873;
java_nio_ByteBufferAsDoubleBuffer$BE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsDoubleBuffer$BE.__JT__CLASS_IDS = [873,870,863,855,656,866,659];
java_nio_ByteBufferAsDoubleBuffer$BE.prototype["java.nio.ByteBufferAsDoubleBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsDoubleBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsDoubleBuffer$1;)V"](p0, null);
	return this;
	return this;
};
java_nio_ByteBufferAsDoubleBuffer$BE.prototype["get(I)D"] = function(p0) { 
	return java_lang_Double["longBitsToDouble(J)D"](java_lang_Long["reverseBytes(J)J"](this["getLong(I)J"](p0)));
};
java_nio_ByteBufferAsDoubleBuffer$BE.prototype["getLong(I)J"] = function(p0) { 
	return java_nio_ByteBufferAsDoubleBuffer.prototype["getLong(I)J"].call((this), p0);
};
java_nio_ByteBufferAsDoubleBuffer$BE.prototype["put(ID)Ljava/nio/DoubleBuffer;"] = function(p0, p1) { 
	return this["putLong(IJ)Ljava/nio/DoubleBuffer;"](p0, com_jtransc_JTranscBits["reverseBytes(J)J"](java_lang_Double["doubleToRawLongBits(D)J"](p1)));
};
java_nio_ByteBufferAsDoubleBuffer$BE.prototype["putLong(IJ)Ljava/nio/DoubleBuffer;"] = function(p0, p1) { 
	return java_nio_ByteBufferAsDoubleBuffer.prototype["putLong(IJ)Ljava/nio/DoubleBuffer;"].call((this), p0, p1);
};
function java_nio_ByteBufferAsDoubleBuffer$1() {
}
java_nio_ByteBufferAsDoubleBuffer$1.prototype = Object.create(java_lang_Object.prototype);
java_nio_ByteBufferAsDoubleBuffer$1.prototype.constructor = java_nio_ByteBufferAsDoubleBuffer$1;
java_nio_ByteBufferAsDoubleBuffer$1.prototype.___id = 0;
java_nio_ByteBufferAsDoubleBuffer$1.SI = function(){};
java_nio_ByteBufferAsDoubleBuffer$1.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsDoubleBuffer$1.__JT__CLASS_ID = 872;
java_nio_ByteBufferAsDoubleBuffer$1.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsDoubleBuffer$1.__JT__CLASS_IDS = [872,656];
function java_nio_ByteBufferAsDoubleBuffer$LE() {
}
java_nio_ByteBufferAsDoubleBuffer$LE.prototype = Object.create(java_nio_ByteBufferAsDoubleBuffer.prototype);
java_nio_ByteBufferAsDoubleBuffer$LE.prototype.constructor = java_nio_ByteBufferAsDoubleBuffer$LE;
java_nio_ByteBufferAsDoubleBuffer$LE.prototype._byteBuffer = null;
java_nio_ByteBufferAsDoubleBuffer$LE.prototype._bytes = null;
java_nio_ByteBufferAsDoubleBuffer$LE.SI = function(){};
java_nio_ByteBufferAsDoubleBuffer$LE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsDoubleBuffer$LE.__JT__CLASS_ID = 871;
java_nio_ByteBufferAsDoubleBuffer$LE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsDoubleBuffer$LE.__JT__CLASS_IDS = [871,870,863,855,656,866,659];
java_nio_ByteBufferAsDoubleBuffer$LE.prototype["java.nio.ByteBufferAsDoubleBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsDoubleBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsDoubleBuffer$1;)V"](p0, null);
	return this;
	return this;
};
java_nio_ByteBufferAsDoubleBuffer$LE.prototype["get(I)D"] = function(p0) { 
	return this.tarray[p0];
};
java_nio_ByteBufferAsDoubleBuffer$LE.prototype["getLong(I)J"] = function(p0) { 
	return java_nio_ByteBufferAsDoubleBuffer.prototype["getLong(I)J"].call((this), p0);
};
java_nio_ByteBufferAsDoubleBuffer$LE.prototype["put(ID)Ljava/nio/DoubleBuffer;"] = function(p0, p1) { 
	this.tarray[p0] = p1; return this;
};
java_nio_ByteBufferAsDoubleBuffer$LE.prototype["putLong(IJ)Ljava/nio/DoubleBuffer;"] = function(p0, p1) { 
	return java_nio_ByteBufferAsDoubleBuffer.prototype["putLong(IJ)Ljava/nio/DoubleBuffer;"].call((this), p0, p1);
};
// ABSTRACT
function java_nio_LongBuffer() {
}
java_nio_LongBuffer.prototype = Object.create(java_nio_Buffer.prototype);
java_nio_LongBuffer.prototype.constructor = java_nio_LongBuffer;
java_nio_LongBuffer.prototype.__elementSizeShift = 0;
java_nio_LongBuffer.prototype._mark = 0;
java_nio_LongBuffer.prototype._block = null;
java_nio_LongBuffer.prototype._position = 0;
java_nio_LongBuffer.prototype._limit = 0;
java_nio_LongBuffer.prototype._capacity = 0;
java_nio_LongBuffer.SI = function(){};
java_nio_LongBuffer.prototype.__JT__CLASS_ID = java_nio_LongBuffer.__JT__CLASS_ID = 864;
java_nio_LongBuffer.prototype.__JT__CLASS_IDS = java_nio_LongBuffer.__JT__CLASS_IDS = [864,855,656,659];
java_nio_LongBuffer.prototype["java.nio.LongBuffer<init>(I)V"] = function(p0) { 
	(this)["java.nio.Buffer<init>(IILjava/nio/internal/MemoryBlock;)V"](3, p0, null);
	return this;
	return this;
};
java_nio_LongBuffer.prototype["hashCode()I"] = function() { 
	var _G = 0, fI1 = 0, lI1 = 0, lI2 = 0, lJ3 = N.lnew(0, 0);
	while (true) {
		switch (_G) {
			case 0:
				lI1 = this._position;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= this._limit))) {
					_G = 2;
					continue;
				}
				fI1 = lI1;
				lI1 = (((lI1 + 1))|0);
				lJ3 = this["get(I)J"](fI1);
				lI2 = ((((((lI2 + N.j2i(lJ3)))|0) ^ N.j2i(N.lshr(lJ3, 32))))|0);
				_G = 1;
				continue;
			case 2:
				return lI2;
			default:
				break;
		}
	}
	return 0;
};
java_nio_LongBuffer.prototype["get(I)J"] = function() { N.methodWithoutBody('java.nio.LongBuffer.get') };
java_nio_LongBuffer.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fJ0 = N.lnew(0, 0), lA2 = null, fI1 = 0, fI2 = 0, fI0 = 0, lI3 = 0, lI4 = 0, lI5 = 0, fA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if ((p0 instanceof java_nio_LongBuffer)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				lA2 = (N.checkCast(p0, java_nio_LongBuffer));
				if (((this["remaining()I"]() == (lA2)["remaining()I"]()))) {
					_G = 2;
					continue;
				}
				return false;
			case 2:
				lI3 = this._position;
				lI4 = (lA2)._position;
				lI5 = 1;
				_G = 3;
				continue;
			case 3:
				if (((lI5 == 0))) {
					_G = 4;
					continue;
				}
				if (((lI3 >= this._limit))) {
					_G = 4;
					continue;
				}
				fI1 = lI3;
				lI3 = (((lI3 + 1))|0);
				fJ0 = this["get(I)J"](fI1);
				fA1 = lA2;
				fI2 = lI4;
				lI4 = (((lI4 + 1))|0);
				if (((((N.lcmp(fJ0, (fA1)["get(I)J"](fI2)))|0) != 0))) {
					_G = 5;
					continue;
				}
				fI0 = 1;
				_G = 6;
				continue;
			case 5:
				fI0 = 0;
				_G = 6;
				continue;
			case 6:
				lI5 = fI0;
				_G = 3;
				continue;
			case 4:
				return ((lI5)!=0);
			default:
				break;
		}
	}
	return false;
};
// ABSTRACT
function java_nio_ByteBufferAsLongBuffer() {
}
java_nio_ByteBufferAsLongBuffer.prototype = Object.create(java_nio_LongBuffer.prototype);
java_nio_ByteBufferAsLongBuffer.prototype.constructor = java_nio_ByteBufferAsLongBuffer;
java_nio_ByteBufferAsLongBuffer.prototype._bytes = null;
java_nio_ByteBufferAsLongBuffer.prototype._byteBuffer = null;
java_nio_ByteBufferAsLongBuffer.SI = function(){};
java_nio_ByteBufferAsLongBuffer.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsLongBuffer.__JT__CLASS_ID = 865;
java_nio_ByteBufferAsLongBuffer.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsLongBuffer.__JT__CLASS_IDS = [865,864,855,656,866,659];
java_nio_ByteBufferAsLongBuffer.prototype["java.nio.ByteBufferAsLongBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsLongBuffer$1;)V"] = function(p0, p1) { 
	this["java.nio.ByteBufferAsLongBuffer<init>(Ljava/nio/ByteBuffer;)V"](p0);
	return this;
	return this;
};
java_nio_ByteBufferAsLongBuffer.prototype["java.nio.ByteBufferAsLongBuffer<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.LongBuffer<init>(I)V"]((((p0["capacity()I"]() / 8))|0));
	this._byteBuffer = p0;
	this._byteBuffer["clear()Ljava/nio/Buffer;"]();
	this._bytes = p0["array()[B"]();
	this["init([B)V"](p0["array()[B"]());
	return this;
	return this;
};
java_nio_ByteBufferAsLongBuffer.prototype["get(I)J"] = function(p0) { 
	var low = this.tarray[p0 * 2 + 0]; var high = this.tarray[p0 * 2 + 1]; return N.lnew(high, low);
};
java_nio_ByteBufferAsLongBuffer["asLongBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/LongBuffer;"] = function(p0) { 
	var lA1 = null;
	lA1 = p0["slice()Ljava/nio/ByteBuffer;"]();
	lA1["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"](p0["order()Ljava/nio/ByteOrder;"]());
	return (java_nio_ByteBufferAsLongBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsLongBuffer;"](lA1, p0._isLittleEndian));
};
java_nio_ByteBufferAsLongBuffer["create(Ljava/nio/ByteBuffer;Z)Ljava/nio/ByteBufferAsLongBuffer;"] = function(p0, p1) { 
	var _G = 0, fA0 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p1)) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_nio_ByteBufferAsLongBuffer$LE()));
				fA0 = tA0;
				(tA0)["java.nio.ByteBufferAsLongBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 1:
				tA1 = ((new java_nio_ByteBufferAsLongBuffer$BE()));
				fA0 = tA1;
				(tA1)["java.nio.ByteBufferAsLongBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"](p0);
				_G = 2;
				continue;
			case 2:return (fA0); 
			default:
				break;
		}
	}
	return null;
};
java_nio_ByteBufferAsLongBuffer.prototype["init([B)V"] = function(p0) { 
	this.tarray = new Int32Array(p0.data.buffer);
};
function java_nio_ByteBufferAsLongBuffer$BE() {
}
java_nio_ByteBufferAsLongBuffer$BE.prototype = Object.create(java_nio_ByteBufferAsLongBuffer.prototype);
java_nio_ByteBufferAsLongBuffer$BE.prototype.constructor = java_nio_ByteBufferAsLongBuffer$BE;
java_nio_ByteBufferAsLongBuffer$BE.prototype._bytes = null;
java_nio_ByteBufferAsLongBuffer$BE.prototype._byteBuffer = null;
java_nio_ByteBufferAsLongBuffer$BE.SI = function(){};
java_nio_ByteBufferAsLongBuffer$BE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsLongBuffer$BE.__JT__CLASS_ID = 869;
java_nio_ByteBufferAsLongBuffer$BE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsLongBuffer$BE.__JT__CLASS_IDS = [869,865,864,855,656,866,659];
java_nio_ByteBufferAsLongBuffer$BE.prototype["java.nio.ByteBufferAsLongBuffer$BE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsLongBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsLongBuffer$1;)V"](p0, null);
	return this;
	return this;
};
java_nio_ByteBufferAsLongBuffer$BE.prototype["get(I)J"] = function(p0) { 
	return com_jtransc_JTranscBits["reverseBytes(J)J"](java_nio_ByteBufferAsLongBuffer.prototype["get(I)J"].call((this), p0));
};
function java_nio_ByteBufferAsLongBuffer$1() {
}
java_nio_ByteBufferAsLongBuffer$1.prototype = Object.create(java_lang_Object.prototype);
java_nio_ByteBufferAsLongBuffer$1.prototype.constructor = java_nio_ByteBufferAsLongBuffer$1;
java_nio_ByteBufferAsLongBuffer$1.prototype.___id = 0;
java_nio_ByteBufferAsLongBuffer$1.SI = function(){};
java_nio_ByteBufferAsLongBuffer$1.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsLongBuffer$1.__JT__CLASS_ID = 868;
java_nio_ByteBufferAsLongBuffer$1.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsLongBuffer$1.__JT__CLASS_IDS = [868,656];
function java_nio_ByteBufferAsLongBuffer$LE() {
}
java_nio_ByteBufferAsLongBuffer$LE.prototype = Object.create(java_nio_ByteBufferAsLongBuffer.prototype);
java_nio_ByteBufferAsLongBuffer$LE.prototype.constructor = java_nio_ByteBufferAsLongBuffer$LE;
java_nio_ByteBufferAsLongBuffer$LE.prototype._bytes = null;
java_nio_ByteBufferAsLongBuffer$LE.prototype._byteBuffer = null;
java_nio_ByteBufferAsLongBuffer$LE.SI = function(){};
java_nio_ByteBufferAsLongBuffer$LE.prototype.__JT__CLASS_ID = java_nio_ByteBufferAsLongBuffer$LE.__JT__CLASS_ID = 867;
java_nio_ByteBufferAsLongBuffer$LE.prototype.__JT__CLASS_IDS = java_nio_ByteBufferAsLongBuffer$LE.__JT__CLASS_IDS = [867,865,864,855,656,866,659];
java_nio_ByteBufferAsLongBuffer$LE.prototype["java.nio.ByteBufferAsLongBuffer$LE<init>(Ljava/nio/ByteBuffer;)V"] = function(p0) { 
	(this)["java.nio.ByteBufferAsLongBuffer<init>(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsLongBuffer$1;)V"](p0, null);
	return this;
	return this;
};
java_nio_ByteBufferAsLongBuffer$LE.prototype["get(I)J"] = function(p0) { 
	var low = this.tarray[p0 * 2 + 0]; var high = this.tarray[p0 * 2 + 1]; return N.lnew(high, low);
};
function java_nio_ByteOrder() {
}
java_nio_ByteOrder.prototype = Object.create(java_lang_Object.prototype);
java_nio_ByteOrder.prototype.constructor = java_nio_ByteOrder;
java_nio_ByteOrder.prototype.__name = null;
java_nio_ByteOrder.prototype._needsSwap = false;
java_nio_ByteOrder.prototype.___id = 0;
java_nio_ByteOrder.SI = function() { 
	java_nio_ByteOrder._LITTLE_ENDIAN = null;
	java_nio_ByteOrder._NATIVE_ORDER = null;
	java_nio_ByteOrder._BIG_ENDIAN = null;
	java_nio_ByteOrder._isLittleEndian = false;
	java_nio_ByteOrder["java.nio.ByteOrder<clinit>()V"]();
};
java_nio_ByteOrder.prototype.__JT__CLASS_ID = java_nio_ByteOrder.__JT__CLASS_ID = 862;
java_nio_ByteOrder.prototype.__JT__CLASS_IDS = java_nio_ByteOrder.__JT__CLASS_IDS = [862,656];
java_nio_ByteOrder.prototype["java.nio.ByteOrder<init>(Ljava/lang/String;Z)V"] = function(p0, p1) { 
	(this)["java.lang.Object<init>()V"]();
	this.__name = p0;
	this._needsSwap = p1;
	return this;
	return this;
};
java_nio_ByteOrder["java.nio.ByteOrder<clinit>()V"] = function() { 
	var _G = 0, fI3 = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				java_nio_ByteOrder._isLittleEndian = com_jtransc_JTranscBits["isLittleEndian()Z"]();
				tA0 = ((new java_nio_ByteOrder()));
				fA0 = tA0;
				(tA0)["java.nio.ByteOrder<init>(Ljava/lang/String;Z)V"](S[36], java_nio_ByteOrder._isLittleEndian);
				java_nio_ByteOrder._BIG_ENDIAN = (fA0);
				tA1 = ((new java_nio_ByteOrder()));
				fA0 = tA1;
				fA1 = tA1;
				fA2 = S[37];
				if (java_nio_ByteOrder._isLittleEndian) {
					_G = 1;
					continue;
				}
				fI3 = 1;
				_G = 2;
				continue;
			case 1:
				fI3 = 0;
				_G = 2;
				continue;
			case 2:
				(fA1)["java.nio.ByteOrder<init>(Ljava/lang/String;Z)V"](fA2, ((fI3)!=0));
				java_nio_ByteOrder._LITTLE_ENDIAN = (fA0);
				if (!(java_nio_ByteOrder._isLittleEndian)) {
					_G = 3;
					continue;
				}
				fA0 = (java_nio_ByteOrder._LITTLE_ENDIAN);
				_G = 4;
				continue;
			case 3:
				fA0 = (java_nio_ByteOrder._BIG_ENDIAN);
				_G = 4;
				continue;
			case 4:
				java_nio_ByteOrder._NATIVE_ORDER = (fA0);
				return;
			default:
				break;
		}
	}
	return;
};
java_nio_ByteOrder.prototype["toString()Ljava/lang/String;"] = function() { 
	return this.__name;
};
java_nio_ByteOrder["nativeOrder()Ljava/nio/ByteOrder;"] = function() { 
	return java_nio_ByteOrder._NATIVE_ORDER;
};
function com_jtransc_compression_jzlib_CRC32() {
}
com_jtransc_compression_jzlib_CRC32.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_compression_jzlib_CRC32.prototype.constructor = com_jtransc_compression_jzlib_CRC32;
com_jtransc_compression_jzlib_CRC32.prototype._v = 0;
com_jtransc_compression_jzlib_CRC32.prototype.___id = 0;
com_jtransc_compression_jzlib_CRC32.SI = function() { 
	com_jtransc_compression_jzlib_CRC32._crc_table = null;
	com_jtransc_compression_jzlib_CRC32["com.jtransc.compression.jzlib.CRC32<clinit>()V"]();
};
com_jtransc_compression_jzlib_CRC32.prototype.__JT__CLASS_ID = com_jtransc_compression_jzlib_CRC32.__JT__CLASS_ID = 860;
com_jtransc_compression_jzlib_CRC32.prototype.__JT__CLASS_IDS = com_jtransc_compression_jzlib_CRC32.__JT__CLASS_IDS = [860,656,861];
com_jtransc_compression_jzlib_CRC32.prototype["com.jtransc.compression.jzlib.CRC32<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	this._v = 0;
	return this;
	return this;
};
com_jtransc_compression_jzlib_CRC32["com.jtransc.compression.jzlib.CRC32<clinit>()V"] = function() { 
	var _G = 0, lI0 = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				com_jtransc_compression_jzlib_CRC32._crc_table = null;
				com_jtransc_compression_jzlib_CRC32._crc_table = new JA_I(256);
				lI0 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI0 >= 256))) {
					_G = 2;
					continue;
				}
				lI1 = lI0;
				lI2 = 8;
				_G = 3;
				continue;
			case 3:
				lI2 = (((lI2 + -1))|0);
				if (((lI2 < 0))) {
					_G = 4;
					continue;
				}
				if ((((((lI1 & 1))|0) == 0))) {
					_G = 5;
					continue;
				}
				lI1 = (((-306674912 ^ (((lI1 >>> 1))|0)))|0);
				_G = 3;
				continue;
			case 5:
				lI1 = (((lI1 >>> 1))|0);
				_G = 3;
				continue;
			case 4:
				com_jtransc_compression_jzlib_CRC32._crc_table.data[lI0] = lI1;
				lI0 = (((lI0 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_compression_jzlib_CRC32.prototype["getValue()I"] = function() { 
	return this._v;
};
com_jtransc_compression_jzlib_CRC32.prototype["reset()V"] = function() { 
	this._v = 0;
	return;
};
com_jtransc_compression_jzlib_CRC32.prototype["update([BII)V"] = function(p0, p1, p2) { 
	var _G = 0, fI1 = 0, fI3 = 0, lI2 = 0, lI3 = 0, lI4 = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI2 = p1;
				lI3 = p2;
				lI4 = (((this._v ^ -1))|0);
				_G = 1;
				continue;
			case 1:
				lI3 = (((lI3 + -1))|0);
				if (((lI3 < 0))) {
					_G = 2;
					continue;
				}
				fA0 = com_jtransc_compression_jzlib_CRC32._crc_table;
				fI1 = lI4;
				fI3 = lI2;
				lI2 = (((lI2 + 1))|0);
				lI4 = ((((fA0.data[((((((fI1 ^ (p0.data[fI3])))|0) & 255))|0)]) ^ (((lI4 >>> 8))|0)))|0);
				_G = 1;
				continue;
			case 2:
				this._v = (((lI4 ^ -1))|0);
				return;
			default:
				break;
		}
	}
	return;
};
function java_util_zip_Checksum() {
}
java_util_zip_Checksum.prototype = Object.create(java_lang_Object_base.prototype);
java_util_zip_Checksum.prototype.constructor = java_util_zip_Checksum;
java_util_zip_Checksum.SI = function(){};
java_util_zip_Checksum.prototype.__JT__CLASS_ID = java_util_zip_Checksum.__JT__CLASS_ID = 859;
java_util_zip_Checksum.prototype.__JT__CLASS_IDS = java_util_zip_Checksum.__JT__CLASS_IDS = [859,656];
java_util_zip_Checksum.prototype["getValue()J"] = function() { N.methodWithoutBody('java.util.zip.Checksum.getValue') };
java_util_zip_Checksum.prototype["reset()V"] = function() { N.methodWithoutBody('java.util.zip.Checksum.reset') };
java_util_zip_Checksum.prototype["update([BII)V"] = function() { N.methodWithoutBody('java.util.zip.Checksum.update') };
function java_util_zip_CRC32() {
}
java_util_zip_CRC32.prototype = Object.create(java_lang_Object.prototype);
java_util_zip_CRC32.prototype.constructor = java_util_zip_CRC32;
java_util_zip_CRC32.prototype._impl = null;
java_util_zip_CRC32.prototype._tbytes = N.lnew(0, 0);
java_util_zip_CRC32.prototype.___id = 0;
java_util_zip_CRC32.SI = function() { 
	java_util_zip_CRC32._temp = null;
	java_util_zip_CRC32["java.util.zip.CRC32<clinit>()V"]();
};
java_util_zip_CRC32.prototype.__JT__CLASS_ID = java_util_zip_CRC32.__JT__CLASS_ID = 858;
java_util_zip_CRC32.prototype.__JT__CLASS_IDS = java_util_zip_CRC32.__JT__CLASS_IDS = [858,656,859];
java_util_zip_CRC32.prototype["java.util.zip.CRC32<init>()V"] = function() { 
	var fA1 = null, tA0 = null;
	(this)["java.lang.Object<init>()V"]();
	tA0 = ((new com_jtransc_compression_jzlib_CRC32()));
	fA1 = tA0;
	(tA0)["com.jtransc.compression.jzlib.CRC32<init>()V"]();
	this._impl = (fA1);
	this._tbytes = N.lnew(0, 0);
	return this;
	return this;
};
java_util_zip_CRC32["java.util.zip.CRC32<clinit>()V"] = function() { 
	java_util_zip_CRC32._temp = new JA_B(1);
	return;
};
java_util_zip_CRC32.prototype["getValue()J"] = function() { 
	return N.land(N.i2j(this._impl["getValue()I"]()), N.lnew(0, -1));
};
java_util_zip_CRC32.prototype["reset()V"] = function() { 
	this._impl["reset()V"]();
	this._tbytes = N.lnew(0, 0);
	return;
};
java_util_zip_CRC32.prototype["update([BII)V"] = function(p0, p1, p2) { 
	this["_update([BII)V"](p0, p1, p2);
	return;
};
java_util_zip_CRC32.prototype["_update([BII)V"] = function(p0, p1, p2) { 
	this._impl["update([BII)V"](p0, p1, p2);
	this._tbytes = N.ladd(this._tbytes, N.i2j(p2));
	return;
};
function java_lang_UnsupportedOperationException() {
}
java_lang_UnsupportedOperationException.prototype = Object.create(java_lang_RuntimeException.prototype);
java_lang_UnsupportedOperationException.prototype.constructor = java_lang_UnsupportedOperationException;
java_lang_UnsupportedOperationException.SI = function(){};
java_lang_UnsupportedOperationException.prototype.__JT__CLASS_ID = java_lang_UnsupportedOperationException.__JT__CLASS_ID = 735;
java_lang_UnsupportedOperationException.prototype.__JT__CLASS_IDS = java_lang_UnsupportedOperationException.__JT__CLASS_IDS = [735,696,680,681,656,658];
java_lang_UnsupportedOperationException.prototype["java.lang.UnsupportedOperationException<init>()V"] = function() { 
	(this)["java.lang.RuntimeException<init>()V"]();
	return this;
	return this;
};
function java_nio_ReadOnlyBufferException() {
}
java_nio_ReadOnlyBufferException.prototype = Object.create(java_lang_UnsupportedOperationException.prototype);
java_nio_ReadOnlyBufferException.prototype.constructor = java_nio_ReadOnlyBufferException;
java_nio_ReadOnlyBufferException.SI = function(){};
java_nio_ReadOnlyBufferException.prototype.__JT__CLASS_ID = java_nio_ReadOnlyBufferException.__JT__CLASS_ID = 857;
java_nio_ReadOnlyBufferException.prototype.__JT__CLASS_IDS = java_nio_ReadOnlyBufferException.__JT__CLASS_IDS = [857,735,696,680,681,656,658];
java_nio_ReadOnlyBufferException.prototype["java.nio.ReadOnlyBufferException<init>()V"] = function() { 
	(this)["java.lang.UnsupportedOperationException<init>()V"]();
	return this;
	return this;
};
function java_nio_internal_MemoryBlock() {
}
java_nio_internal_MemoryBlock.prototype = Object.create(java_lang_Object.prototype);
java_nio_internal_MemoryBlock.prototype.constructor = java_nio_internal_MemoryBlock;
java_nio_internal_MemoryBlock.prototype.___id = 0;
java_nio_internal_MemoryBlock.SI = function(){};
java_nio_internal_MemoryBlock.prototype.__JT__CLASS_ID = java_nio_internal_MemoryBlock.__JT__CLASS_ID = 856;
java_nio_internal_MemoryBlock.prototype.__JT__CLASS_IDS = java_nio_internal_MemoryBlock.__JT__CLASS_IDS = [856,656];
java_nio_internal_MemoryBlock.prototype["java.nio.internal.MemoryBlock<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
function java_nio_ByteBuffer() {
}
java_nio_ByteBuffer.prototype = Object.create(java_nio_Buffer.prototype);
java_nio_ByteBuffer.prototype.constructor = java_nio_ByteBuffer;
java_nio_ByteBuffer.prototype._arrayOffset = 0;
java_nio_ByteBuffer.prototype._backingArray = null;
java_nio_ByteBuffer.prototype._isReadOnly = false;
java_nio_ByteBuffer.prototype._isDirect = false;
java_nio_ByteBuffer.prototype._isNativeOrder = false;
java_nio_ByteBuffer.prototype._isLittleEndian = false;
java_nio_ByteBuffer.prototype._order = null;
java_nio_ByteBuffer.prototype.__elementSizeShift = 0;
java_nio_ByteBuffer.prototype._mark = 0;
java_nio_ByteBuffer.prototype._block = null;
java_nio_ByteBuffer.prototype._position = 0;
java_nio_ByteBuffer.prototype._limit = 0;
java_nio_ByteBuffer.prototype._capacity = 0;
java_nio_ByteBuffer.SI = function(){};
java_nio_ByteBuffer.prototype.__JT__CLASS_ID = java_nio_ByteBuffer.__JT__CLASS_ID = 854;
java_nio_ByteBuffer.prototype.__JT__CLASS_IDS = java_nio_ByteBuffer.__JT__CLASS_IDS = [854,855,656,659];
java_nio_ByteBuffer.prototype["java.nio.ByteBuffer<init>([BZ)V"] = function(p0, p1) { 
	this["java.nio.ByteBuffer<init>(I[BIZ)V"]((p0).length, p0, 0, false);
	this._isDirect = p1;
	return this;
	return this;
};
java_nio_ByteBuffer.prototype["java.nio.ByteBuffer<init>(I[BIZ)V"] = function(p0, p1, p2, p3) { 
	var _G = 0, fA0 = null, fA1 = null, fA2 = null, tA1 = null, tA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.nio.Buffer<init>(IILjava/nio/internal/MemoryBlock;)V"](0, p0, null);
				this._backingArray = p1;
				this._arrayOffset = p2;
				this._isReadOnly = p3;
				this["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"](java_nio_ByteOrder._BIG_ENDIAN);
				if ((((((p2 + p0))|0) <= (p1).length))) {
					_G = 1;
					continue;
				}
				tA1 = ((new java_lang_IndexOutOfBoundsException()));
				fA0 = tA1;
				fA1 = tA1;
				tA2 = ((new java_lang_StringBuilder()));
				fA2 = tA2;
				(tA2)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.IndexOutOfBoundsException<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[40])["append(I)Ljava/lang/StringBuilder;"]((p1).length)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[39])["append(I)Ljava/lang/StringBuilder;"](p0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[38])["append(I)Ljava/lang/StringBuilder;"](p2)["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
java_nio_ByteBuffer.prototype["java.nio.ByteBuffer<init>([B)V"] = function(p0) { 
	this["java.nio.ByteBuffer<init>(I[BIZ)V"]((p0).length, p0, 0, false);
	this._isDirect = false;
	return this;
	return this;
};
java_nio_ByteBuffer.prototype["hashCode()I"] = function() { 
	var _G = 0, fI0 = 0, fI2 = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = this._position;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= this._limit))) {
					_G = 2;
					continue;
				}
				fI0 = lI2;
				fI2 = lI1;
				lI1 = (((lI1 + 1))|0);
				lI2 = (((fI0 + this["get(I)B"](fI2)))|0);
				_G = 1;
				continue;
			case 2:
				return lI2;
			default:
				break;
		}
	}
	return 0;
};
java_nio_ByteBuffer.prototype["get(I)B"] = function(p0) { 
	this["checkIndex(I)V"](p0);
	return (this._backingArray.data[(((this._arrayOffset + p0))|0)]);
};
java_nio_ByteBuffer.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null, fI1 = 0, fI0 = 0, fI2 = 0, lI3 = 0, lI4 = 0, lI5 = 0, fA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if ((p0 instanceof java_nio_ByteBuffer)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				lA2 = (N.checkCast(p0, java_nio_ByteBuffer));
				if (((this["remaining()I"]() == (lA2)["remaining()I"]()))) {
					_G = 2;
					continue;
				}
				return false;
			case 2:
				lI3 = this._position;
				lI4 = (lA2)._position;
				lI5 = 1;
				_G = 3;
				continue;
			case 3:
				if (((lI5 == 0))) {
					_G = 4;
					continue;
				}
				if (((lI3 >= this._limit))) {
					_G = 4;
					continue;
				}
				fI1 = lI3;
				lI3 = (((lI3 + 1))|0);
				fI0 = ((this["get(I)B"](fI1))|0);
				fA1 = lA2;
				fI2 = lI4;
				lI4 = (((lI4 + 1))|0);
				if (((fI0 != (fA1)["get(I)B"](fI2)))) {
					_G = 5;
					continue;
				}
				fI0 = 1;
				_G = 6;
				continue;
			case 5:
				fI0 = 0;
				_G = 6;
				continue;
			case 6:
				lI5 = fI0;
				_G = 3;
				continue;
			case 4:
				return ((lI5)!=0);
			default:
				break;
		}
	}
	return false;
};
java_nio_ByteBuffer.prototype["array()[B"] = function() { 
	this["_checkWritable()V"]();
	return this._backingArray;
};
java_nio_ByteBuffer.prototype["_checkWritable()V"] = function() { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(this._isReadOnly)) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_nio_ReadOnlyBufferException()));
				fA0 = tA0;
				(tA0)["java.nio.ReadOnlyBufferException<init>()V"]();
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				return;
			default:
				break;
		}
	}
	return;
};
java_nio_ByteBuffer["allocateDirect(I)Ljava/nio/ByteBuffer;"] = function(p0) { 
	var _G = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null, tA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 >= 0))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_IllegalArgumentException()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.IllegalArgumentException<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[28])["append(I)Ljava/lang/StringBuilder;"](p0)["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				tA2 = ((new java_nio_ByteBuffer()));
				fA0 = tA2;
				(tA2)["java.nio.ByteBuffer<init>([BZ)V"](new JA_B(p0), true);
				return (fA0);
			default:
				break;
		}
	}
	return null;
};
java_nio_ByteBuffer.prototype["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"] = function(p0) { 
	var _G = 0, lA1 = null, fI1 = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = (p0);
				if (((lA1 != null))) {
					_G = 1;
					continue;
				}
				lA1 = (java_nio_ByteOrder._LITTLE_ENDIAN);
				_G = 1;
				continue;
			case 1:
				this._order = (lA1);
				fA0 = this;
				if (((lA1 != java_nio_ByteOrder["nativeOrder()Ljava/nio/ByteOrder;"]()))) {
					_G = 2;
					continue;
				}
				fI1 = 1;
				_G = 3;
				continue;
			case 2:
				fI1 = 0;
				_G = 3;
				continue;
			case 3:
				fA0._isNativeOrder = ((fI1)!=0);
				fA0 = this;
				if (((lA1 != java_nio_ByteOrder._LITTLE_ENDIAN))) {
					_G = 4;
					continue;
				}
				fI1 = 1;
				_G = 5;
				continue;
			case 4:
				fI1 = 0;
				_G = 5;
				continue;
			case 5:
				fA0._isLittleEndian = ((fI1)!=0);
				return this;
			default:
				break;
		}
	}
	return null;
};
java_nio_ByteBuffer.prototype["asLongBuffer()Ljava/nio/LongBuffer;"] = function() { 
	return java_nio_ByteBufferAsLongBuffer["asLongBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/LongBuffer;"](this);
};
java_nio_ByteBuffer.prototype["slice()Ljava/nio/ByteBuffer;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_nio_ByteBuffer()));
	fA0 = tA0;
	(tA0)["java.nio.ByteBuffer<init>(I[BIZ)V"](this["remaining()I"](), this._backingArray, (((this._arrayOffset + this._position))|0), this._isReadOnly);
	return (fA0);
};
java_nio_ByteBuffer.prototype["order()Ljava/nio/ByteOrder;"] = function() { 
	return this._order;
};
java_nio_ByteBuffer.prototype["asDoubleBuffer()Ljava/nio/DoubleBuffer;"] = function() { 
	return java_nio_ByteBufferAsDoubleBuffer["asDoubleBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/DoubleBuffer;"](this);
};
java_nio_ByteBuffer.prototype["asShortBuffer()Ljava/nio/ShortBuffer;"] = function() { 
	return java_nio_ByteBufferAsShortBuffer["asShortBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/ShortBuffer;"](this);
};
java_nio_ByteBuffer.prototype["asCharBuffer()Ljava/nio/CharBuffer;"] = function() { 
	return java_nio_ByteBufferAsCharBuffer["asCharBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/CharBuffer;"](this);
};
java_nio_ByteBuffer.prototype["asFloatBuffer()Ljava/nio/FloatBuffer;"] = function() { 
	return java_nio_ByteBufferAsFloatBuffer["asFloatBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/FloatBuffer;"](this);
};
java_nio_ByteBuffer.prototype["asIntBuffer()Ljava/nio/IntBuffer;"] = function() { 
	return java_nio_ByteBufferAsIntBuffer["asIntBuffer(Ljava/nio/ByteBuffer;)Ljava/nio/IntBuffer;"](this);
};
java_nio_ByteBuffer["allocate(I)Ljava/nio/ByteBuffer;"] = function(p0) { 
	var _G = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null, tA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 >= 0))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_IllegalArgumentException()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.IllegalArgumentException<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[28])["append(I)Ljava/lang/StringBuilder;"](p0)["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				tA2 = ((new java_nio_ByteBuffer()));
				fA0 = tA2;
				(tA2)["java.nio.ByteBuffer<init>([B)V"](new JA_B(p0));
				return (fA0);
			default:
				break;
		}
	}
	return null;
};
function com_jtransc_FastMemory() {
}
com_jtransc_FastMemory.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_FastMemory.prototype.constructor = com_jtransc_FastMemory;
com_jtransc_FastMemory.prototype._data = null;
com_jtransc_FastMemory.prototype.___id = 0;
com_jtransc_FastMemory.SI = function(){};
com_jtransc_FastMemory.prototype.__JT__CLASS_ID = com_jtransc_FastMemory.__JT__CLASS_ID = 853;
com_jtransc_FastMemory.prototype.__JT__CLASS_IDS = com_jtransc_FastMemory.__JT__CLASS_IDS = [853,656];
com_jtransc_FastMemory.prototype["com.jtransc.FastMemory<init>(I)V"] = function(p0) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.lang.Object<init>()V"]();
				this["_initWithSize(I)V"](p0);
				this["_createViews()V"]();
				if (!(com_jtransc_JTranscSystem["isCpp()Z"]())) {
					_G = 1;
					continue;
				}
				this["_createViewsExtra([B)V"](this._data["array()[B"]());
				_G = 1;
				continue;
			case 1:
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
com_jtransc_FastMemory.prototype["getAlignedInt32(I)I"] = function(p0) { 
	return this.s32[p0];
};
com_jtransc_FastMemory.prototype["setAlignedFloat32(IF)V"] = function(p0, p1) { 
	this.f32[p0] = p1;
};
com_jtransc_FastMemory["alloc(I)Lcom/jtransc/FastMemory;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new com_jtransc_FastMemory()));
	fA0 = tA0;
	(tA0)["com.jtransc.FastMemory<init>(I)V"](p0);
	return (fA0);
};
com_jtransc_FastMemory.prototype["_createViews()V"] = function() { 
	this.view   = new DataView(this.buffer);
	this.s8     = new Int8Array(this.buffer);
	this.u8     = new Uint8Array(this.buffer);
	this.s16    = new Int16Array(this.buffer);
	this.u16    = new Uint16Array(this.buffer);
	this.s32    = new Int32Array(this.buffer);
	this.f32    = new Float32Array(this.buffer);
	this.f64    = new Float64Array(this.buffer);
};
com_jtransc_FastMemory.prototype["_initWithSize(I)V"] = function(p0) { 
	this._length = p0; this.buffer = new ArrayBuffer((this._length + 7) & ~7);
};
com_jtransc_FastMemory.prototype["_createViewsExtra([B)V"] = function(p0) { 
	return;
};
function Benchmark$MyClass() {
}
Benchmark$MyClass.prototype = Object.create(java_lang_Object.prototype);
Benchmark$MyClass.prototype.constructor = Benchmark$MyClass;
Benchmark$MyClass.prototype._b = 0;
Benchmark$MyClass.prototype._d = null;
Benchmark$MyClass.prototype._c = null;
Benchmark$MyClass.prototype._a = 0;
Benchmark$MyClass.prototype.___id = 0;
Benchmark$MyClass.SI = function(){};
Benchmark$MyClass.prototype.__JT__CLASS_ID = Benchmark$MyClass.__JT__CLASS_ID = 852;
Benchmark$MyClass.prototype.__JT__CLASS_IDS = Benchmark$MyClass.__JT__CLASS_IDS = [852,656];
Benchmark$MyClass.prototype["Benchmark$MyClass<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	this._a = 10;
	this._b = 20;
	this._c = S[41];
	this._d = p0;
	return this;
	return this;
};
function com_jtransc_JTranscSystemProperties() {
}
com_jtransc_JTranscSystemProperties.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_JTranscSystemProperties.prototype.constructor = com_jtransc_JTranscSystemProperties;
com_jtransc_JTranscSystemProperties.prototype.___id = 0;
com_jtransc_JTranscSystemProperties.SI = function(){};
com_jtransc_JTranscSystemProperties.prototype.__JT__CLASS_ID = com_jtransc_JTranscSystemProperties.__JT__CLASS_ID = 851;
com_jtransc_JTranscSystemProperties.prototype.__JT__CLASS_IDS = com_jtransc_JTranscSystemProperties.__JT__CLASS_IDS = [851,656];
com_jtransc_JTranscSystemProperties.prototype["com.jtransc.JTranscSystemProperties<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_JTranscSystemProperties["lineSeparator()Ljava/lang/String;"] = function() { 
	return S[42];
};
com_jtransc_JTranscSystemProperties["userHome()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = (new JA_L(1, "[Ljava.lang.String;"));
	fA0 = tA0;
	(tA0).data[0] = (S[43]);
	return com_jtransc_JTranscSystemProperties["getenvs([Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"]((fA0), S[44]);
};
com_jtransc_JTranscSystemProperties["getenvs([Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"] = function(p0, p1) { 
	var _G = 0, lA5 = null, lA6 = null, lI3 = 0, lI4 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI3 = (p0).length;
				lI4 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI4 >= lI3))) {
					_G = 2;
					continue;
				}
				lA5 = ((p0).data[lI4]);
				lA6 = (java_lang_System["getenv(Ljava/lang/String;)Ljava/lang/String;"]((lA5)));
				if (((lA6 == null))) {
					_G = 3;
					continue;
				}
				return (lA6);
			case 3:
				lI4 = (((lI4 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return p1;
			default:
				break;
		}
	}
	return null;
};
com_jtransc_JTranscSystemProperties["userLanguage()Ljava/lang/String;"] = function() { 
	return S[45];
};
com_jtransc_JTranscSystemProperties["tmpdir()Ljava/lang/String;"] = function() { 
	var _G = 0, fA0 = null, lA0 = null, tA0 = null, tA3 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = (new JA_L(3, "[Ljava.lang.String;"));
				fA0 = tA0;
				(tA0).data[0] = (S[46]);
				(fA0).setArraySlice(1, [(S[47]), (S[48])]);
				lA0 = com_jtransc_JTranscSystemProperties["getenvs([Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"]((fA0), S[44]);
				if (!(com_jtransc_JTranscSystem["isWindows()Z"]())) {
					_G = 1;
					continue;
				}
				if (!(lA0["endsWith(Ljava/lang/String;)Z"](S[49]))) {
					_G = 2;
					continue;
				}
				if (lA0["endsWith(Ljava/lang/String;)Z"](S[50])) {
					_G = 1;
					continue;
				}
				_G = 2;
				continue;
			case 2:
				tA3 = ((new java_lang_StringBuilder()));
				fA0 = tA3;
				(tA3)["java.lang.StringBuilder<init>()V"]();
				lA0 = (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](lA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[50])["toString()Ljava/lang/String;"]();
				_G = 1;
				continue;
			case 1:
				return lA0;
			default:
				break;
		}
	}
	return null;
};
com_jtransc_JTranscSystemProperties["fileEncoding()Ljava/lang/String;"] = function() { 
	return S[51];
};
com_jtransc_JTranscSystemProperties["pathSeparator()Ljava/lang/String;"] = function() { 
	return S[52];
};
com_jtransc_JTranscSystemProperties["userDir()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = (new JA_L(1, "[Ljava.lang.String;"));
	fA0 = tA0;
	(tA0).data[0] = (S[43]);
	return com_jtransc_JTranscSystemProperties["getenvs([Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"]((fA0), S[44]);
};
com_jtransc_JTranscSystemProperties["userName()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = (new JA_L(2, "[Ljava.lang.String;"));
	fA0 = tA0;
	(tA0).data[0] = (S[53]);
	(fA0).data[1] = (S[54]);
	return com_jtransc_JTranscSystemProperties["getenvs([Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"]((fA0), S[55]);
};
com_jtransc_JTranscSystemProperties["userVariant()Ljava/lang/String;"] = function() { 
	return S[20];
};
com_jtransc_JTranscSystemProperties["fileSeparator()Ljava/lang/String;"] = function() { 
	return S[49];
};
com_jtransc_JTranscSystemProperties["userRegion()Ljava/lang/String;"] = function() { 
	return S[56];
};
function com_jtransc_JTranscArrays() {
}
com_jtransc_JTranscArrays.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_JTranscArrays.prototype.constructor = com_jtransc_JTranscArrays;
com_jtransc_JTranscArrays.prototype.___id = 0;
com_jtransc_JTranscArrays.SI = function() { 
	com_jtransc_JTranscArrays._EMPTY_BYTE = null;
	com_jtransc_JTranscArrays._EMPTY_CLASS = null;
	com_jtransc_JTranscArrays["com.jtransc.JTranscArrays<clinit>()V"]();
};
com_jtransc_JTranscArrays.prototype.__JT__CLASS_ID = com_jtransc_JTranscArrays.__JT__CLASS_ID = 850;
com_jtransc_JTranscArrays.prototype.__JT__CLASS_IDS = com_jtransc_JTranscArrays.__JT__CLASS_IDS = [850,656];
com_jtransc_JTranscArrays.prototype["com.jtransc.JTranscArrays<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_JTranscArrays["com.jtransc.JTranscArrays<clinit>()V"] = function() { 
	com_jtransc_JTranscArrays._EMPTY_BYTE = new JA_B(0);
	com_jtransc_JTranscArrays._EMPTY_CLASS = new JA_L(0, "[Ljava.lang.Class;");
	return;
};
com_jtransc_JTranscArrays["checkOffsetAndCount(III)V"] = function(p0, p1, p2) { 
	var _G = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if ((((((p1 | p2))|0) < 0))) {
					_G = 1;
					continue;
				}
				if (((p1 > p0))) {
					_G = 1;
					continue;
				}
				if ((((((p0 - p1))|0) >= p2))) {
					_G = 2;
					continue;
				}
				_G = 1;
				continue;
			case 1:
				tA0 = ((new java_lang_ArrayIndexOutOfBoundsException()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.ArrayIndexOutOfBoundsException<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[59])["append(I)Ljava/lang/StringBuilder;"](p0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[58])["append(I)Ljava/lang/StringBuilder;"](p1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[57])["append(I)Ljava/lang/StringBuilder;"](p2)["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
// ABSTRACT
function java_lang_ClassLoader() {
}
java_lang_ClassLoader.prototype = Object.create(java_lang_Object.prototype);
java_lang_ClassLoader.prototype.constructor = java_lang_ClassLoader;
java_lang_ClassLoader.prototype._nativeLibs = null;
java_lang_ClassLoader.prototype._parent = null;
java_lang_ClassLoader.prototype.___id = 0;
java_lang_ClassLoader.SI = function(){};
java_lang_ClassLoader.prototype.__JT__CLASS_ID = java_lang_ClassLoader.__JT__CLASS_ID = 847;
java_lang_ClassLoader.prototype.__JT__CLASS_IDS = java_lang_ClassLoader.__JT__CLASS_IDS = [847,656];
java_lang_ClassLoader.prototype["java.lang.ClassLoader<init>()V"] = function() { 
	this["java.lang.ClassLoader<init>(Ljava/lang/ClassLoader;)V"](null);
	return this;
	return this;
};
java_lang_ClassLoader.prototype["java.lang.ClassLoader<init>(Ljava/lang/ClassLoader;)V"] = function(p0) { 
	var fA1 = null, tA0 = null;
	(this)["java.lang.Object<init>()V"]();
	tA0 = ((new java_util_ArrayList()));
	fA1 = tA0;
	(tA0)["java.util.ArrayList<init>()V"]();
	this._nativeLibs = (fA1);
	this._parent = p0;
	return this;
	return this;
};
java_lang_ClassLoader["getSystemClassLoader()Ljava/lang/ClassLoader;"] = function() { 
	return java_lang__ClassInternalUtils["getSystemClassLoader()Ljava/lang/ClassLoader;"]();
};
function java_lang__ClassInternalUtils$1() {
}
java_lang__ClassInternalUtils$1.prototype = Object.create(java_lang_ClassLoader.prototype);
java_lang__ClassInternalUtils$1.prototype.constructor = java_lang__ClassInternalUtils$1;
java_lang__ClassInternalUtils$1.prototype._nativeLibs = null;
java_lang__ClassInternalUtils$1.prototype._parent = null;
java_lang__ClassInternalUtils$1.SI = function(){};
java_lang__ClassInternalUtils$1.prototype.__JT__CLASS_ID = java_lang__ClassInternalUtils$1.__JT__CLASS_ID = 849;
java_lang__ClassInternalUtils$1.prototype.__JT__CLASS_IDS = java_lang__ClassInternalUtils$1.__JT__CLASS_IDS = [849,847,656];
java_lang__ClassInternalUtils$1.prototype["java.lang._ClassInternalUtils$1<init>()V"] = function() { 
	(this)["java.lang.ClassLoader<init>()V"]();
	return this;
	return this;
};
function java_lang__ClassInternalUtils() {
}
java_lang__ClassInternalUtils.prototype = Object.create(java_lang_Object.prototype);
java_lang__ClassInternalUtils.prototype.constructor = java_lang__ClassInternalUtils;
java_lang__ClassInternalUtils.prototype.___id = 0;
java_lang__ClassInternalUtils.SI = function() { 
	java_lang__ClassInternalUtils._classLoader = null;
};
java_lang__ClassInternalUtils.prototype.__JT__CLASS_ID = java_lang__ClassInternalUtils.__JT__CLASS_ID = 848;
java_lang__ClassInternalUtils.prototype.__JT__CLASS_IDS = java_lang__ClassInternalUtils.__JT__CLASS_IDS = [848,656];
java_lang__ClassInternalUtils.prototype["java.lang._ClassInternalUtils<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang__ClassInternalUtils["getSystemClassLoader()Ljava/lang/ClassLoader;"] = function() { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((java_lang__ClassInternalUtils._classLoader != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang__ClassInternalUtils$1()));
				fA0 = tA0;
				(tA0)["java.lang._ClassInternalUtils$1<init>()V"]();
				java_lang__ClassInternalUtils._classLoader = (fA0);
				_G = 1;
				continue;
			case 1:
				return java_lang__ClassInternalUtils._classLoader;
			default:
				break;
		}
	}
	return null;
};
function java_lang_Thread$UncaughtExceptionHandler() {
}
java_lang_Thread$UncaughtExceptionHandler.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_Thread$UncaughtExceptionHandler.prototype.constructor = java_lang_Thread$UncaughtExceptionHandler;
java_lang_Thread$UncaughtExceptionHandler.SI = function(){};
java_lang_Thread$UncaughtExceptionHandler.prototype.__JT__CLASS_ID = java_lang_Thread$UncaughtExceptionHandler.__JT__CLASS_ID = 846;
java_lang_Thread$UncaughtExceptionHandler.prototype.__JT__CLASS_IDS = java_lang_Thread$UncaughtExceptionHandler.__JT__CLASS_IDS = [846,656];
function java_lang_ThreadGroup() {
}
java_lang_ThreadGroup.prototype = Object.create(java_lang_Object.prototype);
java_lang_ThreadGroup.prototype.constructor = java_lang_ThreadGroup;
java_lang_ThreadGroup.prototype.___id = 0;
java_lang_ThreadGroup.SI = function(){};
java_lang_ThreadGroup.prototype.__JT__CLASS_ID = java_lang_ThreadGroup.__JT__CLASS_ID = 845;
java_lang_ThreadGroup.prototype.__JT__CLASS_IDS = java_lang_ThreadGroup.__JT__CLASS_IDS = [845,656,846];
java_lang_ThreadGroup.prototype["toString()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_StringBuilder()));
	fA0 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"]((this)["getClass()Ljava/lang/Class;"]()["getName()Ljava/lang/String;"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[61])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this["getName()Ljava/lang/String;"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[60])["append(I)Ljava/lang/StringBuilder;"](this["getMaxPriority()I"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[29])["toString()Ljava/lang/String;"]();
};
java_lang_ThreadGroup.prototype["getMaxPriority()I"] = function() { N.methodWithoutBody('java.lang.ThreadGroup.getMaxPriority') };
java_lang_ThreadGroup.prototype["getName()Ljava/lang/String;"] = function() { N.methodWithoutBody('java.lang.ThreadGroup.getName') };
function java_lang_Runnable() {
}
java_lang_Runnable.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_Runnable.prototype.constructor = java_lang_Runnable;
java_lang_Runnable.SI = function(){};
java_lang_Runnable.prototype.__JT__CLASS_ID = java_lang_Runnable.__JT__CLASS_ID = 844;
java_lang_Runnable.prototype.__JT__CLASS_IDS = java_lang_Runnable.__JT__CLASS_IDS = [844,656];
function java_lang_Thread() {
}
java_lang_Thread.prototype = Object.create(java_lang_Object.prototype);
java_lang_Thread.prototype.constructor = java_lang_Thread;
java_lang_Thread.prototype.__name = null;
java_lang_Thread.prototype._group = null;
java_lang_Thread.prototype._classLoader = null;
java_lang_Thread.prototype.___id = 0;
java_lang_Thread.SI = function() { 
	java_lang_Thread.__currentThread = null;
};
java_lang_Thread.prototype.__JT__CLASS_ID = java_lang_Thread.__JT__CLASS_ID = 843;
java_lang_Thread.prototype.__JT__CLASS_IDS = java_lang_Thread.__JT__CLASS_IDS = [843,656,844];
java_lang_Thread.prototype["java.lang.Thread<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	this._classLoader = null;
	return this;
	return this;
};
java_lang_Thread.prototype["toString()Ljava/lang/String;"] = function() { 
	var _G = 0, lA1 = null, fA0 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = (this["getThreadGroup()Ljava/lang/ThreadGroup;"]());
				if (((lA1 == null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[63])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this["getName()Ljava/lang/String;"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[62])["append(I)Ljava/lang/StringBuilder;"](this["getPriority()I"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[62])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"]((lA1)["getName()Ljava/lang/String;"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[29])["toString()Ljava/lang/String;"]();
			case 1:
				tA1 = ((new java_lang_StringBuilder()));
				fA0 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[63])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this["getName()Ljava/lang/String;"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[62])["append(I)Ljava/lang/StringBuilder;"](this["getPriority()I"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[64])["toString()Ljava/lang/String;"]();
			default:
				break;
		}
	}
	return null;
};
java_lang_Thread.prototype["getName()Ljava/lang/String;"] = function() { 
	return this.__name;
};
java_lang_Thread.prototype["getPriority()I"] = function() { 
	return 5;
};
java_lang_Thread.prototype["getThreadGroup()Ljava/lang/ThreadGroup;"] = function() { 
	return this._group;
};
java_lang_Thread["currentThread()Ljava/lang/Thread;"] = function() { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((java_lang_Thread.__currentThread != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_Thread()));
				fA0 = tA0;
				(tA0)["java.lang.Thread<init>()V"]();
				java_lang_Thread.__currentThread = (fA0);
				_G = 1;
				continue;
			case 1:
				return java_lang_Thread.__currentThread;
			default:
				break;
		}
	}
	return null;
};
java_lang_Thread.prototype["getContextClassLoader()Ljava/lang/ClassLoader;"] = function() { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._classLoader != null))) {
					_G = 1;
					continue;
				}
				this._classLoader = java_lang__ClassInternalUtils["getSystemClassLoader()Ljava/lang/ClassLoader;"]();
				_G = 1;
				continue;
			case 1:
				return this._classLoader;
			default:
				break;
		}
	}
	return null;
};
function java_lang_reflect_Array() {
}
java_lang_reflect_Array.prototype = Object.create(java_lang_Object.prototype);
java_lang_reflect_Array.prototype.constructor = java_lang_reflect_Array;
java_lang_reflect_Array.prototype.___id = 0;
java_lang_reflect_Array.SI = function(){};
java_lang_reflect_Array.prototype.__JT__CLASS_ID = java_lang_reflect_Array.__JT__CLASS_ID = 840;
java_lang_reflect_Array.prototype.__JT__CLASS_IDS = java_lang_reflect_Array.__JT__CLASS_IDS = [840,656];
java_lang_reflect_Array.prototype["java.lang.reflect.Array<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang_reflect_Array["newInstance(Ljava/lang/Class;I)Ljava/lang/Object;"] = function(p0, p1) { 
	var _G = 0, fI0 = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null, tA2 = null, tA3 = null, tA4 = null;
	while (true) {
		switch (_G) {
			case 0:
				if ((((p0) != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_NullPointerException()));
				fA0 = tA0;
				(tA0)["java.lang.NullPointerException<init>(Ljava/lang/String;)V"](S[65]);
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				if (p0["isPrimitive()Z"]()) {
					_G = 2;
					continue;
				}
				fI0 = p1;
				tA1 = ((new java_lang_StringBuilder()));
				fA1 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				return java_lang_reflect_Array["newObjectInstance(ILjava/lang/String;)Ljava/lang/Object;"](fI0, (fA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[66])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](p0["getName()Ljava/lang/String;"]())["toString()Ljava/lang/String;"]());
			case 2:
				if ((((p0) != java_lang_Boolean._TYPE))) {
					_G = 3;
					continue;
				}
				return (new JA_Z(p1));
			case 3:
				if ((((p0) != java_lang_Byte._TYPE))) {
					_G = 4;
					continue;
				}
				return (new JA_B(p1));
			case 4:
				if ((((p0) != java_lang_Short._TYPE))) {
					_G = 5;
					continue;
				}
				return (new JA_S(p1));
			case 5:
				if ((((p0) != java_lang_Character._TYPE))) {
					_G = 6;
					continue;
				}
				return (new JA_C(p1));
			case 6:
				if ((((p0) != java_lang_Integer._TYPE))) {
					_G = 7;
					continue;
				}
				return (new JA_I(p1));
			case 7:
				if ((((p0) != java_lang_Long._TYPE))) {
					_G = 8;
					continue;
				}
				return (new JA_J(p1));
			case 8:
				if ((((p0) != java_lang_Float._TYPE))) {
					_G = 9;
					continue;
				}
				return (new JA_F(p1));
			case 9:
				if ((((p0) != java_lang_Double._TYPE))) {
					_G = 10;
					continue;
				}
				return (new JA_D(p1));
			case 10:
				if ((((p0) != java_lang_Void._TYPE))) {
					_G = 11;
					continue;
				}
				tA2 = ((new java_lang_RuntimeException()));
				fA0 = tA2;
				(tA2)["java.lang.RuntimeException<init>(Ljava/lang/String;)V"](S[67]);
				throw new WrappedError(fA0);
				_G = 11;
				continue;
			case 11:
				tA3 = ((new java_lang_RuntimeException()));
				fA0 = tA3;
				fA1 = tA3;
				tA4 = ((new java_lang_StringBuilder()));
				fA2 = tA4;
				(tA4)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.RuntimeException<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[68])["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"]((p0))["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				break;
			default:
				break;
		}
	}
	return null;
};
java_lang_reflect_Array["newObjectInstance(ILjava/lang/String;)Ljava/lang/Object;"] = function(p0, p1) { 
	return new JA_L(p0, N.istr(p1));
};
function java_util_List() {
}
java_util_List.prototype = Object.create(java_lang_Object_base.prototype);
java_util_List.prototype.constructor = java_util_List;
java_util_List.SI = function(){};
java_util_List.prototype.__JT__CLASS_ID = java_util_List.__JT__CLASS_ID = 722;
java_util_List.prototype.__JT__CLASS_IDS = java_util_List.__JT__CLASS_IDS = [722,723,724,656];
java_util_List.prototype["size()I"] = function() { N.methodWithoutBody('java.util.List.size') };
java_util_List.prototype["isEmpty()Z"] = function() { N.methodWithoutBody('java.util.List.isEmpty') };
java_util_List.prototype["iterator()Ljava/util/Iterator;"] = function() { N.methodWithoutBody('java.util.List.iterator') };
java_util_List.prototype["listIterator(I)Ljava/util/ListIterator;"] = function() { N.methodWithoutBody('java.util.List.listIterator') };
java_util_List.prototype["listIterator()Ljava/util/ListIterator;"] = function() { N.methodWithoutBody('java.util.List.listIterator') };
java_util_List.prototype["get(I)Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.List.get') };
java_util_List.prototype["add(ILjava/lang/Object;)V"] = function() { N.methodWithoutBody('java.util.List.add') };
java_util_List.prototype["add(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.List.add') };
java_util_List.prototype["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.List.toArray') };
java_util_List.prototype["hashCode()I"] = function() { N.methodWithoutBody('java.util.List.hashCode') };
java_util_List.prototype["equals(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.List.equals') };
java_util_List.prototype["contains(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.List.contains') };
java_util_List.prototype["indexOf(Ljava/lang/Object;)I"] = function() { N.methodWithoutBody('java.util.List.indexOf') };
java_util_List.prototype["containsAll(Ljava/util/Collection;)Z"] = function() { N.methodWithoutBody('java.util.List.containsAll') };
// ABSTRACT
function java_util_AbstractList() {
}
java_util_AbstractList.prototype = Object.create(java_util_AbstractCollection.prototype);
java_util_AbstractList.prototype.constructor = java_util_AbstractList;
java_util_AbstractList.prototype._modCount = 0;
java_util_AbstractList.SI = function(){};
java_util_AbstractList.prototype.__JT__CLASS_ID = java_util_AbstractList.__JT__CLASS_ID = 727;
java_util_AbstractList.prototype.__JT__CLASS_IDS = java_util_AbstractList.__JT__CLASS_IDS = [727,728,656,722,723,724];
java_util_AbstractList.prototype["java.util.AbstractList<init>()V"] = function() { 
	(this)["java.util.AbstractCollection<init>()V"]();
	return this;
	return this;
};
java_util_AbstractList.prototype["iterator()Ljava/util/Iterator;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_util_AbstractList$SimpleListIterator()));
	fA0 = tA0;
	(tA0)["java.util.AbstractList$SimpleListIterator<init>(Ljava/util/AbstractList;)V"](this);
	return (fA0);
};
java_util_AbstractList.prototype["listIterator()Ljava/util/ListIterator;"] = function() { 
	return this["listIterator(I)Ljava/util/ListIterator;"](0);
};
java_util_AbstractList.prototype["listIterator(I)Ljava/util/ListIterator;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_util_AbstractList$FullListIterator()));
	fA0 = tA0;
	(tA0)["java.util.AbstractList$FullListIterator<init>(Ljava/util/AbstractList;I)V"](this, p0);
	return (fA0);
};
java_util_AbstractList.prototype["get(I)Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.AbstractList.get') };
java_util_AbstractList.prototype["add(Ljava/lang/Object;)Z"] = function(p0) { 
	this["add(ILjava/lang/Object;)V"](this["size()I"](), p0);
	return true;
};
java_util_AbstractList.prototype["add(ILjava/lang/Object;)V"] = function(p0, p1) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_UnsupportedOperationException()));
	fA0 = tA0;
	(tA0)["java.lang.UnsupportedOperationException<init>()V"]();
	throw new WrappedError(fA0);
};
java_util_AbstractList.prototype["hashCode()I"] = function() { 
	var _G = 0, lA3 = null, fI0 = 0, fI1 = 0, lI1 = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 1;
				lA2 = this["iterator()Ljava/util/Iterator;"]();
				_G = 1;
				continue;
			case 1:
				if (!(lA2["hasNext()Z"]())) {
					_G = 2;
					continue;
				}
				lA3 = lA2["next()Ljava/lang/Object;"]();
				fI0 = ((Math.imul(31, lI1))|0);
				if (((lA3 != null))) {
					_G = 3;
					continue;
				}
				fI1 = 0;
				_G = 4;
				continue;
			case 3:
				fI1 = lA3["hashCode()I"]();
				_G = 4;
				continue;
			case 4:
				lI1 = (((fI0 + fI1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
java_util_AbstractList.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA5 = null, lA6 = null, lA2 = null, lA3 = null, lA4 = null;
	while (true) {
		switch (_G) {
			case 0:
				if ((((this) != p0))) {
					_G = 1;
					continue;
				}
				return true;
			case 1:
				if (!(N.isClassId(p0, 722))) {
					_G = 2;
					continue;
				}
				lA2 = N.checkCast(p0, java_util_List);
				if (((lA2["size()I"]() == this["size()I"]()))) {
					_G = 3;
					continue;
				}
				return false;
			case 3:
				lA3 = this["iterator()Ljava/util/Iterator;"]();
				lA4 = lA2["iterator()Ljava/util/Iterator;"]();
				_G = 4;
				continue;
			case 4:
				if (!(lA3["hasNext()Z"]())) {
					_G = 5;
					continue;
				}
				lA5 = lA3["next()Ljava/lang/Object;"]();
				lA6 = lA4["next()Ljava/lang/Object;"]();
				if (((lA5 != null))) {
					_G = 6;
					continue;
				}
				if (((lA6 != null))) {
					_G = 7;
					continue;
				}
				_G = 8;
				continue;
			case 6:
				if (lA5["equals(Ljava/lang/Object;)Z"](lA6)) {
					_G = 8;
					continue;
				}
				_G = 7;
				continue;
			case 7:
				return false;
			case 8:
				_G = 4;
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
};
java_util_AbstractList.prototype["indexOf(Ljava/lang/Object;)I"] = function(p0) { 
	var _G = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA2 = this["listIterator()Ljava/util/ListIterator;"]();
				if (((p0 == null))) {
					_G = 1;
					continue;
				}
				_G = 2;
				continue;
			case 2:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"](lA2["next()Ljava/lang/Object;"]()))) {
					_G = 2;
					continue;
				}
				return lA2["previousIndex()I"]();
			case 1:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				if (((lA2["next()Ljava/lang/Object;"]() != null))) {
					_G = 1;
					continue;
				}
				return lA2["previousIndex()I"]();
			case 3:
				return -1;
			default:
				break;
		}
	}
	return 0;
};
function java_util_RandomAccess() {
}
java_util_RandomAccess.prototype = Object.create(java_lang_Object_base.prototype);
java_util_RandomAccess.prototype.constructor = java_util_RandomAccess;
java_util_RandomAccess.SI = function(){};
java_util_RandomAccess.prototype.__JT__CLASS_ID = java_util_RandomAccess.__JT__CLASS_ID = 725;
java_util_RandomAccess.prototype.__JT__CLASS_IDS = java_util_RandomAccess.__JT__CLASS_IDS = [725,656];
function java_util_Arrays$ArrayList() {
}
java_util_Arrays$ArrayList.prototype = Object.create(java_util_AbstractList.prototype);
java_util_Arrays$ArrayList.prototype.constructor = java_util_Arrays$ArrayList;
java_util_Arrays$ArrayList.prototype._a = null;
java_util_Arrays$ArrayList.prototype._modCount = 0;
java_util_Arrays$ArrayList.SI = function(){};
java_util_Arrays$ArrayList.prototype.__JT__CLASS_ID = java_util_Arrays$ArrayList.__JT__CLASS_ID = 839;
java_util_Arrays$ArrayList.prototype.__JT__CLASS_IDS = java_util_Arrays$ArrayList.__JT__CLASS_IDS = [839,727,728,656,722,658,725,723,724];
java_util_Arrays$ArrayList.prototype["java.util.Arrays$ArrayList<init>([Ljava/lang/Object;)V"] = function(p0) { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.util.AbstractList<init>()V"]();
				if ((((p0) != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_NullPointerException()));
				fA0 = tA0;
				(tA0)["java.lang.NullPointerException<init>(Ljava/lang/String;)V"](S[69]);
				throw new WrappedError(fA0);
				_G = 1;
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
};
java_util_Arrays$ArrayList.prototype["get(I)Ljava/lang/Object;"] = function(p0) { 
	var _G = 0, fA0 = null, tA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						_G = 1;
						continue;
					case 1:
						fA0 = (this._a.data[p0]);
						_G = 2;
						continue;
					case 2:return fA0; 
					case 3:
						fA0 = (J__exception__);
						tA0 = ((new java_lang_IndexOutOfBoundsException()));
						fA0 = tA0;
						(tA0)["java.lang.IndexOutOfBoundsException<init>()V"]();
						throw new WrappedError(fA0);
						break;
					default:
						break;
				}
			}
			return null;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_ArrayIndexOutOfBoundsException)))) {
				_G = 3;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return null;
};
java_util_Arrays$ArrayList.prototype["indexOf(Ljava/lang/Object;)I"] = function(p0) { 
	var _G = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 == null))) {
					_G = 1;
					continue;
				}
				lI2 = 0;
				_G = 2;
				continue;
			case 2:
				if (((lI2 >= this._a.length))) {
					_G = 3;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"]((this._a.data[lI2])))) {
					_G = 4;
					continue;
				}
				return lI2;
			case 4:
				lI2 = (((lI2 + 1))|0);
				_G = 2;
				continue;
			case 3:
				_G = 5;
				continue;
			case 1:
				lI2 = 0;
				_G = 6;
				continue;
			case 6:
				if (((lI2 >= this._a.length))) {
					_G = 5;
					continue;
				}
				if ((((this._a.data[lI2]) != null))) {
					_G = 7;
					continue;
				}
				return lI2;
			case 7:
				lI2 = (((lI2 + 1))|0);
				_G = 6;
				continue;
			case 5:
				return -1;
			default:
				break;
		}
	}
	return 0;
};
java_util_Arrays$ArrayList.prototype["size()I"] = function() { 
	return this._a.length;
};
java_util_Arrays$ArrayList.prototype["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"] = function(p0) { 
	var _G = 0, lA1 = null, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = (p0);
				lI2 = this["size()I"]();
				if (((lI2 <= lA1.length))) {
					_G = 1;
					continue;
				}
				lA1 = (java_util_Arrays["access$000([Ljava/lang/Object;I)[Ljava/lang/Object;"]((lA1), lI2));
				_G = 1;
				continue;
			case 1:
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((this._a), 0, lA1, 0, lI2);
				if (((lI2 >= lA1.length))) {
					_G = 2;
					continue;
				}
				(lA1).data[lI2] = null;
				_G = 2;
				continue;
			case 2:
				return (lA1);
			default:
				break;
		}
	}
	return null;
};
java_util_Arrays$ArrayList.prototype["contains(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null, lA5 = null, lI3 = 0, lI4 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 == null))) {
					_G = 1;
					continue;
				}
				lA2 = (this._a);
				lI3 = lA2.length;
				lI4 = 0;
				_G = 2;
				continue;
			case 2:
				if (((lI4 >= lI3))) {
					_G = 3;
					continue;
				}
				lA5 = ((lA2).data[lI4]);
				if (!(p0["equals(Ljava/lang/Object;)Z"](lA5))) {
					_G = 4;
					continue;
				}
				return true;
			case 4:
				lI4 = (((lI4 + 1))|0);
				_G = 2;
				continue;
			case 3:
				_G = 5;
				continue;
			case 1:
				lA2 = (this._a);
				lI3 = lA2.length;
				lI4 = 0;
				_G = 6;
				continue;
			case 6:
				if (((lI4 >= lI3))) {
					_G = 5;
					continue;
				}
				lA5 = ((lA2).data[lI4]);
				if (((lA5 != null))) {
					_G = 7;
					continue;
				}
				return true;
			case 7:
				lI4 = (((lI4 + 1))|0);
				_G = 6;
				continue;
			case 5:
				return false;
			default:
				break;
		}
	}
	return false;
};
// ABSTRACT
function com_jtransc_charset_JTranscCharset() {
}
com_jtransc_charset_JTranscCharset.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_charset_JTranscCharset.prototype.constructor = com_jtransc_charset_JTranscCharset;
com_jtransc_charset_JTranscCharset.prototype._max = 0;
com_jtransc_charset_JTranscCharset.prototype._min = 0;
com_jtransc_charset_JTranscCharset.prototype._avg = 0.0;
com_jtransc_charset_JTranscCharset.prototype._names = null;
com_jtransc_charset_JTranscCharset.prototype.___id = 0;
com_jtransc_charset_JTranscCharset.SI = function() { 
	com_jtransc_charset_JTranscCharset._charsets = null;
	com_jtransc_charset_JTranscCharset._loadedCharsets = false;
	com_jtransc_charset_JTranscCharset["com.jtransc.charset.JTranscCharset<clinit>()V"]();
};
com_jtransc_charset_JTranscCharset.prototype.__JT__CLASS_ID = com_jtransc_charset_JTranscCharset.__JT__CLASS_ID = 804;
com_jtransc_charset_JTranscCharset.prototype.__JT__CLASS_IDS = com_jtransc_charset_JTranscCharset.__JT__CLASS_IDS = [804,656];
com_jtransc_charset_JTranscCharset.prototype["com.jtransc.charset.JTranscCharset<init>([Ljava/lang/String;IFI)V"] = function(p0, p1, p2, p3) { 
	(this)["java.lang.Object<init>()V"]();
	this._names = p0;
	this._min = p1;
	this._avg = p2;
	this._max = p3;
	return this;
	return this;
};
com_jtransc_charset_JTranscCharset["com.jtransc.charset.JTranscCharset<clinit>()V"] = function() { 
	var fA0 = null, tA0 = null;
	com_jtransc_charset_JTranscCharset._loadedCharsets = false;
	tA0 = ((new com_jtransc_ds_FastStringMap()));
	fA0 = tA0;
	(tA0)["com.jtransc.ds.FastStringMap<init>()V"]();
	com_jtransc_charset_JTranscCharset._charsets = (fA0);
	return;
};
com_jtransc_charset_JTranscCharset.prototype["encode(Ljava/lang/String;)[B"] = function(p0) { 
	var lA2 = null, fA0 = null, tA0 = null;
	tA0 = ((new java_io_ByteArrayOutputStream()));
	fA0 = tA0;
	(tA0)["java.io.ByteArrayOutputStream<init>(I)V"](((((Math.fround((Math.fround(+(p0["length()I"]())) * this["avgBytesPerCharacter()F"]())))|0))|0));
	lA2 = fA0;
	this["encode([CIILjava/io/ByteArrayOutputStream;)V"](p0["toCharArray()[C"](), 0, p0["length()I"](), (lA2));
	return (lA2)["toByteArray()[B"]();
};
com_jtransc_charset_JTranscCharset.prototype["decodeChars([BII)[C"] = function(p0, p1, p2) { 
	return this["decode([BII)Ljava/lang/String;"](p0, p1, p2)["toCharArray()[C"]();
};
com_jtransc_charset_JTranscCharset.prototype["decode([BII)Ljava/lang/String;"] = function(p0, p1, p2) { 
	var lA4 = null, fA0 = null, tA0 = null;
	tA0 = ((new com_jtransc_charset_JTranscCharBuffer()));
	fA0 = tA0;
	(tA0)["com.jtransc.charset.JTranscCharBuffer<init>(I)V"](((((Math.fround((Math.fround(+((p0).length)) / this["avgBytesPerCharacter()F"]())))|0))|0));
	lA4 = fA0;
	this["decode([BIILcom/jtransc/charset/JTranscCharBuffer;)V"](p0, p1, p2, (lA4));
	return (lA4)["toString()Ljava/lang/String;"]();
};
com_jtransc_charset_JTranscCharset.prototype["decode([BIILcom/jtransc/charset/JTranscCharBuffer;)V"] = function() { N.methodWithoutBody('com.jtransc.charset.JTranscCharset.decode') };
com_jtransc_charset_JTranscCharset.prototype["avgBytesPerCharacter()F"] = function() { 
	return this._avg;
};
com_jtransc_charset_JTranscCharset["forName(Ljava/lang/String;)Lcom/jtransc/charset/JTranscCharset;"] = function(p0) { 
	var _G = 0, lA1 = null, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				com_jtransc_charset_JTranscCharset["ensureRegister()V"]();
				lA1 = (N.checkCast(com_jtransc_charset_JTranscCharset._charsets["get(Ljava/lang/String;)Ljava/lang/Object;"](p0["toUpperCase()Ljava/lang/String;"]()["trim()Ljava/lang/String;"]()), com_jtransc_charset_JTranscCharset));
				if (((lA1 != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_nio_charset_UnsupportedCharsetException()));
				fA0 = tA0;
				(tA0)["java.nio.charset.UnsupportedCharsetException<init>(Ljava/lang/String;)V"](p0);
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				return (lA1);
			default:
				break;
		}
	}
	return null;
};
com_jtransc_charset_JTranscCharset["ensureRegister()V"] = function() { 
	var _G = 0, lA1 = null, lA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(com_jtransc_charset_JTranscCharset._loadedCharsets)) {
					_G = 1;
					continue;
				}
				return;
				_G = 1;
				continue;
			case 1:
				com_jtransc_charset_JTranscCharset._loadedCharsets = true;
				lA0 = java_util_ServiceLoader["load(Ljava/lang/Class;)Ljava/util/ServiceLoader;"](N.resolveClass("Lcom/jtransc/charset/JTranscCharset;"))["iterator()Ljava/util/Iterator;"]();
				_G = 2;
				continue;
			case 2:
				if (!(lA0["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				lA1 = N.checkCast(lA0["next()Ljava/lang/Object;"](), com_jtransc_charset_JTranscCharset);
				com_jtransc_charset_JTranscCharset["registerCharset(Lcom/jtransc/charset/JTranscCharset;)V"](lA1);
				_G = 2;
				continue;
			case 3:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_charset_JTranscCharset["registerCharset(Lcom/jtransc/charset/JTranscCharset;)V"] = function(p0) { 
	var _G = 0, lA1 = null, lA4 = null, lI2 = 0, lI3 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((com_jtransc_charset_JTranscCharset._charsets != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new com_jtransc_ds_FastStringMap()));
				fA0 = tA0;
				(tA0)["com.jtransc.ds.FastStringMap<init>()V"]();
				com_jtransc_charset_JTranscCharset._charsets = (fA0);
				_G = 1;
				continue;
			case 1:
				lA1 = (p0["getAliases()[Ljava/lang/String;"]());
				lI2 = lA1.length;
				lI3 = 0;
				_G = 2;
				continue;
			case 2:
				if (((lI3 >= lI2))) {
					_G = 3;
					continue;
				}
				lA4 = ((lA1).data[lI3]);
				com_jtransc_charset_JTranscCharset._charsets["set(Ljava/lang/String;Ljava/lang/Object;)V"]((lA4)["toUpperCase()Ljava/lang/String;"]()["trim()Ljava/lang/String;"](), (p0));
				lI3 = (((lI3 + 1))|0);
				_G = 2;
				continue;
			case 3:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_charset_JTranscCharset.prototype["getAliases()[Ljava/lang/String;"] = function() { 
	return this._names;
};
com_jtransc_charset_JTranscCharset.prototype["encode([CIILjava/io/ByteArrayOutputStream;)V"] = function() { N.methodWithoutBody('com.jtransc.charset.JTranscCharset.encode') };
// ABSTRACT
function com_jtransc_charset_charsets_JTranscCharsetUTF16Base() {
}
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype = Object.create(com_jtransc_charset_JTranscCharset.prototype);
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype.constructor = com_jtransc_charset_charsets_JTranscCharsetUTF16Base;
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype._littleEndian = false;
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype._max = 0;
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype._min = 0;
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype._avg = 0.0;
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype._names = null;
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.SI = function(){};
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype.__JT__CLASS_ID = com_jtransc_charset_charsets_JTranscCharsetUTF16Base.__JT__CLASS_ID = 831;
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype.__JT__CLASS_IDS = com_jtransc_charset_charsets_JTranscCharsetUTF16Base.__JT__CLASS_IDS = [831,804,656];
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype["com.jtransc.charset.charsets.JTranscCharsetUTF16Base<init>([Ljava/lang/String;Z)V"] = function(p0, p1) { 
	(this)["com.jtransc.charset.JTranscCharset<init>([Ljava/lang/String;IFI)V"](p0, 2, 2.0, 2);
	this._littleEndian = p1;
	return this;
	return this;
};
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype["decode([BIILcom/jtransc/charset/JTranscCharBuffer;)V"] = function(p0, p1, p2, p3) { 
	var _G = 0, lI5 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI5 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= p2))) {
					_G = 2;
					continue;
				}
				p3["append(C)V"](((com_jtransc_JTranscBits["readInt16([BIZ)S"](p0, (((p1 + lI5))|0), this._littleEndian))&0xFFFF));
				lI5 = (((lI5 + 2))|0);
				_G = 1;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype["encode([CIILjava/io/ByteArrayOutputStream;)V"] = function(p0, p1, p2, p3) { 
	var _G = 0, lI5 = 0, lI6 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI5 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= p2))) {
					_G = 2;
					continue;
				}
				lI6 = (((p0.data[(((p1 + lI5))|0)]))|0);
				if (!(this._littleEndian)) {
					_G = 3;
					continue;
				}
				p3["write(I)V"]((((lI6 & 255))|0));
				p3["write(I)V"](((((((lI6 >>> 8))|0) & 255))|0));
				_G = 4;
				continue;
			case 3:
				p3["write(I)V"](((((((lI6 >>> 8))|0) & 255))|0));
				p3["write(I)V"]((((lI6 & 255))|0));
				_G = 4;
				continue;
			case 4:
				lI5 = (((lI5 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
function com_jtransc_charset_charsets_JTranscCharsetUTF16BE() {
}
com_jtransc_charset_charsets_JTranscCharsetUTF16BE.prototype = Object.create(com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype);
com_jtransc_charset_charsets_JTranscCharsetUTF16BE.prototype.constructor = com_jtransc_charset_charsets_JTranscCharsetUTF16BE;
com_jtransc_charset_charsets_JTranscCharsetUTF16BE.prototype._littleEndian = false;
com_jtransc_charset_charsets_JTranscCharsetUTF16BE.SI = function(){};
com_jtransc_charset_charsets_JTranscCharsetUTF16BE.prototype.__JT__CLASS_ID = com_jtransc_charset_charsets_JTranscCharsetUTF16BE.__JT__CLASS_ID = 838;
com_jtransc_charset_charsets_JTranscCharsetUTF16BE.prototype.__JT__CLASS_IDS = com_jtransc_charset_charsets_JTranscCharsetUTF16BE.__JT__CLASS_IDS = [838,831,804,656];
com_jtransc_charset_charsets_JTranscCharsetUTF16BE.prototype["com.jtransc.charset.charsets.JTranscCharsetUTF16BE<init>()V"] = function() { 
	var fA1 = null, tA0 = null;
	tA0 = (new JA_L(4, "[Ljava.lang.String;"));
	fA1 = tA0;
	(tA0).data[0] = (S[70]);
	(fA1).setArraySlice(1, [(S[71]), (S[72]), (S[73])]);
	(this)["com.jtransc.charset.charsets.JTranscCharsetUTF16Base<init>([Ljava/lang/String;Z)V"]((fA1), false);
	return this;
	return this;
};
com_jtransc_charset_charsets_JTranscCharsetUTF16BE.prototype["decode([BIILcom/jtransc/charset/JTranscCharBuffer;)V"] = function(p0, p1, p2, p3) { 
	com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype["decode([BIILcom/jtransc/charset/JTranscCharBuffer;)V"].call((this), p0, p1, p2, p3);
	return;
};
com_jtransc_charset_charsets_JTranscCharsetUTF16BE.prototype["encode([CIILjava/io/ByteArrayOutputStream;)V"] = function(p0, p1, p2, p3) { 
	com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype["encode([CIILjava/io/ByteArrayOutputStream;)V"].call((this), p0, p1, p2, p3);
	return;
};
function com_jtransc_charset_JTranscCharsetSingleByte() {
}
com_jtransc_charset_JTranscCharsetSingleByte.prototype = Object.create(com_jtransc_charset_JTranscCharset.prototype);
com_jtransc_charset_JTranscCharsetSingleByte.prototype.constructor = com_jtransc_charset_JTranscCharsetSingleByte;
com_jtransc_charset_JTranscCharsetSingleByte.prototype._invalidChar = 63;
com_jtransc_charset_JTranscCharsetSingleByte.prototype._decode = null;
com_jtransc_charset_JTranscCharsetSingleByte.prototype._encode = null;
com_jtransc_charset_JTranscCharsetSingleByte.prototype._max = 0;
com_jtransc_charset_JTranscCharsetSingleByte.prototype._min = 0;
com_jtransc_charset_JTranscCharsetSingleByte.prototype._avg = 0.0;
com_jtransc_charset_JTranscCharsetSingleByte.prototype._names = null;
com_jtransc_charset_JTranscCharsetSingleByte.SI = function(){};
com_jtransc_charset_JTranscCharsetSingleByte.prototype.__JT__CLASS_ID = com_jtransc_charset_JTranscCharsetSingleByte.__JT__CLASS_ID = 810;
com_jtransc_charset_JTranscCharsetSingleByte.prototype.__JT__CLASS_IDS = com_jtransc_charset_JTranscCharsetSingleByte.__JT__CLASS_IDS = [810,804,656];
com_jtransc_charset_JTranscCharsetSingleByte.prototype["com.jtransc.charset.JTranscCharsetSingleByte<init>([Ljava/lang/String;Ljava/lang/String;)V"] = function(p0, p1) { 
	var _G = 0, lI3 = 0, fA1 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				(this)["com.jtransc.charset.JTranscCharset<init>([Ljava/lang/String;IFI)V"](p0, 1, 1.0, 1);
				this._invalidChar = 63;
				this._decode = p1;
				tA0 = ((new java_util_HashMap()));
				fA1 = tA0;
				(tA0)["java.util.HashMap<init>(I)V"](p1["length()I"]());
				this._encode = (fA1);
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= p1["length()I"]()))) {
					_G = 2;
					continue;
				}
				this._encode["put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"]((java_lang_Character["valueOf(C)Ljava/lang/Character;"](p1["charAt(I)C"](lI3))), (java_lang_Byte["valueOf(B)Ljava/lang/Byte;"](((lI3)<<24>>24))));
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
com_jtransc_charset_JTranscCharsetSingleByte.prototype["decode([BIILcom/jtransc/charset/JTranscCharBuffer;)V"] = function(p0, p1, p2, p3) { 
	var _G = 0, lI5 = 0, lI6 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI5 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= p2))) {
					_G = 2;
					continue;
				}
				lI6 = ((((p0.data[(((p1 + lI5))|0)]) & 255))|0);
				p3["append(C)V"](this._decode["charAt(I)C"](lI6));
				lI5 = (((lI5 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_charset_JTranscCharsetSingleByte.prototype["encode([CIILjava/io/ByteArrayOutputStream;)V"] = function(p0, p1, p2, p3) { 
	var _G = 0, lA7 = null, lI5 = 0, lI6 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI5 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= p2))) {
					_G = 2;
					continue;
				}
				lI6 = (((p0.data[(((p1 + lI5))|0)]))|0);
				lA7 = (N.checkCast(this._encode["get(Ljava/lang/Object;)Ljava/lang/Object;"]((java_lang_Character["valueOf(C)Ljava/lang/Character;"](((lI6)&0xFFFF)))), java_lang_Byte));
				if (((lA7 == null))) {
					_G = 3;
					continue;
				}
				p3["write(I)V"]((((lA7)["byteValue()B"]())|0));
				_G = 4;
				continue;
			case 3:
				p3["write(I)V"](63);
				_G = 4;
				continue;
			case 4:
				lI5 = (((lI5 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
function com_jtransc_charset_charsets_JTranscCharsetLatin1() {
}
com_jtransc_charset_charsets_JTranscCharsetLatin1.prototype = Object.create(com_jtransc_charset_JTranscCharsetSingleByte.prototype);
com_jtransc_charset_charsets_JTranscCharsetLatin1.prototype.constructor = com_jtransc_charset_charsets_JTranscCharsetLatin1;
com_jtransc_charset_charsets_JTranscCharsetLatin1.prototype._invalidChar = 63;
com_jtransc_charset_charsets_JTranscCharsetLatin1.prototype._decode = null;
com_jtransc_charset_charsets_JTranscCharsetLatin1.prototype._encode = null;
com_jtransc_charset_charsets_JTranscCharsetLatin1.SI = function(){};
com_jtransc_charset_charsets_JTranscCharsetLatin1.prototype.__JT__CLASS_ID = com_jtransc_charset_charsets_JTranscCharsetLatin1.__JT__CLASS_ID = 837;
com_jtransc_charset_charsets_JTranscCharsetLatin1.prototype.__JT__CLASS_IDS = com_jtransc_charset_charsets_JTranscCharsetLatin1.__JT__CLASS_IDS = [837,810,804,656];
com_jtransc_charset_charsets_JTranscCharsetLatin1.prototype["com.jtransc.charset.charsets.JTranscCharsetLatin1<init>()V"] = function() { 
	var fA1 = null, tA0 = null;
	tA0 = (new JA_L(15, "[Ljava.lang.String;"));
	fA1 = tA0;
	(tA0).data[0] = (S[74]);
	(fA1).setArraySlice(1, [(S[75]), (S[76]), (S[77]), (S[78]), (S[79]), (S[80]), (S[81]), (S[82]), (S[83]), (S[84]), (S[85]), (S[86]), (S[87]), (S[88])]);
	(this)["com.jtransc.charset.JTranscCharsetSingleByte<init>([Ljava/lang/String;Ljava/lang/String;)V"]((fA1), S[89]);
	return this;
	return this;
};
function com_jtransc_mix_JTranscProcessMulti$Creator() {
}
com_jtransc_mix_JTranscProcessMulti$Creator.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_mix_JTranscProcessMulti$Creator.prototype.constructor = com_jtransc_mix_JTranscProcessMulti$Creator;
com_jtransc_mix_JTranscProcessMulti$Creator.prototype.___id = 0;
com_jtransc_mix_JTranscProcessMulti$Creator.SI = function(){};
com_jtransc_mix_JTranscProcessMulti$Creator.prototype.__JT__CLASS_ID = com_jtransc_mix_JTranscProcessMulti$Creator.__JT__CLASS_ID = 836;
com_jtransc_mix_JTranscProcessMulti$Creator.prototype.__JT__CLASS_IDS = com_jtransc_mix_JTranscProcessMulti$Creator.__JT__CLASS_IDS = [836,656];
com_jtransc_mix_JTranscProcessMulti$Creator.prototype["com.jtransc.mix.JTranscProcessMulti$Creator<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
// ABSTRACT
function java_lang_Process() {
}
java_lang_Process.prototype = Object.create(java_lang_Object.prototype);
java_lang_Process.prototype.constructor = java_lang_Process;
java_lang_Process.prototype.___id = 0;
java_lang_Process.SI = function(){};
java_lang_Process.prototype.__JT__CLASS_ID = java_lang_Process.__JT__CLASS_ID = 835;
java_lang_Process.prototype.__JT__CLASS_IDS = java_lang_Process.__JT__CLASS_IDS = [835,656];
java_lang_Process.prototype["java.lang.Process<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
// ABSTRACT
function com_jtransc_JTranscProcess() {
}
com_jtransc_JTranscProcess.prototype = Object.create(java_lang_Process.prototype);
com_jtransc_JTranscProcess.prototype.constructor = com_jtransc_JTranscProcess;
com_jtransc_JTranscProcess.SI = function(){};
com_jtransc_JTranscProcess.prototype.__JT__CLASS_ID = com_jtransc_JTranscProcess.__JT__CLASS_ID = 834;
com_jtransc_JTranscProcess.prototype.__JT__CLASS_IDS = com_jtransc_JTranscProcess.__JT__CLASS_IDS = [834,835,656];
com_jtransc_JTranscProcess.prototype["com.jtransc.JTranscProcess<init>()V"] = function() { 
	(this)["java.lang.Process<init>()V"]();
	return this;
	return this;
};
function com_jtransc_mix_JTranscProcessMulti() {
}
com_jtransc_mix_JTranscProcessMulti.prototype = Object.create(com_jtransc_JTranscProcess.prototype);
com_jtransc_mix_JTranscProcessMulti.prototype.constructor = com_jtransc_mix_JTranscProcessMulti;
com_jtransc_mix_JTranscProcessMulti.SI = function() { 
	com_jtransc_mix_JTranscProcessMulti._creator = null;
	com_jtransc_mix_JTranscProcessMulti["com.jtransc.mix.JTranscProcessMulti<clinit>()V"]();
};
com_jtransc_mix_JTranscProcessMulti.prototype.__JT__CLASS_ID = com_jtransc_mix_JTranscProcessMulti.__JT__CLASS_ID = 833;
com_jtransc_mix_JTranscProcessMulti.prototype.__JT__CLASS_IDS = com_jtransc_mix_JTranscProcessMulti.__JT__CLASS_IDS = [833,834,835,656];
com_jtransc_mix_JTranscProcessMulti.prototype["com.jtransc.mix.JTranscProcessMulti<init>()V"] = function() { 
	(this)["com.jtransc.JTranscProcess<init>()V"]();
	return this;
	return this;
};
com_jtransc_mix_JTranscProcessMulti["com.jtransc.mix.JTranscProcessMulti<clinit>()V"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new com_jtransc_mix_JTranscProcessMulti$Creator()));
	fA0 = tA0;
	(tA0)["com.jtransc.mix.JTranscProcessMulti$Creator<init>()V"]();
	com_jtransc_mix_JTranscProcessMulti._creator = (fA0);
	return;
};
function com_jtransc_JTranscBits() {
}
com_jtransc_JTranscBits.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_JTranscBits.prototype.constructor = com_jtransc_JTranscBits;
com_jtransc_JTranscBits.prototype.___id = 0;
com_jtransc_JTranscBits.SI = function(){};
com_jtransc_JTranscBits.prototype.__JT__CLASS_ID = com_jtransc_JTranscBits.__JT__CLASS_ID = 832;
com_jtransc_JTranscBits.prototype.__JT__CLASS_IDS = com_jtransc_JTranscBits.__JT__CLASS_IDS = [832,656];
com_jtransc_JTranscBits.prototype["com.jtransc.JTranscBits<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_JTranscBits["readInt16([BIZ)S"] = function(p0, p1, p2) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p2)) {
					_G = 1;
					continue;
				}
				fI0 = ((com_jtransc_JTranscBits["readInt16LE([BI)S"](p0, p1))|0);
				_G = 2;
				continue;
			case 1:
				fI0 = ((com_jtransc_JTranscBits["readInt16BE([BI)S"](p0, p1))|0);
				_G = 2;
				continue;
			case 2:return ((fI0)<<16>>16); 
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_JTranscBits["readInt16BE([BI)S"] = function(p0, p1) { 
	return ((((((((((((p0.data[(((p1 + 0))|0)]) & 255))|0) << 8))|0) | (((((((p0.data[(((p1 + 1))|0)]) & 255))|0) << 0))|0)))|0))<<16>>16);
};
com_jtransc_JTranscBits["readInt16LE([BI)S"] = function(p0, p1) { 
	return ((((((((((((p0.data[(((p1 + 1))|0)]) & 255))|0) << 8))|0) | (((((((p0.data[(((p1 + 0))|0)]) & 255))|0) << 0))|0)))|0))<<16>>16);
};
com_jtransc_JTranscBits["isLittleEndian()Z"] = function() { 
	return N.isLittleEndian;
};
com_jtransc_JTranscBits["reverseBytes(J)J"] = function(p0) { 
	return java_lang_Long["reverseBytes(J)J"](p0);
};
com_jtransc_JTranscBits["writeLongLE([BIJ)V"] = function(p0, p1, p2) { 
	var lI4 = 0, lI5 = 0;
	lI4 = N.j2i(N.lshr(p2, 32));
	lI5 = N.j2i(N.lshr(p2, 0));
	p0.data[(((p1 + 7))|0)] = (((((lI4 >>> 24))|0))<<24>>24);
	p0.data[(((p1 + 6))|0)] = (((((lI4 >>> 16))|0))<<24>>24);
	p0.data[(((p1 + 5))|0)] = (((((lI4 >>> 8))|0))<<24>>24);
	p0.data[(((p1 + 4))|0)] = (((((lI4 >>> 0))|0))<<24>>24);
	p0.data[(((p1 + 3))|0)] = (((((lI5 >>> 24))|0))<<24>>24);
	p0.data[(((p1 + 2))|0)] = (((((lI5 >>> 16))|0))<<24>>24);
	p0.data[(((p1 + 1))|0)] = (((((lI5 >>> 8))|0))<<24>>24);
	p0.data[(((p1 + 0))|0)] = (((((lI5 >>> 0))|0))<<24>>24);
	return;
};
function com_jtransc_charset_charsets_JTranscCharsetUTF16LE() {
}
com_jtransc_charset_charsets_JTranscCharsetUTF16LE.prototype = Object.create(com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype);
com_jtransc_charset_charsets_JTranscCharsetUTF16LE.prototype.constructor = com_jtransc_charset_charsets_JTranscCharsetUTF16LE;
com_jtransc_charset_charsets_JTranscCharsetUTF16LE.prototype._littleEndian = false;
com_jtransc_charset_charsets_JTranscCharsetUTF16LE.SI = function(){};
com_jtransc_charset_charsets_JTranscCharsetUTF16LE.prototype.__JT__CLASS_ID = com_jtransc_charset_charsets_JTranscCharsetUTF16LE.__JT__CLASS_ID = 830;
com_jtransc_charset_charsets_JTranscCharsetUTF16LE.prototype.__JT__CLASS_IDS = com_jtransc_charset_charsets_JTranscCharsetUTF16LE.__JT__CLASS_IDS = [830,831,804,656];
com_jtransc_charset_charsets_JTranscCharsetUTF16LE.prototype["com.jtransc.charset.charsets.JTranscCharsetUTF16LE<init>()V"] = function() { 
	var fA1 = null, tA0 = null;
	tA0 = (new JA_L(4, "[Ljava.lang.String;"));
	fA1 = tA0;
	(tA0).data[0] = (S[90]);
	(fA1).setArraySlice(1, [(S[91]), (S[92]), (S[93])]);
	(this)["com.jtransc.charset.charsets.JTranscCharsetUTF16Base<init>([Ljava/lang/String;Z)V"]((fA1), false);
	return this;
	return this;
};
com_jtransc_charset_charsets_JTranscCharsetUTF16LE.prototype["decode([BIILcom/jtransc/charset/JTranscCharBuffer;)V"] = function(p0, p1, p2, p3) { 
	com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype["decode([BIILcom/jtransc/charset/JTranscCharBuffer;)V"].call((this), p0, p1, p2, p3);
	return;
};
com_jtransc_charset_charsets_JTranscCharsetUTF16LE.prototype["encode([CIILjava/io/ByteArrayOutputStream;)V"] = function(p0, p1, p2, p3) { 
	com_jtransc_charset_charsets_JTranscCharsetUTF16Base.prototype["encode([CIILjava/io/ByteArrayOutputStream;)V"].call((this), p0, p1, p2, p3);
	return;
};
function com_jtransc_charset_charsets_JTranscCharsetUSASCII() {
}
com_jtransc_charset_charsets_JTranscCharsetUSASCII.prototype = Object.create(com_jtransc_charset_JTranscCharsetSingleByte.prototype);
com_jtransc_charset_charsets_JTranscCharsetUSASCII.prototype.constructor = com_jtransc_charset_charsets_JTranscCharsetUSASCII;
com_jtransc_charset_charsets_JTranscCharsetUSASCII.prototype._invalidChar = 63;
com_jtransc_charset_charsets_JTranscCharsetUSASCII.prototype._decode = null;
com_jtransc_charset_charsets_JTranscCharsetUSASCII.prototype._encode = null;
com_jtransc_charset_charsets_JTranscCharsetUSASCII.SI = function(){};
com_jtransc_charset_charsets_JTranscCharsetUSASCII.prototype.__JT__CLASS_ID = com_jtransc_charset_charsets_JTranscCharsetUSASCII.__JT__CLASS_ID = 829;
com_jtransc_charset_charsets_JTranscCharsetUSASCII.prototype.__JT__CLASS_IDS = com_jtransc_charset_charsets_JTranscCharsetUSASCII.__JT__CLASS_IDS = [829,810,804,656];
com_jtransc_charset_charsets_JTranscCharsetUSASCII.prototype["com.jtransc.charset.charsets.JTranscCharsetUSASCII<init>()V"] = function() { 
	var fA1 = null, tA0 = null;
	tA0 = (new JA_L(15, "[Ljava.lang.String;"));
	fA1 = tA0;
	(tA0).data[0] = (S[94]);
	(fA1).setArraySlice(1, [(S[95]), (S[96]), (S[97]), (S[98]), (S[99]), (S[100]), (S[101]), (S[102]), (S[103]), (S[104]), (S[105]), (S[106]), (S[107]), (S[56])]);
	(this)["com.jtransc.charset.JTranscCharsetSingleByte<init>([Ljava/lang/String;Ljava/lang/String;)V"]((fA1), S[108]);
	return this;
	return this;
};
function com_jtransc_charset_charsets_JTranscCharsetUTF8() {
}
com_jtransc_charset_charsets_JTranscCharsetUTF8.prototype = Object.create(com_jtransc_charset_JTranscCharset.prototype);
com_jtransc_charset_charsets_JTranscCharsetUTF8.prototype.constructor = com_jtransc_charset_charsets_JTranscCharsetUTF8;
com_jtransc_charset_charsets_JTranscCharsetUTF8.prototype._max = 0;
com_jtransc_charset_charsets_JTranscCharsetUTF8.prototype._min = 0;
com_jtransc_charset_charsets_JTranscCharsetUTF8.prototype._avg = 0.0;
com_jtransc_charset_charsets_JTranscCharsetUTF8.prototype._names = null;
com_jtransc_charset_charsets_JTranscCharsetUTF8.SI = function(){};
com_jtransc_charset_charsets_JTranscCharsetUTF8.prototype.__JT__CLASS_ID = com_jtransc_charset_charsets_JTranscCharsetUTF8.__JT__CLASS_ID = 828;
com_jtransc_charset_charsets_JTranscCharsetUTF8.prototype.__JT__CLASS_IDS = com_jtransc_charset_charsets_JTranscCharsetUTF8.__JT__CLASS_IDS = [828,804,656];
com_jtransc_charset_charsets_JTranscCharsetUTF8.prototype["com.jtransc.charset.charsets.JTranscCharsetUTF8<init>()V"] = function() { 
	var fA1 = null, tA0 = null;
	tA0 = (new JA_L(2, "[Ljava.lang.String;"));
	fA1 = tA0;
	(tA0).data[0] = (S[51]);
	(fA1).data[1] = (S[109]);
	(this)["com.jtransc.charset.JTranscCharset<init>([Ljava/lang/String;IFI)V"]((fA1), 1, 1.2, 4);
	return this;
	return this;
};
com_jtransc_charset_charsets_JTranscCharsetUTF8.prototype["decode([BIILcom/jtransc/charset/JTranscCharBuffer;)V"] = function(p0, p1, p2, p3) { 
	var _G = 0, fA2 = null, fI1 = 0, fI3 = 0, lI5 = 0, lI6 = 0, lI7 = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI5 = p1;
				lI6 = (((p1 + p2))|0);
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= lI6))) {
					_G = 2;
					continue;
				}
				fA0 = (p0);
				fI1 = lI5;
				lI5 = (((lI5 + 1))|0);
				lI7 = (((((fA0).data[fI1]) & 255))|0);
				switch ((((lI7 >> 4))|0)) {
					case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7:
						_G = 4;
						continue;
					case 8: case 9: case 10: case 11:
						_G = 3;
						continue;
					case 12: case 13:
						_G = 5;
						continue;
					case 14:
						_G = 6;
						continue;
					default:
						_G = 3;
						continue;
				}
				_G = 4;
				continue;
			case 4:
				p3["append(C)V"](((lI7)&0xFFFF));
				_G = 3;
				continue;
			case 5:
				fA0 = (p3);
				fI1 = ((((((lI7 & 31))|0) << 6))|0);
				fA2 = p0;
				fI3 = lI5;
				lI5 = (((lI5 + 1))|0);
				(fA0)["append(C)V"]((((((fI1 | ((((fA2.data[fI3]) & 63))|0)))|0))&0xFFFF));
				_G = 3;
				continue;
			case 6:
				fA0 = (p3);
				fI1 = ((((((lI7 & 15))|0) << 12))|0);
				fA2 = p0;
				fI3 = lI5;
				lI5 = (((lI5 + 1))|0);
				fI1 = (((fI1 | (((((((fA2.data[fI3]) & 63))|0) << 6))|0)))|0);
				fA2 = p0;
				fI3 = lI5;
				lI5 = (((lI5 + 1))|0);
				(fA0)["append(C)V"]((((((fI1 | ((((fA2.data[fI3]) & 63))|0)))|0))&0xFFFF));
				_G = 3;
				continue;
			case 3:
				_G = 1;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_charset_charsets_JTranscCharsetUTF8.prototype["encode([CIILjava/io/ByteArrayOutputStream;)V"] = function(p0, p1, p2, p3) { 
	var _G = 0, lI5 = 0, lI6 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI5 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= p2))) {
					_G = 2;
					continue;
				}
				lI6 = (((p0.data[(((p1 + lI5))|0)]))|0);
				if ((((((lI6 & -128))|0) != 0))) {
					_G = 3;
					continue;
				}
				p3["write(I)V"](lI6);
				_G = 4;
				continue;
			case 3:
				if ((((((lI6 & -2048))|0) != 0))) {
					_G = 5;
					continue;
				}
				p3["write(I)V"]((((((((((lI6 >> 6))|0) & 31))|0) | 192))|0));
				_G = 6;
				continue;
			case 5:
				if ((((((lI6 & -65536))|0) != 0))) {
					_G = 7;
					continue;
				}
				p3["write(I)V"]((((((((((lI6 >> 12))|0) & 15))|0) | 224))|0));
				p3["write(I)V"](com_jtransc_charset_charsets_JTranscCharsetUTF8["createByte(II)I"](lI6, 6));
				_G = 6;
				continue;
			case 7:
				if ((((((lI6 & -2097152))|0) != 0))) {
					_G = 6;
					continue;
				}
				p3["write(I)V"]((((((((((lI6 >> 18))|0) & 7))|0) | 240))|0));
				p3["write(I)V"](com_jtransc_charset_charsets_JTranscCharsetUTF8["createByte(II)I"](lI6, 12));
				p3["write(I)V"](com_jtransc_charset_charsets_JTranscCharsetUTF8["createByte(II)I"](lI6, 6));
				_G = 6;
				continue;
			case 6:
				p3["write(I)V"](((((((lI6 & 63))|0) | 128))|0));
				_G = 4;
				continue;
			case 4:
				lI5 = (((lI5 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_charset_charsets_JTranscCharsetUTF8["createByte(II)I"] = function(p0, p1) { 
	return (((((((((p0 >> p1))|0) & 63))|0) | 128))|0);
};
function java_util_Collections$EmptySet() {
}
java_util_Collections$EmptySet.prototype = Object.create(java_util_AbstractSet.prototype);
java_util_Collections$EmptySet.prototype.constructor = java_util_Collections$EmptySet;
java_util_Collections$EmptySet.SI = function(){};
java_util_Collections$EmptySet.prototype.__JT__CLASS_ID = java_util_Collections$EmptySet.__JT__CLASS_ID = 827;
java_util_Collections$EmptySet.prototype.__JT__CLASS_IDS = java_util_Collections$EmptySet.__JT__CLASS_IDS = [827,816,728,656,658,747,723,724];
java_util_Collections$EmptySet.prototype["java.util.Collections$EmptySet<init>(Ljava/util/Collections$1;)V"] = function(p0) { 
	this["java.util.Collections$EmptySet<init>()V"]();
	return this;
	return this;
};
java_util_Collections$EmptySet.prototype["java.util.Collections$EmptySet<init>()V"] = function() { 
	(this)["java.util.AbstractSet<init>()V"]();
	return this;
	return this;
};
java_util_Collections$EmptySet.prototype["size()I"] = function() { 
	return 0;
};
java_util_Collections$EmptySet.prototype["iterator()Ljava/util/Iterator;"] = function() { 
	return java_util_Collections["access$000()Ljava/util/Iterator;"]();
};
java_util_Collections$EmptySet.prototype["contains(Ljava/lang/Object;)Z"] = function(p0) { 
	return false;
};
// ABSTRACT
function java_util_AbstractMap() {
}
java_util_AbstractMap.prototype = Object.create(java_lang_Object.prototype);
java_util_AbstractMap.prototype.constructor = java_util_AbstractMap;
java_util_AbstractMap.prototype._valuesCollection = null;
java_util_AbstractMap.prototype._keySet = null;
java_util_AbstractMap.prototype.___id = 0;
java_util_AbstractMap.SI = function(){};
java_util_AbstractMap.prototype.__JT__CLASS_ID = java_util_AbstractMap.__JT__CLASS_ID = 812;
java_util_AbstractMap.prototype.__JT__CLASS_IDS = java_util_AbstractMap.__JT__CLASS_IDS = [812,656,746];
java_util_AbstractMap.prototype["java.util.AbstractMap<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_util_AbstractMap.prototype["toString()Ljava/lang/String;"] = function() { 
	var _G = 0, lA1 = null, lA4 = null, lA5 = null, lA3 = null, fA0 = null, tA0 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(this["isEmpty()Z"]())) {
					_G = 1;
					continue;
				}
				return S[110];
			case 1:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>(I)V"](((Math.imul(this["size()I"](), 28))|0));
				lA1 = fA0;
				(lA1)["append(C)Ljava/lang/StringBuilder;"](123);
				lA2 = this["entrySet()Ljava/util/Set;"]()["iterator()Ljava/util/Iterator;"]();
				_G = 2;
				continue;
			case 2:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				lA3 = N.checkCast(lA2["next()Ljava/lang/Object;"](), java_util_Map$Entry);
				lA4 = lA3["getKey()Ljava/lang/Object;"]();
				if (((lA4 == (this)))) {
					_G = 4;
					continue;
				}
				(lA1)["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"](lA4);
				_G = 5;
				continue;
			case 4:
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[12]);
				_G = 5;
				continue;
			case 5:
				(lA1)["append(C)Ljava/lang/StringBuilder;"](61);
				lA5 = lA3["getValue()Ljava/lang/Object;"]();
				if (((lA5 == (this)))) {
					_G = 6;
					continue;
				}
				(lA1)["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"](lA5);
				_G = 7;
				continue;
			case 6:
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[12]);
				_G = 7;
				continue;
			case 7:
				if (!(lA2["hasNext()Z"]())) {
					_G = 8;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[6]);
				_G = 8;
				continue;
			case 8:
				_G = 2;
				continue;
			case 3:
				(lA1)["append(C)Ljava/lang/StringBuilder;"](125);
				return (lA1)["toString()Ljava/lang/String;"]();
			default:
				break;
		}
	}
	return null;
};
java_util_AbstractMap.prototype["entrySet()Ljava/util/Set;"] = function() { N.methodWithoutBody('java.util.AbstractMap.entrySet') };
java_util_AbstractMap.prototype["isEmpty()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this["size()I"]() != 0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_util_AbstractMap.prototype["size()I"] = function() { 
	return this["entrySet()Ljava/util/Set;"]()["size()I"]();
};
java_util_AbstractMap.prototype["clone()Ljava/lang/Object;"] = function() { 
	var lA1 = null;
	lA1 = (N.checkCast(java_lang_Object.prototype["clone()Ljava/lang/Object;"].call((this)), java_util_AbstractMap));
	(lA1)._keySet = null;
	(lA1)._valuesCollection = null;
	return lA1;
};
java_util_AbstractMap.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA3 = null, lA5 = null, lA6 = null, lA7 = null, fI0 = 0, lA4 = null, fA0 = null, lA2 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						if ((((this) != p0))) {
							_G = 1;
							continue;
						}
						return true;
					case 1:
						if (!(N.isClassId(p0, 746))) {
							_G = 2;
							continue;
						}
						lA2 = N.checkCast(p0, java_util_Map);
						if (((this["size()I"]() == lA2["size()I"]()))) {
							_G = 3;
							continue;
						}
						return false;
					case 3:
						lA3 = (this["entrySet()Ljava/util/Set;"]()["iterator()Ljava/util/Iterator;"]());
						_G = 4;
						continue;
					case 4:
						if (!((lA3)["hasNext()Z"]())) {
							_G = 5;
							continue;
						}
						lA4 = N.checkCast((lA3)["next()Ljava/lang/Object;"](), java_util_Map$Entry);
						lA5 = lA4["getKey()Ljava/lang/Object;"]();
						lA6 = lA4["getValue()Ljava/lang/Object;"]();
						lA7 = lA2["get(Ljava/lang/Object;)Ljava/lang/Object;"](lA5);
						if (((lA6 != null))) {
							_G = 6;
							continue;
						}
						if (((lA7 != null))) {
							_G = 7;
							continue;
						}
						if (lA2["containsKey(Ljava/lang/Object;)Z"](lA5)) {
							_G = 8;
							continue;
						}
						_G = 7;
						continue;
					case 7:
						fI0 = 0;
						_G = 9;
						continue;
					case 9:return ((fI0)!=0); 
					case 6:
						if (lA6["equals(Ljava/lang/Object;)Z"](lA7)) {
							_G = 8;
							continue;
						}
						fI0 = 0;
						_G = 10;
						continue;
					case 10:return ((fI0)!=0); 
					case 8:
						_G = 4;
						continue;
					case 5:
						_G = 11;
						continue;
					case 12:
						fA0 = (J__exception__);
						lA3 = fA0;
						return false;
					case 13:
						fA0 = (J__exception__);
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
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 3)) && ((_G < 9)))) && (J__exception__ instanceof java_lang_NullPointerException)))) {
				_G = 12;
				continue;
			}
			if (((((((_G >= 6)) && ((_G < 10)))) && (J__exception__ instanceof java_lang_NullPointerException)))) {
				_G = 12;
				continue;
			}
			if (((((((_G >= 8)) && ((_G < 5)))) && (J__exception__ instanceof java_lang_NullPointerException)))) {
				_G = 12;
				continue;
			}
			if (((((((_G >= 3)) && ((_G < 9)))) && (J__exception__ instanceof java_lang_ClassCastException)))) {
				_G = 13;
				continue;
			}
			if (((((((_G >= 6)) && ((_G < 10)))) && (J__exception__ instanceof java_lang_ClassCastException)))) {
				_G = 13;
				continue;
			}
			if (((((((_G >= 8)) && ((_G < 5)))) && (J__exception__ instanceof java_lang_ClassCastException)))) {
				_G = 13;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return false;
};
java_util_AbstractMap.prototype["get(Ljava/lang/Object;)Ljava/lang/Object;"] = function(p0) { 
	var _G = 0, lA3 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA2 = this["entrySet()Ljava/util/Set;"]()["iterator()Ljava/util/Iterator;"]();
				if (((p0 == null))) {
					_G = 1;
					continue;
				}
				_G = 2;
				continue;
			case 2:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				lA3 = N.checkCast(lA2["next()Ljava/lang/Object;"](), java_util_Map$Entry);
				if (!(p0["equals(Ljava/lang/Object;)Z"](lA3["getKey()Ljava/lang/Object;"]()))) {
					_G = 4;
					continue;
				}
				return lA3["getValue()Ljava/lang/Object;"]();
			case 4:
				_G = 2;
				continue;
			case 1:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				lA3 = N.checkCast(lA2["next()Ljava/lang/Object;"](), java_util_Map$Entry);
				if (((lA3["getKey()Ljava/lang/Object;"]() != null))) {
					_G = 5;
					continue;
				}
				return lA3["getValue()Ljava/lang/Object;"]();
			case 5:
				_G = 1;
				continue;
			case 3:
				return null;
			default:
				break;
		}
	}
	return null;
};
java_util_AbstractMap.prototype["containsKey(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA2 = this["entrySet()Ljava/util/Set;"]()["iterator()Ljava/util/Iterator;"]();
				if (((p0 == null))) {
					_G = 1;
					continue;
				}
				_G = 2;
				continue;
			case 2:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"](N.checkCast(lA2["next()Ljava/lang/Object;"](), java_util_Map$Entry)["getKey()Ljava/lang/Object;"]()))) {
					_G = 2;
					continue;
				}
				return true;
			case 1:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				if (((N.checkCast(lA2["next()Ljava/lang/Object;"](), java_util_Map$Entry)["getKey()Ljava/lang/Object;"]() != null))) {
					_G = 1;
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
};
java_util_AbstractMap.prototype["hashCode()I"] = function() { 
	var _G = 0, lI1 = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				lA2 = this["entrySet()Ljava/util/Set;"]()["iterator()Ljava/util/Iterator;"]();
				_G = 1;
				continue;
			case 1:
				if (!(lA2["hasNext()Z"]())) {
					_G = 2;
					continue;
				}
				lI1 = (((lI1 + N.checkCast(lA2["next()Ljava/lang/Object;"](), java_util_Map$Entry)["hashCode()I"]()))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
java_util_AbstractMap.prototype["put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"] = function(p0, p1) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_UnsupportedOperationException()));
	fA0 = tA0;
	(tA0)["java.lang.UnsupportedOperationException<init>()V"]();
	throw new WrappedError(fA0);
};
function java_util_Collections$EmptyMap() {
}
java_util_Collections$EmptyMap.prototype = Object.create(java_util_AbstractMap.prototype);
java_util_Collections$EmptyMap.prototype.constructor = java_util_Collections$EmptyMap;
java_util_Collections$EmptyMap.prototype._valuesCollection = null;
java_util_Collections$EmptyMap.prototype._keySet = null;
java_util_Collections$EmptyMap.SI = function(){};
java_util_Collections$EmptyMap.prototype.__JT__CLASS_ID = java_util_Collections$EmptyMap.__JT__CLASS_ID = 826;
java_util_Collections$EmptyMap.prototype.__JT__CLASS_IDS = java_util_Collections$EmptyMap.__JT__CLASS_IDS = [826,812,656,658,746];
java_util_Collections$EmptyMap.prototype["java.util.Collections$EmptyMap<init>(Ljava/util/Collections$1;)V"] = function(p0) { 
	this["java.util.Collections$EmptyMap<init>()V"]();
	return this;
	return this;
};
java_util_Collections$EmptyMap.prototype["java.util.Collections$EmptyMap<init>()V"] = function() { 
	(this)["java.util.AbstractMap<init>()V"]();
	return this;
	return this;
};
java_util_Collections$EmptyMap.prototype["entrySet()Ljava/util/Set;"] = function() { 
	return java_util_Collections._EMPTY_SET;
};
java_util_Collections$EmptyMap.prototype["get(Ljava/lang/Object;)Ljava/lang/Object;"] = function(p0) { 
	return null;
};
java_util_Collections$EmptyMap.prototype["containsKey(Ljava/lang/Object;)Z"] = function(p0) { 
	return false;
};
function java_util_Enumeration() {
}
java_util_Enumeration.prototype = Object.create(java_lang_Object_base.prototype);
java_util_Enumeration.prototype.constructor = java_util_Enumeration;
java_util_Enumeration.SI = function(){};
java_util_Enumeration.prototype.__JT__CLASS_ID = java_util_Enumeration.__JT__CLASS_ID = 825;
java_util_Enumeration.prototype.__JT__CLASS_IDS = java_util_Enumeration.__JT__CLASS_IDS = [825,656];
function java_util_Collections$2() {
}
java_util_Collections$2.prototype = Object.create(java_lang_Object.prototype);
java_util_Collections$2.prototype.constructor = java_util_Collections$2;
java_util_Collections$2.prototype.___id = 0;
java_util_Collections$2.SI = function(){};
java_util_Collections$2.prototype.__JT__CLASS_ID = java_util_Collections$2.__JT__CLASS_ID = 824;
java_util_Collections$2.prototype.__JT__CLASS_IDS = java_util_Collections$2.__JT__CLASS_IDS = [824,656,825];
java_util_Collections$2.prototype["java.util.Collections$2<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
function java_util_Collections$EmptyList() {
}
java_util_Collections$EmptyList.prototype = Object.create(java_util_AbstractList.prototype);
java_util_Collections$EmptyList.prototype.constructor = java_util_Collections$EmptyList;
java_util_Collections$EmptyList.prototype._modCount = 0;
java_util_Collections$EmptyList.SI = function(){};
java_util_Collections$EmptyList.prototype.__JT__CLASS_ID = java_util_Collections$EmptyList.__JT__CLASS_ID = 823;
java_util_Collections$EmptyList.prototype.__JT__CLASS_IDS = java_util_Collections$EmptyList.__JT__CLASS_IDS = [823,727,728,656,725,658,722,723,724];
java_util_Collections$EmptyList.prototype["java.util.Collections$EmptyList<init>(Ljava/util/Collections$1;)V"] = function(p0) { 
	this["java.util.Collections$EmptyList<init>()V"]();
	return this;
	return this;
};
java_util_Collections$EmptyList.prototype["java.util.Collections$EmptyList<init>()V"] = function() { 
	(this)["java.util.AbstractList<init>()V"]();
	return this;
	return this;
};
java_util_Collections$EmptyList.prototype["get(I)Ljava/lang/Object;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_IndexOutOfBoundsException()));
	fA0 = tA0;
	(tA0)["java.lang.IndexOutOfBoundsException<init>()V"]();
	throw new WrappedError(fA0);
};
java_util_Collections$EmptyList.prototype["size()I"] = function() { 
	return 0;
};
java_util_Collections$EmptyList.prototype["contains(Ljava/lang/Object;)Z"] = function(p0) { 
	return false;
};
function java_util_Collections$1() {
}
java_util_Collections$1.prototype = Object.create(java_lang_Object.prototype);
java_util_Collections$1.prototype.constructor = java_util_Collections$1;
java_util_Collections$1.prototype.___id = 0;
java_util_Collections$1.SI = function(){};
java_util_Collections$1.prototype.__JT__CLASS_ID = java_util_Collections$1.__JT__CLASS_ID = 822;
java_util_Collections$1.prototype.__JT__CLASS_IDS = java_util_Collections$1.__JT__CLASS_IDS = [822,656,729];
java_util_Collections$1.prototype["java.util.Collections$1<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_util_Collections$1.prototype["next()Ljava/lang/Object;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_util_NoSuchElementException()));
	fA0 = tA0;
	(tA0)["java.util.NoSuchElementException<init>()V"]();
	throw new WrappedError(fA0);
};
java_util_Collections$1.prototype["hasNext()Z"] = function() { 
	return false;
};
function java_util_Collections() {
}
java_util_Collections.prototype = Object.create(java_lang_Object.prototype);
java_util_Collections.prototype.constructor = java_util_Collections;
java_util_Collections.prototype.___id = 0;
java_util_Collections.SI = function() { 
	java_util_Collections._EMPTY_SET = null;
	java_util_Collections._EMPTY_ITERATOR = null;
	java_util_Collections._EMPTY_ENUMERATION = null;
	java_util_Collections._EMPTY_MAP = null;
	java_util_Collections._EMPTY_LIST = null;
	java_util_Collections["java.util.Collections<clinit>()V"]();
};
java_util_Collections.prototype.__JT__CLASS_ID = java_util_Collections.__JT__CLASS_ID = 821;
java_util_Collections.prototype.__JT__CLASS_IDS = java_util_Collections.__JT__CLASS_IDS = [821,656];
java_util_Collections.prototype["java.util.Collections<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_util_Collections["java.util.Collections<clinit>()V"] = function() { 
	var fA0 = null, tA0 = null, tA1 = null, tA2 = null, tA3 = null, tA4 = null;
	tA0 = ((new java_util_Collections$1()));
	fA0 = tA0;
	(tA0)["java.util.Collections$1<init>()V"]();
	java_util_Collections._EMPTY_ITERATOR = (fA0);
	tA1 = ((new java_util_Collections$2()));
	fA0 = tA1;
	(tA1)["java.util.Collections$2<init>()V"]();
	java_util_Collections._EMPTY_ENUMERATION = (fA0);
	tA2 = ((new java_util_Collections$EmptyList()));
	fA0 = tA2;
	(tA2)["java.util.Collections$EmptyList<init>(Ljava/util/Collections$1;)V"](null);
	java_util_Collections._EMPTY_LIST = (fA0);
	tA3 = ((new java_util_Collections$EmptySet()));
	fA0 = tA3;
	(tA3)["java.util.Collections$EmptySet<init>(Ljava/util/Collections$1;)V"](null);
	java_util_Collections._EMPTY_SET = (fA0);
	tA4 = ((new java_util_Collections$EmptyMap()));
	fA0 = tA4;
	(tA4)["java.util.Collections$EmptyMap<init>(Ljava/util/Collections$1;)V"](null);
	java_util_Collections._EMPTY_MAP = (fA0);
	return;
};
java_util_Collections["access$000()Ljava/util/Iterator;"] = function() { 
	return java_util_Collections._EMPTY_ITERATOR;
};
java_util_Collections["roundUpToPowerOfTwo(I)I"] = function(p0) { 
	var lI0 = 0;
	lI0 = p0;
	lI0 = (((lI0 + -1))|0);
	lI0 = (((lI0 | (((lI0 >>> 1))|0)))|0);
	lI0 = (((lI0 | (((lI0 >>> 2))|0)))|0);
	lI0 = (((lI0 | (((lI0 >>> 4))|0)))|0);
	lI0 = (((lI0 | (((lI0 >>> 8))|0)))|0);
	lI0 = (((lI0 | (((lI0 >>> 16))|0)))|0);
	return (((lI0 + 1))|0);
};
java_util_Collections["secondaryHash(Ljava/lang/Object;)I"] = function(p0) { 
	return java_util_Collections["secondaryHash(I)I"](p0["hashCode()I"]());
};
java_util_Collections["secondaryHash(I)I"] = function(p0) { 
	var lI0 = 0;
	lI0 = p0;
	lI0 = (((lI0 + ((((((lI0 << 15))|0) ^ -12931))|0)))|0);
	lI0 = (((lI0 ^ (((lI0 >>> 10))|0)))|0);
	lI0 = (((lI0 + (((lI0 << 3))|0)))|0);
	lI0 = (((lI0 ^ (((lI0 >>> 6))|0)))|0);
	lI0 = (((lI0 + ((((((lI0 << 2))|0) + (((lI0 << 14))|0)))|0)))|0);
	return (((lI0 ^ (((lI0 >>> 16))|0)))|0);
};
function java_lang_CloneNotSupportedException() {
}
java_lang_CloneNotSupportedException.prototype = Object.create(java_lang_Exception.prototype);
java_lang_CloneNotSupportedException.prototype.constructor = java_lang_CloneNotSupportedException;
java_lang_CloneNotSupportedException.SI = function(){};
java_lang_CloneNotSupportedException.prototype.__JT__CLASS_ID = java_lang_CloneNotSupportedException.__JT__CLASS_ID = 820;
java_lang_CloneNotSupportedException.prototype.__JT__CLASS_IDS = java_lang_CloneNotSupportedException.__JT__CLASS_IDS = [820,680,681,656,658];
function java_lang_Error() {
}
java_lang_Error.prototype = Object.create(java_lang_Throwable.prototype);
java_lang_Error.prototype.constructor = java_lang_Error;
java_lang_Error.prototype._thrown = false;
java_lang_Error.prototype._message = null;
java_lang_Error.prototype._writableStackTrace = false;
java_lang_Error.prototype._enableSuppression = false;
java_lang_Error.prototype._cause = null;
java_lang_Error.prototype._stackTrace = null;
java_lang_Error.prototype._supressed = null;
java_lang_Error.SI = function(){};
java_lang_Error.prototype.__JT__CLASS_ID = java_lang_Error.__JT__CLASS_ID = 719;
java_lang_Error.prototype.__JT__CLASS_IDS = java_lang_Error.__JT__CLASS_IDS = [719,681,656,658];
java_lang_Error.prototype["java.lang.Error<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.Throwable<init>(Ljava/lang/String;)V"](p0);
	return this;
	return this;
};
function java_lang_AssertionError() {
}
java_lang_AssertionError.prototype = Object.create(java_lang_Error.prototype);
java_lang_AssertionError.prototype.constructor = java_lang_AssertionError;
java_lang_AssertionError.SI = function(){};
java_lang_AssertionError.prototype.__JT__CLASS_ID = java_lang_AssertionError.__JT__CLASS_ID = 819;
java_lang_AssertionError.prototype.__JT__CLASS_IDS = java_lang_AssertionError.__JT__CLASS_IDS = [819,719,681,656,658];
java_lang_AssertionError.prototype["java.lang.AssertionError<init>(Ljava/lang/Object;)V"] = function(p0) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.lang.Error<init>(Ljava/lang/String;)V"](java_lang_String["valueOf(Ljava/lang/Object;)Ljava/lang/String;"](p0));
				if (!((p0 instanceof java_lang_Throwable))) {
					_G = 1;
					continue;
				}
				this["initCause(Ljava/lang/Throwable;)Ljava/lang/Throwable;"](N.checkCast(p0, java_lang_Throwable));
				_G = 1;
				continue;
			case 1:
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
// ABSTRACT
function java_util_HashMap$HashIterator() {
}
java_util_HashMap$HashIterator.prototype = Object.create(java_lang_Object.prototype);
java_util_HashMap$HashIterator.prototype.constructor = java_util_HashMap$HashIterator;
java_util_HashMap$HashIterator.prototype._nextEntry = null;
java_util_HashMap$HashIterator.prototype._nextIndex = 0;
java_util_HashMap$HashIterator.prototype._this_0 = null;
java_util_HashMap$HashIterator.prototype._expectedModCount = 0;
java_util_HashMap$HashIterator.prototype._lastEntryReturned = null;
java_util_HashMap$HashIterator.prototype.___id = 0;
java_util_HashMap$HashIterator.SI = function(){};
java_util_HashMap$HashIterator.prototype.__JT__CLASS_ID = java_util_HashMap$HashIterator.__JT__CLASS_ID = 818;
java_util_HashMap$HashIterator.prototype.__JT__CLASS_IDS = java_util_HashMap$HashIterator.__JT__CLASS_IDS = [818,656];
java_util_HashMap$HashIterator.prototype["java.util.HashMap$HashIterator<init>(Ljava/util/HashMap;)V"] = function(p0) { 
	var _G = 0, lA2 = null, lA3 = null, fI1 = 0, fA0 = null, fA1 = null, tI1 = 0, tA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				this._this_0 = p0;
				(this)["java.lang.Object<init>()V"]();
				this._nextEntry = this._this_0._entryForNullKey;
				this._expectedModCount = this._this_0._modCount;
				if (((this._nextEntry != null))) {
					_G = 1;
					continue;
				}
				lA2 = (p0._table);
				lA3 = null;
				_G = 2;
				continue;
			case 2:
				if (((lA3 != null))) {
					_G = 3;
					continue;
				}
				if (((this._nextIndex >= lA2.length))) {
					_G = 3;
					continue;
				}
				fA0 = lA2;
				fA1 = (this);
				tA2 = fA1;
				tI1 = this._nextIndex;
				fI1 = tI1;
				(tA2)._nextIndex = (((tI1 + 1))|0);
				lA3 = ((fA0).data[fI1]);
				_G = 2;
				continue;
			case 3:
				this._nextEntry = (lA3);
				_G = 1;
				continue;
			case 1:
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
java_util_HashMap$HashIterator.prototype["hasNext()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._nextEntry == null))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_util_HashMap$HashIterator.prototype["nextEntry()Ljava/util/HashMap$HashMapEntry;"] = function() { 
	var _G = 0, lA1 = null, lA2 = null, lA3 = null, fA0 = null, fA1 = null, tA0 = null, tA1 = null, tA4 = null, tA6 = null, tA5 = null, fI1 = 0, tI3 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._this_0._modCount == this._expectedModCount))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_util_ConcurrentModificationException()));
				fA0 = tA0;
				(tA0)["java.util.ConcurrentModificationException<init>()V"]();
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				if (((this._nextEntry != null))) {
					_G = 2;
					continue;
				}
				tA1 = ((new java_util_NoSuchElementException()));
				fA0 = tA1;
				(tA1)["java.util.NoSuchElementException<init>()V"]();
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				lA1 = (this._nextEntry);
				lA2 = (this._this_0._table);
				lA3 = ((lA1)._next);
				_G = 3;
				continue;
			case 3:
				if (((lA3 != null))) {
					_G = 4;
					continue;
				}
				if (((this._nextIndex >= lA2.length))) {
					_G = 4;
					continue;
				}
				fA0 = lA2;
				fA1 = (this);
				tA4 = fA1;
				tI3 = this._nextIndex;
				fI1 = tI3;
				(tA4)._nextIndex = (((tI3 + 1))|0);
				lA3 = ((fA0).data[fI1]);
				_G = 3;
				continue;
			case 4:
				this._nextEntry = (lA3);
				fA0 = (this);
				tA6 = fA0;
				tA5 = lA1;
				fA0 = tA5;
				(tA6)._lastEntryReturned = (tA5);
				return (fA0);
			default:
				break;
		}
	}
	return null;
};
function java_util_HashMap$EntryIterator() {
}
java_util_HashMap$EntryIterator.prototype = Object.create(java_util_HashMap$HashIterator.prototype);
java_util_HashMap$EntryIterator.prototype.constructor = java_util_HashMap$EntryIterator;
java_util_HashMap$EntryIterator.prototype._this_0_ = null;
java_util_HashMap$EntryIterator.prototype._nextEntry = null;
java_util_HashMap$EntryIterator.prototype._nextIndex = 0;
java_util_HashMap$EntryIterator.prototype._this_0 = null;
java_util_HashMap$EntryIterator.prototype._expectedModCount = 0;
java_util_HashMap$EntryIterator.prototype._lastEntryReturned = null;
java_util_HashMap$EntryIterator.SI = function(){};
java_util_HashMap$EntryIterator.prototype.__JT__CLASS_ID = java_util_HashMap$EntryIterator.__JT__CLASS_ID = 817;
java_util_HashMap$EntryIterator.prototype.__JT__CLASS_IDS = java_util_HashMap$EntryIterator.__JT__CLASS_IDS = [817,818,656,729];
java_util_HashMap$EntryIterator.prototype["java.util.HashMap$EntryIterator<init>(Ljava/util/HashMap;)V"] = function(p0) { 
	this._this_0_ = p0;
	(this)["java.util.HashMap$HashIterator<init>(Ljava/util/HashMap;)V"](p0);
	return this;
	return this;
};
java_util_HashMap$EntryIterator.prototype["java.util.HashMap$EntryIterator<init>(Ljava/util/HashMap;Ljava/util/HashMap$1;)V"] = function(p0, p1) { 
	this["java.util.HashMap$EntryIterator<init>(Ljava/util/HashMap;)V"](p0);
	return this;
	return this;
};
java_util_HashMap$EntryIterator.prototype["next()Ljava/lang/Object;"] = function() { 
	return (this["next()Ljava/util/Map$Entry;"]());
};
java_util_HashMap$EntryIterator.prototype["next()Ljava/util/Map$Entry;"] = function() { 
	return (this["nextEntry()Ljava/util/HashMap$HashMapEntry;"]());
};
function java_util_HashMap$EntrySet() {
}
java_util_HashMap$EntrySet.prototype = Object.create(java_util_AbstractSet.prototype);
java_util_HashMap$EntrySet.prototype.constructor = java_util_HashMap$EntrySet;
java_util_HashMap$EntrySet.prototype._this_0 = null;
java_util_HashMap$EntrySet.SI = function(){};
java_util_HashMap$EntrySet.prototype.__JT__CLASS_ID = java_util_HashMap$EntrySet.__JT__CLASS_ID = 815;
java_util_HashMap$EntrySet.prototype.__JT__CLASS_IDS = java_util_HashMap$EntrySet.__JT__CLASS_IDS = [815,816,728,656,747,723,724];
java_util_HashMap$EntrySet.prototype["java.util.HashMap$EntrySet<init>(Ljava/util/HashMap;Ljava/util/HashMap$1;)V"] = function(p0, p1) { 
	this["java.util.HashMap$EntrySet<init>(Ljava/util/HashMap;)V"](p0);
	return this;
	return this;
};
java_util_HashMap$EntrySet.prototype["java.util.HashMap$EntrySet<init>(Ljava/util/HashMap;)V"] = function(p0) { 
	this._this_0 = p0;
	(this)["java.util.AbstractSet<init>()V"]();
	return this;
	return this;
};
java_util_HashMap$EntrySet.prototype["size()I"] = function() { 
	return this._this_0._size;
};
java_util_HashMap$EntrySet.prototype["isEmpty()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._this_0._size != 0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_util_HashMap$EntrySet.prototype["iterator()Ljava/util/Iterator;"] = function() { 
	return this._this_0["newEntryIterator()Ljava/util/Iterator;"]();
};
java_util_HashMap$EntrySet.prototype["contains(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (N.isClassId(p0, 748)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				lA2 = N.checkCast(p0, java_util_Map$Entry);
				return java_util_HashMap["access$600(Ljava/util/HashMap;Ljava/lang/Object;Ljava/lang/Object;)Z"](this._this_0, lA2["getKey()Ljava/lang/Object;"](), lA2["getValue()Ljava/lang/Object;"]());
			default:
				break;
		}
	}
	return false;
};
function java_util_HashMap$1() {
}
java_util_HashMap$1.prototype = Object.create(java_lang_Object.prototype);
java_util_HashMap$1.prototype.constructor = java_util_HashMap$1;
java_util_HashMap$1.prototype.___id = 0;
java_util_HashMap$1.SI = function(){};
java_util_HashMap$1.prototype.__JT__CLASS_ID = java_util_HashMap$1.__JT__CLASS_ID = 814;
java_util_HashMap$1.prototype.__JT__CLASS_IDS = java_util_HashMap$1.__JT__CLASS_IDS = [814,656];
function java_util_HashMap$HashMapEntry() {
}
java_util_HashMap$HashMapEntry.prototype = Object.create(java_lang_Object.prototype);
java_util_HashMap$HashMapEntry.prototype.constructor = java_util_HashMap$HashMapEntry;
java_util_HashMap$HashMapEntry.prototype._key = null;
java_util_HashMap$HashMapEntry.prototype._next = null;
java_util_HashMap$HashMapEntry.prototype._value = null;
java_util_HashMap$HashMapEntry.prototype._hash = 0;
java_util_HashMap$HashMapEntry.prototype.___id = 0;
java_util_HashMap$HashMapEntry.SI = function(){};
java_util_HashMap$HashMapEntry.prototype.__JT__CLASS_ID = java_util_HashMap$HashMapEntry.__JT__CLASS_ID = 813;
java_util_HashMap$HashMapEntry.prototype.__JT__CLASS_IDS = java_util_HashMap$HashMapEntry.__JT__CLASS_IDS = [813,656,748];
java_util_HashMap$HashMapEntry.prototype["java.util.HashMap$HashMapEntry<init>(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/HashMap$HashMapEntry;)V"] = function(p0, p1, p2, p3) { 
	(this)["java.lang.Object<init>()V"]();
	this._key = p0;
	this._value = p1;
	this._hash = p2;
	this._next = p3;
	return this;
	return this;
};
java_util_HashMap$HashMapEntry.prototype["toString()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_StringBuilder()));
	fA0 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	return (fA0)["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"](this._key)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[7])["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"](this._value)["toString()Ljava/lang/String;"]();
};
java_util_HashMap$HashMapEntry.prototype["hashCode()I"] = function() { 
	var _G = 0, fI0 = 0, fI1 = 0, fA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._key != null))) {
					_G = 1;
					continue;
				}
				fI0 = 0;
				_G = 2;
				continue;
			case 1:
				fI0 = this._key["hashCode()I"]();
				_G = 2;
				continue;
			case 2:
				if (((this._value != null))) {
					_G = 3;
					continue;
				}
				fI1 = 0;
				_G = 4;
				continue;
			case 3:
				fA1 = this._value;
				fI1 = fA1["hashCode()I"]();
				_G = 4;
				continue;
			case 4:
				fI0 = (((fI0 ^ fI1))|0);
				return fI0;
			default:
				break;
		}
	}
	return 0;
};
java_util_HashMap$HashMapEntry.prototype["getKey()Ljava/lang/Object;"] = function() { 
	return this._key;
};
java_util_HashMap$HashMapEntry.prototype["getValue()Ljava/lang/Object;"] = function() { 
	return this._value;
};
java_util_HashMap$HashMapEntry.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (N.isClassId(p0, 748)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				lA2 = N.checkCast(p0, java_util_Map$Entry);
				if (!(java_util_Objects["equals(Ljava/lang/Object;Ljava/lang/Object;)Z"](lA2["getKey()Ljava/lang/Object;"](), this._key))) {
					_G = 2;
					continue;
				}
				if (!(java_util_Objects["equals(Ljava/lang/Object;Ljava/lang/Object;)Z"](lA2["getValue()Ljava/lang/Object;"](), this._value))) {
					_G = 2;
					continue;
				}
				fI0 = 1;
				_G = 3;
				continue;
			case 2:
				fI0 = 0;
				_G = 3;
				continue;
			case 3:
				return ((fI0)!=0);
			default:
				break;
		}
	}
	return false;
};
function java_util_HashMap() {
}
java_util_HashMap.prototype = Object.create(java_util_AbstractMap.prototype);
java_util_HashMap.prototype.constructor = java_util_HashMap;
java_util_HashMap.prototype._table = null;
java_util_HashMap.prototype._threshold = 0;
java_util_HashMap.prototype._size = 0;
java_util_HashMap.prototype._entryForNullKey = null;
java_util_HashMap.prototype._modCount = 0;
java_util_HashMap.prototype._entrySet = null;
java_util_HashMap.prototype._values = null;
java_util_HashMap.prototype._keySet_ = null;
java_util_HashMap.prototype._valuesCollection = null;
java_util_HashMap.prototype._keySet = null;
java_util_HashMap.SI = function() { 
	java_util_HashMap._EMPTY_TABLE = null;
	java_util_HashMap["java.util.HashMap<clinit>()V"]();
};
java_util_HashMap.prototype.__JT__CLASS_ID = java_util_HashMap.__JT__CLASS_ID = 811;
java_util_HashMap.prototype.__JT__CLASS_IDS = java_util_HashMap.__JT__CLASS_IDS = [811,812,656,726,658,746];
java_util_HashMap.prototype["java.util.HashMap<init>()V"] = function() { 
	(this)["java.util.AbstractMap<init>()V"]();
	this._table = N.checkCast(N.checkCast(java_util_HashMap._EMPTY_TABLE, JA_L), JA_L);
	this._threshold = -1;
	return this;
	return this;
};
java_util_HashMap["java.util.HashMap<clinit>()V"] = function() { 
	java_util_HashMap._EMPTY_TABLE = (new JA_L(2, "[Ljava.util.HashMap$HashMapEntry;"));
	return;
};
java_util_HashMap.prototype["java.util.HashMap<init>(I)V"] = function(p0) { 
	var _G = 0, lI1 = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = p0;
				(this)["java.util.AbstractMap<init>()V"]();
				if (((lI1 >= 0))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_IllegalArgumentException()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.IllegalArgumentException<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[111])["append(I)Ljava/lang/StringBuilder;"](lI1)["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				if (((lI1 != 0))) {
					_G = 2;
					continue;
				}
				lA2 = N.checkCast(N.checkCast(java_util_HashMap._EMPTY_TABLE, JA_L), JA_L);
				this._table = lA2;
				this._threshold = -1;
				return this;
				_G = 2;
				continue;
			case 2:
				if (((lI1 >= 4))) {
					_G = 3;
					continue;
				}
				lI1 = 4;
				_G = 4;
				continue;
			case 3:
				if (((lI1 <= 1073741824))) {
					_G = 5;
					continue;
				}
				lI1 = 1073741824;
				_G = 4;
				continue;
			case 5:
				lI1 = java_util_Collections["roundUpToPowerOfTwo(I)I"](lI1);
				_G = 4;
				continue;
			case 4:
				this["makeTable(I)[Ljava/util/HashMap$HashMapEntry;"](lI1);
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
java_util_HashMap.prototype["entrySet()Ljava/util/Set;"] = function() { 
	var _G = 0, lA1 = null, fA0 = null, fA1 = null, tA0 = null, tA2 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = (this._entrySet);
				if (((lA1 == null))) {
					_G = 1;
					continue;
				}
				fA0 = lA1;
				_G = 2;
				continue;
			case 1:
				fA0 = (this);
				tA0 = ((new java_util_HashMap$EntrySet()));
				fA1 = tA0;
				(tA0)["java.util.HashMap$EntrySet<init>(Ljava/util/HashMap;Ljava/util/HashMap$1;)V"](this, null);
				tA2 = fA0;
				tA1 = fA1;
				fA0 = tA1;
				(tA2)._entrySet = (tA1);
				_G = 2;
				continue;
			case 2:return (fA0); 
			default:
				break;
		}
	}
	return null;
};
java_util_HashMap.prototype["newEntryIterator()Ljava/util/Iterator;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_util_HashMap$EntryIterator()));
	fA0 = tA0;
	(tA0)["java.util.HashMap$EntryIterator<init>(Ljava/util/HashMap;Ljava/util/HashMap$1;)V"](this, null);
	return (fA0);
};
java_util_HashMap.prototype["size()I"] = function() { 
	return this._size;
};
java_util_HashMap.prototype["isEmpty()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._size != 0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_util_HashMap.prototype["clone()Ljava/lang/Object;"] = function() { 
	var _G = 0, lA1 = null, lA2 = null, fA0 = null, tA1 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						_G = 1;
						continue;
					case 1:
						lA1 = (N.checkCast(java_util_AbstractMap.prototype["clone()Ljava/lang/Object;"].call((this)), java_util_HashMap));
						_G = 2;
						continue;
					case 2:
						_G = 3;
						continue;
					case 4:
						fA0 = (J__exception__);
						lA2 = fA0;
						tA1 = ((new java_lang_AssertionError()));
						fA0 = tA1;
						(tA1)["java.lang.AssertionError<init>(Ljava/lang/Object;)V"](lA2);
						throw new WrappedError(fA0);
						_G = 3;
						continue;
					case 3:
						(lA1)["makeTable(I)[Ljava/util/HashMap$HashMapEntry;"](this._table.length);
						(lA1)._entryForNullKey = null;
						(lA1)._size = 0;
						(lA1)._keySet_ = null;
						(lA1)._entrySet = null;
						(lA1)._values = null;
						(lA1)["init()V"]();
						(lA1)["constructorPutAll(Ljava/util/Map;)V"]((this));
						return lA1;
					default:
						break;
				}
			}
			return null;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_CloneNotSupportedException)))) {
				_G = 4;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return null;
};
java_util_HashMap.prototype["init()V"] = function() { 
	return;
};
java_util_HashMap.prototype["makeTable(I)[Ljava/util/HashMap$HashMapEntry;"] = function(p0) { 
	var lA2 = null;
	lA2 = N.checkCast(new JA_L(p0, "[Ljava.util.HashMap$HashMapEntry;"), JA_L);
	this._table = lA2;
	this._threshold = ((((((p0 >> 1))|0) + (((p0 >> 2))|0)))|0);
	return lA2;
};
java_util_HashMap.prototype["constructorPutAll(Ljava/util/Map;)V"] = function(p0) { 
	var _G = 0, lA3 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._table != java_util_HashMap._EMPTY_TABLE))) {
					_G = 1;
					continue;
				}
				this["doubleCapacity()[Ljava/util/HashMap$HashMapEntry;"]();
				_G = 1;
				continue;
			case 1:
				lA2 = p0["entrySet()Ljava/util/Set;"]()["iterator()Ljava/util/Iterator;"]();
				_G = 2;
				continue;
			case 2:
				if (!(lA2["hasNext()Z"]())) {
					_G = 3;
					continue;
				}
				lA3 = N.checkCast(lA2["next()Ljava/lang/Object;"](), java_util_Map$Entry);
				this["constructorPut(Ljava/lang/Object;Ljava/lang/Object;)V"](lA3["getKey()Ljava/lang/Object;"](), lA3["getValue()Ljava/lang/Object;"]());
				_G = 2;
				continue;
			case 3:
				return;
			default:
				break;
		}
	}
	return;
};
java_util_HashMap.prototype["constructorPut(Ljava/lang/Object;Ljava/lang/Object;)V"] = function(p0, p1) { 
	var _G = 0, lA3 = null, lA4 = null, lA6 = null, lA7 = null, lI3 = 0, lI5 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 != null))) {
					_G = 1;
					continue;
				}
				lA3 = (this._entryForNullKey);
				if (((lA3 != null))) {
					_G = 2;
					continue;
				}
				this._entryForNullKey = this["constructorNewEntry(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/HashMap$HashMapEntry;)Ljava/util/HashMap$HashMapEntry;"](null, p1, 0, null);
				this._size = (((this._size + 1))|0);
				_G = 3;
				continue;
			case 2:
				(lA3)._value = p1;
				_G = 3;
				continue;
			case 3:
				return;
				_G = 1;
				continue;
			case 1:
				lI3 = java_util_HashMap["secondaryHash(Ljava/lang/Object;)I"](p0);
				lA4 = (this._table);
				lI5 = (((lI3 & (((lA4.length - 1))|0)))|0);
				lA6 = ((lA4).data[lI5]);
				lA7 = lA6;
				_G = 4;
				continue;
			case 4:
				if (((lA7 == null))) {
					_G = 5;
					continue;
				}
				if ((((lA7)._hash != lI3))) {
					_G = 6;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"]((lA7)._key))) {
					_G = 6;
					continue;
				}
				(lA7)._value = p1;
				return;
				_G = 6;
				continue;
			case 6:
				lA7 = ((lA7)._next);
				_G = 4;
				continue;
			case 5:
				(lA4).data[lI5] = (this["constructorNewEntry(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/HashMap$HashMapEntry;)Ljava/util/HashMap$HashMapEntry;"](p0, p1, lI3, (lA6)));
				this._size = (((this._size + 1))|0);
				return;
			default:
				break;
		}
	}
	return;
};
java_util_HashMap.prototype["constructorNewEntry(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/HashMap$HashMapEntry;)Ljava/util/HashMap$HashMapEntry;"] = function(p0, p1, p2, p3) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_util_HashMap$HashMapEntry()));
	fA0 = tA0;
	(tA0)["java.util.HashMap$HashMapEntry<init>(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/HashMap$HashMapEntry;)V"](p0, p1, p2, p3);
	return (fA0);
};
java_util_HashMap["access$600(Ljava/util/HashMap;Ljava/lang/Object;Ljava/lang/Object;)Z"] = function(p0, p1, p2) { 
	return p0["containsMapping(Ljava/lang/Object;Ljava/lang/Object;)Z"](p1, p2);
};
java_util_HashMap.prototype["containsMapping(Ljava/lang/Object;Ljava/lang/Object;)Z"] = function(p0, p1) { 
	var _G = 0, lA3 = null, fI0 = 0, lA4 = null, lA6 = null, lI3 = 0, lI5 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 != null))) {
					_G = 1;
					continue;
				}
				lA3 = (this._entryForNullKey);
				if (((lA3 == null))) {
					_G = 2;
					continue;
				}
				if (!(java_util_Objects["equals(Ljava/lang/Object;Ljava/lang/Object;)Z"](p1, (lA3)._value))) {
					_G = 2;
					continue;
				}
				fI0 = 1;
				_G = 3;
				continue;
			case 2:
				fI0 = 0;
				_G = 3;
				continue;
			case 3:return ((fI0)!=0); 
			case 1:
				lI3 = java_util_HashMap["secondaryHash(Ljava/lang/Object;)I"](p0);
				lA4 = (this._table);
				lI5 = (((lI3 & (((lA4.length - 1))|0)))|0);
				lA6 = ((lA4).data[lI5]);
				_G = 4;
				continue;
			case 4:
				if (((lA6 == null))) {
					_G = 5;
					continue;
				}
				if ((((lA6)._hash != lI3))) {
					_G = 6;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"]((lA6)._key))) {
					_G = 6;
					continue;
				}
				return java_util_Objects["equals(Ljava/lang/Object;Ljava/lang/Object;)Z"](p1, (lA6)._value);
			case 6:
				lA6 = ((lA6)._next);
				_G = 4;
				continue;
			case 5:
				return false;
			default:
				break;
		}
	}
	return false;
};
java_util_HashMap["secondaryHash(Ljava/lang/Object;)I"] = function(p0) { 
	var lI1 = 0;
	lI1 = p0["hashCode()I"]();
	lI1 = (((lI1 ^ ((((((lI1 >>> 20))|0) ^ (((lI1 >>> 12))|0)))|0)))|0);
	lI1 = (((lI1 ^ ((((((lI1 >>> 7))|0) ^ (((lI1 >>> 4))|0)))|0)))|0);
	return lI1;
};
java_util_HashMap.prototype["get(Ljava/lang/Object;)Ljava/lang/Object;"] = function(p0) { 
	var _G = 0, lA2 = null, lA3 = null, lA4 = null, lA5 = null, lI2 = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 != null))) {
					_G = 1;
					continue;
				}
				lA2 = (this._entryForNullKey);
				if (((lA2 != null))) {
					_G = 2;
					continue;
				}
				fA0 = null;
				_G = 3;
				continue;
			case 2:
				fA0 = (lA2)._value;
				_G = 3;
				continue;
			case 3:return fA0; 
			case 1:
				lI2 = p0["hashCode()I"]();
				lI2 = (((lI2 ^ ((((((lI2 >>> 20))|0) ^ (((lI2 >>> 12))|0)))|0)))|0);
				lI2 = (((lI2 ^ ((((((lI2 >>> 7))|0) ^ (((lI2 >>> 4))|0)))|0)))|0);
				lA3 = (this._table);
				lA4 = ((lA3).data[(((lI2 & (((lA3.length - 1))|0)))|0)]);
				_G = 4;
				continue;
			case 4:
				if (((lA4 == null))) {
					_G = 5;
					continue;
				}
				lA5 = (lA4)._key;
				if (((lA5 == p0))) {
					_G = 6;
					continue;
				}
				if ((((lA4)._hash != lI2))) {
					_G = 7;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"](lA5))) {
					_G = 7;
					continue;
				}
				_G = 6;
				continue;
			case 6:
				return (lA4)._value;
			case 7:
				lA4 = ((lA4)._next);
				_G = 4;
				continue;
			case 5:
				return null;
			default:
				break;
		}
	}
	return null;
};
java_util_HashMap.prototype["containsKey(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0, lA3 = null, lA4 = null, lA5 = null, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 != null))) {
					_G = 1;
					continue;
				}
				if (((this._entryForNullKey == null))) {
					_G = 2;
					continue;
				}
				fI0 = 1;
				_G = 3;
				continue;
			case 2:
				fI0 = 0;
				_G = 3;
				continue;
			case 3:return ((fI0)!=0); 
			case 1:
				lI2 = p0["hashCode()I"]();
				lI2 = (((lI2 ^ ((((((lI2 >>> 20))|0) ^ (((lI2 >>> 12))|0)))|0)))|0);
				lI2 = (((lI2 ^ ((((((lI2 >>> 7))|0) ^ (((lI2 >>> 4))|0)))|0)))|0);
				lA3 = (this._table);
				lA4 = ((lA3).data[(((lI2 & (((lA3.length - 1))|0)))|0)]);
				_G = 4;
				continue;
			case 4:
				if (((lA4 == null))) {
					_G = 5;
					continue;
				}
				lA5 = (lA4)._key;
				if (((lA5 == p0))) {
					_G = 6;
					continue;
				}
				if ((((lA4)._hash != lI2))) {
					_G = 7;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"](lA5))) {
					_G = 7;
					continue;
				}
				_G = 6;
				continue;
			case 6:
				return true;
			case 7:
				lA4 = ((lA4)._next);
				_G = 4;
				continue;
			case 5:
				return false;
			default:
				break;
		}
	}
	return false;
};
java_util_HashMap.prototype["doubleCapacity()[Ljava/util/HashMap$HashMapEntry;"] = function() { 
	var _G = 0, lA1 = null, lA4 = null, lA6 = null, lA8 = null, lA9 = null, lI2 = 0, lI3 = 0, lI5 = 0, lI7 = 0, lI10 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = (this._table);
				lI2 = lA1.length;
				if (((lI2 != 1073741824))) {
					_G = 1;
					continue;
				}
				return (lA1);
			case 1:
				lI3 = ((Math.imul(lI2, 2))|0);
				lA4 = (this["makeTable(I)[Ljava/util/HashMap$HashMapEntry;"](lI3));
				if (((this._size != 0))) {
					_G = 2;
					continue;
				}
				return (lA4);
			case 2:
				lI5 = 0;
				_G = 3;
				continue;
			case 3:
				if (((lI5 >= lI2))) {
					_G = 4;
					continue;
				}
				lA6 = ((lA1).data[lI5]);
				if (((lA6 != null))) {
					_G = 5;
					continue;
				}
				_G = 6;
				continue;
			case 5:
				lI7 = ((((lA6)._hash & lI2))|0);
				lA8 = null;
				(lA4).data[(((lI5 | lI7))|0)] = lA6;
				lA9 = ((lA6)._next);
				_G = 7;
				continue;
			case 7:
				if (((lA9 == null))) {
					_G = 8;
					continue;
				}
				lI10 = ((((lA9)._hash & lI2))|0);
				if (((lI10 == lI7))) {
					_G = 9;
					continue;
				}
				if (((lA8 != null))) {
					_G = 10;
					continue;
				}
				(lA4).data[(((lI5 | lI10))|0)] = lA9;
				_G = 11;
				continue;
			case 10:
				(lA8)._next = (lA9);
				_G = 11;
				continue;
			case 11:
				lA8 = lA6;
				lI7 = lI10;
				_G = 9;
				continue;
			case 9:
				lA6 = lA9;
				lA9 = ((lA9)._next);
				_G = 7;
				continue;
			case 8:
				if (((lA8 == null))) {
					_G = 6;
					continue;
				}
				(lA8)._next = null;
				_G = 6;
				continue;
			case 6:
				lI5 = (((lI5 + 1))|0);
				_G = 3;
				continue;
			case 4:
				return (lA4);
			default:
				break;
		}
	}
	return null;
};
java_util_HashMap.prototype["put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"] = function(p0, p1) { 
	var _G = 0, lA4 = null, lA6 = null, lA7 = null, fI0 = 0, lI3 = 0, lI5 = 0, fA0 = null, tI2 = 0, tA3 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 != null))) {
					_G = 1;
					continue;
				}
				return this["putValueForNullKey(Ljava/lang/Object;)Ljava/lang/Object;"](p1);
			case 1:
				lI3 = java_util_HashMap["secondaryHash(Ljava/lang/Object;)I"](p0);
				lA4 = (this._table);
				lI5 = (((lI3 & (((lA4.length - 1))|0)))|0);
				lA6 = ((lA4).data[lI5]);
				_G = 2;
				continue;
			case 2:
				if (((lA6 == null))) {
					_G = 3;
					continue;
				}
				if ((((lA6)._hash != lI3))) {
					_G = 4;
					continue;
				}
				if (!(p0["equals(Ljava/lang/Object;)Z"]((lA6)._key))) {
					_G = 4;
					continue;
				}
				this["preModify(Ljava/util/HashMap$HashMapEntry;)V"]((lA6));
				lA7 = (lA6)._value;
				(lA6)._value = p1;
				return lA7;
			case 4:
				lA6 = ((lA6)._next);
				_G = 2;
				continue;
			case 3:
				this._modCount = (((this._modCount + 1))|0);
				fA0 = (this);
				tA3 = fA0;
				tI2 = this._size;
				fI0 = tI2;
				(tA3)._size = (((tI2 + 1))|0);
				if (((fI0 <= this._threshold))) {
					_G = 5;
					continue;
				}
				lA4 = (this["doubleCapacity()[Ljava/util/HashMap$HashMapEntry;"]());
				lI5 = (((lI3 & (((lA4.length - 1))|0)))|0);
				_G = 5;
				continue;
			case 5:
				this["addNewEntry(Ljava/lang/Object;Ljava/lang/Object;II)V"](p0, p1, lI3, lI5);
				return null;
			default:
				break;
		}
	}
	return null;
};
java_util_HashMap.prototype["addNewEntry(Ljava/lang/Object;Ljava/lang/Object;II)V"] = function(p0, p1, p2, p3) { 
	var fI1 = 0, fA0 = null, fA2 = null, tA0 = null;
	fA0 = (this._table);
	fI1 = p3;
	tA0 = ((new java_util_HashMap$HashMapEntry()));
	fA2 = tA0;
	(tA0)["java.util.HashMap$HashMapEntry<init>(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/HashMap$HashMapEntry;)V"](p0, p1, p2, (((this._table).data[p3])));
	(fA0).data[fI1] = fA2;
	return;
};
java_util_HashMap.prototype["preModify(Ljava/util/HashMap$HashMapEntry;)V"] = function(p0) { 
	return;
};
java_util_HashMap.prototype["putValueForNullKey(Ljava/lang/Object;)Ljava/lang/Object;"] = function(p0) { 
	var _G = 0, lA2 = null, lA3 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA2 = (this._entryForNullKey);
				if (((lA2 != null))) {
					_G = 1;
					continue;
				}
				this["addNewEntryForNullKey(Ljava/lang/Object;)V"](p0);
				this._size = (((this._size + 1))|0);
				this._modCount = (((this._modCount + 1))|0);
				return null;
			case 1:
				this["preModify(Ljava/util/HashMap$HashMapEntry;)V"]((lA2));
				lA3 = (lA2)._value;
				(lA2)._value = p0;
				return lA3;
			default:
				break;
		}
	}
	return null;
};
java_util_HashMap.prototype["addNewEntryForNullKey(Ljava/lang/Object;)V"] = function(p0) { 
	var fA1 = null, tA0 = null;
	tA0 = ((new java_util_HashMap$HashMapEntry()));
	fA1 = tA0;
	(tA0)["java.util.HashMap$HashMapEntry<init>(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/HashMap$HashMapEntry;)V"](null, p0, 0, null);
	this._entryForNullKey = (fA1);
	return;
};
function com_jtransc_charset_charsets_JTranscCharsetIBM866() {
}
com_jtransc_charset_charsets_JTranscCharsetIBM866.prototype = Object.create(com_jtransc_charset_JTranscCharsetSingleByte.prototype);
com_jtransc_charset_charsets_JTranscCharsetIBM866.prototype.constructor = com_jtransc_charset_charsets_JTranscCharsetIBM866;
com_jtransc_charset_charsets_JTranscCharsetIBM866.prototype._invalidChar = 63;
com_jtransc_charset_charsets_JTranscCharsetIBM866.prototype._decode = null;
com_jtransc_charset_charsets_JTranscCharsetIBM866.prototype._encode = null;
com_jtransc_charset_charsets_JTranscCharsetIBM866.SI = function(){};
com_jtransc_charset_charsets_JTranscCharsetIBM866.prototype.__JT__CLASS_ID = com_jtransc_charset_charsets_JTranscCharsetIBM866.__JT__CLASS_ID = 809;
com_jtransc_charset_charsets_JTranscCharsetIBM866.prototype.__JT__CLASS_IDS = com_jtransc_charset_charsets_JTranscCharsetIBM866.__JT__CLASS_IDS = [809,810,804,656];
com_jtransc_charset_charsets_JTranscCharsetIBM866.prototype["com.jtransc.charset.charsets.JTranscCharsetIBM866<init>()V"] = function() { 
	var fA1 = null, tA0 = null;
	tA0 = (new JA_L(5, "[Ljava.lang.String;"));
	fA1 = tA0;
	(tA0).data[0] = (S[112]);
	(fA1).setArraySlice(1, [(S[113]), (S[114]), (S[115]), (S[116])]);
	(this)["com.jtransc.charset.JTranscCharsetSingleByte<init>([Ljava/lang/String;Ljava/lang/String;)V"]((fA1), S[117]);
	return this;
	return this;
};
function java_util_ServiceLoader() {
}
java_util_ServiceLoader.prototype = Object.create(java_lang_Object.prototype);
java_util_ServiceLoader.prototype.constructor = java_util_ServiceLoader;
java_util_ServiceLoader.prototype._service = null;
java_util_ServiceLoader.prototype._list = null;
java_util_ServiceLoader.prototype.___id = 0;
java_util_ServiceLoader.SI = function(){};
java_util_ServiceLoader.prototype.__JT__CLASS_ID = java_util_ServiceLoader.__JT__CLASS_ID = 808;
java_util_ServiceLoader.prototype.__JT__CLASS_IDS = java_util_ServiceLoader.__JT__CLASS_IDS = [808,656,724];
java_util_ServiceLoader.prototype["java.util.ServiceLoader<init>(Ljava/lang/Class;)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	java_util_Objects["requireNonNull(Ljava/lang/Object;)Ljava/lang/Object;"]((p0));
	this._service = p0;
	this._list = java_util_Arrays["asList([Ljava/lang/Object;)Ljava/util/List;"](this["getInstances(Ljava/lang/String;)[Ljava/lang/Object;"](p0["getName()Ljava/lang/String;"]()));
	this["reload()V"]();
	return this;
	return this;
};
java_util_ServiceLoader.prototype["getInstances(Ljava/lang/String;)[Ljava/lang/Object;"] = function(p0) { 
	var out = null;
	out = null;
	if (java_util_Objects["equals(Ljava/lang/Object;Ljava/lang/Object;)Z"](p0, S[118])) {
		out = new JA_L(6, "[Ljava.lang.Object;");
		out.data[0] = (new com_jtransc_charset_charsets_JTranscCharsetUTF8())["com.jtransc.charset.charsets.JTranscCharsetUTF8<init>()V"]();
		out.data[1] = (new com_jtransc_charset_charsets_JTranscCharsetIBM866())["com.jtransc.charset.charsets.JTranscCharsetIBM866<init>()V"]();
		out.data[2] = (new com_jtransc_charset_charsets_JTranscCharsetLatin1())["com.jtransc.charset.charsets.JTranscCharsetLatin1<init>()V"]();
		out.data[3] = (new com_jtransc_charset_charsets_JTranscCharsetUSASCII())["com.jtransc.charset.charsets.JTranscCharsetUSASCII<init>()V"]();
		out.data[4] = (new com_jtransc_charset_charsets_JTranscCharsetUTF16LE())["com.jtransc.charset.charsets.JTranscCharsetUTF16LE<init>()V"]();
		out.data[5] = (new com_jtransc_charset_charsets_JTranscCharsetUTF16BE())["com.jtransc.charset.charsets.JTranscCharsetUTF16BE<init>()V"]();
		return out;
	}
	if (java_util_Objects["equals(Ljava/lang/Object;Ljava/lang/Object;)Z"](p0, S[119])) {
		out = new JA_L(1, "[Ljava.lang.Object;");
		out.data[0] = (new com_jtransc_mix_JTranscProcessMulti())["com.jtransc.mix.JTranscProcessMulti<init>()V"]();
		return out;
	}
	return new JA_L(0, "[Ljava.lang.Object;");
};
java_util_ServiceLoader.prototype["reload()V"] = function() { 
	return;
};
java_util_ServiceLoader.prototype["toString()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_StringBuilder()));
	fA0 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[120])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this._service["getName()Ljava/lang/String;"]())["toString()Ljava/lang/String;"]();
};
java_util_ServiceLoader.prototype["iterator()Ljava/util/Iterator;"] = function() { 
	return this._list["iterator()Ljava/util/Iterator;"]();
};
java_util_ServiceLoader["load(Ljava/lang/Class;)Ljava/util/ServiceLoader;"] = function(p0) { 
	return java_util_ServiceLoader["load(Ljava/lang/Class;Ljava/lang/ClassLoader;)Ljava/util/ServiceLoader;"](p0, java_lang_Thread["currentThread()Ljava/lang/Thread;"]()["getContextClassLoader()Ljava/lang/ClassLoader;"]());
};
java_util_ServiceLoader["load(Ljava/lang/Class;Ljava/lang/ClassLoader;)Ljava/util/ServiceLoader;"] = function(p0, p1) { 
	var _G = 0, lA1 = null, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = (p1);
				if (((lA1 != null))) {
					_G = 1;
					continue;
				}
				lA1 = (java_lang_ClassLoader["getSystemClassLoader()Ljava/lang/ClassLoader;"]());
				_G = 1;
				continue;
			case 1:
				tA0 = ((new java_util_ServiceLoader()));
				fA0 = tA0;
				(tA0)["java.util.ServiceLoader<init>(Ljava/lang/Class;)V"](p0);
				return (fA0);
			default:
				break;
		}
	}
	return null;
};
function java_lang_IllegalArgumentException() {
}
java_lang_IllegalArgumentException.prototype = Object.create(java_lang_RuntimeException.prototype);
java_lang_IllegalArgumentException.prototype.constructor = java_lang_IllegalArgumentException;
java_lang_IllegalArgumentException.SI = function(){};
java_lang_IllegalArgumentException.prototype.__JT__CLASS_ID = java_lang_IllegalArgumentException.__JT__CLASS_ID = 705;
java_lang_IllegalArgumentException.prototype.__JT__CLASS_IDS = java_lang_IllegalArgumentException.__JT__CLASS_IDS = [705,696,680,681,656,658];
java_lang_IllegalArgumentException.prototype["java.lang.IllegalArgumentException<init>()V"] = function() { 
	(this)["java.lang.RuntimeException<init>()V"]();
	return this;
	return this;
};
java_lang_IllegalArgumentException.prototype["java.lang.IllegalArgumentException<init>(Ljava/lang/Throwable;)V"] = function(p0) { 
	(this)["java.lang.RuntimeException<init>(Ljava/lang/Throwable;)V"](p0);
	return this;
	return this;
};
java_lang_IllegalArgumentException.prototype["java.lang.IllegalArgumentException<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.RuntimeException<init>(Ljava/lang/String;)V"](p0);
	return this;
	return this;
};
function java_nio_charset_UnsupportedCharsetException() {
}
java_nio_charset_UnsupportedCharsetException.prototype = Object.create(java_lang_IllegalArgumentException.prototype);
java_nio_charset_UnsupportedCharsetException.prototype.constructor = java_nio_charset_UnsupportedCharsetException;
java_nio_charset_UnsupportedCharsetException.prototype._charsetName = null;
java_nio_charset_UnsupportedCharsetException.SI = function(){};
java_nio_charset_UnsupportedCharsetException.prototype.__JT__CLASS_ID = java_nio_charset_UnsupportedCharsetException.__JT__CLASS_ID = 807;
java_nio_charset_UnsupportedCharsetException.prototype.__JT__CLASS_IDS = java_nio_charset_UnsupportedCharsetException.__JT__CLASS_IDS = [807,705,696,680,681,656,658];
java_nio_charset_UnsupportedCharsetException.prototype["java.nio.charset.UnsupportedCharsetException<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.IllegalArgumentException<init>(Ljava/lang/String;)V"](java_lang_String["valueOf(Ljava/lang/Object;)Ljava/lang/String;"]((p0)));
	this._charsetName = p0;
	return this;
	return this;
};
function com_jtransc_charset_JTranscCharBuffer() {
}
com_jtransc_charset_JTranscCharBuffer.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_charset_JTranscCharBuffer.prototype.constructor = com_jtransc_charset_JTranscCharBuffer;
com_jtransc_charset_JTranscCharBuffer.prototype._buffer = null;
com_jtransc_charset_JTranscCharBuffer.prototype._position = 0;
com_jtransc_charset_JTranscCharBuffer.prototype._size = 0;
com_jtransc_charset_JTranscCharBuffer.prototype.___id = 0;
com_jtransc_charset_JTranscCharBuffer.SI = function(){};
com_jtransc_charset_JTranscCharBuffer.prototype.__JT__CLASS_ID = com_jtransc_charset_JTranscCharBuffer.__JT__CLASS_ID = 806;
com_jtransc_charset_JTranscCharBuffer.prototype.__JT__CLASS_IDS = com_jtransc_charset_JTranscCharBuffer.__JT__CLASS_IDS = [806,656];
com_jtransc_charset_JTranscCharBuffer.prototype["com.jtransc.charset.JTranscCharBuffer<init>(I)V"] = function(p0) { 
	var _G = 0, fI1 = 0;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.lang.Object<init>()V"]();
				if (((p0 >= 64))) {
					_G = 1;
					continue;
				}
				fI1 = 64;
				_G = 2;
				continue;
			case 1:
				fI1 = p0;
				_G = 2;
				continue;
			case 2:
				this._size = fI1;
				this._buffer = new JA_C(this._size);
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
com_jtransc_charset_JTranscCharBuffer.prototype["toString()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_String()));
	fA0 = tA0;
	(tA0)["java.lang.String<init>([CII)V"](this._buffer, 0, this._position);
	return (fA0);
};
com_jtransc_charset_JTranscCharBuffer.prototype["append(C)V"] = function(p0) { 
	var _G = 0, lI3 = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._position != this._size))) {
					_G = 1;
					continue;
				}
				lA2 = new JA_C(((Math.imul(this._size, 2))|0));
				lI3 = 0;
				_G = 2;
				continue;
			case 2:
				if (((lI3 >= this._size))) {
					_G = 3;
					continue;
				}
				lA2.data[lI3] = (this._buffer.data[lI3]);
				lI3 = (((lI3 + 1))|0);
				_G = 2;
				continue;
			case 3:
				this._buffer = lA2;
				this._size = (((this._size + this._size))|0);
				_G = 1;
				continue;
			case 1:
				this._buffer.data[this._position] = p0;
				this._position = (((this._position + 1))|0);
				return;
			default:
				break;
		}
	}
	return;
};
function java_io_Flushable() {
}
java_io_Flushable.prototype = Object.create(java_lang_Object_base.prototype);
java_io_Flushable.prototype.constructor = java_io_Flushable;
java_io_Flushable.SI = function(){};
java_io_Flushable.prototype.__JT__CLASS_ID = java_io_Flushable.__JT__CLASS_ID = 694;
java_io_Flushable.prototype.__JT__CLASS_IDS = java_io_Flushable.__JT__CLASS_IDS = [694,656];
java_io_Flushable.prototype["flush()V"] = function() { N.methodWithoutBody('java.io.Flushable.flush') };
function java_lang_AutoCloseable() {
}
java_lang_AutoCloseable.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_AutoCloseable.prototype.constructor = java_lang_AutoCloseable;
java_lang_AutoCloseable.SI = function(){};
java_lang_AutoCloseable.prototype.__JT__CLASS_ID = java_lang_AutoCloseable.__JT__CLASS_ID = 691;
java_lang_AutoCloseable.prototype.__JT__CLASS_IDS = java_lang_AutoCloseable.__JT__CLASS_IDS = [691,656];
function java_io_Closeable() {
}
java_io_Closeable.prototype = Object.create(java_lang_Object_base.prototype);
java_io_Closeable.prototype.constructor = java_io_Closeable;
java_io_Closeable.SI = function(){};
java_io_Closeable.prototype.__JT__CLASS_ID = java_io_Closeable.__JT__CLASS_ID = 690;
java_io_Closeable.prototype.__JT__CLASS_IDS = java_io_Closeable.__JT__CLASS_IDS = [690,691,656];
// ABSTRACT
function java_io_OutputStream() {
}
java_io_OutputStream.prototype = Object.create(java_lang_Object.prototype);
java_io_OutputStream.prototype.constructor = java_io_OutputStream;
java_io_OutputStream.prototype.___id = 0;
java_io_OutputStream.SI = function(){};
java_io_OutputStream.prototype.__JT__CLASS_ID = java_io_OutputStream.__JT__CLASS_ID = 693;
java_io_OutputStream.prototype.__JT__CLASS_IDS = java_io_OutputStream.__JT__CLASS_IDS = [693,656,690,694,691];
java_io_OutputStream.prototype["java.io.OutputStream<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_io_OutputStream.prototype["write(I)V"] = function() { N.methodWithoutBody('java.io.OutputStream.write') };
java_io_OutputStream.prototype["flush()V"] = function() { 
	return;
};
java_io_OutputStream.prototype["write([BII)V"] = function(p0, p1, p2) { 
	var _G = 0, lI4 = 0;
	while (true) {
		switch (_G) {
			case 0:
				com_jtransc_JTranscArrays["checkOffsetAndCount(III)V"]((p0).length, p1, p2);
				lI4 = p1;
				_G = 1;
				continue;
			case 1:
				if (((lI4 >= (((p1 + p2))|0)))) {
					_G = 2;
					continue;
				}
				this["write(I)V"]((((p0.data[lI4]))|0));
				lI4 = (((lI4 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
java_io_OutputStream.prototype["write([B)V"] = function(p0) { 
	this["write([BII)V"](p0, 0, (p0).length);
	return;
};
function java_io_ByteArrayOutputStream() {
}
java_io_ByteArrayOutputStream.prototype = Object.create(java_io_OutputStream.prototype);
java_io_ByteArrayOutputStream.prototype.constructor = java_io_ByteArrayOutputStream;
java_io_ByteArrayOutputStream.prototype._count = 0;
java_io_ByteArrayOutputStream.prototype._buf = null;
java_io_ByteArrayOutputStream.SI = function(){};
java_io_ByteArrayOutputStream.prototype.__JT__CLASS_ID = java_io_ByteArrayOutputStream.__JT__CLASS_ID = 805;
java_io_ByteArrayOutputStream.prototype.__JT__CLASS_IDS = java_io_ByteArrayOutputStream.__JT__CLASS_IDS = [805,693,656,690,694,691];
java_io_ByteArrayOutputStream.prototype["java.io.ByteArrayOutputStream<init>(I)V"] = function(p0) { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.io.OutputStream<init>()V"]();
				if (((p0 < 0))) {
					_G = 1;
					continue;
				}
				this._buf = new JA_B(p0);
				_G = 2;
				continue;
			case 1:
				tA0 = ((new java_lang_IllegalArgumentException()));
				fA0 = tA0;
				(tA0)["java.lang.IllegalArgumentException<init>(Ljava/lang/String;)V"](S[121]);
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
java_io_ByteArrayOutputStream.prototype["toString()Ljava/lang/String;"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_String()));
	fA0 = tA0;
	(tA0)["java.lang.String<init>([BII)V"](this._buf, 0, this._count);
	return (fA0);
};
java_io_ByteArrayOutputStream.prototype["toByteArray()[B"] = function() { 
	var lA1 = null;
	lA1 = (new JA_B(this._count));
	java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((this._buf), 0, lA1, 0, this._count);
	return (lA1);
};
java_io_ByteArrayOutputStream.prototype["write(I)V"] = function(p0) { 
	var _G = 0, fA0 = null, fI1 = 0, fA1 = null, tI1 = 0, tA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._count != this._buf.length))) {
					_G = 1;
					continue;
				}
				this["expand(I)V"](1);
				_G = 1;
				continue;
			case 1:
				fA0 = this._buf;
				fA1 = (this);
				tA2 = fA1;
				tI1 = this._count;
				fI1 = tI1;
				(tA2)._count = (((tI1 + 1))|0);
				fA0.data[fI1] = ((p0)<<24>>24);
				return;
			default:
				break;
		}
	}
	return;
};
java_io_ByteArrayOutputStream.prototype["expand(I)V"] = function(p0) { 
	var _G = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if ((((((this._count + p0))|0) > this._buf.length))) {
					_G = 1;
					continue;
				}
				return;
				_G = 1;
				continue;
			case 1:
				lA2 = (new JA_B(((Math.imul((((this._count + p0))|0), 2))|0)));
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((this._buf), 0, lA2, 0, this._count);
				this._buf = (lA2);
				return;
			default:
				break;
		}
	}
	return;
};
java_io_ByteArrayOutputStream.prototype["write([BII)V"] = function(p0, p1, p2) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				com_jtransc_JTranscArrays["checkOffsetAndCount(III)V"]((p0).length, p1, p2);
				if (((p2 != 0))) {
					_G = 1;
					continue;
				}
				return;
				_G = 1;
				continue;
			case 1:
				this["expand(I)V"](p2);
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((p0), p1, (this._buf), this._count, p2);
				this._count = (((this._count + p2))|0);
				return;
			default:
				break;
		}
	}
	return;
};
function java_io_IOException() {
}
java_io_IOException.prototype = Object.create(java_lang_Exception.prototype);
java_io_IOException.prototype.constructor = java_io_IOException;
java_io_IOException.SI = function(){};
java_io_IOException.prototype.__JT__CLASS_ID = java_io_IOException.__JT__CLASS_ID = 803;
java_io_IOException.prototype.__JT__CLASS_IDS = java_io_IOException.__JT__CLASS_IDS = [803,680,681,656,658];
function com_jtransc_time_JTranscClock$Impl() {
}
com_jtransc_time_JTranscClock$Impl.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_time_JTranscClock$Impl.prototype.constructor = com_jtransc_time_JTranscClock$Impl;
com_jtransc_time_JTranscClock$Impl.prototype._parent = null;
com_jtransc_time_JTranscClock$Impl.prototype.___id = 0;
com_jtransc_time_JTranscClock$Impl.SI = function(){};
com_jtransc_time_JTranscClock$Impl.prototype.__JT__CLASS_ID = com_jtransc_time_JTranscClock$Impl.__JT__CLASS_ID = 802;
com_jtransc_time_JTranscClock$Impl.prototype.__JT__CLASS_IDS = com_jtransc_time_JTranscClock$Impl.__JT__CLASS_IDS = [802,656];
com_jtransc_time_JTranscClock$Impl.prototype["com.jtransc.time.JTranscClock$Impl<init>(Lcom/jtransc/time/JTranscClock$Impl;)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	this._parent = p0;
	return this;
	return this;
};
com_jtransc_time_JTranscClock$Impl.prototype["fastTime()D"] = function() { 
	return N.getTime();
};
function com_jtransc_time_JTranscClock$1() {
}
com_jtransc_time_JTranscClock$1.prototype = Object.create(com_jtransc_time_JTranscClock$Impl.prototype);
com_jtransc_time_JTranscClock$1.prototype.constructor = com_jtransc_time_JTranscClock$1;
com_jtransc_time_JTranscClock$1.prototype._parent = null;
com_jtransc_time_JTranscClock$1.SI = function(){};
com_jtransc_time_JTranscClock$1.prototype.__JT__CLASS_ID = com_jtransc_time_JTranscClock$1.__JT__CLASS_ID = 801;
com_jtransc_time_JTranscClock$1.prototype.__JT__CLASS_IDS = com_jtransc_time_JTranscClock$1.__JT__CLASS_IDS = [801,802,656];
com_jtransc_time_JTranscClock$1.prototype["com.jtransc.time.JTranscClock$1<init>(Lcom/jtransc/time/JTranscClock$Impl;)V"] = function(p0) { 
	(this)["com.jtransc.time.JTranscClock$Impl<init>(Lcom/jtransc/time/JTranscClock$Impl;)V"](p0);
	return this;
	return this;
};
function com_jtransc_time_JTranscClock() {
}
com_jtransc_time_JTranscClock.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_time_JTranscClock.prototype.constructor = com_jtransc_time_JTranscClock;
com_jtransc_time_JTranscClock.prototype.___id = 0;
com_jtransc_time_JTranscClock.SI = function() { 
	com_jtransc_time_JTranscClock._impl = null;
	com_jtransc_time_JTranscClock["com.jtransc.time.JTranscClock<clinit>()V"]();
};
com_jtransc_time_JTranscClock.prototype.__JT__CLASS_ID = com_jtransc_time_JTranscClock.__JT__CLASS_ID = 800;
com_jtransc_time_JTranscClock.prototype.__JT__CLASS_IDS = com_jtransc_time_JTranscClock.__JT__CLASS_IDS = [800,656];
com_jtransc_time_JTranscClock.prototype["com.jtransc.time.JTranscClock<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_time_JTranscClock["com.jtransc.time.JTranscClock<clinit>()V"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new com_jtransc_time_JTranscClock$1()));
	fA0 = tA0;
	(tA0)["com.jtransc.time.JTranscClock$1<init>(Lcom/jtransc/time/JTranscClock$Impl;)V"](null);
	com_jtransc_time_JTranscClock._impl = (fA0);
	return;
};
function com_jtransc_JTranscVersion() {
}
com_jtransc_JTranscVersion.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_JTranscVersion.prototype.constructor = com_jtransc_JTranscVersion;
com_jtransc_JTranscVersion.prototype.___id = 0;
com_jtransc_JTranscVersion.SI = function(){};
com_jtransc_JTranscVersion.prototype.__JT__CLASS_ID = com_jtransc_JTranscVersion.__JT__CLASS_ID = 799;
com_jtransc_JTranscVersion.prototype.__JT__CLASS_IDS = com_jtransc_JTranscVersion.__JT__CLASS_IDS = [799,656];
com_jtransc_JTranscVersion.prototype["com.jtransc.JTranscVersion<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_JTranscVersion["getVersion()Ljava/lang/String;"] = function() { 
	return S[122];
};
function Benchmark$Task() {
}
Benchmark$Task.prototype = Object.create(java_lang_Object_base.prototype);
Benchmark$Task.prototype.constructor = Benchmark$Task;
Benchmark$Task.SI = function(){};
Benchmark$Task.prototype.__JT__CLASS_ID = Benchmark$Task.__JT__CLASS_ID = 751;
Benchmark$Task.prototype.__JT__CLASS_IDS = Benchmark$Task.__JT__CLASS_IDS = [751,656];
Benchmark$Task.prototype["run()I"] = function() { N.methodWithoutBody('Benchmark$Task.run') };
function Benchmark$20() {
}
Benchmark$20.prototype = Object.create(java_lang_Object.prototype);
Benchmark$20.prototype.constructor = Benchmark$20;
Benchmark$20.prototype._val_farray = null;
Benchmark$20.prototype.___id = 0;
Benchmark$20.SI = function(){};
Benchmark$20.prototype.__JT__CLASS_ID = Benchmark$20.__JT__CLASS_ID = 798;
Benchmark$20.prototype.__JT__CLASS_IDS = Benchmark$20.__JT__CLASS_IDS = [798,656,751];
Benchmark$20.prototype["Benchmark$20<init>([F)V"] = function(p0) { 
	this._val_farray = p0;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$20.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= 1000000))) {
					_G = 2;
					continue;
				}
				this._val_farray.data[lI1] = Math.fround(+(((Math.imul(lI1, 1000))|0)));
				lI1 = (((lI1 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (((((this._val_farray.data[7]))|0))|0);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$21() {
}
Benchmark$21.prototype = Object.create(java_lang_Object.prototype);
Benchmark$21.prototype.constructor = Benchmark$21;
Benchmark$21.prototype._val_darray = null;
Benchmark$21.prototype.___id = 0;
Benchmark$21.SI = function(){};
Benchmark$21.prototype.__JT__CLASS_ID = Benchmark$21.__JT__CLASS_ID = 797;
Benchmark$21.prototype.__JT__CLASS_IDS = Benchmark$21.__JT__CLASS_IDS = [797,656,751];
Benchmark$21.prototype["Benchmark$21<init>([D)V"] = function(p0) { 
	this._val_darray = p0;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$21.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= 1000000))) {
					_G = 2;
					continue;
				}
				this._val_darray.data[lI1] = +(((Math.imul(lI1, 1000))|0));
				lI1 = (((lI1 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (((((this._val_darray.data[7]))|0))|0);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$22() {
}
Benchmark$22.prototype = Object.create(java_lang_Object.prototype);
Benchmark$22.prototype.constructor = Benchmark$22;
Benchmark$22.prototype.___id = 0;
Benchmark$22.SI = function(){};
Benchmark$22.prototype.__JT__CLASS_ID = Benchmark$22.__JT__CLASS_ID = 796;
Benchmark$22.prototype.__JT__CLASS_IDS = Benchmark$22.__JT__CLASS_IDS = [796,656,751];
Benchmark$22.prototype["Benchmark$22<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$22.prototype["run()I"] = function() { 
	var _G = 0, lA1 = null, lI2 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				lA1 = fA0;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 100000))) {
					_G = 2;
					continue;
				}
				(lA1)["append(I)Ljava/lang/StringBuilder;"](lI2);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (lA1)["toString()Ljava/lang/String;"]()["hashCode()I"]();
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$27() {
}
Benchmark$27.prototype = Object.create(java_lang_Object.prototype);
Benchmark$27.prototype.constructor = Benchmark$27;
Benchmark$27.prototype.___id = 0;
Benchmark$27.SI = function(){};
Benchmark$27.prototype.__JT__CLASS_ID = Benchmark$27.__JT__CLASS_ID = 795;
Benchmark$27.prototype.__JT__CLASS_IDS = Benchmark$27.__JT__CLASS_IDS = [795,656,751];
Benchmark$27.prototype["Benchmark$27<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$27.prototype["run()I"] = function() { 
	var lA1 = null, lA2 = null, _G = 0, lI3 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = com_jtransc_simd_MutableMatrixFloat32x4x4["create()Lcom/jtransc/simd/MutableMatrixFloat32x4x4;"]();
				lA1["setTo(FFFFFFFFFFFFFFFF)V"](1.0, 9.0, 1.0, 7.0, 3.0, 2.0, 4.0, 5.0, 3.0, 7.0, 3.0, 3.0, 3.0, 8.0, 4.0, 4.0);
				lA2 = com_jtransc_simd_MutableMatrixFloat32x4x4["create()Lcom/jtransc/simd/MutableMatrixFloat32x4x4;"]();
				lA2["setTo(FFFFFFFFFFFFFFFF)V"](2.0, 3.0, 4.0, 5.0, 2.0, 3.0, 4.0, 5.0, 2.0, 3.0, 4.0, 5.0, 2.0, 3.0, 4.0, 5.0);
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 100000))) {
					_G = 2;
					continue;
				}
				lA1["setToMul44(Lcom/jtransc/simd/MutableMatrixFloat32x4x4;Lcom/jtransc/simd/MutableMatrixFloat32x4x4;)V"](lA1, lA2);
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return ((((lA1["getSumAll()F"]())|0))|0);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$28() {
}
Benchmark$28.prototype = Object.create(java_lang_Object.prototype);
Benchmark$28.prototype.constructor = Benchmark$28;
Benchmark$28.prototype.___id = 0;
Benchmark$28.SI = function(){};
Benchmark$28.prototype.__JT__CLASS_ID = Benchmark$28.__JT__CLASS_ID = 794;
Benchmark$28.prototype.__JT__CLASS_IDS = Benchmark$28.__JT__CLASS_IDS = [794,656,751];
Benchmark$28.prototype["Benchmark$28<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$28.prototype["run()I"] = function() { 
	var _G = 0, lA1 = null, lI2 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				lA1 = fA0;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 100000))) {
					_G = 2;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[41]);
				(lA1)["append(C)Ljava/lang/StringBuilder;"](119);
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[123]);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (lA1)["toString()Ljava/lang/String;"]()["length()I"]();
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$29() {
}
Benchmark$29.prototype = Object.create(java_lang_Object.prototype);
Benchmark$29.prototype.constructor = Benchmark$29;
Benchmark$29.prototype.___id = 0;
Benchmark$29.SI = function(){};
Benchmark$29.prototype.__JT__CLASS_ID = Benchmark$29.__JT__CLASS_ID = 793;
Benchmark$29.prototype.__JT__CLASS_IDS = Benchmark$29.__JT__CLASS_IDS = [793,656,751];
Benchmark$29.prototype["Benchmark$29<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$29.prototype["run()I"] = function() { 
	var _G = 0, lA1 = null, lI2 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				lA1 = fA0;
				(lA1)["ensureCapacity(I)V"](1000000);
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 100000))) {
					_G = 2;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[41]);
				(lA1)["append(C)Ljava/lang/StringBuilder;"](119);
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[123]);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (lA1)["toString()Ljava/lang/String;"]()["length()I"]();
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$23() {
}
Benchmark$23.prototype = Object.create(java_lang_Object.prototype);
Benchmark$23.prototype.constructor = Benchmark$23;
Benchmark$23.prototype.___id = 0;
Benchmark$23.SI = function(){};
Benchmark$23.prototype.__JT__CLASS_ID = Benchmark$23.__JT__CLASS_ID = 792;
Benchmark$23.prototype.__JT__CLASS_IDS = Benchmark$23.__JT__CLASS_IDS = [792,656,751];
Benchmark$23.prototype["Benchmark$23<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$23.prototype["run()I"] = function() { 
	var _G = 0, lA1 = null, lI2 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				lA1 = fA0;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 100000))) {
					_G = 2;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[124]);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (lA1)["toString()Ljava/lang/String;"]()["hashCode()I"]();
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$24() {
}
Benchmark$24.prototype = Object.create(java_lang_Object.prototype);
Benchmark$24.prototype.constructor = Benchmark$24;
Benchmark$24.prototype.___id = 0;
Benchmark$24.SI = function(){};
Benchmark$24.prototype.__JT__CLASS_ID = Benchmark$24.__JT__CLASS_ID = 791;
Benchmark$24.prototype.__JT__CLASS_IDS = Benchmark$24.__JT__CLASS_IDS = [791,656,751];
Benchmark$24.prototype["Benchmark$24<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$24.prototype["run()I"] = function() { 
	var _G = 0, lI3 = 0, lJ1 = N.lnew(0, 0);
	while (true) {
		switch (_G) {
			case 0:
				lJ1 = N.lnew(0, 0);
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 10000))) {
					_G = 2;
					continue;
				}
				lJ1 = N.ladd(N.lmul(N.lnew(0, 17777), N.i2j(lI3)), N.ldiv(lJ1, N.lnew(0, 3)));
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return N.j2i(lJ1);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$25() {
}
Benchmark$25.prototype = Object.create(java_lang_Object.prototype);
Benchmark$25.prototype.constructor = Benchmark$25;
Benchmark$25.prototype.___id = 0;
Benchmark$25.SI = function(){};
Benchmark$25.prototype.__JT__CLASS_ID = Benchmark$25.__JT__CLASS_ID = 790;
Benchmark$25.prototype.__JT__CLASS_IDS = Benchmark$25.__JT__CLASS_IDS = [790,656,751];
Benchmark$25.prototype["Benchmark$25<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$25.prototype["run()I"] = function() { 
	var _G = 0, lI3 = 0, lA1 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = com_jtransc_simd_MutableFloat32x4["create()Lcom/jtransc/simd/MutableFloat32x4;"]();
				lA2 = com_jtransc_simd_MutableFloat32x4["create(FFFF)Lcom/jtransc/simd/MutableFloat32x4;"](2.0, 3.0, 4.0, 5.0);
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 1000000))) {
					_G = 2;
					continue;
				}
				lA1["setToAdd(Lcom/jtransc/simd/MutableFloat32x4;Lcom/jtransc/simd/MutableFloat32x4;)V"](lA1, lA2);
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (((((((((((((lA1["getX()F"]())|0))|0) + ((((lA1["getY()F"]())|0))|0)))|0) + ((((lA1["getZ()F"]())|0))|0)))|0) + ((((lA1["getW()F"]())|0))|0)))|0);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$26() {
}
Benchmark$26.prototype = Object.create(java_lang_Object.prototype);
Benchmark$26.prototype.constructor = Benchmark$26;
Benchmark$26.prototype.___id = 0;
Benchmark$26.SI = function(){};
Benchmark$26.prototype.__JT__CLASS_ID = Benchmark$26.__JT__CLASS_ID = 789;
Benchmark$26.prototype.__JT__CLASS_IDS = Benchmark$26.__JT__CLASS_IDS = [789,656,751];
Benchmark$26.prototype["Benchmark$26<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$26.prototype["run()I"] = function() { 
	var _G = 0, lI3 = 0, lA1 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = com_jtransc_simd_Float32x4["create(FFFF)Lcom/jtransc/simd/Float32x4;"](0.0, 0.0, 0.0, 0.0);
				lA2 = com_jtransc_simd_Float32x4["create(FFFF)Lcom/jtransc/simd/Float32x4;"](2.0, 3.0, 4.0, 5.0);
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 1000000))) {
					_G = 2;
					continue;
				}
				lA1 = com_jtransc_simd_Float32x4["add(Lcom/jtransc/simd/Float32x4;Lcom/jtransc/simd/Float32x4;)Lcom/jtransc/simd/Float32x4;"](lA1, lA2);
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (((((((((((((com_jtransc_simd_Float32x4["getX(Lcom/jtransc/simd/Float32x4;)F"](lA1))|0))|0) + ((((com_jtransc_simd_Float32x4["getY(Lcom/jtransc/simd/Float32x4;)F"](lA1))|0))|0)))|0) + ((((com_jtransc_simd_Float32x4["getZ(Lcom/jtransc/simd/Float32x4;)F"](lA1))|0))|0)))|0) + ((((com_jtransc_simd_Float32x4["getW(Lcom/jtransc/simd/Float32x4;)F"](lA1))|0))|0)))|0);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$10() {
}
Benchmark$10.prototype = Object.create(java_lang_Object.prototype);
Benchmark$10.prototype.constructor = Benchmark$10;
Benchmark$10.prototype.___id = 0;
Benchmark$10.SI = function(){};
Benchmark$10.prototype.__JT__CLASS_ID = Benchmark$10.__JT__CLASS_ID = 788;
Benchmark$10.prototype.__JT__CLASS_IDS = Benchmark$10.__JT__CLASS_IDS = [788,656,751];
Benchmark$10.prototype["Benchmark$10<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$10.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 305419896;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 1000000))) {
					_G = 2;
					continue;
				}
				lI1 = (((lI1 + ((((((lI1 >>> lI2))|0) + (((lI1 >>> ((-(lI2))|0)))|0)))|0)))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$11() {
}
Benchmark$11.prototype = Object.create(java_lang_Object.prototype);
Benchmark$11.prototype.constructor = Benchmark$11;
Benchmark$11.prototype.___id = 0;
Benchmark$11.SI = function(){};
Benchmark$11.prototype.__JT__CLASS_ID = Benchmark$11.__JT__CLASS_ID = 787;
Benchmark$11.prototype.__JT__CLASS_IDS = Benchmark$11.__JT__CLASS_IDS = [787,656,751];
Benchmark$11.prototype["Benchmark$11<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$11.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 1000000))) {
					_G = 2;
					continue;
				}
				lI1 = (((lI1 + Benchmark["calc(II)I"](lI1, lI2)))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$16() {
}
Benchmark$16.prototype = Object.create(java_lang_Object.prototype);
Benchmark$16.prototype.constructor = Benchmark$16;
Benchmark$16.prototype._val_barray = null;
Benchmark$16.prototype.___id = 0;
Benchmark$16.SI = function(){};
Benchmark$16.prototype.__JT__CLASS_ID = Benchmark$16.__JT__CLASS_ID = 786;
Benchmark$16.prototype.__JT__CLASS_IDS = Benchmark$16.__JT__CLASS_IDS = [786,656,751];
Benchmark$16.prototype["Benchmark$16<init>([B)V"] = function(p0) { 
	this._val_barray = p0;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$16.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= 1000000))) {
					_G = 2;
					continue;
				}
				this._val_barray.data[lI1] = ((((Math.imul(lI1, 123456711))|0))<<24>>24);
				lI1 = (((lI1 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (((this._val_barray.data[7]))|0);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$17() {
}
Benchmark$17.prototype = Object.create(java_lang_Object.prototype);
Benchmark$17.prototype.constructor = Benchmark$17;
Benchmark$17.prototype._val_sarray = null;
Benchmark$17.prototype.___id = 0;
Benchmark$17.SI = function(){};
Benchmark$17.prototype.__JT__CLASS_ID = Benchmark$17.__JT__CLASS_ID = 785;
Benchmark$17.prototype.__JT__CLASS_IDS = Benchmark$17.__JT__CLASS_IDS = [785,656,751];
Benchmark$17.prototype["Benchmark$17<init>([S)V"] = function(p0) { 
	this._val_sarray = p0;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$17.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= 1000000))) {
					_G = 2;
					continue;
				}
				this._val_sarray.data[lI1] = ((((Math.imul(lI1, 1000))|0))<<16>>16);
				lI1 = (((lI1 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (((this._val_sarray.data[7]))|0);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$18() {
}
Benchmark$18.prototype = Object.create(java_lang_Object.prototype);
Benchmark$18.prototype.constructor = Benchmark$18;
Benchmark$18.prototype._val_carray = null;
Benchmark$18.prototype.___id = 0;
Benchmark$18.SI = function(){};
Benchmark$18.prototype.__JT__CLASS_ID = Benchmark$18.__JT__CLASS_ID = 784;
Benchmark$18.prototype.__JT__CLASS_IDS = Benchmark$18.__JT__CLASS_IDS = [784,656,751];
Benchmark$18.prototype["Benchmark$18<init>([C)V"] = function(p0) { 
	this._val_carray = p0;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$18.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= 1000000))) {
					_G = 2;
					continue;
				}
				this._val_carray.data[lI1] = ((((Math.imul(lI1, 1000))|0))&0xFFFF);
				lI1 = (((lI1 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (((this._val_carray.data[7]))|0);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$19() {
}
Benchmark$19.prototype = Object.create(java_lang_Object.prototype);
Benchmark$19.prototype.constructor = Benchmark$19;
Benchmark$19.prototype._val_iarray = null;
Benchmark$19.prototype.___id = 0;
Benchmark$19.SI = function(){};
Benchmark$19.prototype.__JT__CLASS_ID = Benchmark$19.__JT__CLASS_ID = 783;
Benchmark$19.prototype.__JT__CLASS_IDS = Benchmark$19.__JT__CLASS_IDS = [783,656,751];
Benchmark$19.prototype["Benchmark$19<init>([I)V"] = function(p0) { 
	this._val_iarray = p0;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$19.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= 1000000))) {
					_G = 2;
					continue;
				}
				this._val_iarray.data[lI1] = ((Math.imul(lI1, 1000))|0);
				lI1 = (((lI1 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (this._val_iarray.data[7]);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$12() {
}
Benchmark$12.prototype = Object.create(java_lang_Object.prototype);
Benchmark$12.prototype.constructor = Benchmark$12;
Benchmark$12.prototype.___id = 0;
Benchmark$12.SI = function(){};
Benchmark$12.prototype.__JT__CLASS_ID = Benchmark$12.__JT__CLASS_ID = 782;
Benchmark$12.prototype.__JT__CLASS_IDS = Benchmark$12.__JT__CLASS_IDS = [782,656,751];
Benchmark$12.prototype["Benchmark$12<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$12.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 1000000))) {
					_G = 2;
					continue;
				}
				lI1 = (((lI1 + this["calc(II)I"](lI1, lI2)))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
Benchmark$12.prototype["calc(II)I"] = function(p0, p1) { 
	return ((Math.imul((((p0 + p1))|0), (((p0 + p1))|0)))|0);
};
function Benchmark$13() {
}
Benchmark$13.prototype = Object.create(java_lang_Object.prototype);
Benchmark$13.prototype.constructor = Benchmark$13;
Benchmark$13.prototype.___id = 0;
Benchmark$13.SI = function(){};
Benchmark$13.prototype.__JT__CLASS_ID = Benchmark$13.__JT__CLASS_ID = 781;
Benchmark$13.prototype.__JT__CLASS_IDS = Benchmark$13.__JT__CLASS_IDS = [781,656,751];
Benchmark$13.prototype["Benchmark$13<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$13.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 1;
				lI2 = 1;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 1000000))) {
					_G = 2;
					continue;
				}
				lI1 = (((lI1 + this["calc(II)I"](lI1, lI2)))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
Benchmark$13.prototype["calc(II)I"] = function(p0, p1) { 
	return ((((((p0 - p1))|0) / (((p0 + p1))|0)))|0);
};
function Benchmark$14() {
}
Benchmark$14.prototype = Object.create(java_lang_Object.prototype);
Benchmark$14.prototype.constructor = Benchmark$14;
Benchmark$14.prototype.___id = 0;
Benchmark$14.SI = function(){};
Benchmark$14.prototype.__JT__CLASS_ID = Benchmark$14.__JT__CLASS_ID = 780;
Benchmark$14.prototype.__JT__CLASS_IDS = Benchmark$14.__JT__CLASS_IDS = [780,656,751];
Benchmark$14.prototype["Benchmark$14<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$14.prototype["run()I"] = function() { 
	var _G = 0, lA3 = null, lA4 = null, lI1 = 0, lI2 = 0, lI5 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 1;
				lI2 = this["rand(I)I"](2);
				lA3 = this["genObj(I)Ljava/lang/Object;"](((((((lI2 + 0))|0) % 2))|0));
				lA4 = this["genObj(I)Ljava/lang/Object;"](((((((lI2 + 1))|0) % 2))|0));
				lI5 = 1;
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= 1000000))) {
					_G = 2;
					continue;
				}
				if (!((lA3 instanceof Benchmark$Test1))) {
					_G = 3;
					continue;
				}
				lI1 = (((lI1 + (((lI5 - 1))|0)))|0);
				_G = 4;
				continue;
			case 3:
				if (!((lA3 instanceof Benchmark$Test2))) {
					_G = 4;
					continue;
				}
				lI1 = (((lI1 + (((lI5 + 2))|0)))|0);
				_G = 4;
				continue;
			case 4:
				if (!((lA4 instanceof Benchmark$Test1))) {
					_G = 5;
					continue;
				}
				lI1 = (((lI1 + (((lI5 - 3))|0)))|0);
				_G = 6;
				continue;
			case 5:
				if (!((lA4 instanceof Benchmark$Test2))) {
					_G = 6;
					continue;
				}
				lI1 = (((lI1 + (((lI5 + 4))|0)))|0);
				_G = 6;
				continue;
			case 6:
				lI5 = (((lI5 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
Benchmark$14.prototype["rand(I)I"] = function(p0) { 
	return N.j2i(N.lrem(java_lang_System["currentTimeMillis()J"](), N.i2j(p0)));
};
Benchmark$14.prototype["genObj(I)Ljava/lang/Object;"] = function(p0) { 
	var _G = 0, fA0 = null, tA1 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				switch (p0) {
					case 0:
						_G = 2;
						continue;
					default:
						_G = 1;
						continue;
				}
				_G = 2;
				continue;
			case 2:
				tA1 = ((new Benchmark$Test1()));
				fA0 = tA1;
				(tA1)["Benchmark$Test1<init>(LBenchmark$1;)V"](null);
				return fA0;
			case 1:
				tA0 = ((new Benchmark$Test2()));
				fA0 = tA0;
				(tA0)["Benchmark$Test2<init>(LBenchmark$1;)V"](null);
				return fA0;
			default:
				break;
		}
	}
	return null;
};
function Benchmark$15() {
}
Benchmark$15.prototype = Object.create(java_lang_Object.prototype);
Benchmark$15.prototype.constructor = Benchmark$15;
Benchmark$15.prototype._val_srcI = null;
Benchmark$15.prototype._val_dstI = null;
Benchmark$15.prototype.___id = 0;
Benchmark$15.SI = function(){};
Benchmark$15.prototype.__JT__CLASS_ID = Benchmark$15.__JT__CLASS_ID = 779;
Benchmark$15.prototype.__JT__CLASS_IDS = Benchmark$15.__JT__CLASS_IDS = [779,656,751];
Benchmark$15.prototype["Benchmark$15<init>([I[I)V"] = function(p0, p1) { 
	this._val_srcI = p0;
	this._val_dstI = p1;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$15.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI1 >= 1024))) {
					_G = 2;
					continue;
				}
				java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((this._val_srcI), 0, (this._val_dstI), lI1, 8192);
				lI1 = (((lI1 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return 0;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$9() {
}
Benchmark$9.prototype = Object.create(java_lang_Object.prototype);
Benchmark$9.prototype.constructor = Benchmark$9;
Benchmark$9.prototype.___id = 0;
Benchmark$9.SI = function(){};
Benchmark$9.prototype.__JT__CLASS_ID = Benchmark$9.__JT__CLASS_ID = 778;
Benchmark$9.prototype.__JT__CLASS_IDS = Benchmark$9.__JT__CLASS_IDS = [778,656,751];
Benchmark$9.prototype["Benchmark$9<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$9.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 305419896;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 1000000))) {
					_G = 2;
					continue;
				}
				lI1 = (((lI1 + ((((((lI1 >> lI2))|0) + (((lI1 >> ((-(lI2))|0)))|0)))|0)))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$8() {
}
Benchmark$8.prototype = Object.create(java_lang_Object.prototype);
Benchmark$8.prototype.constructor = Benchmark$8;
Benchmark$8.prototype.___id = 0;
Benchmark$8.SI = function(){};
Benchmark$8.prototype.__JT__CLASS_ID = Benchmark$8.__JT__CLASS_ID = 777;
Benchmark$8.prototype.__JT__CLASS_IDS = Benchmark$8.__JT__CLASS_IDS = [777,656,751];
Benchmark$8.prototype["Benchmark$8<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$8.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 305419896;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 1000000))) {
					_G = 2;
					continue;
				}
				lI1 = (((lI1 + ((((((lI1 << lI2))|0) + (((lI1 << ((-(lI2))|0)))|0)))|0)))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$7() {
}
Benchmark$7.prototype = Object.create(java_lang_Object.prototype);
Benchmark$7.prototype.constructor = Benchmark$7;
Benchmark$7.prototype.___id = 0;
Benchmark$7.SI = function(){};
Benchmark$7.prototype.__JT__CLASS_ID = Benchmark$7.__JT__CLASS_ID = 776;
Benchmark$7.prototype.__JT__CLASS_IDS = Benchmark$7.__JT__CLASS_IDS = [776,656,751];
Benchmark$7.prototype["Benchmark$7<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$7.prototype["run()I"] = function() { 
	var _G = 0, lI3 = 0, lJ1 = N.lnew(0, 0);
	while (true) {
		switch (_G) {
			case 0:
				lJ1 = N.lnew(0, 305419896);
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 1000000))) {
					_G = 2;
					continue;
				}
				lJ1 = N.ladd(lJ1, N.lushr(lJ1, 1));
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return N.j2i(lJ1);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$41() {
}
Benchmark$41.prototype = Object.create(java_lang_Object.prototype);
Benchmark$41.prototype.constructor = Benchmark$41;
Benchmark$41.prototype._val_bytes = null;
Benchmark$41.prototype.___id = 0;
Benchmark$41.SI = function(){};
Benchmark$41.prototype.__JT__CLASS_ID = Benchmark$41.__JT__CLASS_ID = 775;
Benchmark$41.prototype.__JT__CLASS_IDS = Benchmark$41.__JT__CLASS_IDS = [775,656,751];
Benchmark$41.prototype["Benchmark$41<init>([B)V"] = function(p0) { 
	this._val_bytes = p0;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$41.prototype["run()I"] = function() { 
	var _G = 0, lA1 = null, lA2 = null, fI0 = 0, lI3 = 0, fA0 = null, tA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						_G = 1;
						continue;
					case 1:
						lA1 = (new JA_B(131072));
						tA0 = ((new java_util_zip_Deflater()));
						fA0 = tA0;
						(tA0)["java.util.zip.Deflater<init>(IZ)V"](9, false);
						lA2 = fA0;
						(lA2)["setInput([BII)V"](this._val_bytes, 0, this._val_bytes.length);
						lI3 = (lA2)["deflate([BIII)I"]((lA1), 0, lA1.length, 3);
						fI0 = lI3;
						_G = 2;
						continue;
					case 2:return fI0; 
					case 3:
						fA0 = (J__exception__);
						lA1 = fA0;
						(lA1)["printStackTrace()V"]();
						return 0;
					default:
						break;
				}
			}
			return 0;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 3;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return 0;
};
function Benchmark$6() {
}
Benchmark$6.prototype = Object.create(java_lang_Object.prototype);
Benchmark$6.prototype.constructor = Benchmark$6;
Benchmark$6.prototype.___id = 0;
Benchmark$6.SI = function(){};
Benchmark$6.prototype.__JT__CLASS_ID = Benchmark$6.__JT__CLASS_ID = 774;
Benchmark$6.prototype.__JT__CLASS_IDS = Benchmark$6.__JT__CLASS_IDS = [774,656,751];
Benchmark$6.prototype["Benchmark$6<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$6.prototype["run()I"] = function() { 
	var _G = 0, lI3 = 0, lJ1 = N.lnew(0, 0);
	while (true) {
		switch (_G) {
			case 0:
				lJ1 = N.lnew(0, 305419896);
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 1000000))) {
					_G = 2;
					continue;
				}
				lJ1 = N.ladd(lJ1, N.lshr(lJ1, 1));
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return N.j2i(lJ1);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$42() {
}
Benchmark$42.prototype = Object.create(java_lang_Object.prototype);
Benchmark$42.prototype.constructor = Benchmark$42;
Benchmark$42.prototype._val_bytes = null;
Benchmark$42.prototype.___id = 0;
Benchmark$42.SI = function(){};
Benchmark$42.prototype.__JT__CLASS_ID = Benchmark$42.__JT__CLASS_ID = 773;
Benchmark$42.prototype.__JT__CLASS_IDS = Benchmark$42.__JT__CLASS_IDS = [773,656,751];
Benchmark$42.prototype["Benchmark$42<init>([B)V"] = function(p0) { 
	this._val_bytes = p0;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$42.prototype["run()I"] = function() { 
	var _G = 0, lA1 = null, lA2 = null, fI0 = 0, lI3 = 0, fA0 = null, tA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						_G = 1;
						continue;
					case 1:
						lA1 = (new JA_B(131072));
						tA0 = ((new com_jtransc_compression_jzlib_Deflater()));
						fA0 = tA0;
						(tA0)["com.jtransc.compression.jzlib.Deflater<init>(IZ)V"](9, false);
						lA2 = fA0;
						(lA2)["setInput([BIIZ)V"](this._val_bytes, 0, this._val_bytes.length, false);
						(lA2)["setOutput([BII)V"]((lA1), 0, lA1.length);
						lI3 = (lA2)["deflate(I)I"](3);
						fI0 = lI3;
						_G = 2;
						continue;
					case 2:return fI0; 
					case 3:
						fA0 = (J__exception__);
						lA1 = fA0;
						(lA1)["printStackTrace()V"]();
						return 0;
					default:
						break;
				}
			}
			return 0;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 3;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return 0;
};
function Benchmark$5() {
}
Benchmark$5.prototype = Object.create(java_lang_Object.prototype);
Benchmark$5.prototype.constructor = Benchmark$5;
Benchmark$5.prototype.___id = 0;
Benchmark$5.SI = function(){};
Benchmark$5.prototype.__JT__CLASS_ID = Benchmark$5.__JT__CLASS_ID = 772;
Benchmark$5.prototype.__JT__CLASS_IDS = Benchmark$5.__JT__CLASS_IDS = [772,656,751];
Benchmark$5.prototype["Benchmark$5<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$5.prototype["run()I"] = function() { 
	var _G = 0, lI3 = 0, lJ1 = N.lnew(0, 0);
	while (true) {
		switch (_G) {
			case 0:
				lJ1 = N.lnew(0, 305419896);
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 1000000))) {
					_G = 2;
					continue;
				}
				lJ1 = N.ladd(lJ1, N.lshl(lJ1, 1));
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return N.j2i(lJ1);
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$43() {
}
Benchmark$43.prototype = Object.create(java_lang_Object.prototype);
Benchmark$43.prototype.constructor = Benchmark$43;
Benchmark$43.prototype.___id = 0;
Benchmark$43.SI = function(){};
Benchmark$43.prototype.__JT__CLASS_ID = Benchmark$43.__JT__CLASS_ID = 771;
Benchmark$43.prototype.__JT__CLASS_IDS = Benchmark$43.__JT__CLASS_IDS = [771,656,751];
Benchmark$43.prototype["Benchmark$43<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$43.prototype["run()I"] = function() { 
	var _G = 0, lA1 = null, lA2 = null, lI3 = 0, lI4 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_util_Random()));
				fA0 = tA0;
				(tA0)["java.util.Random<init>(J)V"](N.lnew(0, 0));
				lA1 = fA0;
				lA2 = (new JA_B(65536));
				lI3 = 0;
				lI4 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI4 >= lA2.length))) {
					_G = 2;
					continue;
				}
				(lA2).data[lI4] = (((lA1)["nextInt()I"]())<<24>>24);
				lI3 = (((lI3 + ((lA2).data[lI4])))|0);
				lI4 = (((lI4 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI3;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$4() {
}
Benchmark$4.prototype = Object.create(java_lang_Object.prototype);
Benchmark$4.prototype.constructor = Benchmark$4;
Benchmark$4.prototype.___id = 0;
Benchmark$4.SI = function(){};
Benchmark$4.prototype.__JT__CLASS_ID = Benchmark$4.__JT__CLASS_ID = 770;
Benchmark$4.prototype.__JT__CLASS_IDS = Benchmark$4.__JT__CLASS_IDS = [770,656,751];
Benchmark$4.prototype["Benchmark$4<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$4.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 305419896;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 1000000))) {
					_G = 2;
					continue;
				}
				lI1 = (((lI1 + (((lI1 >>> 1))|0)))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$44() {
}
Benchmark$44.prototype = Object.create(java_lang_Object.prototype);
Benchmark$44.prototype.constructor = Benchmark$44;
Benchmark$44.prototype.___id = 0;
Benchmark$44.SI = function(){};
Benchmark$44.prototype.__JT__CLASS_ID = Benchmark$44.__JT__CLASS_ID = 769;
Benchmark$44.prototype.__JT__CLASS_IDS = Benchmark$44.__JT__CLASS_IDS = [769,656,751];
Benchmark$44.prototype["Benchmark$44<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$44.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0, fA0 = null, tA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						lI1 = 0;
						lI2 = 0;
						_G = 1;
						continue;
					case 1:
						if (((lI2 >= 1000))) {
							_G = 2;
							continue;
						}
						_G = 3;
						continue;
					case 3:
						tA0 = ((new java_lang_Throwable()));
						fA0 = tA0;
						(tA0)["java.lang.Throwable<init>()V"]();
						throw new WrappedError(fA0);
						_G = 4;
						continue;
					case 4:
						fA0 = (J__exception__);
						lI1 = (((lI1 + 1))|0);
						lI2 = (((lI2 + 1))|0);
						_G = 1;
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
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 3)) && ((_G < 4)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 4;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return 0;
};
function Benchmark$3() {
}
Benchmark$3.prototype = Object.create(java_lang_Object.prototype);
Benchmark$3.prototype.constructor = Benchmark$3;
Benchmark$3.prototype.___id = 0;
Benchmark$3.SI = function(){};
Benchmark$3.prototype.__JT__CLASS_ID = Benchmark$3.__JT__CLASS_ID = 768;
Benchmark$3.prototype.__JT__CLASS_IDS = Benchmark$3.__JT__CLASS_IDS = [768,656,751];
Benchmark$3.prototype["Benchmark$3<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$3.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 305419896;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 1000000))) {
					_G = 2;
					continue;
				}
				lI1 = (((lI1 + (((lI1 >> 1))|0)))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$2() {
}
Benchmark$2.prototype = Object.create(java_lang_Object.prototype);
Benchmark$2.prototype.constructor = Benchmark$2;
Benchmark$2.prototype.___id = 0;
Benchmark$2.SI = function(){};
Benchmark$2.prototype.__JT__CLASS_ID = Benchmark$2.__JT__CLASS_ID = 767;
Benchmark$2.prototype.__JT__CLASS_IDS = Benchmark$2.__JT__CLASS_IDS = [767,656,751];
Benchmark$2.prototype["Benchmark$2<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$2.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 305419896;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 1000000))) {
					_G = 2;
					continue;
				}
				lI1 = (((lI1 + (((lI1 << 1))|0)))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$1() {
}
Benchmark$1.prototype = Object.create(java_lang_Object.prototype);
Benchmark$1.prototype.constructor = Benchmark$1;
Benchmark$1.prototype.___id = 0;
Benchmark$1.SI = function(){};
Benchmark$1.prototype.__JT__CLASS_ID = Benchmark$1.__JT__CLASS_ID = 766;
Benchmark$1.prototype.__JT__CLASS_IDS = Benchmark$1.__JT__CLASS_IDS = [766,656,751];
Benchmark$1.prototype["Benchmark$1<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$1.prototype["run()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 1000000))) {
					_G = 2;
					continue;
				}
				lI1 = (((lI1 + lI2))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$40() {
}
Benchmark$40.prototype = Object.create(java_lang_Object.prototype);
Benchmark$40.prototype.constructor = Benchmark$40;
Benchmark$40.prototype._val_hexData = null;
Benchmark$40.prototype.___id = 0;
Benchmark$40.SI = function(){};
Benchmark$40.prototype.__JT__CLASS_ID = Benchmark$40.__JT__CLASS_ID = 765;
Benchmark$40.prototype.__JT__CLASS_IDS = Benchmark$40.__JT__CLASS_IDS = [765,656,751];
Benchmark$40.prototype["Benchmark$40<init>([B)V"] = function(p0) { 
	this._val_hexData = p0;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$40.prototype["run()I"] = function() { 
	var _G = 0, lA2 = null, lI1 = 0, lI3 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				tA0 = ((new com_jtransc_compression_jzlib_CRC32()));
				fA0 = tA0;
				(tA0)["com.jtransc.compression.jzlib.CRC32<init>()V"]();
				lA2 = fA0;
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 10000))) {
					_G = 2;
					continue;
				}
				(lA2)["reset()V"]();
				(lA2)["update([BII)V"](this._val_hexData, 0, this._val_hexData.length);
				lI1 = (((lI1 + (lA2)["getValue()I"]()))|0);
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function java_lang_Runtime() {
}
java_lang_Runtime.prototype = Object.create(java_lang_Object.prototype);
java_lang_Runtime.prototype.constructor = java_lang_Runtime;
java_lang_Runtime.prototype.___id = 0;
java_lang_Runtime.SI = function() { 
	java_lang_Runtime._current = null;
};
java_lang_Runtime.prototype.__JT__CLASS_ID = java_lang_Runtime.__JT__CLASS_ID = 764;
java_lang_Runtime.prototype.__JT__CLASS_IDS = java_lang_Runtime.__JT__CLASS_IDS = [764,656];
java_lang_Runtime.prototype["java.lang.Runtime<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang_Runtime.prototype["gc()V"] = function() { 
	return;
};
java_lang_Runtime["getRuntime()Ljava/lang/Runtime;"] = function() { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((java_lang_Runtime._current != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_Runtime()));
				fA0 = tA0;
				(tA0)["java.lang.Runtime<init>()V"]();
				java_lang_Runtime._current = (fA0);
				_G = 1;
				continue;
			case 1:
				return java_lang_Runtime._current;
			default:
				break;
		}
	}
	return null;
};
java_lang_Runtime.prototype["freeMemory()J"] = function() { 
	return N.lnew(2, 0);
};
java_lang_Runtime.prototype["maxMemory()J"] = function() { 
	return N.lnew(2, 0);
};
java_lang_Runtime.prototype["totalMemory()J"] = function() { 
	return N.lnew(2, 0);
};
function Benchmark$30() {
}
Benchmark$30.prototype = Object.create(java_lang_Object.prototype);
Benchmark$30.prototype.constructor = Benchmark$30;
Benchmark$30.prototype.___id = 0;
Benchmark$30.SI = function(){};
Benchmark$30.prototype.__JT__CLASS_ID = Benchmark$30.__JT__CLASS_ID = 763;
Benchmark$30.prototype.__JT__CLASS_IDS = Benchmark$30.__JT__CLASS_IDS = [763,656,751];
Benchmark$30.prototype["Benchmark$30<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$30.prototype["run()I"] = function() { 
	var lA2 = null, _G = 0, lA1 = null, lI4 = 0, lI5 = 0, lA3 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = java_nio_ByteBuffer["allocate(I)Ljava/nio/ByteBuffer;"](1024)["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"](java_nio_ByteOrder["nativeOrder()Ljava/nio/ByteOrder;"]());
				lA2 = lA1["asIntBuffer()Ljava/nio/IntBuffer;"]();
				lA3 = lA1["asFloatBuffer()Ljava/nio/FloatBuffer;"]();
				lI4 = 0;
				lI5 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= 100000))) {
					_G = 2;
					continue;
				}
				lA3["put(IF)Ljava/nio/FloatBuffer;"](0, Math.fround(+(lI5)));
				lI4 = (((lI4 + lA2["get(I)I"](0)))|0);
				lI5 = (((lI5 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI4;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$31() {
}
Benchmark$31.prototype = Object.create(java_lang_Object.prototype);
Benchmark$31.prototype.constructor = Benchmark$31;
Benchmark$31.prototype.___id = 0;
Benchmark$31.SI = function(){};
Benchmark$31.prototype.__JT__CLASS_ID = Benchmark$31.__JT__CLASS_ID = 762;
Benchmark$31.prototype.__JT__CLASS_IDS = Benchmark$31.__JT__CLASS_IDS = [762,656,751];
Benchmark$31.prototype["Benchmark$31<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$31.prototype["run()I"] = function() { 
	var lA2 = null, _G = 0, lA1 = null, lI4 = 0, lI5 = 0, lA3 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = java_nio_ByteBuffer["allocateDirect(I)Ljava/nio/ByteBuffer;"](1024)["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"](java_nio_ByteOrder["nativeOrder()Ljava/nio/ByteOrder;"]());
				lA2 = lA1["asIntBuffer()Ljava/nio/IntBuffer;"]();
				lA3 = lA1["asFloatBuffer()Ljava/nio/FloatBuffer;"]();
				lI4 = 0;
				lI5 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= 100000))) {
					_G = 2;
					continue;
				}
				lA3["put(IF)Ljava/nio/FloatBuffer;"](0, Math.fround(+(lI5)));
				lI4 = (((lI4 + lA2["get(I)I"](0)))|0);
				lI5 = (((lI5 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI4;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$32() {
}
Benchmark$32.prototype = Object.create(java_lang_Object.prototype);
Benchmark$32.prototype.constructor = Benchmark$32;
Benchmark$32.prototype.___id = 0;
Benchmark$32.SI = function(){};
Benchmark$32.prototype.__JT__CLASS_ID = Benchmark$32.__JT__CLASS_ID = 761;
Benchmark$32.prototype.__JT__CLASS_IDS = Benchmark$32.__JT__CLASS_IDS = [761,656,751];
Benchmark$32.prototype["Benchmark$32<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$32.prototype["run()I"] = function() { 
	var _G = 0, lA1 = null, lI4 = 0, lI5 = 0, lA2 = null, lA3 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = java_nio_ByteBuffer["allocateDirect(I)Ljava/nio/ByteBuffer;"](1024)["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"](java_nio_ByteOrder["nativeOrder()Ljava/nio/ByteOrder;"]());
				lA2 = lA1["asShortBuffer()Ljava/nio/ShortBuffer;"]();
				lA3 = lA1["asCharBuffer()Ljava/nio/CharBuffer;"]();
				lI4 = 0;
				lI5 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= 100000))) {
					_G = 2;
					continue;
				}
				lA3["put(IC)Ljava/nio/CharBuffer;"](0, ((lI5)&0xFFFF));
				lI4 = (((lI4 + lA2["get(I)S"](0)))|0);
				lI5 = (((lI5 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI4;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$33() {
}
Benchmark$33.prototype = Object.create(java_lang_Object.prototype);
Benchmark$33.prototype.constructor = Benchmark$33;
Benchmark$33.prototype.___id = 0;
Benchmark$33.SI = function(){};
Benchmark$33.prototype.__JT__CLASS_ID = Benchmark$33.__JT__CLASS_ID = 760;
Benchmark$33.prototype.__JT__CLASS_IDS = Benchmark$33.__JT__CLASS_IDS = [760,656,751];
Benchmark$33.prototype["Benchmark$33<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$33.prototype["run()I"] = function() { 
	var _G = 0, lA1 = null, lI4 = 0, lI5 = 0, lA2 = null, lA3 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = java_nio_ByteBuffer["allocateDirect(I)Ljava/nio/ByteBuffer;"](1024)["order(Ljava/nio/ByteOrder;)Ljava/nio/ByteBuffer;"](java_nio_ByteOrder["nativeOrder()Ljava/nio/ByteOrder;"]());
				lA2 = lA1["asLongBuffer()Ljava/nio/LongBuffer;"]();
				lA3 = lA1["asDoubleBuffer()Ljava/nio/DoubleBuffer;"]();
				lI4 = 0;
				lI5 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI5 >= 100000))) {
					_G = 2;
					continue;
				}
				lA3["put(ID)Ljava/nio/DoubleBuffer;"](0, +(lI5));
				lI4 = N.j2i(N.ladd(N.i2j(lI4), lA2["get(I)J"](0)));
				lI5 = (((lI5 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI4;
			default:
				break;
		}
	}
	return 0;
};
function java_util_Random() {
}
java_util_Random.prototype = Object.create(java_lang_Object.prototype);
java_util_Random.prototype.constructor = java_util_Random;
java_util_Random.prototype._seed = N.lnew(0, 0);
java_util_Random.prototype._haveNextNextGaussian = false;
java_util_Random.prototype.___id = 0;
java_util_Random.SI = function(){};
java_util_Random.prototype.__JT__CLASS_ID = java_util_Random.__JT__CLASS_ID = 759;
java_util_Random.prototype.__JT__CLASS_IDS = java_util_Random.__JT__CLASS_IDS = [759,656,658];
java_util_Random.prototype["java.util.Random<init>(J)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	this["setSeed(J)V"](p0);
	return this;
	return this;
};
java_util_Random.prototype["nextInt()I"] = function() { 
	return this["next(I)I"](32);
};
java_util_Random.prototype["next(I)I"] = function(p0) { 
	this._seed = N.land(N.ladd(N.lmul(this._seed, N.lnew(5, -554899859)), N.lnew(0, 11)), N.lnew(65535, -1));
	return N.j2i(N.lushr(this._seed, (((48 - p0))|0)));
};
java_util_Random.prototype["setSeed(J)V"] = function(p0) { 
	this._seed = N.land(N.lxor(p0, N.lnew(5, -554899859)), N.lnew(65535, -1));
	this._haveNextNextGaussian = false;
	return;
};
function Benchmark$38() {
}
Benchmark$38.prototype = Object.create(java_lang_Object.prototype);
Benchmark$38.prototype.constructor = Benchmark$38;
Benchmark$38.prototype.___id = 0;
Benchmark$38.SI = function(){};
Benchmark$38.prototype.__JT__CLASS_ID = Benchmark$38.__JT__CLASS_ID = 758;
Benchmark$38.prototype.__JT__CLASS_IDS = Benchmark$38.__JT__CLASS_IDS = [758,656,751];
Benchmark$38.prototype["Benchmark$38<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$38.prototype["run()I"] = function() { 
	var _G = 0, lA3 = null, lI1 = 0, lI2 = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 100000))) {
					_G = 2;
					continue;
				}
				tA0 = ((new Benchmark$MyClass()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["Benchmark$MyClass<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[125])["append(I)Ljava/lang/StringBuilder;"](lI2)["toString()Ljava/lang/String;"]());
				lA3 = fA0;
				lI1 = (((lI1 + (lA3)._b))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$39() {
}
Benchmark$39.prototype = Object.create(java_lang_Object.prototype);
Benchmark$39.prototype.constructor = Benchmark$39;
Benchmark$39.prototype._val_hexData = null;
Benchmark$39.prototype.___id = 0;
Benchmark$39.SI = function(){};
Benchmark$39.prototype.__JT__CLASS_ID = Benchmark$39.__JT__CLASS_ID = 757;
Benchmark$39.prototype.__JT__CLASS_IDS = Benchmark$39.__JT__CLASS_IDS = [757,656,751];
Benchmark$39.prototype["Benchmark$39<init>([B)V"] = function(p0) { 
	this._val_hexData = p0;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$39.prototype["run()I"] = function() { 
	var _G = 0, lA2 = null, lI1 = 0, lI3 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				tA0 = ((new java_util_zip_CRC32()));
				fA0 = tA0;
				(tA0)["java.util.zip.CRC32<init>()V"]();
				lA2 = fA0;
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 10000))) {
					_G = 2;
					continue;
				}
				(lA2)["reset()V"]();
				(lA2)["update([BII)V"](this._val_hexData, 0, this._val_hexData.length);
				lI1 = N.j2i(N.ladd(N.i2j(lI1), (lA2)["getValue()J"]()));
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function com_jtransc_JTranscSystem() {
}
com_jtransc_JTranscSystem.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_JTranscSystem.prototype.constructor = com_jtransc_JTranscSystem;
com_jtransc_JTranscSystem.prototype.___id = 0;
com_jtransc_JTranscSystem.SI = function() { 
	com_jtransc_JTranscSystem._start = 0.0;
	com_jtransc_JTranscSystem["com.jtransc.JTranscSystem<clinit>()V"]();
};
com_jtransc_JTranscSystem.prototype.__JT__CLASS_ID = com_jtransc_JTranscSystem.__JT__CLASS_ID = 756;
com_jtransc_JTranscSystem.prototype.__JT__CLASS_IDS = com_jtransc_JTranscSystem.__JT__CLASS_IDS = [756,656];
com_jtransc_JTranscSystem.prototype["com.jtransc.JTranscSystem<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_JTranscSystem["com.jtransc.JTranscSystem<clinit>()V"] = function() { 
	com_jtransc_JTranscSystem._start = -1.0;
	return;
};
com_jtransc_JTranscSystem["stamp()D"] = function() { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!(((com_jtransc_JTranscSystem._start < 0.0)))) {
					_G = 1;
					continue;
				}
				com_jtransc_JTranscSystem._start = com_jtransc_JTranscSystem["fastTime()D"]();
				_G = 1;
				continue;
			case 1:
				return com_jtransc_JTranscSystem["elapsedTime(DD)D"](com_jtransc_JTranscSystem._start, com_jtransc_JTranscSystem["fastTime()D"]());
			default:
				break;
		}
	}
	return 0.0;
};
com_jtransc_JTranscSystem["elapsedTime(DD)D"] = function(p0, p1) { 
	return +((p1 - p0));
};
com_jtransc_JTranscSystem["fastTime()D"] = function() { 
	return com_jtransc_time_JTranscClock._impl["fastTime()D"]();
};
com_jtransc_JTranscSystem["lineSeparator()Ljava/lang/String;"] = function() { 
	return com_jtransc_JTranscSystemProperties["lineSeparator()Ljava/lang/String;"]();
};
com_jtransc_JTranscSystem["isCpp()Z"] = function() { 
	return false;
};
com_jtransc_JTranscSystem["getRuntimeKind()Ljava/lang/String;"] = function() { 
	return N.str("js");
};
com_jtransc_JTranscSystem["getOS()Ljava/lang/String;"] = function() { 
	var _G = 0, lA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA0 = com_jtransc_JTranscSystem["getOSRaw()Ljava/lang/String;"]()["toLowerCase()Ljava/lang/String;"]();
				if (!(lA0["startsWith(Ljava/lang/String;)Z"](S[126]))) {
					_G = 1;
					continue;
				}
				return S[127];
			case 1:
				if (!(lA0["startsWith(Ljava/lang/String;)Z"](S[128]))) {
					_G = 2;
					continue;
				}
				return S[129];
			case 2:
				if (lA0["startsWith(Ljava/lang/String;)Z"](S[130])) {
					_G = 3;
					continue;
				}
				if (!(lA0["startsWith(Ljava/lang/String;)Z"](S[131]))) {
					_G = 4;
					continue;
				}
				_G = 3;
				continue;
			case 3:return S[130]; 
			case 4:
				if (!(lA0["startsWith(Ljava/lang/String;)Z"](S[132]))) {
					_G = 5;
					continue;
				}
				return S[133];
			case 5:
				return lA0;
			default:
				break;
		}
	}
	return null;
};
com_jtransc_JTranscSystem["getOSRaw()Ljava/lang/String;"] = function() { 
	return N.str(typeof navigator != 'undefined' ? navigator.platform : process.platform);
};
com_jtransc_JTranscSystem["isWindows()Z"] = function() { 
	return com_jtransc_JTranscSystem["getOS()Ljava/lang/String;"]()["toLowerCase()Ljava/lang/String;"]()["startsWith(Ljava/lang/String;)Z"](S[126]);
};
com_jtransc_JTranscSystem["getRuntimeName()Ljava/lang/String;"] = function() { 
	return N.str('jtransc-js');
};
com_jtransc_JTranscSystem["getJavaHome()Ljava/lang/String;"] = function() { 
	return N.str('/jtransc-js');
};
com_jtransc_JTranscSystem["getArch()Ljava/lang/String;"] = function() { 
	return N.str('x86');
};
com_jtransc_JTranscSystem["pathSeparator()Ljava/lang/String;"] = function() { 
	return com_jtransc_JTranscSystemProperties["pathSeparator()Ljava/lang/String;"]();
};
com_jtransc_JTranscSystem["fileSeparator()Ljava/lang/String;"] = function() { 
	return com_jtransc_JTranscSystemProperties["fileSeparator()Ljava/lang/String;"]();
};
function Benchmark$34() {
}
Benchmark$34.prototype = Object.create(java_lang_Object.prototype);
Benchmark$34.prototype.constructor = Benchmark$34;
Benchmark$34.prototype.___id = 0;
Benchmark$34.SI = function(){};
Benchmark$34.prototype.__JT__CLASS_ID = Benchmark$34.__JT__CLASS_ID = 755;
Benchmark$34.prototype.__JT__CLASS_IDS = Benchmark$34.__JT__CLASS_IDS = [755,656,751];
Benchmark$34.prototype["Benchmark$34<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$34.prototype["run()I"] = function() { 
	var _G = 0, lI2 = 0, lI3 = 0, lA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = com_jtransc_FastMemory["alloc(I)Lcom/jtransc/FastMemory;"](1024);
				lI2 = 0;
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 100000))) {
					_G = 2;
					continue;
				}
				lA1["setAlignedFloat32(IF)V"](0, Math.fround(+(lI3)));
				lI2 = (((lI2 + lA1["getAlignedInt32(I)I"](0)))|0);
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI2;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$35() {
}
Benchmark$35.prototype = Object.create(java_lang_Object.prototype);
Benchmark$35.prototype.constructor = Benchmark$35;
Benchmark$35.prototype.___id = 0;
Benchmark$35.SI = function(){};
Benchmark$35.prototype.__JT__CLASS_ID = Benchmark$35.__JT__CLASS_ID = 754;
Benchmark$35.prototype.__JT__CLASS_IDS = Benchmark$35.__JT__CLASS_IDS = [754,656,751];
Benchmark$35.prototype["Benchmark$35<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$35.prototype["run()I"] = function() { 
	var _G = 0, lA3 = null, lI1 = 0, lI2 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= 100000))) {
					_G = 2;
					continue;
				}
				tA0 = ((new Benchmark$MyClass()));
				fA0 = tA0;
				(tA0)["Benchmark$MyClass<init>(Ljava/lang/String;)V"](S[125]);
				lA3 = fA0;
				lI1 = (((lI1 + (lA3)._b))|0);
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$36() {
}
Benchmark$36.prototype = Object.create(java_lang_Object.prototype);
Benchmark$36.prototype.constructor = Benchmark$36;
Benchmark$36.prototype.___id = 0;
Benchmark$36.SI = function(){};
Benchmark$36.prototype.__JT__CLASS_ID = Benchmark$36.__JT__CLASS_ID = 753;
Benchmark$36.prototype.__JT__CLASS_IDS = Benchmark$36.__JT__CLASS_IDS = [753,656,751];
Benchmark$36.prototype["Benchmark$36<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$36.prototype["run()I"] = function() { 
	var _G = 0, lA4 = null, lI1 = 0, lI3 = 0, fA0 = null, lA2 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				lA2 = S[125];
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 100000))) {
					_G = 2;
					continue;
				}
				tA0 = ((new Benchmark$MyClass2()));
				fA0 = tA0;
				(tA0)["Benchmark$MyClass2<init>(Ljava/lang/String;I)V"](lA2, ((Math.imul(lI3, lI1))|0));
				lA4 = fA0;
				lI1 = (((lI1 + (lA4)._b))|0);
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function Benchmark$MyClass2() {
}
Benchmark$MyClass2.prototype = Object.create(java_lang_Object.prototype);
Benchmark$MyClass2.prototype.constructor = Benchmark$MyClass2;
Benchmark$MyClass2.prototype._a = 0;
Benchmark$MyClass2.prototype._c = null;
Benchmark$MyClass2.prototype._d = null;
Benchmark$MyClass2.prototype._b = 0;
Benchmark$MyClass2.prototype.___id = 0;
Benchmark$MyClass2.SI = function(){};
Benchmark$MyClass2.prototype.__JT__CLASS_ID = Benchmark$MyClass2.__JT__CLASS_ID = 752;
Benchmark$MyClass2.prototype.__JT__CLASS_IDS = Benchmark$MyClass2.__JT__CLASS_IDS = [752,656];
Benchmark$MyClass2.prototype["Benchmark$MyClass2<init>(Ljava/lang/String;I)V"] = function(p0, p1) { 
	(this)["java.lang.Object<init>()V"]();
	this._a = 10;
	this._b = 20;
	this._c = S[41];
	this._d = p0;
	this._b = p1;
	return this;
	return this;
};
function Benchmark$37() {
}
Benchmark$37.prototype = Object.create(java_lang_Object.prototype);
Benchmark$37.prototype.constructor = Benchmark$37;
Benchmark$37.prototype._val_objects = null;
Benchmark$37.prototype.___id = 0;
Benchmark$37.SI = function(){};
Benchmark$37.prototype.__JT__CLASS_ID = Benchmark$37.__JT__CLASS_ID = 750;
Benchmark$37.prototype.__JT__CLASS_IDS = Benchmark$37.__JT__CLASS_IDS = [750,656,751];
Benchmark$37.prototype["Benchmark$37<init>([LBenchmark$MyClass2;)V"] = function(p0) { 
	this._val_objects = p0;
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark$37.prototype["run()I"] = function() { 
	var _G = 0, lA4 = null, lI1 = 0, lI3 = 0, fA0 = null, lA2 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = 0;
				lA2 = S[125];
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= 100000))) {
					_G = 2;
					continue;
				}
				tA0 = ((new Benchmark$MyClass2()));
				fA0 = tA0;
				(tA0)["Benchmark$MyClass2<init>(Ljava/lang/String;I)V"](lA2, ((Math.imul(lI3, lI1))|0));
				lA4 = fA0;
				(this._val_objects).data[lI3] = lA4;
				lI1 = (((lI1 + (lA4)._b))|0);
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
function com_jtransc_JTranscWrapped() {
}
com_jtransc_JTranscWrapped.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_JTranscWrapped.prototype.constructor = com_jtransc_JTranscWrapped;
com_jtransc_JTranscWrapped.prototype._item = null;
com_jtransc_JTranscWrapped.prototype.___id = 0;
com_jtransc_JTranscWrapped.SI = function(){};
com_jtransc_JTranscWrapped.prototype.__JT__CLASS_ID = com_jtransc_JTranscWrapped.__JT__CLASS_ID = 749;
com_jtransc_JTranscWrapped.prototype.__JT__CLASS_IDS = com_jtransc_JTranscWrapped.__JT__CLASS_IDS = [749,656];
com_jtransc_JTranscWrapped.prototype["com.jtransc.JTranscWrapped<init>(Ljava/lang/Object;)V"] = function(p0) { 
	this._wrapped = p0;
	return this;
};
com_jtransc_JTranscWrapped.prototype["get(Ljava/lang/String;)Ljava/lang/Object;"] = function(p0) { 
	return N.box(this._wrapped[N.istr(p0)]);
};
com_jtransc_JTranscWrapped.prototype["set(Ljava/lang/String;Ljava/lang/Object;)V"] = function(p0, p1) { 
	this._wrapped[N.istr(p0)] = N.unbox(p1);
};
com_jtransc_JTranscWrapped.prototype["invoke(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/Object;"] = function(p0, p1) { 
	return N.box(this._wrapped[N.istr(p0)].apply(this._wrapped, N.unboxArray(p1.data)));
};
com_jtransc_JTranscWrapped.prototype["toString()Ljava/lang/String;"] = function() { 
	return N.str('' + this._wrapped);
};
function java_lang_ClassCastException() {
}
java_lang_ClassCastException.prototype = Object.create(java_lang_RuntimeException.prototype);
java_lang_ClassCastException.prototype.constructor = java_lang_ClassCastException;
java_lang_ClassCastException.SI = function(){};
java_lang_ClassCastException.prototype.__JT__CLASS_ID = java_lang_ClassCastException.__JT__CLASS_ID = 745;
java_lang_ClassCastException.prototype.__JT__CLASS_IDS = java_lang_ClassCastException.__JT__CLASS_IDS = [745,696,680,681,656,658];
java_lang_ClassCastException.prototype["java.lang.ClassCastException<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.RuntimeException<init>(Ljava/lang/String;)V"](p0);
	return this;
	return this;
};
function java_util_Objects() {
}
java_util_Objects.prototype = Object.create(java_lang_Object.prototype);
java_util_Objects.prototype.constructor = java_util_Objects;
java_util_Objects.prototype.___id = 0;
java_util_Objects.SI = function(){};
java_util_Objects.prototype.__JT__CLASS_ID = java_util_Objects.__JT__CLASS_ID = 744;
java_util_Objects.prototype.__JT__CLASS_IDS = java_util_Objects.__JT__CLASS_IDS = [744,656];
java_util_Objects.prototype["java.util.Objects<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_util_Objects["hashCode(Ljava/lang/Object;)I"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 != null))) {
					_G = 1;
					continue;
				}
				fI0 = 0;
				_G = 2;
				continue;
			case 1:
				fI0 = p0["hashCode()I"]();
				_G = 2;
				continue;
			case 2:return fI0; 
			default:
				break;
		}
	}
	return 0;
};
java_util_Objects["requireNonNull(Ljava/lang/Object;)Ljava/lang/Object;"] = function(p0) { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_NullPointerException()));
				fA0 = tA0;
				(tA0)["java.lang.NullPointerException<init>()V"]();
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				return p0;
			default:
				break;
		}
	}
	return null;
};
java_util_Objects["equals(Ljava/lang/Object;Ljava/lang/Object;)Z"] = function(p0, p1) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 != null))) {
					_G = 1;
					continue;
				}
				if (((p1 != null))) {
					_G = 2;
					continue;
				}
				fI0 = 1;
				_G = 3;
				continue;
			case 2:
				fI0 = 0;
				_G = 3;
				continue;
			case 1:
				fI0 = ((N.z2i(p0["equals(Ljava/lang/Object;)Z"](p1)))|0);
				_G = 3;
				continue;
			case 3:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
function java_lang_SystemInt() {
}
java_lang_SystemInt.prototype = Object.create(java_lang_Object.prototype);
java_lang_SystemInt.prototype.constructor = java_lang_SystemInt;
java_lang_SystemInt.prototype.___id = 0;
java_lang_SystemInt.SI = function() { 
	java_lang_SystemInt.___lastId = 0;
	java_lang_SystemInt["java.lang.SystemInt<clinit>()V"]();
};
java_lang_SystemInt.prototype.__JT__CLASS_ID = java_lang_SystemInt.__JT__CLASS_ID = 743;
java_lang_SystemInt.prototype.__JT__CLASS_IDS = java_lang_SystemInt.__JT__CLASS_IDS = [743,656];
java_lang_SystemInt.prototype["java.lang.SystemInt<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang_SystemInt["java.lang.SystemInt<clinit>()V"] = function() { 
	java_lang_SystemInt.___lastId = 1;
	return;
};
java_lang_SystemInt["identityHashCode(Ljava/lang/Object;)I"] = function(p0) { 
	var _G = 0, fI1 = 0, tI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 == null))) {
					_G = 1;
					continue;
				}
				if (((p0.___id != 0))) {
					_G = 2;
					continue;
				}
				tI0 = java_lang_SystemInt.___lastId;
				fI1 = tI0;
				java_lang_SystemInt.___lastId = (((tI0 + 1))|0);
				p0.___id = fI1;
				_G = 2;
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
};
function java_lang_StackTraceElement() {
}
java_lang_StackTraceElement.prototype = Object.create(java_lang_Object.prototype);
java_lang_StackTraceElement.prototype.constructor = java_lang_StackTraceElement;
java_lang_StackTraceElement.prototype._fileName = null;
java_lang_StackTraceElement.prototype._lineNumber = 0;
java_lang_StackTraceElement.prototype._methodName = null;
java_lang_StackTraceElement.prototype._declaringClass = null;
java_lang_StackTraceElement.prototype.___id = 0;
java_lang_StackTraceElement.SI = function(){};
java_lang_StackTraceElement.prototype.__JT__CLASS_ID = java_lang_StackTraceElement.__JT__CLASS_ID = 742;
java_lang_StackTraceElement.prototype.__JT__CLASS_IDS = java_lang_StackTraceElement.__JT__CLASS_IDS = [742,656,658];
java_lang_StackTraceElement.prototype["java.lang.StackTraceElement<init>(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;I)V"] = function(p0, p1, p2, p3) { 
	var _G = 0, fA0 = null, fA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.lang.Object<init>()V"]();
				fA0 = this;
				if ((((p0) == null))) {
					_G = 1;
					continue;
				}
				fA1 = p0;
				_G = 2;
				continue;
			case 1:
				fA1 = S[134];
				_G = 2;
				continue;
			case 2:
				fA0._declaringClass = fA1;
				fA0 = this;
				if ((((p1) == null))) {
					_G = 3;
					continue;
				}
				fA1 = p1;
				_G = 4;
				continue;
			case 3:
				fA1 = S[134];
				_G = 4;
				continue;
			case 4:
				fA0._methodName = fA1;
				fA0 = this;
				if ((((p2) == null))) {
					_G = 5;
					continue;
				}
				fA1 = p2;
				_G = 6;
				continue;
			case 5:
				fA1 = S[134];
				_G = 6;
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
};
java_lang_StackTraceElement.prototype["toString()Ljava/lang/String;"] = function() { 
	var _G = 0, fA0 = null, fA1 = null, tA0 = null, tA1 = null, tA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				fA0 = ((fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this["getClassName()Ljava/lang/String;"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[135])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this._methodName));
				fA1 = (this);
				if (!((fA1)["isNativeMethod()Z"]())) {
					_G = 1;
					continue;
				}
				fA1 = (S[136]);
				_G = 2;
				continue;
			case 1:
				if (((this._fileName == null))) {
					_G = 3;
					continue;
				}
				if (((this._lineNumber < 0))) {
					_G = 3;
					continue;
				}
				tA1 = ((new java_lang_StringBuilder()));
				fA1 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				fA1 = ((fA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[137])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this._fileName)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[52])["append(I)Ljava/lang/StringBuilder;"](this._lineNumber)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[1])["toString()Ljava/lang/String;"]());
				_G = 2;
				continue;
			case 3:
				if (((this._fileName == null))) {
					_G = 4;
					continue;
				}
				tA2 = ((new java_lang_StringBuilder()));
				fA1 = tA2;
				(tA2)["java.lang.StringBuilder<init>()V"]();
				fA1 = ((fA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[137])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this._fileName)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[1])["toString()Ljava/lang/String;"]());
				_G = 2;
				continue;
			case 4:
				fA1 = (S[138]);
				_G = 2;
				continue;
			case 2:
				fA0 = ((fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"]((fA1))["toString()Ljava/lang/String;"]());
				return (fA0);
			default:
				break;
		}
	}
	return null;
};
java_lang_StackTraceElement.prototype["getClassName()Ljava/lang/String;"] = function() { 
	return this._declaringClass;
};
java_lang_StackTraceElement.prototype["isNativeMethod()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._lineNumber != -2))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_StackTraceElement.prototype["hashCode()I"] = function() { 
	var lI1 = 0;
	lI1 = (((((Math.imul(31, this._declaringClass["hashCode()I"]()))|0) + this._methodName["hashCode()I"]()))|0);
	lI1 = (((((Math.imul(31, lI1))|0) + java_util_Objects["hashCode(Ljava/lang/Object;)I"]((this._fileName))))|0);
	lI1 = (((((Math.imul(31, lI1))|0) + this._lineNumber))|0);
	return lI1;
};
java_lang_StackTraceElement.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0, lA2 = null, fA0 = null, fA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 != (this)))) {
					_G = 1;
					continue;
				}
				return true;
			case 1:
				if ((p0 instanceof java_lang_StackTraceElement)) {
					_G = 2;
					continue;
				}
				return false;
			case 2:
				lA2 = N.checkCast(p0, java_lang_StackTraceElement);
				if (!(lA2._declaringClass["equals(Ljava/lang/Object;)Z"]((this._declaringClass)))) {
					_G = 3;
					continue;
				}
				if (((lA2._lineNumber != this._lineNumber))) {
					_G = 3;
					continue;
				}
				fA0 = (this._methodName);
				fA1 = (lA2._methodName);
				if (!(java_util_Objects["equals(Ljava/lang/Object;Ljava/lang/Object;)Z"](fA0, fA1))) {
					_G = 3;
					continue;
				}
				fA0 = (this._fileName);
				fA1 = (lA2._fileName);
				if (!(java_util_Objects["equals(Ljava/lang/Object;Ljava/lang/Object;)Z"](fA0, fA1))) {
					_G = 3;
					continue;
				}
				fI0 = 1;
				_G = 4;
				continue;
			case 3:
				fI0 = 0;
				_G = 4;
				continue;
			case 4:
				return ((fI0)!=0);
			default:
				break;
		}
	}
	return false;
};
function java_lang_reflect_Type() {
}
java_lang_reflect_Type.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_reflect_Type.prototype.constructor = java_lang_reflect_Type;
java_lang_reflect_Type.SI = function(){};
java_lang_reflect_Type.prototype.__JT__CLASS_ID = java_lang_reflect_Type.__JT__CLASS_ID = 667;
java_lang_reflect_Type.prototype.__JT__CLASS_IDS = java_lang_reflect_Type.__JT__CLASS_IDS = [667,656];
function java_lang_reflect_ParameterizedType() {
}
java_lang_reflect_ParameterizedType.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_reflect_ParameterizedType.prototype.constructor = java_lang_reflect_ParameterizedType;
java_lang_reflect_ParameterizedType.SI = function(){};
java_lang_reflect_ParameterizedType.prototype.__JT__CLASS_ID = java_lang_reflect_ParameterizedType.__JT__CLASS_ID = 741;
java_lang_reflect_ParameterizedType.prototype.__JT__CLASS_IDS = java_lang_reflect_ParameterizedType.__JT__CLASS_IDS = [741,667,656];
function java_lang_reflect_ParameterizedTypeImpl() {
}
java_lang_reflect_ParameterizedTypeImpl.prototype = Object.create(java_lang_Object.prototype);
java_lang_reflect_ParameterizedTypeImpl.prototype.constructor = java_lang_reflect_ParameterizedTypeImpl;
java_lang_reflect_ParameterizedTypeImpl.prototype._ownerType = null;
java_lang_reflect_ParameterizedTypeImpl.prototype._actualTypeArguments = null;
java_lang_reflect_ParameterizedTypeImpl.prototype._rawType = null;
java_lang_reflect_ParameterizedTypeImpl.prototype.___id = 0;
java_lang_reflect_ParameterizedTypeImpl.SI = function(){};
java_lang_reflect_ParameterizedTypeImpl.prototype.__JT__CLASS_ID = java_lang_reflect_ParameterizedTypeImpl.__JT__CLASS_ID = 740;
java_lang_reflect_ParameterizedTypeImpl.prototype.__JT__CLASS_IDS = java_lang_reflect_ParameterizedTypeImpl.__JT__CLASS_IDS = [740,656,741,667];
java_lang_reflect_ParameterizedTypeImpl.prototype["java.lang.reflect.ParameterizedTypeImpl<init>([Ljava/lang/reflect/Type;Ljava/lang/reflect/Type;Ljava/lang/reflect/Type;)V"] = function(p0, p1, p2) { 
	(this)["java.lang.Object<init>()V"]();
	this._actualTypeArguments = p0;
	this._rawType = p1;
	this._ownerType = p2;
	return this;
	return this;
};
java_lang_reflect_ParameterizedTypeImpl.prototype["toString()Ljava/lang/String;"] = function() { 
	var _G = 0, lA1 = null, lI2 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				lA1 = fA0;
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_reflect__InternalUtils["getTypeName(Ljava/lang/reflect/Type;)Ljava/lang/String;"](this._rawType));
				(lA1)["append(C)Ljava/lang/StringBuilder;"](60);
				lI2 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI2 >= this._actualTypeArguments.length))) {
					_G = 2;
					continue;
				}
				if (((lI2 == 0))) {
					_G = 3;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[6]);
				_G = 3;
				continue;
			case 3:
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_reflect__InternalUtils["getTypeName(Ljava/lang/reflect/Type;)Ljava/lang/String;"]((((this._actualTypeArguments).data[lI2]))));
				lI2 = (((lI2 + 1))|0);
				_G = 1;
				continue;
			case 2:
				(lA1)["append(C)Ljava/lang/StringBuilder;"](62);
				return (lA1)["toString()Ljava/lang/String;"]();
			default:
				break;
		}
	}
	return null;
};
function j_ProgramReflection$AllClasses() {
}
j_ProgramReflection$AllClasses.prototype = Object.create(java_lang_Object.prototype);
j_ProgramReflection$AllClasses.prototype.constructor = j_ProgramReflection$AllClasses;
j_ProgramReflection$AllClasses.prototype.___id = 0;
j_ProgramReflection$AllClasses.SI = function(){};
j_ProgramReflection$AllClasses.prototype.__JT__CLASS_ID = j_ProgramReflection$AllClasses.__JT__CLASS_ID = 739;
j_ProgramReflection$AllClasses.prototype.__JT__CLASS_IDS = j_ProgramReflection$AllClasses.__JT__CLASS_IDS = [739,656];
j_ProgramReflection$AllClasses.prototype["j.ProgramReflection$AllClasses<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
j_ProgramReflection$AllClasses["getAllClasses()[Lj/ClassInfo;"] = function() { 
	var out = null;
	out = new JA_L(926, "[Lj.ClassInfo;");
	out.data[655] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](655, null, S[139], 33, 656, JA_I.T([]), JA_I.T([655,656]));
	out.data[656] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](656, null, S[140], 33, -1, JA_I.T([]), JA_I.T([656]));
	out.data[657] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](657, null, S[141], 49, 656, JA_I.T([658,659,660]), JA_I.T([657,656,658,659,660]));
	out.data[658] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](658, null, S[142], 1537, -1, JA_I.T([]), JA_I.T([658]));
	out.data[659] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](659, null, S[143], 1537, -1, JA_I.T([]), JA_I.T([659]));
	out.data[660] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](660, null, S[144], 1537, -1, JA_I.T([]), JA_I.T([660]));
	out.data[661] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](661, null, S[145], 32, 656, JA_I.T([662,658]), JA_I.T([661,656,662,658]));
	out.data[662] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](662, null, S[146], 1537, -1, JA_I.T([]), JA_I.T([662]));
	out.data[663] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](663, null, S[147], 4128, 656, JA_I.T([]), JA_I.T([663,656]));
	out.data[664] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](664, null, S[148], 33, 656, JA_I.T([658,665,660]), JA_I.T([664,656,658,665,660]));
	out.data[665] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](665, null, S[149], 1537, -1, JA_I.T([]), JA_I.T([665]));
	out.data[666] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](666, null, S[150], 49, 656, JA_I.T([658,667,668,670]), JA_I.T([666,656,658,667,668,670,669]));
	out.data[667] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](667, null, S[151], 1537, -1, JA_I.T([]), JA_I.T([667]));
	out.data[668] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](668, null, S[152], 1537, -1, JA_I.T([669]), JA_I.T([668,669]));
	out.data[669] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](669, null, S[153], 1537, -1, JA_I.T([]), JA_I.T([669]));
	out.data[670] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](670, null, S[154], 1537, -1, JA_I.T([]), JA_I.T([670]));
	out.data[671] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](671, null, S[155], 33, 656, JA_I.T([]), JA_I.T([671,656]));
	out.data[672] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](672, null, S[156], 49, 673, JA_I.T([659]), JA_I.T([672,673,656,659]));
	out.data[673] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](673, null, S[157], 1057, 656, JA_I.T([]), JA_I.T([673,656]));
	out.data[674] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](674, null, S[158], 49, 676, JA_I.T([675]), JA_I.T([674,676,656,675,669]));
	out.data[675] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](675, null, S[159], 1537, -1, JA_I.T([]), JA_I.T([675]));
	out.data[676] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](676, null, S[160], 1057, 656, JA_I.T([669]), JA_I.T([676,656,669]));
	out.data[678] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](678, null, S[161], 33, 679, JA_I.T([]), JA_I.T([678,679,680,681,656,658]));
	out.data[679] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](679, null, S[162], 33, 680, JA_I.T([]), JA_I.T([679,680,681,656,658]));
	out.data[680] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](680, null, S[163], 33, 681, JA_I.T([]), JA_I.T([680,681,656,658]));
	out.data[681] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](681, null, S[164], 33, 656, JA_I.T([658]), JA_I.T([681,656,658]));
	out.data[682] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](682, null, S[165], 49, 656, JA_I.T([]), JA_I.T([682,656]));
	out.data[683] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](683, null, S[166], 49, 673, JA_I.T([659]), JA_I.T([683,673,656,659]));
	out.data[684] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](684, null, S[167], 33, 656, JA_I.T([]), JA_I.T([684,656]));
	out.data[685] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](685, null, S[168], 49, 656, JA_I.T([]), JA_I.T([685,656]));
	out.data[686] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](686, null, S[169], 49, 656, JA_I.T([658,659]), JA_I.T([686,656,658,659]));
	out.data[687] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](687, null, S[170], 33, 656, JA_I.T([]), JA_I.T([687,656]));
	out.data[688] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](688, null, S[171], 33, 656, JA_I.T([]), JA_I.T([688,656]));
	out.data[689] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](689, null, S[172], 33, 692, JA_I.T([665,690]), JA_I.T([689,692,693,656,665,690,691,694]));
	out.data[690] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](690, null, S[173], 1537, -1, JA_I.T([691]), JA_I.T([690,691]));
	out.data[691] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](691, null, S[174], 1537, -1, JA_I.T([]), JA_I.T([691]));
	out.data[692] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](692, null, S[175], 33, 693, JA_I.T([]), JA_I.T([692,693,656,690,694,691]));
	out.data[693] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](693, null, S[176], 1057, 656, JA_I.T([690,694]), JA_I.T([693,656,690,694,691]));
	out.data[694] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](694, null, S[177], 1537, -1, JA_I.T([]), JA_I.T([694]));
	out.data[695] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](695, null, S[178], 33, 696, JA_I.T([]), JA_I.T([695,696,680,681,656,658]));
	out.data[696] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](696, null, S[179], 33, 680, JA_I.T([]), JA_I.T([696,680,681,656,658]));
	out.data[697] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](697, null, S[180], 48, 698, JA_I.T([]), JA_I.T([697,698,656,690,691]));
	out.data[698] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](698, null, S[181], 1057, 656, JA_I.T([690]), JA_I.T([698,656,690,691]));
	out.data[699] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](699, null, S[182], 33, 689, JA_I.T([]), JA_I.T([699,689,692,693,656,665,690,691,694]));
	out.data[700] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](700, null, S[183], 32, 701, JA_I.T([]), JA_I.T([700,701,693,656,690,694,691]));
	out.data[701] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](701, null, S[184], 1056, 693, JA_I.T([]), JA_I.T([701,693,656,690,694,691]));
	out.data[702] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](702, null, S[185], 32, 701, JA_I.T([]), JA_I.T([702,701,693,656,690,694,691]));
	out.data[703] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](703, null, S[186], 33, 704, JA_I.T([]), JA_I.T([703,704,696,680,681,656,658]));
	out.data[704] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](704, null, S[187], 33, 696, JA_I.T([]), JA_I.T([704,696,680,681,656,658]));
	out.data[705] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](705, null, S[188], 33, 696, JA_I.T([]), JA_I.T([705,696,680,681,656,658]));
	out.data[706] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](706, null, S[189], 49, 673, JA_I.T([659]), JA_I.T([706,673,656,659]));
	out.data[708] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](708, null, S[190], 49, 673, JA_I.T([659]), JA_I.T([708,673,656,659]));
	out.data[709] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](709, null, S[191], 33, 656, JA_I.T([]), JA_I.T([709,656]));
	out.data[710] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](710, null, S[192], 49, 673, JA_I.T([659]), JA_I.T([710,673,656,659]));
	out.data[711] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](711, null, S[193], 33, 656, JA_I.T([]), JA_I.T([711,656]));
	out.data[712] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](712, null, S[194], 49, 656, JA_I.T([658,659]), JA_I.T([712,656,658,659]));
	out.data[713] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](713, null, S[195], 49, 673, JA_I.T([659]), JA_I.T([713,673,656,659]));
	out.data[714] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](714, null, S[196], 49, 715, JA_I.T([675,668]), JA_I.T([714,715,676,656,675,668,669]));
	out.data[715] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](715, null, S[197], 1057, 676, JA_I.T([]), JA_I.T([715,676,656,669]));
	out.data[716] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](716, null, S[198], 33, 656, JA_I.T([]), JA_I.T([716,656]));
	out.data[719] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](719, null, S[199], 33, 681, JA_I.T([]), JA_I.T([719,681,656,658]));
	out.data[721] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](721, null, S[200], 33, 727, JA_I.T([722,725,726,658]), JA_I.T([721,727,728,656,722,725,726,658,723,724]));
	out.data[722] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](722, null, S[201], 1537, -1, JA_I.T([723]), JA_I.T([722,723,724]));
	out.data[723] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](723, null, S[202], 1537, -1, JA_I.T([724]), JA_I.T([723,724]));
	out.data[724] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](724, null, S[203], 1537, -1, JA_I.T([]), JA_I.T([724]));
	out.data[725] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](725, null, S[204], 1537, -1, JA_I.T([]), JA_I.T([725]));
	out.data[726] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](726, null, S[205], 1537, -1, JA_I.T([]), JA_I.T([726]));
	out.data[727] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](727, null, S[206], 1057, 728, JA_I.T([722]), JA_I.T([727,728,656,722,723,724]));
	out.data[728] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](728, null, S[207], 1057, 656, JA_I.T([723]), JA_I.T([728,656,723,724]));
	out.data[729] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](729, null, S[208], 1537, -1, JA_I.T([]), JA_I.T([729]));
	out.data[730] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](730, null, S[209], 32, 656, JA_I.T([729]), JA_I.T([730,656,729]));
	out.data[731] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](731, null, S[210], 1537, -1, JA_I.T([729]), JA_I.T([731,729]));
	out.data[732] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](732, null, S[211], 48, 730, JA_I.T([731]), JA_I.T([732,730,656,731,729]));
	out.data[733] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](733, null, S[212], 33, 696, JA_I.T([]), JA_I.T([733,696,680,681,656,658]));
	out.data[734] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](734, null, S[213], 33, 696, JA_I.T([]), JA_I.T([734,696,680,681,656,658]));
	out.data[735] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](735, null, S[214], 33, 696, JA_I.T([]), JA_I.T([735,696,680,681,656,658]));
	out.data[736] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](736, null, S[215], 33, 656, JA_I.T([]), JA_I.T([736,656]));
	out.data[737] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](737, null, S[216], 33, 656, JA_I.T([]), JA_I.T([737,656]));
	out.data[738] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](738, null, S[217], 33, 656, JA_I.T([]), JA_I.T([738,656]));
	out.data[739] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](739, null, S[218], 33, 656, JA_I.T([]), JA_I.T([739,656]));
	out.data[741] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](741, null, S[219], 1537, -1, JA_I.T([667]), JA_I.T([741,667]));
	out.data[742] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](742, null, S[220], 49, 656, JA_I.T([658]), JA_I.T([742,656,658]));
	out.data[743] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](743, null, S[221], 33, 656, JA_I.T([]), JA_I.T([743,656]));
	out.data[744] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](744, null, S[222], 49, 656, JA_I.T([]), JA_I.T([744,656]));
	out.data[745] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](745, null, S[223], 33, 696, JA_I.T([]), JA_I.T([745,696,680,681,656,658]));
	out.data[746] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](746, null, S[224], 1537, -1, JA_I.T([]), JA_I.T([746]));
	out.data[747] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](747, null, S[225], 1537, -1, JA_I.T([723]), JA_I.T([747,723,724]));
	out.data[748] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](748, null, S[226], 1537, -1, JA_I.T([]), JA_I.T([748]));
	out.data[749] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](749, null, S[227], 33, 656, JA_I.T([]), JA_I.T([749,656]));
	out.data[750] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](750, null, S[228], 48, 656, JA_I.T([751]), JA_I.T([750,656,751]));
	out.data[751] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](751, null, S[229], 1536, -1, JA_I.T([]), JA_I.T([751]));
	out.data[752] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](752, null, S[230], 32, 656, JA_I.T([]), JA_I.T([752,656]));
	out.data[753] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](753, null, S[231], 48, 656, JA_I.T([751]), JA_I.T([753,656,751]));
	out.data[754] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](754, null, S[232], 48, 656, JA_I.T([751]), JA_I.T([754,656,751]));
	out.data[755] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](755, null, S[233], 48, 656, JA_I.T([751]), JA_I.T([755,656,751]));
	out.data[756] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](756, null, S[234], 33, 656, JA_I.T([]), JA_I.T([756,656]));
	out.data[757] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](757, null, S[235], 48, 656, JA_I.T([751]), JA_I.T([757,656,751]));
	out.data[758] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](758, null, S[236], 48, 656, JA_I.T([751]), JA_I.T([758,656,751]));
	out.data[759] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](759, null, S[237], 33, 656, JA_I.T([658]), JA_I.T([759,656,658]));
	out.data[760] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](760, null, S[238], 48, 656, JA_I.T([751]), JA_I.T([760,656,751]));
	out.data[761] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](761, null, S[239], 48, 656, JA_I.T([751]), JA_I.T([761,656,751]));
	out.data[762] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](762, null, S[240], 48, 656, JA_I.T([751]), JA_I.T([762,656,751]));
	out.data[763] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](763, null, S[241], 48, 656, JA_I.T([751]), JA_I.T([763,656,751]));
	out.data[764] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](764, null, S[242], 33, 656, JA_I.T([]), JA_I.T([764,656]));
	out.data[765] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](765, null, S[243], 48, 656, JA_I.T([751]), JA_I.T([765,656,751]));
	out.data[766] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](766, null, S[244], 48, 656, JA_I.T([751]), JA_I.T([766,656,751]));
	out.data[767] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](767, null, S[245], 48, 656, JA_I.T([751]), JA_I.T([767,656,751]));
	out.data[768] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](768, null, S[246], 48, 656, JA_I.T([751]), JA_I.T([768,656,751]));
	out.data[769] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](769, null, S[247], 48, 656, JA_I.T([751]), JA_I.T([769,656,751]));
	out.data[770] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](770, null, S[248], 48, 656, JA_I.T([751]), JA_I.T([770,656,751]));
	out.data[771] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](771, null, S[249], 48, 656, JA_I.T([751]), JA_I.T([771,656,751]));
	out.data[772] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](772, null, S[250], 48, 656, JA_I.T([751]), JA_I.T([772,656,751]));
	out.data[773] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](773, null, S[251], 48, 656, JA_I.T([751]), JA_I.T([773,656,751]));
	out.data[774] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](774, null, S[252], 48, 656, JA_I.T([751]), JA_I.T([774,656,751]));
	out.data[775] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](775, null, S[253], 48, 656, JA_I.T([751]), JA_I.T([775,656,751]));
	out.data[776] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](776, null, S[254], 48, 656, JA_I.T([751]), JA_I.T([776,656,751]));
	out.data[777] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](777, null, S[255], 48, 656, JA_I.T([751]), JA_I.T([777,656,751]));
	out.data[778] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](778, null, S[256], 48, 656, JA_I.T([751]), JA_I.T([778,656,751]));
	out.data[779] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](779, null, S[257], 48, 656, JA_I.T([751]), JA_I.T([779,656,751]));
	out.data[780] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](780, null, S[258], 48, 656, JA_I.T([751]), JA_I.T([780,656,751]));
	out.data[781] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](781, null, S[259], 48, 656, JA_I.T([751]), JA_I.T([781,656,751]));
	out.data[782] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](782, null, S[260], 48, 656, JA_I.T([751]), JA_I.T([782,656,751]));
	out.data[783] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](783, null, S[261], 48, 656, JA_I.T([751]), JA_I.T([783,656,751]));
	out.data[784] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](784, null, S[262], 48, 656, JA_I.T([751]), JA_I.T([784,656,751]));
	out.data[785] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](785, null, S[263], 48, 656, JA_I.T([751]), JA_I.T([785,656,751]));
	out.data[786] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](786, null, S[264], 48, 656, JA_I.T([751]), JA_I.T([786,656,751]));
	out.data[787] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](787, null, S[265], 48, 656, JA_I.T([751]), JA_I.T([787,656,751]));
	out.data[788] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](788, null, S[266], 48, 656, JA_I.T([751]), JA_I.T([788,656,751]));
	out.data[789] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](789, null, S[267], 48, 656, JA_I.T([751]), JA_I.T([789,656,751]));
	out.data[790] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](790, null, S[268], 48, 656, JA_I.T([751]), JA_I.T([790,656,751]));
	out.data[791] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](791, null, S[269], 48, 656, JA_I.T([751]), JA_I.T([791,656,751]));
	out.data[792] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](792, null, S[270], 48, 656, JA_I.T([751]), JA_I.T([792,656,751]));
	out.data[793] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](793, null, S[271], 48, 656, JA_I.T([751]), JA_I.T([793,656,751]));
	out.data[794] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](794, null, S[272], 48, 656, JA_I.T([751]), JA_I.T([794,656,751]));
	out.data[795] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](795, null, S[273], 48, 656, JA_I.T([751]), JA_I.T([795,656,751]));
	out.data[796] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](796, null, S[274], 48, 656, JA_I.T([751]), JA_I.T([796,656,751]));
	out.data[797] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](797, null, S[275], 48, 656, JA_I.T([751]), JA_I.T([797,656,751]));
	out.data[798] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](798, null, S[276], 48, 656, JA_I.T([751]), JA_I.T([798,656,751]));
	out.data[799] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](799, null, S[277], 33, 656, JA_I.T([]), JA_I.T([799,656]));
	out.data[800] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](800, null, S[278], 33, 656, JA_I.T([]), JA_I.T([800,656]));
	out.data[801] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](801, null, S[279], 48, 802, JA_I.T([]), JA_I.T([801,802,656]));
	out.data[802] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](802, null, S[280], 33, 656, JA_I.T([]), JA_I.T([802,656]));
	out.data[803] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](803, null, S[281], 33, 680, JA_I.T([]), JA_I.T([803,680,681,656,658]));
	out.data[804] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](804, null, S[118], 1057, 656, JA_I.T([]), JA_I.T([804,656]));
	out.data[805] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](805, null, S[282], 33, 693, JA_I.T([]), JA_I.T([805,693,656,690,694,691]));
	out.data[806] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](806, null, S[283], 33, 656, JA_I.T([]), JA_I.T([806,656]));
	out.data[807] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](807, null, S[284], 33, 705, JA_I.T([]), JA_I.T([807,705,696,680,681,656,658]));
	out.data[808] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](808, null, S[285], 49, 656, JA_I.T([724]), JA_I.T([808,656,724]));
	out.data[809] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](809, null, S[286], 33, 810, JA_I.T([]), JA_I.T([809,810,804,656]));
	out.data[810] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](810, null, S[287], 33, 804, JA_I.T([]), JA_I.T([810,804,656]));
	out.data[811] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](811, null, S[288], 33, 812, JA_I.T([726,658]), JA_I.T([811,812,656,726,658,746]));
	out.data[812] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](812, null, S[289], 1057, 656, JA_I.T([746]), JA_I.T([812,656,746]));
	out.data[813] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](813, null, S[290], 32, 656, JA_I.T([748]), JA_I.T([813,656,748]));
	out.data[814] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](814, null, S[291], 4128, 656, JA_I.T([]), JA_I.T([814,656]));
	out.data[815] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](815, null, S[292], 48, 816, JA_I.T([]), JA_I.T([815,816,728,656,747,723,724]));
	out.data[816] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](816, null, S[293], 1057, 728, JA_I.T([747]), JA_I.T([816,728,656,747,723,724]));
	out.data[817] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](817, null, S[294], 48, 818, JA_I.T([729]), JA_I.T([817,818,656,729]));
	out.data[818] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](818, null, S[295], 1056, 656, JA_I.T([]), JA_I.T([818,656]));
	out.data[819] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](819, null, S[296], 33, 719, JA_I.T([]), JA_I.T([819,719,681,656,658]));
	out.data[820] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](820, null, S[297], 33, 680, JA_I.T([]), JA_I.T([820,680,681,656,658]));
	out.data[821] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](821, null, S[298], 33, 656, JA_I.T([]), JA_I.T([821,656]));
	out.data[822] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](822, null, S[299], 48, 656, JA_I.T([729]), JA_I.T([822,656,729]));
	out.data[823] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](823, null, S[300], 48, 727, JA_I.T([725,658]), JA_I.T([823,727,728,656,725,658,722,723,724]));
	out.data[824] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](824, null, S[301], 48, 656, JA_I.T([825]), JA_I.T([824,656,825]));
	out.data[825] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](825, null, S[302], 1537, -1, JA_I.T([]), JA_I.T([825]));
	out.data[826] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](826, null, S[303], 48, 812, JA_I.T([658]), JA_I.T([826,812,656,658,746]));
	out.data[827] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](827, null, S[304], 48, 816, JA_I.T([658]), JA_I.T([827,816,728,656,658,747,723,724]));
	out.data[828] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](828, null, S[305], 33, 804, JA_I.T([]), JA_I.T([828,804,656]));
	out.data[829] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](829, null, S[306], 33, 810, JA_I.T([]), JA_I.T([829,810,804,656]));
	out.data[830] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](830, null, S[307], 33, 831, JA_I.T([]), JA_I.T([830,831,804,656]));
	out.data[831] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](831, null, S[308], 1056, 804, JA_I.T([]), JA_I.T([831,804,656]));
	out.data[833] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](833, null, S[309], 33, 834, JA_I.T([]), JA_I.T([833,834,835,656]));
	out.data[834] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](834, null, S[119], 1057, 835, JA_I.T([]), JA_I.T([834,835,656]));
	out.data[835] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](835, null, S[310], 1057, 656, JA_I.T([]), JA_I.T([835,656]));
	out.data[836] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](836, null, S[311], 33, 656, JA_I.T([]), JA_I.T([836,656]));
	out.data[837] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](837, null, S[312], 33, 810, JA_I.T([]), JA_I.T([837,810,804,656]));
	out.data[838] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](838, null, S[313], 33, 831, JA_I.T([]), JA_I.T([838,831,804,656]));
	out.data[839] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](839, null, S[314], 32, 727, JA_I.T([722,658,725]), JA_I.T([839,727,728,656,722,658,725,723,724]));
	out.data[840] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](840, null, S[315], 49, 656, JA_I.T([]), JA_I.T([840,656]));
	out.data[841] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](841, null, S[316], 9729, -1, JA_I.T([842]), JA_I.T([841,842]));
	out.data[842] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](842, null, S[317], 1537, -1, JA_I.T([]), JA_I.T([842]));
	out.data[843] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](843, null, S[318], 33, 656, JA_I.T([844]), JA_I.T([843,656,844]));
	out.data[844] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](844, null, S[319], 1537, -1, JA_I.T([]), JA_I.T([844]));
	out.data[845] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](845, null, S[320], 33, 656, JA_I.T([846]), JA_I.T([845,656,846]));
	out.data[846] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](846, null, S[321], 1537, -1, JA_I.T([]), JA_I.T([846]));
	out.data[847] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](847, null, S[322], 1057, 656, JA_I.T([]), JA_I.T([847,656]));
	out.data[848] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](848, null, S[323], 32, 656, JA_I.T([]), JA_I.T([848,656]));
	out.data[849] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](849, null, S[324], 48, 847, JA_I.T([]), JA_I.T([849,847,656]));
	out.data[850] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](850, null, S[325], 33, 656, JA_I.T([]), JA_I.T([850,656]));
	out.data[851] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](851, null, S[326], 33, 656, JA_I.T([]), JA_I.T([851,656]));
	out.data[852] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](852, null, S[327], 32, 656, JA_I.T([]), JA_I.T([852,656]));
	out.data[854] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](854, null, S[328], 33, 855, JA_I.T([659]), JA_I.T([854,855,656,659]));
	out.data[855] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](855, null, S[329], 1057, 656, JA_I.T([]), JA_I.T([855,656]));
	out.data[856] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](856, null, S[330], 33, 656, JA_I.T([]), JA_I.T([856,656]));
	out.data[857] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](857, null, S[331], 33, 735, JA_I.T([]), JA_I.T([857,735,696,680,681,656,658]));
	out.data[858] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](858, null, S[332], 33, 656, JA_I.T([859]), JA_I.T([858,656,859]));
	out.data[859] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](859, null, S[333], 1537, -1, JA_I.T([]), JA_I.T([859]));
	out.data[862] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](862, null, S[334], 49, 656, JA_I.T([]), JA_I.T([862,656]));
	out.data[863] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](863, null, S[335], 1057, 855, JA_I.T([659]), JA_I.T([863,855,656,659]));
	out.data[864] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](864, null, S[336], 1057, 855, JA_I.T([659]), JA_I.T([864,855,656,659]));
	out.data[865] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](865, null, S[337], 1056, 864, JA_I.T([866]), JA_I.T([865,864,855,656,866,659]));
	out.data[866] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](866, null, S[338], 1537, -1, JA_I.T([]), JA_I.T([866]));
	out.data[867] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](867, null, S[339], 49, 865, JA_I.T([]), JA_I.T([867,865,864,855,656,866,659]));
	out.data[868] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](868, null, S[340], 4128, 656, JA_I.T([]), JA_I.T([868,656]));
	out.data[869] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](869, null, S[341], 49, 865, JA_I.T([]), JA_I.T([869,865,864,855,656,866,659]));
	out.data[870] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](870, null, S[342], 1056, 863, JA_I.T([866]), JA_I.T([870,863,855,656,866,659]));
	out.data[871] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](871, null, S[343], 49, 870, JA_I.T([]), JA_I.T([871,870,863,855,656,866,659]));
	out.data[872] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](872, null, S[344], 4128, 656, JA_I.T([]), JA_I.T([872,656]));
	out.data[873] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](873, null, S[345], 49, 870, JA_I.T([]), JA_I.T([873,870,863,855,656,866,659]));
	out.data[874] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](874, null, S[346], 33, 656, JA_I.T([]), JA_I.T([874,656]));
	out.data[875] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](875, null, S[347], 1057, 855, JA_I.T([659,660,665,876]), JA_I.T([875,855,656,659,660,665,876]));
	out.data[876] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](876, null, S[348], 1537, -1, JA_I.T([]), JA_I.T([876]));
	out.data[877] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](877, null, S[349], 1057, 855, JA_I.T([659]), JA_I.T([877,855,656,659]));
	out.data[878] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](878, null, S[350], 32, 877, JA_I.T([866]), JA_I.T([878,877,855,656,866,659]));
	out.data[879] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](879, null, S[351], 49, 878, JA_I.T([]), JA_I.T([879,878,877,855,656,866,659]));
	out.data[880] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](880, null, S[352], 4128, 656, JA_I.T([]), JA_I.T([880,656]));
	out.data[881] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](881, null, S[353], 49, 878, JA_I.T([]), JA_I.T([881,878,877,855,656,866,659]));
	out.data[882] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](882, null, S[354], 1056, 875, JA_I.T([866]), JA_I.T([882,875,855,656,866,659,660,665,876]));
	out.data[883] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](883, null, S[355], 49, 882, JA_I.T([]), JA_I.T([883,882,875,855,656,866,659,660,665,876]));
	out.data[884] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](884, null, S[356], 4128, 656, JA_I.T([]), JA_I.T([884,656]));
	out.data[885] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](885, null, S[357], 49, 882, JA_I.T([]), JA_I.T([885,882,875,855,656,866,659,660,665,876]));
	out.data[886] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](886, null, S[358], 1057, 855, JA_I.T([659]), JA_I.T([886,855,656,659]));
	out.data[887] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](887, null, S[359], 1057, 855, JA_I.T([659]), JA_I.T([887,855,656,659]));
	out.data[888] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](888, null, S[360], 32, 887, JA_I.T([866]), JA_I.T([888,887,855,656,866,659]));
	out.data[889] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](889, null, S[361], 49, 888, JA_I.T([]), JA_I.T([889,888,887,855,656,866,659]));
	out.data[890] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](890, null, S[362], 49, 888, JA_I.T([]), JA_I.T([890,888,887,855,656,866,659]));
	out.data[891] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](891, null, S[363], 1056, 886, JA_I.T([866]), JA_I.T([891,886,855,656,866,659]));
	out.data[892] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](892, null, S[364], 49, 891, JA_I.T([]), JA_I.T([892,891,886,855,656,866,659]));
	out.data[893] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](893, null, S[365], 49, 891, JA_I.T([]), JA_I.T([893,891,886,855,656,866,659]));
	out.data[899] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](899, null, S[366], 32, 656, JA_I.T([]), JA_I.T([899,656]));
	out.data[903] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](903, null, S[367], 33, 656, JA_I.T([]), JA_I.T([903,656]));
	out.data[904] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](904, null, S[368], 32, 656, JA_I.T([]), JA_I.T([904,656]));
	out.data[905] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](905, null, S[369], 32, 656, JA_I.T([]), JA_I.T([905,656]));
	out.data[912] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](912, null, S[370], 33, 913, JA_I.T([]), JA_I.T([912,913,914,656,746,726,658]));
	out.data[913] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](913, null, S[371], 33, 914, JA_I.T([746,726,658]), JA_I.T([913,914,656,746,726,658]));
	out.data[914] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](914, null, S[372], 1057, 656, JA_I.T([]), JA_I.T([914,656]));
	out.data[915] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](915, null, S[373], 32, 656, JA_I.T([748]), JA_I.T([915,656,748]));
	out.data[916] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](916, null, S[374], 4128, 656, JA_I.T([]), JA_I.T([916,656]));
	out.data[917] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](917, null, S[375], 48, 816, JA_I.T([]), JA_I.T([917,816,728,656,747,723,724]));
	out.data[918] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](918, null, S[376], 48, 919, JA_I.T([729]), JA_I.T([918,919,656,729]));
	out.data[919] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](919, null, S[377], 1056, 656, JA_I.T([]), JA_I.T([919,656]));
	out.data[920] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](920, null, S[378], 33, 656, JA_I.T([659]), JA_I.T([920,656,659]));
	out.data[921] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](921, null, S[379], 33, 922, JA_I.T([]), JA_I.T([921,922,656]));
	out.data[922] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](922, null, S[380], 1057, 656, JA_I.T([]), JA_I.T([922,656]));
	out.data[923] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](923, null, S[381], 33, 656, JA_I.T([]), JA_I.T([923,656]));
	out.data[924] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](924, null, S[382], 1, 656, JA_I.T([841,842]), JA_I.T([924,656,841,842]));
	out.data[925] = j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"](925, null, S[383], 1, 656, JA_I.T([842,842]), JA_I.T([925,656,842]));
	return out;
};
function j_ClassInfo() {
}
j_ClassInfo.prototype = Object.create(java_lang_Object.prototype);
j_ClassInfo.prototype.constructor = j_ClassInfo;
j_ClassInfo.prototype._interfaces = null;
j_ClassInfo.prototype._modifiers = 0;
j_ClassInfo.prototype._id = 0;
j_ClassInfo.prototype._internalName = null;
j_ClassInfo.prototype._related = null;
j_ClassInfo.prototype.__name = null;
j_ClassInfo.prototype._parent = 0;
j_ClassInfo.prototype.___id = 0;
j_ClassInfo.SI = function(){};
j_ClassInfo.prototype.__JT__CLASS_ID = j_ClassInfo.__JT__CLASS_ID = 738;
j_ClassInfo.prototype.__JT__CLASS_IDS = j_ClassInfo.__JT__CLASS_IDS = [738,656];
j_ClassInfo.prototype["j.ClassInfo<init>(ILjava/lang/String;Ljava/lang/String;II[I[I)V"] = function(p0, p1, p2, p3, p4, p5, p6) { 
	var _G = 0, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA2 = (p1);
				(this)["java.lang.Object<init>()V"]();
				if (((lA2 != null))) {
					_G = 1;
					continue;
				}
				lA2 = (p2);
				_G = 1;
				continue;
			case 1:
				this._id = p0;
				this._internalName = (lA2);
				this.__name = p2;
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
};
j_ClassInfo["create(ILjava/lang/String;Ljava/lang/String;II[I[I)Lj/ClassInfo;"] = function(p0, p1, p2, p3, p4, p5, p6) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new j_ClassInfo()));
	fA0 = tA0;
	(tA0)["j.ClassInfo<init>(ILjava/lang/String;Ljava/lang/String;II[I[I)V"](p0, p1, p2, p3, p4, p5, p6);
	return (fA0);
};
function j_ProgramReflection() {
}
j_ProgramReflection.prototype = Object.create(java_lang_Object.prototype);
j_ProgramReflection.prototype.constructor = j_ProgramReflection;
j_ProgramReflection.prototype.___id = 0;
j_ProgramReflection.SI = function() { 
	j_ProgramReflection.__classInfos = null;
	j_ProgramReflection.__classNames = null;
	j_ProgramReflection.__classInfosByName = null;
};
j_ProgramReflection.prototype.__JT__CLASS_ID = j_ProgramReflection.__JT__CLASS_ID = 737;
j_ProgramReflection.prototype.__JT__CLASS_IDS = j_ProgramReflection.__JT__CLASS_IDS = [737,656];
j_ProgramReflection.prototype["j.ProgramReflection<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
j_ProgramReflection["_ensure()V"] = function() { 
	var _G = 0, lA0 = null, lA3 = null, lI1 = 0, lI2 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((j_ProgramReflection.__classInfos == null))) {
					_G = 1;
					continue;
				}
				return;
				_G = 1;
				continue;
			case 1:
				tA0 = ((new com_jtransc_ds_FastStringMap()));
				fA0 = tA0;
				(tA0)["com.jtransc.ds.FastStringMap<init>()V"]();
				j_ProgramReflection.__classInfosByName = (fA0);
				j_ProgramReflection.__classInfos = j_ProgramReflection["getAllClasses()[Lj/ClassInfo;"]();
				if (((j_ProgramReflection.__classInfos == null))) {
					_G = 2;
					continue;
				}
				j_ProgramReflection.__classNames = new JA_L(j_ProgramReflection.__classInfos.length, "[Ljava.lang.String;");
				lA0 = (j_ProgramReflection.__classInfos);
				lI1 = lA0.length;
				lI2 = 0;
				_G = 3;
				continue;
			case 3:
				if (((lI2 >= lI1))) {
					_G = 2;
					continue;
				}
				lA3 = ((lA0).data[lI2]);
				if (((lA3 != null))) {
					_G = 4;
					continue;
				}
				_G = 5;
				continue;
			case 4:
				j_ProgramReflection.__classInfosByName["set(Ljava/lang/String;Ljava/lang/Object;)V"]((lA3).__name, lA3);
				(j_ProgramReflection.__classNames).data[(lA3)._id] = ((lA3).__name);
				_G = 5;
				continue;
			case 5:
				lI2 = (((lI2 + 1))|0);
				_G = 3;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
j_ProgramReflection["getAllClasses()[Lj/ClassInfo;"] = function() { 
	var _G = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((j_ProgramReflection.__classInfos == null))) {
					_G = 1;
					continue;
				}
				fA0 = j_ProgramReflection.__classInfos;
				_G = 2;
				continue;
			case 1:
				fA0 = j_ProgramReflection$AllClasses["getAllClasses()[Lj/ClassInfo;"]();
				_G = 2;
				continue;
			case 2:return fA0; 
			default:
				break;
		}
	}
	return null;
};
j_ProgramReflection["getClassInfoWithName(Ljava/lang/String;)Lj/ClassInfo;"] = function(p0) { 
	var _G = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				j_ProgramReflection["_ensure()V"]();
				if (!(j_ProgramReflection["hasClassWithName(Ljava/lang/String;)Z"](p0))) {
					_G = 1;
					continue;
				}
				fA0 = (N.checkCast(j_ProgramReflection.__classInfosByName["get(Ljava/lang/String;)Ljava/lang/Object;"](p0), j_ClassInfo));
				_G = 2;
				continue;
			case 1:
				fA0 = null;
				_G = 2;
				continue;
			case 2:return (fA0); 
			default:
				break;
		}
	}
	return null;
};
j_ProgramReflection["hasClassWithName(Ljava/lang/String;)Z"] = function(p0) { 
	j_ProgramReflection["_ensure()V"]();
	return j_ProgramReflection.__classInfosByName["has(Ljava/lang/String;)Z"](p0);
};
j_ProgramReflection["getClassById(I)Ljava/lang/Class;"] = function(p0) { 
	var _G = 0, lA1 = null, fA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						j_ProgramReflection["_ensure()V"]();
						_G = 1;
						continue;
					case 1:
						fA0 = (java_lang_Class["forName(Ljava/lang/String;)Ljava/lang/Class;"]((((j_ProgramReflection.__classNames).data[p0]))));
						_G = 2;
						continue;
					case 2:return (fA0); 
					case 3:
						fA0 = (J__exception__);
						lA1 = fA0;
						(lA1)["printStackTrace()V"]();
						return null;
					default:
						break;
				}
			}
			return null;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_ClassNotFoundException)))) {
				_G = 3;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return null;
};
function java_lang_jtransc_JTranscCoreReflection() {
}
java_lang_jtransc_JTranscCoreReflection.prototype = Object.create(java_lang_Object.prototype);
java_lang_jtransc_JTranscCoreReflection.prototype.constructor = java_lang_jtransc_JTranscCoreReflection;
java_lang_jtransc_JTranscCoreReflection.prototype.___id = 0;
java_lang_jtransc_JTranscCoreReflection.SI = function(){};
java_lang_jtransc_JTranscCoreReflection.prototype.__JT__CLASS_ID = java_lang_jtransc_JTranscCoreReflection.__JT__CLASS_ID = 736;
java_lang_jtransc_JTranscCoreReflection.prototype.__JT__CLASS_IDS = java_lang_jtransc_JTranscCoreReflection.__JT__CLASS_IDS = [736,656];
java_lang_jtransc_JTranscCoreReflection.prototype["java.lang.jtransc.JTranscCoreReflection<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang_jtransc_JTranscCoreReflection["getClassById(I)Ljava/lang/Class;"] = function(p0) { 
	j_ProgramReflection["_ensure()V"]();
	return java_lang_jtransc_JTranscCoreReflection["getClassByName(Ljava/lang/String;)Ljava/lang/Class;"](java_lang_jtransc_JTranscCoreReflection["getClassNameById(I)Ljava/lang/String;"](p0));
};
java_lang_jtransc_JTranscCoreReflection["getClassNameById(I)Ljava/lang/String;"] = function(p0) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (java_lang_jtransc_JTranscCoreReflection["checkClassId(I)Z"](p0)) {
					_G = 1;
					continue;
				}
				return null;
			case 1:
				j_ProgramReflection["_ensure()V"]();
				return (((j_ProgramReflection.__classInfos).data[p0])).__name;
			default:
				break;
		}
	}
	return null;
};
java_lang_jtransc_JTranscCoreReflection["checkClassId(I)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				j_ProgramReflection["_ensure()V"]();
				if (((p0 < 0))) {
					_G = 1;
					continue;
				}
				if (((p0 >= j_ProgramReflection.__classInfos.length))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_jtransc_JTranscCoreReflection["getClassByName(Ljava/lang/String;)Ljava/lang/Class;"] = function(p0) { 
	var _G = 0, fA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						_G = 1;
						continue;
					case 1:
						fA0 = (java_lang_Class["forName(Ljava/lang/String;)Ljava/lang/Class;"](p0));
						_G = 2;
						continue;
					case 2:return (fA0); 
					case 3:
						fA0 = (J__exception__);
						return null;
					default:
						break;
				}
			}
			return null;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_ClassNotFoundException)))) {
				_G = 3;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return null;
};
java_lang_jtransc_JTranscCoreReflection["getClassId(Ljava/lang/Object;)I"] = function(p0) { 
	return p0.__JT__CLASS_ID;
};
java_lang_jtransc_JTranscCoreReflection["isArray(Ljava/lang/Object;)Z"] = function(p0) { 
	return p0 ? (p0 instanceof JA_0) : false;
};
java_lang_jtransc_JTranscCoreReflection["getArrayDescriptor(Ljava/lang/Object;)Ljava/lang/String;"] = function(p0) { 
	return p0 ? N.str(p0.desc) : null;
};
java_lang_jtransc_JTranscCoreReflection["getClassInfoWithName(Ljava/lang/String;)Lj/ClassInfo;"] = function(p0) { 
	return j_ProgramReflection["getClassInfoWithName(Ljava/lang/String;)Lj/ClassInfo;"](p0);
};
java_lang_jtransc_JTranscCoreReflection["getModifiersWithId(I)I"] = function(p0) { 
	j_ProgramReflection["_ensure()V"]();
	return (((j_ProgramReflection.__classInfos).data[p0]))._modifiers;
};
java_lang_jtransc_JTranscCoreReflection["hasClassWithName(Ljava/lang/String;)Z"] = function(p0) { 
	return j_ProgramReflection["hasClassWithName(Ljava/lang/String;)Z"](p0);
};
function java_util_ConcurrentModificationException() {
}
java_util_ConcurrentModificationException.prototype = Object.create(java_lang_RuntimeException.prototype);
java_util_ConcurrentModificationException.prototype.constructor = java_util_ConcurrentModificationException;
java_util_ConcurrentModificationException.SI = function(){};
java_util_ConcurrentModificationException.prototype.__JT__CLASS_ID = java_util_ConcurrentModificationException.__JT__CLASS_ID = 734;
java_util_ConcurrentModificationException.prototype.__JT__CLASS_IDS = java_util_ConcurrentModificationException.__JT__CLASS_IDS = [734,696,680,681,656,658];
java_util_ConcurrentModificationException.prototype["java.util.ConcurrentModificationException<init>()V"] = function() { 
	(this)["java.lang.RuntimeException<init>()V"]();
	return this;
	return this;
};
function java_util_NoSuchElementException() {
}
java_util_NoSuchElementException.prototype = Object.create(java_lang_RuntimeException.prototype);
java_util_NoSuchElementException.prototype.constructor = java_util_NoSuchElementException;
java_util_NoSuchElementException.SI = function(){};
java_util_NoSuchElementException.prototype.__JT__CLASS_ID = java_util_NoSuchElementException.__JT__CLASS_ID = 733;
java_util_NoSuchElementException.prototype.__JT__CLASS_IDS = java_util_NoSuchElementException.__JT__CLASS_IDS = [733,696,680,681,656,658];
java_util_NoSuchElementException.prototype["java.util.NoSuchElementException<init>()V"] = function() { 
	(this)["java.lang.RuntimeException<init>()V"]();
	return this;
	return this;
};
function java_util_ListIterator() {
}
java_util_ListIterator.prototype = Object.create(java_lang_Object_base.prototype);
java_util_ListIterator.prototype.constructor = java_util_ListIterator;
java_util_ListIterator.SI = function(){};
java_util_ListIterator.prototype.__JT__CLASS_ID = java_util_ListIterator.__JT__CLASS_ID = 731;
java_util_ListIterator.prototype.__JT__CLASS_IDS = java_util_ListIterator.__JT__CLASS_IDS = [731,729,656];
java_util_ListIterator.prototype["next()Ljava/lang/Object;"] = function() { N.methodWithoutBody('java.util.ListIterator.next') };
java_util_ListIterator.prototype["hasNext()Z"] = function() { N.methodWithoutBody('java.util.ListIterator.hasNext') };
java_util_ListIterator.prototype["previousIndex()I"] = function() { N.methodWithoutBody('java.util.ListIterator.previousIndex') };
function java_util_AbstractList$SimpleListIterator() {
}
java_util_AbstractList$SimpleListIterator.prototype = Object.create(java_lang_Object.prototype);
java_util_AbstractList$SimpleListIterator.prototype.constructor = java_util_AbstractList$SimpleListIterator;
java_util_AbstractList$SimpleListIterator.prototype._expectedModCount = 0;
java_util_AbstractList$SimpleListIterator.prototype._this_0 = null;
java_util_AbstractList$SimpleListIterator.prototype._pos = 0;
java_util_AbstractList$SimpleListIterator.prototype._lastPosition = 0;
java_util_AbstractList$SimpleListIterator.prototype.___id = 0;
java_util_AbstractList$SimpleListIterator.SI = function(){};
java_util_AbstractList$SimpleListIterator.prototype.__JT__CLASS_ID = java_util_AbstractList$SimpleListIterator.__JT__CLASS_ID = 730;
java_util_AbstractList$SimpleListIterator.prototype.__JT__CLASS_IDS = java_util_AbstractList$SimpleListIterator.__JT__CLASS_IDS = [730,656,729];
java_util_AbstractList$SimpleListIterator.prototype["java.util.AbstractList$SimpleListIterator<init>(Ljava/util/AbstractList;)V"] = function(p0) { 
	this._this_0 = p0;
	(this)["java.lang.Object<init>()V"]();
	this._pos = -1;
	this._lastPosition = -1;
	this._expectedModCount = p0._modCount;
	return this;
	return this;
};
java_util_AbstractList$SimpleListIterator.prototype["next()Ljava/lang/Object;"] = function() { 
	var _G = 0, lA1 = null, fI1 = 0, fA0 = null, fA1 = null, tI1 = 0, tA2 = null, tA4 = null, tA3 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						if (((this._expectedModCount != this._this_0._modCount))) {
							_G = 1;
							continue;
						}
						_G = 2;
						continue;
					case 2:
						lA1 = this._this_0["get(I)Ljava/lang/Object;"]((((this._pos + 1))|0));
						fA0 = (this);
						fA1 = (this);
						tA2 = fA1;
						tI1 = (((this._pos + 1))|0);
						fI1 = tI1;
						(tA2)._pos = tI1;
						(fA0)._lastPosition = fI1;
						fA0 = lA1;
						_G = 3;
						continue;
					case 3:return fA0; 
					case 4:
						fA0 = (J__exception__);
						lA1 = fA0;
						tA4 = ((new java_util_NoSuchElementException()));
						fA0 = tA4;
						(tA4)["java.util.NoSuchElementException<init>()V"]();
						throw new WrappedError(fA0);
						_G = 1;
						continue;
					case 1:
						tA3 = ((new java_util_ConcurrentModificationException()));
						fA0 = tA3;
						(tA3)["java.util.ConcurrentModificationException<init>()V"]();
						throw new WrappedError(fA0);
						break;
					default:
						break;
				}
			}
			return null;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 2)) && ((_G < 3)))) && (J__exception__ instanceof java_lang_IndexOutOfBoundsException)))) {
				_G = 4;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return null;
};
java_util_AbstractList$SimpleListIterator.prototype["hasNext()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if ((((((this._pos + 1))|0) >= this._this_0["size()I"]()))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
function java_util_AbstractList$FullListIterator() {
}
java_util_AbstractList$FullListIterator.prototype = Object.create(java_util_AbstractList$SimpleListIterator.prototype);
java_util_AbstractList$FullListIterator.prototype.constructor = java_util_AbstractList$FullListIterator;
java_util_AbstractList$FullListIterator.prototype._this_0_ = null;
java_util_AbstractList$FullListIterator.prototype._expectedModCount = 0;
java_util_AbstractList$FullListIterator.prototype._this_0 = null;
java_util_AbstractList$FullListIterator.prototype._pos = 0;
java_util_AbstractList$FullListIterator.prototype._lastPosition = 0;
java_util_AbstractList$FullListIterator.SI = function(){};
java_util_AbstractList$FullListIterator.prototype.__JT__CLASS_ID = java_util_AbstractList$FullListIterator.__JT__CLASS_ID = 732;
java_util_AbstractList$FullListIterator.prototype.__JT__CLASS_IDS = java_util_AbstractList$FullListIterator.__JT__CLASS_IDS = [732,730,656,731,729];
java_util_AbstractList$FullListIterator.prototype["java.util.AbstractList$FullListIterator<init>(Ljava/util/AbstractList;I)V"] = function(p0, p1) { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				this._this_0_ = p0;
				(this)["java.util.AbstractList$SimpleListIterator<init>(Ljava/util/AbstractList;)V"](p0);
				if (((p1 < 0))) {
					_G = 1;
					continue;
				}
				if (((p1 > p0["size()I"]()))) {
					_G = 1;
					continue;
				}
				this._pos = (((p1 - 1))|0);
				_G = 2;
				continue;
			case 1:
				tA0 = ((new java_lang_IndexOutOfBoundsException()));
				fA0 = tA0;
				(tA0)["java.lang.IndexOutOfBoundsException<init>()V"]();
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
java_util_AbstractList$FullListIterator.prototype["previousIndex()I"] = function() { 
	return this._pos;
};
function java_util_ArrayList() {
}
java_util_ArrayList.prototype = Object.create(java_util_AbstractList.prototype);
java_util_ArrayList.prototype.constructor = java_util_ArrayList;
java_util_ArrayList.prototype._modCount = 0;
java_util_ArrayList.SI = function(){};
java_util_ArrayList.prototype.__JT__CLASS_ID = java_util_ArrayList.__JT__CLASS_ID = 721;
java_util_ArrayList.prototype.__JT__CLASS_IDS = java_util_ArrayList.__JT__CLASS_IDS = [721,727,728,656,722,725,726,658,723,724];
java_util_ArrayList.prototype["java.util.ArrayList<init>()V"] = function() { 
	this["java.util.ArrayList<init>(I)V"](0);
	return this;
	return this;
};
java_util_ArrayList.prototype["java.util.ArrayList<init>(I)V"] = function(p0) { 
	this._data = [];
	return this;
};
java_util_ArrayList.prototype["size()I"] = function() { 
	return this._data.length;
};
java_util_ArrayList.prototype["isEmpty()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this["size()I"]() != 0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_util_ArrayList.prototype["iterator()Ljava/util/Iterator;"] = function() { 
	return (this["listIterator()Ljava/util/ListIterator;"]());
};
java_util_ArrayList.prototype["get(I)Ljava/lang/Object;"] = function(p0) { 
	this["rangeCheck(I)V"](p0);
	return this["_get(I)Ljava/lang/Object;"](p0);
};
java_util_ArrayList.prototype["_get(I)Ljava/lang/Object;"] = function(p0) { 
	return this._data[p0];
};
java_util_ArrayList.prototype["rangeCheck(I)V"] = function(p0) { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 < this["size()I"]()))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_IndexOutOfBoundsException()));
				fA0 = tA0;
				(tA0)["java.lang.IndexOutOfBoundsException<init>(Ljava/lang/String;)V"](this["outOfBoundsMsg(I)Ljava/lang/String;"](p0));
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				return;
			default:
				break;
		}
	}
	return;
};
java_util_ArrayList.prototype["outOfBoundsMsg(I)Ljava/lang/String;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_StringBuilder()));
	fA0 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[385])["append(I)Ljava/lang/StringBuilder;"](p0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[384])["append(I)Ljava/lang/StringBuilder;"](this["size()I"]())["toString()Ljava/lang/String;"]();
};
java_util_ArrayList.prototype["add(Ljava/lang/Object;)Z"] = function(p0) { 
	this["_add(Ljava/lang/Object;)V"](p0);
	return true;
};
java_util_ArrayList.prototype["_add(Ljava/lang/Object;)V"] = function(p0) { 
	this._data.push(p0);
};
java_util_ArrayList.prototype["add(ILjava/lang/Object;)V"] = function(p0, p1) { 
	this["rangeCheckForAdd(I)V"](p0);
	this["_insert(ILjava/lang/Object;)V"](p0, p1);
	return;
};
java_util_ArrayList.prototype["_insert(ILjava/lang/Object;)V"] = function(p0, p1) { 
	this._data.splice(p0, 0, p1);
};
java_util_ArrayList.prototype["rangeCheckForAdd(I)V"] = function(p0) { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 > this["size()I"]()))) {
					_G = 1;
					continue;
				}
				if (((p0 >= 0))) {
					_G = 2;
					continue;
				}
				_G = 1;
				continue;
			case 1:
				tA0 = ((new java_lang_IndexOutOfBoundsException()));
				fA0 = tA0;
				(tA0)["java.lang.IndexOutOfBoundsException<init>(Ljava/lang/String;)V"](this["outOfBoundsMsg(I)Ljava/lang/String;"](p0));
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
java_util_ArrayList.prototype["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"] = function(p0) { 
	var _G = 0, lA1 = null, lI2 = 0, lI3 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = (p0);
				lI2 = this["size()I"]();
				if (((lA1.length >= lI2))) {
					_G = 1;
					continue;
				}
				lA1 = (N.checkCast(java_util_Arrays["copyOf([Ljava/lang/Object;ILjava/lang/Class;)[Ljava/lang/Object;"](new JA_L(0, "[Ljava.lang.Object;"), lI2, lA1["getClass()Ljava/lang/Class;"]()), JA_L));
				_G = 1;
				continue;
			case 1:
				lI3 = 0;
				_G = 2;
				continue;
			case 2:
				if (((lI3 >= lI2))) {
					_G = 3;
					continue;
				}
				(lA1).data[lI3] = this["_get(I)Ljava/lang/Object;"](lI3);
				lI3 = (((lI3 + 1))|0);
				_G = 2;
				continue;
			case 3:
				return (lA1);
			default:
				break;
		}
	}
	return null;
};
java_util_ArrayList.prototype["contains(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this["indexOf(Ljava/lang/Object;)I"](p0) < 0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_util_ArrayList.prototype["indexOf(Ljava/lang/Object;)I"] = function(p0) { 
	var _G = 0, lI2 = 0, lI3 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI2 = this["size()I"]();
				lI3 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI3 >= lI2))) {
					_G = 2;
					continue;
				}
				if (!(java_util_Objects["equals(Ljava/lang/Object;Ljava/lang/Object;)Z"](p0, this["_get(I)Ljava/lang/Object;"](lI3)))) {
					_G = 3;
					continue;
				}
				return lI3;
			case 3:
				lI3 = (((lI3 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return -1;
			default:
				break;
		}
	}
	return 0;
};
function java_lang_reflect_ArrayType() {
}
java_lang_reflect_ArrayType.prototype = Object.create(java_lang_Object.prototype);
java_lang_reflect_ArrayType.prototype.constructor = java_lang_reflect_ArrayType;
java_lang_reflect_ArrayType.prototype._element = null;
java_lang_reflect_ArrayType.prototype.___id = 0;
java_lang_reflect_ArrayType.SI = function(){};
java_lang_reflect_ArrayType.prototype.__JT__CLASS_ID = java_lang_reflect_ArrayType.__JT__CLASS_ID = 720;
java_lang_reflect_ArrayType.prototype.__JT__CLASS_IDS = java_lang_reflect_ArrayType.__JT__CLASS_IDS = [720,656,667];
java_lang_reflect_ArrayType.prototype["java.lang.reflect.ArrayType<init>(Ljava/lang/reflect/Type;)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	this._element = p0;
	return this;
	return this;
};
function com_jtransc_text_MStringReader() {
}
com_jtransc_text_MStringReader.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_text_MStringReader.prototype.constructor = com_jtransc_text_MStringReader;
com_jtransc_text_MStringReader.prototype._length = 0;
com_jtransc_text_MStringReader.prototype._offset = 0;
com_jtransc_text_MStringReader.prototype._str = null;
com_jtransc_text_MStringReader.prototype.___id = 0;
com_jtransc_text_MStringReader.SI = function(){};
com_jtransc_text_MStringReader.prototype.__JT__CLASS_ID = com_jtransc_text_MStringReader.__JT__CLASS_ID = 718;
com_jtransc_text_MStringReader.prototype.__JT__CLASS_IDS = com_jtransc_text_MStringReader.__JT__CLASS_IDS = [718,656];
com_jtransc_text_MStringReader.prototype["com.jtransc.text.MStringReader<init>(Ljava/lang/String;)V"] = function(p0) { 
	this["com.jtransc.text.MStringReader<init>(Ljava/lang/String;I)V"](p0, 0);
	return this;
	return this;
};
com_jtransc_text_MStringReader.prototype["com.jtransc.text.MStringReader<init>(Ljava/lang/String;I)V"] = function(p0, p1) { 
	(this)["java.lang.Object<init>()V"]();
	this._str = p0;
	this._length = p0["length()I"]();
	this._offset = p1;
	return this;
	return this;
};
com_jtransc_text_MStringReader.prototype["hasMore()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._offset >= this._length))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
com_jtransc_text_MStringReader.prototype["peek()C"] = function() { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (this["hasMore()Z"]()) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_Error()));
				fA0 = tA0;
				(tA0)["java.lang.Error<init>(Ljava/lang/String;)V"](S[386]);
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				return this._str["charAt(I)C"](this._offset);
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_text_MStringReader.prototype["read()C"] = function() { 
	var _G = 0, lI1 = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (this["hasMore()Z"]()) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_Error()));
				fA0 = tA0;
				(tA0)["java.lang.Error<init>(Ljava/lang/String;)V"](S[386]);
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				lI1 = ((this["peek()C"]())|0);
				this["skip()V"]();
				return ((lI1)&0xFFFF);
			default:
				break;
		}
	}
	return 0;
};
com_jtransc_text_MStringReader.prototype["skip()V"] = function() { 
	this["skip(I)V"](1);
	return;
};
com_jtransc_text_MStringReader.prototype["skip(I)V"] = function(p0) { 
	this._offset = (((this._offset + p0))|0);
	return;
};
com_jtransc_text_MStringReader.prototype["readUntil(CCZ)Ljava/lang/String;"] = function(p0, p1, p2) { 
	return this["readUntil(CCCZ)Ljava/lang/String;"](p0, p1, p1, p2);
};
com_jtransc_text_MStringReader.prototype["readUntil(CCCZ)Ljava/lang/String;"] = function(p0, p1, p2, p3) { 
	var _G = 0, lI5 = 0, lI6 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI5 = this._offset;
				_G = 1;
				continue;
			case 1:
				if (!(this["hasMore()Z"]())) {
					_G = 2;
					continue;
				}
				lI6 = ((this["read()C"]())|0);
				if (((lI6 == ((p0)|0)))) {
					_G = 3;
					continue;
				}
				if (((lI6 == ((p1)|0)))) {
					_G = 3;
					continue;
				}
				if (((lI6 != ((p2)|0)))) {
					_G = 4;
					continue;
				}
				_G = 3;
				continue;
			case 3:
				if (p3) {
					_G = 2;
					continue;
				}
				this["skip(I)V"](-1);
				_G = 2;
				continue;
			case 4:
				_G = 1;
				continue;
			case 2:
				return this._str["substring(II)Ljava/lang/String;"](lI5, this._offset);
			default:
				break;
		}
	}
	return null;
};
com_jtransc_text_MStringReader.prototype["expect(C)V"] = function(p0) { 
	var _G = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null, tA2 = null, tA3 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (this["hasMore()Z"]()) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_Error()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.Error<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[388])["append(C)Ljava/lang/StringBuilder;"](p0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[387])["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				if (((this["read()C"]() == ((p0)|0)))) {
					_G = 2;
					continue;
				}
				tA2 = ((new java_lang_Error()));
				fA0 = tA2;
				fA1 = tA2;
				tA3 = ((new java_lang_StringBuilder()));
				fA2 = tA3;
				(tA3)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.Error<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[388])["append(C)Ljava/lang/StringBuilder;"](p0)["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 2;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
function java_lang_reflect_MethodTypeImpl() {
}
java_lang_reflect_MethodTypeImpl.prototype = Object.create(java_lang_Object.prototype);
java_lang_reflect_MethodTypeImpl.prototype.constructor = java_lang_reflect_MethodTypeImpl;
java_lang_reflect_MethodTypeImpl.prototype._args = null;
java_lang_reflect_MethodTypeImpl.prototype._rettype = null;
java_lang_reflect_MethodTypeImpl.prototype.___id = 0;
java_lang_reflect_MethodTypeImpl.SI = function(){};
java_lang_reflect_MethodTypeImpl.prototype.__JT__CLASS_ID = java_lang_reflect_MethodTypeImpl.__JT__CLASS_ID = 717;
java_lang_reflect_MethodTypeImpl.prototype.__JT__CLASS_IDS = java_lang_reflect_MethodTypeImpl.__JT__CLASS_IDS = [717,656,667];
java_lang_reflect_MethodTypeImpl.prototype["java.lang.reflect.MethodTypeImpl<init>([Ljava/lang/reflect/Type;Ljava/lang/reflect/Type;)V"] = function(p0, p1) { 
	(this)["java.lang.Object<init>()V"]();
	this._args = p0;
	this._rettype = p1;
	return this;
	return this;
};
function j_MemberInfo() {
}
j_MemberInfo.prototype = Object.create(java_lang_Object.prototype);
j_MemberInfo.prototype.constructor = j_MemberInfo;
j_MemberInfo.prototype._modifiers = 0;
j_MemberInfo.prototype._desc = null;
j_MemberInfo.prototype._internalName = null;
j_MemberInfo.prototype._id = 0;
j_MemberInfo.prototype.__name = null;
j_MemberInfo.prototype._genericDesc = null;
j_MemberInfo.prototype.___id = 0;
j_MemberInfo.SI = function(){};
j_MemberInfo.prototype.__JT__CLASS_ID = j_MemberInfo.__JT__CLASS_ID = 716;
j_MemberInfo.prototype.__JT__CLASS_IDS = j_MemberInfo.__JT__CLASS_IDS = [716,656];
j_MemberInfo.prototype["j.MemberInfo<init>(ILjava/lang/String;Ljava/lang/String;ILjava/lang/String;Ljava/lang/String;)V"] = function(p0, p1, p2, p3, p4, p5) { 
	var _G = 0, fA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.lang.Object<init>()V"]();
				if ((((p1) == null))) {
					_G = 1;
					continue;
				}
				fA1 = p1;
				_G = 2;
				continue;
			case 1:
				fA1 = p2;
				_G = 2;
				continue;
			case 2:
				this._internalName = fA1;
				this._id = p0;
				this.__name = p2;
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
};
j_MemberInfo["create(ILjava/lang/String;Ljava/lang/String;ILjava/lang/String;Ljava/lang/String;)Lj/MemberInfo;"] = function(p0, p1, p2, p3, p4, p5) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new j_MemberInfo()));
	fA0 = tA0;
	(tA0)["j.MemberInfo<init>(ILjava/lang/String;Ljava/lang/String;ILjava/lang/String;Ljava/lang/String;)V"](p0, p1, p2, p3, p4, p5);
	return (fA0);
};
j_MemberInfo["createList(I[I[I[Ljava/lang/String;[Ljava/lang/String;[Ljava/lang/String;[Ljava/lang/String;)[Lj/MemberInfo;"] = function(p0, p1, p2, p3, p4, p5, p6) { 
	var _G = 0, lA7 = null, fI1 = 0, lI8 = 0, fA0 = null, fA2 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA7 = (new JA_L(p0, "[Lj.MemberInfo;"));
				lI8 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI8 >= p0))) {
					_G = 2;
					continue;
				}
				fA0 = lA7;
				fI1 = lI8;
				tA0 = ((new j_MemberInfo()));
				fA2 = tA0;
				(tA0)["j.MemberInfo<init>(ILjava/lang/String;Ljava/lang/String;ILjava/lang/String;Ljava/lang/String;)V"]((p1.data[lI8]), (((p3).data[lI8])), (((p4).data[lI8])), (p2.data[lI8]), (((p5).data[lI8])), (((p6).data[lI8])));
				(fA0).data[fI1] = fA2;
				lI8 = (((lI8 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return (lA7);
			default:
				break;
		}
	}
	return null;
};
function java_lang_reflect_AnnotatedElement() {
}
java_lang_reflect_AnnotatedElement.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_reflect_AnnotatedElement.prototype.constructor = java_lang_reflect_AnnotatedElement;
java_lang_reflect_AnnotatedElement.SI = function(){};
java_lang_reflect_AnnotatedElement.prototype.__JT__CLASS_ID = java_lang_reflect_AnnotatedElement.__JT__CLASS_ID = 669;
java_lang_reflect_AnnotatedElement.prototype.__JT__CLASS_IDS = java_lang_reflect_AnnotatedElement.__JT__CLASS_IDS = [669,656];
// ABSTRACT
function java_lang_reflect_AccessibleObject() {
}
java_lang_reflect_AccessibleObject.prototype = Object.create(java_lang_Object.prototype);
java_lang_reflect_AccessibleObject.prototype.constructor = java_lang_reflect_AccessibleObject;
java_lang_reflect_AccessibleObject.prototype._info = null;
java_lang_reflect_AccessibleObject.prototype.___id = 0;
java_lang_reflect_AccessibleObject.SI = function(){};
java_lang_reflect_AccessibleObject.prototype.__JT__CLASS_ID = java_lang_reflect_AccessibleObject.__JT__CLASS_ID = 676;
java_lang_reflect_AccessibleObject.prototype.__JT__CLASS_IDS = java_lang_reflect_AccessibleObject.__JT__CLASS_IDS = [676,656,669];
java_lang_reflect_AccessibleObject.prototype["java.lang.reflect.AccessibleObject<init>(Lj/MemberInfo;)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	this._info = p0;
	return this;
	return this;
};
// ABSTRACT
function java_lang_reflect_MethodConstructor() {
}
java_lang_reflect_MethodConstructor.prototype = Object.create(java_lang_reflect_AccessibleObject.prototype);
java_lang_reflect_MethodConstructor.prototype.constructor = java_lang_reflect_MethodConstructor;
java_lang_reflect_MethodConstructor.prototype._modifiers = 0;
java_lang_reflect_MethodConstructor.prototype.__name = null;
java_lang_reflect_MethodConstructor.prototype._methodType = null;
java_lang_reflect_MethodConstructor.prototype._signature = null;
java_lang_reflect_MethodConstructor.prototype._clazz = null;
java_lang_reflect_MethodConstructor.prototype._id = 0;
java_lang_reflect_MethodConstructor.prototype._exceptionTypes = null;
java_lang_reflect_MethodConstructor.prototype._genericSignature = null;
java_lang_reflect_MethodConstructor.prototype._slot = 0;
java_lang_reflect_MethodConstructor.prototype._info = null;
java_lang_reflect_MethodConstructor.SI = function(){};
java_lang_reflect_MethodConstructor.prototype.__JT__CLASS_ID = java_lang_reflect_MethodConstructor.__JT__CLASS_ID = 715;
java_lang_reflect_MethodConstructor.prototype.__JT__CLASS_IDS = java_lang_reflect_MethodConstructor.__JT__CLASS_IDS = [715,676,656,669];
java_lang_reflect_MethodConstructor.prototype["java.lang.reflect.MethodConstructor<init>(Ljava/lang/Class;Lj/MemberInfo;)V"] = function(p0, p1) { 
	(this)["java.lang.reflect.AccessibleObject<init>(Lj/MemberInfo;)V"](p1);
	this._exceptionTypes = new JA_L(0, "[Ljava.lang.Class;");
	this._clazz = p0;
	this._id = p1._id;
	this._slot = p1._id;
	this.__name = p1.__name;
	this._signature = p1._desc;
	this._genericSignature = p1._genericDesc;
	this._modifiers = p1._modifiers;
	return this;
	return this;
};
java_lang_reflect_MethodConstructor.prototype["toString()Ljava/lang/String;"] = function() { 
	var _G = 0, lA4 = null, lA7 = null, lI1 = 0, lI3 = 0, lI5 = 0, lI6 = 0, fA0 = null, tA0 = null, tA1 = null, tA2 = null, tA3 = null, tA4 = null, tA5 = null, tA6 = null, tA7 = null, lA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = this["getModifiers()I"]();
				lA2 = S[20];
				if (((lI1 == 0))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				lA2 = (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](lA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_reflect_Modifier["toString(I)Ljava/lang/String;"](lI1))["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[389])["toString()Ljava/lang/String;"]();
				_G = 1;
				continue;
			case 1:
				if (((this["getReturnType()Ljava/lang/Class;"]() == null))) {
					_G = 2;
					continue;
				}
				tA1 = ((new java_lang_StringBuilder()));
				fA0 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				lA2 = (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](lA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_reflect__InternalUtils["getTypeName(Ljava/lang/reflect/Type;)Ljava/lang/String;"]((this["getReturnType()Ljava/lang/Class;"]())))["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[389])["toString()Ljava/lang/String;"]();
				_G = 2;
				continue;
			case 2:
				tA2 = ((new java_lang_StringBuilder()));
				fA0 = tA2;
				(tA2)["java.lang.StringBuilder<init>()V"]();
				lA2 = (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](lA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_reflect__InternalUtils["getTypeName(Ljava/lang/reflect/Type;)Ljava/lang/String;"]((this["getDeclaringClass()Ljava/lang/Class;"]())))["toString()Ljava/lang/String;"]();
				if (this["isConstructor()Z"]()) {
					_G = 3;
					continue;
				}
				tA3 = ((new java_lang_StringBuilder()));
				fA0 = tA3;
				(tA3)["java.lang.StringBuilder<init>()V"]();
				lA2 = (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](lA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[135])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this["getName()Ljava/lang/String;"]())["toString()Ljava/lang/String;"]();
				_G = 3;
				continue;
			case 3:
				tA4 = ((new java_lang_StringBuilder()));
				fA0 = tA4;
				(tA4)["java.lang.StringBuilder<init>()V"]();
				lA2 = (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](lA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[137])["toString()Ljava/lang/String;"]();
				lI3 = 1;
				lA4 = (this["getParameterTypes()[Ljava/lang/Class;"]());
				lI5 = lA4.length;
				lI6 = 0;
				_G = 4;
				continue;
			case 4:
				if (((lI6 >= lI5))) {
					_G = 5;
					continue;
				}
				lA7 = ((lA4).data[lI6]);
				if (((lI3 != 0))) {
					_G = 6;
					continue;
				}
				tA5 = ((new java_lang_StringBuilder()));
				fA0 = tA5;
				(tA5)["java.lang.StringBuilder<init>()V"]();
				lA2 = (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](lA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[62])["toString()Ljava/lang/String;"]();
				_G = 6;
				continue;
			case 6:
				tA6 = ((new java_lang_StringBuilder()));
				fA0 = tA6;
				(tA6)["java.lang.StringBuilder<init>()V"]();
				lA2 = (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](lA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_reflect__InternalUtils["getTypeName(Ljava/lang/reflect/Type;)Ljava/lang/String;"]((lA7)))["toString()Ljava/lang/String;"]();
				lI3 = 0;
				lI6 = (((lI6 + 1))|0);
				_G = 4;
				continue;
			case 5:
				tA7 = ((new java_lang_StringBuilder()));
				fA0 = tA7;
				(tA7)["java.lang.StringBuilder<init>()V"]();
				lA2 = (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](lA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[1])["toString()Ljava/lang/String;"]();
				return lA2;
			default:
				break;
		}
	}
	return null;
};
java_lang_reflect_MethodConstructor.prototype["getModifiers()I"] = function() { 
	return this._modifiers;
};
java_lang_reflect_MethodConstructor.prototype["getName()Ljava/lang/String;"] = function() { 
	return null;
};
java_lang_reflect_MethodConstructor.prototype["isConstructor()Z"] = function() { N.methodWithoutBody('java.lang.reflect.MethodConstructor.isConstructor') };
java_lang_reflect_MethodConstructor.prototype["getReturnType()Ljava/lang/Class;"] = function() { 
	return null;
};
java_lang_reflect_MethodConstructor.prototype["methodType()Ljava/lang/reflect/MethodTypeImpl;"] = function() { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this._methodType != null))) {
					_G = 1;
					continue;
				}
				this._methodType = java_lang_reflect__InternalUtils["parseMethodType(Ljava/lang/String;Ljava/lang/reflect/Type;)Ljava/lang/reflect/MethodTypeImpl;"](this._signature, null);
				_G = 1;
				continue;
			case 1:
				return this._methodType;
			default:
				break;
		}
	}
	return null;
};
java_lang_reflect_MethodConstructor.prototype["getParameterTypes()[Ljava/lang/Class;"] = function() { 
	return N.checkCast(N.checkCast(this["methodType()Ljava/lang/reflect/MethodTypeImpl;"]()._args, JA_L), JA_L);
};
java_lang_reflect_MethodConstructor.prototype["getDeclaringClass()Ljava/lang/Class;"] = function() { 
	return this._clazz;
};
function java_lang_reflect_Member() {
}
java_lang_reflect_Member.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_reflect_Member.prototype.constructor = java_lang_reflect_Member;
java_lang_reflect_Member.SI = function(){};
java_lang_reflect_Member.prototype.__JT__CLASS_ID = java_lang_reflect_Member.__JT__CLASS_ID = 675;
java_lang_reflect_Member.prototype.__JT__CLASS_IDS = java_lang_reflect_Member.__JT__CLASS_IDS = [675,656];
java_lang_reflect_Member.prototype["getDeclaringClass()Ljava/lang/Class;"] = function() { N.methodWithoutBody('java.lang.reflect.Member.getDeclaringClass') };
java_lang_reflect_Member.prototype["getName()Ljava/lang/String;"] = function() { N.methodWithoutBody('java.lang.reflect.Member.getName') };
java_lang_reflect_Member.prototype["getModifiers()I"] = function() { N.methodWithoutBody('java.lang.reflect.Member.getModifiers') };
function java_lang_reflect_GenericDeclaration() {
}
java_lang_reflect_GenericDeclaration.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_reflect_GenericDeclaration.prototype.constructor = java_lang_reflect_GenericDeclaration;
java_lang_reflect_GenericDeclaration.SI = function(){};
java_lang_reflect_GenericDeclaration.prototype.__JT__CLASS_ID = java_lang_reflect_GenericDeclaration.__JT__CLASS_ID = 668;
java_lang_reflect_GenericDeclaration.prototype.__JT__CLASS_IDS = java_lang_reflect_GenericDeclaration.__JT__CLASS_IDS = [668,669,656];
function java_lang_reflect_Method() {
}
java_lang_reflect_Method.prototype = Object.create(java_lang_reflect_MethodConstructor.prototype);
java_lang_reflect_Method.prototype.constructor = java_lang_reflect_Method;
java_lang_reflect_Method.prototype._modifiers = 0;
java_lang_reflect_Method.prototype.__name = null;
java_lang_reflect_Method.prototype._methodType = null;
java_lang_reflect_Method.prototype._signature = null;
java_lang_reflect_Method.prototype._clazz = null;
java_lang_reflect_Method.prototype._id = 0;
java_lang_reflect_Method.prototype._exceptionTypes = null;
java_lang_reflect_Method.prototype._genericSignature = null;
java_lang_reflect_Method.prototype._slot = 0;
java_lang_reflect_Method.SI = function(){};
java_lang_reflect_Method.prototype.__JT__CLASS_ID = java_lang_reflect_Method.__JT__CLASS_ID = 714;
java_lang_reflect_Method.prototype.__JT__CLASS_IDS = java_lang_reflect_Method.__JT__CLASS_IDS = [714,715,676,656,675,668,669];
java_lang_reflect_Method.prototype["java.lang.reflect.Method<init>(Ljava/lang/Class;Lj/MemberInfo;)V"] = function(p0, p1) { 
	(this)["java.lang.reflect.MethodConstructor<init>(Ljava/lang/Class;Lj/MemberInfo;)V"](p0, p1);
	return this;
	return this;
};
java_lang_reflect_Method.prototype["getModifiers()I"] = function() { 
	return this._modifiers;
};
java_lang_reflect_Method.prototype["getName()Ljava/lang/String;"] = function() { 
	return this.__name;
};
java_lang_reflect_Method.prototype["isConstructor()Z"] = function() { 
	return false;
};
java_lang_reflect_Method.prototype["getReturnType()Ljava/lang/Class;"] = function() { 
	return N.checkCast(this["methodType()Ljava/lang/reflect/MethodTypeImpl;"]()._rettype, java_lang_Class);
};
java_lang_reflect_Method.prototype["getParameterTypes()[Ljava/lang/Class;"] = function() { 
	return N.checkCast(N.checkCast(this["methodType()Ljava/lang/reflect/MethodTypeImpl;"]()._args, JA_L), JA_L);
};
// ABSTRACT
function java_lang_Number() {
}
java_lang_Number.prototype = Object.create(java_lang_Object.prototype);
java_lang_Number.prototype.constructor = java_lang_Number;
java_lang_Number.prototype.___id = 0;
java_lang_Number.SI = function(){};
java_lang_Number.prototype.__JT__CLASS_ID = java_lang_Number.__JT__CLASS_ID = 673;
java_lang_Number.prototype.__JT__CLASS_IDS = java_lang_Number.__JT__CLASS_IDS = [673,656];
java_lang_Number.prototype["java.lang.Number<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang_Number.prototype["doubleValue()D"] = function() { N.methodWithoutBody('java.lang.Number.doubleValue') };
java_lang_Number.prototype["longValue()J"] = function() { N.methodWithoutBody('java.lang.Number.longValue') };
java_lang_Number.prototype["shortValue()S"] = function() { 
	return ((this["intValue()I"]())<<16>>16);
};
java_lang_Number.prototype["intValue()I"] = function() { N.methodWithoutBody('java.lang.Number.intValue') };
java_lang_Number.prototype["byteValue()B"] = function() { 
	return ((this["intValue()I"]())<<24>>24);
};
function java_lang_Byte() {
}
java_lang_Byte.prototype = Object.create(java_lang_Number.prototype);
java_lang_Byte.prototype.constructor = java_lang_Byte;
java_lang_Byte.prototype._value = 0;
java_lang_Byte.SI = function() { 
	java_lang_Byte._cache = null;
	java_lang_Byte._TYPE = null;
	java_lang_Byte["java.lang.Byte<clinit>()V"]();
};
java_lang_Byte.prototype.__JT__CLASS_ID = java_lang_Byte.__JT__CLASS_ID = 713;
java_lang_Byte.prototype.__JT__CLASS_IDS = java_lang_Byte.__JT__CLASS_IDS = [713,673,656,659];
java_lang_Byte.prototype["java.lang.Byte<init>(B)V"] = function(p0) { 
	(this)["java.lang.Number<init>()V"]();
	this._value = p0;
	return this;
	return this;
};
java_lang_Byte["java.lang.Byte<clinit>()V"] = function() { 
	java_lang_Byte._TYPE = java_lang_Class["getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"](S[390]);
	java_lang_Byte._cache = new JA_L(256, "[Ljava.lang.Byte;");
	return;
};
java_lang_Byte["valueOf(B)Ljava/lang/Byte;"] = function(p0) { 
	var _G = 0, fI1 = 0, lI1 = 0, fA0 = null, fA2 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = (((((p0)|0) + 128))|0);
				if (((((java_lang_Byte._cache).data[lI1]) != null))) {
					_G = 1;
					continue;
				}
				fA0 = (java_lang_Byte._cache);
				fI1 = lI1;
				tA0 = ((new java_lang_Byte()));
				fA2 = tA0;
				(tA0)["java.lang.Byte<init>(B)V"](p0);
				(fA0).data[fI1] = fA2;
				_G = 1;
				continue;
			case 1:
				return (((java_lang_Byte._cache).data[lI1]));
			default:
				break;
		}
	}
	return null;
};
java_lang_Byte.prototype["toString()Ljava/lang/String;"] = function() { 
	return java_lang_Integer["toString(I)Ljava/lang/String;"](((this._value)|0));
};
java_lang_Byte.prototype["doubleValue()D"] = function() { 
	return +(this._value);
};
java_lang_Byte.prototype["hashCode()I"] = function() { 
	return java_lang_Byte["hashCode(B)I"](this._value);
};
java_lang_Byte["hashCode(B)I"] = function(p0) { 
	return ((p0)|0);
};
java_lang_Byte.prototype["longValue()J"] = function() { 
	return N.i2j(this._value);
};
java_lang_Byte.prototype["intValue()I"] = function() { 
	return ((this._value)|0);
};
java_lang_Byte.prototype["shortValue()S"] = function() { 
	return ((this._value)<<16>>16);
};
java_lang_Byte.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!((p0 instanceof java_lang_Byte))) {
					_G = 1;
					continue;
				}
				if (((this._value != N.checkCast(p0, java_lang_Byte)["byteValue()B"]()))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_Byte.prototype["byteValue()B"] = function() { 
	return this._value;
};
function java_lang_Boolean() {
}
java_lang_Boolean.prototype = Object.create(java_lang_Object.prototype);
java_lang_Boolean.prototype.constructor = java_lang_Boolean;
java_lang_Boolean.prototype._value = false;
java_lang_Boolean.prototype.___id = 0;
java_lang_Boolean.SI = function() { 
	java_lang_Boolean._TYPE = null;
	java_lang_Boolean._TRUE = null;
	java_lang_Boolean._FALSE = null;
	java_lang_Boolean["java.lang.Boolean<clinit>()V"]();
};
java_lang_Boolean.prototype.__JT__CLASS_ID = java_lang_Boolean.__JT__CLASS_ID = 712;
java_lang_Boolean.prototype.__JT__CLASS_IDS = java_lang_Boolean.__JT__CLASS_IDS = [712,656,658,659];
java_lang_Boolean["java.lang.Boolean<clinit>()V"] = function() { 
	var fA0 = null, tA0 = null, tA1 = null;
	tA0 = ((new java_lang_Boolean()));
	fA0 = tA0;
	(tA0)["java.lang.Boolean<init>(Z)V"](true);
	java_lang_Boolean._TRUE = (fA0);
	tA1 = ((new java_lang_Boolean()));
	fA0 = tA1;
	(tA1)["java.lang.Boolean<init>(Z)V"](false);
	java_lang_Boolean._FALSE = (fA0);
	java_lang_Boolean._TYPE = java_lang_Class["getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"](S[391]);
	return;
};
java_lang_Boolean.prototype["java.lang.Boolean<init>(Z)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	this._value = p0;
	return this;
	return this;
};
java_lang_Boolean.prototype["toString()Ljava/lang/String;"] = function() { 
	return java_lang_Boolean["toString(Z)Ljava/lang/String;"](this._value);
};
java_lang_Boolean["toString(Z)Ljava/lang/String;"] = function(p0) { 
	var _G = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p0)) {
					_G = 1;
					continue;
				}
				fA0 = S[392];
				_G = 2;
				continue;
			case 1:
				fA0 = S[393];
				_G = 2;
				continue;
			case 2:return fA0; 
			default:
				break;
		}
	}
	return null;
};
java_lang_Boolean.prototype["hashCode()I"] = function() { 
	return java_lang_Boolean["hashCode(Z)I"](this._value);
};
java_lang_Boolean["hashCode(Z)I"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p0)) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return fI0; 
			default:
				break;
		}
	}
	return 0;
};
java_lang_Boolean["valueOf(Z)Ljava/lang/Boolean;"] = function(p0) { 
	var _G = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p0)) {
					_G = 1;
					continue;
				}
				fA0 = java_lang_Boolean._TRUE;
				_G = 2;
				continue;
			case 1:
				fA0 = java_lang_Boolean._FALSE;
				_G = 2;
				continue;
			case 2:return fA0; 
			default:
				break;
		}
	}
	return null;
};
java_lang_Boolean.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if ((((this) != p0))) {
					_G = 1;
					continue;
				}
				return true;
			case 1:
				if (((p0 != null))) {
					_G = 2;
					continue;
				}
				return false;
			case 2:
				if ((((this)["getClass()Ljava/lang/Class;"]() == p0["getClass()Ljava/lang/Class;"]()))) {
					_G = 3;
					continue;
				}
				return false;
			case 3:
				if (((this._value != N.checkCast(p0, java_lang_Boolean)._value))) {
					_G = 4;
					continue;
				}
				fI0 = 1;
				_G = 5;
				continue;
			case 4:
				fI0 = 0;
				_G = 5;
				continue;
			case 5:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
function com_jtransc_io_JTranscConsole() {
}
com_jtransc_io_JTranscConsole.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_io_JTranscConsole.prototype.constructor = com_jtransc_io_JTranscConsole;
com_jtransc_io_JTranscConsole.prototype.___id = 0;
com_jtransc_io_JTranscConsole.SI = function(){};
com_jtransc_io_JTranscConsole.prototype.__JT__CLASS_ID = com_jtransc_io_JTranscConsole.__JT__CLASS_ID = 711;
com_jtransc_io_JTranscConsole.prototype.__JT__CLASS_IDS = com_jtransc_io_JTranscConsole.__JT__CLASS_IDS = [711,656];
com_jtransc_io_JTranscConsole.prototype["com.jtransc.io.JTranscConsole<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_io_JTranscConsole["error(Ljava/lang/Object;)V"] = function(p0) { 
	console.error('' + p0);
};
com_jtransc_io_JTranscConsole["logOrError(Ljava/lang/Object;Z)V"] = function(p0, p1) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p1)) {
					_G = 1;
					continue;
				}
				com_jtransc_io_JTranscConsole["error(Ljava/lang/Object;)V"](p0);
				_G = 2;
				continue;
			case 1:
				com_jtransc_io_JTranscConsole["log(Ljava/lang/Object;)V"](p0);
				_G = 2;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
com_jtransc_io_JTranscConsole["log(Ljava/lang/Object;)V"] = function(p0) { 
	console.log('' + p0);
};
function java_lang_Short() {
}
java_lang_Short.prototype = Object.create(java_lang_Number.prototype);
java_lang_Short.prototype.constructor = java_lang_Short;
java_lang_Short.prototype._value = 0;
java_lang_Short.SI = function() { 
	java_lang_Short._TYPE = null;
	java_lang_Short["java.lang.Short<clinit>()V"]();
};
java_lang_Short.prototype.__JT__CLASS_ID = java_lang_Short.__JT__CLASS_ID = 710;
java_lang_Short.prototype.__JT__CLASS_IDS = java_lang_Short.__JT__CLASS_IDS = [710,673,656,659];
java_lang_Short.prototype["java.lang.Short<init>(S)V"] = function(p0) { 
	(this)["java.lang.Number<init>()V"]();
	this._value = p0;
	return this;
	return this;
};
java_lang_Short["java.lang.Short<clinit>()V"] = function() { 
	java_lang_Short._TYPE = java_lang_Class["getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"](S[394]);
	return;
};
java_lang_Short["valueOf(S)Ljava/lang/Short;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_Short()));
	fA0 = tA0;
	(tA0)["java.lang.Short<init>(S)V"](p0);
	return (fA0);
};
java_lang_Short.prototype["toString()Ljava/lang/String;"] = function() { 
	return java_lang_Integer["toString(I)Ljava/lang/String;"](((this._value)|0));
};
java_lang_Short.prototype["doubleValue()D"] = function() { 
	return +(this._value);
};
java_lang_Short.prototype["hashCode()I"] = function() { 
	return java_lang_Short["hashCode(S)I"](this._value);
};
java_lang_Short["hashCode(S)I"] = function(p0) { 
	return ((p0)|0);
};
java_lang_Short.prototype["longValue()J"] = function() { 
	return N.i2j(this._value);
};
java_lang_Short.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!((p0 instanceof java_lang_Short))) {
					_G = 1;
					continue;
				}
				if (((this._value != N.checkCast(p0, java_lang_Short)["shortValue()S"]()))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_Short.prototype["shortValue()S"] = function() { 
	return this._value;
};
java_lang_Short.prototype["intValue()I"] = function() { 
	return ((this._value)|0);
};
java_lang_Short.prototype["byteValue()B"] = function() { 
	return ((this._value)<<24>>24);
};
java_lang_Short["reverseBytes(S)S"] = function(p0) { 
	return (((((((((((((p0)|0) & 65280))|0) >> 8))|0) | ((((((((p0)|0) & 255))|0) << 8))|0)))|0))<<16>>16);
};
function com_jtransc_internal_JTranscCType() {
}
com_jtransc_internal_JTranscCType.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_internal_JTranscCType.prototype.constructor = com_jtransc_internal_JTranscCType;
com_jtransc_internal_JTranscCType.prototype.___id = 0;
com_jtransc_internal_JTranscCType.SI = function(){};
com_jtransc_internal_JTranscCType.prototype.__JT__CLASS_ID = com_jtransc_internal_JTranscCType.__JT__CLASS_ID = 709;
com_jtransc_internal_JTranscCType.prototype.__JT__CLASS_IDS = com_jtransc_internal_JTranscCType.__JT__CLASS_IDS = [709,656];
com_jtransc_internal_JTranscCType.prototype["com.jtransc.internal.JTranscCType<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_internal_JTranscCType["encodeDigit(I)C"] = function(p0) { 
	return java_lang_Character["forDigit(II)C"](p0, 36);
};
com_jtransc_internal_JTranscCType["isDigit(C)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((((p0)|0) < 48))) {
					_G = 1;
					continue;
				}
				if (((((p0)|0) <= 57))) {
					_G = 2;
					continue;
				}
				_G = 1;
				continue;
			case 1:
				if (((((p0)|0) < 97))) {
					_G = 3;
					continue;
				}
				if (((((p0)|0) <= 122))) {
					_G = 2;
					continue;
				}
				_G = 3;
				continue;
			case 3:
				if (((((p0)|0) < 65))) {
					_G = 4;
					continue;
				}
				if (((((p0)|0) > 90))) {
					_G = 4;
					continue;
				}
				_G = 2;
				continue;
			case 2:
				fI0 = 1;
				_G = 5;
				continue;
			case 4:
				fI0 = 0;
				_G = 5;
				continue;
			case 5:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
function java_lang_Long() {
}
java_lang_Long.prototype = Object.create(java_lang_Number.prototype);
java_lang_Long.prototype.constructor = java_lang_Long;
java_lang_Long.prototype._value = N.lnew(0, 0);
java_lang_Long.SI = function() { 
	java_lang_Long._TYPE = null;
	java_lang_Long["java.lang.Long<clinit>()V"]();
};
java_lang_Long.prototype.__JT__CLASS_ID = java_lang_Long.__JT__CLASS_ID = 708;
java_lang_Long.prototype.__JT__CLASS_IDS = java_lang_Long.__JT__CLASS_IDS = [708,673,656,659];
java_lang_Long.prototype["java.lang.Long<init>(J)V"] = function(p0) { 
	(this)["java.lang.Number<init>()V"]();
	this._value = p0;
	return this;
	return this;
};
java_lang_Long["java.lang.Long<clinit>()V"] = function() { 
	java_lang_Long._TYPE = java_lang_Class["getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"](S[395]);
	return;
};
java_lang_Long["valueOf(J)Ljava/lang/Long;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_Long()));
	fA0 = tA0;
	(tA0)["java.lang.Long<init>(J)V"](p0);
	return (fA0);
};
java_lang_Long.prototype["toString()Ljava/lang/String;"] = function() { 
	return java_lang_Long["toString(J)Ljava/lang/String;"](this._value);
};
java_lang_Long["toString(J)Ljava/lang/String;"] = function(p0) { 
	return java_lang_Long["toString(JI)Ljava/lang/String;"](p0, 10);
};
java_lang_Long["toString(JI)Ljava/lang/String;"] = function(p0, p1) { 
	var _G = 0, lA3 = null, fI0 = 0, lI4 = 0, fA0 = null, lJ0 = N.lnew(0, 0), tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				lJ0 = p0;
				if (((((N.lcmp(lJ0, N.lnew(0, 0)))|0) != 0))) {
					_G = 1;
					continue;
				}
				return S[396];
			case 1:
				if (((((N.lcmp(lJ0, N.lnew(-2147483648, 0)))|0) != 0))) {
					_G = 2;
					continue;
				}
				return S[397];
			case 2:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				lA3 = fA0;
				if (((((N.lcmp(lJ0, N.lnew(0, 0)))|0) >= 0))) {
					_G = 3;
					continue;
				}
				fI0 = 1;
				_G = 4;
				continue;
			case 3:
				fI0 = 0;
				_G = 4;
				continue;
			case 4:
				lI4 = fI0;
				if (((lI4 == 0))) {
					_G = 5;
					continue;
				}
				lJ0 = N.lneg(lJ0);
				_G = 5;
				continue;
			case 5:
				if (((((N.lcmp(lJ0, N.lnew(0, 0)))|0) == 0))) {
					_G = 6;
					continue;
				}
				(lA3)["append(C)Ljava/lang/StringBuilder;"](com_jtransc_internal_JTranscCType["encodeDigit(I)C"](N.j2i(N.lrem(N.ladd(N.lrem(lJ0, N.i2j(p1)), N.i2j(p1)), N.i2j(p1)))));
				lJ0 = N.ldiv(lJ0, N.i2j(p1));
				_G = 5;
				continue;
			case 6:
				if (((lI4 == 0))) {
					_G = 7;
					continue;
				}
				(lA3)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[398]);
				_G = 7;
				continue;
			case 7:
				(lA3)["reverse()Ljava/lang/StringBuilder;"]();
				return (lA3)["toString()Ljava/lang/String;"]();
			default:
				break;
		}
	}
	return null;
};
java_lang_Long.prototype["doubleValue()D"] = function() { 
	return N.j2d(this._value);
};
java_lang_Long.prototype["hashCode()I"] = function() { 
	return java_lang_Long["hashCode(J)I"](this._value);
};
java_lang_Long["hashCode(J)I"] = function(p0) { 
	return N.j2i(N.lxor(p0, N.lushr(p0, 32)));
};
java_lang_Long.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if ((p0 instanceof java_lang_Long)) {
					_G = 1;
					continue;
				}
				return false;
			case 1:
				if (((((N.lcmp(this._value, N.checkCast(p0, java_lang_Long)["longValue()J"]()))|0) != 0))) {
					_G = 2;
					continue;
				}
				fI0 = 1;
				_G = 3;
				continue;
			case 2:
				fI0 = 0;
				_G = 3;
				continue;
			case 3:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_Long.prototype["longValue()J"] = function() { 
	return this._value;
};
java_lang_Long.prototype["intValue()I"] = function() { 
	return N.j2i(this._value);
};
java_lang_Long.prototype["shortValue()S"] = function() { 
	return ((N.j2i(this._value))<<16>>16);
};
java_lang_Long.prototype["byteValue()B"] = function() { 
	return ((N.j2i(this._value))<<24>>24);
};
java_lang_Long["reverseBytes(J)J"] = function(p0) { 
	var lJ0 = N.lnew(0, 0);
	lJ0 = p0;
	lJ0 = N.lor(N.land(N.lushr(lJ0, 8), N.lnew(16711935, 16711935)), N.lshl(N.land(lJ0, N.lnew(16711935, 16711935)), 8));
	lJ0 = N.lor(N.land(N.lushr(lJ0, 16), N.lnew(65535, 65535)), N.lshl(N.land(lJ0, N.lnew(65535, 65535)), 16));
	return N.lor(N.lushr(lJ0, 32), N.lshl(lJ0, 32));
};
function com_jtransc_ds_FastStringMap() {
}
com_jtransc_ds_FastStringMap.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_ds_FastStringMap.prototype.constructor = com_jtransc_ds_FastStringMap;
com_jtransc_ds_FastStringMap.prototype.___id = 0;
com_jtransc_ds_FastStringMap.SI = function(){};
com_jtransc_ds_FastStringMap.prototype.__JT__CLASS_ID = com_jtransc_ds_FastStringMap.__JT__CLASS_ID = 707;
com_jtransc_ds_FastStringMap.prototype.__JT__CLASS_IDS = com_jtransc_ds_FastStringMap.__JT__CLASS_IDS = [707,656];
com_jtransc_ds_FastStringMap.prototype["com.jtransc.ds.FastStringMap<init>()V"] = function() { 
	this.data = new Map();
	return this;
};
com_jtransc_ds_FastStringMap.prototype["has(Ljava/lang/String;)Z"] = function(p0) { 
	return this.data.has(N.istr(p0));
};
com_jtransc_ds_FastStringMap.prototype["set(Ljava/lang/String;Ljava/lang/Object;)V"] = function(p0, p1) { 
	this.data.set(N.istr(p0), p1);
};
com_jtransc_ds_FastStringMap.prototype["get(Ljava/lang/String;)Ljava/lang/Object;"] = function(p0) { 
	return this.data.get(N.istr(p0));
};
function java_lang_Double() {
}
java_lang_Double.prototype = Object.create(java_lang_Number.prototype);
java_lang_Double.prototype.constructor = java_lang_Double;
java_lang_Double.prototype._value = 0.0;
java_lang_Double.SI = function() { 
	java_lang_Double._TYPE = null;
	java_lang_Double["java.lang.Double<clinit>()V"]();
};
java_lang_Double.prototype.__JT__CLASS_ID = java_lang_Double.__JT__CLASS_ID = 706;
java_lang_Double.prototype.__JT__CLASS_IDS = java_lang_Double.__JT__CLASS_IDS = [706,673,656,659];
java_lang_Double.prototype["java.lang.Double<init>(D)V"] = function(p0) { 
	(this)["java.lang.Number<init>()V"]();
	this._value = p0;
	return this;
	return this;
};
java_lang_Double["java.lang.Double<clinit>()V"] = function() { 
	java_lang_Double._TYPE = java_lang_Class["getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"](S[399]);
	return;
};
java_lang_Double["valueOf(D)Ljava/lang/Double;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_Double()));
	fA0 = tA0;
	(tA0)["java.lang.Double<init>(D)V"](p0);
	return (fA0);
};
java_lang_Double.prototype["toString()Ljava/lang/String;"] = function() { 
	return java_lang_Double["toString(D)Ljava/lang/String;"](this._value);
};
java_lang_Double["toString(D)Ljava/lang/String;"] = function(p0) { 
	return com_jtransc_text_JTranscStringTools["toString(D)Ljava/lang/String;"](p0);
};
java_lang_Double["isNaN(D)Z"] = function(p0) { 
	return isNaN(p0);
};
java_lang_Double["doubleToRawLongBits(D)J"] = function(p0) { 
	return N.doubleToLongBits(p0);
};
java_lang_Double["isInfinite(D)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (java_lang_Double["isNaN(D)Z"](p0)) {
					_G = 1;
					continue;
				}
				if (java_lang_Double["_isFinite(D)Z"](p0)) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_Double["_isFinite(D)Z"] = function(p0) { 
	return isFinite(p0);
};
java_lang_Double.prototype["hashCode()I"] = function() { 
	return java_lang_Double["hashCode(D)I"](this["doubleValue()D"]());
};
java_lang_Double.prototype["doubleValue()D"] = function() { 
	return this._value;
};
java_lang_Double["hashCode(D)I"] = function(p0) { 
	return N.j2i(java_lang_Double["doubleToLongBits(D)J"](p0));
};
java_lang_Double["doubleToLongBits(D)J"] = function(p0) { 
	return N.doubleToLongBits(p0);
};
java_lang_Double.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!((p0 instanceof java_lang_Double))) {
					_G = 1;
					continue;
				}
				if (((((N.lcmp(java_lang_Double["doubleToLongBits(D)J"](N.checkCast(p0, java_lang_Float)["doubleValue()D"]()), java_lang_Double["doubleToLongBits(D)J"](this["doubleValue()D"]())))|0) != 0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_Double.prototype["longValue()J"] = function() { 
	return N.d2j(this._value);
};
java_lang_Double.prototype["intValue()I"] = function() { 
	return ((((this._value)|0))|0);
};
java_lang_Double.prototype["shortValue()S"] = function() { 
	return ((((this._value)|0))<<16>>16);
};
java_lang_Double.prototype["byteValue()B"] = function() { 
	return ((((this._value)|0))<<24>>24);
};
java_lang_Double["longBitsToDouble(J)D"] = function(p0) { 
	return N.longBitsToDouble(p0);
};
function java_lang_IndexOutOfBoundsException() {
}
java_lang_IndexOutOfBoundsException.prototype = Object.create(java_lang_RuntimeException.prototype);
java_lang_IndexOutOfBoundsException.prototype.constructor = java_lang_IndexOutOfBoundsException;
java_lang_IndexOutOfBoundsException.SI = function(){};
java_lang_IndexOutOfBoundsException.prototype.__JT__CLASS_ID = java_lang_IndexOutOfBoundsException.__JT__CLASS_ID = 704;
java_lang_IndexOutOfBoundsException.prototype.__JT__CLASS_IDS = java_lang_IndexOutOfBoundsException.__JT__CLASS_IDS = [704,696,680,681,656,658];
java_lang_IndexOutOfBoundsException.prototype["java.lang.IndexOutOfBoundsException<init>()V"] = function() { 
	(this)["java.lang.RuntimeException<init>()V"]();
	return this;
	return this;
};
java_lang_IndexOutOfBoundsException.prototype["java.lang.IndexOutOfBoundsException<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.RuntimeException<init>(Ljava/lang/String;)V"](p0);
	return this;
	return this;
};
function java_lang_ArrayIndexOutOfBoundsException() {
}
java_lang_ArrayIndexOutOfBoundsException.prototype = Object.create(java_lang_IndexOutOfBoundsException.prototype);
java_lang_ArrayIndexOutOfBoundsException.prototype.constructor = java_lang_ArrayIndexOutOfBoundsException;
java_lang_ArrayIndexOutOfBoundsException.SI = function(){};
java_lang_ArrayIndexOutOfBoundsException.prototype.__JT__CLASS_ID = java_lang_ArrayIndexOutOfBoundsException.__JT__CLASS_ID = 703;
java_lang_ArrayIndexOutOfBoundsException.prototype.__JT__CLASS_IDS = java_lang_ArrayIndexOutOfBoundsException.__JT__CLASS_IDS = [703,704,696,680,681,656,658];
java_lang_ArrayIndexOutOfBoundsException.prototype["java.lang.ArrayIndexOutOfBoundsException<init>()V"] = function() { 
	(this)["java.lang.IndexOutOfBoundsException<init>()V"]();
	return this;
	return this;
};
java_lang_ArrayIndexOutOfBoundsException.prototype["java.lang.ArrayIndexOutOfBoundsException<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.IndexOutOfBoundsException<init>(Ljava/lang/String;)V"](p0);
	return this;
	return this;
};
// ABSTRACT
function com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream() {
}
com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.prototype = Object.create(java_io_OutputStream.prototype);
com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.prototype.constructor = com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream;
com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.prototype._sb = null;
com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.prototype._error = false;
com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.SI = function(){};
com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.prototype.__JT__CLASS_ID = com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.__JT__CLASS_ID = 701;
com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.prototype.__JT__CLASS_IDS = com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.__JT__CLASS_IDS = [701,693,656,690,694,691];
com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.prototype["com.jtransc.io.JTranscConsolePrintStream$ConsoleBaseStream<init>(Z)V"] = function(p0) { 
	var fA1 = null, tA0 = null;
	(this)["java.io.OutputStream<init>()V"]();
	tA0 = ((new java_lang_StringBuilder()));
	fA1 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	this._sb = (fA1);
	this._error = p0;
	return this;
	return this;
};
com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.prototype["_write(I)V"] = function(p0) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((((((p0)&0xFFFF))|0) != 10))) {
					_G = 1;
					continue;
				}
				com_jtransc_io_JTranscConsole["logOrError(Ljava/lang/Object;Z)V"]((this._sb["toString()Ljava/lang/String;"]()), this._error);
				this._sb["setLength(I)V"](0);
				_G = 2;
				continue;
			case 1:
				this._sb["append(C)Ljava/lang/StringBuilder;"](((p0)&0xFFFF));
				_G = 2;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
function com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream() {
}
com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.prototype = Object.create(com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.prototype);
com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.prototype.constructor = com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream;
com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.prototype._sb = null;
com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.prototype._error = false;
com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.SI = function(){};
com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.prototype.__JT__CLASS_ID = com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.__JT__CLASS_ID = 702;
com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.prototype.__JT__CLASS_IDS = com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.__JT__CLASS_IDS = [702,701,693,656,690,694,691];
com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.prototype["com.jtransc.io.JTranscConsolePrintStream$ConsoleErrorStream<init>()V"] = function() { 
	(this)["com.jtransc.io.JTranscConsolePrintStream$ConsoleBaseStream<init>(Z)V"](true);
	return this;
	return this;
};
com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.prototype["write(I)V"] = function(p0) { 
	this["_write(I)V"](p0);
	return;
};
function com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream() {
}
com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.prototype = Object.create(com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.prototype);
com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.prototype.constructor = com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream;
com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.prototype._sb = null;
com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.prototype._error = false;
com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.SI = function(){};
com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.prototype.__JT__CLASS_ID = com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.__JT__CLASS_ID = 700;
com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.prototype.__JT__CLASS_IDS = com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.__JT__CLASS_IDS = [700,701,693,656,690,694,691];
com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.prototype["com.jtransc.io.JTranscConsolePrintStream$ConsoleOutputStream<init>()V"] = function() { 
	(this)["com.jtransc.io.JTranscConsolePrintStream$ConsoleBaseStream<init>(Z)V"](false);
	return this;
	return this;
};
com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.prototype["write(I)V"] = function(p0) { 
	this["_write(I)V"](p0);
	return;
};
function java_io_FilterOutputStream() {
}
java_io_FilterOutputStream.prototype = Object.create(java_io_OutputStream.prototype);
java_io_FilterOutputStream.prototype.constructor = java_io_FilterOutputStream;
java_io_FilterOutputStream.prototype._out = null;
java_io_FilterOutputStream.SI = function(){};
java_io_FilterOutputStream.prototype.__JT__CLASS_ID = java_io_FilterOutputStream.__JT__CLASS_ID = 692;
java_io_FilterOutputStream.prototype.__JT__CLASS_IDS = java_io_FilterOutputStream.__JT__CLASS_IDS = [692,693,656,690,694,691];
java_io_FilterOutputStream.prototype["java.io.FilterOutputStream<init>(Ljava/io/OutputStream;)V"] = function(p0) { 
	(this)["java.io.OutputStream<init>()V"]();
	this._out = p0;
	return this;
	return this;
};
java_io_FilterOutputStream.prototype["write(I)V"] = function(p0) { 
	this._out["write(I)V"](p0);
	return;
};
java_io_FilterOutputStream.prototype["flush()V"] = function() { 
	this._out["flush()V"]();
	return;
};
java_io_FilterOutputStream.prototype["write([B)V"] = function(p0) { 
	this["write([BII)V"](p0, 0, (p0).length);
	return;
};
java_io_FilterOutputStream.prototype["write([BII)V"] = function(p0, p1, p2) { 
	var _G = 0, lI4 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI4 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI4 >= p2))) {
					_G = 2;
					continue;
				}
				this["write(I)V"]((((p0.data[(((p1 + lI4))|0)]))|0));
				lI4 = (((lI4 + 1))|0);
				_G = 1;
				continue;
			case 2:
				return;
			default:
				break;
		}
	}
	return;
};
function java_io_PrintStream() {
}
java_io_PrintStream.prototype = Object.create(java_io_FilterOutputStream.prototype);
java_io_PrintStream.prototype.constructor = java_io_PrintStream;
java_io_PrintStream.prototype._encoding = null;
java_io_PrintStream.prototype._autoFlush = false;
java_io_PrintStream.prototype._ioError = false;
java_io_PrintStream.prototype._out = null;
java_io_PrintStream.SI = function(){};
java_io_PrintStream.prototype.__JT__CLASS_ID = java_io_PrintStream.__JT__CLASS_ID = 689;
java_io_PrintStream.prototype.__JT__CLASS_IDS = java_io_PrintStream.__JT__CLASS_IDS = [689,692,693,656,665,690,691,694];
java_io_PrintStream.prototype["java.io.PrintStream<init>(Ljava/io/OutputStream;)V"] = function(p0) { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.io.FilterOutputStream<init>(Ljava/io/OutputStream;)V"](p0);
				this._encoding = S[51];
				if ((((p0) != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_NullPointerException()));
				fA0 = tA0;
				(tA0)["java.lang.NullPointerException<init>(Ljava/lang/String;)V"](S[400]);
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
java_io_PrintStream.prototype["println(D)V"] = function(p0) { 
	this["println(Ljava/lang/String;)V"](java_lang_String["valueOf(D)Ljava/lang/String;"](p0));
	return;
};
java_io_PrintStream.prototype["println(Ljava/lang/String;)V"] = function(p0) { 
	this["print(Ljava/lang/String;)V"](p0);
	this["newline()V"]();
	return;
};
java_io_PrintStream.prototype["newline()V"] = function() { 
	this["print(Ljava/lang/String;)V"](java_lang_System["lineSeparator()Ljava/lang/String;"]());
	return;
};
java_io_PrintStream.prototype["print(Ljava/lang/String;)V"] = function(p0) { 
	var _G = 0, fA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						_G = 1;
						continue;
					case 1:
						this["write([B)V"](p0["getBytes(Ljava/lang/String;)[B"](this._encoding));
						_G = 2;
						continue;
					case 2:
						_G = 3;
						continue;
					case 4:
						fA0 = J__exception__;
						this["setError()V"]();
						_G = 3;
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
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_io_IOException)))) {
				_G = 4;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return;
};
java_io_PrintStream.prototype["write(I)V"] = function(p0) { 
	var _G = 0, fI0 = 0, lI2 = 0, lI3 = 0, fA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						if (((this._out != null))) {
							_G = 1;
							continue;
						}
						this["setError()V"]();
						return;
						_G = 1;
						continue;
					case 1:
						this._out["write(I)V"](p0);
						lI2 = (((p0 & 255))|0);
						if (((lI2 == 10))) {
							_G = 2;
							continue;
						}
						if (((lI2 != 21))) {
							_G = 3;
							continue;
						}
						_G = 2;
						continue;
					case 2:
						fI0 = 1;
						_G = 4;
						continue;
					case 3:
						fI0 = 0;
						_G = 4;
						continue;
					case 4:
						lI3 = fI0;
						if (!(this._autoFlush)) {
							_G = 5;
							continue;
						}
						if (((lI3 == 0))) {
							_G = 5;
							continue;
						}
						this["flush()V"]();
						_G = 5;
						continue;
					case 5:
						_G = 6;
						continue;
					case 7:
						fA0 = J__exception__;
						this["setError()V"]();
						_G = 6;
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
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 5)))) && (J__exception__ instanceof java_io_IOException)))) {
				_G = 7;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return;
};
java_io_PrintStream.prototype["flush()V"] = function() { 
	var _G = 0, fA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						if (((this._out == null))) {
							_G = 1;
							continue;
						}
						_G = 2;
						continue;
					case 2:
						this._out["flush()V"]();
						_G = 3;
						continue;
					case 3:
						return;
						_G = 4;
						continue;
					case 4:
						fA0 = J__exception__;
						_G = 1;
						continue;
					case 1:
						this["setError()V"]();
						return;
					default:
						break;
				}
			}
			return;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 2)) && ((_G < 3)))) && (J__exception__ instanceof java_io_IOException)))) {
				_G = 4;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return;
};
java_io_PrintStream.prototype["setError()V"] = function() { 
	this._ioError = true;
	return;
};
java_io_PrintStream.prototype["write([BII)V"] = function(p0, p1, p2) { 
	var _G = 0, lA4 = null, lA6 = null, fA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						com_jtransc_JTranscArrays["checkOffsetAndCount(III)V"]((p0).length, p1, p2);
						fA0 = (this);
						lA4 = (this);
						// MONITOR_ENTER
						_G = 1;
						continue;
					case 1:
						if (((this._out != null))) {
							_G = 2;
							continue;
						}
						this["setError()V"]();
						// MONITOR_EXIT
						_G = 3;
						continue;
					case 3:
						return;
						_G = 2;
						continue;
					case 2:
						this._out["write([BII)V"](p0, p1, p2);
						if (!(this._autoFlush)) {
							_G = 4;
							continue;
						}
						this["flush()V"]();
						_G = 4;
						continue;
					case 4:
						_G = 5;
						continue;
					case 6:
						fA0 = (J__exception__);
						this["setError()V"]();
						_G = 5;
						continue;
					case 5:
						// MONITOR_EXIT
						_G = 7;
						continue;
					case 7:
						_G = 8;
						continue;
					case 9:
						lA6 = (J__exception__);
						// MONITOR_EXIT
						_G = 10;
						continue;
					case 10:
						throw new WrappedError(lA6);
						_G = 8;
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
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 2)) && ((_G < 4)))) && (J__exception__ instanceof java_io_IOException)))) {
				_G = 6;
				continue;
			}
			if (((((((_G >= 1)) && ((_G < 3)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 9;
				continue;
			}
			if (((((((_G >= 2)) && ((_G < 7)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 9;
				continue;
			}
			if (((((((_G >= 9)) && ((_G < 10)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 9;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return;
};
function com_jtransc_io_JTranscConsolePrintStream() {
}
com_jtransc_io_JTranscConsolePrintStream.prototype = Object.create(java_io_PrintStream.prototype);
com_jtransc_io_JTranscConsolePrintStream.prototype.constructor = com_jtransc_io_JTranscConsolePrintStream;
com_jtransc_io_JTranscConsolePrintStream.prototype._error = false;
com_jtransc_io_JTranscConsolePrintStream.prototype._stream = null;
com_jtransc_io_JTranscConsolePrintStream.prototype._encoding = null;
com_jtransc_io_JTranscConsolePrintStream.prototype._autoFlush = false;
com_jtransc_io_JTranscConsolePrintStream.prototype._ioError = false;
com_jtransc_io_JTranscConsolePrintStream.SI = function(){};
com_jtransc_io_JTranscConsolePrintStream.prototype.__JT__CLASS_ID = com_jtransc_io_JTranscConsolePrintStream.__JT__CLASS_ID = 699;
com_jtransc_io_JTranscConsolePrintStream.prototype.__JT__CLASS_IDS = com_jtransc_io_JTranscConsolePrintStream.__JT__CLASS_IDS = [699,689,692,693,656,665,690,691,694];
com_jtransc_io_JTranscConsolePrintStream.prototype["com.jtransc.io.JTranscConsolePrintStream<init>(Z)V"] = function(p0) { 
	var _G = 0, fA1 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(p0)) {
					_G = 1;
					continue;
				}
				tA0 = ((new com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream()));
				fA1 = tA0;
				(tA0)["com.jtransc.io.JTranscConsolePrintStream$ConsoleErrorStream<init>()V"]();
				_G = 2;
				continue;
			case 1:
				tA1 = ((new com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream()));
				fA1 = tA1;
				(tA1)["com.jtransc.io.JTranscConsolePrintStream$ConsoleOutputStream<init>()V"]();
				_G = 2;
				continue;
			case 2:
				this["com.jtransc.io.JTranscConsolePrintStream<init>(Lcom/jtransc/io/JTranscConsolePrintStream$ConsoleBaseStream;Z)V"]((fA1), p0);
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
com_jtransc_io_JTranscConsolePrintStream.prototype["com.jtransc.io.JTranscConsolePrintStream<init>(Lcom/jtransc/io/JTranscConsolePrintStream$ConsoleBaseStream;Z)V"] = function(p0, p1) { 
	(this)["java.io.PrintStream<init>(Ljava/io/OutputStream;)V"]((p0));
	this._stream = p0;
	this._error = p1;
	return this;
	return this;
};
com_jtransc_io_JTranscConsolePrintStream.prototype["println(Ljava/lang/String;)V"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_StringBuilder()));
	fA0 = tA0;
	(tA0)["java.lang.StringBuilder<init>()V"]();
	com_jtransc_io_JTranscConsole["logOrError(Ljava/lang/Object;Z)V"](((fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this._stream._sb["toString()Ljava/lang/String;"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](p0)["toString()Ljava/lang/String;"]()), this._error);
	this._stream._sb["setLength(I)V"](0);
	return;
};
// ABSTRACT
function java_io_InputStream() {
}
java_io_InputStream.prototype = Object.create(java_lang_Object.prototype);
java_io_InputStream.prototype.constructor = java_io_InputStream;
java_io_InputStream.prototype.___id = 0;
java_io_InputStream.SI = function(){};
java_io_InputStream.prototype.__JT__CLASS_ID = java_io_InputStream.__JT__CLASS_ID = 698;
java_io_InputStream.prototype.__JT__CLASS_IDS = java_io_InputStream.__JT__CLASS_IDS = [698,656,690,691];
java_io_InputStream.prototype["java.io.InputStream<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
function java_lang_System$1() {
}
java_lang_System$1.prototype = Object.create(java_io_InputStream.prototype);
java_lang_System$1.prototype.constructor = java_lang_System$1;
java_lang_System$1.SI = function(){};
java_lang_System$1.prototype.__JT__CLASS_ID = java_lang_System$1.__JT__CLASS_ID = 697;
java_lang_System$1.prototype.__JT__CLASS_IDS = java_lang_System$1.__JT__CLASS_IDS = [697,698,656,690,691];
java_lang_System$1.prototype["java.lang.System$1<init>()V"] = function() { 
	(this)["java.io.InputStream<init>()V"]();
	return this;
	return this;
};
function java_lang_NullPointerException() {
}
java_lang_NullPointerException.prototype = Object.create(java_lang_RuntimeException.prototype);
java_lang_NullPointerException.prototype.constructor = java_lang_NullPointerException;
java_lang_NullPointerException.SI = function(){};
java_lang_NullPointerException.prototype.__JT__CLASS_ID = java_lang_NullPointerException.__JT__CLASS_ID = 695;
java_lang_NullPointerException.prototype.__JT__CLASS_IDS = java_lang_NullPointerException.__JT__CLASS_IDS = [695,696,680,681,656,658];
java_lang_NullPointerException.prototype["java.lang.NullPointerException<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.RuntimeException<init>(Ljava/lang/String;)V"](p0);
	return this;
	return this;
};
java_lang_NullPointerException.prototype["java.lang.NullPointerException<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"] = function(p0, p1) { 
	(this)["java.lang.RuntimeException<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"](p0, p1);
	return this;
	return this;
};
java_lang_NullPointerException.prototype["java.lang.NullPointerException<init>(Ljava/lang/Throwable;)V"] = function(p0) { 
	(this)["java.lang.RuntimeException<init>(Ljava/lang/Throwable;)V"](p0);
	return this;
	return this;
};
java_lang_NullPointerException.prototype["java.lang.NullPointerException<init>()V"] = function() { 
	(this)["java.lang.RuntimeException<init>()V"]();
	return this;
	return this;
};
function java_lang_System() {
}
java_lang_System.prototype = Object.create(java_lang_Object.prototype);
java_lang_System.prototype.constructor = java_lang_System;
java_lang_System.prototype.___id = 0;
java_lang_System.SI = function() { 
	java_lang_System._in = null;
	java_lang_System._out = null;
	java_lang_System._err = null;
	java_lang_System.__props = null;
	java_lang_System["java.lang.System<clinit>()V"]();
};
java_lang_System.prototype.__JT__CLASS_ID = java_lang_System.__JT__CLASS_ID = 688;
java_lang_System.prototype.__JT__CLASS_IDS = java_lang_System.__JT__CLASS_IDS = [688,656];
java_lang_System.prototype["java.lang.System<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang_System["java.lang.System<clinit>()V"] = function() { 
	var fA0 = null, tA0 = null, tA1 = null, tA2 = null;
	tA0 = ((new java_lang_System$1()));
	fA0 = tA0;
	(tA0)["java.lang.System$1<init>()V"]();
	java_lang_System._in = (fA0);
	tA1 = ((new com_jtransc_io_JTranscConsolePrintStream()));
	fA0 = tA1;
	(tA1)["com.jtransc.io.JTranscConsolePrintStream<init>(Z)V"](false);
	java_lang_System._out = (fA0);
	tA2 = ((new com_jtransc_io_JTranscConsolePrintStream()));
	fA0 = tA2;
	(tA2)["com.jtransc.io.JTranscConsolePrintStream<init>(Z)V"](true);
	java_lang_System._err = (fA0);
	return;
};
java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"] = function(p0, p1, p2, p3, p4) { 
	N.arraycopy(p0, p1, p2, p3, p4);
};
java_lang_System["identityHashCode(Ljava/lang/Object;)I"] = function(p0) { 
	return java_lang_SystemInt["identityHashCode(Ljava/lang/Object;)I"](p0);
};
java_lang_System["gc()V"] = function() { 
	java_lang_Runtime["getRuntime()Ljava/lang/Runtime;"]()["gc()V"]();
	return;
};
java_lang_System["lineSeparator()Ljava/lang/String;"] = function() { 
	return com_jtransc_JTranscSystem["lineSeparator()Ljava/lang/String;"]();
};
java_lang_System["currentTimeMillis()J"] = function() { 
	return N.d2j(com_jtransc_JTranscSystem["fastTime()D"]());
};
java_lang_System["getProperty(Ljava/lang/String;)Ljava/lang/String;"] = function(p0) { 
	return java_lang_System["getProps()Ljava/util/Properties;"]()["getProperty(Ljava/lang/String;)Ljava/lang/String;"](p0);
};
java_lang_System["getProps()Ljava/util/Properties;"] = function() { 
	var _G = 0, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((java_lang_System.__props != null))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_util_Properties()));
				fA0 = tA0;
				(tA0)["java.util.Properties<init>()V"]();
				java_lang_System.__props = (fA0);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V"](S[401], com_jtransc_JTranscSystem["getArch()Ljava/lang/String;"](), S[402]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V"](S[403], com_jtransc_JTranscSystem["getOS()Ljava/lang/String;"](), S[402]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[404], S[405]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V"](S[406], com_jtransc_JTranscSystem["getRuntimeName()Ljava/lang/String;"](), S[407]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[408], S[409]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[410], S[411]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[412], S[413]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V"](S[414], com_jtransc_JTranscSystem["fileSeparator()Ljava/lang/String;"](), S[49]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V"](S[415], com_jtransc_JTranscSystem["lineSeparator()Ljava/lang/String;"](), S[42]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V"](S[416], com_jtransc_JTranscSystem["pathSeparator()Ljava/lang/String;"](), S[52]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[417], com_jtransc_JTranscSystemProperties["fileEncoding()Ljava/lang/String;"]());
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V"](S[418], com_jtransc_JTranscSystem["getJavaHome()Ljava/lang/String;"](), S[49]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V"](S[419], com_jtransc_JTranscSystem["getRuntimeName()Ljava/lang/String;"](), S[407]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[420], S[421]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[422], S[423]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[424], S[421]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[425], S[426]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[427], S[428]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[429], S[430]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[431], S[421]);
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[432], com_jtransc_JTranscVersion["getVersion()Ljava/lang/String;"]());
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[433], com_jtransc_JTranscSystemProperties["tmpdir()Ljava/lang/String;"]());
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[434], com_jtransc_JTranscSystemProperties["userHome()Ljava/lang/String;"]());
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[435], com_jtransc_JTranscSystemProperties["userDir()Ljava/lang/String;"]());
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[436], com_jtransc_JTranscSystemProperties["userName()Ljava/lang/String;"]());
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[437], com_jtransc_JTranscSystemProperties["userLanguage()Ljava/lang/String;"]());
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[438], com_jtransc_JTranscSystemProperties["userRegion()Ljava/lang/String;"]());
				java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"](S[439], com_jtransc_JTranscSystemProperties["userVariant()Ljava/lang/String;"]());
				_G = 1;
				continue;
			case 1:
				return java_lang_System.__props;
			default:
				break;
		}
	}
	return null;
};
java_lang_System["getenv(Ljava/lang/String;)Ljava/lang/String;"] = function(p0) { 
	return N.str((typeof process != 'undefined') ? process.env[p0] : null);
};
java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;)V"] = function(p0, p1) { 
	java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V"](p0, p1, S[20]);
	return;
};
java_lang_System["_setProperty(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V"] = function(p0, p1, p2) { 
	var _G = 0, lA0 = null, lA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA0 = (p0);
				lA1 = (p1);
				if (((lA0 != null))) {
					_G = 1;
					continue;
				}
				lA0 = (S[20]);
				_G = 1;
				continue;
			case 1:
				if (((lA1 != null))) {
					_G = 2;
					continue;
				}
				lA1 = (p2);
				_G = 2;
				continue;
			case 2:
				java_lang_System["getProps()Ljava/util/Properties;"]()["put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"](lA0, lA1);
				return;
			default:
				break;
		}
	}
	return;
};
function java_util_Arrays() {
}
java_util_Arrays.prototype = Object.create(java_lang_Object.prototype);
java_util_Arrays.prototype.constructor = java_util_Arrays;
java_util_Arrays.prototype.___id = 0;
java_util_Arrays.SI = function(){};
java_util_Arrays.prototype.__JT__CLASS_ID = java_util_Arrays.__JT__CLASS_ID = 687;
java_util_Arrays.prototype.__JT__CLASS_IDS = java_util_Arrays.__JT__CLASS_IDS = [687,656];
java_util_Arrays.prototype["java.util.Arrays<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_util_Arrays["copyOf([CI)[C"] = function(p0, p1) { 
	return java_util_Arrays["copyOfRange([CII)[C"](p0, 0, p1);
};
java_util_Arrays["copyOfRange([CII)[C"] = function(p0, p1, p2) { 
	var lA6 = null, lI3 = 0, lI4 = 0, lI5 = 0;
	lI3 = java_util_Arrays["checkRange(III)I"](p1, p2, (p0).length);
	lI4 = (((p2 - p1))|0);
	lI5 = java_lang_Math["min(II)I"](lI4, (((lI3 - p1))|0));
	lA6 = (new JA_C(lI4));
	java_lang_System["arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"]((p0), p1, lA6, 0, lI5);
	return (lA6);
};
java_util_Arrays["checkRange(III)I"] = function(p0, p1, p2) { 
	var _G = 0, fA0 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 <= p1))) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_IllegalArgumentException()));
				fA0 = tA0;
				(tA0)["java.lang.IllegalArgumentException<init>()V"]();
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				if (((p0 < 0))) {
					_G = 2;
					continue;
				}
				if (((p0 <= p2))) {
					_G = 3;
					continue;
				}
				_G = 2;
				continue;
			case 2:
				tA1 = ((new java_lang_ArrayIndexOutOfBoundsException()));
				fA0 = tA1;
				(tA1)["java.lang.ArrayIndexOutOfBoundsException<init>()V"]();
				throw new WrappedError(fA0);
				_G = 3;
				continue;
			case 3:
				return p2;
			default:
				break;
		}
	}
	return 0;
};
java_util_Arrays["copyOf([Ljava/lang/Object;ILjava/lang/Class;)[Ljava/lang/Object;"] = function(p0, p1, p2) { 
	return java_util_Arrays["copyOfRange([Ljava/lang/Object;IILjava/lang/Class;)[Ljava/lang/Object;"](p0, 0, p1, p2);
};
java_util_Arrays["copyOfRange([Ljava/lang/Object;IILjava/lang/Class;)[Ljava/lang/Object;"] = function(p0, p1, p2, p3) { 
	return JA_L.copyOfRange(p0, p1, p2, p3.name);
};
java_util_Arrays["asList([Ljava/lang/Object;)Ljava/util/List;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_util_Arrays$ArrayList()));
	fA0 = tA0;
	(tA0)["java.util.Arrays$ArrayList<init>([Ljava/lang/Object;)V"](p0);
	return (fA0);
};
java_util_Arrays["access$000([Ljava/lang/Object;I)[Ljava/lang/Object;"] = function(p0, p1) { 
	return java_util_Arrays["newInstance([Ljava/lang/Object;I)[Ljava/lang/Object;"](p0, p1);
};
java_util_Arrays["newInstance([Ljava/lang/Object;I)[Ljava/lang/Object;"] = function(p0, p1) { 
	return N.checkCast(N.checkCast(java_lang_reflect_Array["newInstance(Ljava/lang/Class;I)Ljava/lang/Object;"]((p0)["getClass()Ljava/lang/Class;"]()["getComponentType()Ljava/lang/Class;"](), p1), JA_L), JA_L);
};
function java_lang_Character() {
}
java_lang_Character.prototype = Object.create(java_lang_Object.prototype);
java_lang_Character.prototype.constructor = java_lang_Character;
java_lang_Character.prototype._value = 0;
java_lang_Character.prototype.___id = 0;
java_lang_Character.SI = function() { 
	java_lang_Character._TYPE = null;
	java_lang_Character["java.lang.Character<clinit>()V"]();
};
java_lang_Character.prototype.__JT__CLASS_ID = java_lang_Character.__JT__CLASS_ID = 686;
java_lang_Character.prototype.__JT__CLASS_IDS = java_lang_Character.__JT__CLASS_IDS = [686,656,658,659];
java_lang_Character.prototype["java.lang.Character<init>(C)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	this._value = p0;
	return this;
	return this;
};
java_lang_Character["java.lang.Character<clinit>()V"] = function() { 
	java_lang_Character._TYPE = java_lang_Class["getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"](S[440]);
	return;
};
java_lang_Character["valueOf(C)Ljava/lang/Character;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_Character()));
	fA0 = tA0;
	(tA0)["java.lang.Character<init>(C)V"](p0);
	return (fA0);
};
java_lang_Character.prototype["toString()Ljava/lang/String;"] = function() { 
	return java_lang_Character["toString(C)Ljava/lang/String;"](this._value);
};
java_lang_Character["toString(C)Ljava/lang/String;"] = function(p0) { 
	var fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null;
	tA0 = ((new java_lang_String()));
	fA0 = tA0;
	fA1 = tA0;
	tA1 = (new JA_C(1));
	fA2 = tA1;
	(tA1).data[0] = p0;
	(fA1)["java.lang.String<init>([C)V"]((fA2));
	return (fA0);
};
java_lang_Character["isDigit(C)Z"] = function(p0) { 
	return p0 >= 48 && p0 <= 57;
};
java_lang_Character["forDigit(II)C"] = function(p0, p1) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 < 0))) {
					_G = 1;
					continue;
				}
				if (((p0 > 9))) {
					_G = 1;
					continue;
				}
				return (((((48 + (((p0 - 0))|0)))|0))&0xFFFF);
			case 1:
				if (((p0 < 10))) {
					_G = 2;
					continue;
				}
				if (((p0 > 35))) {
					_G = 2;
					continue;
				}
				return (((((97 + (((p0 - 10))|0)))|0))&0xFFFF);
			case 2:
				return 0;
			default:
				break;
		}
	}
	return 0;
};
java_lang_Character.prototype["hashCode()I"] = function() { 
	return ((this._value)|0);
};
java_lang_Character.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!((p0 instanceof java_lang_Character))) {
					_G = 1;
					continue;
				}
				if (((this._value != N.checkCast(p0, java_lang_Character)._value))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_Character["reverseBytes(C)C"] = function(p0) { 
	return (((((((((((((p0)|0) & 65280))|0) >> 8))|0) | (((((p0)|0) << 8))|0)))|0))&0xFFFF);
};
function java_lang_Math() {
}
java_lang_Math.prototype = Object.create(java_lang_Object.prototype);
java_lang_Math.prototype.constructor = java_lang_Math;
java_lang_Math.prototype.___id = 0;
java_lang_Math.SI = function(){};
java_lang_Math.prototype.__JT__CLASS_ID = java_lang_Math.__JT__CLASS_ID = 685;
java_lang_Math.prototype.__JT__CLASS_IDS = java_lang_Math.__JT__CLASS_IDS = [685,656];
java_lang_Math.prototype["java.lang.Math<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang_Math["min(II)I"] = function(p0, p1) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 >= p1))) {
					_G = 1;
					continue;
				}
				fI0 = p0;
				_G = 2;
				continue;
			case 1:
				fI0 = p1;
				_G = 2;
				continue;
			case 2:return fI0; 
			default:
				break;
		}
	}
	return 0;
};
java_lang_Math["max(II)I"] = function(p0, p1) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 <= p1))) {
					_G = 1;
					continue;
				}
				fI0 = p0;
				_G = 2;
				continue;
			case 1:
				fI0 = p1;
				_G = 2;
				continue;
			case 2:return fI0; 
			default:
				break;
		}
	}
	return 0;
};
function com_jtransc_text_JTranscStringTools() {
}
com_jtransc_text_JTranscStringTools.prototype = Object.create(java_lang_Object.prototype);
com_jtransc_text_JTranscStringTools.prototype.constructor = com_jtransc_text_JTranscStringTools;
com_jtransc_text_JTranscStringTools.prototype.___id = 0;
com_jtransc_text_JTranscStringTools.SI = function(){};
com_jtransc_text_JTranscStringTools.prototype.__JT__CLASS_ID = com_jtransc_text_JTranscStringTools.__JT__CLASS_ID = 684;
com_jtransc_text_JTranscStringTools.prototype.__JT__CLASS_IDS = com_jtransc_text_JTranscStringTools.__JT__CLASS_IDS = [684,656];
com_jtransc_text_JTranscStringTools.prototype["com.jtransc.text.JTranscStringTools<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
com_jtransc_text_JTranscStringTools["toString(F)Ljava/lang/String;"] = function(p0) { 
	var _G = 0, lI2 = 0, lA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = com_jtransc_text_JTranscStringTools["toString(D)Ljava/lang/String;"]((p0));
				lI2 = lA1["indexOf(I)I"](46);
				if (((lI2 < 0))) {
					_G = 1;
					continue;
				}
				return lA1["substring(II)Ljava/lang/String;"](0, java_lang_Math["min(II)I"](lA1["length()I"](), (((lI2 + 6))|0)));
			case 1:
				return lA1;
			default:
				break;
		}
	}
	return null;
};
com_jtransc_text_JTranscStringTools["toString(D)Ljava/lang/String;"] = function(p0) { 
	var _G = 0, lA2 = null, lI3 = 0, lI4 = 0, lI5 = 0, fA0 = null, lJ2 = N.lnew(0, 0), tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (!(java_lang_Double["isNaN(D)Z"](p0))) {
					_G = 1;
					continue;
				}
				return S[441];
			case 1:
				if (!(java_lang_Double["isInfinite(D)Z"](p0))) {
					_G = 2;
					continue;
				}
				if (!(((p0 < 0.0)))) {
					_G = 3;
					continue;
				}
				fA0 = (S[442]);
				_G = 4;
				continue;
			case 3:
				fA0 = (S[443]);
				_G = 4;
				continue;
			case 4:return (fA0); 
			case 2:
				if (((+(N.cmpl(p0, 0.0)) != 0))) {
					_G = 5;
					continue;
				}
				lJ2 = java_lang_Double["doubleToRawLongBits(D)J"](p0);
				if (((((N.lcmp(N.lushr(lJ2, 63), N.lnew(0, 0)))|0) == 0))) {
					_G = 5;
					continue;
				}
				return S[444];
			case 5:
				lA2 = (com_jtransc_text_JTranscStringTools["_toString(D)Ljava/lang/String;"](p0));
				lI3 = 0;
				lI4 = 0;
				_G = 6;
				continue;
			case 6:
				if (((lI4 >= (lA2)["length()I"]()))) {
					_G = 7;
					continue;
				}
				lI5 = (((lA2)["charAt(I)C"](lI4))|0);
				if (java_lang_Character["isDigit(C)Z"](((lI5)&0xFFFF))) {
					_G = 8;
					continue;
				}
				if (((lI5 == 45))) {
					_G = 8;
					continue;
				}
				lI3 = 1;
				_G = 7;
				continue;
			case 8:
				lI4 = (((lI4 + 1))|0);
				_G = 6;
				continue;
			case 7:
				if ((((lA2)["indexOf(Ljava/lang/String;)I"](S[445]) < 0))) {
					_G = 9;
					continue;
				}
				lA2 = ((lA2)["replace(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Ljava/lang/String;"]((S[445]), (S[446])));
				_G = 9;
				continue;
			case 9:
				if ((((lA2)["indexOf(Ljava/lang/String;)I"](S[447]) < 0))) {
					_G = 10;
					continue;
				}
				lA2 = ((lA2)["replace(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Ljava/lang/String;"]((S[447]), (S[448])));
				_G = 10;
				continue;
			case 10:
				if (((lI3 == 0))) {
					_G = 11;
					continue;
				}
				fA0 = lA2;
				_G = 12;
				continue;
			case 11:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				fA0 = ((fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"]((lA2))["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[449])["toString()Ljava/lang/String;"]());
				_G = 12;
				continue;
			case 12:return (fA0); 
			default:
				break;
		}
	}
	return null;
};
com_jtransc_text_JTranscStringTools["_toString(D)Ljava/lang/String;"] = function(p0) { 
	return N.str(String(N.isNegativeZero(+p0) ? '-0' : +p0));
};
function java_lang_Float() {
}
java_lang_Float.prototype = Object.create(java_lang_Number.prototype);
java_lang_Float.prototype.constructor = java_lang_Float;
java_lang_Float.prototype._value = 0.0;
java_lang_Float.SI = function() { 
	java_lang_Float._TYPE = null;
	java_lang_Float["java.lang.Float<clinit>()V"]();
};
java_lang_Float.prototype.__JT__CLASS_ID = java_lang_Float.__JT__CLASS_ID = 683;
java_lang_Float.prototype.__JT__CLASS_IDS = java_lang_Float.__JT__CLASS_IDS = [683,673,656,659];
java_lang_Float.prototype["java.lang.Float<init>(F)V"] = function(p0) { 
	(this)["java.lang.Number<init>()V"]();
	this._value = p0;
	return this;
	return this;
};
java_lang_Float["java.lang.Float<clinit>()V"] = function() { 
	java_lang_Float._TYPE = java_lang_Class["getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"](S[450]);
	return;
};
java_lang_Float["valueOf(F)Ljava/lang/Float;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_Float()));
	fA0 = tA0;
	(tA0)["java.lang.Float<init>(F)V"](p0);
	return (fA0);
};
java_lang_Float.prototype["toString()Ljava/lang/String;"] = function() { 
	return java_lang_Float["toString(F)Ljava/lang/String;"](this._value);
};
java_lang_Float["toString(F)Ljava/lang/String;"] = function(p0) { 
	return com_jtransc_text_JTranscStringTools["toString(F)Ljava/lang/String;"](p0);
};
java_lang_Float.prototype["hashCode()I"] = function() { 
	return java_lang_Float["hashCode(F)I"](this._value);
};
java_lang_Float["hashCode(F)I"] = function(p0) { 
	return java_lang_Float["floatToIntBits(F)I"](p0);
};
java_lang_Float["floatToIntBits(F)I"] = function(p0) { 
	return N.floatToIntBits(p0);
};
java_lang_Float.prototype["doubleValue()D"] = function() { 
	return (this._value);
};
java_lang_Float.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!((p0 instanceof java_lang_Float))) {
					_G = 1;
					continue;
				}
				if (((java_lang_Float["floatToIntBits(F)I"](N.checkCast(p0, java_lang_Float)._value) != java_lang_Float["floatToIntBits(F)I"](this._value)))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_Float.prototype["longValue()J"] = function() { 
	return N.f2j(this._value);
};
java_lang_Float.prototype["intValue()I"] = function() { 
	return ((((this._value)|0))|0);
};
java_lang_Float.prototype["shortValue()S"] = function() { 
	return ((((this._value)|0))<<16>>16);
};
java_lang_Float.prototype["byteValue()B"] = function() { 
	return ((((this._value)|0))<<24>>24);
};
java_lang_Float["intBitsToFloat(I)F"] = function(p0) { 
	return N.intBitsToFloat(p0);
};
java_lang_Float["floatToRawIntBits(F)I"] = function(p0) { 
	return N.floatToIntBits(p0);
};
function java_lang_Void() {
}
java_lang_Void.prototype = Object.create(java_lang_Object.prototype);
java_lang_Void.prototype.constructor = java_lang_Void;
java_lang_Void.prototype.___id = 0;
java_lang_Void.SI = function() { 
	java_lang_Void._TYPE = null;
	java_lang_Void["java.lang.Void<clinit>()V"]();
};
java_lang_Void.prototype.__JT__CLASS_ID = java_lang_Void.__JT__CLASS_ID = 682;
java_lang_Void.prototype.__JT__CLASS_IDS = java_lang_Void.__JT__CLASS_IDS = [682,656];
java_lang_Void.prototype["java.lang.Void<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang_Void["java.lang.Void<clinit>()V"] = function() { 
	java_lang_Void._TYPE = java_lang_Class["getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"](S[451]);
	return;
};
function java_lang_ReflectiveOperationException() {
}
java_lang_ReflectiveOperationException.prototype = Object.create(java_lang_Exception.prototype);
java_lang_ReflectiveOperationException.prototype.constructor = java_lang_ReflectiveOperationException;
java_lang_ReflectiveOperationException.SI = function(){};
java_lang_ReflectiveOperationException.prototype.__JT__CLASS_ID = java_lang_ReflectiveOperationException.__JT__CLASS_ID = 679;
java_lang_ReflectiveOperationException.prototype.__JT__CLASS_IDS = java_lang_ReflectiveOperationException.__JT__CLASS_IDS = [679,680,681,656,658];
java_lang_ReflectiveOperationException.prototype["java.lang.ReflectiveOperationException<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.Exception<init>(Ljava/lang/String;)V"](p0);
	return this;
	return this;
};
java_lang_ReflectiveOperationException.prototype["java.lang.ReflectiveOperationException<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"] = function(p0, p1) { 
	(this)["java.lang.Exception<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"](p0, p1);
	return this;
	return this;
};
java_lang_ReflectiveOperationException.prototype["java.lang.ReflectiveOperationException<init>()V"] = function() { 
	(this)["java.lang.Exception<init>()V"]();
	return this;
	return this;
};
java_lang_ReflectiveOperationException.prototype["java.lang.ReflectiveOperationException<init>(Ljava/lang/Throwable;)V"] = function(p0) { 
	(this)["java.lang.Exception<init>(Ljava/lang/Throwable;)V"](p0);
	return this;
	return this;
};
function java_lang_ClassNotFoundException() {
}
java_lang_ClassNotFoundException.prototype = Object.create(java_lang_ReflectiveOperationException.prototype);
java_lang_ClassNotFoundException.prototype.constructor = java_lang_ClassNotFoundException;
java_lang_ClassNotFoundException.prototype._ex = null;
java_lang_ClassNotFoundException.SI = function(){};
java_lang_ClassNotFoundException.prototype.__JT__CLASS_ID = java_lang_ClassNotFoundException.__JT__CLASS_ID = 678;
java_lang_ClassNotFoundException.prototype.__JT__CLASS_IDS = java_lang_ClassNotFoundException.__JT__CLASS_IDS = [678,679,680,681,656,658];
java_lang_ClassNotFoundException.prototype["java.lang.ClassNotFoundException<init>(Ljava/lang/String;)V"] = function(p0) { 
	(this)["java.lang.ReflectiveOperationException<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"](p0, null);
	return this;
	return this;
};
java_lang_ClassNotFoundException.prototype["java.lang.ClassNotFoundException<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"] = function(p0, p1) { 
	(this)["java.lang.ReflectiveOperationException<init>(Ljava/lang/String;Ljava/lang/Throwable;)V"](p0, null);
	this._ex = p1;
	return this;
	return this;
};
java_lang_ClassNotFoundException.prototype["java.lang.ClassNotFoundException<init>()V"] = function() { 
	(this)["java.lang.ReflectiveOperationException<init>(Ljava/lang/Throwable;)V"](N.checkCast(null, java_lang_Throwable));
	return this;
	return this;
};
function java_lang_reflect__InternalUtils() {
}
java_lang_reflect__InternalUtils.prototype = Object.create(java_lang_Object.prototype);
java_lang_reflect__InternalUtils.prototype.constructor = java_lang_reflect__InternalUtils;
java_lang_reflect__InternalUtils.prototype.___id = 0;
java_lang_reflect__InternalUtils.SI = function(){};
java_lang_reflect__InternalUtils.prototype.__JT__CLASS_ID = java_lang_reflect__InternalUtils.__JT__CLASS_ID = 677;
java_lang_reflect__InternalUtils.prototype.__JT__CLASS_IDS = java_lang_reflect__InternalUtils.__JT__CLASS_IDS = [677,656];
java_lang_reflect__InternalUtils.prototype["java.lang.reflect._InternalUtils<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang_reflect__InternalUtils["getTypeName(Ljava/lang/reflect/Type;)Ljava/lang/String;"] = function(p0) { 
	var _G = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!(((p0) instanceof java_lang_Class))) {
					_G = 1;
					continue;
				}
				return N.checkCast((p0), java_lang_Class)["getName()Ljava/lang/String;"]();
			case 1:
				return (p0)["toString()Ljava/lang/String;"]();
			default:
				break;
		}
	}
	return null;
};
java_lang_reflect__InternalUtils["parseMethodType(Ljava/lang/String;Ljava/lang/reflect/Type;)Ljava/lang/reflect/MethodTypeImpl;"] = function(p0, p1) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new com_jtransc_text_MStringReader()));
	fA0 = tA0;
	(tA0)["com.jtransc.text.MStringReader<init>(Ljava/lang/String;)V"](p0);
	return N.checkCast(java_lang_reflect__InternalUtils["parseType(Lcom/jtransc/text/MStringReader;Ljava/lang/reflect/Type;)Ljava/lang/reflect/Type;"]((fA0), p1), java_lang_reflect_MethodTypeImpl);
};
java_lang_reflect__InternalUtils["parseType(Lcom/jtransc/text/MStringReader;Ljava/lang/reflect/Type;)Ljava/lang/reflect/Type;"] = function(p0, p1) { 
	var _G = 0, lA6 = null, lI2 = 0, lI5 = 0, lI7 = 0, lI8 = 0, fA0 = null, fA1 = null, fA2 = null, tA2 = null, tA3 = null, tA0 = null, tA1 = null, lA10 = null, fI0 = 0, lA3 = null, lA4 = null, lA9 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI2 = ((p0["read()C"]())|0);
				switch (lI2) {
					case 40:
						_G = 2;
						continue;
					case 41: case 42: case 44: case 45: case 46: case 47: case 48: case 49: case 50: case 51: case 52: case 53: case 54: case 55: case 56: case 57: case 58: case 59: case 60: case 61: case 62: case 63: case 64: case 65: case 69: case 71: case 72: case 75: case 77: case 78: case 79: case 80: case 81: case 82: case 85: case 87: case 88: case 89:
						_G = 1;
						continue;
					case 43:
						_G = 3;
						continue;
					case 66:
						_G = 4;
						continue;
					case 67:
						_G = 5;
						continue;
					case 68:
						_G = 6;
						continue;
					case 70:
						_G = 7;
						continue;
					case 73:
						_G = 8;
						continue;
					case 74:
						_G = 9;
						continue;
					case 76: case 84:
						_G = 10;
						continue;
					case 83:
						_G = 11;
						continue;
					case 86:
						_G = 12;
						continue;
					case 90:
						_G = 13;
						continue;
					case 91:
						_G = 14;
						continue;
					default:
						_G = 1;
						continue;
				}
				_G = 2;
				continue;
			case 2:
				lA3 = java_lang_reflect__InternalUtils["parseTypes(Lcom/jtransc/text/MStringReader;Ljava/lang/reflect/Type;)[Ljava/lang/reflect/Type;"](p0, p1);
				p0["expect(C)V"](41);
				lA4 = java_lang_reflect__InternalUtils["parseType(Lcom/jtransc/text/MStringReader;Ljava/lang/reflect/Type;)Ljava/lang/reflect/Type;"](p0, p1);
				tA2 = ((new java_lang_reflect_MethodTypeImpl()));
				fA0 = tA2;
				(tA2)["java.lang.reflect.MethodTypeImpl<init>([Ljava/lang/reflect/Type;Ljava/lang/reflect/Type;)V"](lA3, lA4);
				return (fA0);
			case 3:
				return java_lang_reflect__InternalUtils["parseType(Lcom/jtransc/text/MStringReader;Ljava/lang/reflect/Type;)Ljava/lang/reflect/Type;"](p0, p1);
			case 12:
				return (java_lang_Void._TYPE);
			case 13:
				return (java_lang_Boolean._TYPE);
			case 4:
				return (java_lang_Byte._TYPE);
			case 5:
				return (java_lang_Character._TYPE);
			case 11:
				return (java_lang_Short._TYPE);
			case 6:
				return (java_lang_Double._TYPE);
			case 7:
				return (java_lang_Float._TYPE);
			case 8:
				return (java_lang_Integer._TYPE);
			case 9:
				return (java_lang_Long._TYPE);
			case 14:
				lI5 = (((p0._offset - 1))|0);
				lA6 = (java_lang_reflect__InternalUtils["parseType(Lcom/jtransc/text/MStringReader;Ljava/lang/reflect/Type;)Ljava/lang/reflect/Type;"](p0, p1));
				lI7 = p0._offset;
				if (!((lA6 instanceof java_lang_Class))) {
					_G = 15;
					continue;
				}
				return (java_lang_reflect__InternalUtils["Class_forName0(Ljava/lang/String;)Ljava/lang/Class;"](p0._str["substring(II)Ljava/lang/String;"](lI5, lI7)));
			case 15:
				tA3 = ((new java_lang_reflect_ArrayType()));
				fA0 = tA3;
				(tA3)["java.lang.reflect.ArrayType<init>(Ljava/lang/reflect/Type;)V"]((lA6));
				return (fA0);
			case 10:
				if (((lI2 != 84))) {
					_G = 16;
					continue;
				}
				fI0 = 1;
				_G = 17;
				continue;
			case 16:
				fI0 = 0;
				_G = 17;
				continue;
			case 17:
				lI8 = fI0;
				lA9 = p0["readUntil(CCZ)Ljava/lang/String;"](59, 60, false)["replace(CC)Ljava/lang/String;"](47, 46);
				if (((lI8 == 0))) {
					_G = 18;
					continue;
				}
				fA0 = (java_lang_reflect__InternalUtils["Class_forName0(Ljava/lang/String;)Ljava/lang/Class;"](S[140]));
				_G = 19;
				continue;
			case 18:
				fA0 = (java_lang_reflect__InternalUtils["Class_forName0(Ljava/lang/String;)Ljava/lang/Class;"](lA9));
				_G = 19;
				continue;
			case 19:
				lA10 = fA0;
				if (((p0["peek()C"]() != 60))) {
					_G = 20;
					continue;
				}
				lA10 = (java_lang_reflect__InternalUtils["parseTypeGeneric(Ljava/lang/reflect/Type;Lcom/jtransc/text/MStringReader;Ljava/lang/reflect/Type;)Ljava/lang/reflect/Type;"]((lA10), p0, p1));
				_G = 20;
				continue;
			case 20:
				p0["expect(C)V"](59);
				return (lA10);
			case 1:
				tA0 = ((new java_lang_Error()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.Error<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[453])["append(C)Ljava/lang/StringBuilder;"](((lI2)&0xFFFF))["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[452])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](p0._str)["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				break;
			default:
				break;
		}
	}
	return null;
};
java_lang_reflect__InternalUtils["parseTypes(Lcom/jtransc/text/MStringReader;Ljava/lang/reflect/Type;)[Ljava/lang/reflect/Type;"] = function(p0, p1) { 
	var _G = 0, lA2 = null, fA0 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_util_ArrayList()));
				fA0 = tA0;
				(tA0)["java.util.ArrayList<init>()V"]();
				lA2 = fA0;
				_G = 1;
				continue;
			case 1:
				if (!(p0["hasMore()Z"]())) {
					_G = 2;
					continue;
				}
				if (((p0["peek()C"]() != 41))) {
					_G = 3;
					continue;
				}
				_G = 2;
				continue;
			case 3:
				(lA2)["add(Ljava/lang/Object;)Z"]((java_lang_reflect__InternalUtils["parseType(Lcom/jtransc/text/MStringReader;Ljava/lang/reflect/Type;)Ljava/lang/reflect/Type;"](p0, p1)));
				_G = 1;
				continue;
			case 2:
				return N.checkCast((lA2)["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"]((new JA_L((lA2)["size()I"](), "[Ljava.lang.reflect.Type;"))), JA_L);
			default:
				break;
		}
	}
	return null;
};
java_lang_reflect__InternalUtils["Class_forName0(Ljava/lang/String;)Ljava/lang/Class;"] = function(p0) { 
	var _G = 0, fA0 = null, tA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						_G = 1;
						continue;
					case 1:
						fA0 = (java_lang_Class["forName(Ljava/lang/String;)Ljava/lang/Class;"](p0));
						_G = 2;
						continue;
					case 2:return (fA0); 
					case 3:
						fA0 = (J__exception__);
						tA0 = ((new java_lang_StringBuilder()));
						fA0 = tA0;
						(tA0)["java.lang.StringBuilder<init>()V"]();
						com_jtransc_io_JTranscConsole["error(Ljava/lang/Object;)V"](((fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[455])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](p0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[454])["toString()Ljava/lang/String;"]()));
						return null;
					default:
						break;
				}
			}
			return null;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_ClassNotFoundException)))) {
				_G = 3;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return null;
};
java_lang_reflect__InternalUtils["parseTypeGeneric(Ljava/lang/reflect/Type;Lcom/jtransc/text/MStringReader;Ljava/lang/reflect/Type;)Ljava/lang/reflect/Type;"] = function(p0, p1, p2) { 
	var _G = 0, lA3 = null, fA0 = null, tA0 = null, tA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				p1["expect(C)V"](60);
				tA0 = ((new java_util_ArrayList()));
				fA0 = tA0;
				(tA0)["java.util.ArrayList<init>()V"]();
				lA3 = fA0;
				_G = 1;
				continue;
			case 1:
				if (!(p1["hasMore()Z"]())) {
					_G = 2;
					continue;
				}
				if (((p1["peek()C"]() == 62))) {
					_G = 2;
					continue;
				}
				(lA3)["add(Ljava/lang/Object;)Z"]((java_lang_reflect__InternalUtils["parseType(Lcom/jtransc/text/MStringReader;Ljava/lang/reflect/Type;)Ljava/lang/reflect/Type;"](p1, p2)));
				_G = 1;
				continue;
			case 2:
				p1["expect(C)V"](62);
				tA2 = ((new java_lang_reflect_ParameterizedTypeImpl()));
				fA0 = tA2;
				(tA2)["java.lang.reflect.ParameterizedTypeImpl<init>([Ljava/lang/reflect/Type;Ljava/lang/reflect/Type;Ljava/lang/reflect/Type;)V"](N.checkCast((lA3)["toArray([Ljava/lang/Object;)[Ljava/lang/Object;"]((new JA_L((lA3)["size()I"](), "[Ljava.lang.reflect.Type;"))), JA_L), p0, p2);
				return (fA0);
			default:
				break;
		}
	}
	return null;
};
function java_lang_reflect_Field() {
}
java_lang_reflect_Field.prototype = Object.create(java_lang_reflect_AccessibleObject.prototype);
java_lang_reflect_Field.prototype.constructor = java_lang_reflect_Field;
java_lang_reflect_Field.prototype._clazz = null;
java_lang_reflect_Field.prototype.__name = null;
java_lang_reflect_Field.prototype._modifiers = 0;
java_lang_reflect_Field.prototype._signature = null;
java_lang_reflect_Field.prototype._info = null;
java_lang_reflect_Field.SI = function(){};
java_lang_reflect_Field.prototype.__JT__CLASS_ID = java_lang_reflect_Field.__JT__CLASS_ID = 674;
java_lang_reflect_Field.prototype.__JT__CLASS_IDS = java_lang_reflect_Field.__JT__CLASS_IDS = [674,676,656,675,669];
java_lang_reflect_Field.prototype["toString()Ljava/lang/String;"] = function() { 
	var _G = 0, lI1 = 0, fA0 = null, fA1 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = this["getModifiers()I"]();
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				if (((lI1 != 0))) {
					_G = 1;
					continue;
				}
				fA1 = (S[20]);
				_G = 2;
				continue;
			case 1:
				tA1 = ((new java_lang_StringBuilder()));
				fA1 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				fA1 = ((fA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_reflect_Modifier["toString(I)Ljava/lang/String;"](lI1))["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[389])["toString()Ljava/lang/String;"]());
				_G = 2;
				continue;
			case 2:return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"]((fA1))["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_reflect__InternalUtils["getTypeName(Ljava/lang/reflect/Type;)Ljava/lang/String;"]((this["getType()Ljava/lang/Class;"]())))["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[389])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_reflect__InternalUtils["getTypeName(Ljava/lang/reflect/Type;)Ljava/lang/String;"]((this["getDeclaringClass()Ljava/lang/Class;"]())))["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[135])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this["getName()Ljava/lang/String;"]())["toString()Ljava/lang/String;"](); 
			default:
				break;
		}
	}
	return null;
};
java_lang_reflect_Field.prototype["getDeclaringClass()Ljava/lang/Class;"] = function() { 
	return this._clazz;
};
java_lang_reflect_Field.prototype["getName()Ljava/lang/String;"] = function() { 
	return this.__name;
};
java_lang_reflect_Field.prototype["getModifiers()I"] = function() { 
	return this._modifiers;
};
java_lang_reflect_Field.prototype["getType()Ljava/lang/Class;"] = function() { 
	var _G = 0, lA1 = null, fA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						_G = 1;
						continue;
					case 1:
						fA0 = (java_lang_Class["forName(Ljava/lang/String;)Ljava/lang/Class;"](this._signature));
						_G = 2;
						continue;
					case 2:return (fA0); 
					case 3:
						fA0 = (J__exception__);
						lA1 = fA0;
						(lA1)["printStackTrace()V"]();
						return null;
					default:
						break;
				}
			}
			return null;
		}
		catch (J__i__exception__) {
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 2)))) && (J__exception__ instanceof java_lang_ClassNotFoundException)))) {
				_G = 3;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return null;
};
java_lang_reflect_Field.prototype["hashCode()I"] = function() { 
	return (((this["getDeclaringClass()Ljava/lang/Class;"]()["getName()Ljava/lang/String;"]()["hashCode()I"]() ^ this["getName()Ljava/lang/String;"]()["hashCode()I"]()))|0);
};
java_lang_reflect_Field.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if ((((this) != p0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
function java_lang_Integer() {
}
java_lang_Integer.prototype = Object.create(java_lang_Number.prototype);
java_lang_Integer.prototype.constructor = java_lang_Integer;
java_lang_Integer.prototype._value = 0;
java_lang_Integer.SI = function() { 
	java_lang_Integer._values = null;
	java_lang_Integer._NTZ_TABLE = null;
	java_lang_Integer._TYPE = null;
	java_lang_Integer["java.lang.Integer<clinit>()V"]();
};
java_lang_Integer.prototype.__JT__CLASS_ID = java_lang_Integer.__JT__CLASS_ID = 672;
java_lang_Integer.prototype.__JT__CLASS_IDS = java_lang_Integer.__JT__CLASS_IDS = [672,673,656,659];
java_lang_Integer.prototype["java.lang.Integer<init>(I)V"] = function(p0) { 
	(this)["java.lang.Number<init>()V"]();
	this._value = p0;
	return this;
	return this;
};
java_lang_Integer["java.lang.Integer<clinit>()V"] = function() { 
	var fA0 = null, tA0 = null;
	java_lang_Integer._TYPE = java_lang_Class["getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"](S[456]);
	tA0 = (new JA_B(64));
	fA0 = tA0;
	(tA0).data[0] = 32;
	(fA0).setArraySlice(1, [0, 1, 12, 2, 6, -1, 13, 3, -1, 7, -1, -1, -1, -1, 14, 10, 4, -1, -1, 8, -1, -1, 25, -1, -1, -1, -1, -1, 21, 27, 15, 31, 11, 5, -1, -1, -1, -1, -1, 9, -1, -1, 24, -1, -1, 20, 26, 30, -1, -1, -1, -1, 23, -1, 19, 29, -1, 22, 18, 28, 17, 16, -1]);
	java_lang_Integer._NTZ_TABLE = (fA0);
	return;
};
java_lang_Integer.prototype["toString()Ljava/lang/String;"] = function() { 
	return java_lang_Integer["toString(I)Ljava/lang/String;"](this._value);
};
java_lang_Integer["toString(I)Ljava/lang/String;"] = function(p0) { 
	return java_lang_Integer["toString(II)Ljava/lang/String;"](p0, 10);
};
java_lang_Integer["toString(II)Ljava/lang/String;"] = function(p0, p1) { 
	return N.str((p0|0).toString(p1));
};
java_lang_Integer["valueOf(I)Ljava/lang/Integer;"] = function(p0) { 
	var _G = 0, fI1 = 0, lI1 = 0, fA0 = null, fA2 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((java_lang_Integer._values != null))) {
					_G = 1;
					continue;
				}
				java_lang_Integer._values = new JA_L(256, "[Ljava.lang.Integer;");
				lI1 = -128;
				_G = 2;
				continue;
			case 2:
				if (((lI1 >= 128))) {
					_G = 1;
					continue;
				}
				fA0 = (java_lang_Integer._values);
				fI1 = (((lI1 - -128))|0);
				tA0 = ((new java_lang_Integer()));
				fA2 = tA0;
				(tA0)["java.lang.Integer<init>(I)V"](lI1);
				(fA0).data[fI1] = fA2;
				lI1 = (((lI1 + 1))|0);
				_G = 2;
				continue;
			case 1:
				if (((p0 < -128))) {
					_G = 3;
					continue;
				}
				if (((p0 >= 128))) {
					_G = 3;
					continue;
				}
				return (((java_lang_Integer._values).data[(((p0 - -128))|0)]));
			case 3:
				tA1 = ((new java_lang_Integer()));
				fA0 = tA1;
				(tA1)["java.lang.Integer<init>(I)V"](p0);
				return (fA0);
			default:
				break;
		}
	}
	return null;
};
java_lang_Integer.prototype["hashCode()I"] = function() { 
	return this._value;
};
java_lang_Integer.prototype["doubleValue()D"] = function() { 
	return +(this._value);
};
java_lang_Integer["toHexString(I)Ljava/lang/String;"] = function(p0) { 
	return java_lang_Integer["toUnsignedString(II)Ljava/lang/String;"](p0, 16);
};
java_lang_Integer["toUnsignedString(II)Ljava/lang/String;"] = function(p0, p1) { 
	return N.str((p0 >>> 0).toString(p1));
};
java_lang_Integer["min(II)I"] = function(p0, p1) { 
	return java_lang_Math["min(II)I"](p0, p1);
};
java_lang_Integer.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (!((p0 instanceof java_lang_Integer))) {
					_G = 1;
					continue;
				}
				if (((N.checkCast(p0, java_lang_Integer)._value != this._value))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_Integer.prototype["longValue()J"] = function() { 
	return N.i2j(this._value);
};
java_lang_Integer.prototype["intValue()I"] = function() { 
	return this._value;
};
java_lang_Integer.prototype["shortValue()S"] = function() { 
	return ((this._value)<<16>>16);
};
java_lang_Integer.prototype["byteValue()B"] = function() { 
	return ((this._value)<<24>>24);
};
java_lang_Integer["reverseBytes(I)I"] = function(p0) { 
	return ((((((((((((p0 >>> 24))|0) | ((((((p0 >> 8))|0) & 65280))|0)))|0) | ((((((p0 << 8))|0) & 16711680))|0)))|0) | (((p0 << 24))|0)))|0);
};
function java_lang_reflect_Modifier() {
}
java_lang_reflect_Modifier.prototype = Object.create(java_lang_Object.prototype);
java_lang_reflect_Modifier.prototype.constructor = java_lang_reflect_Modifier;
java_lang_reflect_Modifier.prototype.___id = 0;
java_lang_reflect_Modifier.SI = function(){};
java_lang_reflect_Modifier.prototype.__JT__CLASS_ID = java_lang_reflect_Modifier.__JT__CLASS_ID = 671;
java_lang_reflect_Modifier.prototype.__JT__CLASS_IDS = java_lang_reflect_Modifier.__JT__CLASS_IDS = [671,656];
java_lang_reflect_Modifier.prototype["java.lang.reflect.Modifier<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang_reflect_Modifier["isInterface(I)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if ((((((p0 & 512))|0) == 0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_reflect_Modifier["toString(I)Ljava/lang/String;"] = function(p0) { 
	var _G = 0, lA1 = null, fI0 = 0, lI2 = 0, fA0 = null, tI13 = 0, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				lA1 = fA0;
				if ((((((p0 & 1))|0) == 0))) {
					_G = 1;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[457]);
				_G = 1;
				continue;
			case 1:
				if ((((((p0 & 4))|0) == 0))) {
					_G = 2;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[458]);
				_G = 2;
				continue;
			case 2:
				if ((((((p0 & 2))|0) == 0))) {
					_G = 3;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[459]);
				_G = 3;
				continue;
			case 3:
				if ((((((p0 & 1024))|0) == 0))) {
					_G = 4;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[460]);
				_G = 4;
				continue;
			case 4:
				if ((((((p0 & 8))|0) == 0))) {
					_G = 5;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[461]);
				_G = 5;
				continue;
			case 5:
				if ((((((p0 & 16))|0) == 0))) {
					_G = 6;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[462]);
				_G = 6;
				continue;
			case 6:
				if ((((((p0 & 128))|0) == 0))) {
					_G = 7;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[463]);
				_G = 7;
				continue;
			case 7:
				if ((((((p0 & 64))|0) == 0))) {
					_G = 8;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[464]);
				_G = 8;
				continue;
			case 8:
				if ((((((p0 & 32))|0) == 0))) {
					_G = 9;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[465]);
				_G = 9;
				continue;
			case 9:
				if ((((((p0 & 256))|0) == 0))) {
					_G = 10;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[466]);
				_G = 10;
				continue;
			case 10:
				if ((((((p0 & 2048))|0) == 0))) {
					_G = 11;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[467]);
				_G = 11;
				continue;
			case 11:
				if ((((((p0 & 512))|0) == 0))) {
					_G = 12;
					continue;
				}
				(lA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[468]);
				_G = 12;
				continue;
			case 12:
				tI13 = (lA1)["length()I"]();
				fI0 = tI13;
				lI2 = tI13;
				if (((fI0 <= 0))) {
					_G = 13;
					continue;
				}
				return (lA1)["toString()Ljava/lang/String;"]()["substring(II)Ljava/lang/String;"](0, (((lI2 - 1))|0));
			case 13:
				return S[20];
			default:
				break;
		}
	}
	return null;
};
function java_lang_AnnotatedElement() {
}
java_lang_AnnotatedElement.prototype = Object.create(java_lang_Object_base.prototype);
java_lang_AnnotatedElement.prototype.constructor = java_lang_AnnotatedElement;
java_lang_AnnotatedElement.SI = function(){};
java_lang_AnnotatedElement.prototype.__JT__CLASS_ID = java_lang_AnnotatedElement.__JT__CLASS_ID = 670;
java_lang_AnnotatedElement.prototype.__JT__CLASS_IDS = java_lang_AnnotatedElement.__JT__CLASS_IDS = [670,656];
function java_lang_Class() {
}
java_lang_Class.prototype = Object.create(java_lang_Object.prototype);
java_lang_Class.prototype.constructor = java_lang_Class;
java_lang_Class.prototype.__name = null;
java_lang_Class.prototype._primitive = false;
java_lang_Class.prototype._modifiers = 0;
java_lang_Class.prototype.__accessibleMethods = null;
java_lang_Class.prototype._enumConstants = null;
java_lang_Class.prototype.__allFields = null;
java_lang_Class.prototype.__accessibleFields = null;
java_lang_Class.prototype.__allMethods = null;
java_lang_Class.prototype._id = 0;
java_lang_Class.prototype._related = null;
java_lang_Class.prototype._info = null;
java_lang_Class.prototype.___id = 0;
java_lang_Class.SI = function() { 
	java_lang_Class.__classCache = null;
};
java_lang_Class.prototype.__JT__CLASS_ID = java_lang_Class.__JT__CLASS_ID = 666;
java_lang_Class.prototype.__JT__CLASS_IDS = java_lang_Class.__JT__CLASS_IDS = [666,656,658,667,668,670,669];
java_lang_Class.prototype["java.lang.Class<init>(Ljava/lang/String;Z)V"] = function(p0, p1) { 
	(this)["java.lang.Object<init>()V"]();
	this._primitive = false;
	this._enumConstants = null;
	this.__allFields = null;
	this.__accessibleFields = null;
	this.__allMethods = null;
	this.__accessibleMethods = null;
	this.__name = p0;
	this._primitive = p1;
	this._id = -1;
	return this;
	return this;
};
java_lang_Class.prototype["java.lang.Class<init>(Ljava/lang/String;)V"] = function(p0) { 
	var _G = 0, fA0 = null, fA1 = null, fA2 = null, tA0 = null, tA1 = null;
	while (true) {
		switch (_G) {
			case 0:
				(this)["java.lang.Object<init>()V"]();
				this._primitive = false;
				this._enumConstants = null;
				this.__allFields = null;
				this.__accessibleFields = null;
				this.__allMethods = null;
				this.__accessibleMethods = null;
				this.__name = p0;
				this._primitive = false;
				if (this["_check()Z"]()) {
					_G = 1;
					continue;
				}
				tA0 = ((new java_lang_ClassNotFoundException()));
				fA0 = tA0;
				fA1 = tA0;
				tA1 = ((new java_lang_StringBuilder()));
				fA2 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA1)["java.lang.ClassNotFoundException<init>(Ljava/lang/String;)V"]((fA2)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[469])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](p0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[454])["toString()Ljava/lang/String;"]());
				throw new WrappedError(fA0);
				_G = 1;
				continue;
			case 1:
				return this;
			default:
				break;
		}
	}
	return this;
	return this;
};
java_lang_Class.prototype["toString()Ljava/lang/String;"] = function() { 
	var _G = 0, fA0 = null, fA1 = null, tA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				tA0 = ((new java_lang_StringBuilder()));
				fA0 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				if (!(this["isInterface()Z"]())) {
					_G = 1;
					continue;
				}
				fA1 = S[468];
				_G = 2;
				continue;
			case 1:
				if (!(this["isPrimitive()Z"]())) {
					_G = 3;
					continue;
				}
				fA1 = S[20];
				_G = 2;
				continue;
			case 3:
				fA1 = S[470];
				_G = 2;
				continue;
			case 2:return (fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](fA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](this.__name)["toString()Ljava/lang/String;"](); 
			default:
				break;
		}
	}
	return null;
};
java_lang_Class.prototype["isPrimitive()Z"] = function() { 
	return this._primitive;
};
java_lang_Class.prototype["isInterface()Z"] = function() { 
	return java_lang_reflect_Modifier["isInterface(I)Z"](this["getModifiers()I"]());
};
java_lang_Class.prototype["getModifiers()I"] = function() { 
	return (((this._modifiers & -33))|0);
};
java_lang_Class["getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"] = function(p0) { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_Class()));
	fA0 = tA0;
	(tA0)["java.lang.Class<init>(Ljava/lang/String;Z)V"](p0, true);
	return (fA0);
};
java_lang_Class.prototype["getName()Ljava/lang/String;"] = function() { 
	return this.__name;
};
java_lang_Class["forName(Ljava/lang/String;)Ljava/lang/Class;"] = function(p0) { 
	var _G = 0, lA1 = null, fA0 = null, fA2 = null, tA0 = null, tA1 = null, tA2 = null;
	while (true) {
		switch (_G) {
			case 0:
				if ((((p0) != null))) {
					_G = 1;
					continue;
				}
				return null;
			case 1:
				if (((p0["length()I"]() != 1))) {
					_G = 2;
					continue;
				}
				switch (p0["charAt(I)C"](0)) {
					case 66:
						_G = 3;
						continue;
					case 67:
						_G = 4;
						continue;
					case 68:
						_G = 5;
						continue;
					case 69: case 71: case 72: case 75: case 76: case 77: case 78: case 79: case 80: case 81: case 82: case 84: case 85: case 87: case 88: case 89:
						_G = 2;
						continue;
					case 70:
						_G = 6;
						continue;
					case 73:
						_G = 7;
						continue;
					case 74:
						_G = 8;
						continue;
					case 83:
						_G = 9;
						continue;
					case 86:
						_G = 10;
						continue;
					case 90:
						_G = 11;
						continue;
					default:
						_G = 2;
						continue;
				}
				_G = 10;
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
				if (!(p0["startsWith(Ljava/lang/String;)Z"](S[471]))) {
					_G = 12;
					continue;
				}
				if (!(p0["endsWith(Ljava/lang/String;)Z"](S[472]))) {
					_G = 12;
					continue;
				}
				return java_lang_Class["forName(Ljava/lang/String;)Ljava/lang/Class;"](p0["substring(II)Ljava/lang/String;"](1, (((p0["length()I"]() - 1))|0))["replace(CC)Ljava/lang/String;"](47, 46));
			case 12:
				if (((java_lang_Class.__classCache != null))) {
					_G = 13;
					continue;
				}
				tA0 = ((new com_jtransc_ds_FastStringMap()));
				fA0 = tA0;
				(tA0)["com.jtransc.ds.FastStringMap<init>()V"]();
				java_lang_Class.__classCache = (fA0);
				_G = 13;
				continue;
			case 13:
				if (java_lang_Class.__classCache["has(Ljava/lang/String;)Z"](p0)) {
					_G = 14;
					continue;
				}
				fA0 = (java_lang_Class.__classCache);
				tA1 = ((new java_lang_Class()));
				fA2 = tA1;
				(tA1)["java.lang.Class<init>(Ljava/lang/String;)V"](p0);
				(fA0)["set(Ljava/lang/String;Ljava/lang/Object;)V"](p0, fA2);
				_G = 14;
				continue;
			case 14:
				lA1 = (N.checkCast(java_lang_Class.__classCache["get(Ljava/lang/String;)Ljava/lang/Object;"](p0), java_lang_Class));
				if (((lA1 != null))) {
					_G = 15;
					continue;
				}
				tA2 = ((new java_lang_StringBuilder()));
				fA0 = tA2;
				(tA2)["java.lang.StringBuilder<init>()V"]();
				com_jtransc_io_JTranscConsole["error(Ljava/lang/Object;)V"](((fA0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[473])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](p0)["toString()Ljava/lang/String;"]()));
				_G = 15;
				continue;
			case 15:
				return (lA1);
			default:
				break;
		}
	}
	return null;
};
java_lang_Class.prototype["_check()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				this._info = java_lang_jtransc_JTranscCoreReflection["getClassInfoWithName(Ljava/lang/String;)Lj/ClassInfo;"](this.__name);
				if (((this._info == null))) {
					_G = 1;
					continue;
				}
				this._id = this._info._id;
				this._related = this._info._related;
				this._modifiers = java_lang_jtransc_JTranscCoreReflection["getModifiersWithId(I)I"](this._id);
				_G = 2;
				continue;
			case 1:
				this._id = -1;
				this._related = new JA_I(0);
				this._modifiers = 0;
				_G = 2;
				continue;
			case 2:
				if (this["isArray()Z"]()) {
					_G = 3;
					continue;
				}
				if (((this._id < 0))) {
					_G = 4;
					continue;
				}
				_G = 3;
				continue;
			case 3:
				fI0 = 1;
				_G = 5;
				continue;
			case 4:
				fI0 = 0;
				_G = 5;
				continue;
			case 5:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_Class.prototype["isArray()Z"] = function() { 
	return this.__name["startsWith(Ljava/lang/String;)Z"](S[66]);
};
java_lang_Class.prototype["hashCode()I"] = function() { 
	return this.__name["hashCode()I"]();
};
java_lang_Class.prototype["getDeclaringClass()Ljava/lang/Class;"] = function() { N.methodWithoutBody('java.lang.Class.getDeclaringClass') };
java_lang_Class.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if ((((this) != p0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_Class.prototype["getComponentType()Ljava/lang/Class;"] = function() { 
	var _G = 0, lA1 = null, fA0 = null, tA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						if (!(this["isArray()Z"]())) {
							_G = 1;
							continue;
						}
						_G = 2;
						continue;
					case 2:
						lA1 = (java_lang_Class["forName(Ljava/lang/String;)Ljava/lang/Class;"](this["getName()Ljava/lang/String;"]()["substring(I)Ljava/lang/String;"](1)));
						fA0 = lA1;
						_G = 3;
						continue;
					case 3:return (fA0); 
					case 4:
						fA0 = (J__exception__);
						lA1 = fA0;
						tA0 = ((new java_lang_RuntimeException()));
						fA0 = tA0;
						(tA0)["java.lang.RuntimeException<init>(Ljava/lang/Throwable;)V"]((lA1));
						throw new WrappedError(fA0);
						_G = 1;
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
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 2)) && ((_G < 3)))) && (J__exception__ instanceof java_lang_ClassNotFoundException)))) {
				_G = 4;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return null;
};
function java_lang_StringBuilder() {
}
java_lang_StringBuilder.prototype = Object.create(java_lang_Object.prototype);
java_lang_StringBuilder.prototype.constructor = java_lang_StringBuilder;
java_lang_StringBuilder.prototype.___id = 0;
java_lang_StringBuilder.SI = function(){};
java_lang_StringBuilder.prototype.__JT__CLASS_ID = java_lang_StringBuilder.__JT__CLASS_ID = 664;
java_lang_StringBuilder.prototype.__JT__CLASS_IDS = java_lang_StringBuilder.__JT__CLASS_IDS = [664,656,658,665,660];
java_lang_StringBuilder.prototype["java.lang.StringBuilder<init>()V"] = function() { 
	this["java.lang.StringBuilder<init>(I)V"](16);
	return this;
	return this;
};
java_lang_StringBuilder.prototype["java.lang.StringBuilder<init>(I)V"] = function(p0) { 
	this._str = ''; return this;
	return this;
};
java_lang_StringBuilder.prototype["toString()Ljava/lang/String;"] = function() { 
	return N.str(this._str);
};
java_lang_StringBuilder.prototype["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"] = function(p0) { 
	this._str += N.istr(p0); return this;
};
java_lang_StringBuilder.prototype["length()I"] = function() { 
	return this._str.length;
};
java_lang_StringBuilder.prototype["charAt(I)C"] = function(p0) { 
	return this._str.charCodeAt(p0) & 0xFFFF;
};
java_lang_StringBuilder.prototype["append(C)Ljava/lang/StringBuilder;"] = function(p0) { 
	this._str += N.ichar(p0); return this;
};
java_lang_StringBuilder.prototype["reverse()Ljava/lang/StringBuilder;"] = function() { 
	this._str = this._str.reverse(); return this;
};
java_lang_StringBuilder.prototype["append(I)Ljava/lang/StringBuilder;"] = function(p0) { 
	this._str += p0; return this;
};
java_lang_StringBuilder.prototype["append(Ljava/lang/Object;)Ljava/lang/StringBuilder;"] = function(p0) { 
	return this["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_String["valueOf(Ljava/lang/Object;)Ljava/lang/String;"](p0));
};
java_lang_StringBuilder.prototype["substring(II)Ljava/lang/String;"] = function(p0, p1) { 
	return this["toString()Ljava/lang/String;"]()["substring(II)Ljava/lang/String;"](p0, p1);
};
java_lang_StringBuilder.prototype["indexOf(Ljava/lang/String;)I"] = function(p0) { 
	return this._str.indexOf(N.istr(p0));
};
java_lang_StringBuilder.prototype["setLength(I)V"] = function(p0) { 
	this["delete(II)Ljava/lang/StringBuilder;"](p0, this["length()I"]());
	return;
};
java_lang_StringBuilder.prototype["delete(II)Ljava/lang/StringBuilder;"] = function(p0, p1) { 
	this._str = this._str.substr(0, p0) + this._str.substr(p1); return this;
};
java_lang_StringBuilder.prototype["append(F)Ljava/lang/StringBuilder;"] = function(p0) { 
	return this["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_String["valueOf(F)Ljava/lang/String;"](p0));
};
java_lang_StringBuilder.prototype["ensureCapacity(I)V"] = function(p0) { 

};
java_lang_StringBuilder.prototype["append(J)Ljava/lang/StringBuilder;"] = function(p0) { 
	return this["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_String["valueOf(J)Ljava/lang/String;"](p0));
};
java_lang_StringBuilder.prototype["append(D)Ljava/lang/StringBuilder;"] = function(p0) { 
	return this["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_String["valueOf(D)Ljava/lang/String;"](p0));
};
function java_lang_String$1() {
}
java_lang_String$1.prototype = Object.create(java_lang_Object.prototype);
java_lang_String$1.prototype.constructor = java_lang_String$1;
java_lang_String$1.prototype.___id = 0;
java_lang_String$1.SI = function(){};
java_lang_String$1.prototype.__JT__CLASS_ID = java_lang_String$1.__JT__CLASS_ID = 663;
java_lang_String$1.prototype.__JT__CLASS_IDS = java_lang_String$1.__JT__CLASS_IDS = [663,656];
function java_util_Comparator() {
}
java_util_Comparator.prototype = Object.create(java_lang_Object_base.prototype);
java_util_Comparator.prototype.constructor = java_util_Comparator;
java_util_Comparator.SI = function(){};
java_util_Comparator.prototype.__JT__CLASS_ID = java_util_Comparator.__JT__CLASS_ID = 662;
java_util_Comparator.prototype.__JT__CLASS_IDS = java_util_Comparator.__JT__CLASS_IDS = [662,656];
java_util_Comparator.prototype["compare(Ljava/lang/Object;Ljava/lang/Object;)I"] = function() { N.methodWithoutBody('java.util.Comparator.compare') };
java_util_Comparator.prototype["equals(Ljava/lang/Object;)Z"] = function() { N.methodWithoutBody('java.util.Comparator.equals') };
function java_lang_String$CaseInsensitiveComparator() {
}
java_lang_String$CaseInsensitiveComparator.prototype = Object.create(java_lang_Object.prototype);
java_lang_String$CaseInsensitiveComparator.prototype.constructor = java_lang_String$CaseInsensitiveComparator;
java_lang_String$CaseInsensitiveComparator.prototype.___id = 0;
java_lang_String$CaseInsensitiveComparator.SI = function(){};
java_lang_String$CaseInsensitiveComparator.prototype.__JT__CLASS_ID = java_lang_String$CaseInsensitiveComparator.__JT__CLASS_ID = 661;
java_lang_String$CaseInsensitiveComparator.prototype.__JT__CLASS_IDS = java_lang_String$CaseInsensitiveComparator.__JT__CLASS_IDS = [661,656,662,658];
java_lang_String$CaseInsensitiveComparator.prototype["java.lang.String$CaseInsensitiveComparator<init>(Ljava/lang/String$1;)V"] = function(p0) { 
	this["java.lang.String$CaseInsensitiveComparator<init>()V"]();
	return this;
	return this;
};
java_lang_String$CaseInsensitiveComparator.prototype["java.lang.String$CaseInsensitiveComparator<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
java_lang_String$CaseInsensitiveComparator.prototype["compare(Ljava/lang/Object;Ljava/lang/Object;)I"] = function(p0, p1) { 
	return this["compare(Ljava/lang/String;Ljava/lang/String;)I"](N.checkCast(p0, java_lang_String), N.checkCast(p1, java_lang_String));
};
java_lang_String$CaseInsensitiveComparator.prototype["compare(Ljava/lang/String;Ljava/lang/String;)I"] = function(p0, p1) { 
	return p0["compareToIgnoreCase(Ljava/lang/String;)I"](p1);
};
function java_lang_String() {
}
java_lang_String.prototype = Object.create(java_lang_Object.prototype);
java_lang_String.prototype.constructor = java_lang_String;
java_lang_String.prototype._hash = 0;
java_lang_String.prototype.___id = 0;
java_lang_String.SI = function() { 
	java_lang_String._CASE_INSENSITIVE_ORDER = null;
	java_lang_String["java.lang.String<clinit>()V"]();
};
java_lang_String.prototype.__JT__CLASS_ID = java_lang_String.__JT__CLASS_ID = 657;
java_lang_String.prototype.__JT__CLASS_IDS = java_lang_String.__JT__CLASS_IDS = [657,656,658,659,660];
java_lang_String["java.lang.String<clinit>()V"] = function() { 
	var fA0 = null, tA0 = null;
	tA0 = ((new java_lang_String$CaseInsensitiveComparator()));
	fA0 = tA0;
	(tA0)["java.lang.String$CaseInsensitiveComparator<init>(Ljava/lang/String$1;)V"](null);
	java_lang_String._CASE_INSENSITIVE_ORDER = (fA0);
	return;
};
java_lang_String.prototype["java.lang.String<init>([C)V"] = function(p0) { 
	(this)["java.lang.Object<init>()V"]();
	this._hash = 0;
	this["setChars([C)V"](java_util_Arrays["copyOf([CI)[C"](p0, (p0).length));
	return this;
	return this;
};
java_lang_String.prototype["java.lang.String<init>([BII)V"] = function(p0, p1, p2) { 
	this["java.lang.String<init>([BIILjava/lang/String;Z)V"](p0, p1, p2, S[51], false);
	return this;
	return this;
};
java_lang_String.prototype["java.lang.String<init>([BIILjava/lang/String;Z)V"] = function(p0, p1, p2, p3, p4) { 
	(this)["java.lang.Object<init>()V"]();
	this._hash = 0;
	this["setChars([C)V"](com_jtransc_charset_JTranscCharset["forName(Ljava/lang/String;)Lcom/jtransc/charset/JTranscCharset;"](p3)["decodeChars([BII)[C"](p0, p1, p2));
	return this;
	return this;
};
java_lang_String.prototype["java.lang.String<init>([CII)V"] = function(p0, p1, p2) { 
	(this)["java.lang.Object<init>()V"]();
	this._hash = 0;
	this["setChars([C)V"](java_util_Arrays["copyOfRange([CII)[C"](p0, p1, (((p1 + p2))|0)));
	return this;
	return this;
};
java_lang_String.prototype["toString()Ljava/lang/String;"] = function() { 
	return this;
};
java_lang_String.prototype["substring(II)Ljava/lang/String;"] = function(p0, p1) { 
	return N.str(this._str.slice(p0, p1));
};
java_lang_String.prototype["length()I"] = function() { 
	return this._str.length;
};
java_lang_String.prototype["setChars([C)V"] = function(p0) { 
	this._str = N.charArrayToString(p0, 0, p0.length);
};
java_lang_String.prototype["charAt(I)C"] = function(p0) { 
	return this._str.charCodeAt(p0) & 0xFFFF;
};
java_lang_String.prototype["replace(Ljava/lang/CharSequence;Ljava/lang/CharSequence;)Ljava/lang/String;"] = function(p0, p1) { 
	return this["_replace(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"](p0["toString()Ljava/lang/String;"](), p1["toString()Ljava/lang/String;"]());
};
java_lang_String.prototype["_replace(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"] = function(p0, p1) { 
	return N.str(N.istr(this).replaceAll(N.istr(p0), N.istr(p1)));
};
java_lang_String.prototype["indexOf(Ljava/lang/String;)I"] = function(p0) { 
	return this._str.indexOf(N.istr(p0));
};
java_lang_String.prototype["indexOf(I)I"] = function(p0) { 
	return this._str.indexOf(N.ichar(p0));
};
java_lang_String.prototype["startsWith(Ljava/lang/String;)Z"] = function(p0) { 
	return this._str.startsWith(p0._str);
};
java_lang_String["valueOf(Ljava/lang/Object;)Ljava/lang/String;"] = function(p0) { 
	var _G = 0, fA0 = null;
	while (true) {
		switch (_G) {
			case 0:
				if (((p0 == null))) {
					_G = 1;
					continue;
				}
				fA0 = p0["toString()Ljava/lang/String;"]();
				_G = 2;
				continue;
			case 1:
				fA0 = S[474];
				_G = 2;
				continue;
			case 2:return fA0; 
			default:
				break;
		}
	}
	return null;
};
java_lang_String.prototype["replace(CC)Ljava/lang/String;"] = function(p0, p1) { 
	return N.str(N.istr(this).replaceAll(N.ichar(p0), N.ichar(p1)));
};
java_lang_String.prototype["endsWith(Ljava/lang/String;)Z"] = function(p0) { 
	return this._str.endsWith(p0._str);
};
java_lang_String.prototype["hashCode()I"] = function() { 
	var _G = 0, lI1 = 0, lI2 = 0, lI3 = 0;
	while (true) {
		switch (_G) {
			case 0:
				lI1 = this._hash;
				lI2 = this["length()I"]();
				if (((lI1 != 0))) {
					_G = 1;
					continue;
				}
				if (((lI2 <= 0))) {
					_G = 1;
					continue;
				}
				lI3 = 0;
				_G = 2;
				continue;
			case 2:
				if (((lI3 >= lI2))) {
					_G = 3;
					continue;
				}
				lI1 = (((((Math.imul(31, lI1))|0) + this["charAt(I)C"](lI3)))|0);
				lI3 = (((lI3 + 1))|0);
				_G = 2;
				continue;
			case 3:
				this._hash = lI1;
				_G = 1;
				continue;
			case 1:
				return lI1;
			default:
				break;
		}
	}
	return 0;
};
java_lang_String.prototype["compareToIgnoreCase(Ljava/lang/String;)I"] = function(p0) { 
	return this["toLowerCase()Ljava/lang/String;"]()["compareTo(Ljava/lang/String;)I"](p0["toLowerCase()Ljava/lang/String;"]());
};
java_lang_String.prototype["toLowerCase()Ljava/lang/String;"] = function() { 
	return N.str(this._str.toLowerCase());
};
java_lang_String.prototype["compareTo(Ljava/lang/String;)I"] = function(p0) { 
	return this["_compareTo(Ljava/lang/String;)I"](p0);
};
java_lang_String.prototype["_compareTo(Ljava/lang/String;)I"] = function(p0) { 
	var a = N.istr(this), b = N.istr(p0); return (a < b) ? -1 : ((a > b) ? 1 : 0);
};
java_lang_String.prototype["isEmpty()Z"] = function() { 
	var _G = 0, fI0 = 0;
	while (true) {
		switch (_G) {
			case 0:
				if (((this["length()I"]() != 0))) {
					_G = 1;
					continue;
				}
				fI0 = 1;
				_G = 2;
				continue;
			case 1:
				fI0 = 0;
				_G = 2;
				continue;
			case 2:return ((fI0)!=0); 
			default:
				break;
		}
	}
	return false;
};
java_lang_String["valueOf(D)Ljava/lang/String;"] = function(p0) { 
	return java_lang_Double["toString(D)Ljava/lang/String;"](p0);
};
java_lang_String.prototype["getBytes(Ljava/lang/String;)[B"] = function(p0) { 
	return com_jtransc_charset_JTranscCharset["forName(Ljava/lang/String;)Lcom/jtransc/charset/JTranscCharset;"](p0)["encode(Ljava/lang/String;)[B"](this);
};
java_lang_String.prototype["toCharArray()[C"] = function() { 
	if (!this._arr) this._arr = N.stringToCharArray(this._str); return this._arr;
};
java_lang_String.prototype["trim()Ljava/lang/String;"] = function() { 
	return N.str(this._str.trim());
};
java_lang_String.prototype["toUpperCase()Ljava/lang/String;"] = function() { 
	return N.str(this._str.toUpperCase());
};
java_lang_String.prototype["equals(Ljava/lang/Object;)Z"] = function(p0) { 
	return N.is(p0, java_lang_String) && N.istr(this) == N.istr(p0);
};
java_lang_String.prototype["substring(I)Ljava/lang/String;"] = function(p0) { 
	return N.str(this._str.slice(p0));
};
java_lang_String["valueOf(F)Ljava/lang/String;"] = function(p0) { 
	return java_lang_Float["toString(F)Ljava/lang/String;"](p0);
};
java_lang_String["valueOf(J)Ljava/lang/String;"] = function(p0) { 
	return java_lang_Long["toString(J)Ljava/lang/String;"](p0);
};
function Benchmark() {
}
Benchmark.prototype = Object.create(java_lang_Object.prototype);
Benchmark.prototype.constructor = Benchmark;
Benchmark.prototype.___id = 0;
Benchmark.SI = function() { 
	Benchmark._totalTime = 0.0;
	Benchmark["Benchmark<clinit>()V"]();
};
Benchmark.prototype.__JT__CLASS_ID = Benchmark.__JT__CLASS_ID = 655;
Benchmark.prototype.__JT__CLASS_IDS = Benchmark.__JT__CLASS_IDS = [655,656];
Benchmark.prototype["Benchmark<init>()V"] = function() { 
	(this)["java.lang.Object<init>()V"]();
	return this;
	return this;
};
Benchmark["Benchmark<clinit>()V"] = function() { 
	Benchmark._totalTime = 0.0;
	return;
};
Benchmark["main([Ljava/lang/String;)V"] = function(p0) { 
	var fA1 = null, tA39 = null, tA31 = null, tA35 = null, tA3 = null, tA7 = null, lA14 = null, tA296 = null, lA4 = null, tA22 = null, tA26 = null, lA8 = null, tA10 = null, tA14 = null, tA18 = null, lA1 = null, _G = 0, lI13 = 0, fA0 = null, lA5 = null, tA32 = null, tA36 = null, tA2 = null, tA6 = null, lA11 = null, tA295 = null, tA299 = null, tA23 = null, tA27 = null, tA40 = null, tA11 = null, tA15 = null, lA9 = null, tA19 = null, lA12 = null, tA33 = null, tA37 = null, tA1 = null, tA5 = null, tA9 = null, tA294 = null, tA298 = null, tA301 = null, tA20 = null, tA24 = null, tA28 = null, tA41 = null, lA6 = null, tA12 = null, tA16 = null, lA2 = null, lI15 = 0, tA38 = null, tA30 = null, tA34 = null, tA0 = null, tA4 = null, tA8 = null, lA13 = null, tA297 = null, tA300 = null, lA10 = null, tA21 = null, tA25 = null, tA29 = null, tA13 = null, tA17 = null, lA3 = null, lA7 = null;
	while (true) {
		switch (_G) {
			case 0:
				lA1 = java_lang_Runtime["getRuntime()Ljava/lang/Runtime;"]();
				fA0 = (java_lang_System._out);
				tA0 = ((new java_lang_StringBuilder()));
				fA1 = tA0;
				(tA0)["java.lang.StringBuilder<init>()V"]();
				(fA0)["println(Ljava/lang/String;)V"]((fA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[476])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](com_jtransc_JTranscVersion["getVersion()Ljava/lang/String;"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[475])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](com_jtransc_JTranscSystem["getRuntimeKind()Ljava/lang/String;"]())["toString()Ljava/lang/String;"]());
				fA0 = (java_lang_System._out);
				tA1 = ((new java_lang_StringBuilder()));
				fA1 = tA1;
				(tA1)["java.lang.StringBuilder<init>()V"]();
				(fA0)["println(Ljava/lang/String;)V"]((fA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[477])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_System["getProperty(Ljava/lang/String;)Ljava/lang/String;"](S[408]))["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[475])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_System["getProperty(Ljava/lang/String;)Ljava/lang/String;"](S[410]))["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[475])["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](java_lang_System["getProperty(Ljava/lang/String;)Ljava/lang/String;"](S[412]))["toString()Ljava/lang/String;"]());
				fA0 = (java_lang_System._out);
				tA2 = ((new java_lang_StringBuilder()));
				fA1 = tA2;
				(tA2)["java.lang.StringBuilder<init>()V"]();
				(fA0)["println(Ljava/lang/String;)V"]((fA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[480])["append(J)Ljava/lang/StringBuilder;"](lA1["freeMemory()J"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[479])["append(J)Ljava/lang/StringBuilder;"](lA1["maxMemory()J"]())["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[478])["append(J)Ljava/lang/StringBuilder;"](lA1["totalMemory()J"]())["toString()Ljava/lang/String;"]());
				java_lang_System._out["println(Ljava/lang/String;)V"](S[481]);
				fA0 = (S[482]);
				tA3 = ((new Benchmark$1()));
				fA1 = tA3;
				(tA3)["Benchmark$1<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[483]);
				tA4 = ((new Benchmark$2()));
				fA1 = tA4;
				(tA4)["Benchmark$2<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[484]);
				tA5 = ((new Benchmark$3()));
				fA1 = tA5;
				(tA5)["Benchmark$3<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[485]);
				tA6 = ((new Benchmark$4()));
				fA1 = tA6;
				(tA6)["Benchmark$4<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[486]);
				tA7 = ((new Benchmark$5()));
				fA1 = tA7;
				(tA7)["Benchmark$5<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[487]);
				tA8 = ((new Benchmark$6()));
				fA1 = tA8;
				(tA8)["Benchmark$6<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[488]);
				tA9 = ((new Benchmark$7()));
				fA1 = tA9;
				(tA9)["Benchmark$7<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[489]);
				tA10 = ((new Benchmark$8()));
				fA1 = tA10;
				(tA10)["Benchmark$8<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[490]);
				tA11 = ((new Benchmark$9()));
				fA1 = tA11;
				(tA11)["Benchmark$9<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[491]);
				tA12 = ((new Benchmark$10()));
				fA1 = tA12;
				(tA12)["Benchmark$10<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[492]);
				tA13 = ((new Benchmark$11()));
				fA1 = tA13;
				(tA13)["Benchmark$11<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[493]);
				tA14 = ((new Benchmark$12()));
				fA1 = tA14;
				(tA14)["Benchmark$12<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[494]);
				tA15 = ((new Benchmark$13()));
				fA1 = tA15;
				(tA15)["Benchmark$13<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[495]);
				tA16 = ((new Benchmark$14()));
				fA1 = tA16;
				(tA16)["Benchmark$14<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				lA2 = new JA_I(16384);
				lA3 = new JA_I(16384);
				fA0 = (S[496]);
				tA17 = ((new Benchmark$15()));
				fA1 = tA17;
				(tA17)["Benchmark$15<init>([I[I)V"](lA2, lA3);
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				lA4 = new JA_B(1000000);
				lA5 = new JA_S(1000000);
				lA6 = new JA_C(1000000);
				lA7 = new JA_I(1000000);
				lA8 = new JA_F(1000000);
				lA9 = new JA_D(1000000);
				fA0 = (S[497]);
				tA18 = ((new Benchmark$16()));
				fA1 = tA18;
				(tA18)["Benchmark$16<init>([B)V"](lA4);
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[498]);
				tA19 = ((new Benchmark$17()));
				fA1 = tA19;
				(tA19)["Benchmark$17<init>([S)V"](lA5);
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[499]);
				tA20 = ((new Benchmark$18()));
				fA1 = tA20;
				(tA20)["Benchmark$18<init>([C)V"](lA6);
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[500]);
				tA21 = ((new Benchmark$19()));
				fA1 = tA21;
				(tA21)["Benchmark$19<init>([I)V"](lA7);
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[501]);
				tA22 = ((new Benchmark$20()));
				fA1 = tA22;
				(tA22)["Benchmark$20<init>([F)V"](lA8);
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[502]);
				tA23 = ((new Benchmark$21()));
				fA1 = tA23;
				(tA23)["Benchmark$21<init>([D)V"](lA9);
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[503]);
				tA24 = ((new Benchmark$22()));
				fA1 = tA24;
				(tA24)["Benchmark$22<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[504]);
				tA25 = ((new Benchmark$23()));
				fA1 = tA25;
				(tA25)["Benchmark$23<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[505]);
				tA26 = ((new Benchmark$24()));
				fA1 = tA26;
				(tA26)["Benchmark$24<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[506]);
				tA27 = ((new Benchmark$25()));
				fA1 = tA27;
				(tA27)["Benchmark$25<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[507]);
				tA28 = ((new Benchmark$26()));
				fA1 = tA28;
				(tA28)["Benchmark$26<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[508]);
				tA29 = ((new Benchmark$27()));
				fA1 = tA29;
				(tA29)["Benchmark$27<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[509]);
				tA30 = ((new Benchmark$28()));
				fA1 = tA30;
				(tA30)["Benchmark$28<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[510]);
				tA31 = ((new Benchmark$29()));
				fA1 = tA31;
				(tA31)["Benchmark$29<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[511]);
				tA32 = ((new Benchmark$30()));
				fA1 = tA32;
				(tA32)["Benchmark$30<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[512]);
				tA33 = ((new Benchmark$31()));
				fA1 = tA33;
				(tA33)["Benchmark$31<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[513]);
				tA34 = ((new Benchmark$32()));
				fA1 = tA34;
				(tA34)["Benchmark$32<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[514]);
				tA35 = ((new Benchmark$33()));
				fA1 = tA35;
				(tA35)["Benchmark$33<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[515]);
				tA36 = ((new Benchmark$34()));
				fA1 = tA36;
				(tA36)["Benchmark$34<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[516]);
				tA37 = ((new Benchmark$35()));
				fA1 = tA37;
				(tA37)["Benchmark$35<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				java_lang_System["gc()V"]();
				fA0 = (S[517]);
				tA38 = ((new Benchmark$36()));
				fA1 = tA38;
				(tA38)["Benchmark$36<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				lA10 = new JA_L(100000, "[LBenchmark$MyClass2;");
				fA0 = (S[518]);
				tA39 = ((new Benchmark$37()));
				fA1 = tA39;
				(tA39)["Benchmark$37<init>([LBenchmark$MyClass2;)V"](lA10);
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[519]);
				tA40 = ((new Benchmark$38()));
				fA1 = tA40;
				(tA40)["Benchmark$38<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				tA41 = (new JA_C(253));
				fA0 = tA41;
				(tA41).data[0] = 80;
				(fA0).setArraySlice(1, [75, 3, 4, 10, 3, 0, 0, 0, 0, 73, 158, 116, 72, 163, 28, 41, 28, 12, 0, 0, 0, 12, 0, 0, 0, 9, 0, 0, 0, 104, 101, 108, 108, 111, 46, 116, 120, 116, 72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33, 80, 75, 3, 4, 20, 3, 0, 0, 8, 0, 53, 181, 116, 72, 170, 192, 105, 58, 29, 0, 0, 0, 56, 7, 0, 0, 10, 0, 0, 0, 104, 101, 108, 108, 111, 50, 46, 116, 120, 116, 243, 72, 205, 201, 201, 87, 8, 207, 47, 202, 73, 81, 28, 101, 143, 178, 71, 217, 163, 236, 81, 246, 40, 123, 148, 141, 159, 13, 0, 80, 75, 1, 2, 63, 3, 10, 3, 0, 0, 0, 0, 73, 158, 116, 72, 163, 28, 41, 28, 12, 0, 0, 0, 12, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 128, 164, 129, 0, 0, 0, 0, 104, 101, 108, 108, 111, 46, 116, 120, 116, 80, 75, 1, 2, 63, 3, 20, 3, 0, 0, 8, 0, 53, 181, 116, 72, 170, 192, 105, 58, 29, 0, 0, 0, 56, 7, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 128, 164, 129, 51, 0, 0, 0, 104, 101, 108, 108, 111, 50, 46, 116, 120, 116, 80, 75, 5, 6, 0, 0, 0, 0, 2, 0, 2, 0, 111, 0, 0, 0, 120, 0, 0, 0, 0, 0]);
				lA11 = fA0;
				lA12 = new JA_B(lA11.length);
				lI13 = 0;
				_G = 1;
				continue;
			case 1:
				if (((lI13 >= lA11.length))) {
					_G = 2;
					continue;
				}
				lA12.data[lI13] = ((((lA11).data[lI13]))<<24>>24);
				lI13 = (((lI13 + 1))|0);
				_G = 1;
				continue;
			case 2:
				fA0 = (S[520]);
				tA294 = ((new Benchmark$39()));
				fA1 = tA294;
				(tA294)["Benchmark$39<init>([B)V"](lA12);
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[521]);
				tA295 = ((new Benchmark$40()));
				fA1 = tA295;
				(tA295)["Benchmark$40<init>([B)V"](lA12);
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				tA296 = ((new java_util_Random()));
				fA0 = tA296;
				(tA296)["java.util.Random<init>(J)V"](N.lnew(0, 0));
				lA13 = fA0;
				lA14 = (new JA_B(65536));
				lI15 = 0;
				_G = 3;
				continue;
			case 3:
				if (((lI15 >= lA14.length))) {
					_G = 4;
					continue;
				}
				(lA14).data[lI15] = (((lA13)["nextInt()I"]())<<24>>24);
				lI15 = (((lI15 + 1))|0);
				_G = 3;
				continue;
			case 4:
				fA0 = (S[522]);
				tA297 = ((new Benchmark$41()));
				fA1 = tA297;
				(tA297)["Benchmark$41<init>([B)V"]((lA14));
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[523]);
				tA298 = ((new Benchmark$42()));
				fA1 = tA298;
				(tA298)["Benchmark$42<init>([B)V"]((lA14));
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[524]);
				tA299 = ((new Benchmark$43()));
				fA1 = tA299;
				(tA299)["Benchmark$43<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (S[525]);
				tA300 = ((new Benchmark$44()));
				fA1 = tA300;
				(tA300)["Benchmark$44<init>()V"]();
				Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"]((fA0), (fA1));
				fA0 = (java_lang_System._out);
				tA301 = ((new java_lang_StringBuilder()));
				fA1 = tA301;
				(tA301)["java.lang.StringBuilder<init>()V"]();
				(fA0)["println(Ljava/lang/String;)V"]((fA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[526])["append(D)Ljava/lang/StringBuilder;"](Benchmark._totalTime)["toString()Ljava/lang/String;"]());
				return;
			default:
				break;
		}
	}
	return;
};
Benchmark["benchmark(Ljava/lang/String;LBenchmark$Task;)V"] = function(p0, p1) { 
	var _G = 0, lI4 = 0, lI6 = 0, fA0 = null, fA1 = null, lD4 = 0.0, lD6 = 0.0, lD8 = 0.0, tA0 = null;
	var J__exception__ = null;
	while (true) {
		try {
			while (true) {
				switch (_G) {
					case 0:
						fA0 = (java_lang_System._out);
						tA0 = ((new java_lang_StringBuilder()));
						fA1 = tA0;
						(tA0)["java.lang.StringBuilder<init>()V"]();
						(fA0)["print(Ljava/lang/String;)V"]((fA1)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](p0)["append(Ljava/lang/String;)Ljava/lang/StringBuilder;"](S[527])["toString()Ljava/lang/String;"]());
						java_lang_System._out["flush()V"]();
						_G = 1;
						continue;
					case 1:
						com_jtransc_JTranscSystem["stamp()D"]();
						lI4 = 0;
						_G = 2;
						continue;
					case 2:
						if (((lI4 >= 10))) {
							_G = 3;
							continue;
						}
						p1["run()I"]();
						lI4 = (((lI4 + 1))|0);
						_G = 2;
						continue;
					case 3:
						java_lang_System["gc()V"]();
						lD4 = com_jtransc_JTranscSystem["stamp()D"]();
						lI6 = 0;
						_G = 4;
						continue;
					case 4:
						if (((lI6 >= 10))) {
							_G = 5;
							continue;
						}
						p1["run()I"]();
						lI6 = (((lI6 + 1))|0);
						_G = 4;
						continue;
					case 5:
						lD6 = com_jtransc_JTranscSystem["stamp()D"]();
						lD8 = com_jtransc_JTranscSystem["elapsedTime(DD)D"](lD4, lD6);
						java_lang_System._out["println(D)V"](lD8);
						Benchmark._totalTime = +((Benchmark._totalTime + lD8));
						_G = 6;
						continue;
					case 6:
						_G = 7;
						continue;
					case 8:
						fA0 = (J__exception__);
						com_jtransc_io_JTranscConsole["log(Ljava/lang/Object;)V"]((S[528]));
						_G = 7;
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
			J__exception__ = J__i__exception__.javaThrowable || J__i__exception__;
			if (((((((_G >= 1)) && ((_G < 6)))) && (J__exception__ instanceof java_lang_Throwable)))) {
				_G = 8;
				continue;
			}
			throw J__i__exception__;
		}
	}
	return;
};
Benchmark["calc(II)I"] = function(p0, p1) { 
	return ((Math.imul((((p0 + p1))|0), (((p0 + p1))|0)))|0);
};
__createJavaArrays();
__buildStrings();
N.linit();
java_lang_Object.SI();
Benchmark.SI();
java_lang_String$CaseInsensitiveComparator.SI();
java_lang_String.SI();
java_io_Serializable.SI();
java_lang_Comparable.SI();
java_lang_CharSequence.SI();
java_util_Comparator.SI();
java_lang_String$1.SI();
java_lang_StringBuilder.SI();
java_lang_Appendable.SI();
java_lang_Class.SI();
java_lang_reflect_Type.SI();
java_lang_reflect_GenericDeclaration.SI();
java_lang_reflect_AnnotatedElement.SI();
java_lang_AnnotatedElement.SI();
java_lang_reflect_Modifier.SI();
java_lang_Number.SI();
java_lang_Integer.SI();
java_lang_reflect_AccessibleObject.SI();
java_lang_reflect_Field.SI();
java_lang_reflect_Member.SI();
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
java_lang_System$1.SI();
java_io_InputStream.SI();
com_jtransc_io_JTranscConsolePrintStream.SI();
com_jtransc_io_JTranscConsolePrintStream$ConsoleErrorStream.SI();
java_io_OutputStream.SI();
com_jtransc_io_JTranscConsolePrintStream$ConsoleBaseStream.SI();
com_jtransc_io_JTranscConsolePrintStream$ConsoleOutputStream.SI();
java_io_FilterOutputStream.SI();
java_lang_NullPointerException.SI();
java_lang_RuntimeException.SI();
java_io_PrintStream.SI();
java_lang_System.SI();
java_io_Closeable.SI();
java_lang_AutoCloseable.SI();
java_io_Flushable.SI();
java_lang_IndexOutOfBoundsException.SI();
java_lang_ArrayIndexOutOfBoundsException.SI();
java_lang_IllegalArgumentException.SI();
java_lang_Double.SI();
com_jtransc_ds_FastStringMap.SI();
java_lang_Long.SI();
com_jtransc_internal_JTranscCType.SI();
java_lang_Short.SI();
com_jtransc_io_JTranscConsole.SI();
java_lang_Boolean.SI();
java_lang_Byte.SI();
java_lang_reflect_MethodConstructor.SI();
java_lang_reflect_Method.SI();
j_MemberInfo.SI();
java_lang_reflect_MethodTypeImpl.SI();
com_jtransc_text_MStringReader.SI();
java_lang_Error.SI();
java_lang_reflect_ArrayType.SI();
java_util_AbstractCollection.SI();
java_util_AbstractList.SI();
java_util_ArrayList.SI();
java_util_List.SI();
java_util_Collection.SI();
java_lang_Iterable.SI();
java_util_RandomAccess.SI();
java_lang_Cloneable.SI();
java_util_Iterator.SI();
java_util_AbstractList$SimpleListIterator.SI();
java_util_ListIterator.SI();
java_util_AbstractList$FullListIterator.SI();
java_util_NoSuchElementException.SI();
java_util_ConcurrentModificationException.SI();
java_lang_UnsupportedOperationException.SI();
java_lang_jtransc_JTranscCoreReflection.SI();
j_ProgramReflection.SI();
j_ClassInfo.SI();
j_ProgramReflection$AllClasses.SI();
java_lang_reflect_ParameterizedTypeImpl.SI();
java_lang_reflect_ParameterizedType.SI();
java_lang_StackTraceElement.SI();
java_lang_SystemInt.SI();
java_util_Objects.SI();
java_lang_ClassCastException.SI();
java_util_Map.SI();
java_util_Set.SI();
java_util_Map$Entry.SI();
com_jtransc_JTranscWrapped.SI();
Benchmark$37.SI();
Benchmark$Task.SI();
Benchmark$MyClass2.SI();
Benchmark$36.SI();
Benchmark$35.SI();
Benchmark$34.SI();
com_jtransc_JTranscSystem.SI();
Benchmark$39.SI();
Benchmark$38.SI();
java_util_Random.SI();
Benchmark$33.SI();
Benchmark$32.SI();
Benchmark$31.SI();
Benchmark$30.SI();
java_lang_Runtime.SI();
Benchmark$40.SI();
Benchmark$1.SI();
Benchmark$2.SI();
Benchmark$3.SI();
Benchmark$44.SI();
Benchmark$4.SI();
Benchmark$43.SI();
Benchmark$5.SI();
Benchmark$42.SI();
Benchmark$6.SI();
Benchmark$41.SI();
Benchmark$7.SI();
Benchmark$8.SI();
Benchmark$9.SI();
Benchmark$15.SI();
Benchmark$14.SI();
Benchmark$13.SI();
Benchmark$12.SI();
Benchmark$19.SI();
Benchmark$18.SI();
Benchmark$17.SI();
Benchmark$16.SI();
Benchmark$11.SI();
Benchmark$10.SI();
Benchmark$26.SI();
Benchmark$25.SI();
Benchmark$24.SI();
Benchmark$23.SI();
Benchmark$29.SI();
Benchmark$28.SI();
Benchmark$27.SI();
Benchmark$22.SI();
Benchmark$21.SI();
Benchmark$20.SI();
com_jtransc_JTranscVersion.SI();
com_jtransc_time_JTranscClock$1.SI();
com_jtransc_time_JTranscClock$Impl.SI();
com_jtransc_time_JTranscClock.SI();
java_io_IOException.SI();
com_jtransc_charset_JTranscCharset.SI();
java_io_ByteArrayOutputStream.SI();
com_jtransc_charset_JTranscCharBuffer.SI();
java_nio_charset_UnsupportedCharsetException.SI();
java_util_ServiceLoader.SI();
com_jtransc_charset_JTranscCharsetSingleByte.SI();
com_jtransc_charset_charsets_JTranscCharsetIBM866.SI();
java_util_AbstractMap.SI();
java_util_HashMap$HashMapEntry.SI();
java_util_HashMap.SI();
java_util_HashMap$1.SI();
java_util_AbstractSet.SI();
java_util_HashMap$EntrySet.SI();
java_util_HashMap$HashIterator.SI();
java_util_HashMap$EntryIterator.SI();
java_lang_AssertionError.SI();
java_lang_CloneNotSupportedException.SI();
java_util_Collections$1.SI();
java_util_Collections$2.SI();
java_util_Collections$EmptyList.SI();
java_util_Collections$EmptySet.SI();
java_util_Collections$EmptyMap.SI();
java_util_Collections.SI();
java_util_Enumeration.SI();
com_jtransc_charset_charsets_JTranscCharsetUTF8.SI();
com_jtransc_charset_charsets_JTranscCharsetUSASCII.SI();
com_jtransc_charset_charsets_JTranscCharsetUTF16Base.SI();
com_jtransc_charset_charsets_JTranscCharsetUTF16LE.SI();
com_jtransc_JTranscBits.SI();
java_lang_Process.SI();
com_jtransc_JTranscProcess.SI();
com_jtransc_mix_JTranscProcessMulti$Creator.SI();
com_jtransc_mix_JTranscProcessMulti.SI();
com_jtransc_charset_charsets_JTranscCharsetLatin1.SI();
com_jtransc_charset_charsets_JTranscCharsetUTF16BE.SI();
java_util_Arrays$ArrayList.SI();
java_lang_reflect_Array.SI();
java_lang_SafeVarargs.SI();
java_lang_annotation_Annotation.SI();
java_lang_Thread.SI();
java_lang_Runnable.SI();
java_lang_ThreadGroup.SI();
java_lang_Thread$UncaughtExceptionHandler.SI();
java_lang_ClassLoader.SI();
java_lang__ClassInternalUtils.SI();
java_lang__ClassInternalUtils$1.SI();
com_jtransc_JTranscArrays.SI();
com_jtransc_JTranscSystemProperties.SI();
Benchmark$MyClass.SI();
com_jtransc_FastMemory.SI();
java_nio_Buffer.SI();
java_nio_ByteBuffer.SI();
java_nio_internal_MemoryBlock.SI();
java_nio_ReadOnlyBufferException.SI();
java_util_zip_CRC32.SI();
java_util_zip_Checksum.SI();
com_jtransc_compression_jzlib_CRC32.SI();
com_jtransc_compression_jzlib_Checksum.SI();
java_nio_ByteOrder.SI();
java_nio_DoubleBuffer.SI();
java_nio_LongBuffer.SI();
java_nio_ByteBufferAsLongBuffer.SI();
java_nio_internal_ByteBufferAs.SI();
java_nio_ByteBufferAsLongBuffer$LE.SI();
java_nio_ByteBufferAsLongBuffer$1.SI();
java_nio_ByteBufferAsLongBuffer$BE.SI();
java_nio_ByteBufferAsDoubleBuffer.SI();
java_nio_ByteBufferAsDoubleBuffer$LE.SI();
java_nio_ByteBufferAsDoubleBuffer$1.SI();
java_nio_ByteBufferAsDoubleBuffer$BE.SI();
libcore_io_Memory.SI();
java_nio_CharBuffer.SI();
java_lang_Readable.SI();
java_nio_ShortBuffer.SI();
java_nio_ByteBufferAsShortBuffer.SI();
java_nio_ByteBufferAsShortBuffer$LE.SI();
java_nio_ByteBufferAsShortBuffer$1.SI();
java_nio_ByteBufferAsShortBuffer$BE.SI();
java_nio_ByteBufferAsCharBuffer.SI();
java_nio_ByteBufferAsCharBuffer$LE.SI();
java_nio_ByteBufferAsCharBuffer$1.SI();
java_nio_ByteBufferAsCharBuffer$BE.SI();
java_nio_IntBuffer.SI();
java_nio_FloatBuffer.SI();
java_nio_ByteBufferAsFloatBuffer.SI();
java_nio_ByteBufferAsFloatBuffer$BE.SI();
java_nio_ByteBufferAsFloatBuffer$LE.SI();
java_nio_ByteBufferAsIntBuffer.SI();
java_nio_ByteBufferAsIntBuffer$BE.SI();
java_nio_ByteBufferAsIntBuffer$LE.SI();
com_jtransc_compression_jzlib_ZStream.SI();
com_jtransc_compression_jzlib_Deflater.SI();
com_jtransc_compression_jzlib_Deflate$Config.SI();
com_jtransc_compression_jzlib_Deflate.SI();
com_jtransc_compression_jzlib_Tree.SI();
com_jtransc_compression_jzlib_GZIPHeader.SI();
com_jtransc_compression_jzlib_StaticTree.SI();
com_jtransc_compression_jzlib_GZIPException.SI();
com_jtransc_compression_jzlib_Adler32.SI();
java_util_zip_Deflater.SI();
Benchmark$Test1.SI();
Benchmark$Test2.SI();
com_jtransc_simd_Simd.SI();
com_jtransc_simd_Float32x4.SI();
com_jtransc_simd_MutableFloat32x4.SI();
com_jtransc_simd_MutableFloat32x4Utils.SI();
com_jtransc_simd_MutableMatrixFloat32x4x4.SI();
com_jtransc_simd_MutableMatrixFloat32x4x4Utils.SI();
java_util_Dictionary.SI();
java_util_Hashtable$HashtableEntry.SI();
java_io_ObjectStreamField.SI();
java_lang_ref_WeakReference.SI();
java_lang_ref_Reference.SI();
java_util_Hashtable.SI();
java_util_Properties.SI();
java_util_Hashtable$1.SI();
java_util_Hashtable$EntrySet.SI();
java_util_Hashtable$HashIterator.SI();
java_util_Hashtable$EntryIterator.SI();
java_lang_ref_ReferenceQueue.SI();
java_lang_SafeVarargs$Impl.SI();
java_lang_annotation_Annotation$Impl.SI();
try {
	Benchmark["main([Ljava/lang/String;)V"](N.strArray(N.args()));
} catch (e) {
	console.error(e);
	console.error(e.stack);
}


})(_global);

