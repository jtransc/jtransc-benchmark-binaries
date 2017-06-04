<?php

// JTransc 0.6.3-snapshot : https://github.com/jtransc/jtransc

// php program.php
// hhvm -v Eval.EnableHipHopSyntax=true program.php
if (defined('HHVM_VERSION')) {
	// @TODO: Check minimum HHVM_VERSION
} else {
	if (version_compare(phpversion(), '7.1.0', '<')) die('Requires PHP 7.1 or higher but was ' . phpversion());
}

ob_implicit_flush(true);

// http://hhvm.com/blog/713/hhvm-optimization-tips

set_error_handler('exceptions_error_handler');

function exceptions_error_handler($severity, $message, $filename, $lineno) {
  //if (error_reporting() == 0) return;
  //if (error_reporting() & $severity) {
    throw new ErrorException($message, 0, $severity, $filename, $lineno);
  //}
}

const PHP_INT_BITS_SIZE = PHP_INT_SIZE * 8;
const PHP_INT_BITS_SIZE_M8 = PHP_INT_BITS_SIZE - 8;
const PHP_INT_BITS_SIZE_M16 = PHP_INT_BITS_SIZE - 16;
const PHP_INT_BITS_SIZE_M24 = PHP_INT_BITS_SIZE - 24;
const PHP_INT_BITS_SIZE_M32 = PHP_INT_BITS_SIZE - 32;

final class N {
	static public $DOUBLE_NAN;
	static public $DOUBLE_NEGATIVE_INFINITY;
	static public $DOUBLE_POSITIVE_INFINITY;

	static public $FLOAT_NAN;
	static public $FLOAT_NEGATIVE_INFINITY;
	static public $FLOAT_POSITIVE_INFINITY;

	const MIN_INT32 = -2147483648;
	const MAX_INT32 = 2147483647;

	static function init() {
	}

	static public function utf16_to_utf8(string $str) : string {
		return mb_convert_encoding($str, 'UTF-8', 'UTF-16LE');
	}

	static public function utf8_to_utf16(string $str) : string {
		return mb_convert_encoding($str, 'UTF-16LE', 'UTF-8');
	}

	static public function FIXSHIFT(int $r) : int {
		if ($r < 0) {
			return (32 - ((-$r) & 0x1F)) & 0x1F;
		} else {
			return $r & 0x1F;
		}
	}

	static public function LFIXSHIFT(int $r) : int {
		if ($r < 0) {
			return (64 - ((-$r) & 0x3F)) & 0x3F;
		} else {
			return $r & 0x3F;
		}
	}

	static public function ishl(int $a, int $b) : int { return $a << N::FIXSHIFT($b); }
	static public function ishr(int $a, int $b) : int { return $a >> N::FIXSHIFT($b); }

	static public function iushr(int $a, int $b) : int {
	    $b = N::FIXSHIFT($b);
	    if ($b == 0) return $a;
        return ($a >> $b) & ~(1 << (PHP_INT_BITS_SIZE - 1) >> ($b - 1));
	}

	static public function irem(int $a, int $b) : int { return $a % $b; }
	static public function imul(int $a, int $b) : int { return Int32::mul($a, $b); }
	static public function idiv(int $a, int $b) : int {
		if ($a == PHP_INT_MIN && $b == -1) return -2147483648;
		return intdiv($a, $b);
	}

	static public function i(int $v) { return ($v << PHP_INT_BITS_SIZE_M32) >> PHP_INT_BITS_SIZE_M32; }

	static public function i2j(int $v) : Int64 { return Int64::ofInt($v); }
	static public function j2i(Int64 $v) : int { return $v->low; }
	static public function z2i($v) : int { return $v ? 1 : 0; }
	static public function i2c(int $v) : int { return $v & 0xFFFF; }
	static public function i2b(int $v) : int { return (($v & 0xFF) << (PHP_INT_BITS_SIZE_M8)) >> PHP_INT_BITS_SIZE_M8; }
	static public function i2s(int $v) : int { return (($v & 0xFFFF) << (PHP_INT_BITS_SIZE_M16)) >> PHP_INT_BITS_SIZE_M16; }
	static public function d2j(float $v) : Int64 { return Int64::ofFloat($v); }
	static public function j2d(Int64 $v) : float { return Int64::toFloat($v); }

	static public function sx8(int $v) : int { return ($v << PHP_INT_BITS_SIZE_M8) >> PHP_INT_BITS_SIZE_M8; }
	static public function sx16(int $v) : int { return ($v << PHP_INT_BITS_SIZE_M16) >> PHP_INT_BITS_SIZE_M16; }
	static public function sx32(int $v) : int { return ($v << PHP_INT_BITS_SIZE_M32) >> PHP_INT_BITS_SIZE_M32; }

	static function lnew (int   $h, int   $l) : Int64 { return Int64::make($h, $l); }
	static function lneg (Int64 $l) : Int64 { return Int64::neg($l); }
	static function lsub (Int64 $l, Int64 $r) : Int64 { return Int64::sub($l, $r); }
	static function ladd (Int64 $l, Int64 $r) : Int64 { return Int64::add($l, $r); }
	static function lmul (Int64 $l, Int64 $r) : Int64 { return Int64::mul($l, $r); }
	static function ldiv (Int64 $l, Int64 $r) : Int64 { return Int64::div($l, $r); }
	static function lrem (Int64 $l, Int64 $r) : Int64 { return Int64::rem($l, $r); }
	static function lshl (Int64 $l, int   $r) : Int64 { return Int64::shl($l, $r); }
	static function lshr (Int64 $l, int   $r) : Int64 { return Int64::shr($l, $r); }
	static function lushr(Int64 $l, int   $r) : Int64 { return Int64::ushr($l, $r); }
	static function land (Int64 $l, Int64 $r) : Int64 { return Int64::and($l, $r); }
	static function lxor (Int64 $l, Int64 $r) : Int64 { return Int64::xor($l, $r); }
	static function lor  (Int64 $l, Int64 $r) : Int64 { return Int64::or($l, $r); }

	static function lcmp (Int64 $l, Int64 $r) : int   { return Int64::compare($l, $r); }

	static function cmp ($a, $b) { return ($a < $b) ? (-1) : (($a > $b) ? (1) : 0); }
	static function cmpl($a, $b) { return (is_nan($a) || is_nan($b)) ? (-1) : N::cmp($a, $b); }
	static function cmpg($a, $b) { return (is_nan($a) || is_nan($b)) ? (1) : N::cmp($a, $b); }

	static function monitorEnter($v) { }
	static function monitorExit($v) { }

	static function resolveClass(string $name) {
		return java_lang_Class::forName_Ljava_lang_String__Ljava_lang_Class_(N::str($name));
	}

	static public function getTime() : float {
		return (microtime(true) * 1000);
	}

	static public function nanoTime() : Int64 {
		return N::lmul(N::d2j((microtime(true) * 1000000)), Int64::ofInt(1000));
	}

	static function str(string $str): java_lang_String {
		// UTF-8 string
		$rstr = N::utf8_to_utf16($str);
		$len = (int)(strlen($rstr) / 2);
		$chars = new JA_C($len);
		$chars->data = TypedBuffer::fromString($rstr);
		//for ($n = 0; $n < $len; $n++) {
		//	$low = ord($rstr[$n * 2 + 0]) & 0xFF;
		//	$high = ord($rstr[$n * 2 + 1]) & 0xFF;
		//	$chars->set($n, ($high << 8) | $low);
		//}
		return (new java_lang_String())->java_lang_String_init___C_V($chars);
	}

	static function istr(java_lang_String $str) : string {
		if ($str == null) return null;
		//$out = '';
		//$len = $str->length__I();
		//for ($n = 0; $n < $len; $n++) $out .= chr($str->charAt_I_C($n));
		//return N::utf16_to_utf8($out);
		if ($str->_str == null) {
			$str->_str = N::utf16_to_utf8($str->_value->data->data);
		}
		return $str->_str;
	}

	static public function strArray(array $array) : JA_L {
		return JA_L::fromArray(array_map(function($v) { return N::str($v); }, $array), '[Ljava/lang/String;');
	}

	static public function arraycopy(java_lang_Object $src, int $srcPos, java_lang_Object $dst, int $dstPos, int $len) : void {
		if ($src instanceof JA_Typed) {
			$esize = $src->esize;
			$src->data->copyTo($dst->data, $srcPos * $esize, $dstPos * $esize, $len * $esize);
			//echo "$srcPos, $dstPos, $len, $esize\n";
		} else if ($src instanceof JA_Array) {
			$overlapping = ($src === $dst && $dstPos > $srcPos);
			if ($overlapping) {
				$n = $len;
				while (--$n >= 0) $dst->set($dstPos + $n, $src->get($srcPos + $n));
			} else {
				for ($n = 0; $n < $len; $n++) $dst->set($dstPos + $n, $src->get($srcPos + $n));
			}
		} else {
			throw new Exception("Invalid array");
		}
	}

	static public $tempBuffer;

	static public function longBitsToDouble(Int64 $v) : float {
		N::$tempBuffer->set32(0, $v->low);
		N::$tempBuffer->set32(4, $v->high);
		return N::$tempBuffer->getF64(0);
	}

	static public function doubleToLongBits(float $v) : Int64 {
		N::$tempBuffer->setF64(0, $v);
		$low = N::$tempBuffer->getS32(0);
		$high = N::$tempBuffer->getS32(4);
		return Int64::make($high, $low);
	}


	static public function intBitsToFloat(int $v) : float {
		N::$tempBuffer->set32(0, $v);
		return N::$tempBuffer->getF32(0);
	}

	static public function floatToIntBits(float $v) : int {
		N::$tempBuffer->setF32(0, $v);
		return N::$tempBuffer->getS32(0);
	}

	static public function  unboxBool  (java_lang_Boolean   $i) : bool   { return $i->booleanValue__Z(); }
	static public function  unboxByte  (java_lang_Byte      $i) : int    { return $i->byteValue__B(); }
	static public function  unboxShort (java_lang_Short     $i) : int    { return $i->shortValue__S(); }
	static public function  unboxChar  (java_lang_Character $i) : int    { return $i->charValue__C(); }
	static public function  unboxInt   (java_lang_Integer   $i) : int    { return $i->intValue__I(); }
	static public function  unboxLong  (java_lang_Long      $i) : Int64  { return $i->longValue__J(); }
	static public function  unboxFloat (java_lang_Float     $i) : float  { return $i->floatValue__F(); }
	static public function  unboxDouble(java_lang_Double    $i) : float  { return $i->doubleValue__D(); }

	static public function boxVoid  (         ) : java_lang_Object    { return null; }
	static public function boxBool  (bool   $v) : java_lang_Boolean   { return java_lang_Boolean::valueOf_Z_Ljava_lang_Boolean_($v); }
	static public function boxByte  (int    $v) : java_lang_Byte      { return java_lang_Byte::valueOf_B_Ljava_lang_Byte_($v); }
	static public function boxShort (int    $v) : java_lang_Short     { return java_lang_Short::valueOf_S_Ljava_lang_Short_($v); }
	static public function boxChar  (int    $v) : java_lang_Character { return java_lang_Character::valueOf_C_Ljava_lang_Character_($v); }
	static public function boxInt   (int    $v) : java_lang_Integer   { return java_lang_Integer::valueOf_I_Ljava_lang_Integer_($v); }
	static public function boxLong  (Int64  $v) : java_lang_Long      { return java_lang_Long::valueOf_J_Ljava_lang_Long_($v); }
	static public function boxFloat (float  $v) : java_lang_Float     { return java_lang_Float::valueOf_F_Ljava_lang_Float_($v); }
	static public function boxDouble(float  $v) : java_lang_Double    { return java_lang_Double::valueOf_D_Ljava_lang_Double_($v); }

	static public function fillSecureRandomBytes(TypedBuffer $buffer) {
		$buffer->putBytes(random_bytes($buffer->length), 0);
	}

	static public function checkcast($v, string $classname) {
		if ($v == null) return null;
		if (!is_a($v, $classname)) {
			throw new WrappedThrowable((new java_lang_ClassCastException())->java_lang_ClassCastException_init__Ljava_lang_String__V(N::str("Class cast error. Object '$classname'")));
		}
		return $v;
	}
}

N::$tempBuffer = TypedBuffer::alloc(16);

N::$DOUBLE_NAN = N::longBitsToDouble(Int64::make((int)0x7FF80000, (int)0x00000000));
N::$DOUBLE_NEGATIVE_INFINITY = -INF;
N::$DOUBLE_POSITIVE_INFINITY = +INF;

N::$FLOAT_NAN = N::intBitsToFloat((int)0x7FC00000);
N::$FLOAT_NEGATIVE_INFINITY = -INF;
N::$FLOAT_POSITIVE_INFINITY = +INF;

// @TODO: Critical Performance. All arrays uses this. So this must be as fast as possible. Specially aligned* methods.
final class TypedBuffer {
	public $length = 0;
	public $data = null;

	public function __construct(string $data) { $this->data = $data; $this->length = strlen($data); }

	static public function alloc(int $size) { return new TypedBuffer(str_repeat(chr(0), $size)); }
	static public function allocRepeat(string $base, int $size) { return new TypedBuffer(str_repeat($base, $size)); }
	static public function fromString(string $data) { return new TypedBuffer($data); }

	public function getAllBytes() { return $this->data; }
	public function getRangeBytes(int $start, int $len) { return substr($this->data, $start, $len); }

	public function putBytes(string $bytes, int $offset) : void {
		$len = strlen($bytes);
		for ($n = 0; $n < $len; $n++) $this->data[$offset + $n] = $bytes[$n];
	}

	public function copyTo(TypedBuffer $dstBuffer, int $srcPos, int $dstPos, int $len) : void {
		$overlapping = ($this === $dstBuffer && $dstPos > $srcPos);
		if ($overlapping) {
			$n = $len;
			while (--$n >= 0) $dstBuffer->data[$dstPos + $n] = $this->data[$srcPos + $n];
		} else {
			for ($n = 0; $n < $len; $n++) $dstBuffer->data[$dstPos + $n] = $this->data[$srcPos + $n];
		}
	}

	public function checkIndex(int $n) { if ($n > $this->length) throw new Exception("Index out of bounds $n of {$this->length}"); }

	public function getU8 (int $n) : int   { $this->checkIndex($n + 1); return (ord($this->data[$n]) & 0xFF); }
	public function getU16(int $n) : int   { $this->checkIndex($n + 2); return (ord($this->data[$n + 0]) | (ord($this->data[$n + 1]) << 8)) & 0xFFFF; }
	public function getU32(int $n) : int   { $this->checkIndex($n + 4); return (ord($this->data[$n + 0]) | (ord($this->data[$n + 1]) << 8) | (ord($this->data[$n + 2]) << 16) | (ord($this->data[$n + 3]) << 24)); }
	public function getF32(int $n) : float { $this->checkIndex($n + 4); return unpack("f", substr($this->data, $n, 4))[1]; }
	public function getF64(int $n) : float { $this->checkIndex($n + 8); return unpack("d", substr($this->data, $n, 8))[1]; }

	public function set8 (int $n, int $v) : void { $this->data[$n + 0] = chr(($v >> 0) & 0xFF); }
	public function set16(int $n, int $v) : void { $this->data[$n + 0] = chr(($v >> 0) & 0xFF); $this->data[$n + 1] = chr(($v >> 8) & 0xFF); }
	public function set32(int $n, int $v) : void { $this->data[$n + 0] = chr(($v >> 0) & 0xFF); $this->data[$n + 1] = chr(($v >> 8) & 0xFF); $this->data[$n + 2] = chr(($v >> 16) & 0xFF); $this->data[$n + 3] = chr(($v >> 24) & 0xFF); }
	public function setF32(int $n, float $v) : void { $s = pack('f', $v); for ($m = 0; $m < 4; $m++) $this->data[$n + $m] = $s[$m]; }
	public function setF64(int $n, float $v) : void { $s = pack('d', $v); for ($m = 0; $m < 8; $m++) $this->data[$n + $m] = $s[$m]; }

	public function getS8 (int $n) : int { return N::sx8($this->getU8($n)); }
	public function getS16(int $n) : int { return N::sx16($this->getU16($n)); }
	public function getS32(int $n) : int { return (int)$this->getU32($n); }

	// @TODO: Best performance required:
	public function alignedgetU8 (int $n) : int   { return $this->getU8 ($n * 1); }
	public function alignedgetU16(int $n) : int   { return $this->getU16($n * 2); }
	public function alignedgetU32(int $n) : int   { return $this->getU32($n * 4); }
	public function alignedgetS8 (int $n) : int   { return $this->getS8 ($n * 1); }
	public function alignedgetS16(int $n) : int   { return $this->getS16($n * 2); }
	public function alignedgetS32(int $n) : int   { return $this->getS32($n * 4); }
	public function alignedgetF32(int $n) : float { return $this->getF32($n * 4); }
	public function alignedgetF64(int $n) : float { return $this->getF64($n * 8); }
	public function alignedset8  (int $n, int   $v) : void { $this->set8  ($n * 1, $v); }
	public function alignedset16 (int $n, int   $v) : void { $this->set16 ($n * 2, $v); }
	public function alignedset32 (int $n, int   $v) : void { $this->set32 ($n * 4, $v); }
	public function alignedsetF32(int $n, float $v) : void { $this->setF32($n * 4, $v); }
	public function alignedsetF64(int $n, float $v) : void { $this->setF64($n * 8, $v); }
}

abstract class JA_0 extends java_lang_Object {
	public $length = 0;
	public $desc = '';

	public function __construct(int $length, string $desc) {
		$this->length = $length;
		$this->desc = $desc;
	}

	public function __toString() {
		return $this->desc;
	}
}

abstract class JA_Typed extends JA_0 {
	public $data = null;
	public $esize = 0;

	public function __construct(int $length, int $esize, string $repeat, string $desc) {
		parent::__construct($length, $desc);
		$this->esize = $esize;
		$this->data = TypedBuffer::allocRepeat($repeat, $length);
	}
}

class JA_Array extends JA_0 {
	public $data = null;

	public function __construct(int $length, string $desc, $default = null) {
		parent::__construct($length, $desc);
		$this->data = array_fill(0, $length, $default);
	}

	public function set(int $index, $value) : void { $this->data[$index] = $value; }
	public function get(int $index) { return $this->data[$index]; }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////

class JA_B extends JA_Typed {
	public function __construct($length, $desc = '[B') { parent::__construct($length, 1, "\0", $desc); }
	public function set(int $index, int $value) : void { $this->data->alignedset8($index, $value); }
	public function get(int $index) : int { return $this->data->alignedgetS8($index); }
}

class JA_Z extends JA_B {
	public function __construct(int $length) { parent::__construct($length, '[Z'); }
	public function set(int $index, int $value) : void { $this->data->alignedset8($index, (int)$value); }
	public function get(int $index) : int { return (boolean)$this->data->alignedgetS8($index); }
}

final class JA_C extends JA_Typed {
	public function __construct(int $length) { parent::__construct($length, 2, "\0\0", '[C'); }
	public function set(int $index, int $value) : void { $this->data->alignedset16($index, $value); }
	public function get(int $index) : int { return $this->data->alignedgetU16($index); }
}

final class JA_S extends JA_Typed {
	public function __construct(int $length) { parent::__construct($length, 2, "\0\0", '[S'); }
	public function set(int $index, int $value) : void { $this->data->alignedset16($index, $value); }
	public function get(int $index) : int { return $this->data->alignedgetS16($index); }
}

final class JA_I extends JA_Typed {
	public function __construct(int $length) { parent::__construct($length, 4, "\0\0\0\0", '[I'); }

	static public function T($array) {
		$len = count($array);
		$out = new JA_I($len);
		for ($n = 0; $n < $len; $n++) $out->set($n, $array[$n]);
		return $out;
	}

	public function set(int $index, int $value) : void { $this->data->alignedset32($index, $value); }
	public function get(int $index) : int { return $this->data->alignedgetS32($index); }
}

final class JA_F extends JA_Typed {
	public function __construct(int $length) { parent::__construct($length, 4, pack('f', 0.0), '[F'); }
	public function set(int $index, float $value) : void { $this->data->alignedsetF32($index, $value); }
	public function get(int $index) : float { return $this->data->alignedgetF32($index); }
}

final class JA_D extends JA_Typed {
	public function __construct(int $length) { parent::__construct($length, 8, pack('d', 0.0), '[D'); }
	public function set(int $index, float $value) : void { $this->data->alignedsetF64($index, $value); }
	public function get(int $index) : float { return $this->data->alignedgetF64($index); }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////

final class JA_J extends JA_Array {
	public function __construct(int $length) { parent::__construct($length, '[J', Int64::$zero); }
}

final class JA_L extends JA_Array {
	public function __construct(int $length, string $desc) { parent::__construct($length, $desc, null); }

	static function fromArray(array $items, string $desc) : JA_L {
		$count = count($items);
		$out = new JA_L($count, $desc);
		for ($n = 0; $n < $count; $n++) $out->set($n, $items[$n]);
		return $out;
	}

	static function createMultiSure(string $desc, array $sizes) : JA_0 {
		return JA_L::_createMultiSure($desc, 0, $sizes);
	}

	static function _createMultiSure(string $desc, int $index, array $sizes) : JA_0 {
		if (substr($desc, 0, 1) != "[") return null;
		if ($index >= count($sizes) - 1) return JA_L::create($sizes[$index], $desc);
		$len = $sizes[$index];
		$o = new JA_L($len, $desc);
		$desc2 = substr($desc, 1);
		for ($n = 0; $n < $len; $n++) {
			$o->data[$n] = JA_L::_createMultiSure($desc2, $index + 1, $sizes);
		}
		return $o;
	}

	static function create(int $size, string $desc) {
		switch ($desc) {
			case "[Z": return new JA_Z($size);
			case "[B": return new JA_B($size);
			case "[C": return new JA_C($size);
			case "[S": return new JA_S($size);
			case "[I": return new JA_I($size);
			case "[J": return new JA_J($size);
			case "[F": return new JA_F($size);
			case "[D": return new JA_D($size);
			default: return new JA_L($size, $desc);
		}
	}
}

class WrappedThrowable extends Exception {
	public $t;

	public function __construct($t) { $this->t = $t; }

	//public function __toString() { return '' . $this->t; }
}

final class DivModResult {
	public $quotient;
	public $modulus;

	public function __construct(Int64 $quotient, Int64 $modulus) {
		$this->quotient = $quotient;
		$this->modulus = $modulus;
	}
}

final class Int32 {
	static public function compare(int $a, int $b): int {
		$a |= 0;
		$b |= 0;
		if ($a == $b) {
			return 0;
		} else if ($a > $b) {
			return 1;
		} else {
			return -1;
		}
	}

	static public function ucompare(int $a, int $b): int {
		if ($a < 0) {
			if($b < 0) {
				return ~$b - ~$a | 0;
			} else {
				return 1;
			}
		}
		if ($b < 0) {
			return -1;
		} else {
			return $a - $b | 0;
		}
	}

	static public function mul(int $a, int $b) : int {
		if (PHP_INT_SIZE == 4) {
			$ah = ($a >> 16) & 0xffff;
			$al = $a & 0xffff;
			$bh = ($b >> 16) & 0xffff;
			$bl = $b & 0xffff;
			// the shift by 0 fixes the sign on the high part
			// the final |0 converts the unsigned value into a signed value
			return (($al * $bl) + ((($ah * $bl + $al * $bh) << 16))|0);
		} else {
			return ($a * $b)|0;
		}
	}
}

final class Int64 {
	static public $MAX_INT64;
	static public $MIN_INT64;
	static public $zero;
	static public $one;
	static public $MIN_VALUE;
	static public $MAX_VALUE;

	public $high = 0;
	public $low = 0;

	public function __construct(int $high, int $low) {
		$this->high = $high;
		$this->low = $low;
		//var_dump($high);
		//var_dump($low);
	}

	static public function make(int $high, int $low) : Int64 {
		if ($high == 0) {
			if ($low == 0) return Int64::$zero;
			if ($low == 1) return Int64::$one;
		}
		return new Int64($high, $low);
	}

	static public function ofInt(int $value): Int64 {
		return Int64::make($value >> 31, $value);
	}

	static public function ofFloat(float $f) : Int64 {
		if (is_nan($f) || !is_finite($f)) throw new Exception("Number is NaN or Infinite");
		$noFractions = $f - ($f % 1);
		// 2^53-1 and -2^53: these are parseable without loss of precision
		if ($noFractions > 9007199254740991.0) throw new Exception("Conversion overflow");
		if ($noFractions < -9007199254740991.0) throw new Exception("Conversion underflow");

		$result = Int64::ofInt(0);
		$neg = $noFractions < 0;
		$rest = $neg ? -$noFractions : $noFractions;

		$i = 0;
		while ($rest >= 1) {
			$curr = $rest % 2;
			$rest = $rest / 2;
			if ($curr >= 1) $result = Int64::add($result, Int64::shl(Int64::ofInt(1), $i));
			$i++;
		}

		return $neg ? Int64::neg($result) : $result;
	}

	static public function ofString(string $sParam) : Int64 {
		$base = Int64::ofInt(10);
		$current = Int64::ofInt(0);
		$multiplier = Int64::ofInt(1);
		$sIsNegative = false;

		$s = trim($sParam);
		if ($s->charAt(0) == '-') {
			$sIsNegative = true;
			$s = substr($s, 1, strlen($s));
		}
		$len = strlen(s);

		for ($i = 0; $i < $len; $i++) {
			$digitInt = intval(substr($s, $len - 1 - $i, 1));

			$digit = Int64::ofInt($digitInt);
			if ($sIsNegative) {
				$current = Int64::sub($current, Int64::mul($multiplier, $digit));
				if (!Int64::isNeg($current)) throw new Exception("NumberFormatError: Underflow");
			} else {
				$current = Int64::add($current, Int64::mul($multiplier, $digit));
				if (Int64::isNeg($current)) throw new Exception("NumberFormatError: Overflow");
			}
			$multiplier = Int64::mul($multiplier, $base);
		}
		return $current;
	}

	static public function toInt(Int64 $a) : int {
		return $a->low;
	}

	static public function toFloat(Int64 $v) : float {
		if (Int64::isNeg($v)) {
			return Int64::eq($v, Int64::$MIN_INT64) ? -9223372036854775808.0 : -Int64::toFloat(Int64::neg($v));
		} else {
			$lowf = $v->low;
			$highf = $v->high;
			return $lowf + $highf * pow(2, 32);
		}
	}

	static public function isNeg(Int64 $a) : bool { return $a->high < 0; }
	static public function isZero(Int64 $a) : bool { return $a->high == 0 && $a->low == 0; }
	static public function isNotZero(Int64 $a) : bool { return $a->high != 0 || $a->low != 0; }

// Comparisons

	static private function Integer_compare(int $a, int $b) : int { return Int32::compare($a, $b); }
	static private function Integer_compareUnsigned(int $a, int $b) : int { return Int32::ucompare($a, $b); }

	static public function compare(Int64 $a, Int64 $b) : int {
		$v = $a->high - $b->high;
		if ($v == 0) $v = Int64::Integer_compareUnsigned($a->low, $b->low);
		return ($a->high < 0) ? (($b->high < 0) ? $v : -1) : (($b->high >= 0) ? $v : 1);
	}

	static public function ucompare(Int64 $a, Int64 $b) : int {
		$v = Int64::Integer_compareUnsigned($a->high, $b->high);
		return ($v != 0) ? $v : Int64::Integer_compareUnsigned($a->low, $b->low);
	}

	static public function eq(Int64 $a, Int64 $b) : bool { return ($a->high == $b->high) && ($a->low == $b->low); }
	static public function ne(Int64 $a, Int64 $b) : bool { return ($a->high != $b->high) || ($a->low != $b->low); }
	static public function lt(Int64 $a, Int64 $b) : bool { return Int64::compare($a, $b) < 0; }
	static public function le(Int64 $a, Int64 $b) : bool { return Int64::compare($a, $b) <= 0; }
	static public function gt(Int64 $a, Int64 $b) : bool { return Int64::compare($a, $b) > 0; }
	static public function ge(Int64 $a, Int64 $b) : bool { return Int64::compare($a, $b) >= 0; }

	// Strings
	public function toString(): string {
		$i = $this;
		if (Int64::isZero($i)) return "0";
		$str = "";
		$neg = false;
		if (Int64::isNeg($i)) {
			$neg = true;
			// i = -i; cannot negate here as --9223372036854775808 = -9223372036854775808
		}
		$ten = Int64::ofInt(10);
		while (Int64::isNotZero(i)) {
			$r = Int64::divMod($i, $ten);
			if (Int64::isNeg($r->modulus)) {
				$str = Int64::neg($r->modulus)->low + $str;
				$i = Int64::neg($r->quotient);
			} else {
				$str = $r->modulus->low + $str;
				$i = $r->quotient;
			}
		}
		if ($neg) $str = "-$str";
		return $str;
	}

	static public function divMod(Int64 $dividend, Int64 $divisor) : DivModResult {
		if ($divisor->high == 0) {
			switch ($divisor->low) {
				case 0: throw new Exception("divide by zero");
				case 1: return new DivModResult(Int64::make($dividend->high, $dividend->low), Int64::ofInt(0));
			}
		}
		$divSign = Int64::isNeg($dividend) != Int64::isNeg($divisor);
		$modulus = Int64::isNeg($dividend) ? Int64::neg($dividend) : Int64::make($dividend->high, $dividend->low);
		$divisor = Int64::abs($divisor);

		$quotient = Int64::ofInt(0);
		$mask = Int64::ofInt(1);
		while (!Int64::isNeg($divisor)) {
			$cmp = Int64::ucompare($divisor, $modulus);
			$divisor = Int64::shl($divisor, 1);
			$mask = Int64::shl($mask, 1);
			if ($cmp >= 0) break;
		}
		while (Int64::ne($mask, Int64::ofInt(0))) {
			if (Int64::ucompare($modulus, $divisor) >= 0) {
				$quotient = Int64::or($quotient, $mask);
				$modulus = Int64::sub($modulus, $divisor);
			}
			$mask = Int64::ushr($mask, 1);
			$divisor = Int64::ushr($divisor, 1);
		}
		if ($divSign) $quotient = Int64::neg($quotient);
		if (Int64::isNeg($dividend)) $modulus = Int64::neg($modulus);
		return new DivModResult($quotient, $modulus);
	}

	static public function neg(Int64 $x): Int64 {
		$high = (~$x->high)|0;
		$low = (-$x->low)|0;
		if ($low == 0) $high = ($high + 1)|0;
		return Int64::make($high, $low);
	}

	static public function add(Int64 $a, Int64 $b): Int64 {
		$high = ($a->high + $b->high)|0;
		$low  = ($a->low + $b->low)|0;
		if (Int64::Integer_compareUnsigned($low, $a->low) < 0) {
			$high = ($high + 1)|0;
		}
		return Int64::make($high, $low);
	}

	static public function sub(Int64 $a, Int64 $b) : Int64 {
		$high = ($a->high - $b->high)|0;
		$low = ($a->low - $b->low)|0;
		if (Int64::Integer_compareUnsigned($a->low, $b->low) < 0) {
			$high = ($high - 1)|0;
		}
		return Int64::make($high, $low);
	}

	static public function mul(Int64 $a, Int64 $b) : Int64 {
		$al = $a->low & 65535;
		$ah = N::iushr($a->low, 16);
		$bl = $b->low & 65535;
		$bh = N::iushr($b->low, 16);
		$p00 = Int32::mul($al, $bl);
		$p10 = Int32::mul($ah, $bl);
		$p01 = Int32::mul($al, $bh);
		$p11 = Int32::mul($ah, $bh);
		$low = $p00;
		$high = ($p11 + N::iushr($p01, 16) | 0) + N::iushr($p10, 16) | 0;
		$p01 = $p01 << 16;
		$low = $p00 + $p01 | 0;
		if (Int32::ucompare($low, $p01) < 0) $high = $high + 1 | 0;
		$p10 = $p10 << 16;
		$low = $low + $p10 | 0;
		if (Int32::ucompare($low, $p10) < 0) $high = $high + 1 | 0;
		$high = $high + (Int32::mul($a->low, $b->high) + Int32::mul($a->high, $b->low) | 0) | 0;
		return Int64::make($high, $low);

	}

	static public function div(Int64 $a, Int64 $b) : Int64 { return Int64::divMod($a, $b)->quotient; }
	static public function mod(Int64 $a, Int64 $b) : Int64 { return Int64::divMod($a, $b)->modulus; }
	static public function rem(Int64 $a, Int64 $b) : Int64 { return Int64::divMod($a, $b)->modulus; }

	// BIT-WISE
	static public function not(Int64 $x) : Int64 { return Int64::make(~$x->high, ~$x->low); }

	static public function and(Int64 $a, Int64 $b) : Int64 { return Int64::make($a->high & $b->high, $a->low & $b->low); }
	static public function or (Int64 $a, Int64 $b) : Int64 { return Int64::make($a->high | $b->high, $a->low | $b->low); }
	static public function xor(Int64 $a, Int64 $b) : Int64 { return Int64::make($a->high ^ $b->high, $a->low ^ $b->low); }

	static public function shl(Int64 $a, int $b) : Int64 {
		$b &= 63;
		if ($b == 0) {
			return Int64::make($a->high, $a->low);
		} else if ($b < 32) {
			return Int64::make($a->high << $b | N::iushr($a->low, 32 - $b), $a->low << $b);
		} else {
			return Int64::make($a->low << $b - 32, 0);
		}
	}

	static public function shr(Int64 $a, int $b) : Int64 {
		$b &= 63;
		if ($b == 0) {
			return Int64::make($a->high, $a->low);
		} else if ($b < 32) {
			return Int64::make($a->high >> $b, $a->high << 32 - $b | N::iushr($a->low, $b));
		} else {
			return Int64::make($a->high >> 31, $a->high >> $b - 32);
		}
	}

	static public function ushr(Int64 $a, int $b) : Int64 {
		$b &= 63;
		if ($b == 0) {
			return Int64::make($a->high, $a->low);
		} else if ($b < 32) {
			return Int64::make(N::iushr($a->high, $b), $a->high << 32 - $b | N::iushr($a->low, $b));
		} else {
			return Int64::make(0, N::iushr($a->high, $b - 32));
		}
	}

	static public function sign(Int64 $a) : int {
		if (Int64::isNeg($a)) return -1;
		if (Int64::isNotZero($a)) return +1;
		return 0;
	}

	static public function abs(Int64 $a) : Int64 {
		return Int64::isNeg($a) ? Int64::neg($a) : $a;
	}

	static public function getInternal(Int64 $value) : Int64 {
		return $value;
	}

}

Int64::$MAX_INT64 = new Int64(0x7FFFFFFF|0, 0xFFFFFFFF|0);
Int64::$MIN_INT64 = new Int64(0x80000000|0, 0x00000000|0);
Int64::$zero = new Int64(0, 0);
Int64::$one = new Int64(0, 1);
Int64::$MIN_VALUE = Int64::$MIN_INT64;
Int64::$MAX_VALUE = Int64::$MAX_INT64;


interface java_lang_annotation_Annotation {

}
class java_lang_annotation_Annotation_IFields {

	static public function SI() {
	}
}
class java_lang_Object {

	public $___id = 0;
	public function java_lang_Object_init___V() {
		return $this;
		return $this;
	}
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->getClass__Ljava_lang_Class_()->getName__Ljava_lang_String_())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_Integer::toHexString_I_Ljava_lang_String_($this->hashCode__I()))->toString__Ljava_lang_String_();
	}
	public function getClass__Ljava_lang_Class_() {
		if (!(java_lang_jtransc_JTranscCoreReflection::isArray_Ljava_lang_Object__Z($this))) goto label_1;
		return java_lang_jtransc_JTranscCoreReflection::getClassByName_Ljava_lang_String__Ljava_lang_Class_(java_lang_jtransc_JTranscCoreReflection::getArrayDescriptor_Ljava_lang_Object__Ljava_lang_String_($this));
		label_1:
		return java_lang_jtransc_JTranscCoreReflection::getClassById_I_Ljava_lang_Class_(java_lang_jtransc_JTranscCoreReflection::getClassId_Ljava_lang_Object__I($this));
	}
	public function clone__Ljava_lang_Object_() {
		$G = 0;
		$lA2 = null;
		$lA1 = null;
		$lA3 = null;
		$lA6 = null;
		$lI1 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$fA0 = null;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							if (!(java_lang_jtransc_JTranscCoreReflection::isArray_Ljava_lang_Object__Z($this))) {
								$G = 1;
								continue 2;
							}
							$lI1 = java_lang_reflect_Array::getLength_Ljava_lang_Object__I($this);
							$lA2 = java_lang_reflect_Array::newInstance_Ljava_lang_Class_I_Ljava_lang_Object_($this->getClass__Ljava_lang_Class_()->getComponentType__Ljava_lang_Class_(), $lI1);
							java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V($this, 0, $lA2, 0, $lI1);
							return $lA2;
						case 1:
							$lA1 = ($this->getClass__Ljava_lang_Class_());
							$lA2 = ($lA1)->newInstance__Ljava_lang_Object_();
							$lA3 = (($lA1)->getDeclaredFields___Ljava_lang_reflect_Field_());
							$lI4 = ($lA3)->length;
							$lI5 = 0;
							$G = 2;
							continue 2;
						case 2:
							if ((($lI5 >= $lI4))) {
								$G = 3;
								continue 2;
							}
							$lA6 = (($lA3)->get($lI5));
							($lA6)->set_Ljava_lang_Object_Ljava_lang_Object__V($lA2, ($lA6)->get_Ljava_lang_Object__Ljava_lang_Object_($this));
							$lI5 = ((int)(($lI5 + 1)));
							$G = 2;
							continue 2;
						case 3:
							$fA0 = $lA2;
							$G = 4;
							continue 2;
						case 4:return $fA0; 
						case 5:
							$fA0 = ($J__exception__);
							$lA1 = $fA0;
							$tA0 = ((new java_lang_CloneNotSupportedException()));
							$fA0 = $tA0;
							($tA0)->java_lang_CloneNotSupportedException_init__Ljava_lang_String__V(($lA1)->toString__Ljava_lang_String_());
							throw new WrappedThrowable($fA0);
							break;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 4)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 5;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if ((($this != $p0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function hashCode__I() {
		return java_lang_SystemInt::identityHashCode_Ljava_lang_Object__I($this);
	}
	public function __toString() {
		try { return N::istr($this->toString__Ljava_lang_String_()); } catch (WrappedThrowable $t) { echo $t->t; return '__toString.ERROR:' . $t; } catch (Throwable $t) { echo $t; return '__toString.ERROR:' . $t; }
	}
	public $__JT__CLASS_ID;
	public function __construct($CLASS_ID = 650) {
		$this->__JT__CLASS_ID = $CLASS_ID;
		$this->___id = 0;
	}
	static public function SI() {
	}
}
class java_lang_annotation_Annotation_Impl extends java_lang_Object implements java_lang_annotation_Annotation {

	public function java_lang_annotation_Annotation_Impl_init___V() {
		return $this;
	}
	public function annotationType__Ljava_lang_Class_() {
		return N::resolveClass("Ljava/lang/annotation/Annotation;");
	}
	public function getClass__Ljava_lang_Class_() {
		return N::resolveClass("Ljava/lang/annotation/Annotation;");
	}
	public function toString__Ljava_lang_String_() {
		return (new java_lang_StringBuilder())->java_lang_StringBuilder_init___V()->append_Ljava_lang_Object__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_1)->toString__Ljava_lang_String_();
	}
	public function __construct($CLASS_ID = 924) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_lang_SafeVarargs extends java_lang_annotation_Annotation {

}
class java_lang_SafeVarargs_IFields {

	static public function SI() {
	}
}
class java_lang_SafeVarargs_Impl extends java_lang_Object implements java_lang_SafeVarargs {

	public function java_lang_SafeVarargs_Impl_init___V() {
		return $this;
	}
	public function annotationType__Ljava_lang_Class_() {
		return N::resolveClass("Ljava/lang/SafeVarargs;");
	}
	public function getClass__Ljava_lang_Class_() {
		return N::resolveClass("Ljava/lang/SafeVarargs;");
	}
	public function toString__Ljava_lang_String_() {
		return (new java_lang_StringBuilder())->java_lang_StringBuilder_init___V()->append_Ljava_lang_Object__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_1)->toString__Ljava_lang_String_();
	}
	public function __construct($CLASS_ID = 923) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_ref_ReferenceQueue extends java_lang_Object {

	public function java_lang_ref_ReferenceQueue_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 922) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_lang_ref_Reference extends java_lang_Object {

	public $_referent = null;
	public $_queue = null;
	public function get__Ljava_lang_Object_() {
		return $this->_referent;
	}
	public function java_lang_ref_Reference_init__Ljava_lang_Object__V(?java_lang_Object $p0) {
		$this->java_lang_ref_Reference_init__Ljava_lang_Object_Ljava_lang_ref_ReferenceQueue__V($p0, null);
		return $this;
		return $this;
	}
	public function java_lang_ref_Reference_init__Ljava_lang_Object_Ljava_lang_ref_ReferenceQueue__V(?java_lang_Object $p0, ?java_lang_ref_ReferenceQueue $p1) {
		($this)->java_lang_Object_init___V();
		$this->_referent = $p0;
		$this->_queue = $p1;
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 921) {
		parent::__construct($CLASS_ID);
		$this->_referent = null;
		$this->_queue = null;
	}
	static public function SI() {
	}
}
class java_lang_ref_WeakReference extends java_lang_ref_Reference {

	public function java_lang_ref_WeakReference_init__Ljava_lang_Object__V(?java_lang_Object $p0) {
		($this)->java_lang_ref_Reference_init__Ljava_lang_Object__V($p0);
		return $this;
		return $this;
	}
	public function java_lang_ref_WeakReference_init__Ljava_lang_Object_Ljava_lang_ref_ReferenceQueue__V(?java_lang_Object $p0, ?java_lang_ref_ReferenceQueue $p1) {
		($this)->java_lang_ref_Reference_init__Ljava_lang_Object_Ljava_lang_ref_ReferenceQueue__V($p0, $p1);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 920) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_lang_Comparable {

}
class java_lang_Comparable_IFields {

	static public function SI() {
	}
}
class java_io_ObjectStreamField extends java_lang_Object implements java_lang_Comparable {

	public $_name = null;
	public $_type = null;
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(($this)->getClass__Ljava_lang_Class_()->getName__Ljava_lang_String_())->append_C_Ljava_lang_StringBuilder_(40)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->getName__Ljava_lang_String_())->append_C_Ljava_lang_StringBuilder_(58)->append_Ljava_lang_Object__Ljava_lang_StringBuilder_(($this->getTypeInternal__Ljava_lang_Class_()))->append_C_Ljava_lang_StringBuilder_(41)->toString__Ljava_lang_String_();
	}
	public function getName__Ljava_lang_String_() {
		return $this->_name;
	}
	public function getTypeInternal__Ljava_lang_Class_() {
		if (!((($this->_type) instanceof java_lang_ref_WeakReference))) goto label_1;
		return N::checkcast(N::checkcast($this->_type, "java_lang_ref_WeakReference")->get__Ljava_lang_Object_(), "java_lang_Class");
		label_1:
		return N::checkcast($this->_type, "java_lang_Class");
	}
	public function java_io_ObjectStreamField_init__Ljava_lang_String_Ljava_lang_Class__V(?java_lang_String $p0, ?java_lang_Class $p1) {
		$fA0 = null;
		$fA1 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		($this)->java_lang_Object_init___V();
		if (((($p0) != null))) goto label_1;
		$tA0 = ((new java_lang_NullPointerException()));
		$fA0 = $tA0;
		($tA0)->java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_2);
		throw new WrappedThrowable($fA0);
		label_1:
		if (((($p1) != null))) goto label_3;
		$tA1 = ((new java_lang_NullPointerException()));
		$fA0 = $tA1;
		($tA1)->java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_3);
		throw new WrappedThrowable($fA0);
		label_3:
		$this->_name = $p0;
		$fA0 = ($this);
		$tA2 = ((new java_lang_ref_WeakReference()));
		$fA1 = $tA2;
		($tA2)->java_lang_ref_WeakReference_init__Ljava_lang_Object__V(($p1));
		($fA0)->_type = $fA1;
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 919) {
		parent::__construct($CLASS_ID);
		$this->_name = null;
		$this->_type = null;
	}
	static public function SI() {
	}
}
abstract class java_util_Hashtable_HashIterator extends java_lang_Object {

	public $_expectedModCount = 0;
	public $_nextIndex = 0;
	public $_this_0 = null;
	public $_nextEntry = null;
	public $_lastEntryReturned = null;
	public function java_util_Hashtable_HashIterator_init__Ljava_util_Hashtable__V(?java_util_Hashtable $p0) {
		$lA2 = null;
		$lA3 = null;
		$fI1 = 0;
		$fA0 = null;
		$fA1 = null;
		$tI1 = 0;
		$tA2 = null;
		$this->_this_0 = $p0;
		($this)->java_lang_Object_init___V();
		$this->_expectedModCount = java_util_Hashtable::access_500_Ljava_util_Hashtable__I($this->_this_0);
		$lA2 = (java_util_Hashtable::access_600_Ljava_util_Hashtable___Ljava_util_Hashtable_HashtableEntry_($p0));
		$lA3 = null;
		label_1:
		if ((($lA3 != null))) goto label_2;
		if ((($this->_nextIndex >= ($lA2)->length))) goto label_2;
		$fA0 = $lA2;
		$fA1 = ($this);
		$tA2 = $fA1;
		$tI1 = $this->_nextIndex;
		$fI1 = $tI1;
		($tA2)->_nextIndex = ((int)(($tI1 + 1)));
		$lA3 = (($fA0)->get($fI1));
		goto label_1;
		label_2:
		$this->_nextEntry = ($lA3);
		return $this;
		return $this;
	}
	public function hasNext__Z() {
		$fI0 = 0;
		if ((($this->_nextEntry == null))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function nextEntry__Ljava_util_Hashtable_HashtableEntry_() {
		$lA1 = null;
		$lA2 = null;
		$lA3 = null;
		$fI1 = 0;
		$fA0 = null;
		$fA1 = null;
		$tI3 = 0;
		$tA0 = null;
		$tA1 = null;
		$tA4 = null;
		$tA6 = null;
		$tA5 = null;
		if (((java_util_Hashtable::access_500_Ljava_util_Hashtable__I($this->_this_0) == $this->_expectedModCount))) goto label_1;
		$tA0 = ((new java_util_ConcurrentModificationException()));
		$fA0 = $tA0;
		($tA0)->java_util_ConcurrentModificationException_init___V();
		throw new WrappedThrowable($fA0);
		label_1:
		if ((($this->_nextEntry != null))) goto label_3;
		$tA1 = ((new java_util_NoSuchElementException()));
		$fA0 = $tA1;
		($tA1)->java_util_NoSuchElementException_init___V();
		throw new WrappedThrowable($fA0);
		label_3:
		$lA1 = ($this->_nextEntry);
		$lA2 = (java_util_Hashtable::access_600_Ljava_util_Hashtable___Ljava_util_Hashtable_HashtableEntry_($this->_this_0));
		$lA3 = (($lA1)->_next);
		label_5:
		if ((($lA3 != null))) goto label_6;
		if ((($this->_nextIndex >= ($lA2)->length))) goto label_6;
		$fA0 = $lA2;
		$fA1 = ($this);
		$tA4 = $fA1;
		$tI3 = $this->_nextIndex;
		$fI1 = $tI3;
		($tA4)->_nextIndex = ((int)(($tI3 + 1)));
		$lA3 = (($fA0)->get($fI1));
		goto label_5;
		label_6:
		$this->_nextEntry = ($lA3);
		$fA0 = ($this);
		$tA6 = $fA0;
		$tA5 = $lA1;
		$fA0 = $tA5;
		($tA6)->_lastEntryReturned = ($tA5);
		return ($fA0);
	}
	public function __construct($CLASS_ID = 918) {
		parent::__construct($CLASS_ID);
		$this->_expectedModCount = 0;
		$this->_nextIndex = 0;
		$this->_this_0 = null;
		$this->_nextEntry = null;
		$this->_lastEntryReturned = null;
	}
	static public function SI() {
	}
}
interface java_util_Iterator {

	public function next__Ljava_lang_Object_();
	public function hasNext__Z();
}
class java_util_Iterator_IFields {

	static public function SI() {
	}
}
class java_util_Hashtable_EntryIterator extends java_util_Hashtable_HashIterator implements java_util_Iterator {

	public $_this_0_ = null;
	public function java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable__V(?java_util_Hashtable $p0) {
		$this->_this_0_ = $p0;
		($this)->java_util_Hashtable_HashIterator_init__Ljava_util_Hashtable__V($p0);
		return $this;
		return $this;
	}
	public function next__Ljava_lang_Object_() {
		return ($this->next__Ljava_util_Map_Entry_());
	}
	public function next__Ljava_util_Map_Entry_() {
		return ($this->nextEntry__Ljava_util_Hashtable_HashtableEntry_());
	}
	public function java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V(?java_util_Hashtable $p0, ?java_util_Hashtable_1 $p1) {
		$this->java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 917) {
		parent::__construct($CLASS_ID);
		$this->_this_0_ = null;
	}
	static public function SI() {
	}
}
interface java_lang_Iterable {

	public function iterator__Ljava_util_Iterator_();
}
class java_lang_Iterable_IFields {

	static public function SI() {
	}
}
interface java_util_Collection extends java_lang_Iterable {

	public function size__I();
	public function isEmpty__Z();
	public function iterator__Ljava_util_Iterator_();
	public function add_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function toArray__Ljava_lang_Object___Ljava_lang_Object_(?JA_L $p0);
	public function hashCode__I();
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function contains_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function containsAll_Ljava_util_Collection__Z(?java_util_Collection $p0);
}
class java_util_Collection_IFields {

	static public function SI() {
	}
}
interface java_util_Set extends java_util_Collection {

	public function size__I();
	public function isEmpty__Z();
	public function iterator__Ljava_util_Iterator_();
	public function add_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function toArray__Ljava_lang_Object___Ljava_lang_Object_(?JA_L $p0);
	public function hashCode__I();
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function contains_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function containsAll_Ljava_util_Collection__Z(?java_util_Collection $p0);
}
class java_util_Set_IFields {

	static public function SI() {
	}
}
abstract class java_util_AbstractCollection extends java_lang_Object implements java_util_Collection {

	public function java_util_AbstractCollection_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function toString__Ljava_lang_String_() {
		$lA1 = null;
		$lA3 = null;
		$fA0 = null;
		$tA0 = null;
		$lA2 = null;
		if (!($this->isEmpty__Z())) goto label_1;
		return Bootstrap::$STRINGLIT_4;
		label_1:
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init__I_V(((int)(N::imul($this->size__I(), 16))));
		$lA1 = $fA0;
		($lA1)->append_C_Ljava_lang_StringBuilder_(91);
		$lA2 = $this->iterator__Ljava_util_Iterator_();
		label_3:
		if (!($lA2->hasNext__Z())) goto label_4;
		$lA3 = $lA2->next__Ljava_lang_Object_();
		if ((($lA3 == ($this)))) goto label_6;
		($lA1)->append_Ljava_lang_Object__Ljava_lang_StringBuilder_($lA3);
		goto label_8;
		label_6:
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_5);
		label_8:
		if (!($lA2->hasNext__Z())) goto label_9;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_6);
		label_9:
		goto label_3;
		label_4:
		($lA1)->append_C_Ljava_lang_StringBuilder_(93);
		return ($lA1)->toString__Ljava_lang_String_();
	}
	public function size__I() {
		throw new Exception("Missing body java.util.AbstractCollection.size()I");
	}
	public function isEmpty__Z() {
		$fI0 = 0;
		if ((($this->size__I() != 0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function iterator__Ljava_util_Iterator_() {
		throw new Exception("Missing body java.util.AbstractCollection.iterator()Ljava/util/Iterator;");
	}
	public function add_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_UnsupportedOperationException()));
		$fA0 = $tA0;
		($tA0)->java_lang_UnsupportedOperationException_init___V();
		throw new WrappedThrowable($fA0);
	}
	public function toArray__Ljava_lang_Object___Ljava_lang_Object_(?JA_L $p0) {
		return $this->toArrayList__Ljava_util_ArrayList_()->toArray__Ljava_lang_Object___Ljava_lang_Object_($p0);
	}
	public function toArrayList__Ljava_util_ArrayList_() {
		$lA1 = null;
		$lA3 = null;
		$fA0 = null;
		$tA0 = null;
		$lA2 = null;
		$tA0 = ((new java_util_ArrayList()));
		$fA0 = $tA0;
		($tA0)->java_util_ArrayList_init__I_V($this->size__I());
		$lA1 = $fA0;
		$lA2 = $this->iterator__Ljava_util_Iterator_();
		label_1:
		if (!($lA2->hasNext__Z())) goto label_2;
		$lA3 = $lA2->next__Ljava_lang_Object_();
		($lA1)->add_Ljava_lang_Object__Z($lA3);
		goto label_1;
		label_2:
		return ($lA1);
	}
	public function containsAll_Ljava_util_Collection__Z(?java_util_Collection $p0) {
		$lA2 = null;
		$lA2 = $p0->iterator__Ljava_util_Iterator_();
		label_1:
		if (!($lA2->hasNext__Z())) goto label_2;
		if ($this->contains_Ljava_lang_Object__Z($lA2->next__Ljava_lang_Object_())) goto label_1;
		return false;
		label_2:
		return true;
	}
	public function contains_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA2 = null;
		$lA2 = $this->iterator__Ljava_util_Iterator_();
		if ((($p0 == null))) goto label_1;
		label_2:
		if (!($lA2->hasNext__Z())) goto label_3;
		if (!($p0->equals_Ljava_lang_Object__Z($lA2->next__Ljava_lang_Object_()))) goto label_2;
		return true;
		label_1:
		if (!($lA2->hasNext__Z())) goto label_3;
		if ((($lA2->next__Ljava_lang_Object_() != null))) goto label_1;
		return true;
		label_3:
		return false;
	}
	public function __construct($CLASS_ID = 724) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_util_AbstractSet extends java_util_AbstractCollection implements java_util_Set {

	public function java_util_AbstractSet_init___V() {
		($this)->java_util_AbstractCollection_init___V();
		return $this;
		return $this;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$G = 0;
		$lA2 = null;
		$fI0 = 0;
		$lA3 = null;
		$fA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							if (((($this) != $p0))) {
								$G = 1;
								continue 2;
							}
							return true;
						case 1:
							if (!((($p0) instanceof java_util_Set))) {
								$G = 2;
								continue 2;
							}
							$lA2 = (N::checkcast($p0, "java_util_Set"));
							$G = 3;
							continue 2;
						case 3:
							if ((($this->size__I() != ($lA2)->size__I()))) {
								$G = 4;
								continue 2;
							}
							if (!($this->containsAll_Ljava_util_Collection__Z(($lA2)))) {
								$G = 4;
								continue 2;
							}
							$fI0 = 1;
							$G = 5;
							continue 2;
						case 4:
							$fI0 = 0;
							$G = 5;
							continue 2;
						case 5:return (($fI0)!=0); 
						case 6:
							$fA0 = ($J__exception__);
							$lA3 = $fA0;
							return false;
						case 7:
							$fA0 = ($J__exception__);
							$lA3 = $fA0;
							return false;
						case 2:
							return false;
						default:
							break;
					}
				}
				return false;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 3)) && (($G < 5)))) && (($J__exception__) instanceof java_lang_NullPointerException)))) {
					$G = 6;
					continue 1;
				}
				if ((((((($G >= 3)) && (($G < 5)))) && (($J__exception__) instanceof java_lang_ClassCastException)))) {
					$G = 7;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return false;
	}
	public function hashCode__I() {
		$lA3 = null;
		$fI0 = 0;
		$fI1 = 0;
		$lI1 = 0;
		$lA2 = null;
		$lI1 = 0;
		$lA2 = $this->iterator__Ljava_util_Iterator_();
		label_1:
		if (!($lA2->hasNext__Z())) goto label_2;
		$lA3 = $lA2->next__Ljava_lang_Object_();
		$fI0 = $lI1;
		if ((($lA3 != null))) goto label_4;
		$fI1 = 0;
		goto label_5;
		label_4:
		$fI1 = $lA3->hashCode__I();
		label_5:
		$lI1 = ((int)(($fI0 + $fI1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 824) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_Hashtable_EntrySet extends java_util_AbstractSet {

	public $_this_0 = null;
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$G = 0;
		$lA2 = null;
		$fI0 = 0;
		$lA3 = null;
		$fA0 = null;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$tA0 = ($this->_this_0);
							$fA0 = $tA0;
							$lA2 = $tA0;
							N::monitorEnter($fA0);
							$G = 1;
							continue 2;
						case 1:
							$fI0 = ((int)(N::z2i(parent::equals_Ljava_lang_Object__Z($p0))));
							N::monitorExit($lA2);
							$G = 2;
							continue 2;
						case 2:return (($fI0)!=0); 
						case 3:
							$fA0 = ($J__exception__);
							$lA3 = $fA0;
							N::monitorExit($lA2);
							$G = 4;
							continue 2;
						case 4:throw new WrappedThrowable($lA3); break;
						default:
							break;
					}
				}
				return false;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 3;
					continue 1;
				}
				if ((((((($G >= 3)) && (($G < 4)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 3;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return false;
	}
	public function hashCode__I() {
		return $this->_this_0->hashCode__I();
	}
	public function toString__Ljava_lang_String_() {
		$G = 0;
		$lA1 = null;
		$lA2 = null;
		$fA0 = null;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$tA0 = ($this->_this_0);
							$fA0 = $tA0;
							$lA1 = $tA0;
							N::monitorEnter($fA0);
							$G = 1;
							continue 2;
						case 1:
							$fA0 = (parent::toString__Ljava_lang_String_());
							N::monitorExit($lA1);
							$G = 2;
							continue 2;
						case 2:return ($fA0); 
						case 3:
							$fA0 = ($J__exception__);
							$lA2 = $fA0;
							N::monitorExit($lA1);
							$G = 4;
							continue 2;
						case 4:throw new WrappedThrowable($lA2); break;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 3;
					continue 1;
				}
				if ((((((($G >= 3)) && (($G < 4)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 3;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public function size__I() {
		return $this->_this_0->size__I();
	}
	public function iterator__Ljava_util_Iterator_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_util_Hashtable_EntryIterator()));
		$fA0 = $tA0;
		($tA0)->java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V($this->_this_0, null);
		return ($fA0);
	}
	public function toArray__Ljava_lang_Object___Ljava_lang_Object_(?JA_L $p0) {
		$G = 0;
		$lA2 = null;
		$lA3 = null;
		$fA0 = null;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$tA0 = ($this->_this_0);
							$fA0 = $tA0;
							$lA2 = $tA0;
							N::monitorEnter($fA0);
							$G = 1;
							continue 2;
						case 1:
							$fA0 = (parent::toArray__Ljava_lang_Object___Ljava_lang_Object_($p0));
							N::monitorExit($lA2);
							$G = 2;
							continue 2;
						case 2:return ($fA0); 
						case 3:
							$fA0 = ($J__exception__);
							$lA3 = $fA0;
							N::monitorExit($lA2);
							$G = 4;
							continue 2;
						case 4:throw new WrappedThrowable($lA3); break;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 3;
					continue 1;
				}
				if ((((((($G >= 3)) && (($G < 4)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 3;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public function containsAll_Ljava_util_Collection__Z(?java_util_Collection $p0) {
		$G = 0;
		$lA2 = null;
		$fI0 = 0;
		$lA3 = null;
		$fA0 = null;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$tA0 = ($this->_this_0);
							$fA0 = $tA0;
							$lA2 = $tA0;
							N::monitorEnter($fA0);
							$G = 1;
							continue 2;
						case 1:
							$fI0 = ((int)(N::z2i(parent::containsAll_Ljava_util_Collection__Z($p0))));
							N::monitorExit($lA2);
							$G = 2;
							continue 2;
						case 2:return (($fI0)!=0); 
						case 3:
							$fA0 = ($J__exception__);
							$lA3 = $fA0;
							N::monitorExit($lA2);
							$G = 4;
							continue 2;
						case 4:throw new WrappedThrowable($lA3); break;
						default:
							break;
					}
				}
				return false;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 3;
					continue 1;
				}
				if ((((((($G >= 3)) && (($G < 4)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 3;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return false;
	}
	public function contains_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA2 = null;
		if ((($p0) instanceof java_util_Map_Entry)) goto label_1;
		return false;
		label_1:
		$lA2 = N::checkcast($p0, "java_util_Map_Entry");
		return java_util_Hashtable::access_1100_Ljava_util_Hashtable_Ljava_lang_Object_Ljava_lang_Object__Z($this->_this_0, $lA2->getKey__Ljava_lang_Object_(), $lA2->getValue__Ljava_lang_Object_());
	}
	public function java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V(?java_util_Hashtable $p0, ?java_util_Hashtable_1 $p1) {
		$this->java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable__V($p0);
		return $this;
		return $this;
	}
	public function java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable__V(?java_util_Hashtable $p0) {
		$this->_this_0 = $p0;
		($this)->java_util_AbstractSet_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 916) {
		parent::__construct($CLASS_ID);
		$this->_this_0 = null;
	}
	static public function SI() {
	}
}
class java_util_Hashtable_1 extends java_lang_Object {

	public function __construct($CLASS_ID = 915) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_util_Map_Entry {

	public function getKey__Ljava_lang_Object_();
	public function getValue__Ljava_lang_Object_();
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function hashCode__I();
}
class java_util_Map_Entry_IFields {

	static public function SI() {
	}
}
class java_util_Hashtable_HashtableEntry extends java_lang_Object implements java_util_Map_Entry {

	public $_value = null;
	public $_next = null;
	public $_key = null;
	public $_hash = 0;
	public function java_util_Hashtable_HashtableEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_Hashtable_HashtableEntry__V(?java_lang_Object $p0, ?java_lang_Object $p1, int $p2, ?java_util_Hashtable_HashtableEntry $p3) {
		($this)->java_lang_Object_init___V();
		$this->_key = $p0;
		$this->_value = $p1;
		$this->_hash = $p2;
		$this->_next = $p3;
		return $this;
		return $this;
	}
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_Object__Ljava_lang_StringBuilder_($this->_key)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_7)->append_Ljava_lang_Object__Ljava_lang_StringBuilder_($this->_value)->toString__Ljava_lang_String_();
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		$lA2 = null;
		if ((($p0) instanceof java_util_Map_Entry)) goto label_1;
		return false;
		label_1:
		$lA2 = N::checkcast($p0, "java_util_Map_Entry");
		if (!($this->_key->equals_Ljava_lang_Object__Z($lA2->getKey__Ljava_lang_Object_()))) goto label_3;
		if (!($this->_value->equals_Ljava_lang_Object__Z($lA2->getValue__Ljava_lang_Object_()))) goto label_3;
		$fI0 = 1;
		goto label_4;
		label_3:
		$fI0 = 0;
		label_4:
		return (($fI0)!=0);
	}
	public function hashCode__I() {
		return ((int)(($this->_key->hashCode__I() ^ $this->_value->hashCode__I())));
	}
	public function getKey__Ljava_lang_Object_() {
		return $this->_key;
	}
	public function getValue__Ljava_lang_Object_() {
		return $this->_value;
	}
	public function __construct($CLASS_ID = 914) {
		parent::__construct($CLASS_ID);
		$this->_value = null;
		$this->_next = null;
		$this->_key = null;
		$this->_hash = 0;
	}
	static public function SI() {
	}
}
abstract class java_util_Dictionary extends java_lang_Object {

	public function java_util_Dictionary_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function size__I() {
		throw new Exception("Missing body java.util.Dictionary.size()I");
	}
	public function isEmpty__Z() {
		throw new Exception("Missing body java.util.Dictionary.isEmpty()Z");
	}
	public function get_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0) {
		throw new Exception("Missing body java.util.Dictionary.get(Ljava/lang/Object;)Ljava/lang/Object;");
	}
	public function put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0, ?java_lang_Object $p1) {
		throw new Exception("Missing body java.util.Dictionary.put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
	}
	public function __construct($CLASS_ID = 913) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_util_Map {

	public function entrySet__Ljava_util_Set_();
	public function size__I();
	public function isEmpty__Z();
	public function get_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0);
	public function containsKey_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function hashCode__I();
	public function put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0, ?java_lang_Object $p1);
}
class java_util_Map_IFields {

	static public function SI() {
	}
}
interface java_lang_Cloneable {

}
class java_lang_Cloneable_IFields {

	static public function SI() {
	}
}
interface java_io_Serializable {

}
class java_io_Serializable_IFields {

	static public function SI() {
	}
}
class java_util_Hashtable extends java_util_Dictionary implements java_util_Map, java_lang_Cloneable, java_io_Serializable {

	public $_threshold = 0;
	public $_table = null;
	public static $_EMPTY_TABLE = null;
	public $_size = 0;
	public $_modCount = 0;
	public $_entrySet = null;
	public $_keySet = null;
	public $_values = null;
	public static $_serialPersistentFields = null;
	public function java_util_Hashtable_init___V() {
		($this)->java_util_Dictionary_init___V();
		$this->_table = N::checkcast(N::checkcast(java_util_Hashtable::$_EMPTY_TABLE, "JA_L"), "JA_L");
		$this->_threshold = -1;
		return $this;
		return $this;
	}
	public function size__I() {
		return $this->_size;
	}
	public function isEmpty__Z() {
		$fI0 = 0;
		if ((($this->_size != 0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function get_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0) {
		$lA3 = null;
		$lA4 = null;
		$lA5 = null;
		$lI2 = 0;
		$lI2 = java_util_Collections::secondaryHash_Ljava_lang_Object__I($p0);
		$lA3 = ($this->_table);
		$lA4 = (($lA3)->get(((int)(($lI2 & ((int)((($lA3)->length - 1))))))));
		label_1:
		if ((($lA4 == null))) goto label_2;
		$lA5 = ($lA4)->_key;
		if ((($lA5 == $p0))) goto label_4;
		if (((($lA4)->_hash != $lI2))) goto label_5;
		if (!($p0->equals_Ljava_lang_Object__Z($lA5))) goto label_5;
		label_4:
		return ($lA4)->_value;
		label_5:
		$lA4 = (($lA4)->_next);
		goto label_1;
		label_2:
		return null;
	}
	public function put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0, ?java_lang_Object $p1) {
		$lA4 = null;
		$lA6 = null;
		$lA7 = null;
		$lA8 = null;
		$lI3 = 0;
		$lI5 = 0;
		$fA0 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		$tA5 = null;
		$tA6 = null;
		$fI0 = 0;
		$fI1 = 0;
		$tI4 = 0;
		if ((($p0 != null))) goto label_1;
		$tA0 = ((new java_lang_NullPointerException()));
		$fA0 = $tA0;
		($tA0)->java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_8);
		throw new WrappedThrowable($fA0);
		label_1:
		if ((($p1 != null))) goto label_3;
		$tA1 = ((new java_lang_NullPointerException()));
		$fA0 = $tA1;
		($tA1)->java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_9);
		throw new WrappedThrowable($fA0);
		label_3:
		$lI3 = java_util_Collections::secondaryHash_Ljava_lang_Object__I($p0);
		$lA4 = ($this->_table);
		$lI5 = ((int)(($lI3 & ((int)((($lA4)->length - 1))))));
		$lA6 = (($lA4)->get($lI5));
		$lA7 = $lA6;
		label_5:
		if ((($lA7 == null))) goto label_6;
		if (((($lA7)->_hash != $lI3))) goto label_8;
		if (!($p0->equals_Ljava_lang_Object__Z(($lA7)->_key))) goto label_8;
		$lA8 = ($lA7)->_value;
		($lA7)->_value = $p1;
		return $lA8;
		label_8:
		$lA7 = (($lA7)->_next);
		goto label_5;
		label_6:
		$this->_modCount = ((int)(($this->_modCount + 1)));
		$fA0 = ($this);
		$tA5 = $fA0;
		$tI4 = $this->_size;
		$fI0 = $tI4;
		($tA5)->_size = ((int)(($tI4 + 1)));
		if ((($fI0 <= $this->_threshold))) goto label_10;
		$this->rehash__V();
		$lA4 = ($this->doubleCapacity___Ljava_util_Hashtable_HashtableEntry_());
		$lI5 = ((int)(($lI3 & ((int)((($lA4)->length - 1))))));
		$lA6 = (($lA4)->get($lI5));
		label_10:
		$fA0 = $lA4;
		$fI1 = $lI5;
		$tA6 = ((new java_util_Hashtable_HashtableEntry()));
		$fA2 = $tA6;
		($tA6)->java_util_Hashtable_HashtableEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_Hashtable_HashtableEntry__V($p0, $p1, $lI3, ($lA6));
		($fA0)->set($fI1, $fA2);
		return null;
	}
	public function rehash__V() {
		return;
	}
	public function doubleCapacity___Ljava_util_Hashtable_HashtableEntry_() {
		$lA1 = null;
		$lA4 = null;
		$lA6 = null;
		$lA8 = null;
		$lA9 = null;
		$lI2 = 0;
		$lI3 = 0;
		$lI5 = 0;
		$lI7 = 0;
		$lI10 = 0;
		$lA1 = ($this->_table);
		$lI2 = ($lA1)->length;
		if ((($lI2 != 1073741824))) goto label_1;
		return ($lA1);
		label_1:
		$lI3 = ((int)(N::imul($lI2, 2)));
		$lA4 = ($this->makeTable_I__Ljava_util_Hashtable_HashtableEntry_($lI3));
		if ((($this->_size != 0))) goto label_3;
		return ($lA4);
		label_3:
		$lI5 = 0;
		label_5:
		if ((($lI5 >= $lI2))) goto label_6;
		$lA6 = (($lA1)->get($lI5));
		if ((($lA6 != null))) goto label_8;
		goto label_10;
		label_8:
		$lI7 = ((int)((($lA6)->_hash & $lI2)));
		$lA8 = null;
		($lA4)->set(((int)(($lI5 | $lI7))), $lA6);
		$lA9 = (($lA6)->_next);
		label_11:
		if ((($lA9 == null))) goto label_12;
		$lI10 = ((int)((($lA9)->_hash & $lI2)));
		if ((($lI10 == $lI7))) goto label_14;
		if ((($lA8 != null))) goto label_16;
		($lA4)->set(((int)(($lI5 | $lI10))), $lA9);
		goto label_18;
		label_16:
		($lA8)->_next = ($lA9);
		label_18:
		$lA8 = $lA6;
		$lI7 = $lI10;
		label_14:
		$lA6 = $lA9;
		$lA9 = (($lA9)->_next);
		goto label_11;
		label_12:
		if ((($lA8 == null))) goto label_10;
		($lA8)->_next = null;
		label_10:
		$lI5 = ((int)(($lI5 + 1)));
		goto label_5;
		label_6:
		return ($lA4);
	}
	public function makeTable_I__Ljava_util_Hashtable_HashtableEntry_(int $p0) {
		$lA2 = null;
		$lA2 = N::checkcast(new JA_L($p0, "[Ljava.util.Hashtable\$HashtableEntry;"), "JA_L");
		$this->_table = $lA2;
		$this->_threshold = ((int)((((int)(N::ishr($p0, 1))) + ((int)(N::ishr($p0, 2))))));
		return $lA2;
	}
	public function toString__Ljava_lang_String_() {
		$lA1 = null;
		$lA5 = null;
		$lA6 = null;
		$fI0 = 0;
		$lI3 = 0;
		$lA4 = null;
		$fA0 = null;
		$tI5 = 0;
		$fA1 = null;
		$tA0 = null;
		$lA2 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init__I_V(((int)(N::imul(15, $this->_size))));
		$lA1 = $fA0;
		($lA1)->append_C_Ljava_lang_StringBuilder_(123);
		$lA2 = $this->entrySet__Ljava_util_Set_()->iterator__Ljava_util_Iterator_();
		$lI3 = ((int)(N::z2i($lA2->hasNext__Z())));
		label_1:
		if ((($lI3 == 0))) goto label_2;
		$lA4 = N::checkcast($lA2->next__Ljava_lang_Object_(), "java_util_Map_Entry");
		$lA5 = $lA4->getKey__Ljava_lang_Object_();
		$fA0 = $lA1;
		if ((($lA5 != ($this)))) goto label_4;
		$fA1 = Bootstrap::$STRINGLIT_10;
		goto label_5;
		label_4:
		$fA1 = $lA5->toString__Ljava_lang_String_();
		label_5:
		($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($fA1);
		($lA1)->append_C_Ljava_lang_StringBuilder_(61);
		$lA6 = $lA4->getValue__Ljava_lang_Object_();
		$fA0 = $lA1;
		if ((($lA6 != ($this)))) goto label_6;
		$fA1 = Bootstrap::$STRINGLIT_10;
		goto label_7;
		label_6:
		$fA1 = $lA6->toString__Ljava_lang_String_();
		label_7:
		($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($fA1);
		$tI5 = ((int)(N::z2i($lA2->hasNext__Z())));
		$fI0 = $tI5;
		$lI3 = $tI5;
		if ((($fI0 == 0))) goto label_8;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_6);
		label_8:
		goto label_1;
		label_2:
		($lA1)->append_C_Ljava_lang_StringBuilder_(125);
		return ($lA1)->toString__Ljava_lang_String_();
	}
	public function entrySet__Ljava_util_Set_() {
		$lA1 = null;
		$fA0 = null;
		$fA1 = null;
		$tA0 = null;
		$tA2 = null;
		$tA1 = null;
		$lA1 = ($this->_entrySet);
		if ((($lA1 == null))) goto label_1;
		$fA0 = $lA1;
		goto label_2;
		label_1:
		$fA0 = ($this);
		$tA0 = ((new java_util_Hashtable_EntrySet()));
		$fA1 = $tA0;
		($tA0)->java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V($this, null);
		$tA2 = $fA0;
		$tA1 = $fA1;
		$fA0 = $tA1;
		($tA2)->_entrySet = ($tA1);
		label_2:
		return ($fA0);
	}
	public function hashCode__I() {
		$lA4 = null;
		$lA5 = null;
		$fI0 = 0;
		$fI1 = 0;
		$fI2 = 0;
		$lI1 = 0;
		$lA3 = null;
		$fA2 = null;
		$lA2 = null;
		$lI1 = 0;
		$lA2 = $this->entrySet__Ljava_util_Set_()->iterator__Ljava_util_Iterator_();
		label_1:
		if (!($lA2->hasNext__Z())) goto label_2;
		$lA3 = N::checkcast($lA2->next__Ljava_lang_Object_(), "java_util_Map_Entry");
		$lA4 = $lA3->getKey__Ljava_lang_Object_();
		$lA5 = $lA3->getValue__Ljava_lang_Object_();
		if ((($lA4 == ($this)))) goto label_1;
		if ((($lA5 != ($this)))) goto label_3;
		goto label_1;
		label_3:
		$fI0 = $lI1;
		if ((($lA4 == null))) goto label_5;
		$fI1 = $lA4->hashCode__I();
		goto label_6;
		label_5:
		$fI1 = 0;
		label_6:
		if ((($lA5 == null))) goto label_7;
		$fA2 = $lA5;
		$fI2 = $fA2->hashCode__I();
		goto label_8;
		label_7:
		$fI2 = 0;
		label_8:
		$lI1 = ((int)(($fI0 + ((int)(($fI1 ^ $fI2))))));
		goto label_1;
		label_2:
		return $lI1;
	}
	public static function access_500_Ljava_util_Hashtable__I(?java_util_Hashtable $p0) {
		return $p0->_modCount;
	}
	public static function access_600_Ljava_util_Hashtable___Ljava_util_Hashtable_HashtableEntry_(?java_util_Hashtable $p0) {
		return $p0->_table;
	}
	public static function access_1100_Ljava_util_Hashtable_Ljava_lang_Object_Ljava_lang_Object__Z(?java_util_Hashtable $p0, ?java_lang_Object $p1, ?java_lang_Object $p2) {
		return $p0->containsMapping_Ljava_lang_Object_Ljava_lang_Object__Z($p1, $p2);
	}
	public function containsMapping_Ljava_lang_Object_Ljava_lang_Object__Z(?java_lang_Object $p0, ?java_lang_Object $p1) {
		$lA4 = null;
		$lA6 = null;
		$lI3 = 0;
		$lI5 = 0;
		$lI3 = java_util_Collections::secondaryHash_Ljava_lang_Object__I($p0);
		$lA4 = ($this->_table);
		$lI5 = ((int)(($lI3 & ((int)((($lA4)->length - 1))))));
		$lA6 = (($lA4)->get($lI5));
		label_1:
		if ((($lA6 == null))) goto label_2;
		if (((($lA6)->_hash != $lI3))) goto label_4;
		if (!(($lA6)->_key->equals_Ljava_lang_Object__Z($p0))) goto label_4;
		return ($lA6)->_value->equals_Ljava_lang_Object__Z($p1);
		label_4:
		$lA6 = (($lA6)->_next);
		goto label_1;
		label_2:
		return false;
	}
	public function clone__Ljava_lang_Object_() {
		$G = 0;
		$lA1 = null;
		$lA2 = null;
		$fA0 = null;
		$tA1 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$G = 1;
							continue 2;
						case 1:
							$lA1 = (N::checkcast(parent::clone__Ljava_lang_Object_(), "java_util_Hashtable"));
							$G = 2;
							continue 2;
						case 2:
							$G = 3;
							continue 2;
						case 4:
							$fA0 = ($J__exception__);
							$lA2 = $fA0;
							$tA1 = ((new java_lang_AssertionError()));
							$fA0 = $tA1;
							($tA1)->java_lang_AssertionError_init__Ljava_lang_Object__V($lA2);
							throw new WrappedThrowable($fA0);
							$G = 3;
							continue 2;
						case 3:
							($lA1)->makeTable_I__Ljava_util_Hashtable_HashtableEntry_(($this->_table)->length);
							($lA1)->_size = 0;
							($lA1)->_keySet = null;
							($lA1)->_entrySet = null;
							($lA1)->_values = null;
							($lA1)->constructorPutAll_Ljava_util_Map__V(($this));
							return $lA1;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_CloneNotSupportedException)))) {
					$G = 4;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public function constructorPutAll_Ljava_util_Map__V(?java_util_Map $p0) {
		$lA3 = null;
		$lA2 = null;
		if ((($this->_table != java_util_Hashtable::$_EMPTY_TABLE))) goto label_1;
		$this->doubleCapacity___Ljava_util_Hashtable_HashtableEntry_();
		label_1:
		$lA2 = $p0->entrySet__Ljava_util_Set_()->iterator__Ljava_util_Iterator_();
		label_3:
		if (!($lA2->hasNext__Z())) goto label_4;
		$lA3 = N::checkcast($lA2->next__Ljava_lang_Object_(), "java_util_Map_Entry");
		$this->constructorPut_Ljava_lang_Object_Ljava_lang_Object__V($lA3->getKey__Ljava_lang_Object_(), $lA3->getValue__Ljava_lang_Object_());
		goto label_3;
		label_4:
		return;
	}
	public function constructorPut_Ljava_lang_Object_Ljava_lang_Object__V(?java_lang_Object $p0, ?java_lang_Object $p1) {
		$lA4 = null;
		$lA6 = null;
		$lA7 = null;
		$fI1 = 0;
		$lI3 = 0;
		$lI5 = 0;
		$fA0 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		if ((($p0 != null))) goto label_1;
		$tA0 = ((new java_lang_NullPointerException()));
		$fA0 = $tA0;
		($tA0)->java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_8);
		throw new WrappedThrowable($fA0);
		label_1:
		if ((($p1 != null))) goto label_3;
		$tA1 = ((new java_lang_NullPointerException()));
		$fA0 = $tA1;
		($tA1)->java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_9);
		throw new WrappedThrowable($fA0);
		label_3:
		$lI3 = java_util_Collections::secondaryHash_Ljava_lang_Object__I($p0);
		$lA4 = ($this->_table);
		$lI5 = ((int)(($lI3 & ((int)((($lA4)->length - 1))))));
		$lA6 = (($lA4)->get($lI5));
		$lA7 = $lA6;
		label_5:
		if ((($lA7 == null))) goto label_6;
		if (((($lA7)->_hash != $lI3))) goto label_8;
		if (!($p0->equals_Ljava_lang_Object__Z(($lA7)->_key))) goto label_8;
		($lA7)->_value = $p1;
		return;
		label_8:
		$lA7 = (($lA7)->_next);
		goto label_5;
		label_6:
		$fA0 = $lA4;
		$fI1 = $lI5;
		$tA2 = ((new java_util_Hashtable_HashtableEntry()));
		$fA2 = $tA2;
		($tA2)->java_util_Hashtable_HashtableEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_Hashtable_HashtableEntry__V($p0, $p1, $lI3, ($lA6));
		($fA0)->set($fI1, $fA2);
		$this->_size = ((int)(($this->_size + 1)));
		return;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if (!((($p0) instanceof java_util_Map))) goto label_1;
		if (!($this->entrySet__Ljava_util_Set_()->equals_Ljava_lang_Object__Z((N::checkcast($p0, "java_util_Map")->entrySet__Ljava_util_Set_())))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function containsKey_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA3 = null;
		$lA4 = null;
		$lA5 = null;
		$lI2 = 0;
		$lI2 = java_util_Collections::secondaryHash_Ljava_lang_Object__I($p0);
		$lA3 = ($this->_table);
		$lA4 = (($lA3)->get(((int)(($lI2 & ((int)((($lA3)->length - 1))))))));
		label_1:
		if ((($lA4 == null))) goto label_2;
		$lA5 = ($lA4)->_key;
		if ((($lA5 == $p0))) goto label_4;
		if (((($lA4)->_hash != $lI2))) goto label_5;
		if (!($p0->equals_Ljava_lang_Object__Z($lA5))) goto label_5;
		label_4:
		return true;
		label_5:
		$lA4 = (($lA4)->_next);
		goto label_1;
		label_2:
		return false;
	}
	public static function java_util_Hashtable_clinit___V() {
		$fI2 = 0;
		$fA0 = null;
		$fA1 = null;
		$fA3 = null;
		$tA0 = null;
		$tA1 = null;
		$tA3 = null;
		java_util_Hashtable::$_EMPTY_TABLE = (new JA_L(2, "[Ljava.util.Hashtable\$HashtableEntry;"));
		$tA0 = (new JA_L(2, "[Ljava.io.ObjectStreamField;"));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$fI2 = 0;
		$tA1 = ((new java_io_ObjectStreamField()));
		$fA3 = $tA1;
		($tA1)->java_io_ObjectStreamField_init__Ljava_lang_String_Ljava_lang_Class__V(Bootstrap::$STRINGLIT_11, java_lang_Integer::$_TYPE);
		($fA1)->set($fI2, $fA3);
		$fA1 = $fA0;
		$fI2 = 1;
		$tA3 = ((new java_io_ObjectStreamField()));
		$fA3 = $tA3;
		($tA3)->java_io_ObjectStreamField_init__Ljava_lang_String_Ljava_lang_Class__V(Bootstrap::$STRINGLIT_12, java_lang_Float::$_TYPE);
		($fA1)->set($fI2, $fA3);
		java_util_Hashtable::$_serialPersistentFields = ($fA0);
		return;
	}
	public function __construct($CLASS_ID = 912) {
		parent::__construct($CLASS_ID);
		$this->_threshold = 0;
		$this->_table = null;
		$this->_size = 0;
		$this->_modCount = 0;
		$this->_entrySet = null;
		$this->_keySet = null;
		$this->_values = null;
	}
	static public function SI() {
		java_util_Hashtable::$_EMPTY_TABLE = null;
		java_util_Hashtable::$_serialPersistentFields = null;
		java_util_Hashtable::java_util_Hashtable_clinit___V();
	}
}
class java_util_Properties extends java_util_Hashtable {

	public $_defaults = null;
	public function java_util_Properties_init___V() {
		($this)->java_util_Hashtable_init___V();
		return $this;
		return $this;
	}
	public function getProperty_Ljava_lang_String__Ljava_lang_String_(?java_lang_String $p0) {
		$lA2 = null;
		$lA3 = null;
		$fA0 = null;
		$lA2 = parent::get_Ljava_lang_Object__Ljava_lang_Object_(($p0));
		if (!((($lA2) instanceof java_lang_String))) goto label_1;
		$fA0 = (N::checkcast($lA2, "java_lang_String"));
		goto label_2;
		label_1:
		$fA0 = null;
		label_2:
		$lA3 = $fA0;
		if ((($lA3 != null))) goto label_3;
		if ((($this->_defaults == null))) goto label_3;
		$lA3 = ($this->_defaults->getProperty_Ljava_lang_String__Ljava_lang_String_($p0));
		label_3:
		return ($lA3);
	}
	public function __construct($CLASS_ID = 911) {
		parent::__construct($CLASS_ID);
		$this->_defaults = null;
	}
	static public function SI() {
	}
}
class com_jtransc_simd_MutableMatrixFloat32x4x4Utils extends java_lang_Object {

	public static $_vtemp2 = null;
	public static $_vtemp1 = null;
	public function com_jtransc_simd_MutableMatrixFloat32x4x4Utils_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function com_jtransc_simd_MutableMatrixFloat32x4x4Utils_clinit___V() {
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils::$_vtemp1 = com_jtransc_simd_MutableFloat32x4::create__Lcom_jtransc_simd_MutableFloat32x4_();
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils::$_vtemp2 = com_jtransc_simd_MutableFloat32x4::create__Lcom_jtransc_simd_MutableFloat32x4_();
		return;
	}
	public static function _setToMul44_Lcom_jtransc_simd_MutableMatrixFloat32x4x4_Lcom_jtransc_simd_MutableMatrixFloat32x4x4_Lcom_jtransc_simd_MutableMatrixFloat32x4x4__Lcom_jtransc_simd_MutableMatrixFloat32x4x4_(?com_jtransc_simd_MutableMatrixFloat32x4x4 $p0, ?com_jtransc_simd_MutableMatrixFloat32x4x4 $p1, ?com_jtransc_simd_MutableMatrixFloat32x4x4 $p2) {
		$lA10 = null;
		$lA11 = null;
		$lA12 = null;
		$lA3 = null;
		$lA4 = null;
		$lA5 = null;
		$lA6 = null;
		$lA7 = null;
		$lA8 = null;
		$lA9 = null;
		$lA3 = $p1->getX__Lcom_jtransc_simd_MutableFloat32x4_();
		$lA4 = $p2->getX__Lcom_jtransc_simd_MutableFloat32x4_();
		$lA5 = $p1->getY__Lcom_jtransc_simd_MutableFloat32x4_();
		$lA6 = $p2->getY__Lcom_jtransc_simd_MutableFloat32x4_();
		$lA7 = $p1->getZ__Lcom_jtransc_simd_MutableFloat32x4_();
		$lA8 = $p2->getZ__Lcom_jtransc_simd_MutableFloat32x4_();
		$lA9 = $p1->getW__Lcom_jtransc_simd_MutableFloat32x4_();
		$lA10 = $p2->getW__Lcom_jtransc_simd_MutableFloat32x4_();
		$lA11 = com_jtransc_simd_MutableMatrixFloat32x4x4Utils::$_vtemp1;
		$lA12 = com_jtransc_simd_MutableMatrixFloat32x4x4Utils::$_vtemp2;
		$lA12->setToZero__V();
		$lA11->setToXXXX_Lcom_jtransc_simd_MutableFloat32x4__V($lA4);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA3);
		$lA11->setToYYYY_Lcom_jtransc_simd_MutableFloat32x4__V($lA4);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA5);
		$lA11->setToZZZZ_Lcom_jtransc_simd_MutableFloat32x4__V($lA4);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA7);
		$lA11->setToWWWW_Lcom_jtransc_simd_MutableFloat32x4__V($lA4);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA9);
		$p0->setX_Lcom_jtransc_simd_MutableFloat32x4__V($lA12);
		$lA12->setToZero__V();
		$lA11->setToXXXX_Lcom_jtransc_simd_MutableFloat32x4__V($lA6);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA3);
		$lA11->setToYYYY_Lcom_jtransc_simd_MutableFloat32x4__V($lA6);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA5);
		$lA11->setToZZZZ_Lcom_jtransc_simd_MutableFloat32x4__V($lA6);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA7);
		$lA11->setToWWWW_Lcom_jtransc_simd_MutableFloat32x4__V($lA6);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA9);
		$p0->setY_Lcom_jtransc_simd_MutableFloat32x4__V($lA12);
		$lA12->setToZero__V();
		$lA11->setToXXXX_Lcom_jtransc_simd_MutableFloat32x4__V($lA8);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA3);
		$lA11->setToYYYY_Lcom_jtransc_simd_MutableFloat32x4__V($lA8);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA5);
		$lA11->setToZZZZ_Lcom_jtransc_simd_MutableFloat32x4__V($lA8);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA7);
		$lA11->setToWWWW_Lcom_jtransc_simd_MutableFloat32x4__V($lA8);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA9);
		$p0->setZ_Lcom_jtransc_simd_MutableFloat32x4__V($lA12);
		$lA12->setToZero__V();
		$lA11->setToXXXX_Lcom_jtransc_simd_MutableFloat32x4__V($lA10);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA3);
		$lA11->setToYYYY_Lcom_jtransc_simd_MutableFloat32x4__V($lA10);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA5);
		$lA11->setToZZZZ_Lcom_jtransc_simd_MutableFloat32x4__V($lA10);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA7);
		$lA11->setToWWWW_Lcom_jtransc_simd_MutableFloat32x4__V($lA10);
		$lA12->setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA12, $lA11, $lA9);
		$p0->setW_Lcom_jtransc_simd_MutableFloat32x4__V($lA12);
		return $p0;
	}
	public static function _getSumAll_Lcom_jtransc_simd_MutableMatrixFloat32x4x4__F(?com_jtransc_simd_MutableMatrixFloat32x4x4 $p0) {
		return (((((($p0->getX__Lcom_jtransc_simd_MutableFloat32x4_()->getSumAll__F() + $p0->getY__Lcom_jtransc_simd_MutableFloat32x4_()->getSumAll__F())) + $p0->getZ__Lcom_jtransc_simd_MutableFloat32x4_()->getSumAll__F())) + $p0->getW__Lcom_jtransc_simd_MutableFloat32x4_()->getSumAll__F()));
	}
	public function __construct($CLASS_ID = 910) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils::$_vtemp2 = null;
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils::$_vtemp1 = null;
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils::com_jtransc_simd_MutableMatrixFloat32x4x4Utils_clinit___V();
	}
}
class com_jtransc_simd_MutableMatrixFloat32x4x4 extends java_lang_Object {

	public $_w = null;
	public $_x = null;
	public $_y = null;
	public $_z = null;
	public function com_jtransc_simd_MutableMatrixFloat32x4x4_init___V() {
		($this)->java_lang_Object_init___V();
		$this->_x = com_jtransc_simd_MutableFloat32x4::create__Lcom_jtransc_simd_MutableFloat32x4_();
		$this->_y = com_jtransc_simd_MutableFloat32x4::create__Lcom_jtransc_simd_MutableFloat32x4_();
		$this->_z = com_jtransc_simd_MutableFloat32x4::create__Lcom_jtransc_simd_MutableFloat32x4_();
		$this->_w = com_jtransc_simd_MutableFloat32x4::create__Lcom_jtransc_simd_MutableFloat32x4_();
		return $this;
		return $this;
	}
	public static function com_jtransc_simd_MutableMatrixFloat32x4x4_clinit___V() {
		com_jtransc_simd_Simd::ref__V();
		return;
	}
	public function setTo_FFFFFFFFFFFFFFFF_V(float $p0, float $p1, float $p2, float $p3, float $p4, float $p5, float $p6, float $p7, float $p8, float $p9, float $p10, float $p11, float $p12, float $p13, float $p14, float $p15) {
		$this->getX__Lcom_jtransc_simd_MutableFloat32x4_()->setTo_FFFF_V($p0, $p1, $p2, $p3);
		$this->getY__Lcom_jtransc_simd_MutableFloat32x4_()->setTo_FFFF_V($p4, $p5, $p6, $p7);
		$this->getZ__Lcom_jtransc_simd_MutableFloat32x4_()->setTo_FFFF_V($p8, $p9, $p10, $p11);
		$this->getW__Lcom_jtransc_simd_MutableFloat32x4_()->setTo_FFFF_V($p12, $p13, $p14, $p15);
		return;
	}
	public function getZ__Lcom_jtransc_simd_MutableFloat32x4_() {
		return $this->_z;
	}
	public function getY__Lcom_jtransc_simd_MutableFloat32x4_() {
		return $this->_y;
	}
	public function getX__Lcom_jtransc_simd_MutableFloat32x4_() {
		return $this->_x;
	}
	public function getW__Lcom_jtransc_simd_MutableFloat32x4_() {
		return $this->_w;
	}
	public function setToMul44_Lcom_jtransc_simd_MutableMatrixFloat32x4x4_Lcom_jtransc_simd_MutableMatrixFloat32x4x4__V(?com_jtransc_simd_MutableMatrixFloat32x4x4 $p0, ?com_jtransc_simd_MutableMatrixFloat32x4x4 $p1) {
		com_jtransc_simd_MutableMatrixFloat32x4x4Utils::_setToMul44_Lcom_jtransc_simd_MutableMatrixFloat32x4x4_Lcom_jtransc_simd_MutableMatrixFloat32x4x4_Lcom_jtransc_simd_MutableMatrixFloat32x4x4__Lcom_jtransc_simd_MutableMatrixFloat32x4x4_($this, $p0, $p1);
		return;
	}
	public function setW_Lcom_jtransc_simd_MutableFloat32x4__V(?com_jtransc_simd_MutableFloat32x4 $p0) {
		$this->getW__Lcom_jtransc_simd_MutableFloat32x4_()->setTo_Lcom_jtransc_simd_MutableFloat32x4__V($p0);
		return;
	}
	public function setX_Lcom_jtransc_simd_MutableFloat32x4__V(?com_jtransc_simd_MutableFloat32x4 $p0) {
		$this->getX__Lcom_jtransc_simd_MutableFloat32x4_()->setTo_Lcom_jtransc_simd_MutableFloat32x4__V($p0);
		return;
	}
	public function setY_Lcom_jtransc_simd_MutableFloat32x4__V(?com_jtransc_simd_MutableFloat32x4 $p0) {
		$this->getY__Lcom_jtransc_simd_MutableFloat32x4_()->setTo_Lcom_jtransc_simd_MutableFloat32x4__V($p0);
		return;
	}
	public function setZ_Lcom_jtransc_simd_MutableFloat32x4__V(?com_jtransc_simd_MutableFloat32x4 $p0) {
		$this->getZ__Lcom_jtransc_simd_MutableFloat32x4_()->setTo_Lcom_jtransc_simd_MutableFloat32x4__V($p0);
		return;
	}
	public function getSumAll__F() {
		return com_jtransc_simd_MutableMatrixFloat32x4x4Utils::_getSumAll_Lcom_jtransc_simd_MutableMatrixFloat32x4x4__F($this);
	}
	public static function create__Lcom_jtransc_simd_MutableMatrixFloat32x4x4_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new com_jtransc_simd_MutableMatrixFloat32x4x4()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_simd_MutableMatrixFloat32x4x4_init___V();
		return ($fA0);
	}
	public function __construct($CLASS_ID = 909) {
		parent::__construct($CLASS_ID);
		$this->_w = null;
		$this->_x = null;
		$this->_y = null;
		$this->_z = null;
	}
	static public function SI() {
		com_jtransc_simd_MutableMatrixFloat32x4x4::com_jtransc_simd_MutableMatrixFloat32x4x4_clinit___V();
	}
}
class com_jtransc_simd_MutableFloat32x4Utils extends java_lang_Object {

	public static $_temp = null;
	public function com_jtransc_simd_MutableFloat32x4Utils_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function com_jtransc_simd_MutableFloat32x4Utils_clinit___V() {
		com_jtransc_simd_MutableFloat32x4Utils::$_temp = com_jtransc_simd_MutableFloat32x4::create__Lcom_jtransc_simd_MutableFloat32x4_();
		return;
	}
	public static function toStringInternal_Lcom_jtransc_simd_MutableFloat32x4__Ljava_lang_String_(?com_jtransc_simd_MutableFloat32x4 $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_13)->append_F_Ljava_lang_StringBuilder_($p0->getX__F())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_6)->append_F_Ljava_lang_StringBuilder_($p0->getY__F())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_6)->append_F_Ljava_lang_StringBuilder_($p0->getZ__F())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_6)->append_F_Ljava_lang_StringBuilder_($p0->getW__F())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_1)->toString__Ljava_lang_String_();
	}
	public function __construct($CLASS_ID = 908) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		com_jtransc_simd_MutableFloat32x4Utils::$_temp = null;
		com_jtransc_simd_MutableFloat32x4Utils::com_jtransc_simd_MutableFloat32x4Utils_clinit___V();
	}
}
class com_jtransc_simd_MutableFloat32x4 extends java_lang_Object {

	public $_z = 0.0;
	public $_x = 0.0;
	public $_y = 0.0;
	public $_w = 0.0;
	public function com_jtransc_simd_MutableFloat32x4_init__FFFF_V(float $p0, float $p1, float $p2, float $p3) {
		($this)->java_lang_Object_init___V();
		$this->setTo_FFFF_V($p0, $p1, $p2, $p3);
		return $this;
		return $this;
	}
	public function setTo_FFFF_V(float $p0, float $p1, float $p2, float $p3) {
		$this->_x = $p0;
		$this->_y = $p1;
		$this->_z = $p2;
		$this->_w = $p3;
		return;
	}
	public static function com_jtransc_simd_MutableFloat32x4_clinit___V() {
		com_jtransc_simd_Simd::ref__V();
		return;
	}
	public function toString__Ljava_lang_String_() {
		return com_jtransc_simd_MutableFloat32x4Utils::toStringInternal_Lcom_jtransc_simd_MutableFloat32x4__Ljava_lang_String_($this);
	}
	public static function create__Lcom_jtransc_simd_MutableFloat32x4_() {
		return com_jtransc_simd_MutableFloat32x4::create_FFFF_Lcom_jtransc_simd_MutableFloat32x4_(0.0, 0.0, 0.0, 0.0);
	}
	public static function create_FFFF_Lcom_jtransc_simd_MutableFloat32x4_(float $p0, float $p1, float $p2, float $p3) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new com_jtransc_simd_MutableFloat32x4()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_simd_MutableFloat32x4_init__FFFF_V($p0, $p1, $p2, $p3);
		return ($fA0);
	}
	public function getZ__F() {
		return $this->_z;
	}
	public function getY__F() {
		return $this->_y;
	}
	public function getX__F() {
		return $this->_x;
	}
	public function getW__F() {
		return $this->_w;
	}
	public function setToAdd_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V(?com_jtransc_simd_MutableFloat32x4 $p0, ?com_jtransc_simd_MutableFloat32x4 $p1) {
		$this->setTo_FFFF_V((($p0->_x + $p1->_x)), (($p0->_y + $p1->_y)), (($p0->_z + $p1->_z)), (($p0->_w + $p1->_w)));
		return;
	}
	public function setTo_Lcom_jtransc_simd_MutableFloat32x4__V(?com_jtransc_simd_MutableFloat32x4 $p0) {
		$this->_x = $p0->_x;
		$this->_y = $p0->_y;
		$this->_z = $p0->_z;
		$this->_w = $p0->_w;
		return;
	}
	public function setToZZZZ_Lcom_jtransc_simd_MutableFloat32x4__V(?com_jtransc_simd_MutableFloat32x4 $p0) {
		$p0->setTo_FFFF_V($p0->_z, $p0->_z, $p0->_z, $p0->_z);
		return;
	}
	public function setToAddMul_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V(?com_jtransc_simd_MutableFloat32x4 $p0, ?com_jtransc_simd_MutableFloat32x4 $p1, ?com_jtransc_simd_MutableFloat32x4 $p2) {
		$this->setTo_FFFF_V((($p0->_x + (($p1->_x * $p2->_x)))), (($p0->_y + (($p1->_y * $p2->_y)))), (($p0->_z + (($p1->_z * $p2->_z)))), (($p0->_w + (($p1->_w * $p2->_w)))));
		return;
	}
	public function setToZero__V() {
		$this->setTo_FFFF_V(0.0, 0.0, 0.0, 0.0);
		return;
	}
	public function setToWWWW_Lcom_jtransc_simd_MutableFloat32x4__V(?com_jtransc_simd_MutableFloat32x4 $p0) {
		$p0->setTo_FFFF_V($p0->_w, $p0->_w, $p0->_w, $p0->_w);
		return;
	}
	public function setToXXXX_Lcom_jtransc_simd_MutableFloat32x4__V(?com_jtransc_simd_MutableFloat32x4 $p0) {
		$p0->setTo_FFFF_V($p0->_x, $p0->_x, $p0->_x, $p0->_x);
		return;
	}
	public function setToYYYY_Lcom_jtransc_simd_MutableFloat32x4__V(?com_jtransc_simd_MutableFloat32x4 $p0) {
		$p0->setTo_FFFF_V($p0->_y, $p0->_y, $p0->_y, $p0->_y);
		return;
	}
	public function getSumAll__F() {
		return (((((($this->getX__F() + $this->getY__F())) + $this->getZ__F())) + $this->getW__F()));
	}
	public function __construct($CLASS_ID = 907) {
		parent::__construct($CLASS_ID);
		$this->_z = 0.0;
		$this->_x = 0.0;
		$this->_y = 0.0;
		$this->_w = 0.0;
	}
	static public function SI() {
		com_jtransc_simd_MutableFloat32x4::com_jtransc_simd_MutableFloat32x4_clinit___V();
	}
}
class com_jtransc_simd_Simd extends java_lang_Object {

	public function com_jtransc_simd_Simd_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function ref__V() {
		return;
	}
	public function __construct($CLASS_ID = 906) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_simd_Float32x4 extends java_lang_Object {

	public $_z = 0.0;
	public $_y = 0.0;
	public $_x = 0.0;
	public $_w = 0.0;
	public function com_jtransc_simd_Float32x4_init__FFFF_V(float $p0, float $p1, float $p2, float $p3) {
		($this)->java_lang_Object_init___V();
		$this->_x = $p0;
		$this->_y = $p1;
		$this->_z = $p2;
		$this->_w = $p3;
		return $this;
		return $this;
	}
	public static function com_jtransc_simd_Float32x4_clinit___V() {
		com_jtransc_simd_Simd::ref__V();
		return;
	}
	public static function getW_Lcom_jtransc_simd_Float32x4__F(?com_jtransc_simd_Float32x4 $p0) {
		return $p0->_w;
	}
	public static function getX_Lcom_jtransc_simd_Float32x4__F(?com_jtransc_simd_Float32x4 $p0) {
		return $p0->_x;
	}
	public static function getY_Lcom_jtransc_simd_Float32x4__F(?com_jtransc_simd_Float32x4 $p0) {
		return $p0->_y;
	}
	public static function create_FFFF_Lcom_jtransc_simd_Float32x4_(float $p0, float $p1, float $p2, float $p3) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new com_jtransc_simd_Float32x4()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_simd_Float32x4_init__FFFF_V($p0, $p1, $p2, $p3);
		return ($fA0);
	}
	public static function getZ_Lcom_jtransc_simd_Float32x4__F(?com_jtransc_simd_Float32x4 $p0) {
		return $p0->_z;
	}
	public static function add_Lcom_jtransc_simd_Float32x4_Lcom_jtransc_simd_Float32x4__Lcom_jtransc_simd_Float32x4_(?com_jtransc_simd_Float32x4 $p0, ?com_jtransc_simd_Float32x4 $p1) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new com_jtransc_simd_Float32x4()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_simd_Float32x4_init__FFFF_V((($p0->_x + $p1->_x)), (($p0->_y + $p1->_y)), (($p0->_z + $p1->_z)), (($p0->_w + $p1->_w)));
		return ($fA0);
	}
	public function __construct($CLASS_ID = 905) {
		parent::__construct($CLASS_ID);
		$this->_z = 0.0;
		$this->_y = 0.0;
		$this->_x = 0.0;
		$this->_w = 0.0;
	}
	static public function SI() {
		com_jtransc_simd_Float32x4::com_jtransc_simd_Float32x4_clinit___V();
	}
}
class Benchmark_Test2 extends java_lang_Object {

	public function Benchmark_Test2_init__LBenchmark_1__V(?Benchmark_1 $p0) {
		$this->Benchmark_Test2_init___V();
		return $this;
		return $this;
	}
	public function Benchmark_Test2_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 904) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_Test1 extends java_lang_Object {

	public function Benchmark_Test1_init__LBenchmark_1__V(?Benchmark_1 $p0) {
		$this->Benchmark_Test1_init___V();
		return $this;
		return $this;
	}
	public function Benchmark_Test1_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 903) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_zip_Deflater extends java_lang_Object {

	public $_impl = null;
	public $_inLength = 0;
	public $_inRead = 0;
	public $_noHeader = false;
	public $_compressLevel = 0;
	public $_streamHandle = null;
	public $_flushParm = 0;
	public $_strategy = 0;
	public function setInput__BII_V(?JA_B $p0, int $p1, int $p2) {
		$this->_inLength = $p2;
		$this->_inRead = 0;
		$this->_impl->setInput__BIIZ_V($p0, $p1, $p2, false);
		return;
	}
	public function deflate__BIII_I(?JA_B $p0, int $p1, int $p2, int $p3) {
		return $this->deflateImpl__BIII_I($p0, $p1, $p2, $p3);
	}
	public function deflateImpl__BIII_I(?JA_B $p0, int $p1, int $p2, int $p3) {
		$lJ5 = Int64::make(0, 0);
		$lJ8 = Int64::make(0, 0);
		$this->_impl->setOutput__BII_V($p0, $p1, $p2);
		$lJ5 = $this->_impl->getTotalOut__J();
		$this->_impl->deflate_I_I($p3);
		$lJ8 = $this->_impl->getTotalOut__J();
		return N::j2i((N::lsub($lJ8, $lJ5)));
	}
	public function java_util_zip_Deflater_init__IZ_V(int $p0, bool $p1) {
		$fA1 = null;
		$tA0 = null;
		($this)->java_lang_Object_init___V();
		$this->_flushParm = 0;
		$this->_compressLevel = -1;
		$this->_strategy = 0;
		$this->_streamHandle = Int64::make(-1, -1);
		$this->_compressLevel = $p0;
		$this->_noHeader = $p1;
		$tA0 = ((new com_jtransc_compression_jzlib_Deflater()));
		$fA1 = $tA0;
		($tA0)->com_jtransc_compression_jzlib_Deflater_init__IZ_V($p0, $p1);
		$this->_impl = ($fA1);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 902) {
		parent::__construct($CLASS_ID);
		$this->_impl = null;
		$this->_inLength = 0;
		$this->_inRead = 0;
		$this->_noHeader = false;
		$this->_compressLevel = 0;
		$this->_streamHandle = Int64::make(0, 0);
		$this->_flushParm = 0;
		$this->_strategy = 0;
	}
	static public function SI() {
	}
}
interface com_jtransc_compression_jzlib_Checksum {

	public function getValue__I();
	public function reset__V();
	public function update__BII_V(?JA_B $p0, int $p1, int $p2);
}
class com_jtransc_compression_jzlib_Checksum_IFields {

	static public function SI() {
	}
}
class com_jtransc_compression_jzlib_Adler32 extends java_lang_Object implements com_jtransc_compression_jzlib_Checksum {

	public $_s1 = 0;
	public $_s2 = 0;
	public function com_jtransc_compression_jzlib_Adler32_init___V() {
		($this)->java_lang_Object_init___V();
		$this->_s1 = 1;
		$this->_s2 = 0;
		return $this;
		return $this;
	}
	public function getValue__I() {
		return ((int)((((int)(N::ishl($this->_s2, 16))) | $this->_s1)));
	}
	public function reset__V() {
		$this->_s1 = 1;
		$this->_s2 = 0;
		return;
	}
	public function update__BII_V(?JA_B $p0, int $p1, int $p2) {
		$fA2 = null;
		$lI2 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$lI6 = 0;
		$tA10 = null;
		$fI1 = 0;
		$fI3 = 0;
		$fI0 = 0;
		$fA0 = null;
		$tA5 = null;
		$tA7 = null;
		$tA9 = null;
		$tA1 = null;
		$tA3 = null;
		$lI2 = $p1;
		$lI3 = $p2;
		if ((($lI3 != 1))) goto label_1;
		$fA0 = $this;
		$fI1 = $this->_s1;
		$fA2 = $p0;
		$fI3 = $lI2;
		$lI2 = ((int)(($lI2 + 1)));
		$fA0->_s1 = ((int)(($fI1 + ((int)((($fA2->get($fI3)) & 255))))));
		$tA1 = $this;
		$tA1->_s2 = ((int)(($tA1->_s2 + $this->_s1)));
		$this->_s1 = ((int)(N::irem($this->_s1, 65521)));
		$tA3 = $this;
		$tA3->_s2 = ((int)(N::irem($tA3->_s2, 65521)));
		return;
		label_1:
		$lI4 = ((int)(N::idiv($lI3, 5552)));
		$lI5 = ((int)(N::irem($lI3, 5552)));
		label_3:
		$fI0 = $lI4;
		$lI4 = ((int)(($lI4 + -1)));
		if ((($fI0 <= 0))) goto label_4;
		$lI6 = 5552;
		$lI3 = ((int)(($lI3 - $lI6)));
		label_6:
		$fI0 = $lI6;
		$lI6 = ((int)(($lI6 + -1)));
		if ((($fI0 <= 0))) goto label_7;
		$fA0 = $this;
		$fI1 = $this->_s1;
		$fA2 = $p0;
		$fI3 = $lI2;
		$lI2 = ((int)(($lI2 + 1)));
		$fA0->_s1 = ((int)(($fI1 + ((int)((($fA2->get($fI3)) & 255))))));
		$tA5 = $this;
		$tA5->_s2 = ((int)(($tA5->_s2 + $this->_s1)));
		goto label_6;
		label_7:
		$this->_s1 = ((int)(N::irem($this->_s1, 65521)));
		$tA7 = $this;
		$tA7->_s2 = ((int)(N::irem($tA7->_s2, 65521)));
		goto label_3;
		label_4:
		$lI6 = $lI5;
		$lI3 = ((int)(($lI3 - $lI6)));
		label_9:
		$fI0 = $lI6;
		$lI6 = ((int)(($lI6 + -1)));
		if ((($fI0 <= 0))) goto label_10;
		$fA0 = $this;
		$fI1 = $this->_s1;
		$fA2 = $p0;
		$fI3 = $lI2;
		$lI2 = ((int)(($lI2 + 1)));
		$fA0->_s1 = ((int)(($fI1 + ((int)((($fA2->get($fI3)) & 255))))));
		$tA9 = $this;
		$tA9->_s2 = ((int)(($tA9->_s2 + $this->_s1)));
		goto label_9;
		label_10:
		$tA10 = $this;
		$tA10->_s1 = ((int)(N::irem($tA10->_s1, 65521)));
		$this->_s2 = ((int)(N::irem($this->_s2, 65521)));
		return;
	}
	public function __construct($CLASS_ID = 901) {
		parent::__construct($CLASS_ID);
		$this->_s1 = 0;
		$this->_s2 = 0;
	}
	static public function SI() {
	}
}
class java_lang_Throwable extends java_lang_Object implements java_io_Serializable {

	public $_thrown = false;
	public static $_EMPTY_ARRAY = null;
	public $_message = null;
	public $_writableStackTrace = false;
	public $_enableSuppression = false;
	public $_cause = null;
	public $_stackTrace = null;
	public $_supressed = null;
	public function prepareThrow__Ljava_lang_Throwable_() {
		if ($this->_thrown) goto label_1;
		$this->init_exception__V();
		label_1:
		$this->_thrown = true;
		return $this;
	}
	public function init_exception__V() {
		return;
	}
	public static function java_lang_Throwable_clinit___V() {
		java_lang_Throwable::$_EMPTY_ARRAY = new JA_L(0, "[Ljava.lang.Throwable;");
		return;
	}
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_14)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->_message)->toString__Ljava_lang_String_();
	}
	public function java_lang_Throwable_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_Object_init___V();
		$this->_enableSuppression = false;
		$this->_writableStackTrace = false;
		$this->_thrown = false;
		$this->init_Ljava_lang_String_Ljava_lang_Throwable_ZZ_V($p0, null, false, false);
		return $this;
		return $this;
	}
	public function init_Ljava_lang_String_Ljava_lang_Throwable_ZZ_V(?java_lang_String $p0, ?java_lang_Throwable $p1, bool $p2, bool $p3) {
		$this->_message = $p0;
		$this->_cause = $p1;
		$this->_enableSuppression = $p2;
		$this->_writableStackTrace = $p3;
		return;
	}
	public function java_lang_Throwable_init___V() {
		($this)->java_lang_Object_init___V();
		$this->_enableSuppression = false;
		$this->_writableStackTrace = false;
		$this->_thrown = false;
		$this->init_Ljava_lang_String_Ljava_lang_Throwable_ZZ_V(null, null, false, false);
		return $this;
		return $this;
	}
	public function java_lang_Throwable_init__Ljava_lang_Throwable__V(?java_lang_Throwable $p0) {
		($this)->java_lang_Object_init___V();
		$this->_enableSuppression = false;
		$this->_writableStackTrace = false;
		$this->_thrown = false;
		$this->init_Ljava_lang_String_Ljava_lang_Throwable_ZZ_V(null, $p0, false, false);
		return $this;
		return $this;
	}
	public function java_lang_Throwable_init__Ljava_lang_String_Ljava_lang_Throwable__V(?java_lang_String $p0, ?java_lang_Throwable $p1) {
		($this)->java_lang_Object_init___V();
		$this->_enableSuppression = false;
		$this->_writableStackTrace = false;
		$this->_thrown = false;
		$this->init_Ljava_lang_String_Ljava_lang_Throwable_ZZ_V($p0, $p1, false, false);
		return $this;
		return $this;
	}
	public function printStackTrace__V() {
		$lA1 = null;
		$lA2 = null;
		$lA5 = null;
		$lI3 = 0;
		$lI4 = 0;
		$fA0 = null;
		$tA0 = null;
		com_jtransc_io_JTranscConsole::error_Ljava_lang_Object__V(($this));
		$lA1 = ($this->getStackTraceLazy___Ljava_lang_StackTraceElement_());
		$lA2 = $lA1;
		$lI3 = ($lA2)->length;
		$lI4 = 0;
		label_1:
		if ((($lI4 >= $lI3))) goto label_2;
		$lA5 = (($lA2)->get($lI4));
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		com_jtransc_io_JTranscConsole::error_Ljava_lang_Object__V((($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_15)->append_Ljava_lang_Object__Ljava_lang_StringBuilder_($lA5)->toString__Ljava_lang_String_()));
		$lI4 = ((int)(($lI4 + 1)));
		goto label_1;
		label_2:
		$lA2 = ($this->getSuppressed___Ljava_lang_Throwable_());
		$lI3 = ($lA2)->length;
		$lI4 = 0;
		label_3:
		if ((($lI4 >= $lI3))) goto label_4;
		$lA5 = (($lA2)->get($lI4));
		com_jtransc_io_JTranscConsole::error_Ljava_lang_Object__V((Bootstrap::$STRINGLIT_16));
		($lA5)->printStackTrace__V();
		$lI4 = ((int)(($lI4 + 1)));
		goto label_3;
		label_4:
		$lA2 = ($this->getCause__Ljava_lang_Throwable_());
		if ((($lA2 == null))) goto label_5;
		com_jtransc_io_JTranscConsole::error_Ljava_lang_Object__V((Bootstrap::$STRINGLIT_17));
		($lA2)->printStackTrace__V();
		label_5:
		return;
	}
	public function getStackTraceLazy___Ljava_lang_StackTraceElement_() {
		if ((($this->_stackTrace != null))) goto label_1;
		$this->fillInStackTrace_I_V(0);
		label_1:
		return $this->_stackTrace;
	}
	public function fillInStackTrace_I_V(int $p0) {
		if (!($this->_thrown)) goto label_1;
		$this->genStackTraceFromError__V();
		goto label_3;
		label_1:
		$this->genStackTrace__V();
		label_3:
		$this->setStackTrace__Ljava_lang_StackTraceElement__V($this->getStackTraceInternal___Ljava_lang_StackTraceElement_());
		return;
	}
	public function genStackTrace__V() {
		return;
	}
	public function getStackTraceInternal___Ljava_lang_StackTraceElement_() {
		$fI2 = 0;
		$fA0 = null;
		$fA1 = null;
		$fA3 = null;
		$tA0 = null;
		$tA1 = null;
		$tA3 = null;
		$tA5 = null;
		$tA0 = (new JA_L(3, "[Ljava.lang.StackTraceElement;"));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$fI2 = 0;
		$tA1 = ((new java_lang_StackTraceElement()));
		$fA3 = $tA1;
		($tA1)->java_lang_StackTraceElement_init__Ljava_lang_String_Ljava_lang_String_Ljava_lang_String_I_V(Bootstrap::$STRINGLIT_18, Bootstrap::$STRINGLIT_19, Bootstrap::$STRINGLIT_20, 1);
		($fA1)->set($fI2, $fA3);
		$fA1 = $fA0;
		$fI2 = 1;
		$tA3 = ((new java_lang_StackTraceElement()));
		$fA3 = $tA3;
		($tA3)->java_lang_StackTraceElement_init__Ljava_lang_String_Ljava_lang_String_Ljava_lang_String_I_V(Bootstrap::$STRINGLIT_18, Bootstrap::$STRINGLIT_19, Bootstrap::$STRINGLIT_20, 1);
		($fA1)->set($fI2, $fA3);
		$fA1 = $fA0;
		$fI2 = 2;
		$tA5 = ((new java_lang_StackTraceElement()));
		$fA3 = $tA5;
		($tA5)->java_lang_StackTraceElement_init__Ljava_lang_String_Ljava_lang_String_Ljava_lang_String_I_V(Bootstrap::$STRINGLIT_18, Bootstrap::$STRINGLIT_19, Bootstrap::$STRINGLIT_20, 1);
		($fA1)->set($fI2, $fA3);
		return ($fA0);
	}
	public function setStackTrace__Ljava_lang_StackTraceElement__V(?JA_L $p0) {
		$this->_stackTrace = N::checkcast(($p0)->clone__Ljava_lang_Object_(), "JA_L");
		return;
	}
	public function getMessage__Ljava_lang_String_() {
		return $this->_message;
	}
	public function genStackTraceFromError__V() {
		return;
	}
	public function getSuppressed___Ljava_lang_Throwable_() {
		$fA0 = null;
		if ((($this->_supressed == null))) goto label_1;
		$fA0 = N::checkcast($this->_supressed->toArray__Ljava_lang_Object___Ljava_lang_Object_((java_lang_Throwable::$_EMPTY_ARRAY)), "JA_L");
		goto label_2;
		label_1:
		$fA0 = java_lang_Throwable::$_EMPTY_ARRAY;
		label_2:
		return $fA0;
	}
	public function getCause__Ljava_lang_Throwable_() {
		return $this->_cause;
	}
	public function initCause_Ljava_lang_Throwable__Ljava_lang_Throwable_(?java_lang_Throwable $p0) {
		$this->_cause = $p0;
		return $this->_cause;
	}
	public function __construct($CLASS_ID = 671) {
		parent::__construct($CLASS_ID);
		$this->_thrown = false;
		$this->_message = null;
		$this->_writableStackTrace = false;
		$this->_enableSuppression = false;
		$this->_cause = null;
		$this->_stackTrace = null;
		$this->_supressed = null;
	}
	static public function SI() {
		java_lang_Throwable::$_EMPTY_ARRAY = null;
		java_lang_Throwable::java_lang_Throwable_clinit___V();
	}
}
class java_lang_Exception extends java_lang_Throwable {

	public function java_lang_Exception_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_Throwable_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function java_lang_Exception_init___V() {
		($this)->java_lang_Throwable_init___V();
		return $this;
		return $this;
	}
	public function java_lang_Exception_init__Ljava_lang_Throwable__V(?java_lang_Throwable $p0) {
		($this)->java_lang_Throwable_init__Ljava_lang_Throwable__V($p0);
		return $this;
		return $this;
	}
	public function java_lang_Exception_init__Ljava_lang_String_Ljava_lang_Throwable__V(?java_lang_String $p0, ?java_lang_Throwable $p1) {
		($this)->java_lang_Throwable_init__Ljava_lang_String_Ljava_lang_Throwable__V($p0, $p1);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 670) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_RuntimeException extends java_lang_Exception {

	public function java_lang_RuntimeException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_Exception_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function java_lang_RuntimeException_init___V() {
		($this)->java_lang_Exception_init___V();
		return $this;
		return $this;
	}
	public function java_lang_RuntimeException_init__Ljava_lang_Throwable__V(?java_lang_Throwable $p0) {
		($this)->java_lang_Exception_init__Ljava_lang_Throwable__V($p0);
		return $this;
		return $this;
	}
	public function java_lang_RuntimeException_init__Ljava_lang_String_Ljava_lang_Throwable__V(?java_lang_String $p0, ?java_lang_Throwable $p1) {
		($this)->java_lang_Exception_init__Ljava_lang_String_Ljava_lang_Throwable__V($p0, $p1);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 669) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_compression_jzlib_GZIPException extends java_lang_RuntimeException {

	public function com_jtransc_compression_jzlib_GZIPException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 900) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_compression_jzlib_StaticTree extends java_lang_Object {

	public $_elems = 0;
	public $_max_length = 0;
	public $_extra_base = 0;
	public $_static_tree = null;
	public $_extra_bits = null;
	public static $_static_ltree = null;
	public static $_static_dtree = null;
	public static $_static_bl_desc = null;
	public static $_static_d_desc = null;
	public static $_static_l_desc = null;
	public function com_jtransc_compression_jzlib_StaticTree_init___S_IIII_V(?JA_S $p0, ?JA_I $p1, int $p2, int $p3, int $p4) {
		($this)->java_lang_Object_init___V();
		$this->_static_tree = $p0;
		$this->_extra_bits = $p1;
		$this->_extra_base = $p2;
		$this->_elems = $p3;
		$this->_max_length = $p4;
		return $this;
		return $this;
	}
	public static function com_jtransc_compression_jzlib_StaticTree_clinit___V() {
		$tA636 = null;
		$tA637 = null;
		$tA638 = null;
		$fA0 = null;
		$tA576 = null;
		$tA0 = null;
		$tA0 = (new JA_S(576));
		$fA0 = $tA0;
		($tA0)->set(0, 12);
		($fA0)->set(1, 8);
		($fA0)->set(2, 140);
		($fA0)->set(3, 8);
		($fA0)->set(4, 76);
		($fA0)->set(5, 8);
		($fA0)->set(6, 204);
		($fA0)->set(7, 8);
		($fA0)->set(8, 44);
		($fA0)->set(9, 8);
		($fA0)->set(10, 172);
		($fA0)->set(11, 8);
		($fA0)->set(12, 108);
		($fA0)->set(13, 8);
		($fA0)->set(14, 236);
		($fA0)->set(15, 8);
		($fA0)->set(16, 28);
		($fA0)->set(17, 8);
		($fA0)->set(18, 156);
		($fA0)->set(19, 8);
		($fA0)->set(20, 92);
		($fA0)->set(21, 8);
		($fA0)->set(22, 220);
		($fA0)->set(23, 8);
		($fA0)->set(24, 60);
		($fA0)->set(25, 8);
		($fA0)->set(26, 188);
		($fA0)->set(27, 8);
		($fA0)->set(28, 124);
		($fA0)->set(29, 8);
		($fA0)->set(30, 252);
		($fA0)->set(31, 8);
		($fA0)->set(32, 2);
		($fA0)->set(33, 8);
		($fA0)->set(34, 130);
		($fA0)->set(35, 8);
		($fA0)->set(36, 66);
		($fA0)->set(37, 8);
		($fA0)->set(38, 194);
		($fA0)->set(39, 8);
		($fA0)->set(40, 34);
		($fA0)->set(41, 8);
		($fA0)->set(42, 162);
		($fA0)->set(43, 8);
		($fA0)->set(44, 98);
		($fA0)->set(45, 8);
		($fA0)->set(46, 226);
		($fA0)->set(47, 8);
		($fA0)->set(48, 18);
		($fA0)->set(49, 8);
		($fA0)->set(50, 146);
		($fA0)->set(51, 8);
		($fA0)->set(52, 82);
		($fA0)->set(53, 8);
		($fA0)->set(54, 210);
		($fA0)->set(55, 8);
		($fA0)->set(56, 50);
		($fA0)->set(57, 8);
		($fA0)->set(58, 178);
		($fA0)->set(59, 8);
		($fA0)->set(60, 114);
		($fA0)->set(61, 8);
		($fA0)->set(62, 242);
		($fA0)->set(63, 8);
		($fA0)->set(64, 10);
		($fA0)->set(65, 8);
		($fA0)->set(66, 138);
		($fA0)->set(67, 8);
		($fA0)->set(68, 74);
		($fA0)->set(69, 8);
		($fA0)->set(70, 202);
		($fA0)->set(71, 8);
		($fA0)->set(72, 42);
		($fA0)->set(73, 8);
		($fA0)->set(74, 170);
		($fA0)->set(75, 8);
		($fA0)->set(76, 106);
		($fA0)->set(77, 8);
		($fA0)->set(78, 234);
		($fA0)->set(79, 8);
		($fA0)->set(80, 26);
		($fA0)->set(81, 8);
		($fA0)->set(82, 154);
		($fA0)->set(83, 8);
		($fA0)->set(84, 90);
		($fA0)->set(85, 8);
		($fA0)->set(86, 218);
		($fA0)->set(87, 8);
		($fA0)->set(88, 58);
		($fA0)->set(89, 8);
		($fA0)->set(90, 186);
		($fA0)->set(91, 8);
		($fA0)->set(92, 122);
		($fA0)->set(93, 8);
		($fA0)->set(94, 250);
		($fA0)->set(95, 8);
		($fA0)->set(96, 6);
		($fA0)->set(97, 8);
		($fA0)->set(98, 134);
		($fA0)->set(99, 8);
		($fA0)->set(100, 70);
		($fA0)->set(101, 8);
		($fA0)->set(102, 198);
		($fA0)->set(103, 8);
		($fA0)->set(104, 38);
		($fA0)->set(105, 8);
		($fA0)->set(106, 166);
		($fA0)->set(107, 8);
		($fA0)->set(108, 102);
		($fA0)->set(109, 8);
		($fA0)->set(110, 230);
		($fA0)->set(111, 8);
		($fA0)->set(112, 22);
		($fA0)->set(113, 8);
		($fA0)->set(114, 150);
		($fA0)->set(115, 8);
		($fA0)->set(116, 86);
		($fA0)->set(117, 8);
		($fA0)->set(118, 214);
		($fA0)->set(119, 8);
		($fA0)->set(120, 54);
		($fA0)->set(121, 8);
		($fA0)->set(122, 182);
		($fA0)->set(123, 8);
		($fA0)->set(124, 118);
		($fA0)->set(125, 8);
		($fA0)->set(126, 246);
		($fA0)->set(127, 8);
		($fA0)->set(128, 14);
		($fA0)->set(129, 8);
		($fA0)->set(130, 142);
		($fA0)->set(131, 8);
		($fA0)->set(132, 78);
		($fA0)->set(133, 8);
		($fA0)->set(134, 206);
		($fA0)->set(135, 8);
		($fA0)->set(136, 46);
		($fA0)->set(137, 8);
		($fA0)->set(138, 174);
		($fA0)->set(139, 8);
		($fA0)->set(140, 110);
		($fA0)->set(141, 8);
		($fA0)->set(142, 238);
		($fA0)->set(143, 8);
		($fA0)->set(144, 30);
		($fA0)->set(145, 8);
		($fA0)->set(146, 158);
		($fA0)->set(147, 8);
		($fA0)->set(148, 94);
		($fA0)->set(149, 8);
		($fA0)->set(150, 222);
		($fA0)->set(151, 8);
		($fA0)->set(152, 62);
		($fA0)->set(153, 8);
		($fA0)->set(154, 190);
		($fA0)->set(155, 8);
		($fA0)->set(156, 126);
		($fA0)->set(157, 8);
		($fA0)->set(158, 254);
		($fA0)->set(159, 8);
		($fA0)->set(160, 1);
		($fA0)->set(161, 8);
		($fA0)->set(162, 129);
		($fA0)->set(163, 8);
		($fA0)->set(164, 65);
		($fA0)->set(165, 8);
		($fA0)->set(166, 193);
		($fA0)->set(167, 8);
		($fA0)->set(168, 33);
		($fA0)->set(169, 8);
		($fA0)->set(170, 161);
		($fA0)->set(171, 8);
		($fA0)->set(172, 97);
		($fA0)->set(173, 8);
		($fA0)->set(174, 225);
		($fA0)->set(175, 8);
		($fA0)->set(176, 17);
		($fA0)->set(177, 8);
		($fA0)->set(178, 145);
		($fA0)->set(179, 8);
		($fA0)->set(180, 81);
		($fA0)->set(181, 8);
		($fA0)->set(182, 209);
		($fA0)->set(183, 8);
		($fA0)->set(184, 49);
		($fA0)->set(185, 8);
		($fA0)->set(186, 177);
		($fA0)->set(187, 8);
		($fA0)->set(188, 113);
		($fA0)->set(189, 8);
		($fA0)->set(190, 241);
		($fA0)->set(191, 8);
		($fA0)->set(192, 9);
		($fA0)->set(193, 8);
		($fA0)->set(194, 137);
		($fA0)->set(195, 8);
		($fA0)->set(196, 73);
		($fA0)->set(197, 8);
		($fA0)->set(198, 201);
		($fA0)->set(199, 8);
		($fA0)->set(200, 41);
		($fA0)->set(201, 8);
		($fA0)->set(202, 169);
		($fA0)->set(203, 8);
		($fA0)->set(204, 105);
		($fA0)->set(205, 8);
		($fA0)->set(206, 233);
		($fA0)->set(207, 8);
		($fA0)->set(208, 25);
		($fA0)->set(209, 8);
		($fA0)->set(210, 153);
		($fA0)->set(211, 8);
		($fA0)->set(212, 89);
		($fA0)->set(213, 8);
		($fA0)->set(214, 217);
		($fA0)->set(215, 8);
		($fA0)->set(216, 57);
		($fA0)->set(217, 8);
		($fA0)->set(218, 185);
		($fA0)->set(219, 8);
		($fA0)->set(220, 121);
		($fA0)->set(221, 8);
		($fA0)->set(222, 249);
		($fA0)->set(223, 8);
		($fA0)->set(224, 5);
		($fA0)->set(225, 8);
		($fA0)->set(226, 133);
		($fA0)->set(227, 8);
		($fA0)->set(228, 69);
		($fA0)->set(229, 8);
		($fA0)->set(230, 197);
		($fA0)->set(231, 8);
		($fA0)->set(232, 37);
		($fA0)->set(233, 8);
		($fA0)->set(234, 165);
		($fA0)->set(235, 8);
		($fA0)->set(236, 101);
		($fA0)->set(237, 8);
		($fA0)->set(238, 229);
		($fA0)->set(239, 8);
		($fA0)->set(240, 21);
		($fA0)->set(241, 8);
		($fA0)->set(242, 149);
		($fA0)->set(243, 8);
		($fA0)->set(244, 85);
		($fA0)->set(245, 8);
		($fA0)->set(246, 213);
		($fA0)->set(247, 8);
		($fA0)->set(248, 53);
		($fA0)->set(249, 8);
		($fA0)->set(250, 181);
		($fA0)->set(251, 8);
		($fA0)->set(252, 117);
		($fA0)->set(253, 8);
		($fA0)->set(254, 245);
		($fA0)->set(255, 8);
		($fA0)->set(256, 13);
		($fA0)->set(257, 8);
		($fA0)->set(258, 141);
		($fA0)->set(259, 8);
		($fA0)->set(260, 77);
		($fA0)->set(261, 8);
		($fA0)->set(262, 205);
		($fA0)->set(263, 8);
		($fA0)->set(264, 45);
		($fA0)->set(265, 8);
		($fA0)->set(266, 173);
		($fA0)->set(267, 8);
		($fA0)->set(268, 109);
		($fA0)->set(269, 8);
		($fA0)->set(270, 237);
		($fA0)->set(271, 8);
		($fA0)->set(272, 29);
		($fA0)->set(273, 8);
		($fA0)->set(274, 157);
		($fA0)->set(275, 8);
		($fA0)->set(276, 93);
		($fA0)->set(277, 8);
		($fA0)->set(278, 221);
		($fA0)->set(279, 8);
		($fA0)->set(280, 61);
		($fA0)->set(281, 8);
		($fA0)->set(282, 189);
		($fA0)->set(283, 8);
		($fA0)->set(284, 125);
		($fA0)->set(285, 8);
		($fA0)->set(286, 253);
		($fA0)->set(287, 8);
		($fA0)->set(288, 19);
		($fA0)->set(289, 9);
		($fA0)->set(290, 275);
		($fA0)->set(291, 9);
		($fA0)->set(292, 147);
		($fA0)->set(293, 9);
		($fA0)->set(294, 403);
		($fA0)->set(295, 9);
		($fA0)->set(296, 83);
		($fA0)->set(297, 9);
		($fA0)->set(298, 339);
		($fA0)->set(299, 9);
		($fA0)->set(300, 211);
		($fA0)->set(301, 9);
		($fA0)->set(302, 467);
		($fA0)->set(303, 9);
		($fA0)->set(304, 51);
		($fA0)->set(305, 9);
		($fA0)->set(306, 307);
		($fA0)->set(307, 9);
		($fA0)->set(308, 179);
		($fA0)->set(309, 9);
		($fA0)->set(310, 435);
		($fA0)->set(311, 9);
		($fA0)->set(312, 115);
		($fA0)->set(313, 9);
		($fA0)->set(314, 371);
		($fA0)->set(315, 9);
		($fA0)->set(316, 243);
		($fA0)->set(317, 9);
		($fA0)->set(318, 499);
		($fA0)->set(319, 9);
		($fA0)->set(320, 11);
		($fA0)->set(321, 9);
		($fA0)->set(322, 267);
		($fA0)->set(323, 9);
		($fA0)->set(324, 139);
		($fA0)->set(325, 9);
		($fA0)->set(326, 395);
		($fA0)->set(327, 9);
		($fA0)->set(328, 75);
		($fA0)->set(329, 9);
		($fA0)->set(330, 331);
		($fA0)->set(331, 9);
		($fA0)->set(332, 203);
		($fA0)->set(333, 9);
		($fA0)->set(334, 459);
		($fA0)->set(335, 9);
		($fA0)->set(336, 43);
		($fA0)->set(337, 9);
		($fA0)->set(338, 299);
		($fA0)->set(339, 9);
		($fA0)->set(340, 171);
		($fA0)->set(341, 9);
		($fA0)->set(342, 427);
		($fA0)->set(343, 9);
		($fA0)->set(344, 107);
		($fA0)->set(345, 9);
		($fA0)->set(346, 363);
		($fA0)->set(347, 9);
		($fA0)->set(348, 235);
		($fA0)->set(349, 9);
		($fA0)->set(350, 491);
		($fA0)->set(351, 9);
		($fA0)->set(352, 27);
		($fA0)->set(353, 9);
		($fA0)->set(354, 283);
		($fA0)->set(355, 9);
		($fA0)->set(356, 155);
		($fA0)->set(357, 9);
		($fA0)->set(358, 411);
		($fA0)->set(359, 9);
		($fA0)->set(360, 91);
		($fA0)->set(361, 9);
		($fA0)->set(362, 347);
		($fA0)->set(363, 9);
		($fA0)->set(364, 219);
		($fA0)->set(365, 9);
		($fA0)->set(366, 475);
		($fA0)->set(367, 9);
		($fA0)->set(368, 59);
		($fA0)->set(369, 9);
		($fA0)->set(370, 315);
		($fA0)->set(371, 9);
		($fA0)->set(372, 187);
		($fA0)->set(373, 9);
		($fA0)->set(374, 443);
		($fA0)->set(375, 9);
		($fA0)->set(376, 123);
		($fA0)->set(377, 9);
		($fA0)->set(378, 379);
		($fA0)->set(379, 9);
		($fA0)->set(380, 251);
		($fA0)->set(381, 9);
		($fA0)->set(382, 507);
		($fA0)->set(383, 9);
		($fA0)->set(384, 7);
		($fA0)->set(385, 9);
		($fA0)->set(386, 263);
		($fA0)->set(387, 9);
		($fA0)->set(388, 135);
		($fA0)->set(389, 9);
		($fA0)->set(390, 391);
		($fA0)->set(391, 9);
		($fA0)->set(392, 71);
		($fA0)->set(393, 9);
		($fA0)->set(394, 327);
		($fA0)->set(395, 9);
		($fA0)->set(396, 199);
		($fA0)->set(397, 9);
		($fA0)->set(398, 455);
		($fA0)->set(399, 9);
		($fA0)->set(400, 39);
		($fA0)->set(401, 9);
		($fA0)->set(402, 295);
		($fA0)->set(403, 9);
		($fA0)->set(404, 167);
		($fA0)->set(405, 9);
		($fA0)->set(406, 423);
		($fA0)->set(407, 9);
		($fA0)->set(408, 103);
		($fA0)->set(409, 9);
		($fA0)->set(410, 359);
		($fA0)->set(411, 9);
		($fA0)->set(412, 231);
		($fA0)->set(413, 9);
		($fA0)->set(414, 487);
		($fA0)->set(415, 9);
		($fA0)->set(416, 23);
		($fA0)->set(417, 9);
		($fA0)->set(418, 279);
		($fA0)->set(419, 9);
		($fA0)->set(420, 151);
		($fA0)->set(421, 9);
		($fA0)->set(422, 407);
		($fA0)->set(423, 9);
		($fA0)->set(424, 87);
		($fA0)->set(425, 9);
		($fA0)->set(426, 343);
		($fA0)->set(427, 9);
		($fA0)->set(428, 215);
		($fA0)->set(429, 9);
		($fA0)->set(430, 471);
		($fA0)->set(431, 9);
		($fA0)->set(432, 55);
		($fA0)->set(433, 9);
		($fA0)->set(434, 311);
		($fA0)->set(435, 9);
		($fA0)->set(436, 183);
		($fA0)->set(437, 9);
		($fA0)->set(438, 439);
		($fA0)->set(439, 9);
		($fA0)->set(440, 119);
		($fA0)->set(441, 9);
		($fA0)->set(442, 375);
		($fA0)->set(443, 9);
		($fA0)->set(444, 247);
		($fA0)->set(445, 9);
		($fA0)->set(446, 503);
		($fA0)->set(447, 9);
		($fA0)->set(448, 15);
		($fA0)->set(449, 9);
		($fA0)->set(450, 271);
		($fA0)->set(451, 9);
		($fA0)->set(452, 143);
		($fA0)->set(453, 9);
		($fA0)->set(454, 399);
		($fA0)->set(455, 9);
		($fA0)->set(456, 79);
		($fA0)->set(457, 9);
		($fA0)->set(458, 335);
		($fA0)->set(459, 9);
		($fA0)->set(460, 207);
		($fA0)->set(461, 9);
		($fA0)->set(462, 463);
		($fA0)->set(463, 9);
		($fA0)->set(464, 47);
		($fA0)->set(465, 9);
		($fA0)->set(466, 303);
		($fA0)->set(467, 9);
		($fA0)->set(468, 175);
		($fA0)->set(469, 9);
		($fA0)->set(470, 431);
		($fA0)->set(471, 9);
		($fA0)->set(472, 111);
		($fA0)->set(473, 9);
		($fA0)->set(474, 367);
		($fA0)->set(475, 9);
		($fA0)->set(476, 239);
		($fA0)->set(477, 9);
		($fA0)->set(478, 495);
		($fA0)->set(479, 9);
		($fA0)->set(480, 31);
		($fA0)->set(481, 9);
		($fA0)->set(482, 287);
		($fA0)->set(483, 9);
		($fA0)->set(484, 159);
		($fA0)->set(485, 9);
		($fA0)->set(486, 415);
		($fA0)->set(487, 9);
		($fA0)->set(488, 95);
		($fA0)->set(489, 9);
		($fA0)->set(490, 351);
		($fA0)->set(491, 9);
		($fA0)->set(492, 223);
		($fA0)->set(493, 9);
		($fA0)->set(494, 479);
		($fA0)->set(495, 9);
		($fA0)->set(496, 63);
		($fA0)->set(497, 9);
		($fA0)->set(498, 319);
		($fA0)->set(499, 9);
		($fA0)->set(500, 191);
		($fA0)->set(501, 9);
		($fA0)->set(502, 447);
		($fA0)->set(503, 9);
		($fA0)->set(504, 127);
		($fA0)->set(505, 9);
		($fA0)->set(506, 383);
		($fA0)->set(507, 9);
		($fA0)->set(508, 255);
		($fA0)->set(509, 9);
		($fA0)->set(510, 511);
		($fA0)->set(511, 9);
		($fA0)->set(512, 0);
		($fA0)->set(513, 7);
		($fA0)->set(514, 64);
		($fA0)->set(515, 7);
		($fA0)->set(516, 32);
		($fA0)->set(517, 7);
		($fA0)->set(518, 96);
		($fA0)->set(519, 7);
		($fA0)->set(520, 16);
		($fA0)->set(521, 7);
		($fA0)->set(522, 80);
		($fA0)->set(523, 7);
		($fA0)->set(524, 48);
		($fA0)->set(525, 7);
		($fA0)->set(526, 112);
		($fA0)->set(527, 7);
		($fA0)->set(528, 8);
		($fA0)->set(529, 7);
		($fA0)->set(530, 72);
		($fA0)->set(531, 7);
		($fA0)->set(532, 40);
		($fA0)->set(533, 7);
		($fA0)->set(534, 104);
		($fA0)->set(535, 7);
		($fA0)->set(536, 24);
		($fA0)->set(537, 7);
		($fA0)->set(538, 88);
		($fA0)->set(539, 7);
		($fA0)->set(540, 56);
		($fA0)->set(541, 7);
		($fA0)->set(542, 120);
		($fA0)->set(543, 7);
		($fA0)->set(544, 4);
		($fA0)->set(545, 7);
		($fA0)->set(546, 68);
		($fA0)->set(547, 7);
		($fA0)->set(548, 36);
		($fA0)->set(549, 7);
		($fA0)->set(550, 100);
		($fA0)->set(551, 7);
		($fA0)->set(552, 20);
		($fA0)->set(553, 7);
		($fA0)->set(554, 84);
		($fA0)->set(555, 7);
		($fA0)->set(556, 52);
		($fA0)->set(557, 7);
		($fA0)->set(558, 116);
		($fA0)->set(559, 7);
		($fA0)->set(560, 3);
		($fA0)->set(561, 8);
		($fA0)->set(562, 131);
		($fA0)->set(563, 8);
		($fA0)->set(564, 67);
		($fA0)->set(565, 8);
		($fA0)->set(566, 195);
		($fA0)->set(567, 8);
		($fA0)->set(568, 35);
		($fA0)->set(569, 8);
		($fA0)->set(570, 163);
		($fA0)->set(571, 8);
		($fA0)->set(572, 99);
		($fA0)->set(573, 8);
		($fA0)->set(574, 227);
		($fA0)->set(575, 8);
		com_jtransc_compression_jzlib_StaticTree::$_static_ltree = ($fA0);
		$tA576 = (new JA_S(60));
		$fA0 = $tA576;
		($tA576)->set(0, 0);
		($fA0)->set(1, 5);
		($fA0)->set(2, 16);
		($fA0)->set(3, 5);
		($fA0)->set(4, 8);
		($fA0)->set(5, 5);
		($fA0)->set(6, 24);
		($fA0)->set(7, 5);
		($fA0)->set(8, 4);
		($fA0)->set(9, 5);
		($fA0)->set(10, 20);
		($fA0)->set(11, 5);
		($fA0)->set(12, 12);
		($fA0)->set(13, 5);
		($fA0)->set(14, 28);
		($fA0)->set(15, 5);
		($fA0)->set(16, 2);
		($fA0)->set(17, 5);
		($fA0)->set(18, 18);
		($fA0)->set(19, 5);
		($fA0)->set(20, 10);
		($fA0)->set(21, 5);
		($fA0)->set(22, 26);
		($fA0)->set(23, 5);
		($fA0)->set(24, 6);
		($fA0)->set(25, 5);
		($fA0)->set(26, 22);
		($fA0)->set(27, 5);
		($fA0)->set(28, 14);
		($fA0)->set(29, 5);
		($fA0)->set(30, 30);
		($fA0)->set(31, 5);
		($fA0)->set(32, 1);
		($fA0)->set(33, 5);
		($fA0)->set(34, 17);
		($fA0)->set(35, 5);
		($fA0)->set(36, 9);
		($fA0)->set(37, 5);
		($fA0)->set(38, 25);
		($fA0)->set(39, 5);
		($fA0)->set(40, 5);
		($fA0)->set(41, 5);
		($fA0)->set(42, 21);
		($fA0)->set(43, 5);
		($fA0)->set(44, 13);
		($fA0)->set(45, 5);
		($fA0)->set(46, 29);
		($fA0)->set(47, 5);
		($fA0)->set(48, 3);
		($fA0)->set(49, 5);
		($fA0)->set(50, 19);
		($fA0)->set(51, 5);
		($fA0)->set(52, 11);
		($fA0)->set(53, 5);
		($fA0)->set(54, 27);
		($fA0)->set(55, 5);
		($fA0)->set(56, 7);
		($fA0)->set(57, 5);
		($fA0)->set(58, 23);
		($fA0)->set(59, 5);
		com_jtransc_compression_jzlib_StaticTree::$_static_dtree = ($fA0);
		$tA636 = ((new com_jtransc_compression_jzlib_StaticTree()));
		$fA0 = $tA636;
		($tA636)->com_jtransc_compression_jzlib_StaticTree_init___S_IIII_V(com_jtransc_compression_jzlib_StaticTree::$_static_ltree, com_jtransc_compression_jzlib_Tree::$_extra_lbits, 257, 286, 15);
		com_jtransc_compression_jzlib_StaticTree::$_static_l_desc = ($fA0);
		$tA637 = ((new com_jtransc_compression_jzlib_StaticTree()));
		$fA0 = $tA637;
		($tA637)->com_jtransc_compression_jzlib_StaticTree_init___S_IIII_V(com_jtransc_compression_jzlib_StaticTree::$_static_dtree, com_jtransc_compression_jzlib_Tree::$_extra_dbits, 0, 30, 15);
		com_jtransc_compression_jzlib_StaticTree::$_static_d_desc = ($fA0);
		$tA638 = ((new com_jtransc_compression_jzlib_StaticTree()));
		$fA0 = $tA638;
		($tA638)->com_jtransc_compression_jzlib_StaticTree_init___S_IIII_V(null, com_jtransc_compression_jzlib_Tree::$_extra_blbits, 0, 19, 7);
		com_jtransc_compression_jzlib_StaticTree::$_static_bl_desc = ($fA0);
		return;
	}
	public function __construct($CLASS_ID = 899) {
		parent::__construct($CLASS_ID);
		$this->_elems = 0;
		$this->_max_length = 0;
		$this->_extra_base = 0;
		$this->_static_tree = null;
		$this->_extra_bits = null;
	}
	static public function SI() {
		com_jtransc_compression_jzlib_StaticTree::$_static_ltree = null;
		com_jtransc_compression_jzlib_StaticTree::$_static_dtree = null;
		com_jtransc_compression_jzlib_StaticTree::$_static_bl_desc = null;
		com_jtransc_compression_jzlib_StaticTree::$_static_d_desc = null;
		com_jtransc_compression_jzlib_StaticTree::$_static_l_desc = null;
		com_jtransc_compression_jzlib_StaticTree::com_jtransc_compression_jzlib_StaticTree_clinit___V();
	}
}
class com_jtransc_compression_jzlib_Deflate_Config extends java_lang_Object {

	public $_nice_length = 0;
	public $_max_chain = 0;
	public $_max_lazy = 0;
	public $_good_length = 0;
	public $_func = 0;
	public function com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(int $p0, int $p1, int $p2, int $p3, int $p4) {
		($this)->java_lang_Object_init___V();
		$this->_good_length = $p0;
		$this->_max_lazy = $p1;
		$this->_nice_length = $p2;
		$this->_max_chain = $p3;
		$this->_func = $p4;
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 898) {
		parent::__construct($CLASS_ID);
		$this->_nice_length = 0;
		$this->_max_chain = 0;
		$this->_max_lazy = 0;
		$this->_good_length = 0;
		$this->_func = 0;
	}
	static public function SI() {
	}
}
class com_jtransc_compression_jzlib_GZIPHeader extends java_lang_Object implements java_lang_Cloneable {

	public $_text = false;
	public $_fhcrc = false;
	public $_mtime = null;
	public $_os = 0;
	public $_done = false;
	public $_comment = null;
	public $_extra = null;
	public $_name = null;
	public $_crc = null;
	public function com_jtransc_compression_jzlib_GZIPHeader_init___V() {
		($this)->java_lang_Object_init___V();
		$this->_text = false;
		$this->_fhcrc = false;
		$this->_os = 255;
		$this->_done = false;
		$this->_mtime = Int64::make(0, 0);
		return $this;
		return $this;
	}
	public function clone__Ljava_lang_Object_() {
		$lA1 = null;
		$lA2 = null;
		$lA1 = (N::checkcast(parent::clone__Ljava_lang_Object_(), "com_jtransc_compression_jzlib_GZIPHeader"));
		if (((($lA1)->_extra == null))) goto label_1;
		$lA2 = (new JA_B((($lA1)->_extra)->length));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V((($lA1)->_extra), 0, $lA2, 0, ($lA2)->length);
		($lA1)->_extra = ($lA2);
		label_1:
		if (((($lA1)->_name == null))) goto label_3;
		$lA2 = (new JA_B((($lA1)->_name)->length));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V((($lA1)->_name), 0, $lA2, 0, ($lA2)->length);
		($lA1)->_name = ($lA2);
		label_3:
		if (((($lA1)->_comment == null))) goto label_5;
		$lA2 = (new JA_B((($lA1)->_comment)->length));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V((($lA1)->_comment), 0, $lA2, 0, ($lA2)->length);
		($lA1)->_comment = ($lA2);
		label_5:
		return $lA1;
	}
	public function put_Lcom_jtransc_compression_jzlib_Deflate__V(?com_jtransc_compression_jzlib_Deflate $p0) {
		$lI2 = 0;
		$lI3 = 0;
		$lI2 = 0;
		if (!($this->_text)) goto label_1;
		$lI2 = ((int)(($lI2 | 1)));
		label_1:
		if (!($this->_fhcrc)) goto label_3;
		$lI2 = ((int)(($lI2 | 2)));
		label_3:
		if ((($this->_extra == null))) goto label_5;
		$lI2 = ((int)(($lI2 | 4)));
		label_5:
		if ((($this->_name == null))) goto label_7;
		$lI2 = ((int)(($lI2 | 8)));
		label_7:
		if ((($this->_comment == null))) goto label_9;
		$lI2 = ((int)(($lI2 | 16)));
		label_9:
		$lI3 = 0;
		if ((($p0->_level != 1))) goto label_11;
		$lI3 = ((int)(($lI3 | 4)));
		goto label_13;
		label_11:
		if ((($p0->_level != 9))) goto label_13;
		$lI3 = ((int)(($lI3 | 2)));
		label_13:
		$p0->put_short_I_V(-29921);
		$p0->put_byte_B_V(8);
		$p0->put_byte_B_V((N::i2b($lI2)));
		$p0->put_byte_B_V((N::i2b(N::j2i($this->_mtime))));
		$p0->put_byte_B_V((N::i2b(N::j2i((N::lshr($this->_mtime, 8))))));
		$p0->put_byte_B_V((N::i2b(N::j2i((N::lshr($this->_mtime, 16))))));
		$p0->put_byte_B_V((N::i2b(N::j2i((N::lshr($this->_mtime, 24))))));
		$p0->put_byte_B_V((N::i2b($lI3)));
		$p0->put_byte_B_V((N::i2b($this->_os)));
		if ((($this->_extra == null))) goto label_14;
		$p0->put_byte_B_V((N::i2b(($this->_extra)->length)));
		$p0->put_byte_B_V((N::i2b(((int)(N::ishr(($this->_extra)->length, 8))))));
		$p0->put_byte__BII_V($this->_extra, 0, ($this->_extra)->length);
		label_14:
		if ((($this->_name == null))) goto label_16;
		$p0->put_byte__BII_V($this->_name, 0, ($this->_name)->length);
		$p0->put_byte_B_V(0);
		label_16:
		if ((($this->_comment == null))) goto label_18;
		$p0->put_byte__BII_V($this->_comment, 0, ($this->_comment)->length);
		$p0->put_byte_B_V(0);
		label_18:
		return;
	}
	public function setCRC_J_V(Int64 $p0) {
		$this->_crc = $p0;
		return;
	}
	public function __construct($CLASS_ID = 897) {
		parent::__construct($CLASS_ID);
		$this->_text = false;
		$this->_fhcrc = false;
		$this->_mtime = Int64::make(0, 0);
		$this->_os = 0;
		$this->_done = false;
		$this->_comment = null;
		$this->_extra = null;
		$this->_name = null;
		$this->_crc = Int64::make(0, 0);
	}
	static public function SI() {
	}
}
class com_jtransc_compression_jzlib_Tree extends java_lang_Object {

	public static $_base_dist = null;
	public static $_extra_lbits = null;
	public static $__dist_code = null;
	public static $_extra_dbits = null;
	public static $__length_code = null;
	public static $_base_length = null;
	public static $_extra_blbits = null;
	public static $_bl_order = null;
	public $_dyn_tree = null;
	public $_max_code = 0;
	public $_stat_desc = null;
	public function com_jtransc_compression_jzlib_Tree_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function com_jtransc_compression_jzlib_Tree_clinit___V() {
		$tA97 = null;
		$tA609 = null;
		$fA0 = null;
		$tA29 = null;
		$tA59 = null;
		$tA78 = null;
		$tA865 = null;
		$tA894 = null;
		$tA0 = null;
		$tA0 = (new JA_I(29));
		$fA0 = $tA0;
		($tA0)->set(0, 0);
		($fA0)->set(1, 0);
		($fA0)->set(2, 0);
		($fA0)->set(3, 0);
		($fA0)->set(4, 0);
		($fA0)->set(5, 0);
		($fA0)->set(6, 0);
		($fA0)->set(7, 0);
		($fA0)->set(8, 1);
		($fA0)->set(9, 1);
		($fA0)->set(10, 1);
		($fA0)->set(11, 1);
		($fA0)->set(12, 2);
		($fA0)->set(13, 2);
		($fA0)->set(14, 2);
		($fA0)->set(15, 2);
		($fA0)->set(16, 3);
		($fA0)->set(17, 3);
		($fA0)->set(18, 3);
		($fA0)->set(19, 3);
		($fA0)->set(20, 4);
		($fA0)->set(21, 4);
		($fA0)->set(22, 4);
		($fA0)->set(23, 4);
		($fA0)->set(24, 5);
		($fA0)->set(25, 5);
		($fA0)->set(26, 5);
		($fA0)->set(27, 5);
		($fA0)->set(28, 0);
		com_jtransc_compression_jzlib_Tree::$_extra_lbits = ($fA0);
		$tA29 = (new JA_I(30));
		$fA0 = $tA29;
		($tA29)->set(0, 0);
		($fA0)->set(1, 0);
		($fA0)->set(2, 0);
		($fA0)->set(3, 0);
		($fA0)->set(4, 1);
		($fA0)->set(5, 1);
		($fA0)->set(6, 2);
		($fA0)->set(7, 2);
		($fA0)->set(8, 3);
		($fA0)->set(9, 3);
		($fA0)->set(10, 4);
		($fA0)->set(11, 4);
		($fA0)->set(12, 5);
		($fA0)->set(13, 5);
		($fA0)->set(14, 6);
		($fA0)->set(15, 6);
		($fA0)->set(16, 7);
		($fA0)->set(17, 7);
		($fA0)->set(18, 8);
		($fA0)->set(19, 8);
		($fA0)->set(20, 9);
		($fA0)->set(21, 9);
		($fA0)->set(22, 10);
		($fA0)->set(23, 10);
		($fA0)->set(24, 11);
		($fA0)->set(25, 11);
		($fA0)->set(26, 12);
		($fA0)->set(27, 12);
		($fA0)->set(28, 13);
		($fA0)->set(29, 13);
		com_jtransc_compression_jzlib_Tree::$_extra_dbits = ($fA0);
		$tA59 = (new JA_I(19));
		$fA0 = $tA59;
		($tA59)->set(0, 0);
		($fA0)->set(1, 0);
		($fA0)->set(2, 0);
		($fA0)->set(3, 0);
		($fA0)->set(4, 0);
		($fA0)->set(5, 0);
		($fA0)->set(6, 0);
		($fA0)->set(7, 0);
		($fA0)->set(8, 0);
		($fA0)->set(9, 0);
		($fA0)->set(10, 0);
		($fA0)->set(11, 0);
		($fA0)->set(12, 0);
		($fA0)->set(13, 0);
		($fA0)->set(14, 0);
		($fA0)->set(15, 0);
		($fA0)->set(16, 2);
		($fA0)->set(17, 3);
		($fA0)->set(18, 7);
		com_jtransc_compression_jzlib_Tree::$_extra_blbits = ($fA0);
		$tA78 = (new JA_B(19));
		$fA0 = $tA78;
		($tA78)->set(0, 16);
		($fA0)->set(1, 17);
		($fA0)->set(2, 18);
		($fA0)->set(3, 0);
		($fA0)->set(4, 8);
		($fA0)->set(5, 7);
		($fA0)->set(6, 9);
		($fA0)->set(7, 6);
		($fA0)->set(8, 10);
		($fA0)->set(9, 5);
		($fA0)->set(10, 11);
		($fA0)->set(11, 4);
		($fA0)->set(12, 12);
		($fA0)->set(13, 3);
		($fA0)->set(14, 13);
		($fA0)->set(15, 2);
		($fA0)->set(16, 14);
		($fA0)->set(17, 1);
		($fA0)->set(18, 15);
		com_jtransc_compression_jzlib_Tree::$_bl_order = ($fA0);
		$tA97 = (new JA_B(512));
		$fA0 = $tA97;
		($tA97)->set(0, 0);
		($fA0)->set(1, 1);
		($fA0)->set(2, 2);
		($fA0)->set(3, 3);
		($fA0)->set(4, 4);
		($fA0)->set(5, 4);
		($fA0)->set(6, 5);
		($fA0)->set(7, 5);
		($fA0)->set(8, 6);
		($fA0)->set(9, 6);
		($fA0)->set(10, 6);
		($fA0)->set(11, 6);
		($fA0)->set(12, 7);
		($fA0)->set(13, 7);
		($fA0)->set(14, 7);
		($fA0)->set(15, 7);
		($fA0)->set(16, 8);
		($fA0)->set(17, 8);
		($fA0)->set(18, 8);
		($fA0)->set(19, 8);
		($fA0)->set(20, 8);
		($fA0)->set(21, 8);
		($fA0)->set(22, 8);
		($fA0)->set(23, 8);
		($fA0)->set(24, 9);
		($fA0)->set(25, 9);
		($fA0)->set(26, 9);
		($fA0)->set(27, 9);
		($fA0)->set(28, 9);
		($fA0)->set(29, 9);
		($fA0)->set(30, 9);
		($fA0)->set(31, 9);
		($fA0)->set(32, 10);
		($fA0)->set(33, 10);
		($fA0)->set(34, 10);
		($fA0)->set(35, 10);
		($fA0)->set(36, 10);
		($fA0)->set(37, 10);
		($fA0)->set(38, 10);
		($fA0)->set(39, 10);
		($fA0)->set(40, 10);
		($fA0)->set(41, 10);
		($fA0)->set(42, 10);
		($fA0)->set(43, 10);
		($fA0)->set(44, 10);
		($fA0)->set(45, 10);
		($fA0)->set(46, 10);
		($fA0)->set(47, 10);
		($fA0)->set(48, 11);
		($fA0)->set(49, 11);
		($fA0)->set(50, 11);
		($fA0)->set(51, 11);
		($fA0)->set(52, 11);
		($fA0)->set(53, 11);
		($fA0)->set(54, 11);
		($fA0)->set(55, 11);
		($fA0)->set(56, 11);
		($fA0)->set(57, 11);
		($fA0)->set(58, 11);
		($fA0)->set(59, 11);
		($fA0)->set(60, 11);
		($fA0)->set(61, 11);
		($fA0)->set(62, 11);
		($fA0)->set(63, 11);
		($fA0)->set(64, 12);
		($fA0)->set(65, 12);
		($fA0)->set(66, 12);
		($fA0)->set(67, 12);
		($fA0)->set(68, 12);
		($fA0)->set(69, 12);
		($fA0)->set(70, 12);
		($fA0)->set(71, 12);
		($fA0)->set(72, 12);
		($fA0)->set(73, 12);
		($fA0)->set(74, 12);
		($fA0)->set(75, 12);
		($fA0)->set(76, 12);
		($fA0)->set(77, 12);
		($fA0)->set(78, 12);
		($fA0)->set(79, 12);
		($fA0)->set(80, 12);
		($fA0)->set(81, 12);
		($fA0)->set(82, 12);
		($fA0)->set(83, 12);
		($fA0)->set(84, 12);
		($fA0)->set(85, 12);
		($fA0)->set(86, 12);
		($fA0)->set(87, 12);
		($fA0)->set(88, 12);
		($fA0)->set(89, 12);
		($fA0)->set(90, 12);
		($fA0)->set(91, 12);
		($fA0)->set(92, 12);
		($fA0)->set(93, 12);
		($fA0)->set(94, 12);
		($fA0)->set(95, 12);
		($fA0)->set(96, 13);
		($fA0)->set(97, 13);
		($fA0)->set(98, 13);
		($fA0)->set(99, 13);
		($fA0)->set(100, 13);
		($fA0)->set(101, 13);
		($fA0)->set(102, 13);
		($fA0)->set(103, 13);
		($fA0)->set(104, 13);
		($fA0)->set(105, 13);
		($fA0)->set(106, 13);
		($fA0)->set(107, 13);
		($fA0)->set(108, 13);
		($fA0)->set(109, 13);
		($fA0)->set(110, 13);
		($fA0)->set(111, 13);
		($fA0)->set(112, 13);
		($fA0)->set(113, 13);
		($fA0)->set(114, 13);
		($fA0)->set(115, 13);
		($fA0)->set(116, 13);
		($fA0)->set(117, 13);
		($fA0)->set(118, 13);
		($fA0)->set(119, 13);
		($fA0)->set(120, 13);
		($fA0)->set(121, 13);
		($fA0)->set(122, 13);
		($fA0)->set(123, 13);
		($fA0)->set(124, 13);
		($fA0)->set(125, 13);
		($fA0)->set(126, 13);
		($fA0)->set(127, 13);
		($fA0)->set(128, 14);
		($fA0)->set(129, 14);
		($fA0)->set(130, 14);
		($fA0)->set(131, 14);
		($fA0)->set(132, 14);
		($fA0)->set(133, 14);
		($fA0)->set(134, 14);
		($fA0)->set(135, 14);
		($fA0)->set(136, 14);
		($fA0)->set(137, 14);
		($fA0)->set(138, 14);
		($fA0)->set(139, 14);
		($fA0)->set(140, 14);
		($fA0)->set(141, 14);
		($fA0)->set(142, 14);
		($fA0)->set(143, 14);
		($fA0)->set(144, 14);
		($fA0)->set(145, 14);
		($fA0)->set(146, 14);
		($fA0)->set(147, 14);
		($fA0)->set(148, 14);
		($fA0)->set(149, 14);
		($fA0)->set(150, 14);
		($fA0)->set(151, 14);
		($fA0)->set(152, 14);
		($fA0)->set(153, 14);
		($fA0)->set(154, 14);
		($fA0)->set(155, 14);
		($fA0)->set(156, 14);
		($fA0)->set(157, 14);
		($fA0)->set(158, 14);
		($fA0)->set(159, 14);
		($fA0)->set(160, 14);
		($fA0)->set(161, 14);
		($fA0)->set(162, 14);
		($fA0)->set(163, 14);
		($fA0)->set(164, 14);
		($fA0)->set(165, 14);
		($fA0)->set(166, 14);
		($fA0)->set(167, 14);
		($fA0)->set(168, 14);
		($fA0)->set(169, 14);
		($fA0)->set(170, 14);
		($fA0)->set(171, 14);
		($fA0)->set(172, 14);
		($fA0)->set(173, 14);
		($fA0)->set(174, 14);
		($fA0)->set(175, 14);
		($fA0)->set(176, 14);
		($fA0)->set(177, 14);
		($fA0)->set(178, 14);
		($fA0)->set(179, 14);
		($fA0)->set(180, 14);
		($fA0)->set(181, 14);
		($fA0)->set(182, 14);
		($fA0)->set(183, 14);
		($fA0)->set(184, 14);
		($fA0)->set(185, 14);
		($fA0)->set(186, 14);
		($fA0)->set(187, 14);
		($fA0)->set(188, 14);
		($fA0)->set(189, 14);
		($fA0)->set(190, 14);
		($fA0)->set(191, 14);
		($fA0)->set(192, 15);
		($fA0)->set(193, 15);
		($fA0)->set(194, 15);
		($fA0)->set(195, 15);
		($fA0)->set(196, 15);
		($fA0)->set(197, 15);
		($fA0)->set(198, 15);
		($fA0)->set(199, 15);
		($fA0)->set(200, 15);
		($fA0)->set(201, 15);
		($fA0)->set(202, 15);
		($fA0)->set(203, 15);
		($fA0)->set(204, 15);
		($fA0)->set(205, 15);
		($fA0)->set(206, 15);
		($fA0)->set(207, 15);
		($fA0)->set(208, 15);
		($fA0)->set(209, 15);
		($fA0)->set(210, 15);
		($fA0)->set(211, 15);
		($fA0)->set(212, 15);
		($fA0)->set(213, 15);
		($fA0)->set(214, 15);
		($fA0)->set(215, 15);
		($fA0)->set(216, 15);
		($fA0)->set(217, 15);
		($fA0)->set(218, 15);
		($fA0)->set(219, 15);
		($fA0)->set(220, 15);
		($fA0)->set(221, 15);
		($fA0)->set(222, 15);
		($fA0)->set(223, 15);
		($fA0)->set(224, 15);
		($fA0)->set(225, 15);
		($fA0)->set(226, 15);
		($fA0)->set(227, 15);
		($fA0)->set(228, 15);
		($fA0)->set(229, 15);
		($fA0)->set(230, 15);
		($fA0)->set(231, 15);
		($fA0)->set(232, 15);
		($fA0)->set(233, 15);
		($fA0)->set(234, 15);
		($fA0)->set(235, 15);
		($fA0)->set(236, 15);
		($fA0)->set(237, 15);
		($fA0)->set(238, 15);
		($fA0)->set(239, 15);
		($fA0)->set(240, 15);
		($fA0)->set(241, 15);
		($fA0)->set(242, 15);
		($fA0)->set(243, 15);
		($fA0)->set(244, 15);
		($fA0)->set(245, 15);
		($fA0)->set(246, 15);
		($fA0)->set(247, 15);
		($fA0)->set(248, 15);
		($fA0)->set(249, 15);
		($fA0)->set(250, 15);
		($fA0)->set(251, 15);
		($fA0)->set(252, 15);
		($fA0)->set(253, 15);
		($fA0)->set(254, 15);
		($fA0)->set(255, 15);
		($fA0)->set(256, 0);
		($fA0)->set(257, 0);
		($fA0)->set(258, 16);
		($fA0)->set(259, 17);
		($fA0)->set(260, 18);
		($fA0)->set(261, 18);
		($fA0)->set(262, 19);
		($fA0)->set(263, 19);
		($fA0)->set(264, 20);
		($fA0)->set(265, 20);
		($fA0)->set(266, 20);
		($fA0)->set(267, 20);
		($fA0)->set(268, 21);
		($fA0)->set(269, 21);
		($fA0)->set(270, 21);
		($fA0)->set(271, 21);
		($fA0)->set(272, 22);
		($fA0)->set(273, 22);
		($fA0)->set(274, 22);
		($fA0)->set(275, 22);
		($fA0)->set(276, 22);
		($fA0)->set(277, 22);
		($fA0)->set(278, 22);
		($fA0)->set(279, 22);
		($fA0)->set(280, 23);
		($fA0)->set(281, 23);
		($fA0)->set(282, 23);
		($fA0)->set(283, 23);
		($fA0)->set(284, 23);
		($fA0)->set(285, 23);
		($fA0)->set(286, 23);
		($fA0)->set(287, 23);
		($fA0)->set(288, 24);
		($fA0)->set(289, 24);
		($fA0)->set(290, 24);
		($fA0)->set(291, 24);
		($fA0)->set(292, 24);
		($fA0)->set(293, 24);
		($fA0)->set(294, 24);
		($fA0)->set(295, 24);
		($fA0)->set(296, 24);
		($fA0)->set(297, 24);
		($fA0)->set(298, 24);
		($fA0)->set(299, 24);
		($fA0)->set(300, 24);
		($fA0)->set(301, 24);
		($fA0)->set(302, 24);
		($fA0)->set(303, 24);
		($fA0)->set(304, 25);
		($fA0)->set(305, 25);
		($fA0)->set(306, 25);
		($fA0)->set(307, 25);
		($fA0)->set(308, 25);
		($fA0)->set(309, 25);
		($fA0)->set(310, 25);
		($fA0)->set(311, 25);
		($fA0)->set(312, 25);
		($fA0)->set(313, 25);
		($fA0)->set(314, 25);
		($fA0)->set(315, 25);
		($fA0)->set(316, 25);
		($fA0)->set(317, 25);
		($fA0)->set(318, 25);
		($fA0)->set(319, 25);
		($fA0)->set(320, 26);
		($fA0)->set(321, 26);
		($fA0)->set(322, 26);
		($fA0)->set(323, 26);
		($fA0)->set(324, 26);
		($fA0)->set(325, 26);
		($fA0)->set(326, 26);
		($fA0)->set(327, 26);
		($fA0)->set(328, 26);
		($fA0)->set(329, 26);
		($fA0)->set(330, 26);
		($fA0)->set(331, 26);
		($fA0)->set(332, 26);
		($fA0)->set(333, 26);
		($fA0)->set(334, 26);
		($fA0)->set(335, 26);
		($fA0)->set(336, 26);
		($fA0)->set(337, 26);
		($fA0)->set(338, 26);
		($fA0)->set(339, 26);
		($fA0)->set(340, 26);
		($fA0)->set(341, 26);
		($fA0)->set(342, 26);
		($fA0)->set(343, 26);
		($fA0)->set(344, 26);
		($fA0)->set(345, 26);
		($fA0)->set(346, 26);
		($fA0)->set(347, 26);
		($fA0)->set(348, 26);
		($fA0)->set(349, 26);
		($fA0)->set(350, 26);
		($fA0)->set(351, 26);
		($fA0)->set(352, 27);
		($fA0)->set(353, 27);
		($fA0)->set(354, 27);
		($fA0)->set(355, 27);
		($fA0)->set(356, 27);
		($fA0)->set(357, 27);
		($fA0)->set(358, 27);
		($fA0)->set(359, 27);
		($fA0)->set(360, 27);
		($fA0)->set(361, 27);
		($fA0)->set(362, 27);
		($fA0)->set(363, 27);
		($fA0)->set(364, 27);
		($fA0)->set(365, 27);
		($fA0)->set(366, 27);
		($fA0)->set(367, 27);
		($fA0)->set(368, 27);
		($fA0)->set(369, 27);
		($fA0)->set(370, 27);
		($fA0)->set(371, 27);
		($fA0)->set(372, 27);
		($fA0)->set(373, 27);
		($fA0)->set(374, 27);
		($fA0)->set(375, 27);
		($fA0)->set(376, 27);
		($fA0)->set(377, 27);
		($fA0)->set(378, 27);
		($fA0)->set(379, 27);
		($fA0)->set(380, 27);
		($fA0)->set(381, 27);
		($fA0)->set(382, 27);
		($fA0)->set(383, 27);
		($fA0)->set(384, 28);
		($fA0)->set(385, 28);
		($fA0)->set(386, 28);
		($fA0)->set(387, 28);
		($fA0)->set(388, 28);
		($fA0)->set(389, 28);
		($fA0)->set(390, 28);
		($fA0)->set(391, 28);
		($fA0)->set(392, 28);
		($fA0)->set(393, 28);
		($fA0)->set(394, 28);
		($fA0)->set(395, 28);
		($fA0)->set(396, 28);
		($fA0)->set(397, 28);
		($fA0)->set(398, 28);
		($fA0)->set(399, 28);
		($fA0)->set(400, 28);
		($fA0)->set(401, 28);
		($fA0)->set(402, 28);
		($fA0)->set(403, 28);
		($fA0)->set(404, 28);
		($fA0)->set(405, 28);
		($fA0)->set(406, 28);
		($fA0)->set(407, 28);
		($fA0)->set(408, 28);
		($fA0)->set(409, 28);
		($fA0)->set(410, 28);
		($fA0)->set(411, 28);
		($fA0)->set(412, 28);
		($fA0)->set(413, 28);
		($fA0)->set(414, 28);
		($fA0)->set(415, 28);
		($fA0)->set(416, 28);
		($fA0)->set(417, 28);
		($fA0)->set(418, 28);
		($fA0)->set(419, 28);
		($fA0)->set(420, 28);
		($fA0)->set(421, 28);
		($fA0)->set(422, 28);
		($fA0)->set(423, 28);
		($fA0)->set(424, 28);
		($fA0)->set(425, 28);
		($fA0)->set(426, 28);
		($fA0)->set(427, 28);
		($fA0)->set(428, 28);
		($fA0)->set(429, 28);
		($fA0)->set(430, 28);
		($fA0)->set(431, 28);
		($fA0)->set(432, 28);
		($fA0)->set(433, 28);
		($fA0)->set(434, 28);
		($fA0)->set(435, 28);
		($fA0)->set(436, 28);
		($fA0)->set(437, 28);
		($fA0)->set(438, 28);
		($fA0)->set(439, 28);
		($fA0)->set(440, 28);
		($fA0)->set(441, 28);
		($fA0)->set(442, 28);
		($fA0)->set(443, 28);
		($fA0)->set(444, 28);
		($fA0)->set(445, 28);
		($fA0)->set(446, 28);
		($fA0)->set(447, 28);
		($fA0)->set(448, 29);
		($fA0)->set(449, 29);
		($fA0)->set(450, 29);
		($fA0)->set(451, 29);
		($fA0)->set(452, 29);
		($fA0)->set(453, 29);
		($fA0)->set(454, 29);
		($fA0)->set(455, 29);
		($fA0)->set(456, 29);
		($fA0)->set(457, 29);
		($fA0)->set(458, 29);
		($fA0)->set(459, 29);
		($fA0)->set(460, 29);
		($fA0)->set(461, 29);
		($fA0)->set(462, 29);
		($fA0)->set(463, 29);
		($fA0)->set(464, 29);
		($fA0)->set(465, 29);
		($fA0)->set(466, 29);
		($fA0)->set(467, 29);
		($fA0)->set(468, 29);
		($fA0)->set(469, 29);
		($fA0)->set(470, 29);
		($fA0)->set(471, 29);
		($fA0)->set(472, 29);
		($fA0)->set(473, 29);
		($fA0)->set(474, 29);
		($fA0)->set(475, 29);
		($fA0)->set(476, 29);
		($fA0)->set(477, 29);
		($fA0)->set(478, 29);
		($fA0)->set(479, 29);
		($fA0)->set(480, 29);
		($fA0)->set(481, 29);
		($fA0)->set(482, 29);
		($fA0)->set(483, 29);
		($fA0)->set(484, 29);
		($fA0)->set(485, 29);
		($fA0)->set(486, 29);
		($fA0)->set(487, 29);
		($fA0)->set(488, 29);
		($fA0)->set(489, 29);
		($fA0)->set(490, 29);
		($fA0)->set(491, 29);
		($fA0)->set(492, 29);
		($fA0)->set(493, 29);
		($fA0)->set(494, 29);
		($fA0)->set(495, 29);
		($fA0)->set(496, 29);
		($fA0)->set(497, 29);
		($fA0)->set(498, 29);
		($fA0)->set(499, 29);
		($fA0)->set(500, 29);
		($fA0)->set(501, 29);
		($fA0)->set(502, 29);
		($fA0)->set(503, 29);
		($fA0)->set(504, 29);
		($fA0)->set(505, 29);
		($fA0)->set(506, 29);
		($fA0)->set(507, 29);
		($fA0)->set(508, 29);
		($fA0)->set(509, 29);
		($fA0)->set(510, 29);
		($fA0)->set(511, 29);
		com_jtransc_compression_jzlib_Tree::$__dist_code = ($fA0);
		$tA609 = (new JA_B(256));
		$fA0 = $tA609;
		($tA609)->set(0, 0);
		($fA0)->set(1, 1);
		($fA0)->set(2, 2);
		($fA0)->set(3, 3);
		($fA0)->set(4, 4);
		($fA0)->set(5, 5);
		($fA0)->set(6, 6);
		($fA0)->set(7, 7);
		($fA0)->set(8, 8);
		($fA0)->set(9, 8);
		($fA0)->set(10, 9);
		($fA0)->set(11, 9);
		($fA0)->set(12, 10);
		($fA0)->set(13, 10);
		($fA0)->set(14, 11);
		($fA0)->set(15, 11);
		($fA0)->set(16, 12);
		($fA0)->set(17, 12);
		($fA0)->set(18, 12);
		($fA0)->set(19, 12);
		($fA0)->set(20, 13);
		($fA0)->set(21, 13);
		($fA0)->set(22, 13);
		($fA0)->set(23, 13);
		($fA0)->set(24, 14);
		($fA0)->set(25, 14);
		($fA0)->set(26, 14);
		($fA0)->set(27, 14);
		($fA0)->set(28, 15);
		($fA0)->set(29, 15);
		($fA0)->set(30, 15);
		($fA0)->set(31, 15);
		($fA0)->set(32, 16);
		($fA0)->set(33, 16);
		($fA0)->set(34, 16);
		($fA0)->set(35, 16);
		($fA0)->set(36, 16);
		($fA0)->set(37, 16);
		($fA0)->set(38, 16);
		($fA0)->set(39, 16);
		($fA0)->set(40, 17);
		($fA0)->set(41, 17);
		($fA0)->set(42, 17);
		($fA0)->set(43, 17);
		($fA0)->set(44, 17);
		($fA0)->set(45, 17);
		($fA0)->set(46, 17);
		($fA0)->set(47, 17);
		($fA0)->set(48, 18);
		($fA0)->set(49, 18);
		($fA0)->set(50, 18);
		($fA0)->set(51, 18);
		($fA0)->set(52, 18);
		($fA0)->set(53, 18);
		($fA0)->set(54, 18);
		($fA0)->set(55, 18);
		($fA0)->set(56, 19);
		($fA0)->set(57, 19);
		($fA0)->set(58, 19);
		($fA0)->set(59, 19);
		($fA0)->set(60, 19);
		($fA0)->set(61, 19);
		($fA0)->set(62, 19);
		($fA0)->set(63, 19);
		($fA0)->set(64, 20);
		($fA0)->set(65, 20);
		($fA0)->set(66, 20);
		($fA0)->set(67, 20);
		($fA0)->set(68, 20);
		($fA0)->set(69, 20);
		($fA0)->set(70, 20);
		($fA0)->set(71, 20);
		($fA0)->set(72, 20);
		($fA0)->set(73, 20);
		($fA0)->set(74, 20);
		($fA0)->set(75, 20);
		($fA0)->set(76, 20);
		($fA0)->set(77, 20);
		($fA0)->set(78, 20);
		($fA0)->set(79, 20);
		($fA0)->set(80, 21);
		($fA0)->set(81, 21);
		($fA0)->set(82, 21);
		($fA0)->set(83, 21);
		($fA0)->set(84, 21);
		($fA0)->set(85, 21);
		($fA0)->set(86, 21);
		($fA0)->set(87, 21);
		($fA0)->set(88, 21);
		($fA0)->set(89, 21);
		($fA0)->set(90, 21);
		($fA0)->set(91, 21);
		($fA0)->set(92, 21);
		($fA0)->set(93, 21);
		($fA0)->set(94, 21);
		($fA0)->set(95, 21);
		($fA0)->set(96, 22);
		($fA0)->set(97, 22);
		($fA0)->set(98, 22);
		($fA0)->set(99, 22);
		($fA0)->set(100, 22);
		($fA0)->set(101, 22);
		($fA0)->set(102, 22);
		($fA0)->set(103, 22);
		($fA0)->set(104, 22);
		($fA0)->set(105, 22);
		($fA0)->set(106, 22);
		($fA0)->set(107, 22);
		($fA0)->set(108, 22);
		($fA0)->set(109, 22);
		($fA0)->set(110, 22);
		($fA0)->set(111, 22);
		($fA0)->set(112, 23);
		($fA0)->set(113, 23);
		($fA0)->set(114, 23);
		($fA0)->set(115, 23);
		($fA0)->set(116, 23);
		($fA0)->set(117, 23);
		($fA0)->set(118, 23);
		($fA0)->set(119, 23);
		($fA0)->set(120, 23);
		($fA0)->set(121, 23);
		($fA0)->set(122, 23);
		($fA0)->set(123, 23);
		($fA0)->set(124, 23);
		($fA0)->set(125, 23);
		($fA0)->set(126, 23);
		($fA0)->set(127, 23);
		($fA0)->set(128, 24);
		($fA0)->set(129, 24);
		($fA0)->set(130, 24);
		($fA0)->set(131, 24);
		($fA0)->set(132, 24);
		($fA0)->set(133, 24);
		($fA0)->set(134, 24);
		($fA0)->set(135, 24);
		($fA0)->set(136, 24);
		($fA0)->set(137, 24);
		($fA0)->set(138, 24);
		($fA0)->set(139, 24);
		($fA0)->set(140, 24);
		($fA0)->set(141, 24);
		($fA0)->set(142, 24);
		($fA0)->set(143, 24);
		($fA0)->set(144, 24);
		($fA0)->set(145, 24);
		($fA0)->set(146, 24);
		($fA0)->set(147, 24);
		($fA0)->set(148, 24);
		($fA0)->set(149, 24);
		($fA0)->set(150, 24);
		($fA0)->set(151, 24);
		($fA0)->set(152, 24);
		($fA0)->set(153, 24);
		($fA0)->set(154, 24);
		($fA0)->set(155, 24);
		($fA0)->set(156, 24);
		($fA0)->set(157, 24);
		($fA0)->set(158, 24);
		($fA0)->set(159, 24);
		($fA0)->set(160, 25);
		($fA0)->set(161, 25);
		($fA0)->set(162, 25);
		($fA0)->set(163, 25);
		($fA0)->set(164, 25);
		($fA0)->set(165, 25);
		($fA0)->set(166, 25);
		($fA0)->set(167, 25);
		($fA0)->set(168, 25);
		($fA0)->set(169, 25);
		($fA0)->set(170, 25);
		($fA0)->set(171, 25);
		($fA0)->set(172, 25);
		($fA0)->set(173, 25);
		($fA0)->set(174, 25);
		($fA0)->set(175, 25);
		($fA0)->set(176, 25);
		($fA0)->set(177, 25);
		($fA0)->set(178, 25);
		($fA0)->set(179, 25);
		($fA0)->set(180, 25);
		($fA0)->set(181, 25);
		($fA0)->set(182, 25);
		($fA0)->set(183, 25);
		($fA0)->set(184, 25);
		($fA0)->set(185, 25);
		($fA0)->set(186, 25);
		($fA0)->set(187, 25);
		($fA0)->set(188, 25);
		($fA0)->set(189, 25);
		($fA0)->set(190, 25);
		($fA0)->set(191, 25);
		($fA0)->set(192, 26);
		($fA0)->set(193, 26);
		($fA0)->set(194, 26);
		($fA0)->set(195, 26);
		($fA0)->set(196, 26);
		($fA0)->set(197, 26);
		($fA0)->set(198, 26);
		($fA0)->set(199, 26);
		($fA0)->set(200, 26);
		($fA0)->set(201, 26);
		($fA0)->set(202, 26);
		($fA0)->set(203, 26);
		($fA0)->set(204, 26);
		($fA0)->set(205, 26);
		($fA0)->set(206, 26);
		($fA0)->set(207, 26);
		($fA0)->set(208, 26);
		($fA0)->set(209, 26);
		($fA0)->set(210, 26);
		($fA0)->set(211, 26);
		($fA0)->set(212, 26);
		($fA0)->set(213, 26);
		($fA0)->set(214, 26);
		($fA0)->set(215, 26);
		($fA0)->set(216, 26);
		($fA0)->set(217, 26);
		($fA0)->set(218, 26);
		($fA0)->set(219, 26);
		($fA0)->set(220, 26);
		($fA0)->set(221, 26);
		($fA0)->set(222, 26);
		($fA0)->set(223, 26);
		($fA0)->set(224, 27);
		($fA0)->set(225, 27);
		($fA0)->set(226, 27);
		($fA0)->set(227, 27);
		($fA0)->set(228, 27);
		($fA0)->set(229, 27);
		($fA0)->set(230, 27);
		($fA0)->set(231, 27);
		($fA0)->set(232, 27);
		($fA0)->set(233, 27);
		($fA0)->set(234, 27);
		($fA0)->set(235, 27);
		($fA0)->set(236, 27);
		($fA0)->set(237, 27);
		($fA0)->set(238, 27);
		($fA0)->set(239, 27);
		($fA0)->set(240, 27);
		($fA0)->set(241, 27);
		($fA0)->set(242, 27);
		($fA0)->set(243, 27);
		($fA0)->set(244, 27);
		($fA0)->set(245, 27);
		($fA0)->set(246, 27);
		($fA0)->set(247, 27);
		($fA0)->set(248, 27);
		($fA0)->set(249, 27);
		($fA0)->set(250, 27);
		($fA0)->set(251, 27);
		($fA0)->set(252, 27);
		($fA0)->set(253, 27);
		($fA0)->set(254, 27);
		($fA0)->set(255, 28);
		com_jtransc_compression_jzlib_Tree::$__length_code = ($fA0);
		$tA865 = (new JA_I(29));
		$fA0 = $tA865;
		($tA865)->set(0, 0);
		($fA0)->set(1, 1);
		($fA0)->set(2, 2);
		($fA0)->set(3, 3);
		($fA0)->set(4, 4);
		($fA0)->set(5, 5);
		($fA0)->set(6, 6);
		($fA0)->set(7, 7);
		($fA0)->set(8, 8);
		($fA0)->set(9, 10);
		($fA0)->set(10, 12);
		($fA0)->set(11, 14);
		($fA0)->set(12, 16);
		($fA0)->set(13, 20);
		($fA0)->set(14, 24);
		($fA0)->set(15, 28);
		($fA0)->set(16, 32);
		($fA0)->set(17, 40);
		($fA0)->set(18, 48);
		($fA0)->set(19, 56);
		($fA0)->set(20, 64);
		($fA0)->set(21, 80);
		($fA0)->set(22, 96);
		($fA0)->set(23, 112);
		($fA0)->set(24, 128);
		($fA0)->set(25, 160);
		($fA0)->set(26, 192);
		($fA0)->set(27, 224);
		($fA0)->set(28, 0);
		com_jtransc_compression_jzlib_Tree::$_base_length = ($fA0);
		$tA894 = (new JA_I(30));
		$fA0 = $tA894;
		($tA894)->set(0, 0);
		($fA0)->set(1, 1);
		($fA0)->set(2, 2);
		($fA0)->set(3, 3);
		($fA0)->set(4, 4);
		($fA0)->set(5, 6);
		($fA0)->set(6, 8);
		($fA0)->set(7, 12);
		($fA0)->set(8, 16);
		($fA0)->set(9, 24);
		($fA0)->set(10, 32);
		($fA0)->set(11, 48);
		($fA0)->set(12, 64);
		($fA0)->set(13, 96);
		($fA0)->set(14, 128);
		($fA0)->set(15, 192);
		($fA0)->set(16, 256);
		($fA0)->set(17, 384);
		($fA0)->set(18, 512);
		($fA0)->set(19, 768);
		($fA0)->set(20, 1024);
		($fA0)->set(21, 1536);
		($fA0)->set(22, 2048);
		($fA0)->set(23, 3072);
		($fA0)->set(24, 4096);
		($fA0)->set(25, 6144);
		($fA0)->set(26, 8192);
		($fA0)->set(27, 12288);
		($fA0)->set(28, 16384);
		($fA0)->set(29, 24576);
		com_jtransc_compression_jzlib_Tree::$_base_dist = ($fA0);
		return;
	}
	public function build_tree_Lcom_jtransc_compression_jzlib_Deflate__V(?com_jtransc_compression_jzlib_Deflate $p0) {
		$lA3 = null;
		$lI4 = 0;
		$lI8 = 0;
		$lI6 = 0;
		$fA1 = null;
		$fA3 = null;
		$tI21 = 0;
		$tI25 = 0;
		$tA9 = null;
		$fI2 = 0;
		$fI0 = 0;
		$tA20 = null;
		$tA26 = null;
		$tI8 = 0;
		$tA14 = null;
		$tI16 = 0;
		$lA2 = null;
		$lI7 = 0;
		$lI5 = 0;
		$fA0 = null;
		$fA2 = null;
		$tI22 = 0;
		$tA2 = null;
		$tA6 = null;
		$fI1 = 0;
		$fI3 = 0;
		$tA23 = null;
		$tI1 = 0;
		$tI3 = 0;
		$tI5 = 0;
		$tI7 = 0;
		$tI13 = 0;
		$tA17 = null;
		$tI19 = 0;
		$lA2 = ($this->_dyn_tree);
		$lA3 = ($this->_stat_desc->_static_tree);
		$lI4 = $this->_stat_desc->_elems;
		$lI7 = -1;
		$p0->_heap_len = 0;
		$p0->_heap_max = 573;
		$lI5 = 0;
		label_1:
		if ((($lI5 >= $lI4))) goto label_2;
		if ((((($lA2)->get(((int)(N::imul($lI5, 2))))) == 0))) goto label_4;
		$fA0 = ($p0->_heap);
		$fA1 = ($p0);
		$tA2 = $fA1;
		$tI1 = ((int)(($p0->_heap_len + 1)));
		$fI1 = $tI1;
		($tA2)->_heap_len = $tI1;
		$tI3 = $lI5;
		$fI2 = $tI3;
		$lI7 = $tI3;
		($fA0)->set($fI1, $fI2);
		$p0->_depth->set($lI5, 0);
		goto label_6;
		label_4:
		($lA2)->set(((int)((((int)(N::imul($lI5, 2))) + 1))), 0);
		label_6:
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		if ((($p0->_heap_len >= 2))) goto label_7;
		$fA0 = ($p0->_heap);
		$fA1 = ($p0);
		$tA6 = $fA1;
		$tI5 = ((int)(($p0->_heap_len + 1)));
		$fI1 = $tI5;
		($tA6)->_heap_len = $tI5;
		if ((($lI7 >= 2))) goto label_9;
		$lI7 = ((int)(($lI7 + 1)));
		$fI2 = $lI7;
		goto label_10;
		label_9:
		$fI2 = 0;
		label_10:
		$tA9 = $fA0;
		$tI8 = $fI1;
		$tI7 = $fI2;
		$fI0 = $tI7;
		($tA9)->set($tI8, $tI7);
		$lI8 = $fI0;
		($lA2)->set(((int)(N::imul($lI8, 2))), 1);
		$p0->_depth->set($lI8, 0);
		$p0->_opt_len = ((int)(($p0->_opt_len - 1)));
		if ((($lA3 == null))) goto label_2;
		$p0->_static_len = ((int)(($p0->_static_len - (($lA3)->get(((int)((((int)(N::imul($lI8, 2))) + 1))))))));
		goto label_2;
		label_7:
		$this->_max_code = $lI7;
		$lI5 = ((int)(N::idiv($p0->_heap_len, 2)));
		label_11:
		if ((($lI5 < 1))) goto label_12;
		$p0->pqdownheap__SI_V(($lA2), $lI5);
		$lI5 = ((int)(($lI5 + -1)));
		goto label_11;
		label_12:
		$lI8 = $lI4;
		label_14:
		$lI5 = ($p0->_heap->get(1));
		$fA0 = ($p0->_heap);
		$fI1 = 1;
		$fA2 = ($p0->_heap);
		$fA3 = ($p0);
		$tA14 = $fA3;
		$tI13 = $p0->_heap_len;
		$fI3 = $tI13;
		($tA14)->_heap_len = ((int)(($tI13 - 1)));
		($fA0)->set($fI1, (($fA2)->get($fI3)));
		$p0->pqdownheap__SI_V(($lA2), 1);
		$lI6 = ($p0->_heap->get(1));
		$fA0 = ($p0->_heap);
		$fA1 = ($p0);
		$tA17 = $fA1;
		$tI16 = ((int)(($p0->_heap_max - 1)));
		$fI1 = $tI16;
		($tA17)->_heap_max = $tI16;
		($fA0)->set($fI1, $lI5);
		$fA0 = ($p0->_heap);
		$fA1 = ($p0);
		$tA20 = $fA1;
		$tI19 = ((int)(($p0->_heap_max - 1)));
		$fI1 = $tI19;
		($tA20)->_heap_max = $tI19;
		($fA0)->set($fI1, $lI6);
		($lA2)->set(((int)(N::imul($lI8, 2))), (N::i2s(((int)(((($lA2)->get(((int)(N::imul($lI5, 2))))) + (($lA2)->get(((int)(N::imul($lI6, 2)))))))))));
		$p0->_depth->set($lI8, (N::i2b(((int)((java_lang_Math::max_II_I(((int)(($p0->_depth->get($lI5)))), ((int)(($p0->_depth->get($lI6))))) + 1))))));
		$fA0 = $lA2;
		$fI1 = ((int)((((int)(N::imul($lI5, 2))) + 1)));
		$fA2 = $lA2;
		$fI3 = ((int)((((int)(N::imul($lI6, 2))) + 1)));
		$tA23 = $fA2;
		$tI22 = $fI3;
		$tI21 = ((int)((N::i2s($lI8))));
		$fI2 = $tI21;
		($tA23)->set($tI22, (N::i2s($tI21)));
		($fA0)->set($fI1, (N::i2s($fI2)));
		$fA0 = ($p0->_heap);
		$fI1 = 1;
		$fI2 = $lI8;
		$lI8 = ((int)(($lI8 + 1)));
		($fA0)->set($fI1, $fI2);
		$p0->pqdownheap__SI_V(($lA2), 1);
		if ((($p0->_heap_len >= 2))) goto label_14;
		$fA0 = ($p0->_heap);
		$fA1 = ($p0);
		$tA26 = $fA1;
		$tI25 = ((int)(($p0->_heap_max - 1)));
		$fI1 = $tI25;
		($tA26)->_heap_max = $tI25;
		($fA0)->set($fI1, ($p0->_heap->get(1)));
		$this->gen_bitlen_Lcom_jtransc_compression_jzlib_Deflate__V($p0);
		com_jtransc_compression_jzlib_Tree::gen_codes__SI_S_S_V(($lA2), $lI7, $p0->_bl_count, $p0->_next_code);
		return;
	}
	public function gen_bitlen_Lcom_jtransc_compression_jzlib_Deflate__V(?com_jtransc_compression_jzlib_Deflate $p0) {
		$lA3 = null;
		$lI13 = 0;
		$lI11 = 0;
		$lI12 = 0;
		$lI5 = 0;
		$lI6 = 0;
		$lI10 = 0;
		$lI7 = 0;
		$lI8 = 0;
		$lI9 = 0;
		$tA1 = null;
		$tA5 = null;
		$tA7 = null;
		$tA9 = null;
		$lA2 = null;
		$tI0 = 0;
		$tI4 = 0;
		$tI6 = 0;
		$tI8 = 0;
		$fA0 = null;
		$lA4 = null;
		$lA2 = $this->_dyn_tree;
		$lA3 = ($this->_stat_desc->_static_tree);
		$lA4 = $this->_stat_desc->_extra_bits;
		$lI5 = $this->_stat_desc->_extra_base;
		$lI6 = $this->_stat_desc->_max_length;
		$lI13 = 0;
		$lI10 = 0;
		label_1:
		if ((($lI10 > 15))) goto label_2;
		$p0->_bl_count->set($lI10, 0);
		$lI10 = ((int)(($lI10 + 1)));
		goto label_1;
		label_2:
		$lA2->set(((int)((((int)(N::imul(($p0->_heap->get($p0->_heap_max)), 2))) + 1))), 0);
		$lI7 = ((int)(($p0->_heap_max + 1)));
		label_3:
		if ((($lI7 >= 573))) goto label_4;
		$lI8 = ($p0->_heap->get($lI7));
		$lI10 = ((int)((($lA2->get(((int)((((int)(N::imul(($lA2->get(((int)((((int)(N::imul($lI8, 2))) + 1))))), 2))) + 1))))) + 1)));
		if ((($lI10 <= $lI6))) goto label_6;
		$lI10 = $lI6;
		$lI13 = ((int)(($lI13 + 1)));
		label_6:
		$lA2->set(((int)((((int)(N::imul($lI8, 2))) + 1))), (N::i2s($lI10)));
		if ((($lI8 <= $this->_max_code))) goto label_7;
		goto label_8;
		label_7:
		$tA1 = $p0->_bl_count;
		$tI0 = $lI10;
		$tA1->set($tI0, (N::i2s(((int)((($tA1->get($tI0)) + 1))))));
		$lI11 = 0;
		if ((($lI8 < $lI5))) goto label_9;
		$lI11 = ($lA4->get(((int)(($lI8 - $lI5)))));
		label_9:
		$lI12 = ((int)(($lA2->get(((int)(N::imul($lI8, 2)))))));
		$p0->_opt_len = ((int)(($p0->_opt_len + ((int)(N::imul($lI12, ((int)(($lI10 + $lI11)))))))));
		if ((($lA3 == null))) goto label_8;
		$p0->_static_len = ((int)(($p0->_static_len + ((int)(N::imul($lI12, ((int)(((($lA3)->get(((int)((((int)(N::imul($lI8, 2))) + 1))))) + $lI11)))))))));
		label_8:
		$lI7 = ((int)(($lI7 + 1)));
		goto label_3;
		label_4:
		if ((($lI13 != 0))) goto label_10;
		return;
		label_10:
		$lI10 = ((int)(($lI6 - 1)));
		label_11:
		if (((($p0->_bl_count->get($lI10)) != 0))) goto label_12;
		$lI10 = ((int)(($lI10 + -1)));
		goto label_11;
		label_12:
		$tA5 = $p0->_bl_count;
		$tI4 = $lI10;
		$tA5->set($tI4, (N::i2s(((int)((($tA5->get($tI4)) - 1))))));
		$tA7 = $p0->_bl_count;
		$tI6 = ((int)(($lI10 + 1)));
		$tA7->set($tI6, (N::i2s(((int)((($tA7->get($tI6)) + 2))))));
		$tA9 = $p0->_bl_count;
		$tI8 = $lI6;
		$tA9->set($tI8, (N::i2s(((int)((($tA9->get($tI8)) - 1))))));
		$lI13 = ((int)(($lI13 + -2)));
		if ((($lI13 > 0))) goto label_10;
		$lI10 = $lI6;
		label_14:
		if ((($lI10 == 0))) goto label_15;
		$lI8 = ((int)(($p0->_bl_count->get($lI10))));
		label_17:
		if ((($lI8 == 0))) goto label_18;
		$fA0 = $p0->_heap;
		$lI7 = ((int)(($lI7 + -1)));
		$lI9 = ($fA0->get($lI7));
		if ((($lI9 <= $this->_max_code))) goto label_20;
		goto label_17;
		label_20:
		if (((($lA2->get(((int)((((int)(N::imul($lI9, 2))) + 1))))) == $lI10))) goto label_21;
		$p0->_opt_len = N::j2i((N::ladd(N::i2j($p0->_opt_len), (N::lmul((N::lsub(N::i2j($lI10), N::i2j(($lA2->get(((int)((((int)(N::imul($lI9, 2))) + 1)))))))), N::i2j(($lA2->get(((int)(N::imul($lI9, 2)))))))))));
		$lA2->set(((int)((((int)(N::imul($lI9, 2))) + 1))), (N::i2s($lI10)));
		label_21:
		$lI8 = ((int)(($lI8 + -1)));
		goto label_17;
		label_18:
		$lI10 = ((int)(($lI10 + -1)));
		goto label_14;
		label_15:
		return;
	}
	public static function gen_codes__SI_S_S_V(?JA_S $p0, int $p1, ?JA_S $p2, ?JA_S $p3) {
		$lI4 = 0;
		$lI5 = 0;
		$lI6 = 0;
		$lI7 = 0;
		$fA2 = null;
		$tA5 = null;
		$fA0 = null;
		$fI1 = 0;
		$fI2 = 0;
		$fI3 = 0;
		$tI0 = 0;
		$tI1 = 0;
		$tI4 = 0;
		$tI3 = 0;
		$lI4 = 0;
		$p3->set(0, 0);
		$lI5 = 1;
		label_1:
		if ((($lI5 > 15))) goto label_2;
		$fA0 = $p3;
		$fI1 = $lI5;
		$tI0 = ((int)((N::i2s(((int)(N::ishl(((int)(($lI4 + ($p2->get(((int)(($lI5 - 1)))))))), 1)))))));
		$fI2 = $tI0;
		$lI4 = $tI0;
		$fA0->set($fI1, (N::i2s($fI2)));
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		$lI6 = 0;
		label_4:
		if ((($lI6 > $p1))) goto label_5;
		$lI7 = ((int)(($p0->get(((int)((((int)(N::imul($lI6, 2))) + 1)))))));
		if ((($lI7 != 0))) goto label_7;
		goto label_8;
		label_7:
		$fA0 = $p0;
		$fI1 = ((int)(N::imul($lI6, 2)));
		$tI1 = $lI7;
		$fA2 = ($p3);
		$fI3 = $tI1;
		$tA5 = $fA2;
		$tI4 = $fI3;
		$tI3 = ((int)(($p3->get($tI1))));
		$fI2 = $tI3;
		($tA5)->set($tI4, (N::i2s(((int)(($tI3 + 1))))));
		$fA0->set($fI1, (N::i2s(com_jtransc_compression_jzlib_Tree::bi_reverse_II_I($fI2, $lI7))));
		label_8:
		$lI6 = ((int)(($lI6 + 1)));
		goto label_4;
		label_5:
		return;
	}
	public static function bi_reverse_II_I(int $p0, int $p1) {
		$lI0 = 0;
		$lI1 = 0;
		$lI2 = 0;
		$lI0 = $p0;
		$lI1 = $p1;
		$lI2 = 0;
		label_1:
		$lI2 = ((int)(($lI2 | ((int)(($lI0 & 1))))));
		$lI0 = ((int)(N::iushr($lI0, 1)));
		$lI2 = ((int)(N::ishl($lI2, 1)));
		$lI1 = ((int)(($lI1 + -1)));
		if ((($lI1 > 0))) goto label_1;
		return ((int)(N::iushr($lI2, 1)));
	}
	public static function d_code_I_I(int $p0) {
		$fI0 = 0;
		if ((($p0 >= 256))) goto label_1;
		$fI0 = ((int)((com_jtransc_compression_jzlib_Tree::$__dist_code->get($p0))));
		goto label_2;
		label_1:
		$fI0 = ((int)((com_jtransc_compression_jzlib_Tree::$__dist_code->get(((int)((256 + ((int)(N::iushr($p0, 7))))))))));
		label_2:
		return $fI0;
	}
	public function __construct($CLASS_ID = 896) {
		parent::__construct($CLASS_ID);
		$this->_dyn_tree = null;
		$this->_max_code = 0;
		$this->_stat_desc = null;
	}
	static public function SI() {
		com_jtransc_compression_jzlib_Tree::$_base_dist = null;
		com_jtransc_compression_jzlib_Tree::$_extra_lbits = null;
		com_jtransc_compression_jzlib_Tree::$__dist_code = null;
		com_jtransc_compression_jzlib_Tree::$_extra_dbits = null;
		com_jtransc_compression_jzlib_Tree::$__length_code = null;
		com_jtransc_compression_jzlib_Tree::$_base_length = null;
		com_jtransc_compression_jzlib_Tree::$_extra_blbits = null;
		com_jtransc_compression_jzlib_Tree::$_bl_order = null;
		com_jtransc_compression_jzlib_Tree::com_jtransc_compression_jzlib_Tree_clinit___V();
	}
}
class com_jtransc_compression_jzlib_Deflate extends java_lang_Object implements java_lang_Cloneable {

	public $_bl_desc = null;
	public $_l_desc = null;
	public $_dyn_ltree = null;
	public $_next_code = null;
	public $_depth = null;
	public $_strm = null;
	public $_dyn_dtree = null;
	public $_d_desc = null;
	public $_gheader = null;
	public $_wrap = 0;
	public $_bl_count = null;
	public $_heap = null;
	public $_bl_tree = null;
	public static $_z_errmsg = null;
	public static $_config_table = null;
	public $_prev = null;
	public $_d_buf = 0;
	public $_pending_buf = null;
	public $_head = null;
	public $_l_buf = null;
	public $_window = null;
	public $_pending = 0;
	public $_level = 0;
	public $_lookahead = 0;
	public $_last_flush = 0;
	public $_w_bits = 0;
	public $_hash_size = 0;
	public $_status = 0;
	public $_strstart = 0;
	public $_pending_out = 0;
	public $_max_lazy_match = 0;
	public $_w_size = 0;
	public $_match_start = 0;
	public $_strategy = 0;
	public $_hash_mask = 0;
	public $_match_available = 0;
	public $_ins_h = 0;
	public $_hash_shift = 0;
	public $_w_mask = 0;
	public $_prev_match = 0;
	public $_match_length = 0;
	public $_prev_length = 0;
	public $_block_start = 0;
	public $_static_len = 0;
	public $_data_type = 0;
	public $_opt_len = 0;
	public $_bi_valid = 0;
	public $_bi_buf = 0;
	public $_matches = 0;
	public $_last_lit = 0;
	public $_heap_len = 0;
	public $_heap_max = 0;
	public $_last_eob_len = 0;
	public $_max_chain_length = 0;
	public $_nice_match = 0;
	public $_good_match = 0;
	public $_window_size = 0;
	public $_lit_bufsize = 0;
	public $_pending_buf_size = 0;
	public $_hash_bits = 0;
	public $_method = 0;
	public function com_jtransc_compression_jzlib_Deflate_init__Lcom_jtransc_compression_jzlib_ZStream__V(?com_jtransc_compression_jzlib_ZStream $p0) {
		$fA1 = null;
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		($this)->java_lang_Object_init___V();
		$this->_wrap = 1;
		$fA0 = $this;
		$tA0 = ((new com_jtransc_compression_jzlib_Tree()));
		$fA1 = $tA0;
		($tA0)->com_jtransc_compression_jzlib_Tree_init___V();
		$fA0->_l_desc = ($fA1);
		$fA0 = $this;
		$tA1 = ((new com_jtransc_compression_jzlib_Tree()));
		$fA1 = $tA1;
		($tA1)->com_jtransc_compression_jzlib_Tree_init___V();
		$fA0->_d_desc = ($fA1);
		$fA0 = $this;
		$tA2 = ((new com_jtransc_compression_jzlib_Tree()));
		$fA1 = $tA2;
		($tA2)->com_jtransc_compression_jzlib_Tree_init___V();
		$fA0->_bl_desc = ($fA1);
		$this->_bl_count = new JA_S(16);
		$this->_next_code = new JA_S(16);
		$this->_heap = new JA_I(573);
		$this->_depth = new JA_B(573);
		$this->_gheader = null;
		$this->_strm = $p0;
		$this->_dyn_ltree = new JA_S(1146);
		$this->_dyn_dtree = new JA_S(122);
		$this->_bl_tree = new JA_S(78);
		return $this;
		return $this;
	}
	public static function com_jtransc_compression_jzlib_Deflate_clinit___V() {
		$tA1 = null;
		$tA3 = null;
		$tA5 = null;
		$tA7 = null;
		$tA9 = null;
		$tA10 = null;
		$fA0 = null;
		$fA2 = null;
		$tA0 = null;
		$tA2 = null;
		$tA4 = null;
		$tA6 = null;
		$tA8 = null;
		$fI1 = 0;
		com_jtransc_compression_jzlib_Deflate::$_config_table = new JA_L(10, "[Lcom.jtransc.compression.jzlib.Deflate\$Config;");
		$fA0 = (com_jtransc_compression_jzlib_Deflate::$_config_table);
		$fI1 = 0;
		$tA0 = ((new com_jtransc_compression_jzlib_Deflate_Config()));
		$fA2 = $tA0;
		($tA0)->com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(0, 0, 0, 0, 0);
		($fA0)->set($fI1, $fA2);
		$fA0 = (com_jtransc_compression_jzlib_Deflate::$_config_table);
		$fI1 = 1;
		$tA1 = ((new com_jtransc_compression_jzlib_Deflate_Config()));
		$fA2 = $tA1;
		($tA1)->com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(4, 4, 8, 4, 1);
		($fA0)->set($fI1, $fA2);
		$fA0 = (com_jtransc_compression_jzlib_Deflate::$_config_table);
		$fI1 = 2;
		$tA2 = ((new com_jtransc_compression_jzlib_Deflate_Config()));
		$fA2 = $tA2;
		($tA2)->com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(4, 5, 16, 8, 1);
		($fA0)->set($fI1, $fA2);
		$fA0 = (com_jtransc_compression_jzlib_Deflate::$_config_table);
		$fI1 = 3;
		$tA3 = ((new com_jtransc_compression_jzlib_Deflate_Config()));
		$fA2 = $tA3;
		($tA3)->com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(4, 6, 32, 32, 1);
		($fA0)->set($fI1, $fA2);
		$fA0 = (com_jtransc_compression_jzlib_Deflate::$_config_table);
		$fI1 = 4;
		$tA4 = ((new com_jtransc_compression_jzlib_Deflate_Config()));
		$fA2 = $tA4;
		($tA4)->com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(4, 4, 16, 16, 2);
		($fA0)->set($fI1, $fA2);
		$fA0 = (com_jtransc_compression_jzlib_Deflate::$_config_table);
		$fI1 = 5;
		$tA5 = ((new com_jtransc_compression_jzlib_Deflate_Config()));
		$fA2 = $tA5;
		($tA5)->com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(8, 16, 32, 32, 2);
		($fA0)->set($fI1, $fA2);
		$fA0 = (com_jtransc_compression_jzlib_Deflate::$_config_table);
		$fI1 = 6;
		$tA6 = ((new com_jtransc_compression_jzlib_Deflate_Config()));
		$fA2 = $tA6;
		($tA6)->com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(8, 16, 128, 128, 2);
		($fA0)->set($fI1, $fA2);
		$fA0 = (com_jtransc_compression_jzlib_Deflate::$_config_table);
		$fI1 = 7;
		$tA7 = ((new com_jtransc_compression_jzlib_Deflate_Config()));
		$fA2 = $tA7;
		($tA7)->com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(8, 32, 128, 256, 2);
		($fA0)->set($fI1, $fA2);
		$fA0 = (com_jtransc_compression_jzlib_Deflate::$_config_table);
		$fI1 = 8;
		$tA8 = ((new com_jtransc_compression_jzlib_Deflate_Config()));
		$fA2 = $tA8;
		($tA8)->com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(32, 128, 258, 1024, 2);
		($fA0)->set($fI1, $fA2);
		$fA0 = (com_jtransc_compression_jzlib_Deflate::$_config_table);
		$fI1 = 9;
		$tA9 = ((new com_jtransc_compression_jzlib_Deflate_Config()));
		$fA2 = $tA9;
		($tA9)->com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(32, 258, 258, 4096, 2);
		($fA0)->set($fI1, $fA2);
		$tA10 = (new JA_L(10, "[Ljava.lang.String;"));
		$fA0 = $tA10;
		($tA10)->set(0, (Bootstrap::$STRINGLIT_21));
		($fA0)->set(1, (Bootstrap::$STRINGLIT_22));
		($fA0)->set(2, (Bootstrap::$STRINGLIT_23));
		($fA0)->set(3, (Bootstrap::$STRINGLIT_24));
		($fA0)->set(4, (Bootstrap::$STRINGLIT_25));
		($fA0)->set(5, (Bootstrap::$STRINGLIT_26));
		($fA0)->set(6, (Bootstrap::$STRINGLIT_27));
		($fA0)->set(7, (Bootstrap::$STRINGLIT_28));
		($fA0)->set(8, (Bootstrap::$STRINGLIT_29));
		($fA0)->set(9, (Bootstrap::$STRINGLIT_23));
		com_jtransc_compression_jzlib_Deflate::$_z_errmsg = ($fA0);
		return;
	}
	public function clone__Ljava_lang_Object_() {
		$lA1 = null;
		$lA1 = (N::checkcast(parent::clone__Ljava_lang_Object_(), "com_jtransc_compression_jzlib_Deflate"));
		($lA1)->_pending_buf = $this->dup__B__B(($lA1)->_pending_buf);
		($lA1)->_l_buf = $this->dup__B__B(($lA1)->_l_buf);
		($lA1)->_window = $this->dup__B__B(($lA1)->_window);
		($lA1)->_prev = $this->dup__S__S(($lA1)->_prev);
		($lA1)->_head = $this->dup__S__S(($lA1)->_head);
		($lA1)->_dyn_ltree = $this->dup__S__S(($lA1)->_dyn_ltree);
		($lA1)->_dyn_dtree = $this->dup__S__S(($lA1)->_dyn_dtree);
		($lA1)->_bl_tree = $this->dup__S__S(($lA1)->_bl_tree);
		($lA1)->_bl_count = $this->dup__S__S(($lA1)->_bl_count);
		($lA1)->_next_code = $this->dup__S__S(($lA1)->_next_code);
		($lA1)->_heap = $this->dup__I__I(($lA1)->_heap);
		($lA1)->_depth = $this->dup__B__B(($lA1)->_depth);
		($lA1)->_l_desc->_dyn_tree = ($lA1)->_dyn_ltree;
		($lA1)->_d_desc->_dyn_tree = ($lA1)->_dyn_dtree;
		($lA1)->_bl_desc->_dyn_tree = ($lA1)->_bl_tree;
		if (((($lA1)->_gheader == null))) goto label_1;
		($lA1)->_gheader = N::checkcast(($lA1)->_gheader->clone__Ljava_lang_Object_(), "com_jtransc_compression_jzlib_GZIPHeader");
		label_1:
		return $lA1;
	}
	public function dup__S__S(?JA_S $p0) {
		$lA2 = null;
		$lA2 = (new JA_S((($p0))->length));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($p0), 0, $lA2, 0, ($lA2)->length);
		return ($lA2);
	}
	public function dup__I__I(?JA_I $p0) {
		$lA2 = null;
		$lA2 = (new JA_I((($p0))->length));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($p0), 0, $lA2, 0, ($lA2)->length);
		return ($lA2);
	}
	public function dup__B__B(?JA_B $p0) {
		$lA2 = null;
		$lA2 = (new JA_B((($p0))->length));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($p0), 0, $lA2, 0, ($lA2)->length);
		return ($lA2);
	}
	public function deflate_I_I(int $p0) {
		$fI0 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		if ((($p0 > 4))) goto label_1;
		if ((($p0 >= 0))) goto label_2;
		label_1:
		return -2;
		label_2:
		if ((($this->_strm->_next_out == null))) goto label_3;
		if ((($this->_strm->_next_in != null))) goto label_4;
		if ((($this->_strm->_avail_in != 0))) goto label_3;
		label_4:
		if ((($this->_status != 666))) goto label_5;
		if ((($p0 == 4))) goto label_5;
		label_3:
		$this->_strm->_msg = (((com_jtransc_compression_jzlib_Deflate::$_z_errmsg)->get(4)));
		return -2;
		label_5:
		if ((($this->_strm->_avail_out != 0))) goto label_6;
		$this->_strm->_msg = (((com_jtransc_compression_jzlib_Deflate::$_z_errmsg)->get(7)));
		return -5;
		label_6:
		$lI2 = $this->_last_flush;
		$this->_last_flush = $p0;
		if ((($this->_status != 42))) goto label_8;
		if ((($this->_wrap != 2))) goto label_10;
		$this->getGZIPHeader__Lcom_jtransc_compression_jzlib_GZIPHeader_()->put_Lcom_jtransc_compression_jzlib_Deflate__V($this);
		$this->_status = 113;
		$this->_strm->_adler->reset__V();
		goto label_8;
		label_10:
		$lI3 = ((int)(N::ishl(((int)((8 + ((int)(N::ishl(((int)(($this->_w_bits - 8))), 4)))))), 8)));
		$lI4 = ((int)(N::ishr(((int)((((int)(($this->_level - 1))) & 255))), 1)));
		if ((($lI4 <= 3))) goto label_47;
		$lI4 = 3;
		label_47:
		$lI3 = ((int)(($lI3 | ((int)(N::ishl($lI4, 6))))));
		if ((($this->_strstart == 0))) goto label_48;
		$lI3 = ((int)(($lI3 | 32)));
		label_48:
		$lI3 = ((int)(($lI3 + ((int)((31 - ((int)(N::irem($lI3, 31)))))))));
		$this->_status = 113;
		$this->putShortMSB_I_V($lI3);
		if ((($this->_strstart == 0))) goto label_49;
		$lI5 = $this->_strm->_adler->getValue__I();
		$this->putShortMSB_I_V(((int)(N::iushr($lI5, 16))));
		$this->putShortMSB_I_V(((int)(($lI5 & 65535))));
		label_49:
		$this->_strm->_adler->reset__V();
		label_8:
		if ((($this->_pending == 0))) goto label_12;
		$this->_strm->flush_pending__V();
		if ((($this->_strm->_avail_out != 0))) goto label_14;
		$this->_last_flush = -1;
		return 0;
		label_12:
		if ((($this->_strm->_avail_in != 0))) goto label_14;
		if ((($p0 > $lI2))) goto label_14;
		if ((($p0 == 4))) goto label_14;
		$this->_strm->_msg = (((com_jtransc_compression_jzlib_Deflate::$_z_errmsg)->get(7)));
		return -5;
		label_14:
		if ((($this->_status != 666))) goto label_16;
		if ((($this->_strm->_avail_in == 0))) goto label_16;
		$this->_strm->_msg = (((com_jtransc_compression_jzlib_Deflate::$_z_errmsg)->get(7)));
		return -5;
		label_16:
		if ((($this->_strm->_avail_in != 0))) goto label_18;
		if ((($this->_lookahead != 0))) goto label_18;
		if ((($p0 == 0))) goto label_19;
		if ((($this->_status == 666))) goto label_19;
		label_18:
		$lI3 = -1;
		switch ((((com_jtransc_compression_jzlib_Deflate::$_config_table)->get($this->_level)))->_func) {
			case 0: goto label_21;
			case 1: goto label_22;
			case 2: goto label_23;
			default: goto label_20;
		}
		label_21:
		$lI3 = $this->deflate_stored_I_I($p0);
		goto label_20;
		label_22:
		$lI3 = $this->deflate_fast_I_I($p0);
		goto label_20;
		label_23:
		$lI3 = $this->deflate_slow_I_I($p0);
		goto label_20;
		label_20:
		if ((($lI3 == 2))) goto label_24;
		if ((($lI3 != 3))) goto label_25;
		label_24:
		$this->_status = 666;
		label_25:
		if ((($lI3 == 0))) goto label_26;
		if ((($lI3 != 2))) goto label_27;
		label_26:
		if ((($this->_strm->_avail_out != 0))) goto label_28;
		$this->_last_flush = -1;
		label_28:
		return 0;
		label_27:
		if ((($lI3 != 1))) goto label_19;
		if ((($p0 != 1))) goto label_31;
		$this->_tr_align__V();
		goto label_33;
		label_31:
		$this->_tr_stored_block_IIZ_V(0, 0, false);
		if ((($p0 != 3))) goto label_33;
		$lI4 = 0;
		label_44:
		if ((($lI4 >= $this->_hash_size))) goto label_33;
		$this->_head->set($lI4, 0);
		$lI4 = ((int)(($lI4 + 1)));
		goto label_44;
		label_33:
		$this->_strm->flush_pending__V();
		if ((($this->_strm->_avail_out != 0))) goto label_19;
		$this->_last_flush = -1;
		return 0;
		label_19:
		if ((($p0 == 4))) goto label_35;
		return 0;
		label_35:
		if ((($this->_wrap > 0))) goto label_36;
		return 1;
		label_36:
		if ((($this->_wrap != 2))) goto label_37;
		$lI3 = $this->_strm->_adler->getValue__I();
		$this->put_byte_B_V((N::i2b(((int)(($lI3 & 255))))));
		$this->put_byte_B_V((N::i2b(((int)((((int)(N::ishr($lI3, 8))) & 255))))));
		$this->put_byte_B_V((N::i2b(((int)((((int)(N::ishr($lI3, 16))) & 255))))));
		$this->put_byte_B_V((N::i2b(((int)((((int)(N::ishr($lI3, 24))) & 255))))));
		$this->put_byte_B_V((N::i2b(N::j2i((N::land($this->_strm->_total_in, Int64::make(0, 255)))))));
		$this->put_byte_B_V((N::i2b(N::j2i((N::land((N::lshr($this->_strm->_total_in, 8)), Int64::make(0, 255)))))));
		$this->put_byte_B_V((N::i2b(N::j2i((N::land((N::lshr($this->_strm->_total_in, 16)), Int64::make(0, 255)))))));
		$this->put_byte_B_V((N::i2b(N::j2i((N::land((N::lshr($this->_strm->_total_in, 24)), Int64::make(0, 255)))))));
		$this->getGZIPHeader__Lcom_jtransc_compression_jzlib_GZIPHeader_()->setCRC_J_V(N::i2j($lI3));
		goto label_39;
		label_37:
		$lI3 = $this->_strm->_adler->getValue__I();
		$this->putShortMSB_I_V(((int)(N::iushr($lI3, 16))));
		$this->putShortMSB_I_V(((int)(($lI3 & 65535))));
		label_39:
		$this->_strm->flush_pending__V();
		if ((($this->_wrap <= 0))) goto label_40;
		$this->_wrap = ((int)(-($this->_wrap)));
		label_40:
		if ((($this->_pending == 0))) goto label_41;
		$fI0 = 0;
		goto label_42;
		label_41:
		$fI0 = 1;
		label_42:
		return $fI0;
	}
	public function put_byte_B_V(int $p0) {
		$fA0 = null;
		$fI1 = 0;
		$fA1 = null;
		$tI1 = 0;
		$tA2 = null;
		$fA0 = $this->_pending_buf;
		$fA1 = ($this);
		$tA2 = $fA1;
		$tI1 = $this->_pending;
		$fI1 = $tI1;
		($tA2)->_pending = ((int)(($tI1 + 1)));
		$fA0->set($fI1, $p0);
		return;
	}
	public function put_short_I_V(int $p0) {
		$this->put_byte_B_V((N::i2b($p0)));
		$this->put_byte_B_V((N::i2b(((int)(N::iushr($p0, 8))))));
		return;
	}
	public function put_byte__BII_V(?JA_B $p0, int $p1, int $p2) {
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($p0), $p1, ($this->_pending_buf), $this->_pending, $p2);
		$this->_pending = ((int)(($this->_pending + $p2)));
		return;
	}
	public function deflate_slow_I_I(int $p0) {
		$fI0 = 0;
		$fI1 = 0;
		$lI2 = 0;
		$lI4 = 0;
		$lI3 = 0;
		$fA0 = null;
		$tI3 = 0;
		$tI6 = 0;
		$tA4 = null;
		$tA7 = null;
		$lI2 = 0;
		label_1:
		if ((($this->_lookahead >= 262))) goto label_2;
		$this->fill_window__V();
		if ((($this->_lookahead >= 262))) goto label_4;
		if ((($p0 != 0))) goto label_4;
		return 0;
		label_4:
		if ((($this->_lookahead != 0))) goto label_2;
		goto label_6;
		label_2:
		if ((($this->_lookahead < 3))) goto label_16;
		$this->_ins_h = ((int)((((int)((((int)(N::ishl($this->_ins_h, $this->_hash_shift))) ^ ((int)((($this->_window->get(((int)(($this->_strstart + 2))))) & 255)))))) & $this->_hash_mask)));
		$lI2 = ((int)((($this->_head->get($this->_ins_h)) & 65535)));
		$this->_prev->set(((int)(($this->_strstart & $this->_w_mask))), ($this->_head->get($this->_ins_h)));
		$this->_head->set($this->_ins_h, (N::i2s($this->_strstart)));
		label_16:
		$this->_prev_length = $this->_match_length;
		$this->_prev_match = $this->_match_start;
		$this->_match_length = 2;
		if ((($lI2 == 0))) goto label_18;
		if ((($this->_prev_length >= $this->_max_lazy_match))) goto label_18;
		if (((((int)((((int)(($this->_strstart - $lI2))) & 65535))) > ((int)(($this->_w_size - 262)))))) goto label_18;
		if ((($this->_strategy == 2))) goto label_20;
		$this->_match_length = $this->longest_match_I_I($lI2);
		label_20:
		if ((($this->_match_length > 5))) goto label_18;
		if ((($this->_strategy == 1))) goto label_22;
		if ((($this->_match_length != 3))) goto label_18;
		if (((((int)(($this->_strstart - $this->_match_start))) <= 4096))) goto label_18;
		label_22:
		$this->_match_length = 2;
		label_18:
		if ((($this->_prev_length < 3))) goto label_23;
		if ((($this->_match_length > $this->_prev_length))) goto label_23;
		$lI4 = ((int)((((int)(($this->_strstart + $this->_lookahead))) - 3)));
		$lI3 = ((int)(N::z2i($this->_tr_tally_II_Z(((int)((((int)(($this->_strstart - 1))) - $this->_prev_match))), ((int)(($this->_prev_length - 3)))))));
		$this->_lookahead = ((int)(($this->_lookahead - ((int)(($this->_prev_length - 1))))));
		$this->_prev_length = ((int)(($this->_prev_length - 2)));
		label_25:
		$fA0 = ($this);
		$tA4 = $fA0;
		$tI3 = ((int)(($this->_strstart + 1)));
		$fI0 = $tI3;
		($tA4)->_strstart = $tI3;
		if ((($fI0 > $lI4))) goto label_26;
		$this->_ins_h = ((int)((((int)((((int)(N::ishl($this->_ins_h, $this->_hash_shift))) ^ ((int)((($this->_window->get(((int)(($this->_strstart + 2))))) & 255)))))) & $this->_hash_mask)));
		$lI2 = ((int)((($this->_head->get($this->_ins_h)) & 65535)));
		$this->_prev->set(((int)(($this->_strstart & $this->_w_mask))), ($this->_head->get($this->_ins_h)));
		$this->_head->set($this->_ins_h, (N::i2s($this->_strstart)));
		label_26:
		$fA0 = ($this);
		$tA7 = $fA0;
		$tI6 = ((int)(($this->_prev_length - 1)));
		$fI0 = $tI6;
		($tA7)->_prev_length = $tI6;
		if ((($fI0 != 0))) goto label_25;
		$this->_match_available = 0;
		$this->_match_length = 2;
		$this->_strstart = ((int)(($this->_strstart + 1)));
		if ((($lI3 == 0))) goto label_29;
		$this->flush_block_only_Z_V(false);
		if ((($this->_strm->_avail_out != 0))) goto label_29;
		return 0;
		label_29:
		goto label_1;
		label_23:
		if ((($this->_match_available == 0))) goto label_31;
		$lI3 = ((int)(N::z2i($this->_tr_tally_II_Z(0, ((int)((($this->_window->get(((int)(($this->_strstart - 1))))) & 255)))))));
		if ((($lI3 == 0))) goto label_33;
		$this->flush_block_only_Z_V(false);
		label_33:
		$this->_strstart = ((int)(($this->_strstart + 1)));
		$this->_lookahead = ((int)(($this->_lookahead - 1)));
		if ((($this->_strm->_avail_out != 0))) goto label_1;
		return 0;
		label_31:
		$this->_match_available = 1;
		$this->_strstart = ((int)(($this->_strstart + 1)));
		$this->_lookahead = ((int)(($this->_lookahead - 1)));
		goto label_1;
		label_6:
		if ((($this->_match_available == 0))) goto label_7;
		$lI3 = ((int)(N::z2i($this->_tr_tally_II_Z(0, ((int)((($this->_window->get(((int)(($this->_strstart - 1))))) & 255)))))));
		$this->_match_available = 0;
		label_7:
		$fA0 = ($this);
		if ((($p0 != 4))) goto label_9;
		$fI1 = 1;
		goto label_10;
		label_9:
		$fI1 = 0;
		label_10:
		($fA0)->flush_block_only_Z_V((($fI1)!=0));
		if ((($this->_strm->_avail_out != 0))) goto label_11;
		if ((($p0 != 4))) goto label_13;
		return 2;
		label_13:
		return 0;
		label_11:
		if ((($p0 != 4))) goto label_14;
		$fI0 = 3;
		goto label_15;
		label_14:
		$fI0 = 1;
		label_15:
		return $fI0;
	}
	public function flush_block_only_Z_V(bool $p0) {
		$fI1 = 0;
		if ((($this->_block_start < 0))) goto label_1;
		$fI1 = $this->_block_start;
		goto label_2;
		label_1:
		$fI1 = -1;
		label_2:
		$this->_tr_flush_block_IIZ_V($fI1, ((int)(($this->_strstart - $this->_block_start))), $p0);
		$this->_block_start = $this->_strstart;
		$this->_strm->flush_pending__V();
		return;
	}
	public function _tr_flush_block_IIZ_V(int $p0, int $p1, bool $p2) {
		$fI0 = 0;
		$fI1 = 0;
		$fI2 = 0;
		$lI6 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$fA0 = null;
		$tI0 = 0;
		$lI6 = 0;
		if ((($this->_level <= 0))) goto label_1;
		if ((($this->_data_type != 2))) goto label_3;
		$this->set_data_type__V();
		label_3:
		$this->_l_desc->build_tree_Lcom_jtransc_compression_jzlib_Deflate__V($this);
		$this->_d_desc->build_tree_Lcom_jtransc_compression_jzlib_Deflate__V($this);
		$lI6 = $this->build_bl_tree__I();
		$lI4 = ((int)(N::iushr(((int)((((int)(($this->_opt_len + 3))) + 7))), 3)));
		$lI5 = ((int)(N::iushr(((int)((((int)(($this->_static_len + 3))) + 7))), 3)));
		if ((($lI5 > $lI4))) goto label_4;
		$lI4 = $lI5;
		goto label_4;
		label_1:
		$tI0 = ((int)(($p1 + 5)));
		$fI0 = $tI0;
		$lI5 = $tI0;
		$lI4 = $fI0;
		label_4:
		if (((((int)(($p1 + 4))) > $lI4))) goto label_5;
		if ((($p0 == -1))) goto label_5;
		$this->_tr_stored_block_IIZ_V($p0, $p1, $p2);
		goto label_7;
		label_5:
		if ((($lI5 != $lI4))) goto label_10;
		$fA0 = $this;
		$fI1 = 2;
		if (!($p2)) goto label_12;
		$fI2 = 1;
		goto label_13;
		label_12:
		$fI2 = 0;
		label_13:
		$fA0->send_bits_II_V(((int)(($fI1 + $fI2))), 3);
		$this->compress_block__S_S_V(com_jtransc_compression_jzlib_StaticTree::$_static_ltree, com_jtransc_compression_jzlib_StaticTree::$_static_dtree);
		goto label_7;
		label_10:
		$fA0 = $this;
		$fI1 = 4;
		if (!($p2)) goto label_14;
		$fI2 = 1;
		goto label_15;
		label_14:
		$fI2 = 0;
		label_15:
		$fA0->send_bits_II_V(((int)(($fI1 + $fI2))), 3);
		$this->send_all_trees_III_V(((int)(($this->_l_desc->_max_code + 1))), ((int)(($this->_d_desc->_max_code + 1))), ((int)(($lI6 + 1))));
		$this->compress_block__S_S_V($this->_dyn_ltree, $this->_dyn_dtree);
		label_7:
		$this->init_block__V();
		if (!($p2)) goto label_8;
		$this->bi_windup__V();
		label_8:
		return;
	}
	public function send_all_trees_III_V(int $p0, int $p1, int $p2) {
		$lI4 = 0;
		$this->send_bits_II_V(((int)(($p0 - 257))), 5);
		$this->send_bits_II_V(((int)(($p1 - 1))), 5);
		$this->send_bits_II_V(((int)(($p2 - 4))), 4);
		$lI4 = 0;
		label_1:
		if ((($lI4 >= $p2))) goto label_2;
		$this->send_bits_II_V(((int)(($this->_bl_tree->get(((int)((((int)(N::imul((com_jtransc_compression_jzlib_Tree::$_bl_order->get($lI4)), 2))) + 1))))))), 3);
		$lI4 = ((int)(($lI4 + 1)));
		goto label_1;
		label_2:
		$this->send_tree__SI_V($this->_dyn_ltree, ((int)(($p0 - 1))));
		$this->send_tree__SI_V($this->_dyn_dtree, ((int)(($p1 - 1))));
		return;
	}
	public function send_tree__SI_V(?JA_S $p0, int $p1) {
		$lI4 = 0;
		$lI6 = 0;
		$lI7 = 0;
		$lI8 = 0;
		$lI9 = 0;
		$lI3 = 0;
		$lI5 = 0;
		$lI4 = -1;
		$lI6 = ((int)(($p0->get(1))));
		$lI7 = 0;
		$lI8 = 7;
		$lI9 = 4;
		if ((($lI6 != 0))) goto label_1;
		$lI8 = 138;
		$lI9 = 3;
		label_1:
		$lI3 = 0;
		label_2:
		if ((($lI3 > $p1))) goto label_3;
		$lI5 = $lI6;
		$lI6 = ((int)(($p0->get(((int)((((int)(N::imul(((int)(($lI3 + 1))), 2))) + 1)))))));
		$lI7 = ((int)(($lI7 + 1)));
		if ((($lI7 >= $lI8))) goto label_5;
		if ((($lI5 != $lI6))) goto label_5;
		goto label_7;
		label_5:
		if ((($lI7 >= $lI9))) goto label_8;
		label_9:
		$this->send_code_I_S_V($lI5, $this->_bl_tree);
		$lI7 = ((int)(($lI7 + -1)));
		if ((($lI7 != 0))) goto label_9;
		goto label_10;
		label_8:
		if ((($lI5 == 0))) goto label_15;
		if ((($lI5 == $lI4))) goto label_17;
		$this->send_code_I_S_V($lI5, $this->_bl_tree);
		$lI7 = ((int)(($lI7 + -1)));
		label_17:
		$this->send_code_I_S_V(16, $this->_bl_tree);
		$this->send_bits_II_V(((int)(($lI7 - 3))), 2);
		goto label_10;
		label_15:
		if ((($lI7 > 10))) goto label_19;
		$this->send_code_I_S_V(17, $this->_bl_tree);
		$this->send_bits_II_V(((int)(($lI7 - 3))), 3);
		goto label_10;
		label_19:
		$this->send_code_I_S_V(18, $this->_bl_tree);
		$this->send_bits_II_V(((int)(($lI7 - 11))), 7);
		label_10:
		$lI7 = 0;
		$lI4 = $lI5;
		if ((($lI6 != 0))) goto label_11;
		$lI8 = 138;
		$lI9 = 3;
		goto label_7;
		label_11:
		if ((($lI5 != $lI6))) goto label_13;
		$lI8 = 6;
		$lI9 = 3;
		goto label_7;
		label_13:
		$lI8 = 7;
		$lI9 = 4;
		label_7:
		$lI3 = ((int)(($lI3 + 1)));
		goto label_2;
		label_3:
		return;
	}
	public function send_code_I_S_V(int $p0, ?JA_S $p1) {
		$lI3 = 0;
		$lI3 = ((int)(N::imul($p0, 2)));
		$this->send_bits_II_V(((int)((($p1->get($lI3)) & 65535))), ((int)((($p1->get(((int)(($lI3 + 1))))) & 65535))));
		return;
	}
	public function send_bits_II_V(int $p0, int $p1) {
		if ((($this->_bi_valid <= ((int)((16 - $p1)))))) goto label_1;
		$this->_bi_buf = (N::i2s(((int)(($this->_bi_buf | ((int)((((int)(N::ishl($p0, $this->_bi_valid))) & 65535))))))));
		$this->put_short_I_V(((int)($this->_bi_buf)));
		$this->_bi_buf = (N::i2s(((int)(N::iushr($p0, ((int)((16 - $this->_bi_valid))))))));
		$this->_bi_valid = ((int)(($this->_bi_valid + ((int)(($p1 - 16))))));
		goto label_3;
		label_1:
		$this->_bi_buf = (N::i2s(((int)(($this->_bi_buf | ((int)((((int)(N::ishl($p0, $this->_bi_valid))) & 65535))))))));
		$this->_bi_valid = ((int)(($this->_bi_valid + $p1)));
		label_3:
		return;
	}
	public function bi_windup__V() {
		if ((($this->_bi_valid <= 8))) goto label_1;
		$this->put_short_I_V(((int)($this->_bi_buf)));
		goto label_3;
		label_1:
		if ((($this->_bi_valid <= 0))) goto label_3;
		$this->put_byte_B_V((N::i2b($this->_bi_buf)));
		label_3:
		$this->_bi_buf = 0;
		$this->_bi_valid = 0;
		return;
	}
	public function init_block__V() {
		$fI1 = 0;
		$lI1 = 0;
		$fA1 = null;
		$fA0 = null;
		$tI0 = 0;
		$tI2 = 0;
		$tA1 = null;
		$tA3 = null;
		$lI1 = 0;
		label_1:
		if ((($lI1 >= 286))) goto label_2;
		$this->_dyn_ltree->set(((int)(N::imul($lI1, 2))), 0);
		$lI1 = ((int)(($lI1 + 1)));
		goto label_1;
		label_2:
		$lI1 = 0;
		label_3:
		if ((($lI1 >= 30))) goto label_4;
		$this->_dyn_dtree->set(((int)(N::imul($lI1, 2))), 0);
		$lI1 = ((int)(($lI1 + 1)));
		goto label_3;
		label_4:
		$lI1 = 0;
		label_5:
		if ((($lI1 >= 19))) goto label_6;
		$this->_bl_tree->set(((int)(N::imul($lI1, 2))), 0);
		$lI1 = ((int)(($lI1 + 1)));
		goto label_5;
		label_6:
		$this->_dyn_ltree->set(512, 1);
		$fA0 = $this;
		$fA1 = ($this);
		$tA1 = $fA1;
		$tI0 = 0;
		$fI1 = $tI0;
		($tA1)->_static_len = $tI0;
		$fA0->_opt_len = $fI1;
		$fA0 = $this;
		$fA1 = ($this);
		$tA3 = $fA1;
		$tI2 = 0;
		$fI1 = $tI2;
		($tA3)->_matches = $tI2;
		$fA0->_last_lit = $fI1;
		return;
	}
	public function set_data_type__V() {
		$fI1 = 0;
		$lI1 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$lI1 = 0;
		$lI2 = 0;
		$lI3 = 0;
		label_1:
		if ((($lI1 >= 7))) goto label_2;
		$lI3 = ((int)(($lI3 + ($this->_dyn_ltree->get(((int)(N::imul($lI1, 2))))))));
		$lI1 = ((int)(($lI1 + 1)));
		goto label_1;
		label_2:
		if ((($lI1 >= 128))) goto label_3;
		$lI2 = ((int)(($lI2 + ($this->_dyn_ltree->get(((int)(N::imul($lI1, 2))))))));
		$lI1 = ((int)(($lI1 + 1)));
		goto label_2;
		label_3:
		if ((($lI1 >= 256))) goto label_4;
		$lI3 = ((int)(($lI3 + ($this->_dyn_ltree->get(((int)(N::imul($lI1, 2))))))));
		$lI1 = ((int)(($lI1 + 1)));
		goto label_3;
		label_4:
		if ((($lI3 <= ((int)(N::iushr($lI2, 2)))))) goto label_5;
		$fI1 = 0;
		goto label_6;
		label_5:
		$fI1 = 1;
		label_6:
		$this->_data_type = (N::i2b($fI1));
		return;
	}
	public function pqdownheap__SI_V(?JA_S $p0, int $p1) {
		$fA3 = null;
		$fI1 = 0;
		$fI2 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI2 = $p1;
		$lI3 = ($this->_heap->get($lI2));
		$lI4 = ((int)(N::ishl($lI2, 1)));
		label_1:
		if ((($lI4 > $this->_heap_len))) goto label_2;
		if ((($lI4 >= $this->_heap_len))) goto label_4;
		$fI1 = ($this->_heap->get(((int)(($lI4 + 1)))));
		$fI2 = ($this->_heap->get($lI4));
		$fA3 = $this->_depth;
		if (!(com_jtransc_compression_jzlib_Deflate::smaller__SII_B_Z($p0, $fI1, $fI2, $fA3))) goto label_4;
		$lI4 = ((int)(($lI4 + 1)));
		label_4:
		if (!(com_jtransc_compression_jzlib_Deflate::smaller__SII_B_Z($p0, $lI3, ($this->_heap->get($lI4)), $this->_depth))) goto label_6;
		goto label_2;
		label_6:
		$this->_heap->set($lI2, ($this->_heap->get($lI4)));
		$lI2 = $lI4;
		$lI4 = ((int)(N::ishl($lI4, 1)));
		goto label_1;
		label_2:
		$this->_heap->set($lI2, $lI3);
		return;
	}
	public static function smaller__SII_B_Z(?JA_S $p0, int $p1, int $p2, ?JA_B $p3) {
		$fI0 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$lI4 = ((int)(($p0->get(((int)(N::imul($p1, 2)))))));
		$lI5 = ((int)(($p0->get(((int)(N::imul($p2, 2)))))));
		if ((($lI4 < $lI5))) goto label_1;
		if ((($lI4 != $lI5))) goto label_2;
		if (((($p3->get($p1)) > ($p3->get($p2))))) goto label_2;
		label_1:
		$fI0 = 1;
		goto label_3;
		label_2:
		$fI0 = 0;
		label_3:
		return (($fI0)!=0);
	}
	public function build_bl_tree__I() {
		$lI1 = 0;
		$this->scan_tree__SI_V($this->_dyn_ltree, $this->_l_desc->_max_code);
		$this->scan_tree__SI_V($this->_dyn_dtree, $this->_d_desc->_max_code);
		$this->_bl_desc->build_tree_Lcom_jtransc_compression_jzlib_Deflate__V($this);
		$lI1 = 18;
		label_1:
		if ((($lI1 < 3))) goto label_2;
		if (((($this->_bl_tree->get(((int)((((int)(N::imul((com_jtransc_compression_jzlib_Tree::$_bl_order->get($lI1)), 2))) + 1))))) == 0))) goto label_4;
		goto label_2;
		label_4:
		$lI1 = ((int)(($lI1 + -1)));
		goto label_1;
		label_2:
		$this->_opt_len = ((int)(($this->_opt_len + ((int)((((int)((((int)((((int)(N::imul(3, ((int)(($lI1 + 1)))))) + 5))) + 5))) + 4))))));
		return $lI1;
	}
	public function scan_tree__SI_V(?JA_S $p0, int $p1) {
		$lI4 = 0;
		$lI6 = 0;
		$lI7 = 0;
		$lI8 = 0;
		$lI9 = 0;
		$lI3 = 0;
		$lI5 = 0;
		$tA1 = null;
		$tA3 = null;
		$tA5 = null;
		$tA7 = null;
		$tA9 = null;
		$tI0 = 0;
		$tI2 = 0;
		$tI4 = 0;
		$tI6 = 0;
		$tI8 = 0;
		$lI4 = -1;
		$lI6 = ((int)(($p0->get(1))));
		$lI7 = 0;
		$lI8 = 7;
		$lI9 = 4;
		if ((($lI6 != 0))) goto label_1;
		$lI8 = 138;
		$lI9 = 3;
		label_1:
		$p0->set(((int)((((int)(N::imul(((int)(($p1 + 1))), 2))) + 1))), -1);
		$lI3 = 0;
		label_2:
		if ((($lI3 > $p1))) goto label_3;
		$lI5 = $lI6;
		$lI6 = ((int)(($p0->get(((int)((((int)(N::imul(((int)(($lI3 + 1))), 2))) + 1)))))));
		$lI7 = ((int)(($lI7 + 1)));
		if ((($lI7 >= $lI8))) goto label_5;
		if ((($lI5 != $lI6))) goto label_5;
		goto label_7;
		label_5:
		if ((($lI7 >= $lI9))) goto label_8;
		$tA1 = $this->_bl_tree;
		$tI0 = ((int)(N::imul($lI5, 2)));
		$tA1->set($tI0, (N::i2s(((int)((($tA1->get($tI0)) + $lI7))))));
		goto label_10;
		label_8:
		if ((($lI5 == 0))) goto label_15;
		if ((($lI5 == $lI4))) goto label_17;
		$tA3 = $this->_bl_tree;
		$tI2 = ((int)(N::imul($lI5, 2)));
		$tA3->set($tI2, (N::i2s(((int)((($tA3->get($tI2)) + 1))))));
		label_17:
		$tA5 = $this->_bl_tree;
		$tI4 = 32;
		$tA5->set($tI4, (N::i2s(((int)((($tA5->get($tI4)) + 1))))));
		goto label_10;
		label_15:
		if ((($lI7 > 10))) goto label_18;
		$tA7 = $this->_bl_tree;
		$tI6 = 34;
		$tA7->set($tI6, (N::i2s(((int)((($tA7->get($tI6)) + 1))))));
		goto label_10;
		label_18:
		$tA9 = $this->_bl_tree;
		$tI8 = 36;
		$tA9->set($tI8, (N::i2s(((int)((($tA9->get($tI8)) + 1))))));
		label_10:
		$lI7 = 0;
		$lI4 = $lI5;
		if ((($lI6 != 0))) goto label_11;
		$lI8 = 138;
		$lI9 = 3;
		goto label_7;
		label_11:
		if ((($lI5 != $lI6))) goto label_13;
		$lI8 = 6;
		$lI9 = 3;
		goto label_7;
		label_13:
		$lI8 = 7;
		$lI9 = 4;
		label_7:
		$lI3 = ((int)(($lI3 + 1)));
		goto label_2;
		label_3:
		return;
	}
	public function compress_block__S_S_V(?JA_S $p0, ?JA_S $p1) {
		$lI5 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI6 = 0;
		$lI7 = 0;
		$lI5 = 0;
		if ((($this->_last_lit == 0))) goto label_1;
		label_2:
		$lI3 = ((int)((((int)((((int)(N::ishl(($this->_pending_buf->get(((int)(($this->_d_buf + ((int)(N::imul($lI5, 2)))))))), 8))) & 65280))) | ((int)((($this->_pending_buf->get(((int)((((int)(($this->_d_buf + ((int)(N::imul($lI5, 2)))))) + 1))))) & 255))))));
		$lI4 = ((int)((($this->_l_buf->get($lI5)) & 255)));
		$lI5 = ((int)(($lI5 + 1)));
		if ((($lI3 != 0))) goto label_3;
		$this->send_code_I_S_V($lI4, $p0);
		goto label_5;
		label_3:
		$lI6 = ((int)((com_jtransc_compression_jzlib_Tree::$__length_code->get($lI4))));
		$this->send_code_I_S_V(((int)((((int)(($lI6 + 256))) + 1))), $p0);
		$lI7 = (com_jtransc_compression_jzlib_Tree::$_extra_lbits->get($lI6));
		if ((($lI7 == 0))) goto label_6;
		$lI4 = ((int)(($lI4 - (com_jtransc_compression_jzlib_Tree::$_base_length->get($lI6)))));
		$this->send_bits_II_V($lI4, $lI7);
		label_6:
		$lI3 = ((int)(($lI3 + -1)));
		$lI6 = com_jtransc_compression_jzlib_Tree::d_code_I_I($lI3);
		$this->send_code_I_S_V($lI6, $p1);
		$lI7 = (com_jtransc_compression_jzlib_Tree::$_extra_dbits->get($lI6));
		if ((($lI7 == 0))) goto label_5;
		$lI3 = ((int)(($lI3 - (com_jtransc_compression_jzlib_Tree::$_base_dist->get($lI6)))));
		$this->send_bits_II_V($lI3, $lI7);
		label_5:
		if ((($lI5 < $this->_last_lit))) goto label_2;
		label_1:
		$this->send_code_I_S_V(256, $p0);
		$this->_last_eob_len = ((int)(($p0->get(513))));
		return;
	}
	public function _tr_stored_block_IIZ_V(int $p0, int $p1, bool $p2) {
		$fI1 = 0;
		$fI2 = 0;
		$fI1 = 0;
		if (!($p2)) goto label_1;
		$fI2 = 1;
		goto label_2;
		label_1:
		$fI2 = 0;
		label_2:
		$this->send_bits_II_V(((int)(($fI1 + $fI2))), 3);
		$this->copy_block_IIZ_V($p0, $p1, true);
		return;
	}
	public function copy_block_IIZ_V(int $p0, int $p1, bool $p2) {
		$this->bi_windup__V();
		$this->_last_eob_len = 8;
		if (!($p2)) goto label_1;
		$this->put_short_I_V(((int)((N::i2s($p1)))));
		$this->put_short_I_V(((int)((N::i2s(((int)(($p1 ^ -1))))))));
		label_1:
		$this->put_byte__BII_V($this->_window, $p0, $p1);
		return;
	}
	public function longest_match_I_I(int $p0) {
		$fA0 = null;
		$lI12 = 0;
		$lI2 = 0;
		$lI6 = 0;
		$lI8 = 0;
		$lI10 = 0;
		$lI4 = 0;
		$fI0 = 0;
		$tI0 = 0;
		$fA1 = null;
		$lI11 = 0;
		$lI1 = 0;
		$lI3 = 0;
		$lI7 = 0;
		$lI9 = 0;
		$lI5 = 0;
		$lI1 = $p0;
		$lI2 = $this->_max_chain_length;
		$lI3 = $this->_strstart;
		$lI6 = $this->_prev_length;
		if ((($this->_strstart <= ((int)(($this->_w_size - 262)))))) goto label_1;
		$fI0 = ((int)(($this->_strstart - ((int)(($this->_w_size - 262))))));
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		$lI7 = $fI0;
		$lI8 = $this->_nice_match;
		$lI9 = $this->_w_mask;
		$lI10 = ((int)(($this->_strstart + 258)));
		$lI11 = ((int)(($this->_window->get(((int)((((int)(($lI3 + $lI6))) - 1)))))));
		$lI12 = ((int)(($this->_window->get(((int)(($lI3 + $lI6)))))));
		if ((($this->_prev_length < $this->_good_match))) goto label_3;
		$lI2 = ((int)(N::ishr($lI2, 2)));
		label_3:
		if ((($lI8 <= $this->_lookahead))) goto label_5;
		$lI8 = $this->_lookahead;
		label_5:
		$lI4 = $lI1;
		if (((($this->_window->get(((int)(($lI4 + $lI6))))) != $lI12))) goto label_6;
		if (((($this->_window->get(((int)((((int)(($lI4 + $lI6))) - 1))))) != $lI11))) goto label_6;
		if (((($this->_window->get($lI4)) != ($this->_window->get($lI3))))) goto label_6;
		$fA0 = $this->_window;
		$lI4 = ((int)(($lI4 + 1)));
		if (((($fA0->get($lI4)) == ($this->_window->get(((int)(($lI3 + 1)))))))) goto label_7;
		goto label_6;
		label_7:
		$lI3 = ((int)(($lI3 + 2)));
		$lI4 = ((int)(($lI4 + 1)));
		label_11:
		$fA0 = $this->_window;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = ((int)(($fA0->get($lI3))));
		$fA1 = $this->_window;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1->get($lI4))))) goto label_12;
		$fA0 = $this->_window;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = ((int)(($fA0->get($lI3))));
		$fA1 = $this->_window;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1->get($lI4))))) goto label_12;
		$fA0 = $this->_window;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = ((int)(($fA0->get($lI3))));
		$fA1 = $this->_window;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1->get($lI4))))) goto label_12;
		$fA0 = $this->_window;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = ((int)(($fA0->get($lI3))));
		$fA1 = $this->_window;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1->get($lI4))))) goto label_12;
		$fA0 = $this->_window;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = ((int)(($fA0->get($lI3))));
		$fA1 = $this->_window;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1->get($lI4))))) goto label_12;
		$fA0 = $this->_window;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = ((int)(($fA0->get($lI3))));
		$fA1 = $this->_window;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1->get($lI4))))) goto label_12;
		$fA0 = $this->_window;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = ((int)(($fA0->get($lI3))));
		$fA1 = $this->_window;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1->get($lI4))))) goto label_12;
		$fA0 = $this->_window;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = ((int)(($fA0->get($lI3))));
		$fA1 = $this->_window;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1->get($lI4))))) goto label_12;
		if ((($lI3 < $lI10))) goto label_11;
		label_12:
		$lI5 = ((int)((258 - ((int)(($lI10 - $lI3))))));
		$lI3 = ((int)(($lI10 - 258)));
		if ((($lI5 <= $lI6))) goto label_6;
		$this->_match_start = $lI1;
		$lI6 = $lI5;
		if ((($lI5 < $lI8))) goto label_14;
		goto label_9;
		label_14:
		$lI11 = ((int)(($this->_window->get(((int)((((int)(($lI3 + $lI6))) - 1)))))));
		$lI12 = ((int)(($this->_window->get(((int)(($lI3 + $lI6)))))));
		label_6:
		$tI0 = ((int)((($this->_prev->get(((int)(($lI1 & $lI9))))) & 65535)));
		$fI0 = $tI0;
		$lI1 = $tI0;
		if ((($fI0 <= $lI7))) goto label_9;
		$lI2 = ((int)(($lI2 + -1)));
		if ((($lI2 != 0))) goto label_5;
		label_9:
		if ((($lI6 > $this->_lookahead))) goto label_10;
		return $lI6;
		label_10:
		return $this->_lookahead;
	}
	public function fill_window__V() {
		$fA0 = null;
		$fI1 = 0;
		$fI2 = 0;
		$lI4 = 0;
		$lI1 = 0;
		$lI3 = 0;
		$lI2 = 0;
		label_0:
		$lI4 = ((int)((((int)(($this->_window_size - $this->_lookahead))) - $this->_strstart)));
		if ((($lI4 != 0))) goto label_1;
		if ((($this->_strstart != 0))) goto label_1;
		if ((($this->_lookahead != 0))) goto label_1;
		$lI4 = $this->_w_size;
		goto label_3;
		label_1:
		if ((($lI4 != -1))) goto label_8;
		$lI4 = ((int)(($lI4 + -1)));
		goto label_3;
		label_8:
		if ((($this->_strstart < ((int)((((int)(($this->_w_size + $this->_w_size))) - 262)))))) goto label_3;
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($this->_window), $this->_w_size, ($this->_window), 0, $this->_w_size);
		$this->_match_start = ((int)(($this->_match_start - $this->_w_size)));
		$this->_strstart = ((int)(($this->_strstart - $this->_w_size)));
		$this->_block_start = ((int)(($this->_block_start - $this->_w_size)));
		$lI1 = $this->_hash_size;
		$lI3 = $lI1;
		label_11:
		$fA0 = $this->_head;
		$lI3 = ((int)(($lI3 + -1)));
		$lI2 = ((int)((($fA0->get($lI3)) & 65535)));
		$fA0 = $this->_head;
		$fI1 = $lI3;
		if ((($lI2 < $this->_w_size))) goto label_12;
		$fI2 = ((int)((N::i2s(((int)(($lI2 - $this->_w_size)))))));
		goto label_13;
		label_12:
		$fI2 = 0;
		label_13:
		$fA0->set($fI1, (N::i2s($fI2)));
		$lI1 = ((int)(($lI1 + -1)));
		if ((($lI1 != 0))) goto label_11;
		$lI1 = $this->_w_size;
		$lI3 = $lI1;
		label_15:
		$fA0 = $this->_prev;
		$lI3 = ((int)(($lI3 + -1)));
		$lI2 = ((int)((($fA0->get($lI3)) & 65535)));
		$fA0 = $this->_prev;
		$fI1 = $lI3;
		if ((($lI2 < $this->_w_size))) goto label_16;
		$fI2 = ((int)((N::i2s(((int)(($lI2 - $this->_w_size)))))));
		goto label_17;
		label_16:
		$fI2 = 0;
		label_17:
		$fA0->set($fI1, (N::i2s($fI2)));
		$lI1 = ((int)(($lI1 + -1)));
		if ((($lI1 != 0))) goto label_15;
		$lI4 = ((int)(($lI4 + $this->_w_size)));
		label_3:
		if ((($this->_strm->_avail_in != 0))) goto label_4;
		return;
		label_4:
		$lI1 = $this->_strm->read_buf__BII_I($this->_window, ((int)(($this->_strstart + $this->_lookahead))), $lI4);
		$this->_lookahead = ((int)(($this->_lookahead + $lI1)));
		if ((($this->_lookahead < 3))) goto label_5;
		$this->_ins_h = ((int)((($this->_window->get($this->_strstart)) & 255)));
		$this->_ins_h = ((int)((((int)((((int)(N::ishl($this->_ins_h, $this->_hash_shift))) ^ ((int)((($this->_window->get(((int)(($this->_strstart + 1))))) & 255)))))) & $this->_hash_mask)));
		label_5:
		if ((($this->_lookahead >= 262))) goto label_7;
		if ((($this->_strm->_avail_in != 0))) goto label_0;
		label_7:
		return;
	}
	public function _tr_tally_II_Z(int $p0, int $p1) {
		$fI0 = 0;
		$lI1 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$tA2 = null;
		$tA5 = null;
		$tA7 = null;
		$tI1 = 0;
		$tI4 = 0;
		$tI6 = 0;
		$lI1 = $p0;
		$this->_pending_buf->set(((int)(($this->_d_buf + ((int)(N::imul($this->_last_lit, 2)))))), (N::i2b(((int)(N::iushr($lI1, 8))))));
		$this->_pending_buf->set(((int)((((int)(($this->_d_buf + ((int)(N::imul($this->_last_lit, 2)))))) + 1))), (N::i2b($lI1)));
		$this->_l_buf->set($this->_last_lit, (N::i2b($p1)));
		$this->_last_lit = ((int)(($this->_last_lit + 1)));
		if ((($lI1 != 0))) goto label_1;
		$tA2 = $this->_dyn_ltree;
		$tI1 = ((int)(N::imul($p1, 2)));
		$tA2->set($tI1, (N::i2s(((int)((($tA2->get($tI1)) + 1))))));
		goto label_3;
		label_1:
		$this->_matches = ((int)(($this->_matches + 1)));
		$lI1 = ((int)(($lI1 + -1)));
		$tA5 = $this->_dyn_ltree;
		$tI4 = ((int)(N::imul(((int)((((int)(((com_jtransc_compression_jzlib_Tree::$__length_code->get($p1)) + 256))) + 1))), 2)));
		$tA5->set($tI4, (N::i2s(((int)((($tA5->get($tI4)) + 1))))));
		$tA7 = $this->_dyn_dtree;
		$tI6 = ((int)(N::imul(com_jtransc_compression_jzlib_Tree::d_code_I_I($lI1), 2)));
		$tA7->set($tI6, (N::i2s(((int)((($tA7->get($tI6)) + 1))))));
		label_3:
		if (((((int)(($this->_last_lit & 8191))) != 0))) goto label_4;
		if ((($this->_level <= 2))) goto label_4;
		$lI3 = ((int)(N::imul($this->_last_lit, 8)));
		$lI4 = ((int)(($this->_strstart - $this->_block_start)));
		$lI5 = 0;
		label_6:
		if ((($lI5 >= 30))) goto label_7;
		$lI3 = N::j2i((N::ladd(N::i2j($lI3), (N::lmul(N::i2j(($this->_dyn_dtree->get(((int)(N::imul($lI5, 2)))))), (N::ladd(Int64::make(0, 5), N::i2j((com_jtransc_compression_jzlib_Tree::$_extra_dbits->get($lI5))))))))));
		$lI5 = ((int)(($lI5 + 1)));
		goto label_6;
		label_7:
		$lI3 = ((int)(N::iushr($lI3, 3)));
		if ((($this->_matches >= ((int)(N::idiv($this->_last_lit, 2)))))) goto label_4;
		if ((($lI3 >= ((int)(N::idiv($lI4, 2)))))) goto label_4;
		return true;
		label_4:
		if ((($this->_last_lit != ((int)(($this->_lit_bufsize - 1)))))) goto label_9;
		$fI0 = 1;
		goto label_10;
		label_9:
		$fI0 = 0;
		label_10:
		return (($fI0)!=0);
	}
	public function deflate_fast_I_I(int $p0) {
		$fI0 = 0;
		$fI1 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$fA0 = null;
		$tA1 = null;
		$tI4 = 0;
		$tA7 = null;
		$tA9 = null;
		$tA3 = null;
		$tA5 = null;
		$lI2 = 0;
		label_1:
		if ((($this->_lookahead >= 262))) goto label_2;
		$this->fill_window__V();
		if ((($this->_lookahead >= 262))) goto label_4;
		if ((($p0 != 0))) goto label_4;
		return 0;
		label_4:
		if ((($this->_lookahead != 0))) goto label_2;
		goto label_6;
		label_2:
		if ((($this->_lookahead < 3))) goto label_14;
		$this->_ins_h = ((int)((((int)((((int)(N::ishl($this->_ins_h, $this->_hash_shift))) ^ ((int)((($this->_window->get(((int)(($this->_strstart + 2))))) & 255)))))) & $this->_hash_mask)));
		$lI2 = ((int)((($this->_head->get($this->_ins_h)) & 65535)));
		$this->_prev->set(((int)(($this->_strstart & $this->_w_mask))), ($this->_head->get($this->_ins_h)));
		$this->_head->set($this->_ins_h, (N::i2s($this->_strstart)));
		label_14:
		if (((((int)(N::lcmp(N::i2j($lI2), Int64::make(0, 0)))) == 0))) goto label_16;
		if (((((int)((((int)(($this->_strstart - $lI2))) & 65535))) > ((int)(($this->_w_size - 262)))))) goto label_16;
		if ((($this->_strategy == 2))) goto label_16;
		$this->_match_length = $this->longest_match_I_I($lI2);
		label_16:
		if ((($this->_match_length < 3))) goto label_19;
		$lI3 = ((int)(N::z2i($this->_tr_tally_II_Z(((int)(($this->_strstart - $this->_match_start))), ((int)(($this->_match_length - 3)))))));
		$this->_lookahead = ((int)(($this->_lookahead - $this->_match_length)));
		if ((($this->_match_length > $this->_max_lazy_match))) goto label_21;
		if ((($this->_lookahead < 3))) goto label_21;
		$tA1 = $this;
		$tA1->_match_length = ((int)(($tA1->_match_length - 1)));
		label_23:
		$this->_strstart = ((int)(($this->_strstart + 1)));
		$this->_ins_h = ((int)((((int)((((int)(N::ishl($this->_ins_h, $this->_hash_shift))) ^ ((int)((($this->_window->get(((int)(($this->_strstart + 2))))) & 255)))))) & $this->_hash_mask)));
		$lI2 = ((int)((($this->_head->get($this->_ins_h)) & 65535)));
		$this->_prev->set(((int)(($this->_strstart & $this->_w_mask))), ($this->_head->get($this->_ins_h)));
		$this->_head->set($this->_ins_h, (N::i2s($this->_strstart)));
		$tA3 = ($this);
		$fA0 = $tA3;
		$tA5 = $fA0;
		$tI4 = ((int)((($tA3)->_match_length - 1)));
		$fI0 = $tI4;
		($tA5)->_match_length = $tI4;
		if ((($fI0 != 0))) goto label_23;
		$this->_strstart = ((int)(($this->_strstart + 1)));
		goto label_25;
		label_21:
		$tA7 = $this;
		$tA7->_strstart = ((int)(($tA7->_strstart + $this->_match_length)));
		$this->_match_length = 0;
		$this->_ins_h = ((int)((($this->_window->get($this->_strstart)) & 255)));
		$this->_ins_h = ((int)((((int)((((int)(N::ishl($this->_ins_h, $this->_hash_shift))) ^ ((int)((($this->_window->get(((int)(($this->_strstart + 1))))) & 255)))))) & $this->_hash_mask)));
		goto label_25;
		label_19:
		$lI3 = ((int)(N::z2i($this->_tr_tally_II_Z(0, ((int)((($this->_window->get($this->_strstart)) & 255)))))));
		$this->_lookahead = ((int)(($this->_lookahead - 1)));
		$tA9 = $this;
		$tA9->_strstart = ((int)(($tA9->_strstart + 1)));
		label_25:
		if ((($lI3 == 0))) goto label_1;
		$this->flush_block_only_Z_V(false);
		if ((($this->_strm->_avail_out != 0))) goto label_1;
		return 0;
		label_6:
		$fA0 = ($this);
		if ((($p0 != 4))) goto label_7;
		$fI1 = 1;
		goto label_8;
		label_7:
		$fI1 = 0;
		label_8:
		($fA0)->flush_block_only_Z_V((($fI1)!=0));
		if ((($this->_strm->_avail_out != 0))) goto label_9;
		if ((($p0 != 4))) goto label_11;
		return 2;
		label_11:
		return 0;
		label_9:
		if ((($p0 != 4))) goto label_12;
		$fI0 = 3;
		goto label_13;
		label_12:
		$fI0 = 1;
		label_13:
		return $fI0;
	}
	public function _tr_align__V() {
		$this->send_bits_II_V(2, 3);
		$this->send_code_I_S_V(256, com_jtransc_compression_jzlib_StaticTree::$_static_ltree);
		$this->bi_flush__V();
		if (((((int)((((int)((((int)((1 + $this->_last_eob_len))) + 10))) - $this->_bi_valid))) >= 9))) goto label_1;
		$this->send_bits_II_V(2, 3);
		$this->send_code_I_S_V(256, com_jtransc_compression_jzlib_StaticTree::$_static_ltree);
		$this->bi_flush__V();
		label_1:
		$this->_last_eob_len = 7;
		return;
	}
	public function bi_flush__V() {
		if ((($this->_bi_valid != 16))) goto label_1;
		$this->put_short_I_V(((int)($this->_bi_buf)));
		$this->_bi_buf = 0;
		$this->_bi_valid = 0;
		goto label_3;
		label_1:
		if ((($this->_bi_valid < 8))) goto label_3;
		$this->put_byte_B_V((N::i2b($this->_bi_buf)));
		$this->_bi_buf = (N::i2s(((int)(N::iushr($this->_bi_buf, 8)))));
		$this->_bi_valid = ((int)(($this->_bi_valid - 8)));
		label_3:
		return;
	}
	public function deflate_stored_I_I(int $p0) {
		$fI1 = 0;
		$fI0 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$lI2 = 65535;
		if ((($lI2 <= ((int)(($this->_pending_buf_size - 5)))))) goto label_1;
		$lI2 = ((int)(($this->_pending_buf_size - 5)));
		label_1:
		if ((($this->_lookahead > 1))) goto label_3;
		$this->fill_window__V();
		if ((($this->_lookahead != 0))) goto label_5;
		if ((($p0 != 0))) goto label_5;
		return 0;
		label_5:
		if ((($this->_lookahead != 0))) goto label_3;
		goto label_6;
		label_3:
		$this->_strstart = ((int)(($this->_strstart + $this->_lookahead)));
		$this->_lookahead = 0;
		$lI3 = ((int)(($this->_block_start + $lI2)));
		if ((($this->_strstart == 0))) goto label_15;
		if ((($this->_strstart < $lI3))) goto label_16;
		label_15:
		$this->_lookahead = ((int)(($this->_strstart - $lI3)));
		$this->_strstart = $lI3;
		$this->flush_block_only_Z_V(false);
		if ((($this->_strm->_avail_out != 0))) goto label_16;
		return 0;
		label_16:
		if (((((int)(($this->_strstart - $this->_block_start))) < ((int)(($this->_w_size - 262)))))) goto label_1;
		$this->flush_block_only_Z_V(false);
		if ((($this->_strm->_avail_out != 0))) goto label_1;
		return 0;
		label_6:
		if ((($p0 != 4))) goto label_7;
		$fI1 = 1;
		goto label_8;
		label_7:
		$fI1 = 0;
		label_8:
		$this->flush_block_only_Z_V((($fI1)!=0));
		if ((($this->_strm->_avail_out != 0))) goto label_9;
		if ((($p0 != 4))) goto label_11;
		$fI0 = 2;
		goto label_12;
		label_11:
		$fI0 = 0;
		label_12:
		return $fI0;
		label_9:
		if ((($p0 != 4))) goto label_13;
		$fI0 = 3;
		goto label_14;
		label_13:
		$fI0 = 1;
		label_14:
		return $fI0;
	}
	public function getGZIPHeader__Lcom_jtransc_compression_jzlib_GZIPHeader_() {
		$fA1 = null;
		$tA0 = null;
		if ((($this->_gheader != null))) goto label_1;
		$tA0 = ((new com_jtransc_compression_jzlib_GZIPHeader()));
		$fA1 = $tA0;
		($tA0)->com_jtransc_compression_jzlib_GZIPHeader_init___V();
		$this->_gheader = ($fA1);
		label_1:
		return $this->_gheader;
	}
	public function putShortMSB_I_V(int $p0) {
		$this->put_byte_B_V((N::i2b(((int)(N::ishr($p0, 8))))));
		$this->put_byte_B_V((N::i2b($p0)));
		return;
	}
	public function deflateInit_II_I(int $p0, int $p1) {
		return $this->deflateInit_IIIII_I($p0, 8, $p1, 8, 0);
	}
	public function deflateInit_IIIII_I(int $p0, int $p1, int $p2, int $p3, int $p4) {
		$fA0 = null;
		$lI1 = 0;
		$lI3 = 0;
		$lI6 = 0;
		$fA1 = null;
		$tA0 = null;
		$lI1 = $p0;
		$lI3 = $p2;
		$lI6 = 1;
		$this->_strm->_msg = null;
		if ((($lI1 != -1))) goto label_1;
		$lI1 = 6;
		label_1:
		if ((($lI3 >= 0))) goto label_2;
		$lI6 = 0;
		$lI3 = ((int)(-($lI3)));
		goto label_4;
		label_2:
		if ((($lI3 <= 15))) goto label_4;
		$lI6 = 2;
		$lI3 = ((int)(($lI3 + -16)));
		$fA0 = $this->_strm;
		$tA0 = ((new com_jtransc_compression_jzlib_CRC32()));
		$fA1 = $tA0;
		($tA0)->com_jtransc_compression_jzlib_CRC32_init___V();
		$fA0->_adler = ($fA1);
		label_4:
		if ((($p3 < 1))) goto label_5;
		if ((($p3 > 9))) goto label_5;
		if ((($p1 != 8))) goto label_5;
		if ((($lI3 < 9))) goto label_5;
		if ((($lI3 > 15))) goto label_5;
		if ((($lI1 < 0))) goto label_5;
		if ((($lI1 > 9))) goto label_5;
		if ((($p4 < 0))) goto label_5;
		if ((($p4 <= 2))) goto label_6;
		label_5:
		return -2;
		label_6:
		$this->_strm->_dstate = $this;
		$this->_wrap = $lI6;
		$this->_w_bits = $lI3;
		$this->_w_size = ((int)(N::ishl(1, $this->_w_bits)));
		$this->_w_mask = ((int)(($this->_w_size - 1)));
		$this->_hash_bits = ((int)(($p3 + 7)));
		$this->_hash_size = ((int)(N::ishl(1, $this->_hash_bits)));
		$this->_hash_mask = ((int)(($this->_hash_size - 1)));
		$this->_hash_shift = ((int)(N::idiv(((int)((((int)(($this->_hash_bits + 3))) - 1))), 3)));
		$this->_window = new JA_B(((int)(N::imul($this->_w_size, 2))));
		$this->_prev = new JA_S($this->_w_size);
		$this->_head = new JA_S($this->_hash_size);
		$this->_lit_bufsize = ((int)(N::ishl(1, ((int)(($p3 + 6))))));
		$this->_pending_buf = new JA_B(((int)(N::imul($this->_lit_bufsize, 3))));
		$this->_pending_buf_size = ((int)(N::imul($this->_lit_bufsize, 3)));
		$this->_d_buf = $this->_lit_bufsize;
		$this->_l_buf = new JA_B($this->_lit_bufsize);
		$this->_level = $lI1;
		$this->_strategy = $p4;
		$this->_method = (N::i2b($p1));
		return $this->deflateReset__I();
	}
	public function deflateReset__I() {
		$fJ1 = Int64::make(0, 0);
		$fI1 = 0;
		$tJ0 = Int64::make(0, 0);
		$fA0 = null;
		$fA1 = null;
		$tA1 = null;
		$fA0 = ($this->_strm);
		$fA1 = ($this->_strm);
		$tA1 = $fA1;
		$tJ0 = Int64::make(0, 0);
		$fJ1 = $tJ0;
		($tA1)->_total_out = $tJ0;
		($fA0)->_total_in = $fJ1;
		$this->_strm->_msg = null;
		$this->_strm->_data_type = 2;
		$this->_pending = 0;
		$this->_pending_out = 0;
		if ((($this->_wrap >= 0))) goto label_1;
		$this->_wrap = ((int)(-($this->_wrap)));
		label_1:
		$fA0 = ($this);
		if ((($this->_wrap != 0))) goto label_3;
		$fI1 = 113;
		goto label_4;
		label_3:
		$fI1 = 42;
		label_4:
		($fA0)->_status = $fI1;
		$this->_strm->_adler->reset__V();
		$this->_last_flush = 0;
		$this->tr_init__V();
		$this->lm_init__V();
		return 0;
	}
	public function lm_init__V() {
		$fI1 = 0;
		$lI1 = 0;
		$tI0 = 0;
		$this->_window_size = ((int)(N::imul(2, $this->_w_size)));
		$this->_head->set(((int)(($this->_hash_size - 1))), 0);
		$lI1 = 0;
		label_1:
		if ((($lI1 >= ((int)(($this->_hash_size - 1)))))) goto label_2;
		$this->_head->set($lI1, 0);
		$lI1 = ((int)(($lI1 + 1)));
		goto label_1;
		label_2:
		$this->_max_lazy_match = (((com_jtransc_compression_jzlib_Deflate::$_config_table)->get($this->_level)))->_max_lazy;
		$this->_good_match = (((com_jtransc_compression_jzlib_Deflate::$_config_table)->get($this->_level)))->_good_length;
		$this->_nice_match = (((com_jtransc_compression_jzlib_Deflate::$_config_table)->get($this->_level)))->_nice_length;
		$this->_max_chain_length = (((com_jtransc_compression_jzlib_Deflate::$_config_table)->get($this->_level)))->_max_chain;
		$this->_strstart = 0;
		$this->_block_start = 0;
		$this->_lookahead = 0;
		$tI0 = 2;
		$fI1 = $tI0;
		$this->_prev_length = $tI0;
		$this->_match_length = $fI1;
		$this->_match_available = 0;
		$this->_ins_h = 0;
		return;
	}
	public function tr_init__V() {
		$this->_l_desc->_dyn_tree = $this->_dyn_ltree;
		$this->_l_desc->_stat_desc = com_jtransc_compression_jzlib_StaticTree::$_static_l_desc;
		$this->_d_desc->_dyn_tree = $this->_dyn_dtree;
		$this->_d_desc->_stat_desc = com_jtransc_compression_jzlib_StaticTree::$_static_d_desc;
		$this->_bl_desc->_dyn_tree = $this->_bl_tree;
		$this->_bl_desc->_stat_desc = com_jtransc_compression_jzlib_StaticTree::$_static_bl_desc;
		$this->_bi_buf = 0;
		$this->_bi_valid = 0;
		$this->_last_eob_len = 8;
		$this->init_block__V();
		return;
	}
	public function __construct($CLASS_ID = 895) {
		parent::__construct($CLASS_ID);
		$this->_bl_desc = null;
		$this->_l_desc = null;
		$this->_dyn_ltree = null;
		$this->_next_code = null;
		$this->_depth = null;
		$this->_strm = null;
		$this->_dyn_dtree = null;
		$this->_d_desc = null;
		$this->_gheader = null;
		$this->_wrap = 0;
		$this->_bl_count = null;
		$this->_heap = null;
		$this->_bl_tree = null;
		$this->_prev = null;
		$this->_d_buf = 0;
		$this->_pending_buf = null;
		$this->_head = null;
		$this->_l_buf = null;
		$this->_window = null;
		$this->_pending = 0;
		$this->_level = 0;
		$this->_lookahead = 0;
		$this->_last_flush = 0;
		$this->_w_bits = 0;
		$this->_hash_size = 0;
		$this->_status = 0;
		$this->_strstart = 0;
		$this->_pending_out = 0;
		$this->_max_lazy_match = 0;
		$this->_w_size = 0;
		$this->_match_start = 0;
		$this->_strategy = 0;
		$this->_hash_mask = 0;
		$this->_match_available = 0;
		$this->_ins_h = 0;
		$this->_hash_shift = 0;
		$this->_w_mask = 0;
		$this->_prev_match = 0;
		$this->_match_length = 0;
		$this->_prev_length = 0;
		$this->_block_start = 0;
		$this->_static_len = 0;
		$this->_data_type = 0;
		$this->_opt_len = 0;
		$this->_bi_valid = 0;
		$this->_bi_buf = 0;
		$this->_matches = 0;
		$this->_last_lit = 0;
		$this->_heap_len = 0;
		$this->_heap_max = 0;
		$this->_last_eob_len = 0;
		$this->_max_chain_length = 0;
		$this->_nice_match = 0;
		$this->_good_match = 0;
		$this->_window_size = 0;
		$this->_lit_bufsize = 0;
		$this->_pending_buf_size = 0;
		$this->_hash_bits = 0;
		$this->_method = 0;
	}
	static public function SI() {
		com_jtransc_compression_jzlib_Deflate::$_z_errmsg = null;
		com_jtransc_compression_jzlib_Deflate::$_config_table = null;
		com_jtransc_compression_jzlib_Deflate::com_jtransc_compression_jzlib_Deflate_clinit___V();
	}
}
class com_jtransc_compression_jzlib_ZStream extends java_lang_Object {

	public $_dstate = null;
	public $_avail_out = 0;
	public $_next_out = null;
	public $_next_in = null;
	public $_avail_in = 0;
	public $_msg = null;
	public $_total_in = null;
	public $_adler = null;
	public $_next_out_index = 0;
	public $_total_out = null;
	public $_next_in_index = 0;
	public $_data_type = 0;
	public function flush_pending__V() {
		$lI1 = 0;
		$tA1 = null;
		$tA4 = null;
		$lI1 = $this->_dstate->_pending;
		if ((($lI1 <= $this->_avail_out))) goto label_1;
		$lI1 = $this->_avail_out;
		label_1:
		if ((($lI1 != 0))) goto label_2;
		return;
		label_2:
		if (((($this->_dstate->_pending_buf)->length <= $this->_dstate->_pending_out))) goto label_3;
		if (((($this->_next_out)->length <= $this->_next_out_index))) goto label_3;
		if (((($this->_dstate->_pending_buf)->length < ((int)(($this->_dstate->_pending_out + $lI1)))))) goto label_3;
		if (((($this->_next_out)->length >= ((int)(($this->_next_out_index + $lI1)))))) goto label_3;
		label_3:
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($this->_dstate->_pending_buf), $this->_dstate->_pending_out, ($this->_next_out), $this->_next_out_index, $lI1);
		$this->_next_out_index = ((int)(($this->_next_out_index + $lI1)));
		$tA1 = $this->_dstate;
		$tA1->_pending_out = ((int)(($tA1->_pending_out + $lI1)));
		$this->_total_out = (N::ladd($this->_total_out, N::i2j($lI1)));
		$this->_avail_out = ((int)(($this->_avail_out - $lI1)));
		$tA4 = $this->_dstate;
		$tA4->_pending = ((int)(($tA4->_pending - $lI1)));
		if ((($this->_dstate->_pending != 0))) goto label_4;
		$this->_dstate->_pending_out = 0;
		label_4:
		return;
	}
	public function read_buf__BII_I(?JA_B $p0, int $p1, int $p2) {
		$lI4 = 0;
		$lI4 = $this->_avail_in;
		if ((($lI4 <= $p2))) goto label_1;
		$lI4 = $p2;
		label_1:
		if ((($lI4 != 0))) goto label_2;
		return 0;
		label_2:
		$this->_avail_in = ((int)(($this->_avail_in - $lI4)));
		if ((($this->_dstate->_wrap == 0))) goto label_3;
		$this->_adler->update__BII_V($this->_next_in, $this->_next_in_index, $lI4);
		label_3:
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($this->_next_in), $this->_next_in_index, ($p0), $p1, $lI4);
		$this->_next_in_index = ((int)(($this->_next_in_index + $lI4)));
		$this->_total_in = (N::ladd($this->_total_in, N::i2j($lI4)));
		return $lI4;
	}
	public function deflate_I_I(int $p0) {
		if ((($this->_dstate != null))) goto label_1;
		return -2;
		label_1:
		return $this->_dstate->deflate_I_I($p0);
	}
	public function com_jtransc_compression_jzlib_ZStream_init___V() {
		$fA1 = null;
		$tA0 = null;
		$tA0 = ((new com_jtransc_compression_jzlib_Adler32()));
		$fA1 = $tA0;
		($tA0)->com_jtransc_compression_jzlib_Adler32_init___V();
		$this->com_jtransc_compression_jzlib_ZStream_init__Lcom_jtransc_compression_jzlib_Checksum__V(($fA1));
		return $this;
		return $this;
	}
	public function com_jtransc_compression_jzlib_ZStream_init__Lcom_jtransc_compression_jzlib_Checksum__V(?com_jtransc_compression_jzlib_Checksum $p0) {
		($this)->java_lang_Object_init___V();
		$this->_adler = $p0;
		return $this;
		return $this;
	}
	public function setOutput__BII_V(?JA_B $p0, int $p1, int $p2) {
		$this->_next_out = $p0;
		$this->_next_out_index = $p1;
		$this->_avail_out = $p2;
		return;
	}
	public function setInput__BIIZ_V(?JA_B $p0, int $p1, int $p2, bool $p3) {
		$lA5 = null;
		if ((($p2 > 0))) goto label_1;
		if (!($p3)) goto label_1;
		if ((($this->_next_in == null))) goto label_1;
		return;
		label_1:
		if ((($this->_avail_in <= 0))) goto label_2;
		if (!($p3)) goto label_2;
		$lA5 = (new JA_B(((int)(($this->_avail_in + $p2)))));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($this->_next_in), $this->_next_in_index, $lA5, 0, $this->_avail_in);
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($p0), $p1, $lA5, $this->_avail_in, $p2);
		$this->_next_in = ($lA5);
		$this->_next_in_index = 0;
		$this->_avail_in = ((int)(($this->_avail_in + $p2)));
		goto label_4;
		label_2:
		$this->_next_in = $p0;
		$this->_next_in_index = $p1;
		$this->_avail_in = $p2;
		label_4:
		return;
	}
	public function getTotalOut__J() {
		return $this->_total_out;
	}
	public function __construct($CLASS_ID = 894) {
		parent::__construct($CLASS_ID);
		$this->_dstate = null;
		$this->_avail_out = 0;
		$this->_next_out = null;
		$this->_next_in = null;
		$this->_avail_in = 0;
		$this->_msg = null;
		$this->_total_in = Int64::make(0, 0);
		$this->_adler = null;
		$this->_next_out_index = 0;
		$this->_total_out = Int64::make(0, 0);
		$this->_next_in_index = 0;
		$this->_data_type = 0;
	}
	static public function SI() {
	}
}
class com_jtransc_compression_jzlib_Deflater extends com_jtransc_compression_jzlib_ZStream {

	public $_finished = false;
	public function deflate_I_I(int $p0) {
		$lI2 = 0;
		if ((($this->_dstate != null))) goto label_1;
		return -2;
		label_1:
		$lI2 = $this->_dstate->deflate_I_I($p0);
		if ((($lI2 != 1))) goto label_3;
		$this->_finished = true;
		label_3:
		return $lI2;
	}
	public function com_jtransc_compression_jzlib_Deflater_init__IZ_V(int $p0, bool $p1) {
		$this->com_jtransc_compression_jzlib_Deflater_init__IIZ_V($p0, 15, $p1);
		return $this;
		return $this;
	}
	public function com_jtransc_compression_jzlib_Deflater_init__IIZ_V(int $p0, int $p1, bool $p2) {
		$lI4 = 0;
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		($this)->com_jtransc_compression_jzlib_ZStream_init___V();
		$this->_finished = false;
		$lI4 = $this->init_IIZ_I($p0, $p1, $p2);
		if ((($lI4 == 0))) goto label_1;
		$tA0 = ((new com_jtransc_compression_jzlib_GZIPException()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->com_jtransc_compression_jzlib_GZIPException_init__Ljava_lang_String__V(($fA2)->append_I_Ljava_lang_StringBuilder_($lI4)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_30)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->_msg)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_1:
		return $this;
		return $this;
	}
	public function com_jtransc_compression_jzlib_Deflater_init___V() {
		($this)->com_jtransc_compression_jzlib_ZStream_init___V();
		$this->_finished = false;
		return $this;
		return $this;
	}
	public function init_IIZ_I(int $p0, int $p1, bool $p2) {
		$fI1 = 0;
		$fI2 = 0;
		$fA0 = null;
		$fA1 = null;
		$tA0 = null;
		$this->_finished = false;
		$fA0 = ($this);
		$tA0 = ((new com_jtransc_compression_jzlib_Deflate()));
		$fA1 = $tA0;
		($tA0)->com_jtransc_compression_jzlib_Deflate_init__Lcom_jtransc_compression_jzlib_ZStream__V(($this));
		($fA0)->_dstate = ($fA1);
		$fA0 = ($this->_dstate);
		$fI1 = $p0;
		if (!($p2)) goto label_1;
		$fI2 = ((int)(-($p1)));
		goto label_2;
		label_1:
		$fI2 = $p1;
		label_2:
		return ($fA0)->deflateInit_II_I($fI1, $fI2);
	}
	public function __construct($CLASS_ID = 893) {
		parent::__construct($CLASS_ID);
		$this->_finished = false;
	}
	static public function SI() {
	}
}
class com_jtransc_compression_jzlib_CRC32 extends java_lang_Object implements com_jtransc_compression_jzlib_Checksum {

	public $_v = 0;
	public static $_crc_table = null;
	public function com_jtransc_compression_jzlib_CRC32_init___V() {
		($this)->java_lang_Object_init___V();
		$this->_v = 0;
		return $this;
		return $this;
	}
	public static function com_jtransc_compression_jzlib_CRC32_clinit___V() {
		$lI0 = 0;
		$lI1 = 0;
		$lI2 = 0;
		com_jtransc_compression_jzlib_CRC32::$_crc_table = null;
		com_jtransc_compression_jzlib_CRC32::$_crc_table = new JA_I(256);
		$lI0 = 0;
		label_1:
		if ((($lI0 >= 256))) goto label_2;
		$lI1 = $lI0;
		$lI2 = 8;
		label_4:
		$lI2 = ((int)(($lI2 + -1)));
		if ((($lI2 < 0))) goto label_5;
		if (((((int)(($lI1 & 1))) == 0))) goto label_7;
		$lI1 = ((int)((-306674912 ^ ((int)(N::iushr($lI1, 1))))));
		goto label_4;
		label_7:
		$lI1 = ((int)(N::iushr($lI1, 1)));
		goto label_4;
		label_5:
		com_jtransc_compression_jzlib_CRC32::$_crc_table->set($lI0, $lI1);
		$lI0 = ((int)(($lI0 + 1)));
		goto label_1;
		label_2:
		return;
	}
	public function getValue__I() {
		return $this->_v;
	}
	public function reset__V() {
		$this->_v = 0;
		return;
	}
	public function update__BII_V(?JA_B $p0, int $p1, int $p2) {
		$fI1 = 0;
		$fI3 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$fA0 = null;
		$lI2 = $p1;
		$lI3 = $p2;
		$lI4 = ((int)(($this->_v ^ -1)));
		label_1:
		$lI3 = ((int)(($lI3 + -1)));
		if ((($lI3 < 0))) goto label_2;
		$fA0 = com_jtransc_compression_jzlib_CRC32::$_crc_table;
		$fI1 = $lI4;
		$fI3 = $lI2;
		$lI2 = ((int)(($lI2 + 1)));
		$lI4 = ((int)((($fA0->get(((int)((((int)(($fI1 ^ ($p0->get($fI3))))) & 255))))) ^ ((int)(N::iushr($lI4, 8))))));
		goto label_1;
		label_2:
		$this->_v = ((int)(($lI4 ^ -1)));
		return;
	}
	public function __construct($CLASS_ID = 891) {
		parent::__construct($CLASS_ID);
		$this->_v = 0;
	}
	static public function SI() {
		com_jtransc_compression_jzlib_CRC32::$_crc_table = null;
		com_jtransc_compression_jzlib_CRC32::com_jtransc_compression_jzlib_CRC32_clinit___V();
	}
}
interface java_util_zip_Checksum {

	public function getValue__J();
	public function reset__V();
	public function update__BII_V(?JA_B $p0, int $p1, int $p2);
}
class java_util_zip_Checksum_IFields {

	static public function SI() {
	}
}
class java_util_zip_CRC32 extends java_lang_Object implements java_util_zip_Checksum {

	public $_impl = null;
	public $_tbytes = null;
	public static $_temp = null;
	public function java_util_zip_CRC32_init___V() {
		$fA1 = null;
		$tA0 = null;
		($this)->java_lang_Object_init___V();
		$tA0 = ((new com_jtransc_compression_jzlib_CRC32()));
		$fA1 = $tA0;
		($tA0)->com_jtransc_compression_jzlib_CRC32_init___V();
		$this->_impl = ($fA1);
		$this->_tbytes = Int64::make(0, 0);
		return $this;
		return $this;
	}
	public static function java_util_zip_CRC32_clinit___V() {
		java_util_zip_CRC32::$_temp = new JA_B(1);
		return;
	}
	public function getValue__J() {
		return (N::land(N::i2j($this->_impl->getValue__I()), Int64::make(0, -1)));
	}
	public function reset__V() {
		$this->_impl->reset__V();
		$this->_tbytes = Int64::make(0, 0);
		return;
	}
	public function update__BII_V(?JA_B $p0, int $p1, int $p2) {
		$this->_update__BII_V($p0, $p1, $p2);
		return;
	}
	public function _update__BII_V(?JA_B $p0, int $p1, int $p2) {
		$this->_impl->update__BII_V($p0, $p1, $p2);
		$this->_tbytes = (N::ladd($this->_tbytes, N::i2j($p2)));
		return;
	}
	public function __construct($CLASS_ID = 889) {
		parent::__construct($CLASS_ID);
		$this->_impl = null;
		$this->_tbytes = Int64::make(0, 0);
	}
	static public function SI() {
		java_util_zip_CRC32::$_temp = null;
		java_util_zip_CRC32::java_util_zip_CRC32_clinit___V();
	}
}
interface java_nio_internal_ByteBufferAs {

}
class java_nio_internal_ByteBufferAs_IFields {

	static public function SI() {
	}
}
abstract class java_nio_Buffer extends java_lang_Object {

	public $__elementSizeShift = 0;
	public $_mark = 0;
	public $_block = null;
	public $_position = 0;
	public $_limit = 0;
	public $_capacity = 0;
	public function java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(int $p0, int $p1, ?java_nio_internal_MemoryBlock $p2) {
		$fI1 = 0;
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		$tA3 = null;
		($this)->java_lang_Object_init___V();
		$this->_mark = -1;
		$this->_position = 0;
		$this->__elementSizeShift = $p0;
		if ((($p1 >= 0))) goto label_1;
		$tA0 = ((new java_lang_IllegalArgumentException()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_IllegalArgumentException_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_31)->append_I_Ljava_lang_StringBuilder_($p1)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_1:
		$fA0 = ($this);
		$fA1 = ($this);
		$tA3 = $fA1;
		$fI1 = $p1;
		($tA3)->_limit = $p1;
		($fA0)->_capacity = $fI1;
		$this->_block = $p2;
		return $this;
		return $this;
	}
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(($this)->getClass__Ljava_lang_Class_()->getName__Ljava_lang_String_())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_35)->append_I_Ljava_lang_StringBuilder_($this->_position)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_34)->append_I_Ljava_lang_StringBuilder_($this->_limit)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_33)->append_I_Ljava_lang_StringBuilder_($this->_capacity)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_32)->toString__Ljava_lang_String_();
	}
	public function remaining__I() {
		return ((int)(($this->_limit - $this->_position)));
	}
	public function checkIndex_I_V(int $p0) {
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		if ((($p0 < 0))) goto label_1;
		if ((($p0 < $this->_limit))) goto label_2;
		label_1:
		$tA0 = ((new java_lang_IndexOutOfBoundsException()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_37)->append_I_Ljava_lang_StringBuilder_($p0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_36)->append_I_Ljava_lang_StringBuilder_($this->_limit)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_2:
		return;
	}
	public function capacity__I() {
		return $this->_capacity;
	}
	public function clear__Ljava_nio_Buffer_() {
		$this->_position = 0;
		$this->_mark = -1;
		$this->_limit = $this->_capacity;
		return $this;
	}
	public function __construct($CLASS_ID = 854) {
		parent::__construct($CLASS_ID);
		$this->__elementSizeShift = 0;
		$this->_mark = 0;
		$this->_block = null;
		$this->_position = 0;
		$this->_limit = 0;
		$this->_capacity = 0;
	}
	static public function SI() {
	}
}
abstract class java_nio_IntBuffer extends java_nio_Buffer implements java_lang_Comparable {

	public function java_nio_IntBuffer_init__I_V(int $p0) {
		($this)->java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(2, $p0, null);
		return $this;
		return $this;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA2 = null;
		$fI1 = 0;
		$fI0 = 0;
		$fI2 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$fA1 = null;
		if ((($p0) instanceof java_nio_IntBuffer)) goto label_1;
		return false;
		label_1:
		$lA2 = (N::checkcast($p0, "java_nio_IntBuffer"));
		if ((($this->remaining__I() == ($lA2)->remaining__I()))) goto label_3;
		return false;
		label_3:
		$lI3 = $this->_position;
		$lI4 = ($lA2)->_position;
		$lI5 = 1;
		label_5:
		if ((($lI5 == 0))) goto label_6;
		if ((($lI3 >= $this->_limit))) goto label_6;
		$fI1 = $lI3;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = $this->get_I_I($fI1);
		$fA1 = $lA2;
		$fI2 = $lI4;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1)->get_I_I($fI2)))) goto label_8;
		$fI0 = 1;
		goto label_9;
		label_8:
		$fI0 = 0;
		label_9:
		$lI5 = $fI0;
		goto label_5;
		label_6:
		return (($lI5)!=0);
	}
	public function get_I_I(int $p0) {
		throw new Exception("Missing body java.nio.IntBuffer.get(I)I");
	}
	public function hashCode__I() {
		$fI0 = 0;
		$fI2 = 0;
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = $this->_position;
		$lI2 = 0;
		label_1:
		if ((($lI1 >= $this->_limit))) goto label_2;
		$fI0 = $lI2;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$lI2 = ((int)(($fI0 + $this->get_I_I($fI2))));
		goto label_1;
		label_2:
		return $lI2;
	}
	public function __construct($CLASS_ID = 853) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_nio_ByteBufferAsIntBuffer extends java_nio_IntBuffer implements java_nio_internal_ByteBufferAs {

	public $_byteBuffer = null;
	public $_bytes = null;
	public function java_nio_ByteBufferAsIntBuffer_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_IntBuffer_init__I_V(((int)(N::idiv($p0->capacity__I(), 4))));
		$this->_byteBuffer = $p0;
		$this->_byteBuffer->clear__Ljava_nio_Buffer_();
		$this->_bytes = $p0->array___B();
		$this->init__B_V($p0->array___B());
		return $this;
		return $this;
	}
	public function init__B_V(?JA_B $p0) {
		return;
	}
	public function get_I_I(int $p0) {
		$this->checkIndex_I_V($p0);
		return libcore_io_Memory::peekAlignedIntLE__BI_I($this->_bytes, $p0);
	}
	public static function asIntBuffer_Ljava_nio_ByteBuffer__Ljava_nio_IntBuffer_(?java_nio_ByteBuffer $p0) {
		$lA1 = null;
		$lA1 = $p0->slice__Ljava_nio_ByteBuffer_();
		$lA1->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_($p0->order__Ljava_nio_ByteOrder_());
		return (java_nio_ByteBufferAsIntBuffer::create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsIntBuffer_($lA1, $p0->_isLittleEndian));
	}
	public static function create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsIntBuffer_(?java_nio_ByteBuffer $p0, bool $p1) {
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		if (!($p1)) goto label_1;
		$tA0 = ((new java_nio_ByteBufferAsIntBuffer_LE()));
		$fA0 = $tA0;
		($tA0)->java_nio_ByteBufferAsIntBuffer_LE_init__Ljava_nio_ByteBuffer__V($p0);
		goto label_2;
		label_1:
		$tA1 = ((new java_nio_ByteBufferAsIntBuffer_BE()));
		$fA0 = $tA1;
		($tA1)->java_nio_ByteBufferAsIntBuffer_BE_init__Ljava_nio_ByteBuffer__V($p0);
		label_2:
		return ($fA0);
	}
	public function __construct($CLASS_ID = 886) {
		parent::__construct($CLASS_ID);
		$this->_byteBuffer = null;
		$this->_bytes = null;
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsIntBuffer_LE extends java_nio_ByteBufferAsIntBuffer {

	public function java_nio_ByteBufferAsIntBuffer_LE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsIntBuffer_init__Ljava_nio_ByteBuffer__V($p0);
		return $this;
		return $this;
	}
	public function get_I_I(int $p0) {
		return parent::get_I_I($p0);
	}
	public function __construct($CLASS_ID = 888) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsIntBuffer_BE extends java_nio_ByteBufferAsIntBuffer {

	public function java_nio_ByteBufferAsIntBuffer_BE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsIntBuffer_init__Ljava_nio_ByteBuffer__V($p0);
		return $this;
		return $this;
	}
	public function get_I_I(int $p0) {
		return libcore_io_Memory::peekAlignedIntBE__BI_I($this->_bytes, $p0);
	}
	public function __construct($CLASS_ID = 887) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_nio_ShortBuffer extends java_nio_Buffer implements java_lang_Comparable {

	public function java_nio_ShortBuffer_init__I_V(int $p0) {
		($this)->java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(1, $p0, null);
		return $this;
		return $this;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA2 = null;
		$fI1 = 0;
		$fI0 = 0;
		$fI2 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$fA1 = null;
		if ((($p0) instanceof java_nio_ShortBuffer)) goto label_1;
		return false;
		label_1:
		$lA2 = (N::checkcast($p0, "java_nio_ShortBuffer"));
		if ((($this->remaining__I() == ($lA2)->remaining__I()))) goto label_3;
		return false;
		label_3:
		$lI3 = $this->_position;
		$lI4 = ($lA2)->_position;
		$lI5 = 1;
		label_5:
		if ((($lI5 == 0))) goto label_6;
		if ((($lI3 >= $this->_limit))) goto label_6;
		$fI1 = $lI3;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = ((int)($this->get_I_S($fI1)));
		$fA1 = $lA2;
		$fI2 = $lI4;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1)->get_I_S($fI2)))) goto label_8;
		$fI0 = 1;
		goto label_9;
		label_8:
		$fI0 = 0;
		label_9:
		$lI5 = $fI0;
		goto label_5;
		label_6:
		return (($lI5)!=0);
	}
	public function get_I_S(int $p0) {
		throw new Exception("Missing body java.nio.ShortBuffer.get(I)S");
	}
	public function hashCode__I() {
		$fI0 = 0;
		$fI2 = 0;
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = $this->_position;
		$lI2 = 0;
		label_1:
		if ((($lI1 >= $this->_limit))) goto label_2;
		$fI0 = $lI2;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$lI2 = ((int)(($fI0 + $this->get_I_S($fI2))));
		goto label_1;
		label_2:
		return $lI2;
	}
	public function __construct($CLASS_ID = 862) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsShortBuffer extends java_nio_ShortBuffer implements java_nio_internal_ByteBufferAs {

	public $_bytes = null;
	public $_byteBuffer = null;
	public function get_I_S(int $p0) {
		return libcore_io_Memory::peekAlignedShortLE__BI_S($this->_bytes, $p0);
	}
	public static function asShortBuffer_Ljava_nio_ByteBuffer__Ljava_nio_ShortBuffer_(?java_nio_ByteBuffer $p0) {
		$lA1 = null;
		$lA1 = $p0->slice__Ljava_nio_ByteBuffer_();
		$lA1->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_($p0->order__Ljava_nio_ByteOrder_());
		return (java_nio_ByteBufferAsShortBuffer::create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsShortBuffer_($lA1, $p0->_isLittleEndian));
	}
	public static function create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsShortBuffer_(?java_nio_ByteBuffer $p0, bool $p1) {
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		if (!($p1)) goto label_1;
		$tA0 = ((new java_nio_ByteBufferAsShortBuffer_LE()));
		$fA0 = $tA0;
		($tA0)->java_nio_ByteBufferAsShortBuffer_LE_init__Ljava_nio_ByteBuffer__V($p0);
		goto label_2;
		label_1:
		$tA1 = ((new java_nio_ByteBufferAsShortBuffer_BE()));
		$fA0 = $tA1;
		($tA1)->java_nio_ByteBufferAsShortBuffer_BE_init__Ljava_nio_ByteBuffer__V($p0);
		label_2:
		return ($fA0);
	}
	public function java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsShortBuffer_1__V(?java_nio_ByteBuffer $p0, ?java_nio_ByteBufferAsShortBuffer_1 $p1) {
		$this->java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer__V($p0);
		return $this;
		return $this;
	}
	public function java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ShortBuffer_init__I_V(((int)(N::idiv($p0->capacity__I(), 2))));
		$this->_byteBuffer = $p0;
		$this->_byteBuffer->clear__Ljava_nio_Buffer_();
		$this->_bytes = $p0->array___B();
		$this->init__B_V($p0->array___B());
		return $this;
		return $this;
	}
	public function init__B_V(?JA_B $p0) {
		return;
	}
	public function __construct($CLASS_ID = 882) {
		parent::__construct($CLASS_ID);
		$this->_bytes = null;
		$this->_byteBuffer = null;
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsShortBuffer_BE extends java_nio_ByteBufferAsShortBuffer {

	public function java_nio_ByteBufferAsShortBuffer_BE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsShortBuffer_1__V($p0, null);
		return $this;
		return $this;
	}
	public function get_I_S(int $p0) {
		return java_lang_Short::reverseBytes_S_S(parent::get_I_S($p0));
	}
	public function __construct($CLASS_ID = 885) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsShortBuffer_1 extends java_lang_Object {

	public function __construct($CLASS_ID = 884) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsShortBuffer_LE extends java_nio_ByteBufferAsShortBuffer {

	public function java_nio_ByteBufferAsShortBuffer_LE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsShortBuffer_1__V($p0, null);
		return $this;
		return $this;
	}
	public function get_I_S(int $p0) {
		return parent::get_I_S($p0);
	}
	public function __construct($CLASS_ID = 883) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_lang_Readable {

}
class java_lang_Readable_IFields {

	static public function SI() {
	}
}
interface java_lang_Appendable {

}
class java_lang_Appendable_IFields {

	static public function SI() {
	}
}
interface java_lang_CharSequence {

	public function toString__Ljava_lang_String_();
	public function length__I();
	public function charAt_I_C(int $p0);
}
class java_lang_CharSequence_IFields {

	static public function SI() {
	}
}
abstract class java_nio_CharBuffer extends java_nio_Buffer implements java_lang_Comparable, java_lang_CharSequence, java_lang_Appendable, java_lang_Readable {

	public function java_nio_CharBuffer_init__I_V(int $p0) {
		($this)->java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(1, $p0, null);
		return $this;
		return $this;
	}
	public function toString__Ljava_lang_String_() {
		$lA1 = null;
		$lI2 = 0;
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init__I_V(((int)(($this->_limit - $this->_position))));
		$lA1 = $fA0;
		$lI2 = $this->_position;
		label_1:
		if ((($lI2 >= $this->_limit))) goto label_2;
		($lA1)->append_C_Ljava_lang_StringBuilder_($this->get_I_C($lI2));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return ($lA1)->toString__Ljava_lang_String_();
	}
	public function get_I_C(int $p0) {
		throw new Exception("Missing body java.nio.CharBuffer.get(I)C");
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA2 = null;
		$fI1 = 0;
		$fI0 = 0;
		$fI2 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$fA1 = null;
		if ((($p0) instanceof java_nio_CharBuffer)) goto label_1;
		return false;
		label_1:
		$lA2 = (N::checkcast($p0, "java_nio_CharBuffer"));
		if ((($this->remaining__I() == ($lA2)->remaining__I()))) goto label_3;
		return false;
		label_3:
		$lI3 = $this->_position;
		$lI4 = ($lA2)->_position;
		$lI5 = 1;
		label_5:
		if ((($lI5 == 0))) goto label_6;
		if ((($lI3 >= $this->_limit))) goto label_6;
		$fI1 = $lI3;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = ((int)($this->get_I_C($fI1)));
		$fA1 = $lA2;
		$fI2 = $lI4;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1)->get_I_C($fI2)))) goto label_8;
		$fI0 = 1;
		goto label_9;
		label_8:
		$fI0 = 0;
		label_9:
		$lI5 = $fI0;
		goto label_5;
		label_6:
		return (($lI5)!=0);
	}
	public function hashCode__I() {
		$fI0 = 0;
		$fI2 = 0;
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = $this->_position;
		$lI2 = 0;
		label_1:
		if ((($lI1 >= $this->_limit))) goto label_2;
		$fI0 = $lI2;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$lI2 = ((int)(($fI0 + $this->get_I_C($fI2))));
		goto label_1;
		label_2:
		return $lI2;
	}
	public function length__I() {
		return $this->remaining__I();
	}
	public function charAt_I_C(int $p0) {
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		if ((($p0 < 0))) goto label_1;
		if ((($p0 < $this->remaining__I()))) goto label_2;
		label_1:
		$tA0 = ((new java_lang_IndexOutOfBoundsException()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_37)->append_I_Ljava_lang_StringBuilder_($p0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_38)->append_I_Ljava_lang_StringBuilder_($this->remaining__I())->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_2:
		return $this->get_I_C(((int)(($this->_position + $p0))));
	}
	public function put_IC_Ljava_nio_CharBuffer_(int $p0, int $p1) {
		throw new Exception("Missing body java.nio.CharBuffer.put(IC)Ljava/nio/CharBuffer;");
	}
	public function __construct($CLASS_ID = 859) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_nio_ByteBufferAsCharBuffer extends java_nio_CharBuffer implements java_nio_internal_ByteBufferAs {

	public $_bytes = null;
	public $_byteBuffer = null;
	public function get_I_C(int $p0) {
		return libcore_io_Memory::peekAlignedCharLE__BI_C($this->_bytes, $p0);
	}
	public static function asCharBuffer_Ljava_nio_ByteBuffer__Ljava_nio_CharBuffer_(?java_nio_ByteBuffer $p0) {
		$lA1 = null;
		$lA1 = $p0->slice__Ljava_nio_ByteBuffer_();
		$lA1->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_($p0->order__Ljava_nio_ByteOrder_());
		return (java_nio_ByteBufferAsCharBuffer::create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsCharBuffer_($lA1, $p0->_isLittleEndian));
	}
	public static function create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsCharBuffer_(?java_nio_ByteBuffer $p0, bool $p1) {
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		if (!($p1)) goto label_1;
		$tA0 = ((new java_nio_ByteBufferAsCharBuffer_LE()));
		$fA0 = $tA0;
		($tA0)->java_nio_ByteBufferAsCharBuffer_LE_init__Ljava_nio_ByteBuffer__V($p0);
		goto label_2;
		label_1:
		$tA1 = ((new java_nio_ByteBufferAsCharBuffer_BE()));
		$fA0 = $tA1;
		($tA1)->java_nio_ByteBufferAsCharBuffer_BE_init__Ljava_nio_ByteBuffer__V($p0);
		label_2:
		return ($fA0);
	}
	public function java_nio_ByteBufferAsCharBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsCharBuffer_1__V(?java_nio_ByteBuffer $p0, ?java_nio_ByteBufferAsCharBuffer_1 $p1) {
		$this->java_nio_ByteBufferAsCharBuffer_init__Ljava_nio_ByteBuffer__V($p0);
		return $this;
		return $this;
	}
	public function java_nio_ByteBufferAsCharBuffer_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_CharBuffer_init__I_V(((int)(N::idiv($p0->capacity__I(), 2))));
		$this->_byteBuffer = $p0;
		$this->_byteBuffer->clear__Ljava_nio_Buffer_();
		$this->_bytes = $p0->array___B();
		$this->init__B_V($p0->array___B());
		return $this;
		return $this;
	}
	public function init__B_V(?JA_B $p0) {
		return;
	}
	public function put_IC_Ljava_nio_CharBuffer_(int $p0, int $p1) {
		libcore_io_Memory::pokeAlignedCharLE__BIC_V($this->_bytes, $p0, $p1);
		return ($this);
	}
	public function __construct($CLASS_ID = 878) {
		parent::__construct($CLASS_ID);
		$this->_bytes = null;
		$this->_byteBuffer = null;
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsCharBuffer_BE extends java_nio_ByteBufferAsCharBuffer {

	public function java_nio_ByteBufferAsCharBuffer_BE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsCharBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsCharBuffer_1__V($p0, null);
		return $this;
		return $this;
	}
	public function get_I_C(int $p0) {
		return java_lang_Character::reverseBytes_C_C(parent::get_I_C($p0));
	}
	public function put_IC_Ljava_nio_CharBuffer_(int $p0, int $p1) {
		return parent::put_IC_Ljava_nio_CharBuffer_($p0, java_lang_Character::reverseBytes_C_C($p1));
	}
	public function __construct($CLASS_ID = 881) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsCharBuffer_1 extends java_lang_Object {

	public function __construct($CLASS_ID = 880) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsCharBuffer_LE extends java_nio_ByteBufferAsCharBuffer {

	public function java_nio_ByteBufferAsCharBuffer_LE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsCharBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsCharBuffer_1__V($p0, null);
		return $this;
		return $this;
	}
	public function get_I_C(int $p0) {
		return parent::get_I_C($p0);
	}
	public function put_IC_Ljava_nio_CharBuffer_(int $p0, int $p1) {
		return parent::put_IC_Ljava_nio_CharBuffer_($p0, $p1);
	}
	public function __construct($CLASS_ID = 879) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_nio_DoubleBuffer extends java_nio_Buffer implements java_lang_Comparable {

	public function java_nio_DoubleBuffer_init__I_V(int $p0) {
		($this)->java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(3, $p0, null);
		return $this;
		return $this;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA2 = null;
		$fI1 = 0;
		$fI0 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$fA0 = null;
		$lD6 = 0.0;
		$lD8 = 0.0;
		if ((($p0) instanceof java_nio_DoubleBuffer)) goto label_1;
		return false;
		label_1:
		$lA2 = (N::checkcast($p0, "java_nio_DoubleBuffer"));
		if ((($this->remaining__I() == ($lA2)->remaining__I()))) goto label_3;
		return false;
		label_3:
		$lI3 = $this->_position;
		$lI4 = ($lA2)->_position;
		$lI5 = 1;
		label_5:
		if ((($lI5 == 0))) goto label_6;
		if ((($lI3 >= $this->_limit))) goto label_6;
		$fA0 = ($this);
		$fI1 = $lI3;
		$lI3 = ((int)(($lI3 + 1)));
		$lD6 = ($fA0)->get_I_D($fI1);
		$fA0 = $lA2;
		$fI1 = $lI4;
		$lI4 = ((int)(($lI4 + 1)));
		$lD8 = ($fA0)->get_I_D($fI1);
		if ((((N::cmpl($lD6, $lD8)) == 0))) goto label_8;
		if ((((N::cmpl($lD6, $lD6)) == 0))) goto label_9;
		if ((((N::cmpl($lD8, $lD8)) == 0))) goto label_9;
		label_8:
		$fI0 = 1;
		goto label_10;
		label_9:
		$fI0 = 0;
		label_10:
		$lI5 = $fI0;
		goto label_5;
		label_6:
		return (($lI5)!=0);
	}
	public function get_I_D(int $p0) {
		throw new Exception("Missing body java.nio.DoubleBuffer.get(I)D");
	}
	public function hashCode__I() {
		$fI1 = 0;
		$lI1 = 0;
		$lI2 = 0;
		$lJ3 = Int64::make(0, 0);
		$lI1 = $this->_position;
		$lI2 = 0;
		label_1:
		if ((($lI1 >= $this->_limit))) goto label_2;
		$fI1 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$lJ3 = java_lang_Double::doubleToLongBits_D_J($this->get_I_D($fI1));
		$lI2 = ((int)((((int)(($lI2 + N::j2i($lJ3)))) ^ N::j2i((N::lshr($lJ3, 32))))));
		goto label_1;
		label_2:
		return $lI2;
	}
	public function put_ID_Ljava_nio_DoubleBuffer_(int $p0, float $p1) {
		throw new Exception("Missing body java.nio.DoubleBuffer.put(ID)Ljava/nio/DoubleBuffer;");
	}
	public function __construct($CLASS_ID = 861) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_nio_ByteBufferAsDoubleBuffer extends java_nio_DoubleBuffer implements java_nio_internal_ByteBufferAs {

	public $_bytes = null;
	public $_byteBuffer = null;
	public function get_I_D(int $p0) {
		return libcore_io_Memory::peekAlignedDoubleLE__BI_D($this->_bytes, $p0);
	}
	public static function asDoubleBuffer_Ljava_nio_ByteBuffer__Ljava_nio_DoubleBuffer_(?java_nio_ByteBuffer $p0) {
		$lA1 = null;
		$lA1 = $p0->slice__Ljava_nio_ByteBuffer_();
		$lA1->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_($p0->order__Ljava_nio_ByteOrder_());
		return (java_nio_ByteBufferAsDoubleBuffer::create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsDoubleBuffer_($lA1, $p0->_isLittleEndian));
	}
	public static function create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsDoubleBuffer_(?java_nio_ByteBuffer $p0, bool $p1) {
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		if (!($p1)) goto label_1;
		$tA0 = ((new java_nio_ByteBufferAsDoubleBuffer_LE()));
		$fA0 = $tA0;
		($tA0)->java_nio_ByteBufferAsDoubleBuffer_LE_init__Ljava_nio_ByteBuffer__V($p0);
		goto label_2;
		label_1:
		$tA1 = ((new java_nio_ByteBufferAsDoubleBuffer_BE()));
		$fA0 = $tA1;
		($tA1)->java_nio_ByteBufferAsDoubleBuffer_BE_init__Ljava_nio_ByteBuffer__V($p0);
		label_2:
		return ($fA0);
	}
	public function java_nio_ByteBufferAsDoubleBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsDoubleBuffer_1__V(?java_nio_ByteBuffer $p0, ?java_nio_ByteBufferAsDoubleBuffer_1 $p1) {
		$this->java_nio_ByteBufferAsDoubleBuffer_init__Ljava_nio_ByteBuffer__V($p0);
		return $this;
		return $this;
	}
	public function java_nio_ByteBufferAsDoubleBuffer_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_DoubleBuffer_init__I_V(((int)(N::idiv($p0->capacity__I(), 8))));
		$this->_byteBuffer = $p0;
		$this->_byteBuffer->clear__Ljava_nio_Buffer_();
		$this->_bytes = $p0->array___B();
		$this->init__B_V($p0->array___B());
		return $this;
		return $this;
	}
	public function init__B_V(?JA_B $p0) {
		return;
	}
	public function getLong_I_J(int $p0) {
		return libcore_io_Memory::peekAlignedLongLE__BI_J($this->_bytes, $p0);
	}
	public function put_ID_Ljava_nio_DoubleBuffer_(int $p0, float $p1) {
		libcore_io_Memory::pokeAlignedDoubleLE__BID_V($this->_bytes, $p0, $p1);
		return ($this);
	}
	public function putLong_IJ_Ljava_nio_DoubleBuffer_(int $p0, Int64 $p1) {
		libcore_io_Memory::pokeAlignedLongLE__BIJ_V($this->_bytes, $p0, $p1);
		return ($this);
	}
	public function __construct($CLASS_ID = 874) {
		parent::__construct($CLASS_ID);
		$this->_bytes = null;
		$this->_byteBuffer = null;
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsDoubleBuffer_BE extends java_nio_ByteBufferAsDoubleBuffer {

	public function java_nio_ByteBufferAsDoubleBuffer_BE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsDoubleBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsDoubleBuffer_1__V($p0, null);
		return $this;
		return $this;
	}
	public function get_I_D(int $p0) {
		return java_lang_Double::longBitsToDouble_J_D(java_lang_Long::reverseBytes_J_J($this->getLong_I_J($p0)));
	}
	public function getLong_I_J(int $p0) {
		return parent::getLong_I_J($p0);
	}
	public function put_ID_Ljava_nio_DoubleBuffer_(int $p0, float $p1) {
		return $this->putLong_IJ_Ljava_nio_DoubleBuffer_($p0, com_jtransc_JTranscBits::reverseBytes_J_J(java_lang_Double::doubleToRawLongBits_D_J($p1)));
	}
	public function putLong_IJ_Ljava_nio_DoubleBuffer_(int $p0, Int64 $p1) {
		return parent::putLong_IJ_Ljava_nio_DoubleBuffer_($p0, $p1);
	}
	public function __construct($CLASS_ID = 877) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsDoubleBuffer_1 extends java_lang_Object {

	public function __construct($CLASS_ID = 876) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsDoubleBuffer_LE extends java_nio_ByteBufferAsDoubleBuffer {

	public function java_nio_ByteBufferAsDoubleBuffer_LE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsDoubleBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsDoubleBuffer_1__V($p0, null);
		return $this;
		return $this;
	}
	public function get_I_D(int $p0) {
		return parent::get_I_D($p0);
	}
	public function getLong_I_J(int $p0) {
		return parent::getLong_I_J($p0);
	}
	public function put_ID_Ljava_nio_DoubleBuffer_(int $p0, float $p1) {
		return parent::put_ID_Ljava_nio_DoubleBuffer_($p0, $p1);
	}
	public function putLong_IJ_Ljava_nio_DoubleBuffer_(int $p0, Int64 $p1) {
		return parent::putLong_IJ_Ljava_nio_DoubleBuffer_($p0, $p1);
	}
	public function __construct($CLASS_ID = 875) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_nio_LongBuffer extends java_nio_Buffer implements java_lang_Comparable {

	public function java_nio_LongBuffer_init__I_V(int $p0) {
		($this)->java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(3, $p0, null);
		return $this;
		return $this;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fJ0 = Int64::make(0, 0);
		$lA2 = null;
		$fI1 = 0;
		$fI2 = 0;
		$fI0 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$fA1 = null;
		if ((($p0) instanceof java_nio_LongBuffer)) goto label_1;
		return false;
		label_1:
		$lA2 = (N::checkcast($p0, "java_nio_LongBuffer"));
		if ((($this->remaining__I() == ($lA2)->remaining__I()))) goto label_3;
		return false;
		label_3:
		$lI3 = $this->_position;
		$lI4 = ($lA2)->_position;
		$lI5 = 1;
		label_5:
		if ((($lI5 == 0))) goto label_6;
		if ((($lI3 >= $this->_limit))) goto label_6;
		$fI1 = $lI3;
		$lI3 = ((int)(($lI3 + 1)));
		$fJ0 = $this->get_I_J($fI1);
		$fA1 = $lA2;
		$fI2 = $lI4;
		$lI4 = ((int)(($lI4 + 1)));
		if (((((int)(N::lcmp($fJ0, ($fA1)->get_I_J($fI2)))) != 0))) goto label_8;
		$fI0 = 1;
		goto label_9;
		label_8:
		$fI0 = 0;
		label_9:
		$lI5 = $fI0;
		goto label_5;
		label_6:
		return (($lI5)!=0);
	}
	public function get_I_J(int $p0) {
		throw new Exception("Missing body java.nio.LongBuffer.get(I)J");
	}
	public function hashCode__I() {
		$fI1 = 0;
		$lI1 = 0;
		$lI2 = 0;
		$lJ3 = Int64::make(0, 0);
		$lI1 = $this->_position;
		$lI2 = 0;
		label_1:
		if ((($lI1 >= $this->_limit))) goto label_2;
		$fI1 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$lJ3 = $this->get_I_J($fI1);
		$lI2 = ((int)((((int)(($lI2 + N::j2i($lJ3)))) ^ N::j2i((N::lshr($lJ3, 32))))));
		goto label_1;
		label_2:
		return $lI2;
	}
	public function __construct($CLASS_ID = 863) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_nio_ByteBufferAsLongBuffer extends java_nio_LongBuffer implements java_nio_internal_ByteBufferAs {

	public $_bytes = null;
	public $_byteBuffer = null;
	public function get_I_J(int $p0) {
		return libcore_io_Memory::peekAlignedLongLE__BI_J($this->_bytes, $p0);
	}
	public static function asLongBuffer_Ljava_nio_ByteBuffer__Ljava_nio_LongBuffer_(?java_nio_ByteBuffer $p0) {
		$lA1 = null;
		$lA1 = $p0->slice__Ljava_nio_ByteBuffer_();
		$lA1->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_($p0->order__Ljava_nio_ByteOrder_());
		return (java_nio_ByteBufferAsLongBuffer::create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsLongBuffer_($lA1, $p0->_isLittleEndian));
	}
	public static function create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsLongBuffer_(?java_nio_ByteBuffer $p0, bool $p1) {
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		if (!($p1)) goto label_1;
		$tA0 = ((new java_nio_ByteBufferAsLongBuffer_LE()));
		$fA0 = $tA0;
		($tA0)->java_nio_ByteBufferAsLongBuffer_LE_init__Ljava_nio_ByteBuffer__V($p0);
		goto label_2;
		label_1:
		$tA1 = ((new java_nio_ByteBufferAsLongBuffer_BE()));
		$fA0 = $tA1;
		($tA1)->java_nio_ByteBufferAsLongBuffer_BE_init__Ljava_nio_ByteBuffer__V($p0);
		label_2:
		return ($fA0);
	}
	public function java_nio_ByteBufferAsLongBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsLongBuffer_1__V(?java_nio_ByteBuffer $p0, ?java_nio_ByteBufferAsLongBuffer_1 $p1) {
		$this->java_nio_ByteBufferAsLongBuffer_init__Ljava_nio_ByteBuffer__V($p0);
		return $this;
		return $this;
	}
	public function java_nio_ByteBufferAsLongBuffer_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_LongBuffer_init__I_V(((int)(N::idiv($p0->capacity__I(), 8))));
		$this->_byteBuffer = $p0;
		$this->_byteBuffer->clear__Ljava_nio_Buffer_();
		$this->_bytes = $p0->array___B();
		$this->init__B_V($p0->array___B());
		return $this;
		return $this;
	}
	public function init__B_V(?JA_B $p0) {
		return;
	}
	public function __construct($CLASS_ID = 870) {
		parent::__construct($CLASS_ID);
		$this->_bytes = null;
		$this->_byteBuffer = null;
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsLongBuffer_BE extends java_nio_ByteBufferAsLongBuffer {

	public function java_nio_ByteBufferAsLongBuffer_BE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsLongBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsLongBuffer_1__V($p0, null);
		return $this;
		return $this;
	}
	public function get_I_J(int $p0) {
		return com_jtransc_JTranscBits::reverseBytes_J_J(parent::get_I_J($p0));
	}
	public function __construct($CLASS_ID = 873) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsLongBuffer_1 extends java_lang_Object {

	public function __construct($CLASS_ID = 872) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsLongBuffer_LE extends java_nio_ByteBufferAsLongBuffer {

	public function java_nio_ByteBufferAsLongBuffer_LE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsLongBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsLongBuffer_1__V($p0, null);
		return $this;
		return $this;
	}
	public function get_I_J(int $p0) {
		return parent::get_I_J($p0);
	}
	public function __construct($CLASS_ID = 871) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_nio_FloatBuffer extends java_nio_Buffer implements java_lang_Comparable {

	public function java_nio_FloatBuffer_init__I_V(int $p0) {
		($this)->java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(2, $p0, null);
		return $this;
		return $this;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lF6 = 0.0;
		$lF7 = 0.0;
		$lA2 = null;
		$fI1 = 0;
		$fI0 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$fA0 = null;
		if ((($p0) instanceof java_nio_FloatBuffer)) goto label_1;
		return false;
		label_1:
		$lA2 = (N::checkcast($p0, "java_nio_FloatBuffer"));
		if ((($this->remaining__I() == ($lA2)->remaining__I()))) goto label_3;
		return false;
		label_3:
		$lI3 = $this->_position;
		$lI4 = ($lA2)->_position;
		$lI5 = 1;
		label_5:
		if ((($lI5 == 0))) goto label_6;
		if ((($lI3 >= $this->_limit))) goto label_6;
		$fA0 = ($this);
		$fI1 = $lI3;
		$lI3 = ((int)(($lI3 + 1)));
		$lF6 = ($fA0)->get_I_F($fI1);
		$fA0 = $lA2;
		$fI1 = $lI4;
		$lI4 = ((int)(($lI4 + 1)));
		$lF7 = ($fA0)->get_I_F($fI1);
		if ((((N::cmpl($lF6, $lF7)) == 0))) goto label_8;
		if ((((N::cmpl($lF6, $lF6)) == 0))) goto label_9;
		if ((((N::cmpl($lF7, $lF7)) == 0))) goto label_9;
		label_8:
		$fI0 = 1;
		goto label_10;
		label_9:
		$fI0 = 0;
		label_10:
		$lI5 = $fI0;
		goto label_5;
		label_6:
		return (($lI5)!=0);
	}
	public function get_I_F(int $p0) {
		throw new Exception("Missing body java.nio.FloatBuffer.get(I)F");
	}
	public function hashCode__I() {
		$fI0 = 0;
		$fI2 = 0;
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = $this->_position;
		$lI2 = 0;
		label_1:
		if ((($lI1 >= $this->_limit))) goto label_2;
		$fI0 = $lI2;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$lI2 = ((int)(($fI0 + java_lang_Float::floatToIntBits_F_I($this->get_I_F($fI2)))));
		goto label_1;
		label_2:
		return $lI2;
	}
	public function put_IF_Ljava_nio_FloatBuffer_(int $p0, float $p1) {
		throw new Exception("Missing body java.nio.FloatBuffer.put(IF)Ljava/nio/FloatBuffer;");
	}
	public function __construct($CLASS_ID = 856) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsFloatBuffer extends java_nio_FloatBuffer implements java_nio_internal_ByteBufferAs {

	public $_byteBuffer = null;
	public $_bytes = null;
	public function java_nio_ByteBufferAsFloatBuffer_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_FloatBuffer_init__I_V(((int)(N::idiv($p0->capacity__I(), 4))));
		$this->_byteBuffer = $p0;
		$this->_byteBuffer->clear__Ljava_nio_Buffer_();
		$this->_bytes = $p0->array___B();
		$this->init__B_V($p0->array___B());
		return $this;
		return $this;
	}
	public function init__B_V(?JA_B $p0) {
		return;
	}
	public function get_I_F(int $p0) {
		return libcore_io_Memory::peekAlignedFloatLE__BI_F($this->_bytes, $p0);
	}
	public function put_IF_Ljava_nio_FloatBuffer_(int $p0, float $p1) {
		libcore_io_Memory::pokeAlignedFloatLE__BIF_V($this->_bytes, $p0, $p1);
		return ($this);
	}
	public static function asFloatBuffer_Ljava_nio_ByteBuffer__Ljava_nio_FloatBuffer_(?java_nio_ByteBuffer $p0) {
		$lA1 = null;
		$lA1 = $p0->slice__Ljava_nio_ByteBuffer_();
		$lA1->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_($p0->order__Ljava_nio_ByteOrder_());
		return (java_nio_ByteBufferAsFloatBuffer::create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsFloatBuffer_($lA1, $p0->_isLittleEndian));
	}
	public static function create_Ljava_nio_ByteBuffer_Z_Ljava_nio_ByteBufferAsFloatBuffer_(?java_nio_ByteBuffer $p0, bool $p1) {
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		if (!($p1)) goto label_1;
		$tA0 = ((new java_nio_ByteBufferAsFloatBuffer_LE()));
		$fA0 = $tA0;
		($tA0)->java_nio_ByteBufferAsFloatBuffer_LE_init__Ljava_nio_ByteBuffer__V($p0);
		goto label_2;
		label_1:
		$tA1 = ((new java_nio_ByteBufferAsFloatBuffer_BE()));
		$fA0 = $tA1;
		($tA1)->java_nio_ByteBufferAsFloatBuffer_BE_init__Ljava_nio_ByteBuffer__V($p0);
		label_2:
		return ($fA0);
	}
	public function getInt_I_I(int $p0) {
		return libcore_io_Memory::peekAlignedIntLE__BI_I($this->_bytes, $p0);
	}
	public function putInt_II_Ljava_nio_FloatBuffer_(int $p0, int $p1) {
		libcore_io_Memory::pokeAlignedIntLE__BII_V($this->_bytes, $p0, $p1);
		return ($this);
	}
	public function __construct($CLASS_ID = 864) {
		parent::__construct($CLASS_ID);
		$this->_byteBuffer = null;
		$this->_bytes = null;
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsFloatBuffer_LE extends java_nio_ByteBufferAsFloatBuffer {

	public function java_nio_ByteBufferAsFloatBuffer_LE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsFloatBuffer_init__Ljava_nio_ByteBuffer__V($p0);
		return $this;
		return $this;
	}
	public function get_I_F(int $p0) {
		return parent::get_I_F($p0);
	}
	public function put_IF_Ljava_nio_FloatBuffer_(int $p0, float $p1) {
		return parent::put_IF_Ljava_nio_FloatBuffer_($p0, $p1);
	}
	public function getInt_I_I(int $p0) {
		return parent::getInt_I_I($p0);
	}
	public function putInt_II_Ljava_nio_FloatBuffer_(int $p0, int $p1) {
		return parent::putInt_II_Ljava_nio_FloatBuffer_($p0, $p1);
	}
	public function __construct($CLASS_ID = 869) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBufferAsFloatBuffer_BE extends java_nio_ByteBufferAsFloatBuffer {

	public function java_nio_ByteBufferAsFloatBuffer_BE_init__Ljava_nio_ByteBuffer__V(?java_nio_ByteBuffer $p0) {
		($this)->java_nio_ByteBufferAsFloatBuffer_init__Ljava_nio_ByteBuffer__V($p0);
		return $this;
		return $this;
	}
	public function get_I_F(int $p0) {
		return java_lang_Float::intBitsToFloat_I_F(java_lang_Integer::reverseBytes_I_I($this->getInt_I_I($p0)));
	}
	public function getInt_I_I(int $p0) {
		return parent::getInt_I_I($p0);
	}
	public function put_IF_Ljava_nio_FloatBuffer_(int $p0, float $p1) {
		return $this->putInt_II_Ljava_nio_FloatBuffer_($p0, java_lang_Integer::reverseBytes_I_I(java_lang_Float::floatToRawIntBits_F_I($p1)));
	}
	public function putInt_II_Ljava_nio_FloatBuffer_(int $p0, int $p1) {
		return parent::putInt_II_Ljava_nio_FloatBuffer_($p0, $p1);
	}
	public function __construct($CLASS_ID = 868) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteOrder extends java_lang_Object {

	public $_name = null;
	public $_needsSwap = false;
	public static $_LITTLE_ENDIAN = null;
	public static $_NATIVE_ORDER = null;
	public static $_BIG_ENDIAN = null;
	public static $_isLittleEndian = false;
	public function java_nio_ByteOrder_init__Ljava_lang_String_Z_V(?java_lang_String $p0, bool $p1) {
		($this)->java_lang_Object_init___V();
		$this->_name = $p0;
		$this->_needsSwap = $p1;
		return $this;
		return $this;
	}
	public static function java_nio_ByteOrder_clinit___V() {
		$fI3 = 0;
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		java_nio_ByteOrder::$_isLittleEndian = com_jtransc_JTranscBits::isLittleEndian__Z();
		$tA0 = ((new java_nio_ByteOrder()));
		$fA0 = $tA0;
		($tA0)->java_nio_ByteOrder_init__Ljava_lang_String_Z_V(Bootstrap::$STRINGLIT_39, java_nio_ByteOrder::$_isLittleEndian);
		java_nio_ByteOrder::$_BIG_ENDIAN = ($fA0);
		$tA1 = ((new java_nio_ByteOrder()));
		$fA0 = $tA1;
		$fA1 = $tA1;
		$fA2 = Bootstrap::$STRINGLIT_40;
		if (java_nio_ByteOrder::$_isLittleEndian) goto label_1;
		$fI3 = 1;
		goto label_2;
		label_1:
		$fI3 = 0;
		label_2:
		($fA1)->java_nio_ByteOrder_init__Ljava_lang_String_Z_V($fA2, (($fI3)!=0));
		java_nio_ByteOrder::$_LITTLE_ENDIAN = ($fA0);
		if (!(java_nio_ByteOrder::$_isLittleEndian)) goto label_3;
		$fA0 = (java_nio_ByteOrder::$_LITTLE_ENDIAN);
		goto label_4;
		label_3:
		$fA0 = (java_nio_ByteOrder::$_BIG_ENDIAN);
		label_4:
		java_nio_ByteOrder::$_NATIVE_ORDER = ($fA0);
		return;
	}
	public function toString__Ljava_lang_String_() {
		return $this->_name;
	}
	public static function nativeOrder__Ljava_nio_ByteOrder_() {
		return java_nio_ByteOrder::$_NATIVE_ORDER;
	}
	public function __construct($CLASS_ID = 867) {
		parent::__construct($CLASS_ID);
		$this->_name = null;
		$this->_needsSwap = false;
	}
	static public function SI() {
		java_nio_ByteOrder::$_LITTLE_ENDIAN = null;
		java_nio_ByteOrder::$_NATIVE_ORDER = null;
		java_nio_ByteOrder::$_BIG_ENDIAN = null;
		java_nio_ByteOrder::$_isLittleEndian = false;
		java_nio_ByteOrder::java_nio_ByteOrder_clinit___V();
	}
}
class libcore_io_Memory extends java_lang_Object {

	public static $_SWAPPED = null;
	public static $_NATIVE = null;
	public function libcore_io_Memory_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function libcore_io_Memory_clinit___V() {
		$fA0 = null;
		libcore_io_Memory::$_NATIVE = java_nio_ByteOrder::nativeOrder__Ljava_nio_ByteOrder_();
		if (((libcore_io_Memory::$_NATIVE != java_nio_ByteOrder::$_LITTLE_ENDIAN))) goto label_1;
		$fA0 = java_nio_ByteOrder::$_BIG_ENDIAN;
		goto label_2;
		label_1:
		$fA0 = java_nio_ByteOrder::$_LITTLE_ENDIAN;
		label_2:
		libcore_io_Memory::$_SWAPPED = $fA0;
		return;
	}
	public static function peekAlignedFloatLE__BI_F(?JA_B $p0, int $p1) {
		return libcore_io_Memory::peekFloatLE__BI_F($p0, ((int)(N::imul($p1, 4))));
	}
	public static function peekFloatLE__BI_F(?JA_B $p0, int $p1) {
		return java_lang_Float::intBitsToFloat_I_F(libcore_io_Memory::peekIntLE__BI_I($p0, $p1));
	}
	public static function peekIntLE__BI_I(?JA_B $p0, int $p1) {
		$fA1 = null;
		$fI1 = 0;
		$fI0 = 0;
		$fI2 = 0;
		$lI1 = 0;
		$lI1 = $p1;
		$fI1 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$fI0 = ((int)(N::ishl(((int)((($p0->get($fI1)) & 255))), 0)));
		$fA1 = $p0;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$fI0 = ((int)(($fI0 | ((int)(N::ishl(((int)((($fA1->get($fI2)) & 255))), 8))))));
		$fA1 = $p0;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		return ((int)((((int)(($fI0 | ((int)(N::ishl(((int)((($fA1->get($fI2)) & 255))), 16)))))) | ((int)(N::ishl(((int)((($p0->get($lI1)) & 255))), 24))))));
	}
	public static function pokeAlignedFloatLE__BIF_V(?JA_B $p0, int $p1, float $p2) {
		com_jtransc_JTranscBits::writeFloatLE__BIF_V($p0, ((int)(N::imul($p1, 4))), $p2);
		return;
	}
	public static function peekAlignedIntLE__BI_I(?JA_B $p0, int $p1) {
		return libcore_io_Memory::peekIntLE__BI_I($p0, ((int)(N::imul($p1, 4))));
	}
	public static function pokeAlignedIntLE__BII_V(?JA_B $p0, int $p1, int $p2) {
		com_jtransc_JTranscBits::writeIntLE__BII_V($p0, ((int)(N::imul($p1, 4))), $p2);
		return;
	}
	public static function peekAlignedLongLE__BI_J(?JA_B $p0, int $p1) {
		return libcore_io_Memory::peekLongLE__BI_J($p0, ((int)(N::imul($p1, 8))));
	}
	public static function peekLongLE__BI_J(?JA_B $p0, int $p1) {
		$fA0 = null;
		$fA1 = null;
		$fI1 = 0;
		$fI0 = 0;
		$fI2 = 0;
		$lI1 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$lI1 = $p1;
		$fA0 = $p0;
		$fI1 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$fI0 = ((int)(N::ishl(((int)((($fA0->get($fI1)) & 255))), 0)));
		$fA1 = $p0;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$fI0 = ((int)(($fI0 | ((int)(N::ishl(((int)((($fA1->get($fI2)) & 255))), 8))))));
		$fA1 = $p0;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$fI0 = ((int)(($fI0 | ((int)(N::ishl(((int)((($fA1->get($fI2)) & 255))), 16))))));
		$fA1 = $p0;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$lI2 = ((int)(($fI0 | ((int)(N::ishl(((int)((($fA1->get($fI2)) & 255))), 24))))));
		$fA0 = $p0;
		$fI1 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$fI0 = ((int)(N::ishl(((int)((($fA0->get($fI1)) & 255))), 0)));
		$fA1 = $p0;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$fI0 = ((int)(($fI0 | ((int)(N::ishl(((int)((($fA1->get($fI2)) & 255))), 8))))));
		$fA1 = $p0;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$lI3 = ((int)((((int)(($fI0 | ((int)(N::ishl(((int)((($fA1->get($fI2)) & 255))), 16)))))) | ((int)(N::ishl(((int)((($p0->get($lI1)) & 255))), 24))))));
		return (N::lor((N::lshl(N::i2j($lI3), 32)), (N::land(N::i2j($lI2), Int64::make(0, -1)))));
	}
	public static function peekAlignedDoubleLE__BI_D(?JA_B $p0, int $p1) {
		return libcore_io_Memory::peekDoubleLE__BI_D($p0, ((int)(N::imul($p1, 8))));
	}
	public static function peekDoubleLE__BI_D(?JA_B $p0, int $p1) {
		return java_lang_Double::longBitsToDouble_J_D(libcore_io_Memory::peekLongLE__BI_J($p0, $p1));
	}
	public static function peekAlignedCharLE__BI_C(?JA_B $p0, int $p1) {
		return libcore_io_Memory::peekCharLE__BI_C($p0, ((int)(N::imul($p1, 2))));
	}
	public static function peekCharLE__BI_C(?JA_B $p0, int $p1) {
		return (N::i2c(((int)((((int)(N::ishl(($p0->get(((int)(($p1 + 1))))), 8))) | ((int)((($p0->get($p1)) & 255))))))));
	}
	public static function peekAlignedShortLE__BI_S(?JA_B $p0, int $p1) {
		return libcore_io_Memory::peekShortLE__BI_S($p0, ((int)(N::imul($p1, 2))));
	}
	public static function peekShortLE__BI_S(?JA_B $p0, int $p1) {
		return (N::i2s(((int)((((int)(N::ishl(($p0->get(((int)(($p1 + 1))))), 8))) | ((int)((($p0->get($p1)) & 255))))))));
	}
	public static function peekAlignedIntBE__BI_I(?JA_B $p0, int $p1) {
		return java_lang_Integer::reverseBytes_I_I(libcore_io_Memory::peekAlignedIntLE__BI_I($p0, $p1));
	}
	public static function pokeAlignedDoubleLE__BID_V(?JA_B $p0, int $p1, float $p2) {
		com_jtransc_JTranscBits::writeDoubleLE__BID_V($p0, ((int)(N::imul($p1, 8))), $p2);
		return;
	}
	public static function pokeAlignedLongLE__BIJ_V(?JA_B $p0, int $p1, Int64 $p2) {
		com_jtransc_JTranscBits::writeLongLE__BIJ_V($p0, ((int)(N::imul($p1, 8))), $p2);
		return;
	}
	public static function pokeAlignedCharLE__BIC_V(?JA_B $p0, int $p1, int $p2) {
		com_jtransc_JTranscBits::writeCharLE__BIC_V($p0, ((int)(N::imul($p1, 2))), $p2);
		return;
	}
	public function __construct($CLASS_ID = 866) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		libcore_io_Memory::$_SWAPPED = null;
		libcore_io_Memory::$_NATIVE = null;
		libcore_io_Memory::libcore_io_Memory_clinit___V();
	}
}
class java_lang_UnsupportedOperationException extends java_lang_RuntimeException {

	public function java_lang_UnsupportedOperationException_init___V() {
		($this)->java_lang_RuntimeException_init___V();
		return $this;
		return $this;
	}
	public function java_lang_UnsupportedOperationException_init__Ljava_lang_String_Ljava_lang_Throwable__V(?java_lang_String $p0, ?java_lang_Throwable $p1) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_String_Ljava_lang_Throwable__V($p0, $p1);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 736) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ReadOnlyBufferException extends java_lang_UnsupportedOperationException {

	public function java_nio_ReadOnlyBufferException_init___V() {
		($this)->java_lang_UnsupportedOperationException_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 858) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_ByteBuffer extends java_nio_Buffer implements java_lang_Comparable {

	public $_arrayOffset = 0;
	public $_backingArray = null;
	public $_isReadOnly = false;
	public $_isLittleEndian = false;
	public $_isNativeOrder = false;
	public $_order = null;
	public $_isDirect = false;
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA2 = null;
		$fI1 = 0;
		$fI0 = 0;
		$fI2 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$fA1 = null;
		if ((($p0) instanceof java_nio_ByteBuffer)) goto label_1;
		return false;
		label_1:
		$lA2 = (N::checkcast($p0, "java_nio_ByteBuffer"));
		if ((($this->remaining__I() == ($lA2)->remaining__I()))) goto label_3;
		return false;
		label_3:
		$lI3 = $this->_position;
		$lI4 = ($lA2)->_position;
		$lI5 = 1;
		label_5:
		if ((($lI5 == 0))) goto label_6;
		if ((($lI3 >= $this->_limit))) goto label_6;
		$fI1 = $lI3;
		$lI3 = ((int)(($lI3 + 1)));
		$fI0 = ((int)($this->get_I_B($fI1)));
		$fA1 = $lA2;
		$fI2 = $lI4;
		$lI4 = ((int)(($lI4 + 1)));
		if ((($fI0 != ($fA1)->get_I_B($fI2)))) goto label_8;
		$fI0 = 1;
		goto label_9;
		label_8:
		$fI0 = 0;
		label_9:
		$lI5 = $fI0;
		goto label_5;
		label_6:
		return (($lI5)!=0);
	}
	public function get_I_B(int $p0) {
		$this->checkIndex_I_V($p0);
		return ($this->_backingArray->get(((int)(($this->_arrayOffset + $p0)))));
	}
	public function hashCode__I() {
		$fI0 = 0;
		$fI2 = 0;
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = $this->_position;
		$lI2 = 0;
		label_1:
		if ((($lI1 >= $this->_limit))) goto label_2;
		$fI0 = $lI2;
		$fI2 = $lI1;
		$lI1 = ((int)(($lI1 + 1)));
		$lI2 = ((int)(($fI0 + $this->get_I_B($fI2))));
		goto label_1;
		label_2:
		return $lI2;
	}
	public function array___B() {
		$this->_checkWritable__V();
		return $this->_backingArray;
	}
	public function _checkWritable__V() {
		$fA0 = null;
		$tA0 = null;
		if (!($this->_isReadOnly)) goto label_1;
		$tA0 = ((new java_nio_ReadOnlyBufferException()));
		$fA0 = $tA0;
		($tA0)->java_nio_ReadOnlyBufferException_init___V();
		throw new WrappedThrowable($fA0);
		label_1:
		return;
	}
	public function asFloatBuffer__Ljava_nio_FloatBuffer_() {
		return java_nio_ByteBufferAsFloatBuffer::asFloatBuffer_Ljava_nio_ByteBuffer__Ljava_nio_FloatBuffer_($this);
	}
	public function slice__Ljava_nio_ByteBuffer_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_nio_ByteBuffer()));
		$fA0 = $tA0;
		($tA0)->java_nio_ByteBuffer_init__I_BIZ_V($this->remaining__I(), $this->_backingArray, ((int)(($this->_arrayOffset + $this->_position))), $this->_isReadOnly);
		return ($fA0);
	}
	public function java_nio_ByteBuffer_init__I_BIZ_V(int $p0, ?JA_B $p1, int $p2, bool $p3) {
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA1 = null;
		$tA2 = null;
		($this)->java_nio_Buffer_init__IILjava_nio_internal_MemoryBlock__V(0, $p0, null);
		$this->_backingArray = $p1;
		$this->_arrayOffset = $p2;
		$this->_isReadOnly = $p3;
		$this->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder::$_BIG_ENDIAN);
		if (((((int)(($p2 + $p0))) <= (($p1))->length))) goto label_1;
		$tA1 = ((new java_lang_IndexOutOfBoundsException()));
		$fA0 = $tA1;
		$fA1 = $tA1;
		$tA2 = ((new java_lang_StringBuilder()));
		$fA2 = $tA2;
		($tA2)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_43)->append_I_Ljava_lang_StringBuilder_((($p1))->length)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_42)->append_I_Ljava_lang_StringBuilder_($p0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_41)->append_I_Ljava_lang_StringBuilder_($p2)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_1:
		return $this;
		return $this;
	}
	public function order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(?java_nio_ByteOrder $p0) {
		$lA1 = null;
		$fI1 = 0;
		$fA0 = null;
		$lA1 = ($p0);
		if ((($lA1 != null))) goto label_1;
		$lA1 = (java_nio_ByteOrder::$_LITTLE_ENDIAN);
		label_1:
		$this->_order = ($lA1);
		$fA0 = $this;
		if ((($lA1 != java_nio_ByteOrder::nativeOrder__Ljava_nio_ByteOrder_()))) goto label_2;
		$fI1 = 1;
		goto label_3;
		label_2:
		$fI1 = 0;
		label_3:
		$fA0->_isNativeOrder = (($fI1)!=0);
		$fA0 = $this;
		if ((($lA1 != java_nio_ByteOrder::$_LITTLE_ENDIAN))) goto label_4;
		$fI1 = 1;
		goto label_5;
		label_4:
		$fI1 = 0;
		label_5:
		$fA0->_isLittleEndian = (($fI1)!=0);
		return $this;
	}
	public function order__Ljava_nio_ByteOrder_() {
		return $this->_order;
	}
	public function asLongBuffer__Ljava_nio_LongBuffer_() {
		return java_nio_ByteBufferAsLongBuffer::asLongBuffer_Ljava_nio_ByteBuffer__Ljava_nio_LongBuffer_($this);
	}
	public function asDoubleBuffer__Ljava_nio_DoubleBuffer_() {
		return java_nio_ByteBufferAsDoubleBuffer::asDoubleBuffer_Ljava_nio_ByteBuffer__Ljava_nio_DoubleBuffer_($this);
	}
	public function asCharBuffer__Ljava_nio_CharBuffer_() {
		return java_nio_ByteBufferAsCharBuffer::asCharBuffer_Ljava_nio_ByteBuffer__Ljava_nio_CharBuffer_($this);
	}
	public function asShortBuffer__Ljava_nio_ShortBuffer_() {
		return java_nio_ByteBufferAsShortBuffer::asShortBuffer_Ljava_nio_ByteBuffer__Ljava_nio_ShortBuffer_($this);
	}
	public function asIntBuffer__Ljava_nio_IntBuffer_() {
		return java_nio_ByteBufferAsIntBuffer::asIntBuffer_Ljava_nio_ByteBuffer__Ljava_nio_IntBuffer_($this);
	}
	public static function allocateDirect_I_Ljava_nio_ByteBuffer_(int $p0) {
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		if ((($p0 >= 0))) goto label_1;
		$tA0 = ((new java_lang_IllegalArgumentException()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_IllegalArgumentException_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_31)->append_I_Ljava_lang_StringBuilder_($p0)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_1:
		$tA2 = ((new java_nio_ByteBuffer()));
		$fA0 = $tA2;
		($tA2)->java_nio_ByteBuffer_init___BZ_V(new JA_B($p0), true);
		return ($fA0);
	}
	public function java_nio_ByteBuffer_init___BZ_V(?JA_B $p0, bool $p1) {
		$this->java_nio_ByteBuffer_init__I_BIZ_V((($p0))->length, $p0, 0, false);
		$this->_isDirect = $p1;
		return $this;
		return $this;
	}
	public static function allocate_I_Ljava_nio_ByteBuffer_(int $p0) {
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		if ((($p0 >= 0))) goto label_1;
		$tA0 = ((new java_lang_IllegalArgumentException()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_IllegalArgumentException_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_31)->append_I_Ljava_lang_StringBuilder_($p0)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_1:
		$tA2 = ((new java_nio_ByteBuffer()));
		$fA0 = $tA2;
		($tA2)->java_nio_ByteBuffer_init___B_V(new JA_B($p0));
		return ($fA0);
	}
	public function java_nio_ByteBuffer_init___B_V(?JA_B $p0) {
		$this->java_nio_ByteBuffer_init__I_BIZ_V((($p0))->length, $p0, 0, false);
		$this->_isDirect = false;
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 857) {
		parent::__construct($CLASS_ID);
		$this->_arrayOffset = 0;
		$this->_backingArray = null;
		$this->_isReadOnly = false;
		$this->_isLittleEndian = false;
		$this->_isNativeOrder = false;
		$this->_order = null;
		$this->_isDirect = false;
	}
	static public function SI() {
	}
}
class java_nio_internal_MemoryBlock extends java_lang_Object {

	public function java_nio_internal_MemoryBlock_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 855) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_FastMemory extends java_lang_Object {

	public $_dataInt = null;
	public $_dataFloat = null;
	public $_data = null;
	public $_dataShort = null;
	public $_dataDouble = null;
	public $_dataChar = null;
	public $_dataLong = null;
	public $_length = 0;
	public function getAlignedInt32_I_I(int $p0) {
		return $this->_dataInt->get_I_I($p0);
	}
	public function setAlignedFloat32_IF_V(int $p0, float $p1) {
		$this->_dataFloat->put_IF_Ljava_nio_FloatBuffer_($p0, $p1);
		return;
	}
	public static function alloc_I_Lcom_jtransc_FastMemory_(int $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new com_jtransc_FastMemory()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_FastMemory_init__I_V($p0);
		return ($fA0);
	}
	public function com_jtransc_FastMemory_init__I_V(int $p0) {
		($this)->java_lang_Object_init___V();
		$this->_initWithSize_I_V($p0);
		$this->_createViews__V();
		if (!(com_jtransc_JTranscSystem::isCpp__Z())) goto label_1;
		$this->_createViewsExtra__B_V($this->_data->array___B());
		label_1:
		return $this;
		return $this;
	}
	public function _createViews__V() {
		$this->_dataChar = $this->_data->asCharBuffer__Ljava_nio_CharBuffer_();
		$this->_dataShort = $this->_data->asShortBuffer__Ljava_nio_ShortBuffer_();
		$this->_dataInt = $this->_data->asIntBuffer__Ljava_nio_IntBuffer_();
		$this->_dataLong = $this->_data->asLongBuffer__Ljava_nio_LongBuffer_();
		$this->_dataFloat = $this->_data->asFloatBuffer__Ljava_nio_FloatBuffer_();
		$this->_dataDouble = $this->_data->asDoubleBuffer__Ljava_nio_DoubleBuffer_();
		return;
	}
	public function _initWithSize_I_V(int $p0) {
		$this->_length = $p0;
		$this->_data = java_nio_ByteBuffer::allocateDirect_I_Ljava_nio_ByteBuffer_(((int)((((int)(($p0 + 15))) & -16))))->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder::nativeOrder__Ljava_nio_ByteOrder_());
		return;
	}
	public function _createViewsExtra__B_V(?JA_B $p0) {
		return;
	}
	public function __construct($CLASS_ID = 852) {
		parent::__construct($CLASS_ID);
		$this->_dataInt = null;
		$this->_dataFloat = null;
		$this->_data = null;
		$this->_dataShort = null;
		$this->_dataDouble = null;
		$this->_dataChar = null;
		$this->_dataLong = null;
		$this->_length = 0;
	}
	static public function SI() {
	}
}
class Benchmark_MyClass extends java_lang_Object {

	public $_b = 0;
	public $_d = null;
	public $_c = null;
	public $_a = 0;
	public function Benchmark_MyClass_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_Object_init___V();
		$this->_a = 10;
		$this->_b = 20;
		$this->_c = Bootstrap::$STRINGLIT_44;
		$this->_d = $p0;
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 851) {
		parent::__construct($CLASS_ID);
		$this->_b = 0;
		$this->_d = null;
		$this->_c = null;
		$this->_a = 0;
	}
	static public function SI() {
	}
}
class com_jtransc_JTranscSystemProperties extends java_lang_Object {

	public function com_jtransc_JTranscSystemProperties_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function lineSeparator__Ljava_lang_String_() {
		return Bootstrap::$STRINGLIT_45;
	}
	public static function userHome__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = (new JA_L(1, "[Ljava.lang.String;"));
		$fA0 = $tA0;
		($tA0)->set(0, (Bootstrap::$STRINGLIT_46));
		return com_jtransc_JTranscSystemProperties::getenvs__Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_(($fA0), Bootstrap::$STRINGLIT_47);
	}
	public static function getenvs__Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_(?JA_L $p0, ?java_lang_String $p1) {
		$lA5 = null;
		$lA6 = null;
		$lI3 = 0;
		$lI4 = 0;
		$lI3 = (($p0))->length;
		$lI4 = 0;
		label_1:
		if ((($lI4 >= $lI3))) goto label_2;
		$lA5 = (($p0)->get($lI4));
		$lA6 = (java_lang_System::getenv_Ljava_lang_String__Ljava_lang_String_(($lA5)));
		if ((($lA6 == null))) goto label_3;
		return ($lA6);
		label_3:
		$lI4 = ((int)(($lI4 + 1)));
		goto label_1;
		label_2:
		return $p1;
	}
	public static function userLanguage__Ljava_lang_String_() {
		return Bootstrap::$STRINGLIT_48;
	}
	public static function tmpdir__Ljava_lang_String_() {
		$fA0 = null;
		$lA0 = null;
		$tA0 = null;
		$tA3 = null;
		$tA0 = (new JA_L(3, "[Ljava.lang.String;"));
		$fA0 = $tA0;
		($tA0)->set(0, (Bootstrap::$STRINGLIT_49));
		($fA0)->set(1, (Bootstrap::$STRINGLIT_50));
		($fA0)->set(2, (Bootstrap::$STRINGLIT_51));
		$lA0 = com_jtransc_JTranscSystemProperties::getenvs__Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_(($fA0), Bootstrap::$STRINGLIT_47);
		if (!(com_jtransc_JTranscSystem::isWindows__Z())) goto label_1;
		if (!($lA0->endsWith_Ljava_lang_String__Z(Bootstrap::$STRINGLIT_52))) goto label_3;
		if ($lA0->endsWith_Ljava_lang_String__Z(Bootstrap::$STRINGLIT_53)) goto label_1;
		label_3:
		$tA3 = ((new java_lang_StringBuilder()));
		$fA0 = $tA3;
		($tA3)->java_lang_StringBuilder_init___V();
		$lA0 = ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($lA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_53)->toString__Ljava_lang_String_();
		label_1:
		return $lA0;
	}
	public static function fileEncoding__Ljava_lang_String_() {
		return Bootstrap::$STRINGLIT_54;
	}
	public static function pathSeparator__Ljava_lang_String_() {
		return Bootstrap::$STRINGLIT_55;
	}
	public static function userDir__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = (new JA_L(1, "[Ljava.lang.String;"));
		$fA0 = $tA0;
		($tA0)->set(0, (Bootstrap::$STRINGLIT_46));
		return com_jtransc_JTranscSystemProperties::getenvs__Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_(($fA0), Bootstrap::$STRINGLIT_47);
	}
	public static function userName__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = (new JA_L(2, "[Ljava.lang.String;"));
		$fA0 = $tA0;
		($tA0)->set(0, (Bootstrap::$STRINGLIT_56));
		($fA0)->set(1, (Bootstrap::$STRINGLIT_57));
		return com_jtransc_JTranscSystemProperties::getenvs__Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_(($fA0), Bootstrap::$STRINGLIT_58);
	}
	public static function userVariant__Ljava_lang_String_() {
		return Bootstrap::$STRINGLIT_23;
	}
	public static function fileSeparator__Ljava_lang_String_() {
		return Bootstrap::$STRINGLIT_52;
	}
	public static function userRegion__Ljava_lang_String_() {
		return Bootstrap::$STRINGLIT_59;
	}
	public function __construct($CLASS_ID = 850) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_JTranscArrays extends java_lang_Object {

	public static $_EMPTY_BYTE = null;
	public static $_EMPTY_CLASS = null;
	public function com_jtransc_JTranscArrays_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function com_jtransc_JTranscArrays_clinit___V() {
		com_jtransc_JTranscArrays::$_EMPTY_BYTE = new JA_B(0);
		com_jtransc_JTranscArrays::$_EMPTY_CLASS = new JA_L(0, "[Ljava.lang.Class;");
		return;
	}
	public static function checkOffsetAndCount_III_V(int $p0, int $p1, int $p2) {
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		if (((((int)(($p1 | $p2))) < 0))) goto label_1;
		if ((($p1 > $p0))) goto label_1;
		if (((((int)(($p0 - $p1))) >= $p2))) goto label_2;
		label_1:
		$tA0 = ((new java_lang_ArrayIndexOutOfBoundsException()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_ArrayIndexOutOfBoundsException_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_62)->append_I_Ljava_lang_StringBuilder_($p0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_61)->append_I_Ljava_lang_StringBuilder_($p1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_60)->append_I_Ljava_lang_StringBuilder_($p2)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_2:
		return;
	}
	public function __construct($CLASS_ID = 849) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		com_jtransc_JTranscArrays::$_EMPTY_BYTE = null;
		com_jtransc_JTranscArrays::$_EMPTY_CLASS = null;
		com_jtransc_JTranscArrays::com_jtransc_JTranscArrays_clinit___V();
	}
}
abstract class java_lang_ClassLoader extends java_lang_Object {

	public $_nativeLibs = null;
	public $_parent = null;
	public function java_lang_ClassLoader_init___V() {
		$this->java_lang_ClassLoader_init__Ljava_lang_ClassLoader__V(null);
		return $this;
		return $this;
	}
	public function java_lang_ClassLoader_init__Ljava_lang_ClassLoader__V(?java_lang_ClassLoader $p0) {
		$fA1 = null;
		$tA0 = null;
		($this)->java_lang_Object_init___V();
		$tA0 = ((new java_util_ArrayList()));
		$fA1 = $tA0;
		($tA0)->java_util_ArrayList_init___V();
		$this->_nativeLibs = ($fA1);
		$this->_parent = $p0;
		return $this;
		return $this;
	}
	public static function getSystemClassLoader__Ljava_lang_ClassLoader_() {
		return java_lang__ClassInternalUtils::getSystemClassLoader__Ljava_lang_ClassLoader_();
	}
	public function __construct($CLASS_ID = 846) {
		parent::__construct($CLASS_ID);
		$this->_nativeLibs = null;
		$this->_parent = null;
	}
	static public function SI() {
	}
}
class java_lang__ClassInternalUtils_1 extends java_lang_ClassLoader {

	public function java_lang__ClassInternalUtils_1_init___V() {
		($this)->java_lang_ClassLoader_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 848) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang__ClassInternalUtils extends java_lang_Object {

	public static $_classLoader = null;
	public function java_lang__ClassInternalUtils_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function getSystemClassLoader__Ljava_lang_ClassLoader_() {
		$fA0 = null;
		$tA0 = null;
		if (((java_lang__ClassInternalUtils::$_classLoader != null))) goto label_1;
		$tA0 = ((new java_lang__ClassInternalUtils_1()));
		$fA0 = $tA0;
		($tA0)->java_lang__ClassInternalUtils_1_init___V();
		java_lang__ClassInternalUtils::$_classLoader = ($fA0);
		label_1:
		return java_lang__ClassInternalUtils::$_classLoader;
	}
	public function __construct($CLASS_ID = 847) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		java_lang__ClassInternalUtils::$_classLoader = null;
	}
}
interface java_lang_Thread_UncaughtExceptionHandler {

}
class java_lang_Thread_UncaughtExceptionHandler_IFields {

	static public function SI() {
	}
}
class java_lang_ThreadGroup extends java_lang_Object implements java_lang_Thread_UncaughtExceptionHandler {

	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(($this)->getClass__Ljava_lang_Class_()->getName__Ljava_lang_String_())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_64)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->getName__Ljava_lang_String_())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_63)->append_I_Ljava_lang_StringBuilder_($this->getMaxPriority__I())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_32)->toString__Ljava_lang_String_();
	}
	public function getMaxPriority__I() {
		throw new Exception("Missing body java.lang.ThreadGroup.getMaxPriority()I");
	}
	public function getName__Ljava_lang_String_() {
		throw new Exception("Missing body java.lang.ThreadGroup.getName()Ljava/lang/String;");
	}
	public function __construct($CLASS_ID = 844) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_lang_Runnable {

}
class java_lang_Runnable_IFields {

	static public function SI() {
	}
}
class java_lang_Thread extends java_lang_Object implements java_lang_Runnable {

	public $_name = null;
	public $_group = null;
	public static $__currentThread = null;
	public $_classLoader = null;
	public function toString__Ljava_lang_String_() {
		$lA1 = null;
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		$lA1 = ($this->getThreadGroup__Ljava_lang_ThreadGroup_());
		if ((($lA1 == null))) goto label_1;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_66)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->getName__Ljava_lang_String_())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_65)->append_I_Ljava_lang_StringBuilder_($this->getPriority__I())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_65)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(($lA1)->getName__Ljava_lang_String_())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_32)->toString__Ljava_lang_String_();
		label_1:
		$tA1 = ((new java_lang_StringBuilder()));
		$fA0 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_66)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->getName__Ljava_lang_String_())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_65)->append_I_Ljava_lang_StringBuilder_($this->getPriority__I())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_67)->toString__Ljava_lang_String_();
	}
	public function getName__Ljava_lang_String_() {
		return $this->_name;
	}
	public function getPriority__I() {
		return 5;
	}
	public function getThreadGroup__Ljava_lang_ThreadGroup_() {
		return $this->_group;
	}
	public static function currentThread__Ljava_lang_Thread_() {
		$fA0 = null;
		$tA0 = null;
		if (((java_lang_Thread::$__currentThread != null))) goto label_1;
		$tA0 = ((new java_lang_Thread()));
		$fA0 = $tA0;
		($tA0)->java_lang_Thread_init___V();
		java_lang_Thread::$__currentThread = ($fA0);
		label_1:
		return java_lang_Thread::$__currentThread;
	}
	public function java_lang_Thread_init___V() {
		($this)->java_lang_Object_init___V();
		$this->_classLoader = null;
		return $this;
		return $this;
	}
	public function getContextClassLoader__Ljava_lang_ClassLoader_() {
		if ((($this->_classLoader != null))) goto label_1;
		$this->_classLoader = java_lang__ClassInternalUtils::getSystemClassLoader__Ljava_lang_ClassLoader_();
		label_1:
		return $this->_classLoader;
	}
	public function __construct($CLASS_ID = 842) {
		parent::__construct($CLASS_ID);
		$this->_name = null;
		$this->_group = null;
		$this->_classLoader = null;
	}
	static public function SI() {
		java_lang_Thread::$__currentThread = null;
	}
}
abstract class com_jtransc_charset_JTranscCharset extends java_lang_Object {

	public $_max = 0;
	public $_min = 0;
	public $_avg = 0.0;
	public $_names = null;
	public static $_charsets = null;
	public static $_loadedCharsets = false;
	public function com_jtransc_charset_JTranscCharset_init___Ljava_lang_String_IFI_V(?JA_L $p0, int $p1, float $p2, int $p3) {
		($this)->java_lang_Object_init___V();
		$this->_names = $p0;
		$this->_min = $p1;
		$this->_avg = $p2;
		$this->_max = $p3;
		return $this;
		return $this;
	}
	public static function com_jtransc_charset_JTranscCharset_clinit___V() {
		$fA0 = null;
		$tA0 = null;
		com_jtransc_charset_JTranscCharset::$_loadedCharsets = false;
		$tA0 = ((new com_jtransc_ds_FastStringMap()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_ds_FastStringMap_init___V();
		com_jtransc_charset_JTranscCharset::$_charsets = ($fA0);
		return;
	}
	public function encode_Ljava_lang_String___B(?java_lang_String $p0) {
		$lA2 = null;
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_io_ByteArrayOutputStream()));
		$fA0 = $tA0;
		($tA0)->java_io_ByteArrayOutputStream_init__I_V(((int)(((((((double)($p0->length__I())) * $this->avgBytesPerCharacter__F())))|0))));
		$lA2 = $fA0;
		$this->encode__CIILjava_io_ByteArrayOutputStream__V($p0->toCharArray___C(), 0, $p0->length__I(), ($lA2));
		return ($lA2)->toByteArray___B();
	}
	public function decodeChars__BII__C(?JA_B $p0, int $p1, int $p2) {
		return $this->decode__BII_Ljava_lang_String_($p0, $p1, $p2)->toCharArray___C();
	}
	public function decode__BII_Ljava_lang_String_(?JA_B $p0, int $p1, int $p2) {
		$lA4 = null;
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new com_jtransc_charset_JTranscCharBuffer()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_charset_JTranscCharBuffer_init__I_V(((int)(((((((double)((($p0))->length)) / $this->avgBytesPerCharacter__F())))|0))));
		$lA4 = $fA0;
		$this->decode__BIILcom_jtransc_charset_JTranscCharBuffer__V($p0, $p1, $p2, ($lA4));
		return ($lA4)->toString__Ljava_lang_String_();
	}
	public function decode__BIILcom_jtransc_charset_JTranscCharBuffer__V(?JA_B $p0, int $p1, int $p2, ?com_jtransc_charset_JTranscCharBuffer $p3) {
		throw new Exception("Missing body com.jtransc.charset.JTranscCharset.decode([BIILcom/jtransc/charset/JTranscCharBuffer;)V");
	}
	public function avgBytesPerCharacter__F() {
		return $this->_avg;
	}
	public static function forName_Ljava_lang_String__Lcom_jtransc_charset_JTranscCharset_(?java_lang_String $p0) {
		$lA1 = null;
		$fA0 = null;
		$tA0 = null;
		com_jtransc_charset_JTranscCharset::ensureRegister__V();
		$lA1 = (N::checkcast(com_jtransc_charset_JTranscCharset::$_charsets->get_Ljava_lang_String__Ljava_lang_Object_($p0->toUpperCase__Ljava_lang_String_()->trim__Ljava_lang_String_()), "com_jtransc_charset_JTranscCharset"));
		if ((($lA1 != null))) goto label_1;
		$tA0 = ((new java_nio_charset_UnsupportedCharsetException()));
		$fA0 = $tA0;
		($tA0)->java_nio_charset_UnsupportedCharsetException_init__Ljava_lang_String__V($p0);
		throw new WrappedThrowable($fA0);
		label_1:
		return ($lA1);
	}
	public static function ensureRegister__V() {
		$lA1 = null;
		$lA0 = null;
		if (!(com_jtransc_charset_JTranscCharset::$_loadedCharsets)) goto label_1;
		return;
		label_1:
		com_jtransc_charset_JTranscCharset::$_loadedCharsets = true;
		$lA0 = java_util_ServiceLoader::load_Ljava_lang_Class__Ljava_util_ServiceLoader_(N::resolveClass("Lcom/jtransc/charset/JTranscCharset;"))->iterator__Ljava_util_Iterator_();
		label_2:
		if (!($lA0->hasNext__Z())) goto label_3;
		$lA1 = N::checkcast($lA0->next__Ljava_lang_Object_(), "com_jtransc_charset_JTranscCharset");
		com_jtransc_charset_JTranscCharset::registerCharset_Lcom_jtransc_charset_JTranscCharset__V($lA1);
		goto label_2;
		label_3:
		return;
	}
	public static function registerCharset_Lcom_jtransc_charset_JTranscCharset__V(?com_jtransc_charset_JTranscCharset $p0) {
		$lA1 = null;
		$lA4 = null;
		$lI2 = 0;
		$lI3 = 0;
		$fA0 = null;
		$tA0 = null;
		if (((com_jtransc_charset_JTranscCharset::$_charsets != null))) goto label_1;
		$tA0 = ((new com_jtransc_ds_FastStringMap()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_ds_FastStringMap_init___V();
		com_jtransc_charset_JTranscCharset::$_charsets = ($fA0);
		label_1:
		$lA1 = ($p0->getAliases___Ljava_lang_String_());
		$lI2 = ($lA1)->length;
		$lI3 = 0;
		label_2:
		if ((($lI3 >= $lI2))) goto label_3;
		$lA4 = (($lA1)->get($lI3));
		com_jtransc_charset_JTranscCharset::$_charsets->set_Ljava_lang_String_Ljava_lang_Object__V(($lA4)->toUpperCase__Ljava_lang_String_()->trim__Ljava_lang_String_(), ($p0));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_2;
		label_3:
		return;
	}
	public function getAliases___Ljava_lang_String_() {
		return $this->_names;
	}
	public function encode__CIILjava_io_ByteArrayOutputStream__V(?JA_C $p0, int $p1, int $p2, ?java_io_ByteArrayOutputStream $p3) {
		throw new Exception("Missing body com.jtransc.charset.JTranscCharset.encode([CIILjava/io/ByteArrayOutputStream;)V");
	}
	public function __construct($CLASS_ID = 809) {
		parent::__construct($CLASS_ID);
		$this->_max = 0;
		$this->_min = 0;
		$this->_avg = 0.0;
		$this->_names = null;
	}
	static public function SI() {
		com_jtransc_charset_JTranscCharset::$_charsets = null;
		com_jtransc_charset_JTranscCharset::$_loadedCharsets = false;
		com_jtransc_charset_JTranscCharset::com_jtransc_charset_JTranscCharset_clinit___V();
	}
}
abstract class com_jtransc_charset_charsets_JTranscCharsetUTF16Base extends com_jtransc_charset_JTranscCharset {

	public $_littleEndian = false;
	public function com_jtransc_charset_charsets_JTranscCharsetUTF16Base_init___Ljava_lang_String_Z_V(?JA_L $p0, bool $p1) {
		($this)->com_jtransc_charset_JTranscCharset_init___Ljava_lang_String_IFI_V($p0, 2, 2.0, 2);
		$this->_littleEndian = $p1;
		return $this;
		return $this;
	}
	public function decode__BIILcom_jtransc_charset_JTranscCharBuffer__V(?JA_B $p0, int $p1, int $p2, ?com_jtransc_charset_JTranscCharBuffer $p3) {
		$lI5 = 0;
		$lI5 = 0;
		label_1:
		if ((($lI5 >= $p2))) goto label_2;
		$p3->append_C_V((N::i2c(com_jtransc_JTranscBits::readInt16__BIZ_S($p0, ((int)(($p1 + $lI5))), $this->_littleEndian))));
		$lI5 = ((int)(($lI5 + 2)));
		goto label_1;
		label_2:
		return;
	}
	public function encode__CIILjava_io_ByteArrayOutputStream__V(?JA_C $p0, int $p1, int $p2, ?java_io_ByteArrayOutputStream $p3) {
		$lI5 = 0;
		$lI6 = 0;
		$lI5 = 0;
		label_1:
		if ((($lI5 >= $p2))) goto label_2;
		$lI6 = ((int)(($p0->get(((int)(($p1 + $lI5)))))));
		if (!($this->_littleEndian)) goto label_4;
		$p3->write_I_V(((int)(($lI6 & 255))));
		$p3->write_I_V(((int)((((int)(N::iushr($lI6, 8))) & 255))));
		goto label_6;
		label_4:
		$p3->write_I_V(((int)((((int)(N::iushr($lI6, 8))) & 255))));
		$p3->write_I_V(((int)(($lI6 & 255))));
		label_6:
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		return;
	}
	public function __construct($CLASS_ID = 838) {
		parent::__construct($CLASS_ID);
		$this->_littleEndian = false;
	}
	static public function SI() {
	}
}
class com_jtransc_charset_charsets_JTranscCharsetUTF16BE extends com_jtransc_charset_charsets_JTranscCharsetUTF16Base {

	public function com_jtransc_charset_charsets_JTranscCharsetUTF16BE_init___V() {
		$fA1 = null;
		$tA0 = null;
		$tA0 = (new JA_L(4, "[Ljava.lang.String;"));
		$fA1 = $tA0;
		($tA0)->set(0, (Bootstrap::$STRINGLIT_68));
		($fA1)->set(1, (Bootstrap::$STRINGLIT_69));
		($fA1)->set(2, (Bootstrap::$STRINGLIT_70));
		($fA1)->set(3, (Bootstrap::$STRINGLIT_71));
		($this)->com_jtransc_charset_charsets_JTranscCharsetUTF16Base_init___Ljava_lang_String_Z_V(($fA1), false);
		return $this;
		return $this;
	}
	public function decode__BIILcom_jtransc_charset_JTranscCharBuffer__V(?JA_B $p0, int $p1, int $p2, ?com_jtransc_charset_JTranscCharBuffer $p3) {
		parent::decode__BIILcom_jtransc_charset_JTranscCharBuffer__V($p0, $p1, $p2, $p3);
		return;
	}
	public function encode__CIILjava_io_ByteArrayOutputStream__V(?JA_C $p0, int $p1, int $p2, ?java_io_ByteArrayOutputStream $p3) {
		parent::encode__CIILjava_io_ByteArrayOutputStream__V($p0, $p1, $p2, $p3);
		return;
	}
	public function __construct($CLASS_ID = 841) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_charset_JTranscCharsetSingleByte extends com_jtransc_charset_JTranscCharset {

	public $_invalidChar = 0;
	public $_decode = null;
	public $_encode = null;
	public function com_jtransc_charset_JTranscCharsetSingleByte_init___Ljava_lang_String_Ljava_lang_String__V(?JA_L $p0, ?java_lang_String $p1) {
		$lI3 = 0;
		$fA1 = null;
		$tA0 = null;
		($this)->com_jtransc_charset_JTranscCharset_init___Ljava_lang_String_IFI_V($p0, 1, 1.0, 1);
		$this->_invalidChar = 63;
		$this->_decode = $p1;
		$tA0 = ((new java_util_HashMap()));
		$fA1 = $tA0;
		($tA0)->java_util_HashMap_init__I_V($p1->length__I());
		$this->_encode = ($fA1);
		$lI3 = 0;
		label_1:
		if ((($lI3 >= $p1->length__I()))) goto label_2;
		$this->_encode->put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_((java_lang_Character::valueOf_C_Ljava_lang_Character_($p1->charAt_I_C($lI3))), (java_lang_Byte::valueOf_B_Ljava_lang_Byte_((N::i2b($lI3)))));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return $this;
		return $this;
	}
	public function decode__BIILcom_jtransc_charset_JTranscCharBuffer__V(?JA_B $p0, int $p1, int $p2, ?com_jtransc_charset_JTranscCharBuffer $p3) {
		$lI5 = 0;
		$lI6 = 0;
		$lI5 = 0;
		label_1:
		if ((($lI5 >= $p2))) goto label_2;
		$lI6 = ((int)((($p0->get(((int)(($p1 + $lI5))))) & 255)));
		$p3->append_C_V($this->_decode->charAt_I_C($lI6));
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		return;
	}
	public function encode__CIILjava_io_ByteArrayOutputStream__V(?JA_C $p0, int $p1, int $p2, ?java_io_ByteArrayOutputStream $p3) {
		$lA7 = null;
		$lI5 = 0;
		$lI6 = 0;
		$lI5 = 0;
		label_1:
		if ((($lI5 >= $p2))) goto label_2;
		$lI6 = ((int)(($p0->get(((int)(($p1 + $lI5)))))));
		$lA7 = (N::checkcast($this->_encode->get_Ljava_lang_Object__Ljava_lang_Object_((java_lang_Character::valueOf_C_Ljava_lang_Character_((N::i2c($lI6))))), "java_lang_Byte"));
		if ((($lA7 == null))) goto label_4;
		$p3->write_I_V(((int)(($lA7)->byteValue__B())));
		goto label_6;
		label_4:
		$p3->write_I_V(63);
		label_6:
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		return;
	}
	public function __construct($CLASS_ID = 815) {
		parent::__construct($CLASS_ID);
		$this->_invalidChar = 63;
		$this->_decode = null;
		$this->_encode = null;
	}
	static public function SI() {
	}
}
class com_jtransc_charset_charsets_JTranscCharsetLatin1 extends com_jtransc_charset_JTranscCharsetSingleByte {

	public function com_jtransc_charset_charsets_JTranscCharsetLatin1_init___V() {
		$fA1 = null;
		$tA0 = null;
		$tA0 = (new JA_L(15, "[Ljava.lang.String;"));
		$fA1 = $tA0;
		($tA0)->set(0, (Bootstrap::$STRINGLIT_72));
		($fA1)->set(1, (Bootstrap::$STRINGLIT_73));
		($fA1)->set(2, (Bootstrap::$STRINGLIT_74));
		($fA1)->set(3, (Bootstrap::$STRINGLIT_75));
		($fA1)->set(4, (Bootstrap::$STRINGLIT_76));
		($fA1)->set(5, (Bootstrap::$STRINGLIT_77));
		($fA1)->set(6, (Bootstrap::$STRINGLIT_78));
		($fA1)->set(7, (Bootstrap::$STRINGLIT_79));
		($fA1)->set(8, (Bootstrap::$STRINGLIT_80));
		($fA1)->set(9, (Bootstrap::$STRINGLIT_81));
		($fA1)->set(10, (Bootstrap::$STRINGLIT_82));
		($fA1)->set(11, (Bootstrap::$STRINGLIT_83));
		($fA1)->set(12, (Bootstrap::$STRINGLIT_84));
		($fA1)->set(13, (Bootstrap::$STRINGLIT_85));
		($fA1)->set(14, (Bootstrap::$STRINGLIT_86));
		($this)->com_jtransc_charset_JTranscCharsetSingleByte_init___Ljava_lang_String_Ljava_lang_String__V(($fA1), Bootstrap::$STRINGLIT_87);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 840) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_JTranscBits extends java_lang_Object {

	public function com_jtransc_JTranscBits_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function readInt16__BIZ_S(?JA_B $p0, int $p1, bool $p2) {
		$fI0 = 0;
		if (!($p2)) goto label_1;
		$fI0 = ((int)(com_jtransc_JTranscBits::readInt16LE__BI_S($p0, $p1)));
		goto label_2;
		label_1:
		$fI0 = ((int)(com_jtransc_JTranscBits::readInt16BE__BI_S($p0, $p1)));
		label_2:
		return (N::i2s($fI0));
	}
	public static function readInt16BE__BI_S(?JA_B $p0, int $p1) {
		return (N::i2s(((int)((((int)(N::ishl(((int)((($p0->get(((int)(($p1 + 0))))) & 255))), 8))) | ((int)(N::ishl(((int)((($p0->get(((int)(($p1 + 1))))) & 255))), 0))))))));
	}
	public static function readInt16LE__BI_S(?JA_B $p0, int $p1) {
		return (N::i2s(((int)((((int)(N::ishl(((int)((($p0->get(((int)(($p1 + 1))))) & 255))), 8))) | ((int)(N::ishl(((int)((($p0->get(((int)(($p1 + 0))))) & 255))), 0))))))));
	}
	public static function isLittleEndian__Z() {
		return true;
	}
	public static function writeFloatLE__BIF_V(?JA_B $p0, int $p1, float $p2) {
		com_jtransc_JTranscBits::writeIntLE__BII_V($p0, $p1, java_lang_Float::floatToRawIntBits_F_I($p2));
		return;
	}
	public static function writeIntLE__BII_V(?JA_B $p0, int $p1, int $p2) {
		$p0->set(((int)(($p1 + 3))), (N::i2b(((int)((((int)(N::iushr($p2, 24))) & 255))))));
		$p0->set(((int)(($p1 + 2))), (N::i2b(((int)((((int)(N::iushr($p2, 16))) & 255))))));
		$p0->set(((int)(($p1 + 1))), (N::i2b(((int)((((int)(N::iushr($p2, 8))) & 255))))));
		$p0->set(((int)(($p1 + 0))), (N::i2b(((int)((((int)(N::iushr($p2, 0))) & 255))))));
		return;
	}
	public static function reverseBytes_J_J(Int64 $p0) {
		return java_lang_Long::reverseBytes_J_J($p0);
	}
	public static function writeDoubleLE__BID_V(?JA_B $p0, int $p1, float $p2) {
		com_jtransc_JTranscBits::writeLongLE__BIJ_V($p0, $p1, java_lang_Double::doubleToRawLongBits_D_J($p2));
		return;
	}
	public static function writeLongLE__BIJ_V(?JA_B $p0, int $p1, Int64 $p2) {
		$lI4 = 0;
		$lI5 = 0;
		$lI4 = N::j2i((N::lshr($p2, 32)));
		$lI5 = N::j2i((N::lshr($p2, 0)));
		$p0->set(((int)(($p1 + 7))), (N::i2b(((int)(N::iushr($lI4, 24))))));
		$p0->set(((int)(($p1 + 6))), (N::i2b(((int)(N::iushr($lI4, 16))))));
		$p0->set(((int)(($p1 + 5))), (N::i2b(((int)(N::iushr($lI4, 8))))));
		$p0->set(((int)(($p1 + 4))), (N::i2b(((int)(N::iushr($lI4, 0))))));
		$p0->set(((int)(($p1 + 3))), (N::i2b(((int)(N::iushr($lI5, 24))))));
		$p0->set(((int)(($p1 + 2))), (N::i2b(((int)(N::iushr($lI5, 16))))));
		$p0->set(((int)(($p1 + 1))), (N::i2b(((int)(N::iushr($lI5, 8))))));
		$p0->set(((int)(($p1 + 0))), (N::i2b(((int)(N::iushr($lI5, 0))))));
		return;
	}
	public static function writeCharLE__BIC_V(?JA_B $p0, int $p1, int $p2) {
		$p0->set(((int)(($p1 + 1))), (N::i2b(((int)((((int)(N::iushr(((int)($p2)), 8))) & 255))))));
		$p0->set(((int)(($p1 + 0))), (N::i2b(((int)((((int)(N::iushr(((int)($p2)), 0))) & 255))))));
		return;
	}
	public function __construct($CLASS_ID = 839) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_charset_charsets_JTranscCharsetUTF16LE extends com_jtransc_charset_charsets_JTranscCharsetUTF16Base {

	public function com_jtransc_charset_charsets_JTranscCharsetUTF16LE_init___V() {
		$fA1 = null;
		$tA0 = null;
		$tA0 = (new JA_L(4, "[Ljava.lang.String;"));
		$fA1 = $tA0;
		($tA0)->set(0, (Bootstrap::$STRINGLIT_88));
		($fA1)->set(1, (Bootstrap::$STRINGLIT_89));
		($fA1)->set(2, (Bootstrap::$STRINGLIT_90));
		($fA1)->set(3, (Bootstrap::$STRINGLIT_91));
		($this)->com_jtransc_charset_charsets_JTranscCharsetUTF16Base_init___Ljava_lang_String_Z_V(($fA1), false);
		return $this;
		return $this;
	}
	public function decode__BIILcom_jtransc_charset_JTranscCharBuffer__V(?JA_B $p0, int $p1, int $p2, ?com_jtransc_charset_JTranscCharBuffer $p3) {
		parent::decode__BIILcom_jtransc_charset_JTranscCharBuffer__V($p0, $p1, $p2, $p3);
		return;
	}
	public function encode__CIILjava_io_ByteArrayOutputStream__V(?JA_C $p0, int $p1, int $p2, ?java_io_ByteArrayOutputStream $p3) {
		parent::encode__CIILjava_io_ByteArrayOutputStream__V($p0, $p1, $p2, $p3);
		return;
	}
	public function __construct($CLASS_ID = 837) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_charset_charsets_JTranscCharsetUSASCII extends com_jtransc_charset_JTranscCharsetSingleByte {

	public function com_jtransc_charset_charsets_JTranscCharsetUSASCII_init___V() {
		$fA1 = null;
		$tA0 = null;
		$tA0 = (new JA_L(15, "[Ljava.lang.String;"));
		$fA1 = $tA0;
		($tA0)->set(0, (Bootstrap::$STRINGLIT_92));
		($fA1)->set(1, (Bootstrap::$STRINGLIT_93));
		($fA1)->set(2, (Bootstrap::$STRINGLIT_94));
		($fA1)->set(3, (Bootstrap::$STRINGLIT_95));
		($fA1)->set(4, (Bootstrap::$STRINGLIT_96));
		($fA1)->set(5, (Bootstrap::$STRINGLIT_97));
		($fA1)->set(6, (Bootstrap::$STRINGLIT_98));
		($fA1)->set(7, (Bootstrap::$STRINGLIT_99));
		($fA1)->set(8, (Bootstrap::$STRINGLIT_100));
		($fA1)->set(9, (Bootstrap::$STRINGLIT_101));
		($fA1)->set(10, (Bootstrap::$STRINGLIT_102));
		($fA1)->set(11, (Bootstrap::$STRINGLIT_103));
		($fA1)->set(12, (Bootstrap::$STRINGLIT_104));
		($fA1)->set(13, (Bootstrap::$STRINGLIT_105));
		($fA1)->set(14, (Bootstrap::$STRINGLIT_59));
		($this)->com_jtransc_charset_JTranscCharsetSingleByte_init___Ljava_lang_String_Ljava_lang_String__V(($fA1), Bootstrap::$STRINGLIT_106);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 836) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_charset_charsets_JTranscCharsetUTF8 extends com_jtransc_charset_JTranscCharset {

	public function com_jtransc_charset_charsets_JTranscCharsetUTF8_init___V() {
		$fA1 = null;
		$tA0 = null;
		$tA0 = (new JA_L(2, "[Ljava.lang.String;"));
		$fA1 = $tA0;
		($tA0)->set(0, (Bootstrap::$STRINGLIT_54));
		($fA1)->set(1, (Bootstrap::$STRINGLIT_107));
		($this)->com_jtransc_charset_JTranscCharset_init___Ljava_lang_String_IFI_V(($fA1), 1, 1.2, 4);
		return $this;
		return $this;
	}
	public function decode__BIILcom_jtransc_charset_JTranscCharBuffer__V(?JA_B $p0, int $p1, int $p2, ?com_jtransc_charset_JTranscCharBuffer $p3) {
		$fA2 = null;
		$fI1 = 0;
		$fI3 = 0;
		$lI5 = 0;
		$lI6 = 0;
		$lI7 = 0;
		$fA0 = null;
		$lI5 = $p1;
		$lI6 = ((int)(($p1 + $p2)));
		label_1:
		if ((($lI5 >= $lI6))) goto label_2;
		$fA0 = ($p0);
		$fI1 = $lI5;
		$lI5 = ((int)(($lI5 + 1)));
		$lI7 = ((int)(((($fA0)->get($fI1)) & 255)));
		switch (((int)(N::ishr($lI7, 4)))) {
			case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7: goto label_5;
			case 8: case 9: case 10: case 11: goto label_4;
			case 12: case 13: goto label_6;
			case 14: goto label_7;
			default: goto label_4;
		}
		label_5:
		$p3->append_C_V((N::i2c($lI7)));
		goto label_4;
		label_6:
		$fA0 = ($p3);
		$fI1 = ((int)(N::ishl(((int)(($lI7 & 31))), 6)));
		$fA2 = $p0;
		$fI3 = $lI5;
		$lI5 = ((int)(($lI5 + 1)));
		($fA0)->append_C_V((N::i2c(((int)(($fI1 | ((int)((($fA2->get($fI3)) & 63)))))))));
		goto label_4;
		label_7:
		$fA0 = ($p3);
		$fI1 = ((int)(N::ishl(((int)(($lI7 & 15))), 12)));
		$fA2 = $p0;
		$fI3 = $lI5;
		$lI5 = ((int)(($lI5 + 1)));
		$fI1 = ((int)(($fI1 | ((int)(N::ishl(((int)((($fA2->get($fI3)) & 63))), 6))))));
		$fA2 = $p0;
		$fI3 = $lI5;
		$lI5 = ((int)(($lI5 + 1)));
		($fA0)->append_C_V((N::i2c(((int)(($fI1 | ((int)((($fA2->get($fI3)) & 63)))))))));
		label_4:
		goto label_1;
		label_2:
		return;
	}
	public function encode__CIILjava_io_ByteArrayOutputStream__V(?JA_C $p0, int $p1, int $p2, ?java_io_ByteArrayOutputStream $p3) {
		$lI5 = 0;
		$lI6 = 0;
		$lI5 = 0;
		label_1:
		if ((($lI5 >= $p2))) goto label_2;
		$lI6 = ((int)(($p0->get(((int)(($p1 + $lI5)))))));
		if (((((int)(($lI6 & -128))) != 0))) goto label_4;
		$p3->write_I_V($lI6);
		goto label_6;
		label_4:
		if (((((int)(($lI6 & -2048))) != 0))) goto label_7;
		$p3->write_I_V(((int)((((int)((((int)(N::ishr($lI6, 6))) & 31))) | 192))));
		goto label_9;
		label_7:
		if (((((int)(($lI6 & -65536))) != 0))) goto label_10;
		$p3->write_I_V(((int)((((int)((((int)(N::ishr($lI6, 12))) & 15))) | 224))));
		$p3->write_I_V(com_jtransc_charset_charsets_JTranscCharsetUTF8::createByte_II_I($lI6, 6));
		goto label_9;
		label_10:
		if (((((int)(($lI6 & -2097152))) != 0))) goto label_9;
		$p3->write_I_V(((int)((((int)((((int)(N::ishr($lI6, 18))) & 7))) | 240))));
		$p3->write_I_V(com_jtransc_charset_charsets_JTranscCharsetUTF8::createByte_II_I($lI6, 12));
		$p3->write_I_V(com_jtransc_charset_charsets_JTranscCharsetUTF8::createByte_II_I($lI6, 6));
		label_9:
		$p3->write_I_V(((int)((((int)(($lI6 & 63))) | 128))));
		label_6:
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		return;
	}
	public static function createByte_II_I(int $p0, int $p1) {
		return ((int)((((int)((((int)(N::ishr($p0, $p1))) & 63))) | 128)));
	}
	public function __construct($CLASS_ID = 835) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_Collections_EmptySet extends java_util_AbstractSet implements java_io_Serializable {

	public function size__I() {
		return 0;
	}
	public function iterator__Ljava_util_Iterator_() {
		return java_util_Collections::access_000__Ljava_util_Iterator_();
	}
	public function contains_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		return false;
	}
	public function java_util_Collections_EmptySet_init__Ljava_util_Collections_1__V(?java_util_Collections_1 $p0) {
		$this->java_util_Collections_EmptySet_init___V();
		return $this;
		return $this;
	}
	public function java_util_Collections_EmptySet_init___V() {
		($this)->java_util_AbstractSet_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 834) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_util_AbstractMap extends java_lang_Object implements java_util_Map {

	public $_valuesCollection = null;
	public $_keySet = null;
	public function java_util_AbstractMap_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function toString__Ljava_lang_String_() {
		$lA1 = null;
		$lA4 = null;
		$lA5 = null;
		$lA3 = null;
		$fA0 = null;
		$tA0 = null;
		$lA2 = null;
		if (!($this->isEmpty__Z())) goto label_1;
		return Bootstrap::$STRINGLIT_108;
		label_1:
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init__I_V(((int)(N::imul($this->size__I(), 28))));
		$lA1 = $fA0;
		($lA1)->append_C_Ljava_lang_StringBuilder_(123);
		$lA2 = $this->entrySet__Ljava_util_Set_()->iterator__Ljava_util_Iterator_();
		label_3:
		if (!($lA2->hasNext__Z())) goto label_4;
		$lA3 = N::checkcast($lA2->next__Ljava_lang_Object_(), "java_util_Map_Entry");
		$lA4 = $lA3->getKey__Ljava_lang_Object_();
		if ((($lA4 == ($this)))) goto label_6;
		($lA1)->append_Ljava_lang_Object__Ljava_lang_StringBuilder_($lA4);
		goto label_8;
		label_6:
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_10);
		label_8:
		($lA1)->append_C_Ljava_lang_StringBuilder_(61);
		$lA5 = $lA3->getValue__Ljava_lang_Object_();
		if ((($lA5 == ($this)))) goto label_9;
		($lA1)->append_Ljava_lang_Object__Ljava_lang_StringBuilder_($lA5);
		goto label_11;
		label_9:
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_10);
		label_11:
		if (!($lA2->hasNext__Z())) goto label_12;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_6);
		label_12:
		goto label_3;
		label_4:
		($lA1)->append_C_Ljava_lang_StringBuilder_(125);
		return ($lA1)->toString__Ljava_lang_String_();
	}
	public function entrySet__Ljava_util_Set_() {
		throw new Exception("Missing body java.util.AbstractMap.entrySet()Ljava/util/Set;");
	}
	public function isEmpty__Z() {
		$fI0 = 0;
		if ((($this->size__I() != 0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function size__I() {
		return $this->entrySet__Ljava_util_Set_()->size__I();
	}
	public function clone__Ljava_lang_Object_() {
		$lA1 = null;
		$lA1 = (N::checkcast(parent::clone__Ljava_lang_Object_(), "java_util_AbstractMap"));
		($lA1)->_keySet = null;
		($lA1)->_valuesCollection = null;
		return $lA1;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$G = 0;
		$lA3 = null;
		$lA5 = null;
		$lA6 = null;
		$lA7 = null;
		$fI0 = 0;
		$lA4 = null;
		$fA0 = null;
		$lA2 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							if (((($this) != $p0))) {
								$G = 1;
								continue 2;
							}
							return true;
						case 1:
							if (!((($p0) instanceof java_util_Map))) {
								$G = 2;
								continue 2;
							}
							$lA2 = N::checkcast($p0, "java_util_Map");
							if ((($this->size__I() == $lA2->size__I()))) {
								$G = 3;
								continue 2;
							}
							return false;
						case 3:
							$lA3 = ($this->entrySet__Ljava_util_Set_()->iterator__Ljava_util_Iterator_());
							$G = 4;
							continue 2;
						case 4:
							if (!(($lA3)->hasNext__Z())) {
								$G = 5;
								continue 2;
							}
							$lA4 = N::checkcast(($lA3)->next__Ljava_lang_Object_(), "java_util_Map_Entry");
							$lA5 = $lA4->getKey__Ljava_lang_Object_();
							$lA6 = $lA4->getValue__Ljava_lang_Object_();
							$lA7 = $lA2->get_Ljava_lang_Object__Ljava_lang_Object_($lA5);
							if ((($lA6 != null))) {
								$G = 6;
								continue 2;
							}
							if ((($lA7 != null))) {
								$G = 7;
								continue 2;
							}
							if ($lA2->containsKey_Ljava_lang_Object__Z($lA5)) {
								$G = 8;
								continue 2;
							}
							$G = 7;
							continue 2;
						case 7:
							$fI0 = 0;
							$G = 9;
							continue 2;
						case 9:return (($fI0)!=0); 
						case 6:
							if ($lA6->equals_Ljava_lang_Object__Z($lA7)) {
								$G = 8;
								continue 2;
							}
							$fI0 = 0;
							$G = 10;
							continue 2;
						case 10:return (($fI0)!=0); 
						case 8:
							$G = 4;
							continue 2;
						case 5:
							$G = 11;
							continue 2;
						case 12:
							$fA0 = ($J__exception__);
							$lA3 = $fA0;
							return false;
						case 13:
							$fA0 = ($J__exception__);
							$lA3 = $fA0;
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
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 3)) && (($G < 9)))) && (($J__exception__) instanceof java_lang_NullPointerException)))) {
					$G = 12;
					continue 1;
				}
				if ((((((($G >= 6)) && (($G < 10)))) && (($J__exception__) instanceof java_lang_NullPointerException)))) {
					$G = 12;
					continue 1;
				}
				if ((((((($G >= 8)) && (($G < 5)))) && (($J__exception__) instanceof java_lang_NullPointerException)))) {
					$G = 12;
					continue 1;
				}
				if ((((((($G >= 3)) && (($G < 9)))) && (($J__exception__) instanceof java_lang_ClassCastException)))) {
					$G = 13;
					continue 1;
				}
				if ((((((($G >= 6)) && (($G < 10)))) && (($J__exception__) instanceof java_lang_ClassCastException)))) {
					$G = 13;
					continue 1;
				}
				if ((((((($G >= 8)) && (($G < 5)))) && (($J__exception__) instanceof java_lang_ClassCastException)))) {
					$G = 13;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return false;
	}
	public function get_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0) {
		$lA3 = null;
		$lA2 = null;
		$lA2 = $this->entrySet__Ljava_util_Set_()->iterator__Ljava_util_Iterator_();
		if ((($p0 == null))) goto label_1;
		label_2:
		if (!($lA2->hasNext__Z())) goto label_3;
		$lA3 = N::checkcast($lA2->next__Ljava_lang_Object_(), "java_util_Map_Entry");
		if (!($p0->equals_Ljava_lang_Object__Z($lA3->getKey__Ljava_lang_Object_()))) goto label_5;
		return $lA3->getValue__Ljava_lang_Object_();
		label_5:
		goto label_2;
		label_1:
		if (!($lA2->hasNext__Z())) goto label_3;
		$lA3 = N::checkcast($lA2->next__Ljava_lang_Object_(), "java_util_Map_Entry");
		if ((($lA3->getKey__Ljava_lang_Object_() != null))) goto label_8;
		return $lA3->getValue__Ljava_lang_Object_();
		label_8:
		goto label_1;
		label_3:
		return null;
	}
	public function containsKey_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA2 = null;
		$lA2 = $this->entrySet__Ljava_util_Set_()->iterator__Ljava_util_Iterator_();
		if ((($p0 == null))) goto label_1;
		label_2:
		if (!($lA2->hasNext__Z())) goto label_3;
		if (!($p0->equals_Ljava_lang_Object__Z(N::checkcast($lA2->next__Ljava_lang_Object_(), "java_util_Map_Entry")->getKey__Ljava_lang_Object_()))) goto label_2;
		return true;
		label_1:
		if (!($lA2->hasNext__Z())) goto label_3;
		if (((N::checkcast($lA2->next__Ljava_lang_Object_(), "java_util_Map_Entry")->getKey__Ljava_lang_Object_() != null))) goto label_1;
		return true;
		label_3:
		return false;
	}
	public function hashCode__I() {
		$lI1 = 0;
		$lA2 = null;
		$lI1 = 0;
		$lA2 = $this->entrySet__Ljava_util_Set_()->iterator__Ljava_util_Iterator_();
		label_1:
		if (!($lA2->hasNext__Z())) goto label_2;
		$lI1 = ((int)(($lI1 + N::checkcast($lA2->next__Ljava_lang_Object_(), "java_util_Map_Entry")->hashCode__I())));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0, ?java_lang_Object $p1) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_UnsupportedOperationException()));
		$fA0 = $tA0;
		($tA0)->java_lang_UnsupportedOperationException_init___V();
		throw new WrappedThrowable($fA0);
	}
	public function __construct($CLASS_ID = 818) {
		parent::__construct($CLASS_ID);
		$this->_valuesCollection = null;
		$this->_keySet = null;
	}
	static public function SI() {
	}
}
class java_util_Collections_EmptyMap extends java_util_AbstractMap implements java_io_Serializable {

	public function entrySet__Ljava_util_Set_() {
		return java_util_Collections::$_EMPTY_SET;
	}
	public function get_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0) {
		return null;
	}
	public function containsKey_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		return false;
	}
	public function java_util_Collections_EmptyMap_init__Ljava_util_Collections_1__V(?java_util_Collections_1 $p0) {
		$this->java_util_Collections_EmptyMap_init___V();
		return $this;
		return $this;
	}
	public function java_util_Collections_EmptyMap_init___V() {
		($this)->java_util_AbstractMap_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 833) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_util_Enumeration {

}
class java_util_Enumeration_IFields {

	static public function SI() {
	}
}
class java_util_Collections_2 extends java_lang_Object implements java_util_Enumeration {

	public function java_util_Collections_2_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 831) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_util_List extends java_util_Collection {

	public function size__I();
	public function isEmpty__Z();
	public function iterator__Ljava_util_Iterator_();
	public function listIterator_I_Ljava_util_ListIterator_(int $p0);
	public function listIterator__Ljava_util_ListIterator_();
	public function get_I_Ljava_lang_Object_(int $p0);
	public function add_ILjava_lang_Object__V(int $p0, ?java_lang_Object $p1);
	public function add_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function toArray__Ljava_lang_Object___Ljava_lang_Object_(?JA_L $p0);
	public function hashCode__I();
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function contains_Ljava_lang_Object__Z(?java_lang_Object $p0);
	public function indexOf_Ljava_lang_Object__I(?java_lang_Object $p0);
	public function containsAll_Ljava_util_Collection__Z(?java_util_Collection $p0);
}
class java_util_List_IFields {

	static public function SI() {
	}
}
abstract class java_util_AbstractList extends java_util_AbstractCollection implements java_util_List {

	public $_modCount = 0;
	public function java_util_AbstractList_init___V() {
		($this)->java_util_AbstractCollection_init___V();
		return $this;
		return $this;
	}
	public function iterator__Ljava_util_Iterator_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_util_AbstractList_SimpleListIterator()));
		$fA0 = $tA0;
		($tA0)->java_util_AbstractList_SimpleListIterator_init__Ljava_util_AbstractList__V($this);
		return ($fA0);
	}
	public function listIterator__Ljava_util_ListIterator_() {
		return $this->listIterator_I_Ljava_util_ListIterator_(0);
	}
	public function listIterator_I_Ljava_util_ListIterator_(int $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_util_AbstractList_FullListIterator()));
		$fA0 = $tA0;
		($tA0)->java_util_AbstractList_FullListIterator_init__Ljava_util_AbstractList_I_V($this, $p0);
		return ($fA0);
	}
	public function get_I_Ljava_lang_Object_(int $p0) {
		throw new Exception("Missing body java.util.AbstractList.get(I)Ljava/lang/Object;");
	}
	public function add_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$this->add_ILjava_lang_Object__V($this->size__I(), $p0);
		return true;
	}
	public function add_ILjava_lang_Object__V(int $p0, ?java_lang_Object $p1) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_UnsupportedOperationException()));
		$fA0 = $tA0;
		($tA0)->java_lang_UnsupportedOperationException_init___V();
		throw new WrappedThrowable($fA0);
	}
	public function hashCode__I() {
		$lA3 = null;
		$fI0 = 0;
		$fI1 = 0;
		$lI1 = 0;
		$lA2 = null;
		$lI1 = 1;
		$lA2 = $this->iterator__Ljava_util_Iterator_();
		label_1:
		if (!($lA2->hasNext__Z())) goto label_2;
		$lA3 = $lA2->next__Ljava_lang_Object_();
		$fI0 = ((int)(N::imul(31, $lI1)));
		if ((($lA3 != null))) goto label_4;
		$fI1 = 0;
		goto label_5;
		label_4:
		$fI1 = $lA3->hashCode__I();
		label_5:
		$lI1 = ((int)(($fI0 + $fI1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA5 = null;
		$lA6 = null;
		$lA2 = null;
		$lA3 = null;
		$lA4 = null;
		if (((($this) != $p0))) goto label_1;
		return true;
		label_1:
		if (!((($p0) instanceof java_util_List))) goto label_3;
		$lA2 = N::checkcast($p0, "java_util_List");
		if ((($lA2->size__I() == $this->size__I()))) goto label_5;
		return false;
		label_5:
		$lA3 = $this->iterator__Ljava_util_Iterator_();
		$lA4 = $lA2->iterator__Ljava_util_Iterator_();
		label_7:
		if (!($lA3->hasNext__Z())) goto label_8;
		$lA5 = $lA3->next__Ljava_lang_Object_();
		$lA6 = $lA4->next__Ljava_lang_Object_();
		if ((($lA5 != null))) goto label_10;
		if ((($lA6 != null))) goto label_11;
		goto label_12;
		label_10:
		if ($lA5->equals_Ljava_lang_Object__Z($lA6)) goto label_12;
		label_11:
		return false;
		label_12:
		goto label_7;
		label_8:
		return true;
		label_3:
		return false;
	}
	public function indexOf_Ljava_lang_Object__I(?java_lang_Object $p0) {
		$lA2 = null;
		$lA2 = $this->listIterator__Ljava_util_ListIterator_();
		if ((($p0 == null))) goto label_1;
		label_2:
		if (!($lA2->hasNext__Z())) goto label_3;
		if (!($p0->equals_Ljava_lang_Object__Z($lA2->next__Ljava_lang_Object_()))) goto label_2;
		return $lA2->previousIndex__I();
		label_1:
		if (!($lA2->hasNext__Z())) goto label_3;
		if ((($lA2->next__Ljava_lang_Object_() != null))) goto label_1;
		return $lA2->previousIndex__I();
		label_3:
		return -1;
	}
	public function __construct($CLASS_ID = 723) {
		parent::__construct($CLASS_ID);
		$this->_modCount = 0;
	}
	static public function SI() {
	}
}
interface java_util_RandomAccess {

}
class java_util_RandomAccess_IFields {

	static public function SI() {
	}
}
class java_util_Collections_EmptyList extends java_util_AbstractList implements java_util_RandomAccess, java_io_Serializable {

	public function get_I_Ljava_lang_Object_(int $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_IndexOutOfBoundsException()));
		$fA0 = $tA0;
		($tA0)->java_lang_IndexOutOfBoundsException_init___V();
		throw new WrappedThrowable($fA0);
	}
	public function size__I() {
		return 0;
	}
	public function contains_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		return false;
	}
	public function java_util_Collections_EmptyList_init__Ljava_util_Collections_1__V(?java_util_Collections_1 $p0) {
		$this->java_util_Collections_EmptyList_init___V();
		return $this;
		return $this;
	}
	public function java_util_Collections_EmptyList_init___V() {
		($this)->java_util_AbstractList_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 830) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_Collections_1 extends java_lang_Object implements java_util_Iterator {

	public function java_util_Collections_1_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function next__Ljava_lang_Object_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_util_NoSuchElementException()));
		$fA0 = $tA0;
		($tA0)->java_util_NoSuchElementException_init___V();
		throw new WrappedThrowable($fA0);
	}
	public function hasNext__Z() {
		return false;
	}
	public function __construct($CLASS_ID = 829) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_Collections extends java_lang_Object {

	public static $_EMPTY_SET = null;
	public static $_EMPTY_ITERATOR = null;
	public static $_EMPTY_ENUMERATION = null;
	public static $_EMPTY_MAP = null;
	public static $_EMPTY_LIST = null;
	public function java_util_Collections_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function java_util_Collections_clinit___V() {
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		$tA3 = null;
		$tA4 = null;
		$tA0 = ((new java_util_Collections_1()));
		$fA0 = $tA0;
		($tA0)->java_util_Collections_1_init___V();
		java_util_Collections::$_EMPTY_ITERATOR = ($fA0);
		$tA1 = ((new java_util_Collections_2()));
		$fA0 = $tA1;
		($tA1)->java_util_Collections_2_init___V();
		java_util_Collections::$_EMPTY_ENUMERATION = ($fA0);
		$tA2 = ((new java_util_Collections_EmptyList()));
		$fA0 = $tA2;
		($tA2)->java_util_Collections_EmptyList_init__Ljava_util_Collections_1__V(null);
		java_util_Collections::$_EMPTY_LIST = ($fA0);
		$tA3 = ((new java_util_Collections_EmptySet()));
		$fA0 = $tA3;
		($tA3)->java_util_Collections_EmptySet_init__Ljava_util_Collections_1__V(null);
		java_util_Collections::$_EMPTY_SET = ($fA0);
		$tA4 = ((new java_util_Collections_EmptyMap()));
		$fA0 = $tA4;
		($tA4)->java_util_Collections_EmptyMap_init__Ljava_util_Collections_1__V(null);
		java_util_Collections::$_EMPTY_MAP = ($fA0);
		return;
	}
	public static function access_000__Ljava_util_Iterator_() {
		return java_util_Collections::$_EMPTY_ITERATOR;
	}
	public static function roundUpToPowerOfTwo_I_I(int $p0) {
		$lI0 = 0;
		$lI0 = $p0;
		$lI0 = ((int)(($lI0 + -1)));
		$lI0 = ((int)(($lI0 | ((int)(N::iushr($lI0, 1))))));
		$lI0 = ((int)(($lI0 | ((int)(N::iushr($lI0, 2))))));
		$lI0 = ((int)(($lI0 | ((int)(N::iushr($lI0, 4))))));
		$lI0 = ((int)(($lI0 | ((int)(N::iushr($lI0, 8))))));
		$lI0 = ((int)(($lI0 | ((int)(N::iushr($lI0, 16))))));
		return ((int)(($lI0 + 1)));
	}
	public static function secondaryHash_Ljava_lang_Object__I(?java_lang_Object $p0) {
		return java_util_Collections::secondaryHash_I_I($p0->hashCode__I());
	}
	public static function secondaryHash_I_I(int $p0) {
		$lI0 = 0;
		$lI0 = $p0;
		$lI0 = ((int)(($lI0 + ((int)((((int)(N::ishl($lI0, 15))) ^ -12931))))));
		$lI0 = ((int)(($lI0 ^ ((int)(N::iushr($lI0, 10))))));
		$lI0 = ((int)(($lI0 + ((int)(N::ishl($lI0, 3))))));
		$lI0 = ((int)(($lI0 ^ ((int)(N::iushr($lI0, 6))))));
		$lI0 = ((int)(($lI0 + ((int)((((int)(N::ishl($lI0, 2))) + ((int)(N::ishl($lI0, 14)))))))));
		return ((int)(($lI0 ^ ((int)(N::iushr($lI0, 16))))));
	}
	public function __construct($CLASS_ID = 828) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		java_util_Collections::$_EMPTY_SET = null;
		java_util_Collections::$_EMPTY_ITERATOR = null;
		java_util_Collections::$_EMPTY_ENUMERATION = null;
		java_util_Collections::$_EMPTY_MAP = null;
		java_util_Collections::$_EMPTY_LIST = null;
		java_util_Collections::java_util_Collections_clinit___V();
	}
}
class java_lang_Error extends java_lang_Throwable {

	public function java_lang_Error_init__Ljava_lang_Throwable__V(?java_lang_Throwable $p0) {
		($this)->java_lang_Throwable_init__Ljava_lang_Throwable__V($p0);
		return $this;
		return $this;
	}
	public function java_lang_Error_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_Throwable_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function java_lang_Error_init__Ljava_lang_String_Ljava_lang_Throwable__V(?java_lang_String $p0, ?java_lang_Throwable $p1) {
		($this)->java_lang_Throwable_init__Ljava_lang_String_Ljava_lang_Throwable__V($p0, $p1);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 715) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_AssertionError extends java_lang_Error {

	public function java_lang_AssertionError_init__Ljava_lang_Object__V(?java_lang_Object $p0) {
		($this)->java_lang_Error_init__Ljava_lang_String__V(java_lang_String::valueOf_Ljava_lang_Object__Ljava_lang_String_($p0));
		if (!((($p0) instanceof java_lang_Throwable))) goto label_1;
		$this->initCause_Ljava_lang_Throwable__Ljava_lang_Throwable_(N::checkcast($p0, "java_lang_Throwable"));
		label_1:
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 827) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_util_HashMap_HashIterator extends java_lang_Object {

	public $_nextEntry = null;
	public $_nextIndex = 0;
	public $_this_0 = null;
	public $_expectedModCount = 0;
	public $_lastEntryReturned = null;
	public function java_util_HashMap_HashIterator_init__Ljava_util_HashMap__V(?java_util_HashMap $p0) {
		$lA2 = null;
		$lA3 = null;
		$fI1 = 0;
		$fA0 = null;
		$fA1 = null;
		$tI1 = 0;
		$tA2 = null;
		$this->_this_0 = $p0;
		($this)->java_lang_Object_init___V();
		$this->_nextEntry = $this->_this_0->_entryForNullKey;
		$this->_expectedModCount = $this->_this_0->_modCount;
		if ((($this->_nextEntry != null))) goto label_1;
		$lA2 = ($p0->_table);
		$lA3 = null;
		label_3:
		if ((($lA3 != null))) goto label_4;
		if ((($this->_nextIndex >= ($lA2)->length))) goto label_4;
		$fA0 = $lA2;
		$fA1 = ($this);
		$tA2 = $fA1;
		$tI1 = $this->_nextIndex;
		$fI1 = $tI1;
		($tA2)->_nextIndex = ((int)(($tI1 + 1)));
		$lA3 = (($fA0)->get($fI1));
		goto label_3;
		label_4:
		$this->_nextEntry = ($lA3);
		label_1:
		return $this;
		return $this;
	}
	public function hasNext__Z() {
		$fI0 = 0;
		if ((($this->_nextEntry == null))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function nextEntry__Ljava_util_HashMap_HashMapEntry_() {
		$lA1 = null;
		$lA2 = null;
		$lA3 = null;
		$fI1 = 0;
		$fA0 = null;
		$fA1 = null;
		$tI3 = 0;
		$tA0 = null;
		$tA1 = null;
		$tA4 = null;
		$tA6 = null;
		$tA5 = null;
		if ((($this->_this_0->_modCount == $this->_expectedModCount))) goto label_1;
		$tA0 = ((new java_util_ConcurrentModificationException()));
		$fA0 = $tA0;
		($tA0)->java_util_ConcurrentModificationException_init___V();
		throw new WrappedThrowable($fA0);
		label_1:
		if ((($this->_nextEntry != null))) goto label_3;
		$tA1 = ((new java_util_NoSuchElementException()));
		$fA0 = $tA1;
		($tA1)->java_util_NoSuchElementException_init___V();
		throw new WrappedThrowable($fA0);
		label_3:
		$lA1 = ($this->_nextEntry);
		$lA2 = ($this->_this_0->_table);
		$lA3 = (($lA1)->_next);
		label_5:
		if ((($lA3 != null))) goto label_6;
		if ((($this->_nextIndex >= ($lA2)->length))) goto label_6;
		$fA0 = $lA2;
		$fA1 = ($this);
		$tA4 = $fA1;
		$tI3 = $this->_nextIndex;
		$fI1 = $tI3;
		($tA4)->_nextIndex = ((int)(($tI3 + 1)));
		$lA3 = (($fA0)->get($fI1));
		goto label_5;
		label_6:
		$this->_nextEntry = ($lA3);
		$fA0 = ($this);
		$tA6 = $fA0;
		$tA5 = $lA1;
		$fA0 = $tA5;
		($tA6)->_lastEntryReturned = ($tA5);
		return ($fA0);
	}
	public function __construct($CLASS_ID = 826) {
		parent::__construct($CLASS_ID);
		$this->_nextEntry = null;
		$this->_nextIndex = 0;
		$this->_this_0 = null;
		$this->_expectedModCount = 0;
		$this->_lastEntryReturned = null;
	}
	static public function SI() {
	}
}
class java_util_HashMap_EntryIterator extends java_util_HashMap_HashIterator implements java_util_Iterator {

	public $_this_0_ = null;
	public function java_util_HashMap_EntryIterator_init__Ljava_util_HashMap__V(?java_util_HashMap $p0) {
		$this->_this_0_ = $p0;
		($this)->java_util_HashMap_HashIterator_init__Ljava_util_HashMap__V($p0);
		return $this;
		return $this;
	}
	public function next__Ljava_lang_Object_() {
		return ($this->next__Ljava_util_Map_Entry_());
	}
	public function next__Ljava_util_Map_Entry_() {
		return ($this->nextEntry__Ljava_util_HashMap_HashMapEntry_());
	}
	public function java_util_HashMap_EntryIterator_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V(?java_util_HashMap $p0, ?java_util_HashMap_1 $p1) {
		$this->java_util_HashMap_EntryIterator_init__Ljava_util_HashMap__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 825) {
		parent::__construct($CLASS_ID);
		$this->_this_0_ = null;
	}
	static public function SI() {
	}
}
class java_util_HashMap_EntrySet extends java_util_AbstractSet {

	public $_this_0 = null;
	public function contains_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA2 = null;
		if ((($p0) instanceof java_util_Map_Entry)) goto label_1;
		return false;
		label_1:
		$lA2 = N::checkcast($p0, "java_util_Map_Entry");
		return java_util_HashMap::access_600_Ljava_util_HashMap_Ljava_lang_Object_Ljava_lang_Object__Z($this->_this_0, $lA2->getKey__Ljava_lang_Object_(), $lA2->getValue__Ljava_lang_Object_());
	}
	public function size__I() {
		return $this->_this_0->_size;
	}
	public function isEmpty__Z() {
		$fI0 = 0;
		if ((($this->_this_0->_size != 0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function iterator__Ljava_util_Iterator_() {
		return $this->_this_0->newEntryIterator__Ljava_util_Iterator_();
	}
	public function java_util_HashMap_EntrySet_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V(?java_util_HashMap $p0, ?java_util_HashMap_1 $p1) {
		$this->java_util_HashMap_EntrySet_init__Ljava_util_HashMap__V($p0);
		return $this;
		return $this;
	}
	public function java_util_HashMap_EntrySet_init__Ljava_util_HashMap__V(?java_util_HashMap $p0) {
		$this->_this_0 = $p0;
		($this)->java_util_AbstractSet_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 823) {
		parent::__construct($CLASS_ID);
		$this->_this_0 = null;
	}
	static public function SI() {
	}
}
class java_util_HashMap_1 extends java_lang_Object {

	public function __construct($CLASS_ID = 822) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_HashMap_HashMapEntry extends java_lang_Object implements java_util_Map_Entry {

	public $_key = null;
	public $_next = null;
	public $_value = null;
	public $_hash = 0;
	public function java_util_HashMap_HashMapEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__V(?java_lang_Object $p0, ?java_lang_Object $p1, int $p2, ?java_util_HashMap_HashMapEntry $p3) {
		($this)->java_lang_Object_init___V();
		$this->_key = $p0;
		$this->_value = $p1;
		$this->_hash = $p2;
		$this->_next = $p3;
		return $this;
		return $this;
	}
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_Object__Ljava_lang_StringBuilder_($this->_key)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_7)->append_Ljava_lang_Object__Ljava_lang_StringBuilder_($this->_value)->toString__Ljava_lang_String_();
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		$lA2 = null;
		if ((($p0) instanceof java_util_Map_Entry)) goto label_1;
		return false;
		label_1:
		$lA2 = N::checkcast($p0, "java_util_Map_Entry");
		if (!(java_util_Objects::equals_Ljava_lang_Object_Ljava_lang_Object__Z($lA2->getKey__Ljava_lang_Object_(), $this->_key))) goto label_3;
		if (!(java_util_Objects::equals_Ljava_lang_Object_Ljava_lang_Object__Z($lA2->getValue__Ljava_lang_Object_(), $this->_value))) goto label_3;
		$fI0 = 1;
		goto label_4;
		label_3:
		$fI0 = 0;
		label_4:
		return (($fI0)!=0);
	}
	public function getKey__Ljava_lang_Object_() {
		return $this->_key;
	}
	public function getValue__Ljava_lang_Object_() {
		return $this->_value;
	}
	public function hashCode__I() {
		$fI0 = 0;
		$fI1 = 0;
		$fA1 = null;
		if ((($this->_key != null))) goto label_1;
		$fI0 = 0;
		goto label_2;
		label_1:
		$fI0 = $this->_key->hashCode__I();
		label_2:
		if ((($this->_value != null))) goto label_3;
		$fI1 = 0;
		goto label_4;
		label_3:
		$fA1 = $this->_value;
		$fI1 = $fA1->hashCode__I();
		label_4:
		$fI0 = ((int)(($fI0 ^ $fI1)));
		return $fI0;
	}
	public function __construct($CLASS_ID = 820) {
		parent::__construct($CLASS_ID);
		$this->_key = null;
		$this->_next = null;
		$this->_value = null;
		$this->_hash = 0;
	}
	static public function SI() {
	}
}
class java_util_HashMap extends java_util_AbstractMap implements java_lang_Cloneable, java_io_Serializable {

	public $_table = null;
	public $_threshold = 0;
	public static $_EMPTY_TABLE = null;
	public $_entryForNullKey = null;
	public $_size = 0;
	public $_modCount = 0;
	public $_entrySet = null;
	public $_values = null;
	public $_keySet_ = null;
	public function java_util_HashMap_init___V() {
		($this)->java_util_AbstractMap_init___V();
		$this->_table = N::checkcast(N::checkcast(java_util_HashMap::$_EMPTY_TABLE, "JA_L"), "JA_L");
		$this->_threshold = -1;
		return $this;
		return $this;
	}
	public function entrySet__Ljava_util_Set_() {
		$lA1 = null;
		$fA0 = null;
		$fA1 = null;
		$tA0 = null;
		$tA2 = null;
		$tA1 = null;
		$lA1 = ($this->_entrySet);
		if ((($lA1 == null))) goto label_1;
		$fA0 = $lA1;
		goto label_2;
		label_1:
		$fA0 = ($this);
		$tA0 = ((new java_util_HashMap_EntrySet()));
		$fA1 = $tA0;
		($tA0)->java_util_HashMap_EntrySet_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V($this, null);
		$tA2 = $fA0;
		$tA1 = $fA1;
		$fA0 = $tA1;
		($tA2)->_entrySet = ($tA1);
		label_2:
		return ($fA0);
	}
	public static function access_600_Ljava_util_HashMap_Ljava_lang_Object_Ljava_lang_Object__Z(?java_util_HashMap $p0, ?java_lang_Object $p1, ?java_lang_Object $p2) {
		return $p0->containsMapping_Ljava_lang_Object_Ljava_lang_Object__Z($p1, $p2);
	}
	public function containsMapping_Ljava_lang_Object_Ljava_lang_Object__Z(?java_lang_Object $p0, ?java_lang_Object $p1) {
		$lA3 = null;
		$fI0 = 0;
		$lA4 = null;
		$lA6 = null;
		$lI3 = 0;
		$lI5 = 0;
		if ((($p0 != null))) goto label_1;
		$lA3 = ($this->_entryForNullKey);
		if ((($lA3 == null))) goto label_3;
		if (!(java_util_Objects::equals_Ljava_lang_Object_Ljava_lang_Object__Z($p1, ($lA3)->_value))) goto label_3;
		$fI0 = 1;
		goto label_4;
		label_3:
		$fI0 = 0;
		label_4:
		return (($fI0)!=0);
		label_1:
		$lI3 = java_util_HashMap::secondaryHash_Ljava_lang_Object__I($p0);
		$lA4 = ($this->_table);
		$lI5 = ((int)(($lI3 & ((int)((($lA4)->length - 1))))));
		$lA6 = (($lA4)->get($lI5));
		label_5:
		if ((($lA6 == null))) goto label_6;
		if (((($lA6)->_hash != $lI3))) goto label_8;
		if (!($p0->equals_Ljava_lang_Object__Z(($lA6)->_key))) goto label_8;
		return java_util_Objects::equals_Ljava_lang_Object_Ljava_lang_Object__Z($p1, ($lA6)->_value);
		label_8:
		$lA6 = (($lA6)->_next);
		goto label_5;
		label_6:
		return false;
	}
	public static function secondaryHash_Ljava_lang_Object__I(?java_lang_Object $p0) {
		$lI1 = 0;
		$lI1 = $p0->hashCode__I();
		$lI1 = ((int)(($lI1 ^ ((int)((((int)(N::iushr($lI1, 20))) ^ ((int)(N::iushr($lI1, 12)))))))));
		$lI1 = ((int)(($lI1 ^ ((int)((((int)(N::iushr($lI1, 7))) ^ ((int)(N::iushr($lI1, 4)))))))));
		return $lI1;
	}
	public function newEntryIterator__Ljava_util_Iterator_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_util_HashMap_EntryIterator()));
		$fA0 = $tA0;
		($tA0)->java_util_HashMap_EntryIterator_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V($this, null);
		return ($fA0);
	}
	public function size__I() {
		return $this->_size;
	}
	public function isEmpty__Z() {
		$fI0 = 0;
		if ((($this->_size != 0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function clone__Ljava_lang_Object_() {
		$G = 0;
		$lA1 = null;
		$lA2 = null;
		$fA0 = null;
		$tA1 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$G = 1;
							continue 2;
						case 1:
							$lA1 = (N::checkcast(parent::clone__Ljava_lang_Object_(), "java_util_HashMap"));
							$G = 2;
							continue 2;
						case 2:
							$G = 3;
							continue 2;
						case 4:
							$fA0 = ($J__exception__);
							$lA2 = $fA0;
							$tA1 = ((new java_lang_AssertionError()));
							$fA0 = $tA1;
							($tA1)->java_lang_AssertionError_init__Ljava_lang_Object__V($lA2);
							throw new WrappedThrowable($fA0);
							$G = 3;
							continue 2;
						case 3:
							($lA1)->makeTable_I__Ljava_util_HashMap_HashMapEntry_(($this->_table)->length);
							($lA1)->_entryForNullKey = null;
							($lA1)->_size = 0;
							($lA1)->_keySet_ = null;
							($lA1)->_entrySet = null;
							($lA1)->_values = null;
							($lA1)->init__V();
							($lA1)->constructorPutAll_Ljava_util_Map__V(($this));
							return $lA1;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_CloneNotSupportedException)))) {
					$G = 4;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public function init__V() {
		return;
	}
	public function makeTable_I__Ljava_util_HashMap_HashMapEntry_(int $p0) {
		$lA2 = null;
		$lA2 = N::checkcast(new JA_L($p0, "[Ljava.util.HashMap\$HashMapEntry;"), "JA_L");
		$this->_table = $lA2;
		$this->_threshold = ((int)((((int)(N::ishr($p0, 1))) + ((int)(N::ishr($p0, 2))))));
		return $lA2;
	}
	public function constructorPutAll_Ljava_util_Map__V(?java_util_Map $p0) {
		$lA3 = null;
		$lA2 = null;
		if ((($this->_table != java_util_HashMap::$_EMPTY_TABLE))) goto label_1;
		$this->doubleCapacity___Ljava_util_HashMap_HashMapEntry_();
		label_1:
		$lA2 = $p0->entrySet__Ljava_util_Set_()->iterator__Ljava_util_Iterator_();
		label_3:
		if (!($lA2->hasNext__Z())) goto label_4;
		$lA3 = N::checkcast($lA2->next__Ljava_lang_Object_(), "java_util_Map_Entry");
		$this->constructorPut_Ljava_lang_Object_Ljava_lang_Object__V($lA3->getKey__Ljava_lang_Object_(), $lA3->getValue__Ljava_lang_Object_());
		goto label_3;
		label_4:
		return;
	}
	public function constructorPut_Ljava_lang_Object_Ljava_lang_Object__V(?java_lang_Object $p0, ?java_lang_Object $p1) {
		$lA3 = null;
		$lA4 = null;
		$lA6 = null;
		$lA7 = null;
		$lI3 = 0;
		$lI5 = 0;
		if ((($p0 != null))) goto label_1;
		$lA3 = ($this->_entryForNullKey);
		if ((($lA3 != null))) goto label_3;
		$this->_entryForNullKey = $this->constructorNewEntry_Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__Ljava_util_HashMap_HashMapEntry_(null, $p1, 0, null);
		$this->_size = ((int)(($this->_size + 1)));
		goto label_5;
		label_3:
		($lA3)->_value = $p1;
		label_5:
		return;
		label_1:
		$lI3 = java_util_HashMap::secondaryHash_Ljava_lang_Object__I($p0);
		$lA4 = ($this->_table);
		$lI5 = ((int)(($lI3 & ((int)((($lA4)->length - 1))))));
		$lA6 = (($lA4)->get($lI5));
		$lA7 = $lA6;
		label_6:
		if ((($lA7 == null))) goto label_7;
		if (((($lA7)->_hash != $lI3))) goto label_9;
		if (!($p0->equals_Ljava_lang_Object__Z(($lA7)->_key))) goto label_9;
		($lA7)->_value = $p1;
		return;
		label_9:
		$lA7 = (($lA7)->_next);
		goto label_6;
		label_7:
		($lA4)->set($lI5, ($this->constructorNewEntry_Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__Ljava_util_HashMap_HashMapEntry_($p0, $p1, $lI3, ($lA6))));
		$this->_size = ((int)(($this->_size + 1)));
		return;
	}
	public function constructorNewEntry_Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__Ljava_util_HashMap_HashMapEntry_(?java_lang_Object $p0, ?java_lang_Object $p1, int $p2, ?java_util_HashMap_HashMapEntry $p3) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_util_HashMap_HashMapEntry()));
		$fA0 = $tA0;
		($tA0)->java_util_HashMap_HashMapEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__V($p0, $p1, $p2, $p3);
		return ($fA0);
	}
	public function doubleCapacity___Ljava_util_HashMap_HashMapEntry_() {
		$lA1 = null;
		$lA4 = null;
		$lA6 = null;
		$lA8 = null;
		$lA9 = null;
		$lI2 = 0;
		$lI3 = 0;
		$lI5 = 0;
		$lI7 = 0;
		$lI10 = 0;
		$lA1 = ($this->_table);
		$lI2 = ($lA1)->length;
		if ((($lI2 != 1073741824))) goto label_1;
		return ($lA1);
		label_1:
		$lI3 = ((int)(N::imul($lI2, 2)));
		$lA4 = ($this->makeTable_I__Ljava_util_HashMap_HashMapEntry_($lI3));
		if ((($this->_size != 0))) goto label_3;
		return ($lA4);
		label_3:
		$lI5 = 0;
		label_5:
		if ((($lI5 >= $lI2))) goto label_6;
		$lA6 = (($lA1)->get($lI5));
		if ((($lA6 != null))) goto label_8;
		goto label_10;
		label_8:
		$lI7 = ((int)((($lA6)->_hash & $lI2)));
		$lA8 = null;
		($lA4)->set(((int)(($lI5 | $lI7))), $lA6);
		$lA9 = (($lA6)->_next);
		label_11:
		if ((($lA9 == null))) goto label_12;
		$lI10 = ((int)((($lA9)->_hash & $lI2)));
		if ((($lI10 == $lI7))) goto label_14;
		if ((($lA8 != null))) goto label_16;
		($lA4)->set(((int)(($lI5 | $lI10))), $lA9);
		goto label_18;
		label_16:
		($lA8)->_next = ($lA9);
		label_18:
		$lA8 = $lA6;
		$lI7 = $lI10;
		label_14:
		$lA6 = $lA9;
		$lA9 = (($lA9)->_next);
		goto label_11;
		label_12:
		if ((($lA8 == null))) goto label_10;
		($lA8)->_next = null;
		label_10:
		$lI5 = ((int)(($lI5 + 1)));
		goto label_5;
		label_6:
		return ($lA4);
	}
	public function get_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0) {
		$lA2 = null;
		$lA3 = null;
		$lA4 = null;
		$lA5 = null;
		$lI2 = 0;
		$fA0 = null;
		if ((($p0 != null))) goto label_1;
		$lA2 = ($this->_entryForNullKey);
		if ((($lA2 != null))) goto label_3;
		$fA0 = null;
		goto label_4;
		label_3:
		$fA0 = ($lA2)->_value;
		label_4:
		return $fA0;
		label_1:
		$lI2 = $p0->hashCode__I();
		$lI2 = ((int)(($lI2 ^ ((int)((((int)(N::iushr($lI2, 20))) ^ ((int)(N::iushr($lI2, 12)))))))));
		$lI2 = ((int)(($lI2 ^ ((int)((((int)(N::iushr($lI2, 7))) ^ ((int)(N::iushr($lI2, 4)))))))));
		$lA3 = ($this->_table);
		$lA4 = (($lA3)->get(((int)(($lI2 & ((int)((($lA3)->length - 1))))))));
		label_5:
		if ((($lA4 == null))) goto label_6;
		$lA5 = ($lA4)->_key;
		if ((($lA5 == $p0))) goto label_8;
		if (((($lA4)->_hash != $lI2))) goto label_9;
		if (!($p0->equals_Ljava_lang_Object__Z($lA5))) goto label_9;
		label_8:
		return ($lA4)->_value;
		label_9:
		$lA4 = (($lA4)->_next);
		goto label_5;
		label_6:
		return null;
	}
	public function containsKey_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		$lA3 = null;
		$lA4 = null;
		$lA5 = null;
		$lI2 = 0;
		if ((($p0 != null))) goto label_1;
		if ((($this->_entryForNullKey == null))) goto label_3;
		$fI0 = 1;
		goto label_4;
		label_3:
		$fI0 = 0;
		label_4:
		return (($fI0)!=0);
		label_1:
		$lI2 = $p0->hashCode__I();
		$lI2 = ((int)(($lI2 ^ ((int)((((int)(N::iushr($lI2, 20))) ^ ((int)(N::iushr($lI2, 12)))))))));
		$lI2 = ((int)(($lI2 ^ ((int)((((int)(N::iushr($lI2, 7))) ^ ((int)(N::iushr($lI2, 4)))))))));
		$lA3 = ($this->_table);
		$lA4 = (($lA3)->get(((int)(($lI2 & ((int)((($lA3)->length - 1))))))));
		label_5:
		if ((($lA4 == null))) goto label_6;
		$lA5 = ($lA4)->_key;
		if ((($lA5 == $p0))) goto label_8;
		if (((($lA4)->_hash != $lI2))) goto label_9;
		if (!($p0->equals_Ljava_lang_Object__Z($lA5))) goto label_9;
		label_8:
		return true;
		label_9:
		$lA4 = (($lA4)->_next);
		goto label_5;
		label_6:
		return false;
	}
	public static function java_util_HashMap_clinit___V() {
		java_util_HashMap::$_EMPTY_TABLE = (new JA_L(2, "[Ljava.util.HashMap\$HashMapEntry;"));
		return;
	}
	public function put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0, ?java_lang_Object $p1) {
		$lA4 = null;
		$lA6 = null;
		$lA7 = null;
		$fI0 = 0;
		$lI3 = 0;
		$lI5 = 0;
		$fA0 = null;
		$tI2 = 0;
		$tA3 = null;
		if ((($p0 != null))) goto label_1;
		return $this->putValueForNullKey_Ljava_lang_Object__Ljava_lang_Object_($p1);
		label_1:
		$lI3 = java_util_HashMap::secondaryHash_Ljava_lang_Object__I($p0);
		$lA4 = ($this->_table);
		$lI5 = ((int)(($lI3 & ((int)((($lA4)->length - 1))))));
		$lA6 = (($lA4)->get($lI5));
		label_3:
		if ((($lA6 == null))) goto label_4;
		if (((($lA6)->_hash != $lI3))) goto label_6;
		if (!($p0->equals_Ljava_lang_Object__Z(($lA6)->_key))) goto label_6;
		$this->preModify_Ljava_util_HashMap_HashMapEntry__V(($lA6));
		$lA7 = ($lA6)->_value;
		($lA6)->_value = $p1;
		return $lA7;
		label_6:
		$lA6 = (($lA6)->_next);
		goto label_3;
		label_4:
		$this->_modCount = ((int)(($this->_modCount + 1)));
		$fA0 = ($this);
		$tA3 = $fA0;
		$tI2 = $this->_size;
		$fI0 = $tI2;
		($tA3)->_size = ((int)(($tI2 + 1)));
		if ((($fI0 <= $this->_threshold))) goto label_8;
		$lA4 = ($this->doubleCapacity___Ljava_util_HashMap_HashMapEntry_());
		$lI5 = ((int)(($lI3 & ((int)((($lA4)->length - 1))))));
		label_8:
		$this->addNewEntry_Ljava_lang_Object_Ljava_lang_Object_II_V($p0, $p1, $lI3, $lI5);
		return null;
	}
	public function addNewEntry_Ljava_lang_Object_Ljava_lang_Object_II_V(?java_lang_Object $p0, ?java_lang_Object $p1, int $p2, int $p3) {
		$fI1 = 0;
		$fA0 = null;
		$fA2 = null;
		$tA0 = null;
		$fA0 = ($this->_table);
		$fI1 = $p3;
		$tA0 = ((new java_util_HashMap_HashMapEntry()));
		$fA2 = $tA0;
		($tA0)->java_util_HashMap_HashMapEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__V($p0, $p1, $p2, ((($this->_table)->get($p3))));
		($fA0)->set($fI1, $fA2);
		return;
	}
	public function preModify_Ljava_util_HashMap_HashMapEntry__V(?java_util_HashMap_HashMapEntry $p0) {
		return;
	}
	public function putValueForNullKey_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0) {
		$lA2 = null;
		$lA3 = null;
		$lA2 = ($this->_entryForNullKey);
		if ((($lA2 != null))) goto label_1;
		$this->addNewEntryForNullKey_Ljava_lang_Object__V($p0);
		$this->_size = ((int)(($this->_size + 1)));
		$this->_modCount = ((int)(($this->_modCount + 1)));
		return null;
		label_1:
		$this->preModify_Ljava_util_HashMap_HashMapEntry__V(($lA2));
		$lA3 = ($lA2)->_value;
		($lA2)->_value = $p0;
		return $lA3;
	}
	public function addNewEntryForNullKey_Ljava_lang_Object__V(?java_lang_Object $p0) {
		$fA1 = null;
		$tA0 = null;
		$tA0 = ((new java_util_HashMap_HashMapEntry()));
		$fA1 = $tA0;
		($tA0)->java_util_HashMap_HashMapEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__V(null, $p0, 0, null);
		$this->_entryForNullKey = ($fA1);
		return;
	}
	public function java_util_HashMap_init__I_V(int $p0) {
		$lI1 = 0;
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		$lA2 = null;
		$lI1 = $p0;
		($this)->java_util_AbstractMap_init___V();
		if ((($lI1 >= 0))) goto label_1;
		$tA0 = ((new java_lang_IllegalArgumentException()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_IllegalArgumentException_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_109)->append_I_Ljava_lang_StringBuilder_($lI1)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_1:
		if ((($lI1 != 0))) goto label_3;
		$lA2 = N::checkcast(N::checkcast(java_util_HashMap::$_EMPTY_TABLE, "JA_L"), "JA_L");
		$this->_table = $lA2;
		$this->_threshold = -1;
		return $this;
		label_3:
		if ((($lI1 >= 4))) goto label_5;
		$lI1 = 4;
		goto label_7;
		label_5:
		if ((($lI1 <= 1073741824))) goto label_8;
		$lI1 = 1073741824;
		goto label_7;
		label_8:
		$lI1 = java_util_Collections::roundUpToPowerOfTwo_I_I($lI1);
		label_7:
		$this->makeTable_I__Ljava_util_HashMap_HashMapEntry_($lI1);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 817) {
		parent::__construct($CLASS_ID);
		$this->_table = null;
		$this->_threshold = 0;
		$this->_entryForNullKey = null;
		$this->_size = 0;
		$this->_modCount = 0;
		$this->_entrySet = null;
		$this->_values = null;
		$this->_keySet_ = null;
	}
	static public function SI() {
		java_util_HashMap::$_EMPTY_TABLE = null;
		java_util_HashMap::java_util_HashMap_clinit___V();
	}
}
class com_jtransc_charset_charsets_JTranscCharsetIBM866 extends com_jtransc_charset_JTranscCharsetSingleByte {

	public function com_jtransc_charset_charsets_JTranscCharsetIBM866_init___V() {
		$fA1 = null;
		$tA0 = null;
		$tA0 = (new JA_L(5, "[Ljava.lang.String;"));
		$fA1 = $tA0;
		($tA0)->set(0, (Bootstrap::$STRINGLIT_110));
		($fA1)->set(1, (Bootstrap::$STRINGLIT_111));
		($fA1)->set(2, (Bootstrap::$STRINGLIT_112));
		($fA1)->set(3, (Bootstrap::$STRINGLIT_113));
		($fA1)->set(4, (Bootstrap::$STRINGLIT_114));
		($this)->com_jtransc_charset_JTranscCharsetSingleByte_init___Ljava_lang_String_Ljava_lang_String__V(($fA1), Bootstrap::$STRINGLIT_115);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 814) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_ServiceLoader extends java_lang_Object implements java_lang_Iterable {

	public $_service = null;
	public $_list = null;
	public function java_util_ServiceLoader_init__Ljava_lang_Class__V(?java_lang_Class $p0) {
		($this)->java_lang_Object_init___V();
		java_util_Objects::requireNonNull_Ljava_lang_Object__Ljava_lang_Object_(($p0));
		$this->_service = $p0;
		$this->_list = java_util_Arrays::asList__Ljava_lang_Object__Ljava_util_List_($this->getInstances_Ljava_lang_String___Ljava_lang_Object_($p0->getName__Ljava_lang_String_()));
		$this->reload__V();
		return $this;
		return $this;
	}
	public function getInstances_Ljava_lang_String___Ljava_lang_Object_(?java_lang_String $p0) {
		$_out = null;
		$_out = null;
		if (java_util_Objects::equals_Ljava_lang_Object_Ljava_lang_Object__Z($p0, Bootstrap::$STRINGLIT_116)) {
			$_out = new JA_L(6, "[Ljava.lang.Object;");
			$_out->set(0, (new com_jtransc_charset_charsets_JTranscCharsetUTF8())->com_jtransc_charset_charsets_JTranscCharsetUTF8_init___V());
			$_out->set(1, (new com_jtransc_charset_charsets_JTranscCharsetIBM866())->com_jtransc_charset_charsets_JTranscCharsetIBM866_init___V());
			$_out->set(2, (new com_jtransc_charset_charsets_JTranscCharsetLatin1())->com_jtransc_charset_charsets_JTranscCharsetLatin1_init___V());
			$_out->set(3, (new com_jtransc_charset_charsets_JTranscCharsetUSASCII())->com_jtransc_charset_charsets_JTranscCharsetUSASCII_init___V());
			$_out->set(4, (new com_jtransc_charset_charsets_JTranscCharsetUTF16LE())->com_jtransc_charset_charsets_JTranscCharsetUTF16LE_init___V());
			$_out->set(5, (new com_jtransc_charset_charsets_JTranscCharsetUTF16BE())->com_jtransc_charset_charsets_JTranscCharsetUTF16BE_init___V());
			return $_out;
		}
		if (java_util_Objects::equals_Ljava_lang_Object_Ljava_lang_Object__Z($p0, Bootstrap::$STRINGLIT_117)) {
			$_out = new JA_L(0, "[Ljava.lang.Object;");
			return $_out;
		}
		return new JA_L(0, "[Ljava.lang.Object;");
	}
	public function reload__V() {
		return;
	}
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_118)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->_service->getName__Ljava_lang_String_())->toString__Ljava_lang_String_();
	}
	public function iterator__Ljava_util_Iterator_() {
		return $this->_list->iterator__Ljava_util_Iterator_();
	}
	public static function load_Ljava_lang_Class__Ljava_util_ServiceLoader_(?java_lang_Class $p0) {
		return java_util_ServiceLoader::load_Ljava_lang_Class_Ljava_lang_ClassLoader__Ljava_util_ServiceLoader_($p0, java_lang_Thread::currentThread__Ljava_lang_Thread_()->getContextClassLoader__Ljava_lang_ClassLoader_());
	}
	public static function load_Ljava_lang_Class_Ljava_lang_ClassLoader__Ljava_util_ServiceLoader_(?java_lang_Class $p0, ?java_lang_ClassLoader $p1) {
		$lA1 = null;
		$fA0 = null;
		$tA0 = null;
		$lA1 = ($p1);
		if ((($lA1 != null))) goto label_1;
		$lA1 = (java_lang_ClassLoader::getSystemClassLoader__Ljava_lang_ClassLoader_());
		label_1:
		$tA0 = ((new java_util_ServiceLoader()));
		$fA0 = $tA0;
		($tA0)->java_util_ServiceLoader_init__Ljava_lang_Class__V($p0);
		return ($fA0);
	}
	public function __construct($CLASS_ID = 813) {
		parent::__construct($CLASS_ID);
		$this->_service = null;
		$this->_list = null;
	}
	static public function SI() {
	}
}
class java_lang_IllegalArgumentException extends java_lang_RuntimeException {

	public function java_lang_IllegalArgumentException_init___V() {
		($this)->java_lang_RuntimeException_init___V();
		return $this;
		return $this;
	}
	public function java_lang_IllegalArgumentException_init__Ljava_lang_Throwable__V(?java_lang_Throwable $p0) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_Throwable__V($p0);
		return $this;
		return $this;
	}
	public function java_lang_IllegalArgumentException_init__Ljava_lang_String_Ljava_lang_Throwable__V(?java_lang_String $p0, ?java_lang_Throwable $p1) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_String_Ljava_lang_Throwable__V($p0, $p1);
		return $this;
		return $this;
	}
	public function java_lang_IllegalArgumentException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 682) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_nio_charset_UnsupportedCharsetException extends java_lang_IllegalArgumentException {

	public $_charsetName = null;
	public function java_nio_charset_UnsupportedCharsetException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_IllegalArgumentException_init__Ljava_lang_String__V(java_lang_String::valueOf_Ljava_lang_Object__Ljava_lang_String_(($p0)));
		$this->_charsetName = $p0;
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 812) {
		parent::__construct($CLASS_ID);
		$this->_charsetName = null;
	}
	static public function SI() {
	}
}
class com_jtransc_charset_JTranscCharBuffer extends java_lang_Object {

	public $_buffer = null;
	public $_position = 0;
	public $_size = 0;
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_String()));
		$fA0 = $tA0;
		($tA0)->java_lang_String_init___CII_V($this->_buffer, 0, $this->_position);
		return ($fA0);
	}
	public function com_jtransc_charset_JTranscCharBuffer_init__I_V(int $p0) {
		$fI1 = 0;
		($this)->java_lang_Object_init___V();
		if ((($p0 >= 64))) goto label_1;
		$fI1 = 64;
		goto label_2;
		label_1:
		$fI1 = $p0;
		label_2:
		$this->_size = $fI1;
		$this->_buffer = new JA_C($this->_size);
		return $this;
		return $this;
	}
	public function append_C_V(int $p0) {
		$lI3 = 0;
		$lA2 = null;
		if ((($this->_position != $this->_size))) goto label_1;
		$lA2 = new JA_C(((int)(N::imul($this->_size, 2))));
		$lI3 = 0;
		label_3:
		if ((($lI3 >= $this->_size))) goto label_4;
		$lA2->set($lI3, ($this->_buffer->get($lI3)));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_3;
		label_4:
		$this->_buffer = $lA2;
		$this->_size = ((int)(($this->_size + $this->_size)));
		label_1:
		$this->_buffer->set($this->_position, $p0);
		$this->_position = ((int)(($this->_position + 1)));
		return;
	}
	public function __construct($CLASS_ID = 811) {
		parent::__construct($CLASS_ID);
		$this->_buffer = null;
		$this->_position = 0;
		$this->_size = 0;
	}
	static public function SI() {
	}
}
interface java_io_Flushable {

	public function flush__V();
}
class java_io_Flushable_IFields {

	static public function SI() {
	}
}
interface java_lang_AutoCloseable {

}
class java_lang_AutoCloseable_IFields {

	static public function SI() {
	}
}
interface java_io_Closeable extends java_lang_AutoCloseable {

}
class java_io_Closeable_IFields {

	static public function SI() {
	}
}
abstract class java_io_OutputStream extends java_lang_Object implements java_io_Closeable, java_io_Flushable {

	public function java_io_OutputStream_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function write_I_V(int $p0) {
		throw new Exception("Missing body java.io.OutputStream.write(I)V");
	}
	public function flush__V() {
		return;
	}
	public function write__BII_V(?JA_B $p0, int $p1, int $p2) {
		$lI4 = 0;
		com_jtransc_JTranscArrays::checkOffsetAndCount_III_V((($p0))->length, $p1, $p2);
		$lI4 = $p1;
		label_1:
		if ((($lI4 >= ((int)(($p1 + $p2)))))) goto label_2;
		$this->write_I_V(((int)(($p0->get($lI4)))));
		$lI4 = ((int)(($lI4 + 1)));
		goto label_1;
		label_2:
		return;
	}
	public function write__B_V(?JA_B $p0) {
		$this->write__BII_V($p0, 0, (($p0))->length);
		return;
	}
	public function __construct($CLASS_ID = 666) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_io_ByteArrayOutputStream extends java_io_OutputStream {

	public $_count = 0;
	public $_buf = null;
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_String()));
		$fA0 = $tA0;
		($tA0)->java_lang_String_init___BII_V($this->_buf, 0, $this->_count);
		return ($fA0);
	}
	public function toByteArray___B() {
		$lA1 = null;
		$lA1 = (new JA_B($this->_count));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($this->_buf), 0, $lA1, 0, $this->_count);
		return ($lA1);
	}
	public function java_io_ByteArrayOutputStream_init__I_V(int $p0) {
		$fA0 = null;
		$tA0 = null;
		($this)->java_io_OutputStream_init___V();
		if ((($p0 < 0))) goto label_1;
		$this->_buf = new JA_B($p0);
		goto label_3;
		label_1:
		$tA0 = ((new java_lang_IllegalArgumentException()));
		$fA0 = $tA0;
		($tA0)->java_lang_IllegalArgumentException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_119);
		throw new WrappedThrowable($fA0);
		label_3:
		return $this;
		return $this;
	}
	public function write_I_V(int $p0) {
		$fA0 = null;
		$fI1 = 0;
		$fA1 = null;
		$tI1 = 0;
		$tA2 = null;
		if ((($this->_count != ($this->_buf)->length))) goto label_1;
		$this->expand_I_V(1);
		label_1:
		$fA0 = $this->_buf;
		$fA1 = ($this);
		$tA2 = $fA1;
		$tI1 = $this->_count;
		$fI1 = $tI1;
		($tA2)->_count = ((int)(($tI1 + 1)));
		$fA0->set($fI1, (N::i2b($p0)));
		return;
	}
	public function expand_I_V(int $p0) {
		$lA2 = null;
		if (((((int)(($this->_count + $p0))) > ($this->_buf)->length))) goto label_1;
		return;
		label_1:
		$lA2 = (new JA_B(((int)(N::imul(((int)(($this->_count + $p0))), 2)))));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($this->_buf), 0, $lA2, 0, $this->_count);
		$this->_buf = ($lA2);
		return;
	}
	public function write__BII_V(?JA_B $p0, int $p1, int $p2) {
		com_jtransc_JTranscArrays::checkOffsetAndCount_III_V((($p0))->length, $p1, $p2);
		if ((($p2 != 0))) goto label_1;
		return;
		label_1:
		$this->expand_I_V($p2);
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($p0), $p1, ($this->_buf), $this->_count, $p2);
		$this->_count = ((int)(($this->_count + $p2)));
		return;
	}
	public function __construct($CLASS_ID = 810) {
		parent::__construct($CLASS_ID);
		$this->_count = 0;
		$this->_buf = null;
	}
	static public function SI() {
	}
}
class java_io_IOException extends java_lang_Exception {

	public function __construct($CLASS_ID = 808) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_time_JTranscClock_Impl extends java_lang_Object {

	public $_parent = null;
	public function com_jtransc_time_JTranscClock_Impl_init__Lcom_jtransc_time_JTranscClock_Impl__V(?com_jtransc_time_JTranscClock_Impl $p0) {
		($this)->java_lang_Object_init___V();
		$this->_parent = $p0;
		return $this;
		return $this;
	}
	public function fastTime__D() {
		return N::getTime();
	}
	public function __construct($CLASS_ID = 807) {
		parent::__construct($CLASS_ID);
		$this->_parent = null;
	}
	static public function SI() {
	}
}
class com_jtransc_time_JTranscClock_1 extends com_jtransc_time_JTranscClock_Impl {

	public function com_jtransc_time_JTranscClock_1_init__Lcom_jtransc_time_JTranscClock_Impl__V(?com_jtransc_time_JTranscClock_Impl $p0) {
		($this)->com_jtransc_time_JTranscClock_Impl_init__Lcom_jtransc_time_JTranscClock_Impl__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 806) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_time_JTranscClock extends java_lang_Object {

	public static $_impl = null;
	public function com_jtransc_time_JTranscClock_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function com_jtransc_time_JTranscClock_clinit___V() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new com_jtransc_time_JTranscClock_1()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_time_JTranscClock_1_init__Lcom_jtransc_time_JTranscClock_Impl__V(null);
		com_jtransc_time_JTranscClock::$_impl = ($fA0);
		return;
	}
	public function __construct($CLASS_ID = 805) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		com_jtransc_time_JTranscClock::$_impl = null;
		com_jtransc_time_JTranscClock::com_jtransc_time_JTranscClock_clinit___V();
	}
}
class com_jtransc_JTranscVersion extends java_lang_Object {

	public function com_jtransc_JTranscVersion_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function getVersion__Ljava_lang_String_() {
		return Bootstrap::$STRINGLIT_120;
	}
	public function __construct($CLASS_ID = 804) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface Benchmark_Task {

	public function run__I();
}
class Benchmark_Task_IFields {

	static public function SI() {
	}
}
class Benchmark_20 extends java_lang_Object implements Benchmark_Task {

	public $_val_farray = null;
	public function Benchmark_20_init___F_V(?JA_F $p0) {
		$this->_val_farray = $p0;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI1 = 0;
		label_1:
		if ((($lI1 >= 1000000))) goto label_2;
		$this->_val_farray->set($lI1, ((double)(((int)(N::imul($lI1, 1000))))));
		$lI1 = ((int)(($lI1 + 1)));
		goto label_1;
		label_2:
		return ((int)(((($this->_val_farray->get(7)))|0)));
	}
	public function __construct($CLASS_ID = 803) {
		parent::__construct($CLASS_ID);
		$this->_val_farray = null;
	}
	static public function SI() {
	}
}
class Benchmark_21 extends java_lang_Object implements Benchmark_Task {

	public $_val_darray = null;
	public function Benchmark_21_init___D_V(?JA_D $p0) {
		$this->_val_darray = $p0;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI1 = 0;
		label_1:
		if ((($lI1 >= 1000000))) goto label_2;
		$this->_val_darray->set($lI1, ((double)(((int)(N::imul($lI1, 1000))))));
		$lI1 = ((int)(($lI1 + 1)));
		goto label_1;
		label_2:
		return ((int)(((int)(($this->_val_darray->get(7))))));
	}
	public function __construct($CLASS_ID = 802) {
		parent::__construct($CLASS_ID);
		$this->_val_darray = null;
	}
	static public function SI() {
	}
}
class Benchmark_22 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_22_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA1 = null;
		$lI2 = 0;
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$lA1 = $fA0;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 100000))) goto label_2;
		($lA1)->append_I_Ljava_lang_StringBuilder_($lI2);
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return ($lA1)->toString__Ljava_lang_String_()->hashCode__I();
	}
	public function __construct($CLASS_ID = 801) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_27 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_27_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA1 = null;
		$lA2 = null;
		$lI3 = 0;
		$lA1 = com_jtransc_simd_MutableMatrixFloat32x4x4::create__Lcom_jtransc_simd_MutableMatrixFloat32x4x4_();
		$lA1->setTo_FFFFFFFFFFFFFFFF_V(1.0, 9.0, 1.0, 7.0, 3.0, 2.0, 4.0, 5.0, 3.0, 7.0, 3.0, 3.0, 3.0, 8.0, 4.0, 4.0);
		$lA2 = com_jtransc_simd_MutableMatrixFloat32x4x4::create__Lcom_jtransc_simd_MutableMatrixFloat32x4x4_();
		$lA2->setTo_FFFFFFFFFFFFFFFF_V(2.0, 3.0, 4.0, 5.0, 2.0, 3.0, 4.0, 5.0, 2.0, 3.0, 4.0, 5.0, 2.0, 3.0, 4.0, 5.0);
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 100000))) goto label_2;
		$lA1->setToMul44_Lcom_jtransc_simd_MutableMatrixFloat32x4x4_Lcom_jtransc_simd_MutableMatrixFloat32x4x4__V($lA1, $lA2);
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return ((int)((($lA1->getSumAll__F())|0)));
	}
	public function __construct($CLASS_ID = 800) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_28 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_28_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA1 = null;
		$lI2 = 0;
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$lA1 = $fA0;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 100000))) goto label_2;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_44);
		($lA1)->append_C_Ljava_lang_StringBuilder_(119);
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_121);
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return ($lA1)->toString__Ljava_lang_String_()->length__I();
	}
	public function __construct($CLASS_ID = 799) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_29 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_29_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA1 = null;
		$lI2 = 0;
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$lA1 = $fA0;
		($lA1)->ensureCapacity_I_V(1000000);
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 100000))) goto label_2;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_44);
		($lA1)->append_C_Ljava_lang_StringBuilder_(119);
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_121);
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return ($lA1)->toString__Ljava_lang_String_()->length__I();
	}
	public function __construct($CLASS_ID = 798) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_23 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_23_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA1 = null;
		$lI2 = 0;
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$lA1 = $fA0;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 100000))) goto label_2;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_122);
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return ($lA1)->toString__Ljava_lang_String_()->hashCode__I();
	}
	public function __construct($CLASS_ID = 797) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_24 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_24_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI3 = 0;
		$lJ1 = Int64::make(0, 0);
		$lJ1 = Int64::make(0, 0);
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 10000))) goto label_2;
		$lJ1 = (N::ladd((N::lmul(Int64::make(0, 17777), N::i2j($lI3))), (N::ldiv($lJ1, Int64::make(0, 3)))));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return N::j2i($lJ1);
	}
	public function __construct($CLASS_ID = 796) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_25 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_25_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI3 = 0;
		$lA1 = null;
		$lA2 = null;
		$lA1 = com_jtransc_simd_MutableFloat32x4::create__Lcom_jtransc_simd_MutableFloat32x4_();
		$lA2 = com_jtransc_simd_MutableFloat32x4::create_FFFF_Lcom_jtransc_simd_MutableFloat32x4_(2.0, 3.0, 4.0, 5.0);
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 1000000))) goto label_2;
		$lA1->setToAdd_Lcom_jtransc_simd_MutableFloat32x4_Lcom_jtransc_simd_MutableFloat32x4__V($lA1, $lA2);
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return ((int)((((int)((((int)((((int)((($lA1->getX__F())|0))) + ((int)((($lA1->getY__F())|0)))))) + ((int)((($lA1->getZ__F())|0)))))) + ((int)((($lA1->getW__F())|0))))));
	}
	public function __construct($CLASS_ID = 795) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_26 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_26_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI3 = 0;
		$lA1 = null;
		$lA2 = null;
		$lA1 = com_jtransc_simd_Float32x4::create_FFFF_Lcom_jtransc_simd_Float32x4_(0.0, 0.0, 0.0, 0.0);
		$lA2 = com_jtransc_simd_Float32x4::create_FFFF_Lcom_jtransc_simd_Float32x4_(2.0, 3.0, 4.0, 5.0);
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 1000000))) goto label_2;
		$lA1 = com_jtransc_simd_Float32x4::add_Lcom_jtransc_simd_Float32x4_Lcom_jtransc_simd_Float32x4__Lcom_jtransc_simd_Float32x4_($lA1, $lA2);
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return ((int)((((int)((((int)((((int)(((com_jtransc_simd_Float32x4::getX_Lcom_jtransc_simd_Float32x4__F($lA1))|0))) + ((int)(((com_jtransc_simd_Float32x4::getY_Lcom_jtransc_simd_Float32x4__F($lA1))|0)))))) + ((int)(((com_jtransc_simd_Float32x4::getZ_Lcom_jtransc_simd_Float32x4__F($lA1))|0)))))) + ((int)(((com_jtransc_simd_Float32x4::getW_Lcom_jtransc_simd_Float32x4__F($lA1))|0))))));
	}
	public function __construct($CLASS_ID = 794) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_10 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_10_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = 305419896;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 1000000))) goto label_2;
		$lI1 = ((int)(($lI1 + ((int)((((int)(N::iushr($lI1, $lI2))) + ((int)(N::iushr($lI1, ((int)(-($lI2))))))))))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 793) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_11 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_11_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = 0;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 1000000))) goto label_2;
		$lI1 = ((int)(($lI1 + Benchmark::calc_II_I($lI1, $lI2))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 792) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_16 extends java_lang_Object implements Benchmark_Task {

	public $_val_barray = null;
	public function Benchmark_16_init___B_V(?JA_B $p0) {
		$this->_val_barray = $p0;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI1 = 0;
		label_1:
		if ((($lI1 >= 1000000))) goto label_2;
		$this->_val_barray->set($lI1, (N::i2b(((int)(N::imul($lI1, 123456711))))));
		$lI1 = ((int)(($lI1 + 1)));
		goto label_1;
		label_2:
		return ((int)(($this->_val_barray->get(7))));
	}
	public function __construct($CLASS_ID = 791) {
		parent::__construct($CLASS_ID);
		$this->_val_barray = null;
	}
	static public function SI() {
	}
}
class Benchmark_17 extends java_lang_Object implements Benchmark_Task {

	public $_val_sarray = null;
	public function Benchmark_17_init___S_V(?JA_S $p0) {
		$this->_val_sarray = $p0;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI1 = 0;
		label_1:
		if ((($lI1 >= 1000000))) goto label_2;
		$this->_val_sarray->set($lI1, (N::i2s(((int)(N::imul($lI1, 1000))))));
		$lI1 = ((int)(($lI1 + 1)));
		goto label_1;
		label_2:
		return ((int)(($this->_val_sarray->get(7))));
	}
	public function __construct($CLASS_ID = 790) {
		parent::__construct($CLASS_ID);
		$this->_val_sarray = null;
	}
	static public function SI() {
	}
}
class Benchmark_18 extends java_lang_Object implements Benchmark_Task {

	public $_val_carray = null;
	public function Benchmark_18_init___C_V(?JA_C $p0) {
		$this->_val_carray = $p0;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI1 = 0;
		label_1:
		if ((($lI1 >= 1000000))) goto label_2;
		$this->_val_carray->set($lI1, (N::i2c(((int)(N::imul($lI1, 1000))))));
		$lI1 = ((int)(($lI1 + 1)));
		goto label_1;
		label_2:
		return ((int)(($this->_val_carray->get(7))));
	}
	public function __construct($CLASS_ID = 789) {
		parent::__construct($CLASS_ID);
		$this->_val_carray = null;
	}
	static public function SI() {
	}
}
class Benchmark_19 extends java_lang_Object implements Benchmark_Task {

	public $_val_iarray = null;
	public function Benchmark_19_init___I_V(?JA_I $p0) {
		$this->_val_iarray = $p0;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI1 = 0;
		label_1:
		if ((($lI1 >= 1000000))) goto label_2;
		$this->_val_iarray->set($lI1, ((int)(N::imul($lI1, 1000))));
		$lI1 = ((int)(($lI1 + 1)));
		goto label_1;
		label_2:
		return ($this->_val_iarray->get(7));
	}
	public function __construct($CLASS_ID = 788) {
		parent::__construct($CLASS_ID);
		$this->_val_iarray = null;
	}
	static public function SI() {
	}
}
class Benchmark_12 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_12_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = 0;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 1000000))) goto label_2;
		$lI1 = ((int)(($lI1 + $this->calc_II_I($lI1, $lI2))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function calc_II_I(int $p0, int $p1) {
		return ((int)(N::imul(((int)(($p0 + $p1))), ((int)(($p0 + $p1))))));
	}
	public function __construct($CLASS_ID = 787) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_13 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_13_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = 1;
		$lI2 = 1;
		label_1:
		if ((($lI2 >= 1000000))) goto label_2;
		$lI1 = ((int)(($lI1 + $this->calc_II_I($lI1, $lI2))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function calc_II_I(int $p0, int $p1) {
		return ((int)(N::idiv(((int)(($p0 - $p1))), ((int)(($p0 + $p1))))));
	}
	public function __construct($CLASS_ID = 786) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_14 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_14_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA3 = null;
		$lA4 = null;
		$lI1 = 0;
		$lI2 = 0;
		$lI5 = 0;
		$lI1 = 1;
		$lI2 = $this->rand_I_I(2);
		$lA3 = $this->genObj_I_Ljava_lang_Object_(((int)(N::irem(((int)(($lI2 + 0))), 2))));
		$lA4 = $this->genObj_I_Ljava_lang_Object_(((int)(N::irem(((int)(($lI2 + 1))), 2))));
		$lI5 = 1;
		label_1:
		if ((($lI5 >= 1000000))) goto label_2;
		if (!((($lA3) instanceof Benchmark_Test1))) goto label_4;
		$lI1 = ((int)(($lI1 + ((int)(($lI5 - 1))))));
		goto label_6;
		label_4:
		if (!((($lA3) instanceof Benchmark_Test2))) goto label_6;
		$lI1 = ((int)(($lI1 + ((int)(($lI5 + 2))))));
		label_6:
		if (!((($lA4) instanceof Benchmark_Test1))) goto label_7;
		$lI1 = ((int)(($lI1 + ((int)(($lI5 - 3))))));
		goto label_9;
		label_7:
		if (!((($lA4) instanceof Benchmark_Test2))) goto label_9;
		$lI1 = ((int)(($lI1 + ((int)(($lI5 + 4))))));
		label_9:
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function rand_I_I(int $p0) {
		return N::j2i((N::lrem(java_lang_System::currentTimeMillis__J(), N::i2j($p0))));
	}
	public function genObj_I_Ljava_lang_Object_(int $p0) {
		$fA0 = null;
		$tA1 = null;
		$tA0 = null;
		switch ($p0) {
			case 0: goto label_2;
			default: goto label_1;
		}
		label_2:
		$tA1 = ((new Benchmark_Test1()));
		$fA0 = $tA1;
		($tA1)->Benchmark_Test1_init__LBenchmark_1__V(null);
		return $fA0;
		label_1:
		$tA0 = ((new Benchmark_Test2()));
		$fA0 = $tA0;
		($tA0)->Benchmark_Test2_init__LBenchmark_1__V(null);
		return $fA0;
	}
	public function __construct($CLASS_ID = 785) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_15 extends java_lang_Object implements Benchmark_Task {

	public $_val_srcI = null;
	public $_val_dstI = null;
	public function Benchmark_15_init___I_I_V(?JA_I $p0, ?JA_I $p1) {
		$this->_val_srcI = $p0;
		$this->_val_dstI = $p1;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI1 = 0;
		label_1:
		if ((($lI1 >= 1024))) goto label_2;
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($this->_val_srcI), 0, ($this->_val_dstI), $lI1, 8192);
		$lI1 = ((int)(($lI1 + 1)));
		goto label_1;
		label_2:
		return 0;
	}
	public function __construct($CLASS_ID = 784) {
		parent::__construct($CLASS_ID);
		$this->_val_srcI = null;
		$this->_val_dstI = null;
	}
	static public function SI() {
	}
}
class Benchmark_9 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_9_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = 305419896;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 1000000))) goto label_2;
		$lI1 = ((int)(($lI1 + ((int)((((int)(N::ishr($lI1, $lI2))) + ((int)(N::ishr($lI1, ((int)(-($lI2))))))))))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 783) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_8 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_8_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = 305419896;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 1000000))) goto label_2;
		$lI1 = ((int)(($lI1 + ((int)((((int)(N::ishl($lI1, $lI2))) + ((int)(N::ishl($lI1, ((int)(-($lI2))))))))))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 782) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_7 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_7_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI3 = 0;
		$lJ1 = Int64::make(0, 0);
		$lJ1 = Int64::make(0, 305419896);
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 1000000))) goto label_2;
		$lJ1 = (N::ladd($lJ1, (N::lushr($lJ1, 1))));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return N::j2i($lJ1);
	}
	public function __construct($CLASS_ID = 781) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_41 extends java_lang_Object implements Benchmark_Task {

	public $_val_bytes = null;
	public function Benchmark_41_init___B_V(?JA_B $p0) {
		$this->_val_bytes = $p0;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$G = 0;
		$lA1 = null;
		$lA2 = null;
		$fI0 = 0;
		$lI3 = 0;
		$fA0 = null;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$G = 1;
							continue 2;
						case 1:
							$lA1 = (new JA_B(131072));
							$tA0 = ((new java_util_zip_Deflater()));
							$fA0 = $tA0;
							($tA0)->java_util_zip_Deflater_init__IZ_V(9, false);
							$lA2 = $fA0;
							($lA2)->setInput__BII_V($this->_val_bytes, 0, ($this->_val_bytes)->length);
							$lI3 = ($lA2)->deflate__BIII_I(($lA1), 0, ($lA1)->length, 3);
							$fI0 = $lI3;
							$G = 2;
							continue 2;
						case 2:return $fI0; 
						case 3:
							$fA0 = ($J__exception__);
							$lA1 = $fA0;
							($lA1)->printStackTrace__V();
							return 0;
						default:
							break;
					}
				}
				return 0;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 3;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return 0;
	}
	public function __construct($CLASS_ID = 780) {
		parent::__construct($CLASS_ID);
		$this->_val_bytes = null;
	}
	static public function SI() {
	}
}
class Benchmark_6 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_6_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI3 = 0;
		$lJ1 = Int64::make(0, 0);
		$lJ1 = Int64::make(0, 305419896);
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 1000000))) goto label_2;
		$lJ1 = (N::ladd($lJ1, (N::lshr($lJ1, 1))));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return N::j2i($lJ1);
	}
	public function __construct($CLASS_ID = 779) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_42 extends java_lang_Object implements Benchmark_Task {

	public $_val_bytes = null;
	public function Benchmark_42_init___B_V(?JA_B $p0) {
		$this->_val_bytes = $p0;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$G = 0;
		$lA1 = null;
		$lA2 = null;
		$fI0 = 0;
		$lI3 = 0;
		$fA0 = null;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$G = 1;
							continue 2;
						case 1:
							$lA1 = (new JA_B(131072));
							$tA0 = ((new com_jtransc_compression_jzlib_Deflater()));
							$fA0 = $tA0;
							($tA0)->com_jtransc_compression_jzlib_Deflater_init__IZ_V(9, false);
							$lA2 = $fA0;
							($lA2)->setInput__BIIZ_V($this->_val_bytes, 0, ($this->_val_bytes)->length, false);
							($lA2)->setOutput__BII_V(($lA1), 0, ($lA1)->length);
							$lI3 = ($lA2)->deflate_I_I(3);
							$fI0 = $lI3;
							$G = 2;
							continue 2;
						case 2:return $fI0; 
						case 3:
							$fA0 = ($J__exception__);
							$lA1 = $fA0;
							($lA1)->printStackTrace__V();
							return 0;
						default:
							break;
					}
				}
				return 0;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 3;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return 0;
	}
	public function __construct($CLASS_ID = 778) {
		parent::__construct($CLASS_ID);
		$this->_val_bytes = null;
	}
	static public function SI() {
	}
}
class Benchmark_5 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_5_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI3 = 0;
		$lJ1 = Int64::make(0, 0);
		$lJ1 = Int64::make(0, 305419896);
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 1000000))) goto label_2;
		$lJ1 = (N::ladd($lJ1, (N::lshl($lJ1, 1))));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return N::j2i($lJ1);
	}
	public function __construct($CLASS_ID = 777) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_43 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_43_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA1 = null;
		$lA2 = null;
		$lI3 = 0;
		$lI4 = 0;
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_util_Random()));
		$fA0 = $tA0;
		($tA0)->java_util_Random_init__J_V(Int64::make(0, 0));
		$lA1 = $fA0;
		$lA2 = (new JA_B(65536));
		$lI3 = 0;
		$lI4 = 0;
		label_1:
		if ((($lI4 >= ($lA2)->length))) goto label_2;
		($lA2)->set($lI4, (N::i2b(($lA1)->nextInt__I())));
		$lI3 = ((int)(($lI3 + (($lA2)->get($lI4)))));
		$lI4 = ((int)(($lI4 + 1)));
		goto label_1;
		label_2:
		return $lI3;
	}
	public function __construct($CLASS_ID = 776) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_4 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_4_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = 305419896;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 1000000))) goto label_2;
		$lI1 = ((int)(($lI1 + ((int)(N::iushr($lI1, 1))))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 775) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_44 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_44_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$G = 0;
		$lI1 = 0;
		$lI2 = 0;
		$fA0 = null;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$lI1 = 0;
							$lI2 = 0;
							$G = 1;
							continue 2;
						case 1:
							if ((($lI2 >= 1000))) {
								$G = 2;
								continue 2;
							}
							$G = 3;
							continue 2;
						case 3:
							$tA0 = ((new java_lang_Throwable()));
							$fA0 = $tA0;
							($tA0)->java_lang_Throwable_init___V();
							throw new WrappedThrowable($fA0);
							$G = 4;
							continue 2;
						case 4:
							$fA0 = ($J__exception__);
							$lI1 = ((int)(($lI1 + 1)));
							$lI2 = ((int)(($lI2 + 1)));
							$G = 1;
							continue 2;
						case 2:
							return $lI1;
						default:
							break;
					}
				}
				return 0;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 3)) && (($G < 4)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 4;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return 0;
	}
	public function __construct($CLASS_ID = 774) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_3 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_3_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = 305419896;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 1000000))) goto label_2;
		$lI1 = ((int)(($lI1 + ((int)(N::ishr($lI1, 1))))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 773) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_2 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_2_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = 305419896;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 1000000))) goto label_2;
		$lI1 = ((int)(($lI1 + ((int)(N::ishl($lI1, 1))))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 772) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_1 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_1_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI1 = 0;
		$lI2 = 0;
		$lI1 = 0;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 1000000))) goto label_2;
		$lI1 = ((int)(($lI1 + $lI2)));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 771) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_40 extends java_lang_Object implements Benchmark_Task {

	public $_val_hexData = null;
	public function Benchmark_40_init___B_V(?JA_B $p0) {
		$this->_val_hexData = $p0;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA2 = null;
		$lI1 = 0;
		$lI3 = 0;
		$fA0 = null;
		$tA0 = null;
		$lI1 = 0;
		$tA0 = ((new com_jtransc_compression_jzlib_CRC32()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_compression_jzlib_CRC32_init___V();
		$lA2 = $fA0;
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 10000))) goto label_2;
		($lA2)->reset__V();
		($lA2)->update__BII_V($this->_val_hexData, 0, ($this->_val_hexData)->length);
		$lI1 = ((int)(($lI1 + ($lA2)->getValue__I())));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 770) {
		parent::__construct($CLASS_ID);
		$this->_val_hexData = null;
	}
	static public function SI() {
	}
}
class java_lang_Runtime extends java_lang_Object {

	public static $_current = null;
	public function java_lang_Runtime_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function gc__V() {
		gc_collect_cycles();
	}
	public static function getRuntime__Ljava_lang_Runtime_() {
		$fA0 = null;
		$tA0 = null;
		if (((java_lang_Runtime::$_current != null))) goto label_1;
		$tA0 = ((new java_lang_Runtime()));
		$fA0 = $tA0;
		($tA0)->java_lang_Runtime_init___V();
		java_lang_Runtime::$_current = ($fA0);
		label_1:
		return java_lang_Runtime::$_current;
	}
	public function freeMemory__J() {
		return N::d2j((float)0.0);
	}
	public function maxMemory__J() {
		return N::d2j((float)memory_get_usage());
	}
	public function totalMemory__J() {
		return N::d2j((float)memory_get_peak_usage());
	}
	public function __construct($CLASS_ID = 769) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		java_lang_Runtime::$_current = null;
	}
}
class Benchmark_30 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_30_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA2 = null;
		$lA1 = null;
		$lI4 = 0;
		$lI5 = 0;
		$lA3 = null;
		$lA1 = java_nio_ByteBuffer::allocate_I_Ljava_nio_ByteBuffer_(1024)->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder::nativeOrder__Ljava_nio_ByteOrder_());
		$lA2 = $lA1->asIntBuffer__Ljava_nio_IntBuffer_();
		$lA3 = $lA1->asFloatBuffer__Ljava_nio_FloatBuffer_();
		$lI4 = 0;
		$lI5 = 0;
		label_1:
		if ((($lI5 >= 100000))) goto label_2;
		$lA3->put_IF_Ljava_nio_FloatBuffer_(0, ((double)($lI5)));
		$lI4 = ((int)(($lI4 + $lA2->get_I_I(0))));
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		return $lI4;
	}
	public function __construct($CLASS_ID = 768) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_31 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_31_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA2 = null;
		$lA1 = null;
		$lI4 = 0;
		$lI5 = 0;
		$lA3 = null;
		$lA1 = java_nio_ByteBuffer::allocateDirect_I_Ljava_nio_ByteBuffer_(1024)->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder::nativeOrder__Ljava_nio_ByteOrder_());
		$lA2 = $lA1->asIntBuffer__Ljava_nio_IntBuffer_();
		$lA3 = $lA1->asFloatBuffer__Ljava_nio_FloatBuffer_();
		$lI4 = 0;
		$lI5 = 0;
		label_1:
		if ((($lI5 >= 100000))) goto label_2;
		$lA3->put_IF_Ljava_nio_FloatBuffer_(0, ((double)($lI5)));
		$lI4 = ((int)(($lI4 + $lA2->get_I_I(0))));
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		return $lI4;
	}
	public function __construct($CLASS_ID = 767) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_32 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_32_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA1 = null;
		$lI4 = 0;
		$lI5 = 0;
		$lA2 = null;
		$lA3 = null;
		$lA1 = java_nio_ByteBuffer::allocateDirect_I_Ljava_nio_ByteBuffer_(1024)->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder::nativeOrder__Ljava_nio_ByteOrder_());
		$lA2 = $lA1->asShortBuffer__Ljava_nio_ShortBuffer_();
		$lA3 = $lA1->asCharBuffer__Ljava_nio_CharBuffer_();
		$lI4 = 0;
		$lI5 = 0;
		label_1:
		if ((($lI5 >= 100000))) goto label_2;
		$lA3->put_IC_Ljava_nio_CharBuffer_(0, (N::i2c($lI5)));
		$lI4 = ((int)(($lI4 + $lA2->get_I_S(0))));
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		return $lI4;
	}
	public function __construct($CLASS_ID = 766) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_33 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_33_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA1 = null;
		$lI4 = 0;
		$lI5 = 0;
		$lA2 = null;
		$lA3 = null;
		$lA1 = java_nio_ByteBuffer::allocateDirect_I_Ljava_nio_ByteBuffer_(1024)->order_Ljava_nio_ByteOrder__Ljava_nio_ByteBuffer_(java_nio_ByteOrder::nativeOrder__Ljava_nio_ByteOrder_());
		$lA2 = $lA1->asLongBuffer__Ljava_nio_LongBuffer_();
		$lA3 = $lA1->asDoubleBuffer__Ljava_nio_DoubleBuffer_();
		$lI4 = 0;
		$lI5 = 0;
		label_1:
		if ((($lI5 >= 100000))) goto label_2;
		$lA3->put_ID_Ljava_nio_DoubleBuffer_(0, ((double)($lI5)));
		$lI4 = N::j2i((N::ladd(N::i2j($lI4), $lA2->get_I_J(0))));
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		return $lI4;
	}
	public function __construct($CLASS_ID = 765) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_Random extends java_lang_Object implements java_io_Serializable {

	public $_seed = null;
	public $_haveNextNextGaussian = false;
	public function nextInt__I() {
		return $this->next_I_I(32);
	}
	public function next_I_I(int $p0) {
		$this->_seed = (N::land((N::ladd((N::lmul($this->_seed, Int64::make(5, -554899859))), Int64::make(0, 11))), Int64::make(65535, -1)));
		return N::j2i((N::lushr($this->_seed, ((int)((48 - $p0))))));
	}
	public function java_util_Random_init__J_V(Int64 $p0) {
		($this)->java_lang_Object_init___V();
		$this->setSeed_J_V($p0);
		return $this;
		return $this;
	}
	public function setSeed_J_V(Int64 $p0) {
		$this->_seed = (N::land((N::lxor($p0, Int64::make(5, -554899859))), Int64::make(65535, -1)));
		$this->_haveNextNextGaussian = false;
		return;
	}
	public function __construct($CLASS_ID = 764) {
		parent::__construct($CLASS_ID);
		$this->_seed = Int64::make(0, 0);
		$this->_haveNextNextGaussian = false;
	}
	static public function SI() {
	}
}
class Benchmark_38 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_38_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA3 = null;
		$lI1 = 0;
		$lI2 = 0;
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		$lI1 = 0;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 100000))) goto label_2;
		$tA0 = ((new Benchmark_MyClass()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->Benchmark_MyClass_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_123)->append_I_Ljava_lang_StringBuilder_($lI2)->toString__Ljava_lang_String_());
		$lA3 = $fA0;
		$lI1 = ((int)(($lI1 + ($lA3)->_b)));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 763) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_39 extends java_lang_Object implements Benchmark_Task {

	public $_val_hexData = null;
	public function Benchmark_39_init___B_V(?JA_B $p0) {
		$this->_val_hexData = $p0;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA2 = null;
		$lI1 = 0;
		$lI3 = 0;
		$fA0 = null;
		$tA0 = null;
		$lI1 = 0;
		$tA0 = ((new java_util_zip_CRC32()));
		$fA0 = $tA0;
		($tA0)->java_util_zip_CRC32_init___V();
		$lA2 = $fA0;
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 10000))) goto label_2;
		($lA2)->reset__V();
		($lA2)->update__BII_V($this->_val_hexData, 0, ($this->_val_hexData)->length);
		$lI1 = N::j2i((N::ladd(N::i2j($lI1), ($lA2)->getValue__J())));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 762) {
		parent::__construct($CLASS_ID);
		$this->_val_hexData = null;
	}
	static public function SI() {
	}
}
class com_jtransc_JTranscSystem extends java_lang_Object {

	public static $_start = 0.0;
	public function com_jtransc_JTranscSystem_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function com_jtransc_JTranscSystem_clinit___V() {
		com_jtransc_JTranscSystem::$_start = -1.0;
		return;
	}
	public static function stamp__D() {
		if (!(((com_jtransc_JTranscSystem::$_start < 0.0)))) goto label_1;
		com_jtransc_JTranscSystem::$_start = com_jtransc_JTranscSystem::fastTime__D();
		label_1:
		return com_jtransc_JTranscSystem::elapsedTime_DD_D(com_jtransc_JTranscSystem::$_start, com_jtransc_JTranscSystem::fastTime__D());
	}
	public static function elapsedTime_DD_D(float $p0, float $p1) {
		return (($p1 - $p0));
	}
	public static function fastTime__D() {
		return com_jtransc_time_JTranscClock::$_impl->fastTime__D();
	}
	public static function lineSeparator__Ljava_lang_String_() {
		return com_jtransc_JTranscSystemProperties::lineSeparator__Ljava_lang_String_();
	}
	public static function isCpp__Z() {
		return false;
	}
	public static function getRuntimeKind__Ljava_lang_String_() {
		return N::str('php');
	}
	public static function getOS__Ljava_lang_String_() {
		$lA0 = null;
		$lA0 = com_jtransc_JTranscSystem::getOSRaw__Ljava_lang_String_()->toLowerCase__Ljava_lang_String_();
		if (!($lA0->startsWith_Ljava_lang_String__Z(Bootstrap::$STRINGLIT_124))) goto label_1;
		return Bootstrap::$STRINGLIT_125;
		label_1:
		if (!($lA0->startsWith_Ljava_lang_String__Z(Bootstrap::$STRINGLIT_126))) goto label_2;
		return Bootstrap::$STRINGLIT_127;
		label_2:
		if ($lA0->startsWith_Ljava_lang_String__Z(Bootstrap::$STRINGLIT_128)) goto label_3;
		if (!($lA0->startsWith_Ljava_lang_String__Z(Bootstrap::$STRINGLIT_129))) goto label_4;
		label_3:
		return Bootstrap::$STRINGLIT_128;
		label_4:
		if (!($lA0->startsWith_Ljava_lang_String__Z(Bootstrap::$STRINGLIT_130))) goto label_5;
		return Bootstrap::$STRINGLIT_131;
		label_5:
		return $lA0;
	}
	public static function getOSRaw__Ljava_lang_String_() {
		if (!(com_jtransc_JTranscSystem::isJTransc__Z())) goto label_1;
		return Bootstrap::$STRINGLIT_132;
		label_1:
		return java_lang_System::getProperty_Ljava_lang_String__Ljava_lang_String_(Bootstrap::$STRINGLIT_133);
	}
	public static function isJTransc__Z() {
		return com_jtransc_JTranscSystem::usingJTransc__Z();
	}
	public static function usingJTransc__Z() {
		return true;
	}
	public static function isWindows__Z() {
		return com_jtransc_JTranscSystem::getOS__Ljava_lang_String_()->toLowerCase__Ljava_lang_String_()->startsWith_Ljava_lang_String__Z(Bootstrap::$STRINGLIT_124);
	}
	public static function getRuntimeName__Ljava_lang_String_() {
		return Bootstrap::$STRINGLIT_134;
	}
	public static function getJavaHome__Ljava_lang_String_() {
		return java_lang_System::getenv_Ljava_lang_String__Ljava_lang_String_(Bootstrap::$STRINGLIT_135);
	}
	public static function getArch__Ljava_lang_String_() {
		if (!(com_jtransc_JTranscSystem::isJvm__Z())) goto label_1;
		return java_lang_System::getProperty_Ljava_lang_String__Ljava_lang_String_(Bootstrap::$STRINGLIT_136);
		label_1:
		return Bootstrap::$STRINGLIT_137;
	}
	public static function isJvm__Z() {
		$fI0 = 0;
		if (com_jtransc_JTranscSystem::isJTransc__Z()) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public static function pathSeparator__Ljava_lang_String_() {
		return com_jtransc_JTranscSystemProperties::pathSeparator__Ljava_lang_String_();
	}
	public static function fileSeparator__Ljava_lang_String_() {
		return com_jtransc_JTranscSystemProperties::fileSeparator__Ljava_lang_String_();
	}
	public function __construct($CLASS_ID = 761) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		com_jtransc_JTranscSystem::$_start = 0.0;
		com_jtransc_JTranscSystem::com_jtransc_JTranscSystem_clinit___V();
	}
}
class Benchmark_34 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_34_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lI2 = 0;
		$lI3 = 0;
		$lA1 = null;
		$lA1 = com_jtransc_FastMemory::alloc_I_Lcom_jtransc_FastMemory_(1024);
		$lI2 = 0;
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 100000))) goto label_2;
		$lA1->setAlignedFloat32_IF_V(0, ((double)($lI3)));
		$lI2 = ((int)(($lI2 + $lA1->getAlignedInt32_I_I(0))));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return $lI2;
	}
	public function __construct($CLASS_ID = 760) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_35 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_35_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA3 = null;
		$lI1 = 0;
		$lI2 = 0;
		$fA0 = null;
		$tA0 = null;
		$lI1 = 0;
		$lI2 = 0;
		label_1:
		if ((($lI2 >= 100000))) goto label_2;
		$tA0 = ((new Benchmark_MyClass()));
		$fA0 = $tA0;
		($tA0)->Benchmark_MyClass_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_123);
		$lA3 = $fA0;
		$lI1 = ((int)(($lI1 + ($lA3)->_b)));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 759) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_36 extends java_lang_Object implements Benchmark_Task {

	public function Benchmark_36_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA4 = null;
		$lI1 = 0;
		$lI3 = 0;
		$fA0 = null;
		$lA2 = null;
		$tA0 = null;
		$lI1 = 0;
		$lA2 = Bootstrap::$STRINGLIT_123;
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 100000))) goto label_2;
		$tA0 = ((new Benchmark_MyClass2()));
		$fA0 = $tA0;
		($tA0)->Benchmark_MyClass2_init__Ljava_lang_String_I_V($lA2, ((int)(N::imul($lI3, $lI1))));
		$lA4 = $fA0;
		$lI1 = ((int)(($lI1 + ($lA4)->_b)));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 758) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class Benchmark_MyClass2 extends java_lang_Object {

	public $_a = 0;
	public $_c = null;
	public $_d = null;
	public $_b = 0;
	public function Benchmark_MyClass2_init__Ljava_lang_String_I_V(?java_lang_String $p0, int $p1) {
		($this)->java_lang_Object_init___V();
		$this->_a = 10;
		$this->_b = 20;
		$this->_c = Bootstrap::$STRINGLIT_44;
		$this->_d = $p0;
		$this->_b = $p1;
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 757) {
		parent::__construct($CLASS_ID);
		$this->_a = 0;
		$this->_c = null;
		$this->_d = null;
		$this->_b = 0;
	}
	static public function SI() {
	}
}
class Benchmark_37 extends java_lang_Object implements Benchmark_Task {

	public $_val_objects = null;
	public function Benchmark_37_init___LBenchmark_MyClass2__V(?JA_L $p0) {
		$this->_val_objects = $p0;
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function run__I() {
		$lA4 = null;
		$lI1 = 0;
		$lI3 = 0;
		$fA0 = null;
		$lA2 = null;
		$tA0 = null;
		$lI1 = 0;
		$lA2 = Bootstrap::$STRINGLIT_123;
		$lI3 = 0;
		label_1:
		if ((($lI3 >= 100000))) goto label_2;
		$tA0 = ((new Benchmark_MyClass2()));
		$fA0 = $tA0;
		($tA0)->Benchmark_MyClass2_init__Ljava_lang_String_I_V($lA2, ((int)(N::imul($lI3, $lI1))));
		$lA4 = $fA0;
		($this->_val_objects)->set($lI3, $lA4);
		$lI1 = ((int)(($lI1 + ($lA4)->_b)));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return $lI1;
	}
	public function __construct($CLASS_ID = 755) {
		parent::__construct($CLASS_ID);
		$this->_val_objects = null;
	}
	static public function SI() {
	}
}
class java_lang_ClassCastException extends java_lang_RuntimeException {

	public function java_lang_ClassCastException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 754) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_Arrays_ArrayList extends java_util_AbstractList implements java_util_List, java_io_Serializable, java_util_RandomAccess {

	public $_a = null;
	public function java_util_Arrays_ArrayList_init___Ljava_lang_Object__V(?JA_L $p0) {
		$fA0 = null;
		$tA0 = null;
		($this)->java_util_AbstractList_init___V();
		if (((($p0) != null))) goto label_1;
		$tA0 = ((new java_lang_NullPointerException()));
		$fA0 = $tA0;
		($tA0)->java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_138);
		throw new WrappedThrowable($fA0);
		label_1:
		$this->_a = $p0;
		return $this;
		return $this;
	}
	public function get_I_Ljava_lang_Object_(int $p0) {
		$G = 0;
		$fA0 = null;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$G = 1;
							continue 2;
						case 1:
							$fA0 = ($this->_a->get($p0));
							$G = 2;
							continue 2;
						case 2:return $fA0; 
						case 3:
							$fA0 = ($J__exception__);
							$tA0 = ((new java_lang_IndexOutOfBoundsException()));
							$fA0 = $tA0;
							($tA0)->java_lang_IndexOutOfBoundsException_init___V();
							throw new WrappedThrowable($fA0);
							break;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_ArrayIndexOutOfBoundsException)))) {
					$G = 3;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public function size__I() {
		return ($this->_a)->length;
	}
	public function toArray__Ljava_lang_Object___Ljava_lang_Object_(?JA_L $p0) {
		$lA1 = null;
		$lI2 = 0;
		$lA1 = ($p0);
		$lI2 = $this->size__I();
		if ((($lI2 <= ($lA1)->length))) goto label_1;
		$lA1 = (java_util_Arrays::access_000__Ljava_lang_Object_I__Ljava_lang_Object_(($lA1), $lI2));
		label_1:
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($this->_a), 0, $lA1, 0, $lI2);
		if ((($lI2 >= ($lA1)->length))) goto label_3;
		($lA1)->set($lI2, null);
		label_3:
		return ($lA1);
	}
	public function indexOf_Ljava_lang_Object__I(?java_lang_Object $p0) {
		$lI2 = 0;
		if ((($p0 == null))) goto label_1;
		$lI2 = 0;
		label_3:
		if ((($lI2 >= ($this->_a)->length))) goto label_4;
		if (!($p0->equals_Ljava_lang_Object__Z(($this->_a->get($lI2))))) goto label_6;
		return $lI2;
		label_6:
		$lI2 = ((int)(($lI2 + 1)));
		goto label_3;
		label_4:
		goto label_8;
		label_1:
		$lI2 = 0;
		label_9:
		if ((($lI2 >= ($this->_a)->length))) goto label_8;
		if (((($this->_a->get($lI2)) != null))) goto label_11;
		return $lI2;
		label_11:
		$lI2 = ((int)(($lI2 + 1)));
		goto label_9;
		label_8:
		return -1;
	}
	public function contains_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lA2 = null;
		$lA5 = null;
		$lI3 = 0;
		$lI4 = 0;
		if ((($p0 == null))) goto label_1;
		$lA2 = ($this->_a);
		$lI3 = ($lA2)->length;
		$lI4 = 0;
		label_3:
		if ((($lI4 >= $lI3))) goto label_4;
		$lA5 = (($lA2)->get($lI4));
		if (!($p0->equals_Ljava_lang_Object__Z($lA5))) goto label_5;
		return true;
		label_5:
		$lI4 = ((int)(($lI4 + 1)));
		goto label_3;
		label_4:
		goto label_7;
		label_1:
		$lA2 = ($this->_a);
		$lI3 = ($lA2)->length;
		$lI4 = 0;
		label_8:
		if ((($lI4 >= $lI3))) goto label_7;
		$lA5 = (($lA2)->get($lI4));
		if ((($lA5 != null))) goto label_9;
		return true;
		label_9:
		$lI4 = ((int)(($lI4 + 1)));
		goto label_8;
		label_7:
		return false;
	}
	public function __construct($CLASS_ID = 751) {
		parent::__construct($CLASS_ID);
		$this->_a = null;
	}
	static public function SI() {
	}
}
class j_ProgramReflection_AllConstructors extends java_lang_Object {

	public function j_ProgramReflection_AllConstructors_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function getConstructors_I__Lj_MemberInfo_(int $p0) {
		if ((($p0 >= 857))) {
			return j_ProgramReflection_AllConstructors::getConstructors2_I__Lj_MemberInfo_($p0);
		}
		if ((($p0 >= 755))) {
			return j_ProgramReflection_AllConstructors::getConstructors1_I__Lj_MemberInfo_($p0);
		}
		if ((($p0 >= 649))) {
			return j_ProgramReflection_AllConstructors::getConstructors0_I__Lj_MemberInfo_($p0);
		}
		return null;
	}
	public static function getConstructors0_I__Lj_MemberInfo_(int $p0) {
		$_out = null;
		switch ($p0) {
			case 649:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7204, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 650:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7205, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 651:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7213, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_141, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7493, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_142, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7768, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_143, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7769, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_144, null));
				return $_out;
			case 655:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7210, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_145, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7211, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 658:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7225, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7226, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			case 660:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7214, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 661:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7216, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 662:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7220, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_147, null));
				return $_out;
			case 665:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7218, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_147, null));
				return $_out;
			case 666:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7219, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 668:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7240, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7258, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7461, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_149, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7510, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_150, null));
				return $_out;
			case 669:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7241, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7255, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7456, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_149, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7509, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_150, null));
				return $_out;
			case 670:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7242, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7256, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7457, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_149, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7506, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_150, null));
				return $_out;
			case 671:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7243, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7257, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7458, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_149, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7507, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_150, null));
				return $_out;
			case 672:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7228, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 673:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7237, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 674:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7245, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 675:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7246, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 676:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7247, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_151, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7251, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_152, null));
				return $_out;
			case 677:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7248, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 678:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7249, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_151, null));
				return $_out;
			case 679:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7250, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 680:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7260, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7418, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				return $_out;
			case 681:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7259, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7417, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				return $_out;
			case 682:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7254, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7462, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_149, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7511, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_150, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7779, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				return $_out;
			case 683:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7287, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_153, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7362, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_148, null));
				return $_out;
			case 688:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7265, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 689:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7668, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			case 690:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7268, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 691:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7272, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 692:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7283, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_154, null));
				return $_out;
			case 693:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7546, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_155, Bootstrap::$STRINGLIT_156));
				return $_out;
			case 695:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7503, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_157, null));
				return $_out;
			case 697:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7504, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7514, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_150, null));
				return $_out;
			case 698:
				$_out = new JA_L(3, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7460, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_149, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7505, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_150, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7515, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				return $_out;
			case 699:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7302, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 700:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7305, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_158, null));
				return $_out;
			case 701:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7309, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 702:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7313, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_159, null));
				return $_out;
			case 704:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7338, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_160, null));
				return $_out;
			case 705:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7343, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 706:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7348, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_161, null));
				return $_out;
			case 707:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7351, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 708:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7353, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_151, null));
				return $_out;
			case 709:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7357, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_162, null));
				return $_out;
			case 710:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7363, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_155, Bootstrap::$STRINGLIT_156));
				return $_out;
			case 711:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7367, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_155, Bootstrap::$STRINGLIT_156));
				return $_out;
			case 712:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7364, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_163, null));
				return $_out;
			case 715:
				$_out = new JA_L(3, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7459, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_149, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7479, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7508, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_150, null));
				return $_out;
			case 717:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7387, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7388, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			case 723:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7386, null, Bootstrap::$STRINGLIT_139, 4, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 724:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7385, null, Bootstrap::$STRINGLIT_139, 4, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 726:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7403, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_164, null));
				return $_out;
			case 728:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7407, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_165, null));
				return $_out;
			case 729:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7421, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 730:
				$_out = new JA_L(3, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7422, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7463, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_149, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7512, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_150, null));
				return $_out;
			case 731:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7435, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 732:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7437, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 733:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7439, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 734:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7441, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_166, null));
				return $_out;
			case 735:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7446, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 736:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7468, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7513, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_150, null));
				return $_out;
			case 739:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7524, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_167, null));
				return $_out;
			case 740:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7551, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				return $_out;
			case 741:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7537, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 742:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7548, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 743:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7654, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				return $_out;
			case 744:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7563, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				return $_out;
			case 745:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7553, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_155, Bootstrap::$STRINGLIT_156));
				return $_out;
			case 747:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7561, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 748:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7570, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 749:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7581, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 750:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7652, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 751:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7656, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_168, Bootstrap::$STRINGLIT_169));
				return $_out;
			case 754:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7682, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				return $_out;
			default:
				break;
		}
		return null;
	}
	public static function getConstructors1_I__Lj_MemberInfo_(int $p0) {
		$_out = null;
		switch ($p0) {
			case 755:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7697, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_170, null));
				return $_out;
			case 757:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7698, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_171, null));
				return $_out;
			case 758:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7699, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 759:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7700, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 760:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7701, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 761:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7702, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 762:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7704, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_172, null));
				return $_out;
			case 763:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7705, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 764:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8209, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_160, null));
				return $_out;
			case 765:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7706, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 766:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7707, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 767:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7708, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 768:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7709, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 769:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7710, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 770:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7711, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_172, null));
				return $_out;
			case 771:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7712, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 772:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7713, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 773:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7714, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 774:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7715, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 775:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7716, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 776:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7717, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 777:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7718, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 778:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7719, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_172, null));
				return $_out;
			case 779:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7720, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 780:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7721, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_172, null));
				return $_out;
			case 781:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7722, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 782:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7723, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 783:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7724, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 784:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7725, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_173, null));
				return $_out;
			case 785:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7726, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 786:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7727, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 787:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7728, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 788:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7729, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_174, null));
				return $_out;
			case 789:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7730, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_142, null));
				return $_out;
			case 790:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7731, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_175, null));
				return $_out;
			case 791:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7732, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_172, null));
				return $_out;
			case 792:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7733, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 793:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7734, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 794:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7735, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 795:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7736, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 796:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7737, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 797:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7738, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 798:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7739, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 799:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7740, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 800:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7741, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 801:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7742, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 802:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7743, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_176, null));
				return $_out;
			case 803:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7744, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_177, null));
				return $_out;
			case 804:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7745, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 805:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7750, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 806:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7752, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_178, null));
				return $_out;
			case 807:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7753, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_178, null));
				return $_out;
			case 809:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7764, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_179, null));
				return $_out;
			case 810:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7949, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			case 811:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7775, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			case 812:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7778, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				return $_out;
			case 813:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7787, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_180, Bootstrap::$STRINGLIT_181));
				return $_out;
			case 814:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7790, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 815:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7791, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_182, null));
				return $_out;
			case 817:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7793, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7885, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			case 818:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7792, null, Bootstrap::$STRINGLIT_139, 4, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 820:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7794, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_183, Bootstrap::$STRINGLIT_184));
				return $_out;
			case 823:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7849, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_185, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7850, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_186, null));
				return $_out;
			case 824:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7815, null, Bootstrap::$STRINGLIT_139, 4, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 825:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7843, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_186, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7848, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_185, null));
				return $_out;
			case 826:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7842, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_186, null));
				return $_out;
			case 827:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7865, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_187, null));
				return $_out;
			case 828:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7886, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 829:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7888, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 830:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7902, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_188, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7903, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 831:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7894, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 833:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7904, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_188, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7905, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 834:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7906, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_188, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7907, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 835:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7911, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 836:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7913, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 837:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7914, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 838:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7915, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_189, null));
				return $_out;
			case 840:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7922, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 841:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7923, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 842:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7939, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 846:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7944, null, Bootstrap::$STRINGLIT_139, 4, Bootstrap::$STRINGLIT_140, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7945, null, Bootstrap::$STRINGLIT_139, 4, Bootstrap::$STRINGLIT_190, null));
				return $_out;
			case 847:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7941, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 848:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7943, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 849:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7978, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 850:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7986, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 851:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7993, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_148, null));
				return $_out;
			case 853:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7996, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			case 854:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7998, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_191, null));
				return $_out;
			case 855:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(7997, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 856:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8005, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			default:
				break;
		}
		return null;
	}
	public static function getConstructors2_I__Lj_MemberInfo_(int $p0) {
		$_out = null;
		switch ($p0) {
			case 857:
				$_out = new JA_L(3, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8063, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_192, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8156, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_193, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8199, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_172, null));
				return $_out;
			case 858:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8018, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 859:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8020, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			case 861:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8027, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			case 862:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8031, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			case 863:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8035, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_146, null));
				return $_out;
			case 864:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8040, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 866:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8045, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 867:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8047, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_153, null));
				return $_out;
			case 868:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8066, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 869:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8076, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 870:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8089, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_195, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8090, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 871:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8088, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 873:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8093, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 874:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8105, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_196, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8106, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 875:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8104, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 877:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8109, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 878:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8121, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_197, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8122, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 879:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8120, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 881:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8125, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 882:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8135, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_198, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8136, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 883:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8134, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 885:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8139, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 886:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8143, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 887:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8148, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 888:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8151, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_194, null));
				return $_out;
			case 889:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8159, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 898:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8220, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_199, null));
				return $_out;
			case 902:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8291, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_200, null));
				return $_out;
			case 903:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8300, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_201, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8301, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 904:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8302, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_201, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8303, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 911:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8390, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 912:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8383, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 913:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8382, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 914:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8384, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_202, Bootstrap::$STRINGLIT_203));
				return $_out;
			case 916:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8426, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_204, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8427, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_205, null));
				return $_out;
			case 917:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8415, null, Bootstrap::$STRINGLIT_139, 2, Bootstrap::$STRINGLIT_205, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8420, null, Bootstrap::$STRINGLIT_139, 4096, Bootstrap::$STRINGLIT_204, null));
				return $_out;
			case 918:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8412, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_205, null));
				return $_out;
			case 919:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8438, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_206, Bootstrap::$STRINGLIT_207));
				return $_out;
			case 920:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8439, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_187, Bootstrap::$STRINGLIT_208));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8443, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_209, Bootstrap::$STRINGLIT_210));
				return $_out;
			case 921:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8440, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_187, Bootstrap::$STRINGLIT_208));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8442, null, Bootstrap::$STRINGLIT_139, 0, Bootstrap::$STRINGLIT_209, Bootstrap::$STRINGLIT_210));
				return $_out;
			case 922:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8441, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 923:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8479, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			case 924:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(8483, null, Bootstrap::$STRINGLIT_139, 1, Bootstrap::$STRINGLIT_140, null));
				return $_out;
			default:
				break;
		}
		return null;
	}
	public function __construct($CLASS_ID = 750) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_Objects extends java_lang_Object {

	public function java_util_Objects_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function hashCode_Ljava_lang_Object__I(?java_lang_Object $p0) {
		$fI0 = 0;
		if ((($p0 != null))) goto label_1;
		$fI0 = 0;
		goto label_2;
		label_1:
		$fI0 = $p0->hashCode__I();
		label_2:
		return $fI0;
	}
	public static function equals_Ljava_lang_Object_Ljava_lang_Object__Z(?java_lang_Object $p0, ?java_lang_Object $p1) {
		$fI0 = 0;
		if ((($p0 != null))) goto label_1;
		if ((($p1 != null))) goto label_2;
		$fI0 = 1;
		goto label_3;
		label_2:
		$fI0 = 0;
		goto label_3;
		label_1:
		$fI0 = ((int)(N::z2i($p0->equals_Ljava_lang_Object__Z($p1))));
		label_3:
		return (($fI0)!=0);
	}
	public static function requireNonNull_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0) {
		$fA0 = null;
		$tA0 = null;
		if ((($p0 != null))) goto label_1;
		$tA0 = ((new java_lang_NullPointerException()));
		$fA0 = $tA0;
		($tA0)->java_lang_NullPointerException_init___V();
		throw new WrappedThrowable($fA0);
		label_1:
		return $p0;
	}
	public function __construct($CLASS_ID = 749) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_SystemInt extends java_lang_Object {

	public static $___lastId = 0;
	public function java_lang_SystemInt_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function java_lang_SystemInt_clinit___V() {
		java_lang_SystemInt::$___lastId = 1;
		return;
	}
	public static function identityHashCode_Ljava_lang_Object__I(?java_lang_Object $p0) {
		$fI1 = 0;
		$tI0 = 0;
		if ((($p0 == null))) goto label_1;
		if ((($p0->___id != 0))) goto label_3;
		$tI0 = java_lang_SystemInt::$___lastId;
		$fI1 = $tI0;
		java_lang_SystemInt::$___lastId = ((int)(($tI0 + 1)));
		$p0->___id = $fI1;
		label_3:
		return $p0->___id;
		label_1:
		return 0;
	}
	public function __construct($CLASS_ID = 748) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		java_lang_SystemInt::$___lastId = 0;
		java_lang_SystemInt::java_lang_SystemInt_clinit___V();
	}
}
class j_ProgramReflection_DynamicNewInvoke extends java_lang_Object {

	public function j_ProgramReflection_DynamicNewInvoke_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function dynamicNew_II_Ljava_lang_Object__Ljava_lang_Object_(int $p0, int $p1, ?JA_L $p2) {
		if ((($p1 >= 8135))) {
			return j_ProgramReflection_DynamicNewInvoke::dynamicNew2_II_Ljava_lang_Object__Ljava_lang_Object_($p0, $p1, $p2);
		}
		if ((($p1 >= 7700))) {
			return j_ProgramReflection_DynamicNewInvoke::dynamicNew1_II_Ljava_lang_Object__Ljava_lang_Object_($p0, $p1, $p2);
		}
		if ((($p1 >= 7204))) {
			return j_ProgramReflection_DynamicNewInvoke::dynamicNew0_II_Ljava_lang_Object__Ljava_lang_Object_($p0, $p1, $p2);
		}
		return null;
	}
	public static function dynamicNew0_II_Ljava_lang_Object__Ljava_lang_Object_(int $p0, int $p1, ?JA_L $p2) {
		switch ($p1) {
			case 7204:return ((new Benchmark())->Benchmark_init___V()); 
			case 7205:return (new java_lang_Object())->java_lang_Object_init___V(); 
			case 7210:return ((new java_lang_String_CaseInsensitiveComparator())->java_lang_String_CaseInsensitiveComparator_init__Ljava_lang_String_1__V((($p2->get(0))))); 
			case 7211:return ((new java_lang_String_CaseInsensitiveComparator())->java_lang_String_CaseInsensitiveComparator_init___V()); 
			case 7213:return ((new java_lang_String())->java_lang_String_init___CII_V((($p2->get(0))), N::unboxInt((($p2->get(1)))), N::unboxInt((($p2->get(2)))))); 
			case 7214:return ((new java_util_Arrays())->java_util_Arrays_init___V()); 
			case 7216:return ((new java_lang_System())->java_lang_System_init___V()); 
			case 7218:return ((new java_io_FilterOutputStream())->java_io_FilterOutputStream_init__Ljava_io_OutputStream__V((($p2->get(0))))); 
			case 7220:return ((new java_io_PrintStream())->java_io_PrintStream_init__Ljava_io_OutputStream__V((($p2->get(0))))); 
			case 7225:return ((new java_lang_StringBuilder())->java_lang_StringBuilder_init___V()); 
			case 7226:return ((new java_lang_StringBuilder())->java_lang_StringBuilder_init__I_V(N::unboxInt((($p2->get(0)))))); 
			case 7228:return ((new java_lang_jtransc_JTranscStrings())->java_lang_jtransc_JTranscStrings_init___V()); 
			case 7237:return ((new java_lang_Math())->java_lang_Math_init___V()); 
			case 7240:return ((new java_lang_NullPointerException())->java_lang_NullPointerException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7241:return ((new java_lang_RuntimeException())->java_lang_RuntimeException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7242:return ((new java_lang_Exception())->java_lang_Exception_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7243:return ((new java_lang_Throwable())->java_lang_Throwable_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7245:return ((new java_lang_System_1())->java_lang_System_1_init___V()); 
			case 7247:return ((new com_jtransc_io_JTranscConsolePrintStream())->com_jtransc_io_JTranscConsolePrintStream_init__Z_V(N::unboxBool((($p2->get(0)))))); 
			case 7248:return ((new com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream())->com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream_init___V()); 
			case 7250:return ((new com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream())->com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream_init___V()); 
			case 7251:return ((new com_jtransc_io_JTranscConsolePrintStream())->com_jtransc_io_JTranscConsolePrintStream_init__Lcom_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_Z_V((($p2->get(0))), N::unboxBool((($p2->get(1)))))); 
			case 7254:return ((new java_lang_IllegalArgumentException())->java_lang_IllegalArgumentException_init___V()); 
			case 7255:return ((new java_lang_RuntimeException())->java_lang_RuntimeException_init___V()); 
			case 7256:return ((new java_lang_Exception())->java_lang_Exception_init___V()); 
			case 7257:return ((new java_lang_Throwable())->java_lang_Throwable_init___V()); 
			case 7258:return ((new java_lang_NullPointerException())->java_lang_NullPointerException_init___V()); 
			case 7259:return ((new java_lang_IndexOutOfBoundsException())->java_lang_IndexOutOfBoundsException_init___V()); 
			case 7260:return ((new java_lang_ArrayIndexOutOfBoundsException())->java_lang_ArrayIndexOutOfBoundsException_init___V()); 
			case 7265:return ((new java_lang_reflect_Modifier())->java_lang_reflect_Modifier_init___V()); 
			case 7272:return ((new java_lang_IntegerTools())->java_lang_IntegerTools_init___V()); 
			case 7283:return ((new java_lang_Character())->java_lang_Character_init__C_V(N::unboxChar((($p2->get(0)))))); 
			case 7287:return ((new java_lang_Class())->java_lang_Class_init__Ljava_lang_String_Z_V((($p2->get(0))), N::unboxBool((($p2->get(1)))))); 
			case 7302:return ((new java_lang_Void())->java_lang_Void_init___V()); 
			case 7305:return ((new java_lang_Float())->java_lang_Float_init__F_V(N::unboxFloat((($p2->get(0)))))); 
			case 7309:return ((new com_jtransc_text_JTranscStringTools())->com_jtransc_text_JTranscStringTools_init___V()); 
			case 7313:return ((new java_lang_Double())->java_lang_Double_init__D_V(N::unboxDouble((($p2->get(0)))))); 
			case 7338:return ((new java_lang_Long())->java_lang_Long_init__J_V(N::unboxLong((($p2->get(0)))))); 
			case 7343:return ((new com_jtransc_internal_JTranscCType())->com_jtransc_internal_JTranscCType_init___V()); 
			case 7348:return ((new java_lang_Short())->java_lang_Short_init__S_V(N::unboxShort((($p2->get(0)))))); 
			case 7351:return ((new com_jtransc_io_JTranscConsole())->com_jtransc_io_JTranscConsole_init___V()); 
			case 7353:return ((new java_lang_Boolean())->java_lang_Boolean_init__Z_V(N::unboxBool((($p2->get(0)))))); 
			case 7357:return ((new java_lang_Byte())->java_lang_Byte_init__B_V(N::unboxByte((($p2->get(0)))))); 
			case 7362:return ((new java_lang_Class())->java_lang_Class_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7363:return ((new java_lang_reflect_Method())->java_lang_reflect_Method_init__Ljava_lang_Class_Lj_MemberInfo__V((($p2->get(0))), (($p2->get(1))))); 
			case 7364:return ((new j_MemberInfo())->j_MemberInfo_init__ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__V(N::unboxInt((($p2->get(0)))), (($p2->get(1))), (($p2->get(2))), N::unboxInt((($p2->get(3)))), (($p2->get(4))), (($p2->get(5))))); 
			case 7387:return ((new java_util_ArrayList())->java_util_ArrayList_init___V()); 
			case 7388:return ((new java_util_ArrayList())->java_util_ArrayList_init__I_V(N::unboxInt((($p2->get(0)))))); 
			case 7403:return ((new java_util_AbstractList_SimpleListIterator())->java_util_AbstractList_SimpleListIterator_init__Ljava_util_AbstractList__V((($p2->get(0))))); 
			case 7407:return ((new java_util_AbstractList_FullListIterator())->java_util_AbstractList_FullListIterator_init__Ljava_util_AbstractList_I_V((($p2->get(0))), N::unboxInt((($p2->get(1)))))); 
			case 7417:return ((new java_lang_IndexOutOfBoundsException())->java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7418:return ((new java_lang_ArrayIndexOutOfBoundsException())->java_lang_ArrayIndexOutOfBoundsException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7421:return ((new java_util_NoSuchElementException())->java_util_NoSuchElementException_init___V()); 
			case 7422:return ((new java_util_ConcurrentModificationException())->java_util_ConcurrentModificationException_init___V()); 
			case 7435:return ((new java_lang_reflect_Array())->java_lang_reflect_Array_init___V()); 
			case 7437:return ((new java_lang_jtransc_JTranscCoreReflection())->java_lang_jtransc_JTranscCoreReflection_init___V()); 
			case 7439:return ((new j_ProgramReflection())->j_ProgramReflection_init___V()); 
			case 7441:return ((new j_ClassInfo())->j_ClassInfo_init__ILjava_lang_String_Ljava_lang_String_II_I_I_V(N::unboxInt((($p2->get(0)))), (($p2->get(1))), (($p2->get(2))), N::unboxInt((($p2->get(3)))), N::unboxInt((($p2->get(4)))), (($p2->get(5))), (($p2->get(6))))); 
			case 7446:return ((new j_ProgramReflection_AllClasses())->j_ProgramReflection_AllClasses_init___V()); 
			case 7456:return ((new java_lang_RuntimeException())->java_lang_RuntimeException_init__Ljava_lang_Throwable__V((($p2->get(0))))); 
			case 7457:return ((new java_lang_Exception())->java_lang_Exception_init__Ljava_lang_Throwable__V((($p2->get(0))))); 
			case 7458:return ((new java_lang_Throwable())->java_lang_Throwable_init__Ljava_lang_Throwable__V((($p2->get(0))))); 
			case 7459:return ((new java_lang_Error())->java_lang_Error_init__Ljava_lang_Throwable__V((($p2->get(0))))); 
			case 7460:return ((new java_lang_ReflectiveOperationException())->java_lang_ReflectiveOperationException_init__Ljava_lang_Throwable__V((($p2->get(0))))); 
			case 7461:return ((new java_lang_NullPointerException())->java_lang_NullPointerException_init__Ljava_lang_Throwable__V((($p2->get(0))))); 
			case 7462:return ((new java_lang_IllegalArgumentException())->java_lang_IllegalArgumentException_init__Ljava_lang_Throwable__V((($p2->get(0))))); 
			case 7463:return ((new java_util_ConcurrentModificationException())->java_util_ConcurrentModificationException_init__Ljava_lang_Throwable__V((($p2->get(0))))); 
			case 7468:return ((new java_lang_UnsupportedOperationException())->java_lang_UnsupportedOperationException_init___V()); 
			case 7479:return ((new java_lang_Error())->java_lang_Error_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7493:return ((new java_lang_String())->java_lang_String_init___C_V((($p2->get(0))))); 
			case 7504:return ((new java_lang_ClassNotFoundException())->java_lang_ClassNotFoundException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7505:return ((new java_lang_ReflectiveOperationException())->java_lang_ReflectiveOperationException_init__Ljava_lang_String_Ljava_lang_Throwable__V((($p2->get(0))), (($p2->get(1))))); 
			case 7506:return ((new java_lang_Exception())->java_lang_Exception_init__Ljava_lang_String_Ljava_lang_Throwable__V((($p2->get(0))), (($p2->get(1))))); 
			case 7507:return ((new java_lang_Throwable())->java_lang_Throwable_init__Ljava_lang_String_Ljava_lang_Throwable__V((($p2->get(0))), (($p2->get(1))))); 
			case 7508:return ((new java_lang_Error())->java_lang_Error_init__Ljava_lang_String_Ljava_lang_Throwable__V((($p2->get(0))), (($p2->get(1))))); 
			case 7509:return ((new java_lang_RuntimeException())->java_lang_RuntimeException_init__Ljava_lang_String_Ljava_lang_Throwable__V((($p2->get(0))), (($p2->get(1))))); 
			case 7510:return ((new java_lang_NullPointerException())->java_lang_NullPointerException_init__Ljava_lang_String_Ljava_lang_Throwable__V((($p2->get(0))), (($p2->get(1))))); 
			case 7511:return ((new java_lang_IllegalArgumentException())->java_lang_IllegalArgumentException_init__Ljava_lang_String_Ljava_lang_Throwable__V((($p2->get(0))), (($p2->get(1))))); 
			case 7512:return ((new java_util_ConcurrentModificationException())->java_util_ConcurrentModificationException_init__Ljava_lang_String_Ljava_lang_Throwable__V((($p2->get(0))), (($p2->get(1))))); 
			case 7513:return ((new java_lang_UnsupportedOperationException())->java_lang_UnsupportedOperationException_init__Ljava_lang_String_Ljava_lang_Throwable__V((($p2->get(0))), (($p2->get(1))))); 
			case 7514:return ((new java_lang_ClassNotFoundException())->java_lang_ClassNotFoundException_init__Ljava_lang_String_Ljava_lang_Throwable__V((($p2->get(0))), (($p2->get(1))))); 
			case 7515:return ((new java_lang_ReflectiveOperationException())->java_lang_ReflectiveOperationException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7524:return ((new java_lang_StackTraceElement())->java_lang_StackTraceElement_init__Ljava_lang_String_Ljava_lang_String_Ljava_lang_String_I_V((($p2->get(0))), (($p2->get(1))), (($p2->get(2))), N::unboxInt((($p2->get(3)))))); 
			case 7537:return ((new j_ProgramReflection_DynamicGetSet())->j_ProgramReflection_DynamicGetSet_init___V()); 
			case 7546:return ((new java_lang_reflect_Field())->java_lang_reflect_Field_init__Ljava_lang_Class_Lj_MemberInfo__V((($p2->get(0))), (($p2->get(1))))); 
			case 7548:return ((new j_ProgramReflection_AllFields())->j_ProgramReflection_AllFields_init___V()); 
			case 7551:return ((new java_lang_CloneNotSupportedException())->java_lang_CloneNotSupportedException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7553:return ((new java_lang_reflect_Constructor())->java_lang_reflect_Constructor_init__Ljava_lang_Class_Lj_MemberInfo__V((($p2->get(0))), (($p2->get(1))))); 
			case 7561:return ((new j_ProgramReflection_DynamicNewInvoke())->j_ProgramReflection_DynamicNewInvoke_init___V()); 
			case 7563:return ((new java_lang_InstantiationException())->java_lang_InstantiationException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7570:return ((new java_lang_SystemInt())->java_lang_SystemInt_init___V()); 
			case 7581:return ((new java_util_Objects())->java_util_Objects_init___V()); 
			case 7652:return ((new j_ProgramReflection_AllConstructors())->j_ProgramReflection_AllConstructors_init___V()); 
			case 7654:return ((new java_lang_NoSuchMethodException())->java_lang_NoSuchMethodException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7656:return ((new java_util_Arrays_ArrayList())->java_util_Arrays_ArrayList_init___Ljava_lang_Object__V((($p2->get(0))))); 
			case 7668:return ((new java_lang_Integer())->java_lang_Integer_init__I_V(N::unboxInt((($p2->get(0)))))); 
			case 7682:return ((new java_lang_ClassCastException())->java_lang_ClassCastException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7697:return ((new Benchmark_37())->Benchmark_37_init___LBenchmark_MyClass2__V((($p2->get(0))))); 
			case 7698:return ((new Benchmark_MyClass2())->Benchmark_MyClass2_init__Ljava_lang_String_I_V((($p2->get(0))), N::unboxInt((($p2->get(1)))))); 
			case 7699:return ((new Benchmark_36())->Benchmark_36_init___V()); 
			default:
				break;
		}
		return null;
	}
	public static function dynamicNew1_II_Ljava_lang_Object__Ljava_lang_Object_(int $p0, int $p1, ?JA_L $p2) {
		switch ($p1) {
			case 7700:return ((new Benchmark_35())->Benchmark_35_init___V()); 
			case 7701:return ((new Benchmark_34())->Benchmark_34_init___V()); 
			case 7702:return ((new com_jtransc_JTranscSystem())->com_jtransc_JTranscSystem_init___V()); 
			case 7704:return ((new Benchmark_39())->Benchmark_39_init___B_V((($p2->get(0))))); 
			case 7705:return ((new Benchmark_38())->Benchmark_38_init___V()); 
			case 7706:return ((new Benchmark_33())->Benchmark_33_init___V()); 
			case 7707:return ((new Benchmark_32())->Benchmark_32_init___V()); 
			case 7708:return ((new Benchmark_31())->Benchmark_31_init___V()); 
			case 7709:return ((new Benchmark_30())->Benchmark_30_init___V()); 
			case 7710:return ((new java_lang_Runtime())->java_lang_Runtime_init___V()); 
			case 7711:return ((new Benchmark_40())->Benchmark_40_init___B_V((($p2->get(0))))); 
			case 7712:return ((new Benchmark_1())->Benchmark_1_init___V()); 
			case 7713:return ((new Benchmark_2())->Benchmark_2_init___V()); 
			case 7714:return ((new Benchmark_3())->Benchmark_3_init___V()); 
			case 7715:return ((new Benchmark_44())->Benchmark_44_init___V()); 
			case 7716:return ((new Benchmark_4())->Benchmark_4_init___V()); 
			case 7717:return ((new Benchmark_43())->Benchmark_43_init___V()); 
			case 7718:return ((new Benchmark_5())->Benchmark_5_init___V()); 
			case 7719:return ((new Benchmark_42())->Benchmark_42_init___B_V((($p2->get(0))))); 
			case 7720:return ((new Benchmark_6())->Benchmark_6_init___V()); 
			case 7721:return ((new Benchmark_41())->Benchmark_41_init___B_V((($p2->get(0))))); 
			case 7722:return ((new Benchmark_7())->Benchmark_7_init___V()); 
			case 7723:return ((new Benchmark_8())->Benchmark_8_init___V()); 
			case 7724:return ((new Benchmark_9())->Benchmark_9_init___V()); 
			case 7725:return ((new Benchmark_15())->Benchmark_15_init___I_I_V((($p2->get(0))), (($p2->get(1))))); 
			case 7726:return ((new Benchmark_14())->Benchmark_14_init___V()); 
			case 7727:return ((new Benchmark_13())->Benchmark_13_init___V()); 
			case 7728:return ((new Benchmark_12())->Benchmark_12_init___V()); 
			case 7729:return ((new Benchmark_19())->Benchmark_19_init___I_V((($p2->get(0))))); 
			case 7730:return ((new Benchmark_18())->Benchmark_18_init___C_V((($p2->get(0))))); 
			case 7731:return ((new Benchmark_17())->Benchmark_17_init___S_V((($p2->get(0))))); 
			case 7732:return ((new Benchmark_16())->Benchmark_16_init___B_V((($p2->get(0))))); 
			case 7733:return ((new Benchmark_11())->Benchmark_11_init___V()); 
			case 7734:return ((new Benchmark_10())->Benchmark_10_init___V()); 
			case 7735:return ((new Benchmark_26())->Benchmark_26_init___V()); 
			case 7736:return ((new Benchmark_25())->Benchmark_25_init___V()); 
			case 7737:return ((new Benchmark_24())->Benchmark_24_init___V()); 
			case 7738:return ((new Benchmark_23())->Benchmark_23_init___V()); 
			case 7739:return ((new Benchmark_29())->Benchmark_29_init___V()); 
			case 7740:return ((new Benchmark_28())->Benchmark_28_init___V()); 
			case 7741:return ((new Benchmark_27())->Benchmark_27_init___V()); 
			case 7742:return ((new Benchmark_22())->Benchmark_22_init___V()); 
			case 7743:return ((new Benchmark_21())->Benchmark_21_init___D_V((($p2->get(0))))); 
			case 7744:return ((new Benchmark_20())->Benchmark_20_init___F_V((($p2->get(0))))); 
			case 7745:return ((new com_jtransc_JTranscVersion())->com_jtransc_JTranscVersion_init___V()); 
			case 7750:return ((new com_jtransc_time_JTranscClock())->com_jtransc_time_JTranscClock_init___V()); 
			case 7752:return ((new com_jtransc_time_JTranscClock_1())->com_jtransc_time_JTranscClock_1_init__Lcom_jtransc_time_JTranscClock_Impl__V((($p2->get(0))))); 
			case 7753:return ((new com_jtransc_time_JTranscClock_Impl())->com_jtransc_time_JTranscClock_Impl_init__Lcom_jtransc_time_JTranscClock_Impl__V((($p2->get(0))))); 
			case 7768:return ((new java_lang_String())->java_lang_String_init___BII_V((($p2->get(0))), N::unboxInt((($p2->get(1)))), N::unboxInt((($p2->get(2)))))); 
			case 7769:return ((new java_lang_String())->java_lang_String_init___BIILjava_lang_String_Z_V((($p2->get(0))), N::unboxInt((($p2->get(1)))), N::unboxInt((($p2->get(2)))), (($p2->get(3))), N::unboxBool((($p2->get(4)))))); 
			case 7775:return ((new com_jtransc_charset_JTranscCharBuffer())->com_jtransc_charset_JTranscCharBuffer_init__I_V(N::unboxInt((($p2->get(0)))))); 
			case 7778:return ((new java_nio_charset_UnsupportedCharsetException())->java_nio_charset_UnsupportedCharsetException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7779:return ((new java_lang_IllegalArgumentException())->java_lang_IllegalArgumentException_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7787:return ((new java_util_ServiceLoader())->java_util_ServiceLoader_init__Ljava_lang_Class__V((($p2->get(0))))); 
			case 7790:return ((new com_jtransc_charset_charsets_JTranscCharsetIBM866())->com_jtransc_charset_charsets_JTranscCharsetIBM866_init___V()); 
			case 7791:return ((new com_jtransc_charset_JTranscCharsetSingleByte())->com_jtransc_charset_JTranscCharsetSingleByte_init___Ljava_lang_String_Ljava_lang_String__V((($p2->get(0))), (($p2->get(1))))); 
			case 7793:return ((new java_util_HashMap())->java_util_HashMap_init___V()); 
			case 7794:return ((new java_util_HashMap_HashMapEntry())->java_util_HashMap_HashMapEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_HashMap_HashMapEntry__V(($p2->get(0)), ($p2->get(1)), N::unboxInt((($p2->get(2)))), (($p2->get(3))))); 
			case 7843:return ((new java_util_HashMap_EntryIterator())->java_util_HashMap_EntryIterator_init__Ljava_util_HashMap__V((($p2->get(0))))); 
			case 7848:return ((new java_util_HashMap_EntryIterator())->java_util_HashMap_EntryIterator_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V((($p2->get(0))), (($p2->get(1))))); 
			case 7849:return ((new java_util_HashMap_EntrySet())->java_util_HashMap_EntrySet_init__Ljava_util_HashMap_Ljava_util_HashMap_1__V((($p2->get(0))), (($p2->get(1))))); 
			case 7850:return ((new java_util_HashMap_EntrySet())->java_util_HashMap_EntrySet_init__Ljava_util_HashMap__V((($p2->get(0))))); 
			case 7865:return ((new java_lang_AssertionError())->java_lang_AssertionError_init__Ljava_lang_Object__V(($p2->get(0)))); 
			case 7885:return ((new java_util_HashMap())->java_util_HashMap_init__I_V(N::unboxInt((($p2->get(0)))))); 
			case 7886:return ((new java_util_Collections())->java_util_Collections_init___V()); 
			case 7888:return ((new java_util_Collections_1())->java_util_Collections_1_init___V()); 
			case 7894:return ((new java_util_Collections_2())->java_util_Collections_2_init___V()); 
			case 7902:return ((new java_util_Collections_EmptyList())->java_util_Collections_EmptyList_init__Ljava_util_Collections_1__V((($p2->get(0))))); 
			case 7903:return ((new java_util_Collections_EmptyList())->java_util_Collections_EmptyList_init___V()); 
			case 7904:return ((new java_util_Collections_EmptyMap())->java_util_Collections_EmptyMap_init__Ljava_util_Collections_1__V((($p2->get(0))))); 
			case 7905:return ((new java_util_Collections_EmptyMap())->java_util_Collections_EmptyMap_init___V()); 
			case 7906:return ((new java_util_Collections_EmptySet())->java_util_Collections_EmptySet_init__Ljava_util_Collections_1__V((($p2->get(0))))); 
			case 7907:return ((new java_util_Collections_EmptySet())->java_util_Collections_EmptySet_init___V()); 
			case 7911:return ((new com_jtransc_charset_charsets_JTranscCharsetUTF8())->com_jtransc_charset_charsets_JTranscCharsetUTF8_init___V()); 
			case 7913:return ((new com_jtransc_charset_charsets_JTranscCharsetUSASCII())->com_jtransc_charset_charsets_JTranscCharsetUSASCII_init___V()); 
			case 7914:return ((new com_jtransc_charset_charsets_JTranscCharsetUTF16LE())->com_jtransc_charset_charsets_JTranscCharsetUTF16LE_init___V()); 
			case 7922:return ((new com_jtransc_charset_charsets_JTranscCharsetLatin1())->com_jtransc_charset_charsets_JTranscCharsetLatin1_init___V()); 
			case 7923:return ((new com_jtransc_charset_charsets_JTranscCharsetUTF16BE())->com_jtransc_charset_charsets_JTranscCharsetUTF16BE_init___V()); 
			case 7939:return ((new java_lang_Thread())->java_lang_Thread_init___V()); 
			case 7941:return ((new java_lang__ClassInternalUtils())->java_lang__ClassInternalUtils_init___V()); 
			case 7943:return ((new java_lang__ClassInternalUtils_1())->java_lang__ClassInternalUtils_1_init___V()); 
			case 7949:return ((new java_io_ByteArrayOutputStream())->java_io_ByteArrayOutputStream_init__I_V(N::unboxInt((($p2->get(0)))))); 
			case 7978:return ((new com_jtransc_JTranscArrays())->com_jtransc_JTranscArrays_init___V()); 
			case 7986:return ((new com_jtransc_JTranscSystemProperties())->com_jtransc_JTranscSystemProperties_init___V()); 
			case 7993:return ((new Benchmark_MyClass())->Benchmark_MyClass_init__Ljava_lang_String__V((($p2->get(0))))); 
			case 7997:return ((new java_nio_internal_MemoryBlock())->java_nio_internal_MemoryBlock_init___V()); 
			case 8018:return ((new java_nio_ReadOnlyBufferException())->java_nio_ReadOnlyBufferException_init___V()); 
			case 8040:return ((new java_nio_ByteBufferAsFloatBuffer())->java_nio_ByteBufferAsFloatBuffer_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8045:return ((new libcore_io_Memory())->libcore_io_Memory_init___V()); 
			case 8047:return ((new java_nio_ByteOrder())->java_nio_ByteOrder_init__Ljava_lang_String_Z_V((($p2->get(0))), N::unboxBool((($p2->get(1)))))); 
			case 8063:return ((new java_nio_ByteBuffer())->java_nio_ByteBuffer_init__I_BIZ_V(N::unboxInt((($p2->get(0)))), (($p2->get(1))), N::unboxInt((($p2->get(2)))), N::unboxBool((($p2->get(3)))))); 
			case 8066:return ((new java_nio_ByteBufferAsFloatBuffer_BE())->java_nio_ByteBufferAsFloatBuffer_BE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8076:return ((new java_nio_ByteBufferAsFloatBuffer_LE())->java_nio_ByteBufferAsFloatBuffer_LE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8088:return ((new java_nio_ByteBufferAsLongBuffer_LE())->java_nio_ByteBufferAsLongBuffer_LE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8093:return ((new java_nio_ByteBufferAsLongBuffer_BE())->java_nio_ByteBufferAsLongBuffer_BE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8104:return ((new java_nio_ByteBufferAsDoubleBuffer_LE())->java_nio_ByteBufferAsDoubleBuffer_LE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8109:return ((new java_nio_ByteBufferAsDoubleBuffer_BE())->java_nio_ByteBufferAsDoubleBuffer_BE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8120:return ((new java_nio_ByteBufferAsCharBuffer_LE())->java_nio_ByteBufferAsCharBuffer_LE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8125:return ((new java_nio_ByteBufferAsCharBuffer_BE())->java_nio_ByteBufferAsCharBuffer_BE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8134:return ((new java_nio_ByteBufferAsShortBuffer_LE())->java_nio_ByteBufferAsShortBuffer_LE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			default:
				break;
		}
		return null;
	}
	public static function dynamicNew2_II_Ljava_lang_Object__Ljava_lang_Object_(int $p0, int $p1, ?JA_L $p2) {
		switch ($p1) {
			case 8135:return ((new java_nio_ByteBufferAsShortBuffer())->java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer_Ljava_nio_ByteBufferAsShortBuffer_1__V((($p2->get(0))), (($p2->get(1))))); 
			case 8136:return ((new java_nio_ByteBufferAsShortBuffer())->java_nio_ByteBufferAsShortBuffer_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8139:return ((new java_nio_ByteBufferAsShortBuffer_BE())->java_nio_ByteBufferAsShortBuffer_BE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8148:return ((new java_nio_ByteBufferAsIntBuffer_BE())->java_nio_ByteBufferAsIntBuffer_BE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8151:return ((new java_nio_ByteBufferAsIntBuffer_LE())->java_nio_ByteBufferAsIntBuffer_LE_init__Ljava_nio_ByteBuffer__V((($p2->get(0))))); 
			case 8156:return ((new java_nio_ByteBuffer())->java_nio_ByteBuffer_init___BZ_V((($p2->get(0))), N::unboxBool((($p2->get(1)))))); 
			case 8159:return ((new java_util_zip_CRC32())->java_util_zip_CRC32_init___V()); 
			case 8199:return ((new java_nio_ByteBuffer())->java_nio_ByteBuffer_init___B_V((($p2->get(0))))); 
			case 8209:return ((new java_util_Random())->java_util_Random_init__J_V(N::unboxLong((($p2->get(0)))))); 
			case 8220:return ((new com_jtransc_compression_jzlib_Deflate_Config())->com_jtransc_compression_jzlib_Deflate_Config_init__IIIII_V(N::unboxInt((($p2->get(0)))), N::unboxInt((($p2->get(1)))), N::unboxInt((($p2->get(2)))), N::unboxInt((($p2->get(3)))), N::unboxInt((($p2->get(4)))))); 
			case 8291:return ((new java_util_zip_Deflater())->java_util_zip_Deflater_init__IZ_V(N::unboxInt((($p2->get(0)))), N::unboxBool((($p2->get(1)))))); 
			case 8300:return ((new Benchmark_Test1())->Benchmark_Test1_init__LBenchmark_1__V((($p2->get(0))))); 
			case 8301:return ((new Benchmark_Test1())->Benchmark_Test1_init___V()); 
			case 8302:return ((new Benchmark_Test2())->Benchmark_Test2_init__LBenchmark_1__V((($p2->get(0))))); 
			case 8303:return ((new Benchmark_Test2())->Benchmark_Test2_init___V()); 
			case 8383:return ((new java_util_Hashtable())->java_util_Hashtable_init___V()); 
			case 8384:return ((new java_util_Hashtable_HashtableEntry())->java_util_Hashtable_HashtableEntry_init__Ljava_lang_Object_Ljava_lang_Object_ILjava_util_Hashtable_HashtableEntry__V(($p2->get(0)), ($p2->get(1)), N::unboxInt((($p2->get(2)))), (($p2->get(3))))); 
			case 8390:return ((new java_util_Properties())->java_util_Properties_init___V()); 
			case 8415:return ((new java_util_Hashtable_EntryIterator())->java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable__V((($p2->get(0))))); 
			case 8420:return ((new java_util_Hashtable_EntryIterator())->java_util_Hashtable_EntryIterator_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V((($p2->get(0))), (($p2->get(1))))); 
			case 8426:return ((new java_util_Hashtable_EntrySet())->java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable_Ljava_util_Hashtable_1__V((($p2->get(0))), (($p2->get(1))))); 
			case 8427:return ((new java_util_Hashtable_EntrySet())->java_util_Hashtable_EntrySet_init__Ljava_util_Hashtable__V((($p2->get(0))))); 
			case 8438:return ((new java_io_ObjectStreamField())->java_io_ObjectStreamField_init__Ljava_lang_String_Ljava_lang_Class__V((($p2->get(0))), (($p2->get(1))))); 
			case 8439:return ((new java_lang_ref_WeakReference())->java_lang_ref_WeakReference_init__Ljava_lang_Object__V(($p2->get(0)))); 
			case 8441:return ((new java_lang_ref_ReferenceQueue())->java_lang_ref_ReferenceQueue_init___V()); 
			case 8443:return ((new java_lang_ref_WeakReference())->java_lang_ref_WeakReference_init__Ljava_lang_Object_Ljava_lang_ref_ReferenceQueue__V(($p2->get(0)), (($p2->get(1))))); 
			case 8479:return ((new java_lang_SafeVarargs_Impl())->java_lang_SafeVarargs_Impl_init___V()); 
			case 8483:return ((new java_lang_annotation_Annotation_Impl())->java_lang_annotation_Annotation_Impl_init___V()); 
			default:
				break;
		}
		return null;
	}
	public function __construct($CLASS_ID = 747) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_ReflectiveOperationException extends java_lang_Exception {

	public function java_lang_ReflectiveOperationException_init__Ljava_lang_Throwable__V(?java_lang_Throwable $p0) {
		($this)->java_lang_Exception_init__Ljava_lang_Throwable__V($p0);
		return $this;
		return $this;
	}
	public function java_lang_ReflectiveOperationException_init__Ljava_lang_String_Ljava_lang_Throwable__V(?java_lang_String $p0, ?java_lang_Throwable $p1) {
		($this)->java_lang_Exception_init__Ljava_lang_String_Ljava_lang_Throwable__V($p0, $p1);
		return $this;
		return $this;
	}
	public function java_lang_ReflectiveOperationException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_Exception_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 698) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_reflect_InvocationTargetException extends java_lang_ReflectiveOperationException {

	public function __construct($CLASS_ID = 746) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_lang_reflect_AnnotatedElement {

}
class java_lang_reflect_AnnotatedElement_IFields {

	static public function SI() {
	}
}
abstract class java_lang_reflect_AccessibleObject extends java_lang_Object implements java_lang_reflect_AnnotatedElement {

	public $_info = null;
	public function java_lang_reflect_AccessibleObject_init__Lj_MemberInfo__V(?j_MemberInfo $p0) {
		($this)->java_lang_Object_init___V();
		$this->_info = $p0;
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 695) {
		parent::__construct($CLASS_ID);
		$this->_info = null;
	}
	static public function SI() {
	}
}
abstract class java_lang_reflect_MethodConstructor extends java_lang_reflect_AccessibleObject {

	public $_modifiers = 0;
	public $_name = null;
	public $_methodType = null;
	public $_signature = null;
	public $_clazz = null;
	public $_id = 0;
	public $_exceptionTypes = null;
	public $_genericSignature = null;
	public $_slot = 0;
	public function java_lang_reflect_MethodConstructor_init__Ljava_lang_Class_Lj_MemberInfo__V(?java_lang_Class $p0, ?j_MemberInfo $p1) {
		($this)->java_lang_reflect_AccessibleObject_init__Lj_MemberInfo__V($p1);
		$this->_exceptionTypes = new JA_L(0, "[Ljava.lang.Class;");
		$this->_clazz = $p0;
		$this->_id = $p1->_id;
		$this->_slot = $p1->_id;
		$this->_name = $p1->_name;
		$this->_signature = $p1->_desc;
		$this->_genericSignature = $p1->_genericDesc;
		$this->_modifiers = $p1->_modifiers;
		return $this;
		return $this;
	}
	public function toString__Ljava_lang_String_() {
		$lA4 = null;
		$lA7 = null;
		$lI1 = 0;
		$lI3 = 0;
		$lI5 = 0;
		$lI6 = 0;
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		$tA3 = null;
		$tA4 = null;
		$tA5 = null;
		$tA6 = null;
		$tA7 = null;
		$lA2 = null;
		$lI1 = $this->getModifiers__I();
		$lA2 = Bootstrap::$STRINGLIT_23;
		if ((($lI1 == 0))) goto label_1;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$lA2 = ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($lA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect_Modifier::toString_I_Ljava_lang_String_($lI1))->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_211)->toString__Ljava_lang_String_();
		label_1:
		if ((($this->getReturnType__Ljava_lang_Class_() == null))) goto label_2;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA0 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		$lA2 = ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($lA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils::getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(($this->getReturnType__Ljava_lang_Class_())))->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_211)->toString__Ljava_lang_String_();
		label_2:
		$tA2 = ((new java_lang_StringBuilder()));
		$fA0 = $tA2;
		($tA2)->java_lang_StringBuilder_init___V();
		$lA2 = ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($lA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils::getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(($this->getDeclaringClass__Ljava_lang_Class_())))->toString__Ljava_lang_String_();
		if ($this->isConstructor__Z()) goto label_4;
		$tA3 = ((new java_lang_StringBuilder()));
		$fA0 = $tA3;
		($tA3)->java_lang_StringBuilder_init___V();
		$lA2 = ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($lA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_212)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->getName__Ljava_lang_String_())->toString__Ljava_lang_String_();
		label_4:
		$tA4 = ((new java_lang_StringBuilder()));
		$fA0 = $tA4;
		($tA4)->java_lang_StringBuilder_init___V();
		$lA2 = ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($lA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_213)->toString__Ljava_lang_String_();
		$lI3 = 1;
		$lA4 = ($this->getParameterTypes___Ljava_lang_Class_());
		$lI5 = ($lA4)->length;
		$lI6 = 0;
		label_6:
		if ((($lI6 >= $lI5))) goto label_7;
		$lA7 = (($lA4)->get($lI6));
		if ((($lI3 != 0))) goto label_8;
		$tA5 = ((new java_lang_StringBuilder()));
		$fA0 = $tA5;
		($tA5)->java_lang_StringBuilder_init___V();
		$lA2 = ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($lA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_65)->toString__Ljava_lang_String_();
		label_8:
		$tA6 = ((new java_lang_StringBuilder()));
		$fA0 = $tA6;
		($tA6)->java_lang_StringBuilder_init___V();
		$lA2 = ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($lA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils::getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(($lA7)))->toString__Ljava_lang_String_();
		$lI3 = 0;
		$lI6 = ((int)(($lI6 + 1)));
		goto label_6;
		label_7:
		$tA7 = ((new java_lang_StringBuilder()));
		$fA0 = $tA7;
		($tA7)->java_lang_StringBuilder_init___V();
		$lA2 = ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($lA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_1)->toString__Ljava_lang_String_();
		return $lA2;
	}
	public function getModifiers__I() {
		return $this->_modifiers;
	}
	public function getName__Ljava_lang_String_() {
		return null;
	}
	public function isConstructor__Z() {
		throw new Exception("Missing body java.lang.reflect.MethodConstructor.isConstructor()Z");
	}
	public function getReturnType__Ljava_lang_Class_() {
		return null;
	}
	public function methodType__Ljava_lang_reflect_MethodTypeImpl_() {
		if ((($this->_methodType != null))) goto label_1;
		$this->_methodType = java_lang_reflect__InternalUtils::parseMethodType_Ljava_lang_String_Ljava_lang_reflect_Type__Ljava_lang_reflect_MethodTypeImpl_($this->_signature, null);
		label_1:
		return $this->_methodType;
	}
	public function getParameterTypes___Ljava_lang_Class_() {
		return N::checkcast(N::checkcast($this->methodType__Ljava_lang_reflect_MethodTypeImpl_()->_args, "JA_L"), "JA_L");
	}
	public function getDeclaringClass__Ljava_lang_Class_() {
		return $this->_clazz;
	}
	public function __construct($CLASS_ID = 711) {
		parent::__construct($CLASS_ID);
		$this->_modifiers = 0;
		$this->_name = null;
		$this->_methodType = null;
		$this->_signature = null;
		$this->_clazz = null;
		$this->_id = 0;
		$this->_exceptionTypes = null;
		$this->_genericSignature = null;
		$this->_slot = 0;
	}
	static public function SI() {
	}
}
interface java_lang_reflect_Member {

	public function getDeclaringClass__Ljava_lang_Class_();
	public function getName__Ljava_lang_String_();
	public function getModifiers__I();
}
class java_lang_reflect_Member_IFields {

	static public function SI() {
	}
}
interface java_lang_reflect_GenericDeclaration extends java_lang_reflect_AnnotatedElement {

}
class java_lang_reflect_GenericDeclaration_IFields {

	static public function SI() {
	}
}
class java_lang_reflect_Constructor extends java_lang_reflect_MethodConstructor implements java_lang_reflect_Member, java_lang_reflect_GenericDeclaration {

	public function java_lang_reflect_Constructor_init__Ljava_lang_Class_Lj_MemberInfo__V(?java_lang_Class $p0, ?j_MemberInfo $p1) {
		($this)->java_lang_reflect_MethodConstructor_init__Ljava_lang_Class_Lj_MemberInfo__V($p0, $p1);
		return $this;
		return $this;
	}
	public function getName__Ljava_lang_String_() {
		return $this->getDeclaringClass__Ljava_lang_Class_()->getName__Ljava_lang_String_();
	}
	public function getDeclaringClass__Ljava_lang_Class_() {
		return $this->_clazz;
	}
	public function isConstructor__Z() {
		return true;
	}
	public function getParameterTypes___Ljava_lang_Class_() {
		return N::checkcast(N::checkcast($this->methodType__Ljava_lang_reflect_MethodTypeImpl_()->_args, "JA_L"), "JA_L");
	}
	public function newInstance__Ljava_lang_Object__Ljava_lang_Object_(?JA_L $p0) {
		return j_ProgramReflection::dynamicNew_II_Ljava_lang_Object__Ljava_lang_Object_($this->_clazz->_id, $this->_slot, $p0);
	}
	public function __construct($CLASS_ID = 745) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_InstantiationException extends java_lang_ReflectiveOperationException {

	public function java_lang_InstantiationException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_ReflectiveOperationException_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 744) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_NoSuchMethodException extends java_lang_Exception {

	public function java_lang_NoSuchMethodException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_Exception_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 743) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class j_ProgramReflection_AllFields extends java_lang_Object {

	public function j_ProgramReflection_AllFields_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function getFields_I__Lj_MemberInfo_(int $p0) {
		if ((($p0 >= 857))) {
			return j_ProgramReflection_AllFields::getFields2_I__Lj_MemberInfo_($p0);
		}
		if ((($p0 >= 755))) {
			return j_ProgramReflection_AllFields::getFields1_I__Lj_MemberInfo_($p0);
		}
		if ((($p0 >= 649))) {
			return j_ProgramReflection_AllFields::getFields0_I__Lj_MemberInfo_($p0);
		}
		return null;
	}
	public static function getFields0_I__Lj_MemberInfo_(int $p0) {
		$_out = null;
		switch ($p0) {
			case 649:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2250, null, Bootstrap::$STRINGLIT_214, 10, Bootstrap::$STRINGLIT_215, null));
				return $_out;
			case 651:
				$_out = new JA_L(3, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2137, null, Bootstrap::$STRINGLIT_216, 25, Bootstrap::$STRINGLIT_217, Bootstrap::$STRINGLIT_218));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2140, null, Bootstrap::$STRINGLIT_219, 2, Bootstrap::$STRINGLIT_220, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2145, null, Bootstrap::$STRINGLIT_221, 1, Bootstrap::$STRINGLIT_222, null));
				return $_out;
			case 658:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2138, null, Bootstrap::$STRINGLIT_223, 4, Bootstrap::$STRINGLIT_222, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2139, null, Bootstrap::$STRINGLIT_224, 4, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 661:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2150, null, Bootstrap::$STRINGLIT_225, 25, Bootstrap::$STRINGLIT_226, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2151, null, Bootstrap::$STRINGLIT_227, 25, Bootstrap::$STRINGLIT_228, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2152, null, Bootstrap::$STRINGLIT_229, 25, Bootstrap::$STRINGLIT_228, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2538, null, Bootstrap::$STRINGLIT_230, 10, Bootstrap::$STRINGLIT_231, null));
				return $_out;
			case 662:
				$_out = new JA_L(3, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2146, null, Bootstrap::$STRINGLIT_232, 2, Bootstrap::$STRINGLIT_233, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2324, null, Bootstrap::$STRINGLIT_234, 2, Bootstrap::$STRINGLIT_235, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2325, null, Bootstrap::$STRINGLIT_236, 2, Bootstrap::$STRINGLIT_235, null));
				return $_out;
			case 665:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2141, null, Bootstrap::$STRINGLIT_227, 4, Bootstrap::$STRINGLIT_237, null));
				return $_out;
			case 671:
				$_out = new JA_L(8, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2142, null, Bootstrap::$STRINGLIT_238, 2, Bootstrap::$STRINGLIT_235, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2143, null, Bootstrap::$STRINGLIT_239, 10, Bootstrap::$STRINGLIT_240, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2144, null, Bootstrap::$STRINGLIT_241, 2, Bootstrap::$STRINGLIT_233, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2147, null, Bootstrap::$STRINGLIT_242, 2, Bootstrap::$STRINGLIT_235, null));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2148, null, Bootstrap::$STRINGLIT_243, 2, Bootstrap::$STRINGLIT_235, null));
				$_out->set(5, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2149, null, Bootstrap::$STRINGLIT_244, 2, Bootstrap::$STRINGLIT_245, null));
				$_out->set(6, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2241, null, Bootstrap::$STRINGLIT_246, 2, Bootstrap::$STRINGLIT_247, null));
				$_out->set(7, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2247, null, Bootstrap::$STRINGLIT_248, 2, Bootstrap::$STRINGLIT_249, Bootstrap::$STRINGLIT_250));
				return $_out;
			case 676:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2155, null, Bootstrap::$STRINGLIT_251, 16, Bootstrap::$STRINGLIT_235, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2156, null, Bootstrap::$STRINGLIT_252, 16, Bootstrap::$STRINGLIT_253, null));
				return $_out;
			case 678:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2153, null, Bootstrap::$STRINGLIT_254, 1, Bootstrap::$STRINGLIT_255, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2154, null, Bootstrap::$STRINGLIT_251, 18, Bootstrap::$STRINGLIT_235, null));
				return $_out;
			case 683:
				$_out = new JA_L(12, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2157, null, Bootstrap::$STRINGLIT_256, 2, Bootstrap::$STRINGLIT_233, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2158, null, Bootstrap::$STRINGLIT_257, 2, Bootstrap::$STRINGLIT_235, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2159, null, Bootstrap::$STRINGLIT_258, 2, Bootstrap::$STRINGLIT_220, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2184, null, Bootstrap::$STRINGLIT_259, 10, Bootstrap::$STRINGLIT_260, Bootstrap::$STRINGLIT_261));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2228, null, Bootstrap::$STRINGLIT_262, 2, Bootstrap::$STRINGLIT_263, null));
				$_out->set(5, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2229, null, Bootstrap::$STRINGLIT_264, 194, Bootstrap::$STRINGLIT_265, Bootstrap::$STRINGLIT_266));
				$_out->set(6, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2230, null, Bootstrap::$STRINGLIT_267, 2, Bootstrap::$STRINGLIT_268, null));
				$_out->set(7, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2231, null, Bootstrap::$STRINGLIT_269, 2, Bootstrap::$STRINGLIT_268, null));
				$_out->set(8, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2232, null, Bootstrap::$STRINGLIT_270, 2, Bootstrap::$STRINGLIT_263, null));
				$_out->set(9, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2234, null, Bootstrap::$STRINGLIT_271, 1, Bootstrap::$STRINGLIT_220, null));
				$_out->set(10, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2235, null, Bootstrap::$STRINGLIT_272, 1, Bootstrap::$STRINGLIT_273, null));
				$_out->set(11, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2236, null, Bootstrap::$STRINGLIT_274, 1, Bootstrap::$STRINGLIT_275, null));
				return $_out;
			case 689:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2160, null, Bootstrap::$STRINGLIT_221, 2, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2183, null, Bootstrap::$STRINGLIT_276, 25, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_278));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2248, null, Bootstrap::$STRINGLIT_279, 10, Bootstrap::$STRINGLIT_280, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2249, null, Bootstrap::$STRINGLIT_281, 26, Bootstrap::$STRINGLIT_282, null));
				return $_out;
			case 692:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2161, null, Bootstrap::$STRINGLIT_221, 18, Bootstrap::$STRINGLIT_283, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2162, null, Bootstrap::$STRINGLIT_276, 25, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_284));
				return $_out;
			case 693:
				$_out = new JA_L(6, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2163, null, Bootstrap::$STRINGLIT_285, 1, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_286));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2164, null, Bootstrap::$STRINGLIT_256, 1, Bootstrap::$STRINGLIT_233, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2165, null, Bootstrap::$STRINGLIT_258, 4, Bootstrap::$STRINGLIT_220, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2166, null, Bootstrap::$STRINGLIT_287, 129, Bootstrap::$STRINGLIT_233, null));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2242, null, Bootstrap::$STRINGLIT_288, 1, Bootstrap::$STRINGLIT_220, null));
				$_out->set(5, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2243, null, Bootstrap::$STRINGLIT_289, 129, Bootstrap::$STRINGLIT_233, null));
				return $_out;
			case 695:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2227, null, Bootstrap::$STRINGLIT_274, 1, Bootstrap::$STRINGLIT_290, null));
				return $_out;
			case 697:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2233, null, Bootstrap::$STRINGLIT_291, 2, Bootstrap::$STRINGLIT_245, null));
				return $_out;
			case 699:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2167, null, Bootstrap::$STRINGLIT_276, 25, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_292));
				return $_out;
			case 700:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2168, null, Bootstrap::$STRINGLIT_221, 18, Bootstrap::$STRINGLIT_293, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2169, null, Bootstrap::$STRINGLIT_276, 25, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_294));
				return $_out;
			case 702:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2170, null, Bootstrap::$STRINGLIT_221, 18, Bootstrap::$STRINGLIT_215, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2171, null, Bootstrap::$STRINGLIT_276, 25, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_295));
				return $_out;
			case 704:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2172, null, Bootstrap::$STRINGLIT_221, 18, Bootstrap::$STRINGLIT_296, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2173, null, Bootstrap::$STRINGLIT_276, 25, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_297));
				return $_out;
			case 706:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2174, null, Bootstrap::$STRINGLIT_221, 18, Bootstrap::$STRINGLIT_298, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2175, null, Bootstrap::$STRINGLIT_276, 25, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_299));
				return $_out;
			case 708:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2176, null, Bootstrap::$STRINGLIT_276, 25, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_300));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2177, null, Bootstrap::$STRINGLIT_301, 25, Bootstrap::$STRINGLIT_302, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2178, null, Bootstrap::$STRINGLIT_303, 25, Bootstrap::$STRINGLIT_302, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2179, null, Bootstrap::$STRINGLIT_221, 18, Bootstrap::$STRINGLIT_235, null));
				return $_out;
			case 709:
				$_out = new JA_L(3, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2180, null, Bootstrap::$STRINGLIT_304, 26, Bootstrap::$STRINGLIT_305, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2181, null, Bootstrap::$STRINGLIT_221, 18, Bootstrap::$STRINGLIT_306, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2182, null, Bootstrap::$STRINGLIT_276, 25, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_307));
				return $_out;
			case 711:
				$_out = new JA_L(8, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2191, null, Bootstrap::$STRINGLIT_258, 4, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2192, null, Bootstrap::$STRINGLIT_256, 4, Bootstrap::$STRINGLIT_233, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2196, null, Bootstrap::$STRINGLIT_287, 132, Bootstrap::$STRINGLIT_233, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2222, null, Bootstrap::$STRINGLIT_285, 4, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_286));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2223, null, Bootstrap::$STRINGLIT_271, 4, Bootstrap::$STRINGLIT_220, null));
				$_out->set(5, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2224, null, Bootstrap::$STRINGLIT_308, 4, Bootstrap::$STRINGLIT_309, Bootstrap::$STRINGLIT_310));
				$_out->set(6, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2225, null, Bootstrap::$STRINGLIT_289, 132, Bootstrap::$STRINGLIT_233, null));
				$_out->set(7, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2226, null, Bootstrap::$STRINGLIT_288, 4, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 717:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2201, null, Bootstrap::$STRINGLIT_224, 2, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2202, null, Bootstrap::$STRINGLIT_223, 2, Bootstrap::$STRINGLIT_265, null));
				return $_out;
			case 723:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2204, null, Bootstrap::$STRINGLIT_311, 132, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 726:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2203, null, Bootstrap::$STRINGLIT_312, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2205, null, Bootstrap::$STRINGLIT_313, 4112, Bootstrap::$STRINGLIT_314, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2206, null, Bootstrap::$STRINGLIT_315, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2207, null, Bootstrap::$STRINGLIT_316, 0, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 728:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2208, null, Bootstrap::$STRINGLIT_313, 4112, Bootstrap::$STRINGLIT_314, null));
				return $_out;
			case 733:
				$_out = new JA_L(3, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2216, null, Bootstrap::$STRINGLIT_317, 9, Bootstrap::$STRINGLIT_318, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2217, null, Bootstrap::$STRINGLIT_319, 9, Bootstrap::$STRINGLIT_320, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2218, null, Bootstrap::$STRINGLIT_321, 9, Bootstrap::$STRINGLIT_260, Bootstrap::$STRINGLIT_322));
				return $_out;
			case 739:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2237, null, Bootstrap::$STRINGLIT_323, 2, Bootstrap::$STRINGLIT_233, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2238, null, Bootstrap::$STRINGLIT_324, 2, Bootstrap::$STRINGLIT_220, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2239, null, Bootstrap::$STRINGLIT_325, 2, Bootstrap::$STRINGLIT_233, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2240, null, Bootstrap::$STRINGLIT_326, 2, Bootstrap::$STRINGLIT_233, null));
				return $_out;
			case 748:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2244, null, Bootstrap::$STRINGLIT_327, 10, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 751:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2246, null, Bootstrap::$STRINGLIT_122, 18, Bootstrap::$STRINGLIT_265, Bootstrap::$STRINGLIT_328));
				return $_out;
			default:
				break;
		}
		return null;
	}
	public static function getFields1_I__Lj_MemberInfo_(int $p0) {
		$_out = null;
		switch ($p0) {
			case 755:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2255, null, Bootstrap::$STRINGLIT_329, 4112, Bootstrap::$STRINGLIT_330, null));
				return $_out;
			case 757:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2251, null, Bootstrap::$STRINGLIT_122, 1, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2252, null, Bootstrap::$STRINGLIT_331, 1, Bootstrap::$STRINGLIT_233, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2253, null, Bootstrap::$STRINGLIT_332, 1, Bootstrap::$STRINGLIT_233, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2254, null, Bootstrap::$STRINGLIT_333, 1, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 761:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2256, null, Bootstrap::$STRINGLIT_334, 8, Bootstrap::$STRINGLIT_215, null));
				return $_out;
			case 762:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2257, null, Bootstrap::$STRINGLIT_335, 4112, Bootstrap::$STRINGLIT_282, null));
				return $_out;
			case 764:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2378, null, Bootstrap::$STRINGLIT_336, 2, Bootstrap::$STRINGLIT_296, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2379, null, Bootstrap::$STRINGLIT_337, 2, Bootstrap::$STRINGLIT_235, null));
				return $_out;
			case 769:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2271, null, Bootstrap::$STRINGLIT_338, 10, Bootstrap::$STRINGLIT_339, null));
				return $_out;
			case 770:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2258, null, Bootstrap::$STRINGLIT_335, 4112, Bootstrap::$STRINGLIT_282, null));
				return $_out;
			case 778:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2259, null, Bootstrap::$STRINGLIT_340, 4112, Bootstrap::$STRINGLIT_282, null));
				return $_out;
			case 780:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2260, null, Bootstrap::$STRINGLIT_340, 4112, Bootstrap::$STRINGLIT_282, null));
				return $_out;
			case 784:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2261, null, Bootstrap::$STRINGLIT_341, 4112, Bootstrap::$STRINGLIT_273, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2262, null, Bootstrap::$STRINGLIT_342, 4112, Bootstrap::$STRINGLIT_273, null));
				return $_out;
			case 788:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2263, null, Bootstrap::$STRINGLIT_343, 4112, Bootstrap::$STRINGLIT_273, null));
				return $_out;
			case 789:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2264, null, Bootstrap::$STRINGLIT_344, 4112, Bootstrap::$STRINGLIT_222, null));
				return $_out;
			case 790:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2265, null, Bootstrap::$STRINGLIT_345, 4112, Bootstrap::$STRINGLIT_346, null));
				return $_out;
			case 791:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2266, null, Bootstrap::$STRINGLIT_347, 4112, Bootstrap::$STRINGLIT_282, null));
				return $_out;
			case 802:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2267, null, Bootstrap::$STRINGLIT_348, 4112, Bootstrap::$STRINGLIT_349, null));
				return $_out;
			case 803:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2268, null, Bootstrap::$STRINGLIT_350, 4112, Bootstrap::$STRINGLIT_351, null));
				return $_out;
			case 805:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2270, null, Bootstrap::$STRINGLIT_352, 9, Bootstrap::$STRINGLIT_353, null));
				return $_out;
			case 807:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2269, null, Bootstrap::$STRINGLIT_354, 1, Bootstrap::$STRINGLIT_353, null));
				return $_out;
			case 809:
				$_out = new JA_L(6, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2272, null, Bootstrap::$STRINGLIT_355, 18, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2273, null, Bootstrap::$STRINGLIT_356, 18, Bootstrap::$STRINGLIT_220, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2274, null, Bootstrap::$STRINGLIT_357, 18, Bootstrap::$STRINGLIT_293, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2275, null, Bootstrap::$STRINGLIT_358, 18, Bootstrap::$STRINGLIT_320, null));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2276, null, Bootstrap::$STRINGLIT_359, 10, Bootstrap::$STRINGLIT_260, Bootstrap::$STRINGLIT_360));
				$_out->set(5, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2277, null, Bootstrap::$STRINGLIT_361, 10, Bootstrap::$STRINGLIT_235, null));
				return $_out;
			case 810:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2278, null, Bootstrap::$STRINGLIT_362, 4, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2279, null, Bootstrap::$STRINGLIT_363, 4, Bootstrap::$STRINGLIT_282, null));
				return $_out;
			case 811:
				$_out = new JA_L(3, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2280, null, Bootstrap::$STRINGLIT_223, 2, Bootstrap::$STRINGLIT_222, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2281, null, Bootstrap::$STRINGLIT_364, 2, Bootstrap::$STRINGLIT_220, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2282, null, Bootstrap::$STRINGLIT_365, 2, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 812:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2283, null, Bootstrap::$STRINGLIT_366, 2, Bootstrap::$STRINGLIT_233, null));
				return $_out;
			case 813:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2284, null, Bootstrap::$STRINGLIT_367, 18, Bootstrap::$STRINGLIT_277, Bootstrap::$STRINGLIT_368));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2285, null, Bootstrap::$STRINGLIT_369, 18, Bootstrap::$STRINGLIT_370, Bootstrap::$STRINGLIT_371));
				return $_out;
			case 815:
				$_out = new JA_L(3, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2308, null, Bootstrap::$STRINGLIT_372, 16, Bootstrap::$STRINGLIT_306, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2309, null, Bootstrap::$STRINGLIT_373, 16, Bootstrap::$STRINGLIT_233, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2310, null, Bootstrap::$STRINGLIT_374, 16, Bootstrap::$STRINGLIT_375, Bootstrap::$STRINGLIT_376));
				return $_out;
			case 817:
				$_out = new JA_L(9, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2290, null, Bootstrap::$STRINGLIT_377, 128, Bootstrap::$STRINGLIT_378, Bootstrap::$STRINGLIT_379));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2291, null, Bootstrap::$STRINGLIT_11, 130, Bootstrap::$STRINGLIT_220, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2292, null, Bootstrap::$STRINGLIT_380, 26, Bootstrap::$STRINGLIT_381, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2294, null, Bootstrap::$STRINGLIT_382, 128, Bootstrap::$STRINGLIT_383, Bootstrap::$STRINGLIT_384));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2295, null, Bootstrap::$STRINGLIT_365, 128, Bootstrap::$STRINGLIT_220, null));
				$_out->set(5, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2297, null, Bootstrap::$STRINGLIT_311, 128, Bootstrap::$STRINGLIT_220, null));
				$_out->set(6, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2303, null, Bootstrap::$STRINGLIT_385, 130, Bootstrap::$STRINGLIT_386, Bootstrap::$STRINGLIT_387));
				$_out->set(7, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2306, null, Bootstrap::$STRINGLIT_279, 130, Bootstrap::$STRINGLIT_388, Bootstrap::$STRINGLIT_389));
				$_out->set(8, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2307, null, Bootstrap::$STRINGLIT_390, 130, Bootstrap::$STRINGLIT_386, Bootstrap::$STRINGLIT_391));
				return $_out;
			case 818:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2304, null, Bootstrap::$STRINGLIT_392, 0, Bootstrap::$STRINGLIT_388, Bootstrap::$STRINGLIT_389));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2305, null, Bootstrap::$STRINGLIT_390, 0, Bootstrap::$STRINGLIT_386, Bootstrap::$STRINGLIT_391));
				return $_out;
			case 820:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2286, null, Bootstrap::$STRINGLIT_393, 16, Bootstrap::$STRINGLIT_394, Bootstrap::$STRINGLIT_395));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2287, null, Bootstrap::$STRINGLIT_396, 0, Bootstrap::$STRINGLIT_383, Bootstrap::$STRINGLIT_384));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2288, null, Bootstrap::$STRINGLIT_221, 0, Bootstrap::$STRINGLIT_394, Bootstrap::$STRINGLIT_397));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2289, null, Bootstrap::$STRINGLIT_219, 16, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 823:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2293, null, Bootstrap::$STRINGLIT_313, 4112, Bootstrap::$STRINGLIT_398, null));
				return $_out;
			case 825:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2301, null, Bootstrap::$STRINGLIT_313, 4112, Bootstrap::$STRINGLIT_398, null));
				return $_out;
			case 826:
				$_out = new JA_L(5, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2296, null, Bootstrap::$STRINGLIT_399, 0, Bootstrap::$STRINGLIT_383, Bootstrap::$STRINGLIT_384));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2298, null, Bootstrap::$STRINGLIT_400, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2299, null, Bootstrap::$STRINGLIT_313, 4112, Bootstrap::$STRINGLIT_398, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2300, null, Bootstrap::$STRINGLIT_312, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2302, null, Bootstrap::$STRINGLIT_401, 0, Bootstrap::$STRINGLIT_383, Bootstrap::$STRINGLIT_384));
				return $_out;
			case 828:
				$_out = new JA_L(5, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2311, null, Bootstrap::$STRINGLIT_402, 25, Bootstrap::$STRINGLIT_386, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2312, null, Bootstrap::$STRINGLIT_403, 26, Bootstrap::$STRINGLIT_404, Bootstrap::$STRINGLIT_405));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2313, null, Bootstrap::$STRINGLIT_406, 26, Bootstrap::$STRINGLIT_407, Bootstrap::$STRINGLIT_408));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2314, null, Bootstrap::$STRINGLIT_409, 25, Bootstrap::$STRINGLIT_375, null));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2315, null, Bootstrap::$STRINGLIT_410, 25, Bootstrap::$STRINGLIT_370, null));
				return $_out;
			case 838:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2316, null, Bootstrap::$STRINGLIT_411, 2, Bootstrap::$STRINGLIT_235, null));
				return $_out;
			case 842:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2317, null, Bootstrap::$STRINGLIT_256, 1, Bootstrap::$STRINGLIT_233, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2318, null, Bootstrap::$STRINGLIT_412, 2, Bootstrap::$STRINGLIT_413, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2319, null, Bootstrap::$STRINGLIT_414, 10, Bootstrap::$STRINGLIT_415, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2320, null, Bootstrap::$STRINGLIT_416, 2, Bootstrap::$STRINGLIT_417, null));
				return $_out;
			case 846:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2321, null, Bootstrap::$STRINGLIT_418, 2, Bootstrap::$STRINGLIT_249, Bootstrap::$STRINGLIT_419));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2322, null, Bootstrap::$STRINGLIT_354, 2, Bootstrap::$STRINGLIT_417, null));
				return $_out;
			case 847:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2323, null, Bootstrap::$STRINGLIT_416, 10, Bootstrap::$STRINGLIT_417, null));
				return $_out;
			case 849:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2326, null, Bootstrap::$STRINGLIT_420, 25, Bootstrap::$STRINGLIT_282, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2327, null, Bootstrap::$STRINGLIT_421, 25, Bootstrap::$STRINGLIT_309, Bootstrap::$STRINGLIT_310));
				return $_out;
			case 851:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2328, null, Bootstrap::$STRINGLIT_333, 1, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2329, null, Bootstrap::$STRINGLIT_332, 1, Bootstrap::$STRINGLIT_233, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2330, null, Bootstrap::$STRINGLIT_331, 1, Bootstrap::$STRINGLIT_233, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2331, null, Bootstrap::$STRINGLIT_122, 1, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 854:
				$_out = new JA_L(6, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2333, null, Bootstrap::$STRINGLIT_422, 16, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2334, null, Bootstrap::$STRINGLIT_423, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2335, null, Bootstrap::$STRINGLIT_424, 16, Bootstrap::$STRINGLIT_425, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2336, null, Bootstrap::$STRINGLIT_364, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2337, null, Bootstrap::$STRINGLIT_426, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(5, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2338, null, Bootstrap::$STRINGLIT_427, 16, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			default:
				break;
		}
		return null;
	}
	public static function getFields2_I__Lj_MemberInfo_(int $p0) {
		$_out = null;
		switch ($p0) {
			case 857:
				$_out = new JA_L(7, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2341, null, Bootstrap::$STRINGLIT_428, 16, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2342, null, Bootstrap::$STRINGLIT_429, 17, Bootstrap::$STRINGLIT_282, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2343, null, Bootstrap::$STRINGLIT_430, 18, Bootstrap::$STRINGLIT_235, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2358, null, Bootstrap::$STRINGLIT_431, 0, Bootstrap::$STRINGLIT_235, null));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2359, null, Bootstrap::$STRINGLIT_432, 0, Bootstrap::$STRINGLIT_235, null));
				$_out->set(5, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2360, null, Bootstrap::$STRINGLIT_433, 0, Bootstrap::$STRINGLIT_434, null));
				$_out->set(6, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2372, null, Bootstrap::$STRINGLIT_435, 2, Bootstrap::$STRINGLIT_235, null));
				return $_out;
			case 864:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2348, null, Bootstrap::$STRINGLIT_436, 16, Bootstrap::$STRINGLIT_437, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2349, null, Bootstrap::$STRINGLIT_438, 16, Bootstrap::$STRINGLIT_282, null));
				return $_out;
			case 866:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2356, null, Bootstrap::$STRINGLIT_439, 10, Bootstrap::$STRINGLIT_434, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2357, null, Bootstrap::$STRINGLIT_440, 10, Bootstrap::$STRINGLIT_434, null));
				return $_out;
			case 867:
				$_out = new JA_L(6, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2350, null, Bootstrap::$STRINGLIT_256, 18, Bootstrap::$STRINGLIT_233, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2351, null, Bootstrap::$STRINGLIT_441, 16, Bootstrap::$STRINGLIT_235, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2352, null, Bootstrap::$STRINGLIT_40, 25, Bootstrap::$STRINGLIT_434, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2353, null, Bootstrap::$STRINGLIT_442, 26, Bootstrap::$STRINGLIT_434, null));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2354, null, Bootstrap::$STRINGLIT_39, 25, Bootstrap::$STRINGLIT_434, null));
				$_out->set(5, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2355, null, Bootstrap::$STRINGLIT_431, 26, Bootstrap::$STRINGLIT_235, null));
				return $_out;
			case 870:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2361, null, Bootstrap::$STRINGLIT_438, 16, Bootstrap::$STRINGLIT_282, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2362, null, Bootstrap::$STRINGLIT_436, 16, Bootstrap::$STRINGLIT_437, null));
				return $_out;
			case 874:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2363, null, Bootstrap::$STRINGLIT_438, 16, Bootstrap::$STRINGLIT_282, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2364, null, Bootstrap::$STRINGLIT_436, 16, Bootstrap::$STRINGLIT_437, null));
				return $_out;
			case 878:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2365, null, Bootstrap::$STRINGLIT_438, 16, Bootstrap::$STRINGLIT_282, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2366, null, Bootstrap::$STRINGLIT_436, 16, Bootstrap::$STRINGLIT_437, null));
				return $_out;
			case 882:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2367, null, Bootstrap::$STRINGLIT_438, 16, Bootstrap::$STRINGLIT_282, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2368, null, Bootstrap::$STRINGLIT_436, 16, Bootstrap::$STRINGLIT_437, null));
				return $_out;
			case 886:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2369, null, Bootstrap::$STRINGLIT_436, 16, Bootstrap::$STRINGLIT_437, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2370, null, Bootstrap::$STRINGLIT_438, 16, Bootstrap::$STRINGLIT_282, null));
				return $_out;
			case 889:
				$_out = new JA_L(3, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2375, null, Bootstrap::$STRINGLIT_352, 2, Bootstrap::$STRINGLIT_443, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2376, null, Bootstrap::$STRINGLIT_444, 0, Bootstrap::$STRINGLIT_296, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2377, null, Bootstrap::$STRINGLIT_445, 10, Bootstrap::$STRINGLIT_282, null));
				return $_out;
			case 898:
				$_out = new JA_L(5, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2411, null, Bootstrap::$STRINGLIT_446, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2412, null, Bootstrap::$STRINGLIT_447, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2413, null, Bootstrap::$STRINGLIT_448, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2414, null, Bootstrap::$STRINGLIT_449, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2415, null, Bootstrap::$STRINGLIT_450, 0, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 902:
				$_out = new JA_L(8, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2491, null, Bootstrap::$STRINGLIT_352, 0, Bootstrap::$STRINGLIT_451, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2492, null, Bootstrap::$STRINGLIT_452, 2, Bootstrap::$STRINGLIT_220, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2493, null, Bootstrap::$STRINGLIT_453, 2, Bootstrap::$STRINGLIT_220, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2494, null, Bootstrap::$STRINGLIT_454, 2, Bootstrap::$STRINGLIT_235, null));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2495, null, Bootstrap::$STRINGLIT_455, 2, Bootstrap::$STRINGLIT_220, null));
				$_out->set(5, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2496, null, Bootstrap::$STRINGLIT_456, 2, Bootstrap::$STRINGLIT_296, null));
				$_out->set(6, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2497, null, Bootstrap::$STRINGLIT_457, 2, Bootstrap::$STRINGLIT_220, null));
				$_out->set(7, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2498, null, Bootstrap::$STRINGLIT_458, 2, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 911:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2539, null, Bootstrap::$STRINGLIT_459, 4, Bootstrap::$STRINGLIT_231, null));
				return $_out;
			case 912:
				$_out = new JA_L(9, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2518, null, Bootstrap::$STRINGLIT_11, 130, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2519, null, Bootstrap::$STRINGLIT_377, 130, Bootstrap::$STRINGLIT_460, Bootstrap::$STRINGLIT_461));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2520, null, Bootstrap::$STRINGLIT_380, 26, Bootstrap::$STRINGLIT_381, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2521, null, Bootstrap::$STRINGLIT_365, 130, Bootstrap::$STRINGLIT_220, null));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2522, null, Bootstrap::$STRINGLIT_311, 130, Bootstrap::$STRINGLIT_220, null));
				$_out->set(5, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2530, null, Bootstrap::$STRINGLIT_385, 130, Bootstrap::$STRINGLIT_386, Bootstrap::$STRINGLIT_387));
				$_out->set(6, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2531, null, Bootstrap::$STRINGLIT_390, 130, Bootstrap::$STRINGLIT_386, Bootstrap::$STRINGLIT_391));
				$_out->set(7, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2532, null, Bootstrap::$STRINGLIT_279, 130, Bootstrap::$STRINGLIT_388, Bootstrap::$STRINGLIT_389));
				$_out->set(8, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2536, null, Bootstrap::$STRINGLIT_462, 26, Bootstrap::$STRINGLIT_463, null));
				return $_out;
			case 914:
				$_out = new JA_L(4, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2514, null, Bootstrap::$STRINGLIT_221, 0, Bootstrap::$STRINGLIT_394, Bootstrap::$STRINGLIT_397));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2515, null, Bootstrap::$STRINGLIT_396, 0, Bootstrap::$STRINGLIT_464, Bootstrap::$STRINGLIT_465));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2516, null, Bootstrap::$STRINGLIT_393, 16, Bootstrap::$STRINGLIT_394, Bootstrap::$STRINGLIT_395));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2517, null, Bootstrap::$STRINGLIT_219, 16, Bootstrap::$STRINGLIT_220, null));
				return $_out;
			case 916:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2523, null, Bootstrap::$STRINGLIT_313, 4112, Bootstrap::$STRINGLIT_466, null));
				return $_out;
			case 917:
				$_out = new JA_L(1, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2528, null, Bootstrap::$STRINGLIT_313, 4112, Bootstrap::$STRINGLIT_466, null));
				return $_out;
			case 918:
				$_out = new JA_L(5, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2524, null, Bootstrap::$STRINGLIT_312, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2525, null, Bootstrap::$STRINGLIT_400, 0, Bootstrap::$STRINGLIT_220, null));
				$_out->set(2, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2526, null, Bootstrap::$STRINGLIT_313, 4112, Bootstrap::$STRINGLIT_466, null));
				$_out->set(3, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2527, null, Bootstrap::$STRINGLIT_399, 0, Bootstrap::$STRINGLIT_464, Bootstrap::$STRINGLIT_465));
				$_out->set(4, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2529, null, Bootstrap::$STRINGLIT_401, 0, Bootstrap::$STRINGLIT_464, Bootstrap::$STRINGLIT_465));
				return $_out;
			case 919:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2533, null, Bootstrap::$STRINGLIT_256, 2, Bootstrap::$STRINGLIT_233, null));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2534, null, Bootstrap::$STRINGLIT_467, 2, Bootstrap::$STRINGLIT_394, null));
				return $_out;
			case 921:
				$_out = new JA_L(2, "[Lj.MemberInfo;");
				$_out->set(0, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2535, null, Bootstrap::$STRINGLIT_468, 2, Bootstrap::$STRINGLIT_394, Bootstrap::$STRINGLIT_469));
				$_out->set(1, j_MemberInfo::create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(2537, null, Bootstrap::$STRINGLIT_470, 64, Bootstrap::$STRINGLIT_471, Bootstrap::$STRINGLIT_472));
				return $_out;
			default:
				break;
		}
		return null;
	}
	public function __construct($CLASS_ID = 742) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class j_ProgramReflection_DynamicGetSet extends java_lang_Object {

	public function j_ProgramReflection_DynamicGetSet_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function dynamicSet_IILjava_lang_Object_Ljava_lang_Object__V(int $p0, int $p1, ?java_lang_Object $p2, ?java_lang_Object $p3) {
		if ((($p1 >= 2368))) {
			j_ProgramReflection_DynamicGetSet::dynamicSet2_IILjava_lang_Object_Ljava_lang_Object__V($p0, $p1, $p2, $p3);
			return;
		}
		if ((($p1 >= 2261))) {
			j_ProgramReflection_DynamicGetSet::dynamicSet1_IILjava_lang_Object_Ljava_lang_Object__V($p0, $p1, $p2, $p3);
			return;
		}
		if ((($p1 >= 2137))) {
			j_ProgramReflection_DynamicGetSet::dynamicSet0_IILjava_lang_Object_Ljava_lang_Object__V($p0, $p1, $p2, $p3);
			return;
		}
	}
	public static function dynamicGet_IILjava_lang_Object__Ljava_lang_Object_(int $p0, int $p1, ?java_lang_Object $p2) {
		if ((($p1 >= 2368))) {
			return j_ProgramReflection_DynamicGetSet::dynamicGet2_IILjava_lang_Object__Ljava_lang_Object_($p0, $p1, $p2);
		}
		if ((($p1 >= 2261))) {
			return j_ProgramReflection_DynamicGetSet::dynamicGet1_IILjava_lang_Object__Ljava_lang_Object_($p0, $p1, $p2);
		}
		if ((($p1 >= 2137))) {
			return j_ProgramReflection_DynamicGetSet::dynamicGet0_IILjava_lang_Object__Ljava_lang_Object_($p0, $p1, $p2);
		}
		return null;
	}
	public static function dynamicGet0_IILjava_lang_Object__Ljava_lang_Object_(int $p0, int $p1, ?java_lang_Object $p2) {
		switch ($p1) {
			case 2137:return (java_lang_String::$_CASE_INSENSITIVE_ORDER); 
			case 2138:return (($p2)->_buffer); 
			case 2139:return N::boxInt(($p2)->_length); 
			case 2140:return N::boxInt(($p2)->_hash); 
			case 2141:return (($p2)->__out); 
			case 2142:return N::boxBool(($p2)->_thrown); 
			case 2143:return (java_lang_Throwable::$_EMPTY_ARRAY); 
			case 2144:return (($p2)->_message); 
			case 2145:return (($p2)->_value); 
			case 2146:return (($p2)->_encoding); 
			case 2147:return N::boxBool(($p2)->_writableStackTrace); 
			case 2148:return N::boxBool(($p2)->_enableSuppression); 
			case 2149:return (($p2)->_cause); 
			case 2150:return (java_lang_System::$__in); 
			case 2151:return (java_lang_System::$__out); 
			case 2152:return (java_lang_System::$_err); 
			case 2153:return (($p2)->_sb); 
			case 2154:return N::boxBool(($p2)->_error); 
			case 2155:return N::boxBool(($p2)->_error); 
			case 2156:return (($p2)->_stream); 
			case 2157:return (($p2)->_name); 
			case 2158:return N::boxBool(($p2)->_primitive); 
			case 2159:return N::boxInt(($p2)->_modifiers); 
			case 2160:return N::boxInt(($p2)->_value); 
			case 2161:return N::boxChar(($p2)->_value); 
			case 2162:return (java_lang_Character::$_TYPE); 
			case 2163:return (($p2)->_clazz); 
			case 2164:return (($p2)->_name); 
			case 2165:return N::boxInt(($p2)->_modifiers); 
			case 2166:return (($p2)->_signature); 
			case 2167:return (java_lang_Void::$_TYPE); 
			case 2168:return N::boxFloat(($p2)->_value); 
			case 2169:return (java_lang_Float::$_TYPE); 
			case 2170:return N::boxDouble(($p2)->_value); 
			case 2171:return (java_lang_Double::$_TYPE); 
			case 2172:return N::boxLong(($p2)->_value); 
			case 2173:return (java_lang_Long::$_TYPE); 
			case 2174:return N::boxShort(($p2)->_value); 
			case 2175:return (java_lang_Short::$_TYPE); 
			case 2176:return (java_lang_Boolean::$_TYPE); 
			case 2177:return (java_lang_Boolean::$_TRUE); 
			case 2178:return (java_lang_Boolean::$_FALSE); 
			case 2179:return N::boxBool(($p2)->_value); 
			case 2180:return (java_lang_Byte::$_cache); 
			case 2181:return N::boxByte(($p2)->_value); 
			case 2182:return (java_lang_Byte::$_TYPE); 
			case 2183:return (java_lang_Integer::$_TYPE); 
			case 2184:return (java_lang_Class::$__classCache); 
			case 2191:return N::boxInt(($p2)->_modifiers); 
			case 2192:return (($p2)->_name); 
			case 2196:return (($p2)->_signature); 
			case 2201:return N::boxInt(($p2)->_length); 
			case 2202:return (($p2)->_buffer); 
			case 2203:return N::boxInt(($p2)->_expectedModCount); 
			case 2204:return N::boxInt(($p2)->_modCount); 
			case 2205:return (($p2)->_this_0); 
			case 2206:return N::boxInt(($p2)->_pos); 
			case 2207:return N::boxInt(($p2)->_lastPosition); 
			case 2208:return (($p2)->_this_0_); 
			case 2216:return (j_ProgramReflection::$__classInfos); 
			case 2217:return (j_ProgramReflection::$__classNames); 
			case 2218:return (j_ProgramReflection::$__classInfosByName); 
			case 2222:return (($p2)->_clazz); 
			case 2223:return N::boxInt(($p2)->_id); 
			case 2224:return (($p2)->_exceptionTypes); 
			case 2225:return (($p2)->_genericSignature); 
			case 2226:return N::boxInt(($p2)->_slot); 
			case 2227:return (($p2)->_info); 
			case 2228:return (($p2)->__accessibleMethods); 
			case 2229:return (($p2)->_enumConstants); 
			case 2230:return (($p2)->__allFields); 
			case 2231:return (($p2)->__accessibleFields); 
			case 2232:return (($p2)->__allMethods); 
			case 2233:return (($p2)->_ex); 
			case 2234:return N::boxInt(($p2)->_id); 
			case 2235:return (($p2)->_related); 
			case 2236:return (($p2)->_info); 
			case 2237:return (($p2)->_fileName); 
			case 2238:return N::boxInt(($p2)->_lineNumber); 
			case 2239:return (($p2)->_methodName); 
			case 2240:return (($p2)->_declaringClass); 
			case 2241:return (($p2)->_stackTrace); 
			case 2242:return N::boxInt(($p2)->_slot); 
			case 2243:return (($p2)->_genericSignature); 
			case 2244:return N::boxInt(java_lang_SystemInt::$___lastId); 
			case 2246:return (($p2)->_a); 
			case 2247:return (($p2)->_supressed); 
			case 2248:return (java_lang_Integer::$_values); 
			case 2249:return (java_lang_Integer::$_NTZ_TABLE); 
			case 2250:return N::boxDouble(Benchmark::$_totalTime); 
			case 2251:return N::boxInt(($p2)->_a); 
			case 2252:return (($p2)->_c); 
			case 2253:return (($p2)->_d); 
			case 2254:return N::boxInt(($p2)->_b); 
			case 2255:return (($p2)->_val_objects); 
			case 2256:return N::boxDouble(com_jtransc_JTranscSystem::$_start); 
			case 2257:return (($p2)->_val_hexData); 
			case 2258:return (($p2)->_val_hexData); 
			case 2259:return (($p2)->_val_bytes); 
			case 2260:return (($p2)->_val_bytes); 
			default:
				break;
		}
		return null;
	}
	public static function dynamicGet1_IILjava_lang_Object__Ljava_lang_Object_(int $p0, int $p1, ?java_lang_Object $p2) {
		switch ($p1) {
			case 2261:return (($p2)->_val_srcI); 
			case 2262:return (($p2)->_val_dstI); 
			case 2263:return (($p2)->_val_iarray); 
			case 2264:return (($p2)->_val_carray); 
			case 2265:return (($p2)->_val_sarray); 
			case 2266:return (($p2)->_val_barray); 
			case 2267:return (($p2)->_val_darray); 
			case 2268:return (($p2)->_val_farray); 
			case 2269:return (($p2)->_parent); 
			case 2270:return (com_jtransc_time_JTranscClock::$_impl); 
			case 2271:return (java_lang_Runtime::$_current); 
			case 2272:return N::boxInt(($p2)->_max); 
			case 2273:return N::boxInt(($p2)->_min); 
			case 2274:return N::boxFloat(($p2)->_avg); 
			case 2275:return (($p2)->_names); 
			case 2276:return (com_jtransc_charset_JTranscCharset::$_charsets); 
			case 2277:return N::boxBool(com_jtransc_charset_JTranscCharset::$_loadedCharsets); 
			case 2278:return N::boxInt(($p2)->_count); 
			case 2279:return (($p2)->_buf); 
			case 2280:return (($p2)->_buffer); 
			case 2281:return N::boxInt(($p2)->_position); 
			case 2282:return N::boxInt(($p2)->_size); 
			case 2283:return (($p2)->_charsetName); 
			case 2284:return (($p2)->_service); 
			case 2285:return (($p2)->_list); 
			case 2286:return ($p2)->_key; 
			case 2287:return (($p2)->_next); 
			case 2288:return ($p2)->_value; 
			case 2289:return N::boxInt(($p2)->_hash); 
			case 2290:return (($p2)->_table); 
			case 2291:return N::boxInt(($p2)->_threshold); 
			case 2292:return (java_util_HashMap::$_EMPTY_TABLE); 
			case 2293:return (($p2)->_this_0); 
			case 2294:return (($p2)->_entryForNullKey); 
			case 2295:return N::boxInt(($p2)->_size); 
			case 2296:return (($p2)->_nextEntry); 
			case 2297:return N::boxInt(($p2)->_modCount); 
			case 2298:return N::boxInt(($p2)->_nextIndex); 
			case 2299:return (($p2)->_this_0); 
			case 2300:return N::boxInt(($p2)->_expectedModCount); 
			case 2301:return (($p2)->_this_0_); 
			case 2302:return (($p2)->_lastEntryReturned); 
			case 2303:return (($p2)->_entrySet); 
			case 2304:return (($p2)->_valuesCollection); 
			case 2305:return (($p2)->_keySet); 
			case 2306:return (($p2)->_values); 
			case 2307:return (($p2)->_keySet_); 
			case 2308:return N::boxByte(($p2)->_invalidChar); 
			case 2309:return (($p2)->_decode); 
			case 2310:return (($p2)->_encode); 
			case 2311:return (java_util_Collections::$_EMPTY_SET); 
			case 2312:return (java_util_Collections::$_EMPTY_ITERATOR); 
			case 2313:return (java_util_Collections::$_EMPTY_ENUMERATION); 
			case 2314:return (java_util_Collections::$_EMPTY_MAP); 
			case 2315:return (java_util_Collections::$_EMPTY_LIST); 
			case 2316:return N::boxBool(($p2)->_littleEndian); 
			case 2317:return (($p2)->_name); 
			case 2318:return (($p2)->_group); 
			case 2319:return (java_lang_Thread::$__currentThread); 
			case 2320:return (($p2)->_classLoader); 
			case 2321:return (($p2)->_nativeLibs); 
			case 2322:return (($p2)->_parent); 
			case 2323:return (java_lang__ClassInternalUtils::$_classLoader); 
			case 2324:return N::boxBool(($p2)->_autoFlush); 
			case 2325:return N::boxBool(($p2)->_ioError); 
			case 2326:return (com_jtransc_JTranscArrays::$_EMPTY_BYTE); 
			case 2327:return (com_jtransc_JTranscArrays::$_EMPTY_CLASS); 
			case 2328:return N::boxInt(($p2)->_b); 
			case 2329:return (($p2)->_d); 
			case 2330:return (($p2)->_c); 
			case 2331:return N::boxInt(($p2)->_a); 
			case 2333:return N::boxInt(($p2)->__elementSizeShift); 
			case 2334:return N::boxInt(($p2)->_mark); 
			case 2335:return (($p2)->_block); 
			case 2336:return N::boxInt(($p2)->_position); 
			case 2337:return N::boxInt(($p2)->_limit); 
			case 2338:return N::boxInt(($p2)->_capacity); 
			case 2341:return N::boxInt(($p2)->_arrayOffset); 
			case 2342:return (($p2)->_backingArray); 
			case 2343:return N::boxBool(($p2)->_isReadOnly); 
			case 2348:return (($p2)->_byteBuffer); 
			case 2349:return (($p2)->_bytes); 
			case 2350:return (($p2)->_name); 
			case 2351:return N::boxBool(($p2)->_needsSwap); 
			case 2352:return (java_nio_ByteOrder::$_LITTLE_ENDIAN); 
			case 2353:return (java_nio_ByteOrder::$_NATIVE_ORDER); 
			case 2354:return (java_nio_ByteOrder::$_BIG_ENDIAN); 
			case 2355:return N::boxBool(java_nio_ByteOrder::$_isLittleEndian); 
			case 2356:return (libcore_io_Memory::$_SWAPPED); 
			case 2357:return (libcore_io_Memory::$_NATIVE); 
			case 2358:return N::boxBool(($p2)->_isLittleEndian); 
			case 2359:return N::boxBool(($p2)->_isNativeOrder); 
			case 2360:return (($p2)->_order); 
			case 2361:return (($p2)->_bytes); 
			case 2362:return (($p2)->_byteBuffer); 
			case 2363:return (($p2)->_bytes); 
			case 2364:return (($p2)->_byteBuffer); 
			case 2365:return (($p2)->_bytes); 
			case 2366:return (($p2)->_byteBuffer); 
			case 2367:return (($p2)->_bytes); 
			default:
				break;
		}
		return null;
	}
	public static function dynamicGet2_IILjava_lang_Object__Ljava_lang_Object_(int $p0, int $p1, ?java_lang_Object $p2) {
		switch ($p1) {
			case 2368:return (($p2)->_byteBuffer); 
			case 2369:return (($p2)->_byteBuffer); 
			case 2370:return (($p2)->_bytes); 
			case 2372:return N::boxBool(($p2)->_isDirect); 
			case 2375:return (($p2)->_impl); 
			case 2376:return N::boxLong(($p2)->_tbytes); 
			case 2377:return (java_util_zip_CRC32::$_temp); 
			case 2378:return N::boxLong(($p2)->_seed); 
			case 2379:return N::boxBool(($p2)->_haveNextNextGaussian); 
			case 2411:return N::boxInt(($p2)->_nice_length); 
			case 2412:return N::boxInt(($p2)->_max_chain); 
			case 2413:return N::boxInt(($p2)->_max_lazy); 
			case 2414:return N::boxInt(($p2)->_good_length); 
			case 2415:return N::boxInt(($p2)->_func); 
			case 2491:return (($p2)->_impl); 
			case 2492:return N::boxInt(($p2)->_inLength); 
			case 2493:return N::boxInt(($p2)->_inRead); 
			case 2494:return N::boxBool(($p2)->_noHeader); 
			case 2495:return N::boxInt(($p2)->_compressLevel); 
			case 2496:return N::boxLong(($p2)->_streamHandle); 
			case 2497:return N::boxInt(($p2)->_flushParm); 
			case 2498:return N::boxInt(($p2)->_strategy); 
			case 2514:return ($p2)->_value; 
			case 2515:return (($p2)->_next); 
			case 2516:return ($p2)->_key; 
			case 2517:return N::boxInt(($p2)->_hash); 
			case 2518:return N::boxInt(($p2)->_threshold); 
			case 2519:return (($p2)->_table); 
			case 2520:return (java_util_Hashtable::$_EMPTY_TABLE); 
			case 2521:return N::boxInt(($p2)->_size); 
			case 2522:return N::boxInt(($p2)->_modCount); 
			case 2523:return (($p2)->_this_0); 
			case 2524:return N::boxInt(($p2)->_expectedModCount); 
			case 2525:return N::boxInt(($p2)->_nextIndex); 
			case 2526:return (($p2)->_this_0); 
			case 2527:return (($p2)->_nextEntry); 
			case 2528:return (($p2)->_this_0_); 
			case 2529:return (($p2)->_lastEntryReturned); 
			case 2530:return (($p2)->_entrySet); 
			case 2531:return (($p2)->_keySet); 
			case 2532:return (($p2)->_values); 
			case 2533:return (($p2)->_name); 
			case 2534:return ($p2)->_type; 
			case 2535:return ($p2)->_referent; 
			case 2536:return (java_util_Hashtable::$_serialPersistentFields); 
			case 2537:return (($p2)->_queue); 
			case 2538:return (java_lang_System::$__props); 
			case 2539:return (($p2)->_defaults); 
			default:
				break;
		}
		return null;
	}
	public static function dynamicSet0_IILjava_lang_Object_Ljava_lang_Object__V(int $p0, int $p1, ?java_lang_Object $p2, ?java_lang_Object $p3) {
		switch ($p1) {
			case 2137:java_lang_String::$_CASE_INSENSITIVE_ORDER = ($p3); break;
			case 2138:($p2)->_buffer = ($p3); break;
			case 2139:($p2)->_length = N::unboxInt(($p3)); break;
			case 2140:($p2)->_hash = N::unboxInt(($p3)); break;
			case 2141:($p2)->__out = ($p3); break;
			case 2142:($p2)->_thrown = N::unboxBool(($p3)); break;
			case 2143:java_lang_Throwable::$_EMPTY_ARRAY = ($p3); break;
			case 2144:($p2)->_message = ($p3); break;
			case 2145:($p2)->_value = ($p3); break;
			case 2146:($p2)->_encoding = ($p3); break;
			case 2147:($p2)->_writableStackTrace = N::unboxBool(($p3)); break;
			case 2148:($p2)->_enableSuppression = N::unboxBool(($p3)); break;
			case 2149:($p2)->_cause = ($p3); break;
			case 2150:java_lang_System::$__in = ($p3); break;
			case 2151:java_lang_System::$__out = ($p3); break;
			case 2152:java_lang_System::$_err = ($p3); break;
			case 2153:($p2)->_sb = ($p3); break;
			case 2154:($p2)->_error = N::unboxBool(($p3)); break;
			case 2155:($p2)->_error = N::unboxBool(($p3)); break;
			case 2156:($p2)->_stream = ($p3); break;
			case 2157:($p2)->_name = ($p3); break;
			case 2158:($p2)->_primitive = N::unboxBool(($p3)); break;
			case 2159:($p2)->_modifiers = N::unboxInt(($p3)); break;
			case 2160:($p2)->_value = N::unboxInt(($p3)); break;
			case 2161:($p2)->_value = N::unboxChar(($p3)); break;
			case 2162:java_lang_Character::$_TYPE = ($p3); break;
			case 2163:($p2)->_clazz = ($p3); break;
			case 2164:($p2)->_name = ($p3); break;
			case 2165:($p2)->_modifiers = N::unboxInt(($p3)); break;
			case 2166:($p2)->_signature = ($p3); break;
			case 2167:java_lang_Void::$_TYPE = ($p3); break;
			case 2168:($p2)->_value = N::unboxFloat(($p3)); break;
			case 2169:java_lang_Float::$_TYPE = ($p3); break;
			case 2170:($p2)->_value = N::unboxDouble(($p3)); break;
			case 2171:java_lang_Double::$_TYPE = ($p3); break;
			case 2172:($p2)->_value = N::unboxLong(($p3)); break;
			case 2173:java_lang_Long::$_TYPE = ($p3); break;
			case 2174:($p2)->_value = N::unboxShort(($p3)); break;
			case 2175:java_lang_Short::$_TYPE = ($p3); break;
			case 2176:java_lang_Boolean::$_TYPE = ($p3); break;
			case 2177:java_lang_Boolean::$_TRUE = ($p3); break;
			case 2178:java_lang_Boolean::$_FALSE = ($p3); break;
			case 2179:($p2)->_value = N::unboxBool(($p3)); break;
			case 2180:java_lang_Byte::$_cache = ($p3); break;
			case 2181:($p2)->_value = N::unboxByte(($p3)); break;
			case 2182:java_lang_Byte::$_TYPE = ($p3); break;
			case 2183:java_lang_Integer::$_TYPE = ($p3); break;
			case 2184:java_lang_Class::$__classCache = ($p3); break;
			case 2191:($p2)->_modifiers = N::unboxInt(($p3)); break;
			case 2192:($p2)->_name = ($p3); break;
			case 2196:($p2)->_signature = ($p3); break;
			case 2201:($p2)->_length = N::unboxInt(($p3)); break;
			case 2202:($p2)->_buffer = ($p3); break;
			case 2203:($p2)->_expectedModCount = N::unboxInt(($p3)); break;
			case 2204:($p2)->_modCount = N::unboxInt(($p3)); break;
			case 2205:($p2)->_this_0 = ($p3); break;
			case 2206:($p2)->_pos = N::unboxInt(($p3)); break;
			case 2207:($p2)->_lastPosition = N::unboxInt(($p3)); break;
			case 2208:($p2)->_this_0_ = ($p3); break;
			case 2216:j_ProgramReflection::$__classInfos = ($p3); break;
			case 2217:j_ProgramReflection::$__classNames = ($p3); break;
			case 2218:j_ProgramReflection::$__classInfosByName = ($p3); break;
			case 2222:($p2)->_clazz = ($p3); break;
			case 2223:($p2)->_id = N::unboxInt(($p3)); break;
			case 2224:($p2)->_exceptionTypes = ($p3); break;
			case 2225:($p2)->_genericSignature = ($p3); break;
			case 2226:($p2)->_slot = N::unboxInt(($p3)); break;
			case 2227:($p2)->_info = ($p3); break;
			case 2228:($p2)->__accessibleMethods = ($p3); break;
			case 2229:($p2)->_enumConstants = ($p3); break;
			case 2230:($p2)->__allFields = ($p3); break;
			case 2231:($p2)->__accessibleFields = ($p3); break;
			case 2232:($p2)->__allMethods = ($p3); break;
			case 2233:($p2)->_ex = ($p3); break;
			case 2234:($p2)->_id = N::unboxInt(($p3)); break;
			case 2235:($p2)->_related = ($p3); break;
			case 2236:($p2)->_info = ($p3); break;
			case 2237:($p2)->_fileName = ($p3); break;
			case 2238:($p2)->_lineNumber = N::unboxInt(($p3)); break;
			case 2239:($p2)->_methodName = ($p3); break;
			case 2240:($p2)->_declaringClass = ($p3); break;
			case 2241:($p2)->_stackTrace = ($p3); break;
			case 2242:($p2)->_slot = N::unboxInt(($p3)); break;
			case 2243:($p2)->_genericSignature = ($p3); break;
			case 2244:java_lang_SystemInt::$___lastId = N::unboxInt(($p3)); break;
			case 2246:($p2)->_a = ($p3); break;
			case 2247:($p2)->_supressed = ($p3); break;
			case 2248:java_lang_Integer::$_values = ($p3); break;
			case 2249:java_lang_Integer::$_NTZ_TABLE = ($p3); break;
			case 2250:Benchmark::$_totalTime = N::unboxDouble(($p3)); break;
			case 2251:($p2)->_a = N::unboxInt(($p3)); break;
			case 2252:($p2)->_c = ($p3); break;
			case 2253:($p2)->_d = ($p3); break;
			case 2254:($p2)->_b = N::unboxInt(($p3)); break;
			case 2255:($p2)->_val_objects = ($p3); break;
			case 2256:com_jtransc_JTranscSystem::$_start = N::unboxDouble(($p3)); break;
			case 2257:($p2)->_val_hexData = ($p3); break;
			case 2258:($p2)->_val_hexData = ($p3); break;
			case 2259:($p2)->_val_bytes = ($p3); break;
			case 2260:($p2)->_val_bytes = ($p3); break;
			default:
				break;
		}
	}
	public static function dynamicSet1_IILjava_lang_Object_Ljava_lang_Object__V(int $p0, int $p1, ?java_lang_Object $p2, ?java_lang_Object $p3) {
		switch ($p1) {
			case 2261:($p2)->_val_srcI = ($p3); break;
			case 2262:($p2)->_val_dstI = ($p3); break;
			case 2263:($p2)->_val_iarray = ($p3); break;
			case 2264:($p2)->_val_carray = ($p3); break;
			case 2265:($p2)->_val_sarray = ($p3); break;
			case 2266:($p2)->_val_barray = ($p3); break;
			case 2267:($p2)->_val_darray = ($p3); break;
			case 2268:($p2)->_val_farray = ($p3); break;
			case 2269:($p2)->_parent = ($p3); break;
			case 2270:com_jtransc_time_JTranscClock::$_impl = ($p3); break;
			case 2271:java_lang_Runtime::$_current = ($p3); break;
			case 2272:($p2)->_max = N::unboxInt(($p3)); break;
			case 2273:($p2)->_min = N::unboxInt(($p3)); break;
			case 2274:($p2)->_avg = N::unboxFloat(($p3)); break;
			case 2275:($p2)->_names = ($p3); break;
			case 2276:com_jtransc_charset_JTranscCharset::$_charsets = ($p3); break;
			case 2277:com_jtransc_charset_JTranscCharset::$_loadedCharsets = N::unboxBool(($p3)); break;
			case 2278:($p2)->_count = N::unboxInt(($p3)); break;
			case 2279:($p2)->_buf = ($p3); break;
			case 2280:($p2)->_buffer = ($p3); break;
			case 2281:($p2)->_position = N::unboxInt(($p3)); break;
			case 2282:($p2)->_size = N::unboxInt(($p3)); break;
			case 2283:($p2)->_charsetName = ($p3); break;
			case 2284:($p2)->_service = ($p3); break;
			case 2285:($p2)->_list = ($p3); break;
			case 2286:($p2)->_key = $p3; break;
			case 2287:($p2)->_next = ($p3); break;
			case 2288:($p2)->_value = $p3; break;
			case 2289:($p2)->_hash = N::unboxInt(($p3)); break;
			case 2290:($p2)->_table = ($p3); break;
			case 2291:($p2)->_threshold = N::unboxInt(($p3)); break;
			case 2292:java_util_HashMap::$_EMPTY_TABLE = ($p3); break;
			case 2293:($p2)->_this_0 = ($p3); break;
			case 2294:($p2)->_entryForNullKey = ($p3); break;
			case 2295:($p2)->_size = N::unboxInt(($p3)); break;
			case 2296:($p2)->_nextEntry = ($p3); break;
			case 2297:($p2)->_modCount = N::unboxInt(($p3)); break;
			case 2298:($p2)->_nextIndex = N::unboxInt(($p3)); break;
			case 2299:($p2)->_this_0 = ($p3); break;
			case 2300:($p2)->_expectedModCount = N::unboxInt(($p3)); break;
			case 2301:($p2)->_this_0_ = ($p3); break;
			case 2302:($p2)->_lastEntryReturned = ($p3); break;
			case 2303:($p2)->_entrySet = ($p3); break;
			case 2304:($p2)->_valuesCollection = ($p3); break;
			case 2305:($p2)->_keySet = ($p3); break;
			case 2306:($p2)->_values = ($p3); break;
			case 2307:($p2)->_keySet_ = ($p3); break;
			case 2308:($p2)->_invalidChar = N::unboxByte(($p3)); break;
			case 2309:($p2)->_decode = ($p3); break;
			case 2310:($p2)->_encode = ($p3); break;
			case 2311:java_util_Collections::$_EMPTY_SET = ($p3); break;
			case 2312:java_util_Collections::$_EMPTY_ITERATOR = ($p3); break;
			case 2313:java_util_Collections::$_EMPTY_ENUMERATION = ($p3); break;
			case 2314:java_util_Collections::$_EMPTY_MAP = ($p3); break;
			case 2315:java_util_Collections::$_EMPTY_LIST = ($p3); break;
			case 2316:($p2)->_littleEndian = N::unboxBool(($p3)); break;
			case 2317:($p2)->_name = ($p3); break;
			case 2318:($p2)->_group = ($p3); break;
			case 2319:java_lang_Thread::$__currentThread = ($p3); break;
			case 2320:($p2)->_classLoader = ($p3); break;
			case 2321:($p2)->_nativeLibs = ($p3); break;
			case 2322:($p2)->_parent = ($p3); break;
			case 2323:java_lang__ClassInternalUtils::$_classLoader = ($p3); break;
			case 2324:($p2)->_autoFlush = N::unboxBool(($p3)); break;
			case 2325:($p2)->_ioError = N::unboxBool(($p3)); break;
			case 2326:com_jtransc_JTranscArrays::$_EMPTY_BYTE = ($p3); break;
			case 2327:com_jtransc_JTranscArrays::$_EMPTY_CLASS = ($p3); break;
			case 2328:($p2)->_b = N::unboxInt(($p3)); break;
			case 2329:($p2)->_d = ($p3); break;
			case 2330:($p2)->_c = ($p3); break;
			case 2331:($p2)->_a = N::unboxInt(($p3)); break;
			case 2333:($p2)->__elementSizeShift = N::unboxInt(($p3)); break;
			case 2334:($p2)->_mark = N::unboxInt(($p3)); break;
			case 2335:($p2)->_block = ($p3); break;
			case 2336:($p2)->_position = N::unboxInt(($p3)); break;
			case 2337:($p2)->_limit = N::unboxInt(($p3)); break;
			case 2338:($p2)->_capacity = N::unboxInt(($p3)); break;
			case 2341:($p2)->_arrayOffset = N::unboxInt(($p3)); break;
			case 2342:($p2)->_backingArray = ($p3); break;
			case 2343:($p2)->_isReadOnly = N::unboxBool(($p3)); break;
			case 2348:($p2)->_byteBuffer = ($p3); break;
			case 2349:($p2)->_bytes = ($p3); break;
			case 2350:($p2)->_name = ($p3); break;
			case 2351:($p2)->_needsSwap = N::unboxBool(($p3)); break;
			case 2352:java_nio_ByteOrder::$_LITTLE_ENDIAN = ($p3); break;
			case 2353:java_nio_ByteOrder::$_NATIVE_ORDER = ($p3); break;
			case 2354:java_nio_ByteOrder::$_BIG_ENDIAN = ($p3); break;
			case 2355:java_nio_ByteOrder::$_isLittleEndian = N::unboxBool(($p3)); break;
			case 2356:libcore_io_Memory::$_SWAPPED = ($p3); break;
			case 2357:libcore_io_Memory::$_NATIVE = ($p3); break;
			case 2358:($p2)->_isLittleEndian = N::unboxBool(($p3)); break;
			case 2359:($p2)->_isNativeOrder = N::unboxBool(($p3)); break;
			case 2360:($p2)->_order = ($p3); break;
			case 2361:($p2)->_bytes = ($p3); break;
			case 2362:($p2)->_byteBuffer = ($p3); break;
			case 2363:($p2)->_bytes = ($p3); break;
			case 2364:($p2)->_byteBuffer = ($p3); break;
			case 2365:($p2)->_bytes = ($p3); break;
			case 2366:($p2)->_byteBuffer = ($p3); break;
			case 2367:($p2)->_bytes = ($p3); break;
			default:
				break;
		}
	}
	public static function dynamicSet2_IILjava_lang_Object_Ljava_lang_Object__V(int $p0, int $p1, ?java_lang_Object $p2, ?java_lang_Object $p3) {
		switch ($p1) {
			case 2368:($p2)->_byteBuffer = ($p3); break;
			case 2369:($p2)->_byteBuffer = ($p3); break;
			case 2370:($p2)->_bytes = ($p3); break;
			case 2372:($p2)->_isDirect = N::unboxBool(($p3)); break;
			case 2375:($p2)->_impl = ($p3); break;
			case 2376:($p2)->_tbytes = N::unboxLong(($p3)); break;
			case 2377:java_util_zip_CRC32::$_temp = ($p3); break;
			case 2378:($p2)->_seed = N::unboxLong(($p3)); break;
			case 2379:($p2)->_haveNextNextGaussian = N::unboxBool(($p3)); break;
			case 2411:($p2)->_nice_length = N::unboxInt(($p3)); break;
			case 2412:($p2)->_max_chain = N::unboxInt(($p3)); break;
			case 2413:($p2)->_max_lazy = N::unboxInt(($p3)); break;
			case 2414:($p2)->_good_length = N::unboxInt(($p3)); break;
			case 2415:($p2)->_func = N::unboxInt(($p3)); break;
			case 2491:($p2)->_impl = ($p3); break;
			case 2492:($p2)->_inLength = N::unboxInt(($p3)); break;
			case 2493:($p2)->_inRead = N::unboxInt(($p3)); break;
			case 2494:($p2)->_noHeader = N::unboxBool(($p3)); break;
			case 2495:($p2)->_compressLevel = N::unboxInt(($p3)); break;
			case 2496:($p2)->_streamHandle = N::unboxLong(($p3)); break;
			case 2497:($p2)->_flushParm = N::unboxInt(($p3)); break;
			case 2498:($p2)->_strategy = N::unboxInt(($p3)); break;
			case 2514:($p2)->_value = $p3; break;
			case 2515:($p2)->_next = ($p3); break;
			case 2516:($p2)->_key = $p3; break;
			case 2517:($p2)->_hash = N::unboxInt(($p3)); break;
			case 2518:($p2)->_threshold = N::unboxInt(($p3)); break;
			case 2519:($p2)->_table = ($p3); break;
			case 2520:java_util_Hashtable::$_EMPTY_TABLE = ($p3); break;
			case 2521:($p2)->_size = N::unboxInt(($p3)); break;
			case 2522:($p2)->_modCount = N::unboxInt(($p3)); break;
			case 2523:($p2)->_this_0 = ($p3); break;
			case 2524:($p2)->_expectedModCount = N::unboxInt(($p3)); break;
			case 2525:($p2)->_nextIndex = N::unboxInt(($p3)); break;
			case 2526:($p2)->_this_0 = ($p3); break;
			case 2527:($p2)->_nextEntry = ($p3); break;
			case 2528:($p2)->_this_0_ = ($p3); break;
			case 2529:($p2)->_lastEntryReturned = ($p3); break;
			case 2530:($p2)->_entrySet = ($p3); break;
			case 2531:($p2)->_keySet = ($p3); break;
			case 2532:($p2)->_values = ($p3); break;
			case 2533:($p2)->_name = ($p3); break;
			case 2534:($p2)->_type = $p3; break;
			case 2535:($p2)->_referent = $p3; break;
			case 2536:java_util_Hashtable::$_serialPersistentFields = ($p3); break;
			case 2537:($p2)->_queue = ($p3); break;
			case 2538:java_lang_System::$__props = ($p3); break;
			case 2539:($p2)->_defaults = ($p3); break;
			default:
				break;
		}
	}
	public function __construct($CLASS_ID = 741) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_CloneNotSupportedException extends java_lang_Exception {

	public function java_lang_CloneNotSupportedException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_Exception_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 740) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_StackTraceElement extends java_lang_Object implements java_io_Serializable {

	public $_fileName = null;
	public $_lineNumber = 0;
	public $_methodName = null;
	public $_declaringClass = null;
	public function java_lang_StackTraceElement_init__Ljava_lang_String_Ljava_lang_String_Ljava_lang_String_I_V(?java_lang_String $p0, ?java_lang_String $p1, ?java_lang_String $p2, int $p3) {
		$fA0 = null;
		$fA1 = null;
		($this)->java_lang_Object_init___V();
		$fA0 = $this;
		if (((($p0) == null))) goto label_1;
		$fA1 = $p0;
		goto label_2;
		label_1:
		$fA1 = Bootstrap::$STRINGLIT_473;
		label_2:
		$fA0->_declaringClass = $fA1;
		$fA0 = $this;
		if (((($p1) == null))) goto label_3;
		$fA1 = $p1;
		goto label_4;
		label_3:
		$fA1 = Bootstrap::$STRINGLIT_473;
		label_4:
		$fA0->_methodName = $fA1;
		$fA0 = $this;
		if (((($p2) == null))) goto label_5;
		$fA1 = $p2;
		goto label_6;
		label_5:
		$fA1 = Bootstrap::$STRINGLIT_473;
		label_6:
		$fA0->_fileName = $fA1;
		$this->_lineNumber = $p3;
		return $this;
		return $this;
	}
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$fA1 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$fA0 = (($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->getClassName__Ljava_lang_String_())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_212)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->_methodName));
		$fA1 = ($this);
		if (!(($fA1)->isNativeMethod__Z())) goto label_1;
		$fA1 = (Bootstrap::$STRINGLIT_474);
		goto label_2;
		label_1:
		if ((($this->_fileName == null))) goto label_3;
		if ((($this->_lineNumber < 0))) goto label_3;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA1 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		$fA1 = (($fA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_213)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->_fileName)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_55)->append_I_Ljava_lang_StringBuilder_($this->_lineNumber)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_1)->toString__Ljava_lang_String_());
		goto label_2;
		label_3:
		if ((($this->_fileName == null))) goto label_4;
		$tA2 = ((new java_lang_StringBuilder()));
		$fA1 = $tA2;
		($tA2)->java_lang_StringBuilder_init___V();
		$fA1 = (($fA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_213)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->_fileName)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_1)->toString__Ljava_lang_String_());
		goto label_2;
		label_4:
		$fA1 = (Bootstrap::$STRINGLIT_475);
		label_2:
		$fA0 = (($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(($fA1))->toString__Ljava_lang_String_());
		return ($fA0);
	}
	public function getClassName__Ljava_lang_String_() {
		return $this->_declaringClass;
	}
	public function isNativeMethod__Z() {
		$fI0 = 0;
		if ((($this->_lineNumber != -2))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function hashCode__I() {
		$lI1 = 0;
		$lI1 = ((int)((((int)(N::imul(31, $this->_declaringClass->hashCode__I()))) + $this->_methodName->hashCode__I())));
		$lI1 = ((int)((((int)(N::imul(31, $lI1))) + java_util_Objects::hashCode_Ljava_lang_Object__I(($this->_fileName)))));
		$lI1 = ((int)((((int)(N::imul(31, $lI1))) + $this->_lineNumber)));
		return $lI1;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		$lA2 = null;
		$fA0 = null;
		$fA1 = null;
		if ((($p0 != ($this)))) goto label_1;
		return true;
		label_1:
		if ((($p0) instanceof java_lang_StackTraceElement)) goto label_2;
		return false;
		label_2:
		$lA2 = N::checkcast($p0, "java_lang_StackTraceElement");
		if (!($lA2->_declaringClass->equals_Ljava_lang_Object__Z(($this->_declaringClass)))) goto label_3;
		if ((($lA2->_lineNumber != $this->_lineNumber))) goto label_3;
		$fA0 = ($this->_methodName);
		$fA1 = ($lA2->_methodName);
		if (!(java_util_Objects::equals_Ljava_lang_Object_Ljava_lang_Object__Z($fA0, $fA1))) goto label_3;
		$fA0 = ($this->_fileName);
		$fA1 = ($lA2->_fileName);
		if (!(java_util_Objects::equals_Ljava_lang_Object_Ljava_lang_Object__Z($fA0, $fA1))) goto label_3;
		$fI0 = 1;
		goto label_4;
		label_3:
		$fI0 = 0;
		label_4:
		return (($fI0)!=0);
	}
	public function __construct($CLASS_ID = 739) {
		parent::__construct($CLASS_ID);
		$this->_fileName = null;
		$this->_lineNumber = 0;
		$this->_methodName = null;
		$this->_declaringClass = null;
	}
	static public function SI() {
	}
}
interface java_lang_reflect_Type {

}
class java_lang_reflect_Type_IFields {

	static public function SI() {
	}
}
interface java_lang_reflect_ParameterizedType extends java_lang_reflect_Type {

}
class java_lang_reflect_ParameterizedType_IFields {

	static public function SI() {
	}
}
class java_lang_reflect_ParameterizedTypeImpl extends java_lang_Object implements java_lang_reflect_ParameterizedType {

	public $_ownerType = null;
	public $_actualTypeArguments = null;
	public $_rawType = null;
	public function java_lang_reflect_ParameterizedTypeImpl_init___Ljava_lang_reflect_Type_Ljava_lang_reflect_Type_Ljava_lang_reflect_Type__V(?JA_L $p0, ?java_lang_reflect_Type $p1, ?java_lang_reflect_Type $p2) {
		($this)->java_lang_Object_init___V();
		$this->_actualTypeArguments = $p0;
		$this->_rawType = $p1;
		$this->_ownerType = $p2;
		return $this;
		return $this;
	}
	public function toString__Ljava_lang_String_() {
		$lA1 = null;
		$lI2 = 0;
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$lA1 = $fA0;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils::getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_($this->_rawType));
		($lA1)->append_C_Ljava_lang_StringBuilder_(60);
		$lI2 = 0;
		label_1:
		if ((($lI2 >= ($this->_actualTypeArguments)->length))) goto label_2;
		if ((($lI2 == 0))) goto label_4;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_6);
		label_4:
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils::getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(((($this->_actualTypeArguments)->get($lI2)))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		($lA1)->append_C_Ljava_lang_StringBuilder_(62);
		return ($lA1)->toString__Ljava_lang_String_();
	}
	public function __construct($CLASS_ID = 737) {
		parent::__construct($CLASS_ID);
		$this->_ownerType = null;
		$this->_actualTypeArguments = null;
		$this->_rawType = null;
	}
	static public function SI() {
	}
}
class j_ProgramReflection_AllClasses extends java_lang_Object {

	public function j_ProgramReflection_AllClasses_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function getAllClasses___Lj_ClassInfo_() {
		$_out = null;
		$_out = new JA_L(925, "[Lj.ClassInfo;");
		$_out->set(649, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(649, null, Bootstrap::$STRINGLIT_476, 33, 650, JA_I::T([]), JA_I::T([649,650])));
		$_out->set(650, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(650, null, Bootstrap::$STRINGLIT_477, 33, -1, JA_I::T([]), JA_I::T([650])));
		$_out->set(651, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(651, null, Bootstrap::$STRINGLIT_478, 49, 650, JA_I::T([652,653,654]), JA_I::T([651,650,652,653,654])));
		$_out->set(652, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(652, null, Bootstrap::$STRINGLIT_479, 1537, -1, JA_I::T([]), JA_I::T([652])));
		$_out->set(653, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(653, null, Bootstrap::$STRINGLIT_480, 1537, -1, JA_I::T([]), JA_I::T([653])));
		$_out->set(654, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(654, null, Bootstrap::$STRINGLIT_481, 1537, -1, JA_I::T([]), JA_I::T([654])));
		$_out->set(655, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(655, null, Bootstrap::$STRINGLIT_482, 32, 650, JA_I::T([656,652]), JA_I::T([655,650,656,652])));
		$_out->set(656, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(656, null, Bootstrap::$STRINGLIT_483, 1537, -1, JA_I::T([]), JA_I::T([656])));
		$_out->set(657, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(657, null, Bootstrap::$STRINGLIT_484, 4128, 650, JA_I::T([]), JA_I::T([657,650])));
		$_out->set(658, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(658, null, Bootstrap::$STRINGLIT_485, 33, 650, JA_I::T([652,659,654]), JA_I::T([658,650,652,659,654])));
		$_out->set(659, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(659, null, Bootstrap::$STRINGLIT_486, 1537, -1, JA_I::T([]), JA_I::T([659])));
		$_out->set(660, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(660, null, Bootstrap::$STRINGLIT_487, 33, 650, JA_I::T([]), JA_I::T([660,650])));
		$_out->set(661, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(661, null, Bootstrap::$STRINGLIT_488, 33, 650, JA_I::T([]), JA_I::T([661,650])));
		$_out->set(662, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(662, null, Bootstrap::$STRINGLIT_489, 33, 665, JA_I::T([659,663]), JA_I::T([662,665,666,650,659,663,664,667])));
		$_out->set(663, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(663, null, Bootstrap::$STRINGLIT_490, 1537, -1, JA_I::T([664]), JA_I::T([663,664])));
		$_out->set(664, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(664, null, Bootstrap::$STRINGLIT_491, 1537, -1, JA_I::T([]), JA_I::T([664])));
		$_out->set(665, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(665, null, Bootstrap::$STRINGLIT_492, 33, 666, JA_I::T([]), JA_I::T([665,666,650,663,667,664])));
		$_out->set(666, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(666, null, Bootstrap::$STRINGLIT_493, 1057, 650, JA_I::T([663,667]), JA_I::T([666,650,663,667,664])));
		$_out->set(667, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(667, null, Bootstrap::$STRINGLIT_494, 1537, -1, JA_I::T([]), JA_I::T([667])));
		$_out->set(668, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(668, null, Bootstrap::$STRINGLIT_495, 33, 669, JA_I::T([]), JA_I::T([668,669,670,671,650,652])));
		$_out->set(669, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(669, null, Bootstrap::$STRINGLIT_496, 33, 670, JA_I::T([]), JA_I::T([669,670,671,650,652])));
		$_out->set(670, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(670, null, Bootstrap::$STRINGLIT_497, 33, 671, JA_I::T([]), JA_I::T([670,671,650,652])));
		$_out->set(671, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(671, null, Bootstrap::$STRINGLIT_498, 33, 650, JA_I::T([652]), JA_I::T([671,650,652])));
		$_out->set(672, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(672, null, Bootstrap::$STRINGLIT_499, 33, 650, JA_I::T([]), JA_I::T([672,650])));
		$_out->set(673, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(673, null, Bootstrap::$STRINGLIT_500, 49, 650, JA_I::T([]), JA_I::T([673,650])));
		$_out->set(674, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(674, null, Bootstrap::$STRINGLIT_501, 48, 675, JA_I::T([]), JA_I::T([674,675,650,663,664])));
		$_out->set(675, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(675, null, Bootstrap::$STRINGLIT_502, 1057, 650, JA_I::T([663]), JA_I::T([675,650,663,664])));
		$_out->set(676, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(676, null, Bootstrap::$STRINGLIT_503, 33, 662, JA_I::T([]), JA_I::T([676,662,665,666,650,659,663,664,667])));
		$_out->set(677, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(677, null, Bootstrap::$STRINGLIT_504, 32, 678, JA_I::T([]), JA_I::T([677,678,666,650,663,667,664])));
		$_out->set(678, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(678, null, Bootstrap::$STRINGLIT_505, 1056, 666, JA_I::T([]), JA_I::T([678,666,650,663,667,664])));
		$_out->set(679, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(679, null, Bootstrap::$STRINGLIT_506, 32, 678, JA_I::T([]), JA_I::T([679,678,666,650,663,667,664])));
		$_out->set(680, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(680, null, Bootstrap::$STRINGLIT_507, 33, 681, JA_I::T([]), JA_I::T([680,681,669,670,671,650,652])));
		$_out->set(681, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(681, null, Bootstrap::$STRINGLIT_508, 33, 669, JA_I::T([]), JA_I::T([681,669,670,671,650,652])));
		$_out->set(682, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(682, null, Bootstrap::$STRINGLIT_509, 33, 669, JA_I::T([]), JA_I::T([682,669,670,671,650,652])));
		$_out->set(683, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(683, null, Bootstrap::$STRINGLIT_510, 49, 650, JA_I::T([652,684,685,687]), JA_I::T([683,650,652,684,685,687,686])));
		$_out->set(684, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(684, null, Bootstrap::$STRINGLIT_511, 1537, -1, JA_I::T([]), JA_I::T([684])));
		$_out->set(685, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(685, null, Bootstrap::$STRINGLIT_512, 1537, -1, JA_I::T([686]), JA_I::T([685,686])));
		$_out->set(686, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(686, null, Bootstrap::$STRINGLIT_513, 1537, -1, JA_I::T([]), JA_I::T([686])));
		$_out->set(687, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(687, null, Bootstrap::$STRINGLIT_514, 1537, -1, JA_I::T([]), JA_I::T([687])));
		$_out->set(688, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(688, null, Bootstrap::$STRINGLIT_515, 33, 650, JA_I::T([]), JA_I::T([688,650])));
		$_out->set(689, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(689, null, Bootstrap::$STRINGLIT_516, 49, 690, JA_I::T([653]), JA_I::T([689,690,650,653])));
		$_out->set(690, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(690, null, Bootstrap::$STRINGLIT_517, 1057, 650, JA_I::T([]), JA_I::T([690,650])));
		$_out->set(691, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(691, null, Bootstrap::$STRINGLIT_518, 32, 650, JA_I::T([]), JA_I::T([691,650])));
		$_out->set(692, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(692, null, Bootstrap::$STRINGLIT_519, 49, 650, JA_I::T([652,653]), JA_I::T([692,650,652,653])));
		$_out->set(693, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(693, null, Bootstrap::$STRINGLIT_520, 49, 695, JA_I::T([694]), JA_I::T([693,695,650,694,686])));
		$_out->set(694, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(694, null, Bootstrap::$STRINGLIT_521, 1537, -1, JA_I::T([]), JA_I::T([694])));
		$_out->set(695, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(695, null, Bootstrap::$STRINGLIT_522, 1057, 650, JA_I::T([686]), JA_I::T([695,650,686])));
		$_out->set(697, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(697, null, Bootstrap::$STRINGLIT_523, 33, 698, JA_I::T([]), JA_I::T([697,698,670,671,650,652])));
		$_out->set(698, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(698, null, Bootstrap::$STRINGLIT_524, 33, 670, JA_I::T([]), JA_I::T([698,670,671,650,652])));
		$_out->set(699, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(699, null, Bootstrap::$STRINGLIT_525, 49, 650, JA_I::T([]), JA_I::T([699,650])));
		$_out->set(700, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(700, null, Bootstrap::$STRINGLIT_526, 49, 690, JA_I::T([653]), JA_I::T([700,690,650,653])));
		$_out->set(701, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(701, null, Bootstrap::$STRINGLIT_527, 33, 650, JA_I::T([]), JA_I::T([701,650])));
		$_out->set(702, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(702, null, Bootstrap::$STRINGLIT_528, 49, 690, JA_I::T([653]), JA_I::T([702,690,650,653])));
		$_out->set(704, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(704, null, Bootstrap::$STRINGLIT_529, 49, 690, JA_I::T([653]), JA_I::T([704,690,650,653])));
		$_out->set(705, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(705, null, Bootstrap::$STRINGLIT_530, 33, 650, JA_I::T([]), JA_I::T([705,650])));
		$_out->set(706, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(706, null, Bootstrap::$STRINGLIT_531, 49, 690, JA_I::T([653]), JA_I::T([706,690,650,653])));
		$_out->set(707, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(707, null, Bootstrap::$STRINGLIT_532, 33, 650, JA_I::T([]), JA_I::T([707,650])));
		$_out->set(708, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(708, null, Bootstrap::$STRINGLIT_533, 49, 650, JA_I::T([652,653]), JA_I::T([708,650,652,653])));
		$_out->set(709, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(709, null, Bootstrap::$STRINGLIT_534, 49, 690, JA_I::T([653]), JA_I::T([709,690,650,653])));
		$_out->set(710, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(710, null, Bootstrap::$STRINGLIT_535, 49, 711, JA_I::T([694,685]), JA_I::T([710,711,695,650,694,685,686])));
		$_out->set(711, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(711, null, Bootstrap::$STRINGLIT_536, 1057, 695, JA_I::T([]), JA_I::T([711,695,650,686])));
		$_out->set(712, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(712, null, Bootstrap::$STRINGLIT_537, 33, 650, JA_I::T([]), JA_I::T([712,650])));
		$_out->set(715, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(715, null, Bootstrap::$STRINGLIT_538, 33, 671, JA_I::T([]), JA_I::T([715,671,650,652])));
		$_out->set(717, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(717, null, Bootstrap::$STRINGLIT_539, 33, 723, JA_I::T([718,721,722,652]), JA_I::T([717,723,724,650,718,721,722,652,719,720])));
		$_out->set(718, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(718, null, Bootstrap::$STRINGLIT_540, 1537, -1, JA_I::T([719]), JA_I::T([718,719,720])));
		$_out->set(719, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(719, null, Bootstrap::$STRINGLIT_541, 1537, -1, JA_I::T([720]), JA_I::T([719,720])));
		$_out->set(720, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(720, null, Bootstrap::$STRINGLIT_542, 1537, -1, JA_I::T([]), JA_I::T([720])));
		$_out->set(721, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(721, null, Bootstrap::$STRINGLIT_543, 1537, -1, JA_I::T([]), JA_I::T([721])));
		$_out->set(722, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(722, null, Bootstrap::$STRINGLIT_544, 1537, -1, JA_I::T([]), JA_I::T([722])));
		$_out->set(723, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(723, null, Bootstrap::$STRINGLIT_545, 1057, 724, JA_I::T([718]), JA_I::T([723,724,650,718,719,720])));
		$_out->set(724, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(724, null, Bootstrap::$STRINGLIT_546, 1057, 650, JA_I::T([719]), JA_I::T([724,650,719,720])));
		$_out->set(725, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(725, null, Bootstrap::$STRINGLIT_547, 1537, -1, JA_I::T([]), JA_I::T([725])));
		$_out->set(726, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(726, null, Bootstrap::$STRINGLIT_548, 32, 650, JA_I::T([725]), JA_I::T([726,650,725])));
		$_out->set(727, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(727, null, Bootstrap::$STRINGLIT_549, 1537, -1, JA_I::T([725]), JA_I::T([727,725])));
		$_out->set(728, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(728, null, Bootstrap::$STRINGLIT_550, 48, 726, JA_I::T([727]), JA_I::T([728,726,650,727,725])));
		$_out->set(729, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(729, null, Bootstrap::$STRINGLIT_551, 33, 669, JA_I::T([]), JA_I::T([729,669,670,671,650,652])));
		$_out->set(730, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(730, null, Bootstrap::$STRINGLIT_552, 33, 669, JA_I::T([]), JA_I::T([730,669,670,671,650,652])));
		$_out->set(731, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(731, null, Bootstrap::$STRINGLIT_553, 49, 650, JA_I::T([]), JA_I::T([731,650])));
		$_out->set(732, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(732, null, Bootstrap::$STRINGLIT_554, 33, 650, JA_I::T([]), JA_I::T([732,650])));
		$_out->set(733, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(733, null, Bootstrap::$STRINGLIT_555, 33, 650, JA_I::T([]), JA_I::T([733,650])));
		$_out->set(734, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(734, null, Bootstrap::$STRINGLIT_556, 33, 650, JA_I::T([]), JA_I::T([734,650])));
		$_out->set(735, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(735, null, Bootstrap::$STRINGLIT_557, 33, 650, JA_I::T([]), JA_I::T([735,650])));
		$_out->set(736, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(736, null, Bootstrap::$STRINGLIT_558, 33, 669, JA_I::T([]), JA_I::T([736,669,670,671,650,652])));
		$_out->set(738, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(738, null, Bootstrap::$STRINGLIT_559, 1537, -1, JA_I::T([684]), JA_I::T([738,684])));
		$_out->set(739, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(739, null, Bootstrap::$STRINGLIT_560, 49, 650, JA_I::T([652]), JA_I::T([739,650,652])));
		$_out->set(740, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(740, null, Bootstrap::$STRINGLIT_561, 33, 670, JA_I::T([]), JA_I::T([740,670,671,650,652])));
		$_out->set(741, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(741, null, Bootstrap::$STRINGLIT_562, 33, 650, JA_I::T([]), JA_I::T([741,650])));
		$_out->set(742, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(742, null, Bootstrap::$STRINGLIT_563, 33, 650, JA_I::T([]), JA_I::T([742,650])));
		$_out->set(743, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(743, null, Bootstrap::$STRINGLIT_564, 33, 670, JA_I::T([]), JA_I::T([743,670,671,650,652])));
		$_out->set(744, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(744, null, Bootstrap::$STRINGLIT_565, 33, 698, JA_I::T([]), JA_I::T([744,698,670,671,650,652])));
		$_out->set(745, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(745, null, Bootstrap::$STRINGLIT_566, 49, 711, JA_I::T([694,685]), JA_I::T([745,711,695,650,694,685,686])));
		$_out->set(746, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(746, null, Bootstrap::$STRINGLIT_567, 33, 698, JA_I::T([]), JA_I::T([746,698,670,671,650,652])));
		$_out->set(747, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(747, null, Bootstrap::$STRINGLIT_568, 33, 650, JA_I::T([]), JA_I::T([747,650])));
		$_out->set(748, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(748, null, Bootstrap::$STRINGLIT_569, 33, 650, JA_I::T([]), JA_I::T([748,650])));
		$_out->set(749, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(749, null, Bootstrap::$STRINGLIT_570, 49, 650, JA_I::T([]), JA_I::T([749,650])));
		$_out->set(750, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(750, null, Bootstrap::$STRINGLIT_571, 33, 650, JA_I::T([]), JA_I::T([750,650])));
		$_out->set(751, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(751, null, Bootstrap::$STRINGLIT_572, 32, 723, JA_I::T([718,652,721]), JA_I::T([751,723,724,650,718,652,721,719,720])));
		$_out->set(752, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(752, null, Bootstrap::$STRINGLIT_573, 9729, -1, JA_I::T([753]), JA_I::T([752,753])));
		$_out->set(753, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(753, null, Bootstrap::$STRINGLIT_574, 1537, -1, JA_I::T([]), JA_I::T([753])));
		$_out->set(754, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(754, null, Bootstrap::$STRINGLIT_575, 33, 669, JA_I::T([]), JA_I::T([754,669,670,671,650,652])));
		$_out->set(755, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(755, null, Bootstrap::$STRINGLIT_576, 48, 650, JA_I::T([756]), JA_I::T([755,650,756])));
		$_out->set(756, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(756, null, Bootstrap::$STRINGLIT_577, 1536, -1, JA_I::T([]), JA_I::T([756])));
		$_out->set(757, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(757, null, Bootstrap::$STRINGLIT_578, 32, 650, JA_I::T([]), JA_I::T([757,650])));
		$_out->set(758, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(758, null, Bootstrap::$STRINGLIT_579, 48, 650, JA_I::T([756]), JA_I::T([758,650,756])));
		$_out->set(759, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(759, null, Bootstrap::$STRINGLIT_580, 48, 650, JA_I::T([756]), JA_I::T([759,650,756])));
		$_out->set(760, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(760, null, Bootstrap::$STRINGLIT_581, 48, 650, JA_I::T([756]), JA_I::T([760,650,756])));
		$_out->set(761, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(761, null, Bootstrap::$STRINGLIT_582, 33, 650, JA_I::T([]), JA_I::T([761,650])));
		$_out->set(762, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(762, null, Bootstrap::$STRINGLIT_583, 48, 650, JA_I::T([756]), JA_I::T([762,650,756])));
		$_out->set(763, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(763, null, Bootstrap::$STRINGLIT_584, 48, 650, JA_I::T([756]), JA_I::T([763,650,756])));
		$_out->set(764, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(764, null, Bootstrap::$STRINGLIT_585, 33, 650, JA_I::T([652]), JA_I::T([764,650,652])));
		$_out->set(765, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(765, null, Bootstrap::$STRINGLIT_586, 48, 650, JA_I::T([756]), JA_I::T([765,650,756])));
		$_out->set(766, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(766, null, Bootstrap::$STRINGLIT_587, 48, 650, JA_I::T([756]), JA_I::T([766,650,756])));
		$_out->set(767, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(767, null, Bootstrap::$STRINGLIT_588, 48, 650, JA_I::T([756]), JA_I::T([767,650,756])));
		$_out->set(768, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(768, null, Bootstrap::$STRINGLIT_589, 48, 650, JA_I::T([756]), JA_I::T([768,650,756])));
		$_out->set(769, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(769, null, Bootstrap::$STRINGLIT_590, 33, 650, JA_I::T([]), JA_I::T([769,650])));
		$_out->set(770, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(770, null, Bootstrap::$STRINGLIT_591, 48, 650, JA_I::T([756]), JA_I::T([770,650,756])));
		$_out->set(771, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(771, null, Bootstrap::$STRINGLIT_592, 48, 650, JA_I::T([756]), JA_I::T([771,650,756])));
		$_out->set(772, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(772, null, Bootstrap::$STRINGLIT_593, 48, 650, JA_I::T([756]), JA_I::T([772,650,756])));
		$_out->set(773, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(773, null, Bootstrap::$STRINGLIT_594, 48, 650, JA_I::T([756]), JA_I::T([773,650,756])));
		$_out->set(774, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(774, null, Bootstrap::$STRINGLIT_595, 48, 650, JA_I::T([756]), JA_I::T([774,650,756])));
		$_out->set(775, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(775, null, Bootstrap::$STRINGLIT_596, 48, 650, JA_I::T([756]), JA_I::T([775,650,756])));
		$_out->set(776, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(776, null, Bootstrap::$STRINGLIT_597, 48, 650, JA_I::T([756]), JA_I::T([776,650,756])));
		$_out->set(777, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(777, null, Bootstrap::$STRINGLIT_598, 48, 650, JA_I::T([756]), JA_I::T([777,650,756])));
		$_out->set(778, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(778, null, Bootstrap::$STRINGLIT_599, 48, 650, JA_I::T([756]), JA_I::T([778,650,756])));
		$_out->set(779, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(779, null, Bootstrap::$STRINGLIT_600, 48, 650, JA_I::T([756]), JA_I::T([779,650,756])));
		$_out->set(780, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(780, null, Bootstrap::$STRINGLIT_601, 48, 650, JA_I::T([756]), JA_I::T([780,650,756])));
		$_out->set(781, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(781, null, Bootstrap::$STRINGLIT_602, 48, 650, JA_I::T([756]), JA_I::T([781,650,756])));
		$_out->set(782, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(782, null, Bootstrap::$STRINGLIT_603, 48, 650, JA_I::T([756]), JA_I::T([782,650,756])));
		$_out->set(783, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(783, null, Bootstrap::$STRINGLIT_604, 48, 650, JA_I::T([756]), JA_I::T([783,650,756])));
		$_out->set(784, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(784, null, Bootstrap::$STRINGLIT_605, 48, 650, JA_I::T([756]), JA_I::T([784,650,756])));
		$_out->set(785, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(785, null, Bootstrap::$STRINGLIT_606, 48, 650, JA_I::T([756]), JA_I::T([785,650,756])));
		$_out->set(786, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(786, null, Bootstrap::$STRINGLIT_607, 48, 650, JA_I::T([756]), JA_I::T([786,650,756])));
		$_out->set(787, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(787, null, Bootstrap::$STRINGLIT_608, 48, 650, JA_I::T([756]), JA_I::T([787,650,756])));
		$_out->set(788, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(788, null, Bootstrap::$STRINGLIT_609, 48, 650, JA_I::T([756]), JA_I::T([788,650,756])));
		$_out->set(789, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(789, null, Bootstrap::$STRINGLIT_610, 48, 650, JA_I::T([756]), JA_I::T([789,650,756])));
		$_out->set(790, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(790, null, Bootstrap::$STRINGLIT_611, 48, 650, JA_I::T([756]), JA_I::T([790,650,756])));
		$_out->set(791, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(791, null, Bootstrap::$STRINGLIT_612, 48, 650, JA_I::T([756]), JA_I::T([791,650,756])));
		$_out->set(792, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(792, null, Bootstrap::$STRINGLIT_613, 48, 650, JA_I::T([756]), JA_I::T([792,650,756])));
		$_out->set(793, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(793, null, Bootstrap::$STRINGLIT_614, 48, 650, JA_I::T([756]), JA_I::T([793,650,756])));
		$_out->set(794, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(794, null, Bootstrap::$STRINGLIT_615, 48, 650, JA_I::T([756]), JA_I::T([794,650,756])));
		$_out->set(795, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(795, null, Bootstrap::$STRINGLIT_616, 48, 650, JA_I::T([756]), JA_I::T([795,650,756])));
		$_out->set(796, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(796, null, Bootstrap::$STRINGLIT_617, 48, 650, JA_I::T([756]), JA_I::T([796,650,756])));
		$_out->set(797, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(797, null, Bootstrap::$STRINGLIT_618, 48, 650, JA_I::T([756]), JA_I::T([797,650,756])));
		$_out->set(798, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(798, null, Bootstrap::$STRINGLIT_619, 48, 650, JA_I::T([756]), JA_I::T([798,650,756])));
		$_out->set(799, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(799, null, Bootstrap::$STRINGLIT_620, 48, 650, JA_I::T([756]), JA_I::T([799,650,756])));
		$_out->set(800, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(800, null, Bootstrap::$STRINGLIT_621, 48, 650, JA_I::T([756]), JA_I::T([800,650,756])));
		$_out->set(801, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(801, null, Bootstrap::$STRINGLIT_622, 48, 650, JA_I::T([756]), JA_I::T([801,650,756])));
		$_out->set(802, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(802, null, Bootstrap::$STRINGLIT_623, 48, 650, JA_I::T([756]), JA_I::T([802,650,756])));
		$_out->set(803, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(803, null, Bootstrap::$STRINGLIT_624, 48, 650, JA_I::T([756]), JA_I::T([803,650,756])));
		$_out->set(804, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(804, null, Bootstrap::$STRINGLIT_625, 33, 650, JA_I::T([]), JA_I::T([804,650])));
		$_out->set(805, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(805, null, Bootstrap::$STRINGLIT_626, 33, 650, JA_I::T([]), JA_I::T([805,650])));
		$_out->set(806, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(806, null, Bootstrap::$STRINGLIT_627, 48, 807, JA_I::T([]), JA_I::T([806,807,650])));
		$_out->set(807, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(807, null, Bootstrap::$STRINGLIT_628, 33, 650, JA_I::T([]), JA_I::T([807,650])));
		$_out->set(808, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(808, null, Bootstrap::$STRINGLIT_629, 33, 670, JA_I::T([]), JA_I::T([808,670,671,650,652])));
		$_out->set(809, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(809, null, Bootstrap::$STRINGLIT_116, 1057, 650, JA_I::T([]), JA_I::T([809,650])));
		$_out->set(810, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(810, null, Bootstrap::$STRINGLIT_630, 33, 666, JA_I::T([]), JA_I::T([810,666,650,663,667,664])));
		$_out->set(811, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(811, null, Bootstrap::$STRINGLIT_631, 33, 650, JA_I::T([]), JA_I::T([811,650])));
		$_out->set(812, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(812, null, Bootstrap::$STRINGLIT_632, 33, 682, JA_I::T([]), JA_I::T([812,682,669,670,671,650,652])));
		$_out->set(813, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(813, null, Bootstrap::$STRINGLIT_633, 49, 650, JA_I::T([720]), JA_I::T([813,650,720])));
		$_out->set(814, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(814, null, Bootstrap::$STRINGLIT_634, 33, 815, JA_I::T([]), JA_I::T([814,815,809,650])));
		$_out->set(815, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(815, null, Bootstrap::$STRINGLIT_635, 33, 809, JA_I::T([]), JA_I::T([815,809,650])));
		$_out->set(816, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(816, null, Bootstrap::$STRINGLIT_636, 1537, -1, JA_I::T([]), JA_I::T([816])));
		$_out->set(817, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(817, null, Bootstrap::$STRINGLIT_637, 33, 818, JA_I::T([722,652]), JA_I::T([817,818,650,722,652,816])));
		$_out->set(818, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(818, null, Bootstrap::$STRINGLIT_638, 1057, 650, JA_I::T([816]), JA_I::T([818,650,816])));
		$_out->set(819, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(819, null, Bootstrap::$STRINGLIT_639, 1537, -1, JA_I::T([]), JA_I::T([819])));
		$_out->set(820, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(820, null, Bootstrap::$STRINGLIT_640, 32, 650, JA_I::T([819]), JA_I::T([820,650,819])));
		$_out->set(821, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(821, null, Bootstrap::$STRINGLIT_641, 1537, -1, JA_I::T([719]), JA_I::T([821,719,720])));
		$_out->set(822, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(822, null, Bootstrap::$STRINGLIT_642, 4128, 650, JA_I::T([]), JA_I::T([822,650])));
		$_out->set(823, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(823, null, Bootstrap::$STRINGLIT_643, 48, 824, JA_I::T([]), JA_I::T([823,824,724,650,821,719,720])));
		$_out->set(824, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(824, null, Bootstrap::$STRINGLIT_644, 1057, 724, JA_I::T([821]), JA_I::T([824,724,650,821,719,720])));
		$_out->set(825, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(825, null, Bootstrap::$STRINGLIT_645, 48, 826, JA_I::T([725]), JA_I::T([825,826,650,725])));
		$_out->set(826, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(826, null, Bootstrap::$STRINGLIT_646, 1056, 650, JA_I::T([]), JA_I::T([826,650])));
		$_out->set(827, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(827, null, Bootstrap::$STRINGLIT_647, 33, 715, JA_I::T([]), JA_I::T([827,715,671,650,652])));
		$_out->set(828, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(828, null, Bootstrap::$STRINGLIT_648, 33, 650, JA_I::T([]), JA_I::T([828,650])));
		$_out->set(829, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(829, null, Bootstrap::$STRINGLIT_649, 48, 650, JA_I::T([725]), JA_I::T([829,650,725])));
		$_out->set(830, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(830, null, Bootstrap::$STRINGLIT_650, 48, 723, JA_I::T([721,652]), JA_I::T([830,723,724,650,721,652,718,719,720])));
		$_out->set(831, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(831, null, Bootstrap::$STRINGLIT_651, 48, 650, JA_I::T([832]), JA_I::T([831,650,832])));
		$_out->set(832, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(832, null, Bootstrap::$STRINGLIT_652, 1537, -1, JA_I::T([]), JA_I::T([832])));
		$_out->set(833, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(833, null, Bootstrap::$STRINGLIT_653, 48, 818, JA_I::T([652]), JA_I::T([833,818,650,652,816])));
		$_out->set(834, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(834, null, Bootstrap::$STRINGLIT_654, 48, 824, JA_I::T([652]), JA_I::T([834,824,724,650,652,821,719,720])));
		$_out->set(835, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(835, null, Bootstrap::$STRINGLIT_655, 33, 809, JA_I::T([]), JA_I::T([835,809,650])));
		$_out->set(836, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(836, null, Bootstrap::$STRINGLIT_656, 33, 815, JA_I::T([]), JA_I::T([836,815,809,650])));
		$_out->set(837, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(837, null, Bootstrap::$STRINGLIT_657, 33, 838, JA_I::T([]), JA_I::T([837,838,809,650])));
		$_out->set(838, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(838, null, Bootstrap::$STRINGLIT_658, 1056, 809, JA_I::T([]), JA_I::T([838,809,650])));
		$_out->set(840, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(840, null, Bootstrap::$STRINGLIT_659, 33, 815, JA_I::T([]), JA_I::T([840,815,809,650])));
		$_out->set(841, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(841, null, Bootstrap::$STRINGLIT_660, 33, 838, JA_I::T([]), JA_I::T([841,838,809,650])));
		$_out->set(842, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(842, null, Bootstrap::$STRINGLIT_661, 33, 650, JA_I::T([843]), JA_I::T([842,650,843])));
		$_out->set(843, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(843, null, Bootstrap::$STRINGLIT_662, 1537, -1, JA_I::T([]), JA_I::T([843])));
		$_out->set(844, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(844, null, Bootstrap::$STRINGLIT_663, 33, 650, JA_I::T([845]), JA_I::T([844,650,845])));
		$_out->set(845, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(845, null, Bootstrap::$STRINGLIT_664, 1537, -1, JA_I::T([]), JA_I::T([845])));
		$_out->set(846, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(846, null, Bootstrap::$STRINGLIT_665, 1057, 650, JA_I::T([]), JA_I::T([846,650])));
		$_out->set(847, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(847, null, Bootstrap::$STRINGLIT_666, 32, 650, JA_I::T([]), JA_I::T([847,650])));
		$_out->set(848, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(848, null, Bootstrap::$STRINGLIT_667, 48, 846, JA_I::T([]), JA_I::T([848,846,650])));
		$_out->set(849, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(849, null, Bootstrap::$STRINGLIT_668, 33, 650, JA_I::T([]), JA_I::T([849,650])));
		$_out->set(850, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(850, null, Bootstrap::$STRINGLIT_669, 33, 650, JA_I::T([]), JA_I::T([850,650])));
		$_out->set(851, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(851, null, Bootstrap::$STRINGLIT_670, 32, 650, JA_I::T([]), JA_I::T([851,650])));
		$_out->set(853, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(853, null, Bootstrap::$STRINGLIT_671, 1057, 854, JA_I::T([653]), JA_I::T([853,854,650,653])));
		$_out->set(854, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(854, null, Bootstrap::$STRINGLIT_672, 1057, 650, JA_I::T([]), JA_I::T([854,650])));
		$_out->set(855, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(855, null, Bootstrap::$STRINGLIT_673, 33, 650, JA_I::T([]), JA_I::T([855,650])));
		$_out->set(856, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(856, null, Bootstrap::$STRINGLIT_674, 1057, 854, JA_I::T([653]), JA_I::T([856,854,650,653])));
		$_out->set(857, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(857, null, Bootstrap::$STRINGLIT_675, 33, 854, JA_I::T([653]), JA_I::T([857,854,650,653])));
		$_out->set(858, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(858, null, Bootstrap::$STRINGLIT_676, 33, 736, JA_I::T([]), JA_I::T([858,736,669,670,671,650,652])));
		$_out->set(859, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(859, null, Bootstrap::$STRINGLIT_677, 1057, 854, JA_I::T([653,654,659,860]), JA_I::T([859,854,650,653,654,659,860])));
		$_out->set(860, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(860, null, Bootstrap::$STRINGLIT_678, 1537, -1, JA_I::T([]), JA_I::T([860])));
		$_out->set(861, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(861, null, Bootstrap::$STRINGLIT_679, 1057, 854, JA_I::T([653]), JA_I::T([861,854,650,653])));
		$_out->set(862, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(862, null, Bootstrap::$STRINGLIT_680, 1057, 854, JA_I::T([653]), JA_I::T([862,854,650,653])));
		$_out->set(863, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(863, null, Bootstrap::$STRINGLIT_681, 1057, 854, JA_I::T([653]), JA_I::T([863,854,650,653])));
		$_out->set(864, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(864, null, Bootstrap::$STRINGLIT_682, 32, 856, JA_I::T([865]), JA_I::T([864,856,854,650,865,653])));
		$_out->set(865, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(865, null, Bootstrap::$STRINGLIT_683, 1537, -1, JA_I::T([]), JA_I::T([865])));
		$_out->set(866, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(866, null, Bootstrap::$STRINGLIT_684, 33, 650, JA_I::T([]), JA_I::T([866,650])));
		$_out->set(867, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(867, null, Bootstrap::$STRINGLIT_685, 49, 650, JA_I::T([]), JA_I::T([867,650])));
		$_out->set(868, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(868, null, Bootstrap::$STRINGLIT_686, 49, 864, JA_I::T([]), JA_I::T([868,864,856,854,650,865,653])));
		$_out->set(869, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(869, null, Bootstrap::$STRINGLIT_687, 49, 864, JA_I::T([]), JA_I::T([869,864,856,854,650,865,653])));
		$_out->set(870, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(870, null, Bootstrap::$STRINGLIT_688, 1056, 863, JA_I::T([865]), JA_I::T([870,863,854,650,865,653])));
		$_out->set(871, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(871, null, Bootstrap::$STRINGLIT_689, 49, 870, JA_I::T([]), JA_I::T([871,870,863,854,650,865,653])));
		$_out->set(872, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(872, null, Bootstrap::$STRINGLIT_690, 4128, 650, JA_I::T([]), JA_I::T([872,650])));
		$_out->set(873, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(873, null, Bootstrap::$STRINGLIT_691, 49, 870, JA_I::T([]), JA_I::T([873,870,863,854,650,865,653])));
		$_out->set(874, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(874, null, Bootstrap::$STRINGLIT_692, 1056, 861, JA_I::T([865]), JA_I::T([874,861,854,650,865,653])));
		$_out->set(875, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(875, null, Bootstrap::$STRINGLIT_693, 49, 874, JA_I::T([]), JA_I::T([875,874,861,854,650,865,653])));
		$_out->set(876, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(876, null, Bootstrap::$STRINGLIT_694, 4128, 650, JA_I::T([]), JA_I::T([876,650])));
		$_out->set(877, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(877, null, Bootstrap::$STRINGLIT_695, 49, 874, JA_I::T([]), JA_I::T([877,874,861,854,650,865,653])));
		$_out->set(878, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(878, null, Bootstrap::$STRINGLIT_696, 1056, 859, JA_I::T([865]), JA_I::T([878,859,854,650,865,653,654,659,860])));
		$_out->set(879, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(879, null, Bootstrap::$STRINGLIT_697, 49, 878, JA_I::T([]), JA_I::T([879,878,859,854,650,865,653,654,659,860])));
		$_out->set(880, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(880, null, Bootstrap::$STRINGLIT_698, 4128, 650, JA_I::T([]), JA_I::T([880,650])));
		$_out->set(881, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(881, null, Bootstrap::$STRINGLIT_699, 49, 878, JA_I::T([]), JA_I::T([881,878,859,854,650,865,653,654,659,860])));
		$_out->set(882, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(882, null, Bootstrap::$STRINGLIT_700, 32, 862, JA_I::T([865]), JA_I::T([882,862,854,650,865,653])));
		$_out->set(883, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(883, null, Bootstrap::$STRINGLIT_701, 49, 882, JA_I::T([]), JA_I::T([883,882,862,854,650,865,653])));
		$_out->set(884, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(884, null, Bootstrap::$STRINGLIT_702, 4128, 650, JA_I::T([]), JA_I::T([884,650])));
		$_out->set(885, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(885, null, Bootstrap::$STRINGLIT_703, 49, 882, JA_I::T([]), JA_I::T([885,882,862,854,650,865,653])));
		$_out->set(886, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(886, null, Bootstrap::$STRINGLIT_704, 1056, 853, JA_I::T([865]), JA_I::T([886,853,854,650,865,653])));
		$_out->set(887, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(887, null, Bootstrap::$STRINGLIT_705, 49, 886, JA_I::T([]), JA_I::T([887,886,853,854,650,865,653])));
		$_out->set(888, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(888, null, Bootstrap::$STRINGLIT_706, 49, 886, JA_I::T([]), JA_I::T([888,886,853,854,650,865,653])));
		$_out->set(889, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(889, null, Bootstrap::$STRINGLIT_707, 33, 650, JA_I::T([890]), JA_I::T([889,650,890])));
		$_out->set(890, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(890, null, Bootstrap::$STRINGLIT_708, 1537, -1, JA_I::T([]), JA_I::T([890])));
		$_out->set(898, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(898, null, Bootstrap::$STRINGLIT_709, 32, 650, JA_I::T([]), JA_I::T([898,650])));
		$_out->set(902, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(902, null, Bootstrap::$STRINGLIT_710, 33, 650, JA_I::T([]), JA_I::T([902,650])));
		$_out->set(903, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(903, null, Bootstrap::$STRINGLIT_711, 32, 650, JA_I::T([]), JA_I::T([903,650])));
		$_out->set(904, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(904, null, Bootstrap::$STRINGLIT_712, 32, 650, JA_I::T([]), JA_I::T([904,650])));
		$_out->set(911, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(911, null, Bootstrap::$STRINGLIT_713, 33, 912, JA_I::T([]), JA_I::T([911,912,913,650,816,722,652])));
		$_out->set(912, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(912, null, Bootstrap::$STRINGLIT_714, 33, 913, JA_I::T([816,722,652]), JA_I::T([912,913,650,816,722,652])));
		$_out->set(913, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(913, null, Bootstrap::$STRINGLIT_715, 1057, 650, JA_I::T([]), JA_I::T([913,650])));
		$_out->set(914, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(914, null, Bootstrap::$STRINGLIT_716, 32, 650, JA_I::T([819]), JA_I::T([914,650,819])));
		$_out->set(915, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(915, null, Bootstrap::$STRINGLIT_717, 4128, 650, JA_I::T([]), JA_I::T([915,650])));
		$_out->set(916, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(916, null, Bootstrap::$STRINGLIT_718, 48, 824, JA_I::T([]), JA_I::T([916,824,724,650,821,719,720])));
		$_out->set(917, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(917, null, Bootstrap::$STRINGLIT_719, 48, 918, JA_I::T([725]), JA_I::T([917,918,650,725])));
		$_out->set(918, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(918, null, Bootstrap::$STRINGLIT_720, 1056, 650, JA_I::T([]), JA_I::T([918,650])));
		$_out->set(919, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(919, null, Bootstrap::$STRINGLIT_721, 33, 650, JA_I::T([653]), JA_I::T([919,650,653])));
		$_out->set(920, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(920, null, Bootstrap::$STRINGLIT_722, 33, 921, JA_I::T([]), JA_I::T([920,921,650])));
		$_out->set(921, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(921, null, Bootstrap::$STRINGLIT_723, 1057, 650, JA_I::T([]), JA_I::T([921,650])));
		$_out->set(922, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(922, null, Bootstrap::$STRINGLIT_724, 33, 650, JA_I::T([]), JA_I::T([922,650])));
		$_out->set(923, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(923, null, Bootstrap::$STRINGLIT_725, 1, 650, JA_I::T([752,753]), JA_I::T([923,650,752,753])));
		$_out->set(924, j_ClassInfo::create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(924, null, Bootstrap::$STRINGLIT_726, 1, 650, JA_I::T([753,753]), JA_I::T([924,650,753])));
		return $_out;
	}
	public function __construct($CLASS_ID = 735) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class j_ClassInfo extends java_lang_Object {

	public $_interfaces = null;
	public $_modifiers = 0;
	public $_id = 0;
	public $_internalName = null;
	public $_related = null;
	public $_name = null;
	public $_parent = 0;
	public function j_ClassInfo_init__ILjava_lang_String_Ljava_lang_String_II_I_I_V(int $p0, ?java_lang_String $p1, ?java_lang_String $p2, int $p3, int $p4, ?JA_I $p5, ?JA_I $p6) {
		$lA2 = null;
		$lA2 = ($p1);
		($this)->java_lang_Object_init___V();
		if ((($lA2 != null))) goto label_1;
		$lA2 = ($p2);
		label_1:
		$this->_id = $p0;
		$this->_internalName = ($lA2);
		$this->_name = $p2;
		$this->_modifiers = $p3;
		$this->_parent = $p4;
		$this->_interfaces = $p5;
		$this->_related = $p6;
		return $this;
		return $this;
	}
	public static function create_ILjava_lang_String_Ljava_lang_String_II_I_I_Lj_ClassInfo_(int $p0, ?java_lang_String $p1, ?java_lang_String $p2, int $p3, int $p4, ?JA_I $p5, ?JA_I $p6) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new j_ClassInfo()));
		$fA0 = $tA0;
		($tA0)->j_ClassInfo_init__ILjava_lang_String_Ljava_lang_String_II_I_I_V($p0, $p1, $p2, $p3, $p4, $p5, $p6);
		return ($fA0);
	}
	public function __construct($CLASS_ID = 734) {
		parent::__construct($CLASS_ID);
		$this->_interfaces = null;
		$this->_modifiers = 0;
		$this->_id = 0;
		$this->_internalName = null;
		$this->_related = null;
		$this->_name = null;
		$this->_parent = 0;
	}
	static public function SI() {
	}
}
class j_ProgramReflection extends java_lang_Object {

	public static $__classInfos = null;
	public static $__classNames = null;
	public static $__classInfosByName = null;
	public function j_ProgramReflection_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function _ensure__V() {
		$lA0 = null;
		$lA3 = null;
		$lI1 = 0;
		$lI2 = 0;
		$fA0 = null;
		$tA0 = null;
		if (((j_ProgramReflection::$__classInfos == null))) goto label_1;
		return;
		label_1:
		$tA0 = ((new com_jtransc_ds_FastStringMap()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_ds_FastStringMap_init___V();
		j_ProgramReflection::$__classInfosByName = ($fA0);
		j_ProgramReflection::$__classInfos = j_ProgramReflection::getAllClasses___Lj_ClassInfo_();
		if (((j_ProgramReflection::$__classInfos == null))) goto label_2;
		j_ProgramReflection::$__classNames = new JA_L((j_ProgramReflection::$__classInfos)->length, "[Ljava.lang.String;");
		$lA0 = (j_ProgramReflection::$__classInfos);
		$lI1 = ($lA0)->length;
		$lI2 = 0;
		label_4:
		if ((($lI2 >= $lI1))) goto label_2;
		$lA3 = (($lA0)->get($lI2));
		if ((($lA3 != null))) goto label_5;
		goto label_6;
		label_5:
		j_ProgramReflection::$__classInfosByName->set_Ljava_lang_String_Ljava_lang_Object__V(($lA3)->_name, $lA3);
		(j_ProgramReflection::$__classNames)->set(($lA3)->_id, (($lA3)->_name));
		label_6:
		$lI2 = ((int)(($lI2 + 1)));
		goto label_4;
		label_2:
		return;
	}
	public static function getAllClasses___Lj_ClassInfo_() {
		$fA0 = null;
		if (((j_ProgramReflection::$__classInfos == null))) goto label_1;
		$fA0 = j_ProgramReflection::$__classInfos;
		goto label_2;
		label_1:
		$fA0 = j_ProgramReflection_AllClasses::getAllClasses___Lj_ClassInfo_();
		label_2:
		return $fA0;
	}
	public static function getClassInfoWithName_Ljava_lang_String__Lj_ClassInfo_(?java_lang_String $p0) {
		$fA0 = null;
		j_ProgramReflection::_ensure__V();
		if (!(j_ProgramReflection::hasClassWithName_Ljava_lang_String__Z($p0))) goto label_1;
		$fA0 = (N::checkcast(j_ProgramReflection::$__classInfosByName->get_Ljava_lang_String__Ljava_lang_Object_($p0), "j_ClassInfo"));
		goto label_2;
		label_1:
		$fA0 = null;
		label_2:
		return ($fA0);
	}
	public static function hasClassWithName_Ljava_lang_String__Z(?java_lang_String $p0) {
		j_ProgramReflection::_ensure__V();
		return j_ProgramReflection::$__classInfosByName->has_Ljava_lang_String__Z($p0);
	}
	public static function dynamicSet_IILjava_lang_Object_Ljava_lang_Object__V(int $p0, int $p1, ?java_lang_Object $p2, ?java_lang_Object $p3) {
		j_ProgramReflection_DynamicGetSet::dynamicSet_IILjava_lang_Object_Ljava_lang_Object__V($p0, $p1, $p2, $p3);
		return;
	}
	public static function dynamicGet_IILjava_lang_Object__Ljava_lang_Object_(int $p0, int $p1, ?java_lang_Object $p2) {
		return j_ProgramReflection_DynamicGetSet::dynamicGet_IILjava_lang_Object__Ljava_lang_Object_($p0, $p1, $p2);
	}
	public static function getFields_I__Lj_MemberInfo_(int $p0) {
		return j_ProgramReflection_AllFields::getFields_I__Lj_MemberInfo_($p0);
	}
	public static function dynamicNew_II_Ljava_lang_Object__Ljava_lang_Object_(int $p0, int $p1, ?JA_L $p2) {
		return j_ProgramReflection_DynamicNewInvoke::dynamicNew_II_Ljava_lang_Object__Ljava_lang_Object_($p0, $p1, $p2);
	}
	public static function getConstructors_I__Lj_MemberInfo_(int $p0) {
		return j_ProgramReflection_AllConstructors::getConstructors_I__Lj_MemberInfo_($p0);
	}
	public static function getClassById_I_Ljava_lang_Class_(int $p0) {
		$G = 0;
		$lA1 = null;
		$fA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							j_ProgramReflection::_ensure__V();
							$G = 1;
							continue 2;
						case 1:
							$fA0 = (java_lang_Class::forName_Ljava_lang_String__Ljava_lang_Class_((((j_ProgramReflection::$__classNames)->get($p0)))));
							$G = 2;
							continue 2;
						case 2:return ($fA0); 
						case 3:
							$fA0 = ($J__exception__);
							$lA1 = $fA0;
							($lA1)->printStackTrace__V();
							return null;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_ClassNotFoundException)))) {
					$G = 3;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public function __construct($CLASS_ID = 733) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		j_ProgramReflection::$__classInfos = null;
		j_ProgramReflection::$__classNames = null;
		j_ProgramReflection::$__classInfosByName = null;
	}
}
class java_lang_jtransc_JTranscCoreReflection extends java_lang_Object {

	public function java_lang_jtransc_JTranscCoreReflection_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function getClassById_I_Ljava_lang_Class_(int $p0) {
		j_ProgramReflection::_ensure__V();
		return java_lang_jtransc_JTranscCoreReflection::getClassByName_Ljava_lang_String__Ljava_lang_Class_(java_lang_jtransc_JTranscCoreReflection::getClassNameById_I_Ljava_lang_String_($p0));
	}
	public static function getClassNameById_I_Ljava_lang_String_(int $p0) {
		if (java_lang_jtransc_JTranscCoreReflection::checkClassId_I_Z($p0)) goto label_1;
		return null;
		label_1:
		j_ProgramReflection::_ensure__V();
		return (((j_ProgramReflection::$__classInfos)->get($p0)))->_name;
	}
	public static function checkClassId_I_Z(int $p0) {
		$fI0 = 0;
		j_ProgramReflection::_ensure__V();
		if ((($p0 < 0))) goto label_1;
		if ((($p0 >= (j_ProgramReflection::$__classInfos)->length))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public static function getClassByName_Ljava_lang_String__Ljava_lang_Class_(?java_lang_String $p0) {
		$G = 0;
		$fA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$G = 1;
							continue 2;
						case 1:
							$fA0 = (java_lang_Class::forName_Ljava_lang_String__Ljava_lang_Class_($p0));
							$G = 2;
							continue 2;
						case 2:return ($fA0); 
						case 3:
							$fA0 = ($J__exception__);
							return null;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_ClassNotFoundException)))) {
					$G = 3;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public static function getClassId_Ljava_lang_Object__I(?java_lang_Object $p0) {
		return $p0->__JT__CLASS_ID;
	}
	public static function isArray_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		return $p0 instanceof JA_0;
	}
	public static function getArrayDescriptor_Ljava_lang_Object__Ljava_lang_String_(?java_lang_Object $p0) {
		return N::str($p0->desc);
	}
	public static function getClassInfoWithName_Ljava_lang_String__Lj_ClassInfo_(?java_lang_String $p0) {
		return j_ProgramReflection::getClassInfoWithName_Ljava_lang_String__Lj_ClassInfo_($p0);
	}
	public static function getModifiersWithId_I_I(int $p0) {
		j_ProgramReflection::_ensure__V();
		return (((j_ProgramReflection::$__classInfos)->get($p0)))->_modifiers;
	}
	public static function getDeclaredFields_Ljava_lang_Class___Ljava_lang_reflect_Field_(?java_lang_Class $p0) {
		$lA1 = null;
		$fI0 = 0;
		$lA3 = null;
		$fI1 = 0;
		$lI2 = 0;
		$lI4 = 0;
		$fA0 = null;
		$fA2 = null;
		$tA0 = null;
		$lA1 = (j_ProgramReflection::getFields_I__Lj_MemberInfo_(java_lang_jtransc_JTranscCoreReflection::getClassId_Ljava_lang_Class__I($p0)));
		if ((($lA1 == null))) goto label_1;
		$fI0 = ($lA1)->length;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		$lI2 = $fI0;
		$lA3 = (new JA_L($lI2, "[Ljava.lang.reflect.Field;"));
		$lI4 = 0;
		label_3:
		if ((($lI4 >= $lI2))) goto label_4;
		$fA0 = $lA3;
		$fI1 = $lI4;
		$tA0 = ((new java_lang_reflect_Field()));
		$fA2 = $tA0;
		($tA0)->java_lang_reflect_Field_init__Ljava_lang_Class_Lj_MemberInfo__V($p0, ((($lA1)->get($lI4))));
		($fA0)->set($fI1, $fA2);
		$lI4 = ((int)(($lI4 + 1)));
		goto label_3;
		label_4:
		return ($lA3);
	}
	public static function getClassId_Ljava_lang_Class__I(?java_lang_Class $p0) {
		return $p0->_id;
	}
	public static function getDeclaredConstructors_Ljava_lang_Class___Ljava_lang_reflect_Constructor_(?java_lang_Class $p0) {
		$lA1 = null;
		$fI0 = 0;
		$lA3 = null;
		$fI1 = 0;
		$lI2 = 0;
		$lI4 = 0;
		$fA0 = null;
		$fA2 = null;
		$tA0 = null;
		$lA1 = (j_ProgramReflection::getConstructors_I__Lj_MemberInfo_(java_lang_jtransc_JTranscCoreReflection::getClassId_Ljava_lang_Class__I($p0)));
		if ((($lA1 == null))) goto label_1;
		$fI0 = ($lA1)->length;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		$lI2 = $fI0;
		$lA3 = (new JA_L($lI2, "[Ljava.lang.reflect.Constructor;"));
		$lI4 = 0;
		label_3:
		if ((($lI4 >= $lI2))) goto label_4;
		$fA0 = $lA3;
		$fI1 = $lI4;
		$tA0 = ((new java_lang_reflect_Constructor()));
		$fA2 = $tA0;
		($tA0)->java_lang_reflect_Constructor_init__Ljava_lang_Class_Lj_MemberInfo__V($p0, ((($lA1)->get($lI4))));
		($fA0)->set($fI1, $fA2);
		$lI4 = ((int)(($lI4 + 1)));
		goto label_3;
		label_4:
		return ($lA3);
	}
	public static function hasClassWithName_Ljava_lang_String__Z(?java_lang_String $p0) {
		return j_ProgramReflection::hasClassWithName_Ljava_lang_String__Z($p0);
	}
	public function __construct($CLASS_ID = 732) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_reflect_Array extends java_lang_Object {

	public function java_lang_reflect_Array_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function newInstance_Ljava_lang_Class_I_Ljava_lang_Object_(?java_lang_Class $p0, int $p1) {
		$fI0 = 0;
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		$tA3 = null;
		$tA4 = null;
		if (((($p0) != null))) goto label_1;
		$tA0 = ((new java_lang_NullPointerException()));
		$fA0 = $tA0;
		($tA0)->java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_727);
		throw new WrappedThrowable($fA0);
		label_1:
		if ($p0->isPrimitive__Z()) goto label_2;
		$fI0 = $p1;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA1 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		return java_lang_reflect_Array::newObjectInstance_ILjava_lang_String__Ljava_lang_Object_($fI0, ($fA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_728)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($p0->getName__Ljava_lang_String_())->toString__Ljava_lang_String_());
		label_2:
		if (((($p0) != java_lang_Boolean::$_TYPE))) goto label_3;
		return (new JA_Z($p1));
		label_3:
		if (((($p0) != java_lang_Byte::$_TYPE))) goto label_4;
		return (new JA_B($p1));
		label_4:
		if (((($p0) != java_lang_Short::$_TYPE))) goto label_5;
		return (new JA_S($p1));
		label_5:
		if (((($p0) != java_lang_Character::$_TYPE))) goto label_6;
		return (new JA_C($p1));
		label_6:
		if (((($p0) != java_lang_Integer::$_TYPE))) goto label_7;
		return (new JA_I($p1));
		label_7:
		if (((($p0) != java_lang_Long::$_TYPE))) goto label_8;
		return (new JA_J($p1));
		label_8:
		if (((($p0) != java_lang_Float::$_TYPE))) goto label_9;
		return (new JA_F($p1));
		label_9:
		if (((($p0) != java_lang_Double::$_TYPE))) goto label_10;
		return (new JA_D($p1));
		label_10:
		if (((($p0) != java_lang_Void::$_TYPE))) goto label_11;
		$tA2 = ((new java_lang_RuntimeException()));
		$fA0 = $tA2;
		($tA2)->java_lang_RuntimeException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_729);
		throw new WrappedThrowable($fA0);
		label_11:
		$tA3 = ((new java_lang_RuntimeException()));
		$fA0 = $tA3;
		$fA1 = $tA3;
		$tA4 = ((new java_lang_StringBuilder()));
		$fA2 = $tA4;
		($tA4)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_RuntimeException_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_730)->append_Ljava_lang_Object__Ljava_lang_StringBuilder_(($p0))->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
	}
	public static function newObjectInstance_ILjava_lang_String__Ljava_lang_Object_(int $p0, ?java_lang_String $p1) {
		return new JA_L($p0, N::istr($p1));
	}
	public static function getLength_Ljava_lang_Object__I(?java_lang_Object $p0) {
		return $p0->length;
	}
	public function __construct($CLASS_ID = 731) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_ConcurrentModificationException extends java_lang_RuntimeException {

	public function java_util_ConcurrentModificationException_init___V() {
		($this)->java_lang_RuntimeException_init___V();
		return $this;
		return $this;
	}
	public function java_util_ConcurrentModificationException_init__Ljava_lang_Throwable__V(?java_lang_Throwable $p0) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_Throwable__V($p0);
		return $this;
		return $this;
	}
	public function java_util_ConcurrentModificationException_init__Ljava_lang_String_Ljava_lang_Throwable__V(?java_lang_String $p0, ?java_lang_Throwable $p1) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_String_Ljava_lang_Throwable__V($p0, $p1);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 730) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_util_NoSuchElementException extends java_lang_RuntimeException {

	public function java_util_NoSuchElementException_init___V() {
		($this)->java_lang_RuntimeException_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 729) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_util_ListIterator extends java_util_Iterator {

	public function next__Ljava_lang_Object_();
	public function hasNext__Z();
	public function previousIndex__I();
}
class java_util_ListIterator_IFields {

	static public function SI() {
	}
}
class java_util_AbstractList_SimpleListIterator extends java_lang_Object implements java_util_Iterator {

	public $_expectedModCount = 0;
	public $_this_0 = null;
	public $_pos = 0;
	public $_lastPosition = 0;
	public function java_util_AbstractList_SimpleListIterator_init__Ljava_util_AbstractList__V(?java_util_AbstractList $p0) {
		$this->_this_0 = $p0;
		($this)->java_lang_Object_init___V();
		$this->_pos = -1;
		$this->_lastPosition = -1;
		$this->_expectedModCount = $p0->_modCount;
		return $this;
		return $this;
	}
	public function next__Ljava_lang_Object_() {
		$G = 0;
		$lA1 = null;
		$fI1 = 0;
		$fA0 = null;
		$fA1 = null;
		$tI1 = 0;
		$tA2 = null;
		$tA4 = null;
		$tA3 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							if ((($this->_expectedModCount != $this->_this_0->_modCount))) {
								$G = 1;
								continue 2;
							}
							$G = 2;
							continue 2;
						case 2:
							$lA1 = $this->_this_0->get_I_Ljava_lang_Object_(((int)(($this->_pos + 1))));
							$fA0 = ($this);
							$fA1 = ($this);
							$tA2 = $fA1;
							$tI1 = ((int)(($this->_pos + 1)));
							$fI1 = $tI1;
							($tA2)->_pos = $tI1;
							($fA0)->_lastPosition = $fI1;
							$fA0 = $lA1;
							$G = 3;
							continue 2;
						case 3:return $fA0; 
						case 4:
							$fA0 = ($J__exception__);
							$lA1 = $fA0;
							$tA4 = ((new java_util_NoSuchElementException()));
							$fA0 = $tA4;
							($tA4)->java_util_NoSuchElementException_init___V();
							throw new WrappedThrowable($fA0);
							$G = 1;
							continue 2;
						case 1:
							$tA3 = ((new java_util_ConcurrentModificationException()));
							$fA0 = $tA3;
							($tA3)->java_util_ConcurrentModificationException_init___V();
							throw new WrappedThrowable($fA0);
							break;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 2)) && (($G < 3)))) && (($J__exception__) instanceof java_lang_IndexOutOfBoundsException)))) {
					$G = 4;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public function hasNext__Z() {
		$fI0 = 0;
		if (((((int)(($this->_pos + 1))) >= $this->_this_0->size__I()))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function __construct($CLASS_ID = 726) {
		parent::__construct($CLASS_ID);
		$this->_expectedModCount = 0;
		$this->_this_0 = null;
		$this->_pos = 0;
		$this->_lastPosition = 0;
	}
	static public function SI() {
	}
}
class java_util_AbstractList_FullListIterator extends java_util_AbstractList_SimpleListIterator implements java_util_ListIterator {

	public $_this_0_ = null;
	public function java_util_AbstractList_FullListIterator_init__Ljava_util_AbstractList_I_V(?java_util_AbstractList $p0, int $p1) {
		$fA0 = null;
		$tA0 = null;
		$this->_this_0_ = $p0;
		($this)->java_util_AbstractList_SimpleListIterator_init__Ljava_util_AbstractList__V($p0);
		if ((($p1 < 0))) goto label_1;
		if ((($p1 > $p0->size__I()))) goto label_1;
		$this->_pos = ((int)(($p1 - 1)));
		goto label_3;
		label_1:
		$tA0 = ((new java_lang_IndexOutOfBoundsException()));
		$fA0 = $tA0;
		($tA0)->java_lang_IndexOutOfBoundsException_init___V();
		throw new WrappedThrowable($fA0);
		label_3:
		return $this;
		return $this;
	}
	public function previousIndex__I() {
		return $this->_pos;
	}
	public function __construct($CLASS_ID = 728) {
		parent::__construct($CLASS_ID);
		$this->_this_0_ = null;
	}
	static public function SI() {
	}
}
class java_util_ArrayList extends java_util_AbstractList implements java_util_List, java_util_RandomAccess, java_lang_Cloneable, java_io_Serializable {

	public $_length = 0;
	public $_buffer = null;
	public function java_util_ArrayList_init___V() {
		$this->java_util_ArrayList_init__I_V(0);
		return $this;
		return $this;
	}
	public function java_util_ArrayList_init__I_V(int $p0) {
		($this)->java_util_AbstractList_init___V();
		$this->_buffer = new JA_L($p0, "[Ljava.lang.Object;");
		$this->_length = 0;
		return $this;
		return $this;
	}
	public function size__I() {
		return $this->_length;
	}
	public function isEmpty__Z() {
		$fI0 = 0;
		if ((($this->size__I() != 0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function iterator__Ljava_util_Iterator_() {
		return ($this->listIterator__Ljava_util_ListIterator_());
	}
	public function get_I_Ljava_lang_Object_(int $p0) {
		$this->rangeCheck_I_V($p0);
		return $this->_get_I_Ljava_lang_Object_($p0);
	}
	public function _get_I_Ljava_lang_Object_(int $p0) {
		return ($this->_buffer->get($p0));
	}
	public function rangeCheck_I_V(int $p0) {
		$fA0 = null;
		$tA0 = null;
		if ((($p0 < $this->size__I()))) goto label_1;
		$tA0 = ((new java_lang_IndexOutOfBoundsException()));
		$fA0 = $tA0;
		($tA0)->java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V($this->outOfBoundsMsg_I_Ljava_lang_String_($p0));
		throw new WrappedThrowable($fA0);
		label_1:
		return;
	}
	public function outOfBoundsMsg_I_Ljava_lang_String_(int $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_732)->append_I_Ljava_lang_StringBuilder_($p0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_731)->append_I_Ljava_lang_StringBuilder_($this->size__I())->toString__Ljava_lang_String_();
	}
	public function add_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$this->_add_Ljava_lang_Object__V($p0);
		return true;
	}
	public function _add_Ljava_lang_Object__V(?java_lang_Object $p0) {
		$fI1 = 0;
		$fA0 = null;
		$fA1 = null;
		$tI1 = 0;
		$tA2 = null;
		$this->ensure_I_V(((int)(($this->_length + 1))));
		$fA0 = $this->_buffer;
		$fA1 = ($this);
		$tA2 = $fA1;
		$tI1 = $this->_length;
		$fI1 = $tI1;
		($tA2)->_length = ((int)(($tI1 + 1)));
		$fA0->set($fI1, $p0);
		return;
	}
	public function ensure_I_V(int $p0) {
		if ((($p0 <= ($this->_buffer)->length))) goto label_1;
		$this->_buffer = java_util_Arrays::copyOf__Ljava_lang_Object_I__Ljava_lang_Object_($this->_buffer, java_lang_Math::max_II_I($p0, ((int)((((int)(N::imul(($this->_buffer)->length, 2))) + 2)))));
		label_1:
		return;
	}
	public function add_ILjava_lang_Object__V(int $p0, ?java_lang_Object $p1) {
		$this->rangeCheckForAdd_I_V($p0);
		$this->_insert_ILjava_lang_Object__V($p0, $p1);
		return;
	}
	public function _insert_ILjava_lang_Object__V(int $p0, ?java_lang_Object $p1) {
		$this->makeHole_II_V($p0, 1);
		$this->_buffer->set($p0, $p1);
		return;
	}
	public function makeHole_II_V(int $p0, int $p1) {
		$this->ensure_I_V(((int)(($this->_length + $p1))));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($this->_buffer), $p0, ($this->_buffer), ((int)(($p0 + $p1))), ((int)((((int)(($this->_length - $p0))) - $p1))));
		$this->_length = ((int)(($this->_length + $p1)));
		return;
	}
	public function rangeCheckForAdd_I_V(int $p0) {
		$fA0 = null;
		$tA0 = null;
		if ((($p0 > $this->size__I()))) goto label_1;
		if ((($p0 >= 0))) goto label_2;
		label_1:
		$tA0 = ((new java_lang_IndexOutOfBoundsException()));
		$fA0 = $tA0;
		($tA0)->java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V($this->outOfBoundsMsg_I_Ljava_lang_String_($p0));
		throw new WrappedThrowable($fA0);
		label_2:
		return;
	}
	public function toArray__Ljava_lang_Object___Ljava_lang_Object_(?JA_L $p0) {
		$lA1 = null;
		$lI2 = 0;
		$lI3 = 0;
		$lA1 = ($p0);
		$lI2 = $this->size__I();
		if (((($lA1)->length >= $lI2))) goto label_1;
		$lA1 = (N::checkcast(java_util_Arrays::copyOf__Ljava_lang_Object_ILjava_lang_Class___Ljava_lang_Object_(new JA_L(0, "[Ljava.lang.Object;"), $lI2, $lA1->getClass__Ljava_lang_Class_()), "JA_L"));
		label_1:
		$lI3 = 0;
		label_2:
		if ((($lI3 >= $lI2))) goto label_3;
		($lA1)->set($lI3, $this->_get_I_Ljava_lang_Object_($lI3));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_2;
		label_3:
		return ($lA1);
	}
	public function contains_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if ((($this->indexOf_Ljava_lang_Object__I($p0) < 0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function indexOf_Ljava_lang_Object__I(?java_lang_Object $p0) {
		$lI2 = 0;
		$lI3 = 0;
		$lI2 = $this->size__I();
		$lI3 = 0;
		label_1:
		if ((($lI3 >= $lI2))) goto label_2;
		if (!(java_util_Objects::equals_Ljava_lang_Object_Ljava_lang_Object__Z($p0, $this->_get_I_Ljava_lang_Object_($lI3)))) goto label_3;
		return $lI3;
		label_3:
		$lI3 = ((int)(($lI3 + 1)));
		goto label_1;
		label_2:
		return -1;
	}
	public function __construct($CLASS_ID = 717) {
		parent::__construct($CLASS_ID);
		$this->_length = 0;
		$this->_buffer = null;
	}
	static public function SI() {
	}
}
class java_lang_reflect_ArrayType extends java_lang_Object implements java_lang_reflect_Type {

	public $_element = null;
	public function java_lang_reflect_ArrayType_init__Ljava_lang_reflect_Type__V(?java_lang_reflect_Type $p0) {
		($this)->java_lang_Object_init___V();
		$this->_element = $p0;
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 716) {
		parent::__construct($CLASS_ID);
		$this->_element = null;
	}
	static public function SI() {
	}
}
class com_jtransc_text_MStringReader extends java_lang_Object {

	public $_length = 0;
	public $_offset = 0;
	public $_str = null;
	public function com_jtransc_text_MStringReader_init__Ljava_lang_String__V(?java_lang_String $p0) {
		$this->com_jtransc_text_MStringReader_init__Ljava_lang_String_I_V($p0, 0);
		return $this;
		return $this;
	}
	public function com_jtransc_text_MStringReader_init__Ljava_lang_String_I_V(?java_lang_String $p0, int $p1) {
		($this)->java_lang_Object_init___V();
		$this->_str = $p0;
		$this->_length = $p0->length__I();
		$this->_offset = $p1;
		return $this;
		return $this;
	}
	public function hasMore__Z() {
		$fI0 = 0;
		if ((($this->_offset >= $this->_length))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function peek__C() {
		$fA0 = null;
		$tA0 = null;
		if ($this->hasMore__Z()) goto label_1;
		$tA0 = ((new java_lang_Error()));
		$fA0 = $tA0;
		($tA0)->java_lang_Error_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_733);
		throw new WrappedThrowable($fA0);
		label_1:
		return $this->_str->charAt_I_C($this->_offset);
	}
	public function read__C() {
		$lI1 = 0;
		$fA0 = null;
		$tA0 = null;
		if ($this->hasMore__Z()) goto label_1;
		$tA0 = ((new java_lang_Error()));
		$fA0 = $tA0;
		($tA0)->java_lang_Error_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_733);
		throw new WrappedThrowable($fA0);
		label_1:
		$lI1 = ((int)($this->peek__C()));
		$this->skip__V();
		return (N::i2c($lI1));
	}
	public function skip__V() {
		$this->skip_I_V(1);
		return;
	}
	public function skip_I_V(int $p0) {
		$this->_offset = ((int)(($this->_offset + $p0)));
		return;
	}
	public function readUntil_CCZ_Ljava_lang_String_(int $p0, int $p1, bool $p2) {
		return $this->readUntil_CCCZ_Ljava_lang_String_($p0, $p1, $p1, $p2);
	}
	public function readUntil_CCCZ_Ljava_lang_String_(int $p0, int $p1, int $p2, bool $p3) {
		$lI5 = 0;
		$lI6 = 0;
		$lI5 = $this->_offset;
		label_1:
		if (!($this->hasMore__Z())) goto label_2;
		$lI6 = ((int)($this->read__C()));
		if ((($lI6 == ((int)($p0))))) goto label_4;
		if ((($lI6 == ((int)($p1))))) goto label_4;
		if ((($lI6 != ((int)($p2))))) goto label_5;
		label_4:
		if ($p3) goto label_2;
		$this->skip_I_V(-1);
		goto label_2;
		label_5:
		goto label_1;
		label_2:
		return $this->_str->substring_II_Ljava_lang_String_($lI5, $this->_offset);
	}
	public function expect_C_V(int $p0) {
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		$tA3 = null;
		if ($this->hasMore__Z()) goto label_1;
		$tA0 = ((new java_lang_Error()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_Error_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_735)->append_C_Ljava_lang_StringBuilder_($p0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_734)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_1:
		if ((($this->read__C() == ((int)($p0))))) goto label_2;
		$tA2 = ((new java_lang_Error()));
		$fA0 = $tA2;
		$fA1 = $tA2;
		$tA3 = ((new java_lang_StringBuilder()));
		$fA2 = $tA3;
		($tA3)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_Error_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_735)->append_C_Ljava_lang_StringBuilder_($p0)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_2:
		return;
	}
	public function __construct($CLASS_ID = 714) {
		parent::__construct($CLASS_ID);
		$this->_length = 0;
		$this->_offset = 0;
		$this->_str = null;
	}
	static public function SI() {
	}
}
class java_lang_reflect_MethodTypeImpl extends java_lang_Object implements java_lang_reflect_Type {

	public $_args = null;
	public $_rettype = null;
	public function java_lang_reflect_MethodTypeImpl_init___Ljava_lang_reflect_Type_Ljava_lang_reflect_Type__V(?JA_L $p0, ?java_lang_reflect_Type $p1) {
		($this)->java_lang_Object_init___V();
		$this->_args = $p0;
		$this->_rettype = $p1;
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 713) {
		parent::__construct($CLASS_ID);
		$this->_args = null;
		$this->_rettype = null;
	}
	static public function SI() {
	}
}
class j_MemberInfo extends java_lang_Object {

	public $_modifiers = 0;
	public $_desc = null;
	public $_internalName = null;
	public $_id = 0;
	public $_name = null;
	public $_genericDesc = null;
	public function j_MemberInfo_init__ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__V(int $p0, ?java_lang_String $p1, ?java_lang_String $p2, int $p3, ?java_lang_String $p4, ?java_lang_String $p5) {
		$fA1 = null;
		($this)->java_lang_Object_init___V();
		if (((($p1) == null))) goto label_1;
		$fA1 = $p1;
		goto label_2;
		label_1:
		$fA1 = $p2;
		label_2:
		$this->_internalName = $fA1;
		$this->_id = $p0;
		$this->_name = $p2;
		$this->_modifiers = $p3;
		$this->_desc = $p4;
		$this->_genericDesc = $p5;
		return $this;
		return $this;
	}
	public static function create_ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__Lj_MemberInfo_(int $p0, ?java_lang_String $p1, ?java_lang_String $p2, int $p3, ?java_lang_String $p4, ?java_lang_String $p5) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new j_MemberInfo()));
		$fA0 = $tA0;
		($tA0)->j_MemberInfo_init__ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__V($p0, $p1, $p2, $p3, $p4, $p5);
		return ($fA0);
	}
	public static function createList_I_I_I_Ljava_lang_String__Ljava_lang_String__Ljava_lang_String__Ljava_lang_String___Lj_MemberInfo_(int $p0, ?JA_I $p1, ?JA_I $p2, ?JA_L $p3, ?JA_L $p4, ?JA_L $p5, ?JA_L $p6) {
		$lA7 = null;
		$fI1 = 0;
		$lI8 = 0;
		$fA0 = null;
		$fA2 = null;
		$tA0 = null;
		$lA7 = (new JA_L($p0, "[Lj.MemberInfo;"));
		$lI8 = 0;
		label_1:
		if ((($lI8 >= $p0))) goto label_2;
		$fA0 = $lA7;
		$fI1 = $lI8;
		$tA0 = ((new j_MemberInfo()));
		$fA2 = $tA0;
		($tA0)->j_MemberInfo_init__ILjava_lang_String_Ljava_lang_String_ILjava_lang_String_Ljava_lang_String__V(($p1->get($lI8)), ((($p3)->get($lI8))), ((($p4)->get($lI8))), ($p2->get($lI8)), ((($p5)->get($lI8))), ((($p6)->get($lI8))));
		($fA0)->set($fI1, $fA2);
		$lI8 = ((int)(($lI8 + 1)));
		goto label_1;
		label_2:
		return ($lA7);
	}
	public function __construct($CLASS_ID = 712) {
		parent::__construct($CLASS_ID);
		$this->_modifiers = 0;
		$this->_desc = null;
		$this->_internalName = null;
		$this->_id = 0;
		$this->_name = null;
		$this->_genericDesc = null;
	}
	static public function SI() {
	}
}
class java_lang_reflect_Method extends java_lang_reflect_MethodConstructor implements java_lang_reflect_Member, java_lang_reflect_GenericDeclaration {

	public function java_lang_reflect_Method_init__Ljava_lang_Class_Lj_MemberInfo__V(?java_lang_Class $p0, ?j_MemberInfo $p1) {
		($this)->java_lang_reflect_MethodConstructor_init__Ljava_lang_Class_Lj_MemberInfo__V($p0, $p1);
		return $this;
		return $this;
	}
	public function getModifiers__I() {
		return $this->_modifiers;
	}
	public function getName__Ljava_lang_String_() {
		return $this->_name;
	}
	public function isConstructor__Z() {
		return false;
	}
	public function getReturnType__Ljava_lang_Class_() {
		return N::checkcast($this->methodType__Ljava_lang_reflect_MethodTypeImpl_()->_rettype, "java_lang_Class");
	}
	public function getParameterTypes___Ljava_lang_Class_() {
		return N::checkcast(N::checkcast($this->methodType__Ljava_lang_reflect_MethodTypeImpl_()->_args, "JA_L"), "JA_L");
	}
	public function __construct($CLASS_ID = 710) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class java_lang_Number extends java_lang_Object {

	public function java_lang_Number_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function doubleValue__D() {
		throw new Exception("Missing body java.lang.Number.doubleValue()D");
	}
	public function longValue__J() {
		throw new Exception("Missing body java.lang.Number.longValue()J");
	}
	public function shortValue__S() {
		return (N::i2s($this->intValue__I()));
	}
	public function intValue__I() {
		throw new Exception("Missing body java.lang.Number.intValue()I");
	}
	public function byteValue__B() {
		return (N::i2b($this->intValue__I()));
	}
	public function floatValue__F() {
		throw new Exception("Missing body java.lang.Number.floatValue()F");
	}
	public function __construct($CLASS_ID = 690) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_Byte extends java_lang_Number implements java_lang_Comparable {

	public static $_cache = null;
	public $_value = 0;
	public static $_TYPE = null;
	public static function valueOf_B_Ljava_lang_Byte_(int $p0) {
		$fI1 = 0;
		$lI1 = 0;
		$fA0 = null;
		$fA2 = null;
		$tA0 = null;
		$lI1 = ((int)((((int)($p0)) + 128)));
		if (((((java_lang_Byte::$_cache)->get($lI1)) != null))) goto label_1;
		$fA0 = (java_lang_Byte::$_cache);
		$fI1 = $lI1;
		$tA0 = ((new java_lang_Byte()));
		$fA2 = $tA0;
		($tA0)->java_lang_Byte_init__B_V($p0);
		($fA0)->set($fI1, $fA2);
		label_1:
		return (((java_lang_Byte::$_cache)->get($lI1)));
	}
	public function java_lang_Byte_init__B_V(int $p0) {
		($this)->java_lang_Number_init___V();
		$this->_value = $p0;
		return $this;
		return $this;
	}
	public static function java_lang_Byte_clinit___V() {
		java_lang_Byte::$_TYPE = java_lang_Class::getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap::$STRINGLIT_736);
		java_lang_Byte::$_cache = new JA_L(256, "[Ljava.lang.Byte;");
		return;
	}
	public function toString__Ljava_lang_String_() {
		return java_lang_Integer::toString_I_Ljava_lang_String_(((int)($this->_value)));
	}
	public function doubleValue__D() {
		return ((double)($this->_value));
	}
	public function hashCode__I() {
		return java_lang_Byte::hashCode_B_I($this->_value);
	}
	public static function hashCode_B_I(int $p0) {
		return ((int)($p0));
	}
	public function longValue__J() {
		return N::i2j($this->_value);
	}
	public function intValue__I() {
		return ((int)($this->_value));
	}
	public function shortValue__S() {
		return (N::i2s($this->_value));
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if (!((($p0) instanceof java_lang_Byte))) goto label_1;
		if ((($this->_value != N::checkcast($p0, "java_lang_Byte")->byteValue__B()))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function byteValue__B() {
		return $this->_value;
	}
	public function floatValue__F() {
		return ((double)($this->_value));
	}
	public function __construct($CLASS_ID = 709) {
		parent::__construct($CLASS_ID);
		$this->_value = 0;
	}
	static public function SI() {
		java_lang_Byte::$_cache = null;
		java_lang_Byte::$_TYPE = null;
		java_lang_Byte::java_lang_Byte_clinit___V();
	}
}
class java_lang_Boolean extends java_lang_Object implements java_io_Serializable, java_lang_Comparable {

	public static $_TYPE = null;
	public static $_TRUE = null;
	public static $_FALSE = null;
	public $_value = false;
	public static function java_lang_Boolean_clinit___V() {
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		$tA0 = ((new java_lang_Boolean()));
		$fA0 = $tA0;
		($tA0)->java_lang_Boolean_init__Z_V(true);
		java_lang_Boolean::$_TRUE = ($fA0);
		$tA1 = ((new java_lang_Boolean()));
		$fA0 = $tA1;
		($tA1)->java_lang_Boolean_init__Z_V(false);
		java_lang_Boolean::$_FALSE = ($fA0);
		java_lang_Boolean::$_TYPE = java_lang_Class::getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap::$STRINGLIT_737);
		return;
	}
	public function java_lang_Boolean_init__Z_V(bool $p0) {
		($this)->java_lang_Object_init___V();
		$this->_value = $p0;
		return $this;
		return $this;
	}
	public function toString__Ljava_lang_String_() {
		return java_lang_Boolean::toString_Z_Ljava_lang_String_($this->_value);
	}
	public static function toString_Z_Ljava_lang_String_(bool $p0) {
		$fA0 = null;
		if (!($p0)) goto label_1;
		$fA0 = Bootstrap::$STRINGLIT_738;
		goto label_2;
		label_1:
		$fA0 = Bootstrap::$STRINGLIT_739;
		label_2:
		return $fA0;
	}
	public function hashCode__I() {
		return java_lang_Boolean::hashCode_Z_I($this->_value);
	}
	public static function hashCode_Z_I(bool $p0) {
		$fI0 = 0;
		if (!($p0)) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return $fI0;
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if (((($this) != $p0))) goto label_1;
		return true;
		label_1:
		if ((($p0 != null))) goto label_2;
		return false;
		label_2:
		if (((($this)->getClass__Ljava_lang_Class_() == $p0->getClass__Ljava_lang_Class_()))) goto label_3;
		return false;
		label_3:
		if ((($this->_value != N::checkcast($p0, "java_lang_Boolean")->_value))) goto label_4;
		$fI0 = 1;
		goto label_5;
		label_4:
		$fI0 = 0;
		label_5:
		return (($fI0)!=0);
	}
	public function booleanValue__Z() {
		return $this->_value;
	}
	public static function valueOf_Z_Ljava_lang_Boolean_(bool $p0) {
		$fA0 = null;
		if (!($p0)) goto label_1;
		$fA0 = java_lang_Boolean::$_TRUE;
		goto label_2;
		label_1:
		$fA0 = java_lang_Boolean::$_FALSE;
		label_2:
		return $fA0;
	}
	public function __construct($CLASS_ID = 708) {
		parent::__construct($CLASS_ID);
		$this->_value = false;
	}
	static public function SI() {
		java_lang_Boolean::$_TYPE = null;
		java_lang_Boolean::$_TRUE = null;
		java_lang_Boolean::$_FALSE = null;
		java_lang_Boolean::java_lang_Boolean_clinit___V();
	}
}
class com_jtransc_io_JTranscConsole extends java_lang_Object {

	public function com_jtransc_io_JTranscConsole_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function error_Ljava_lang_Object__V(?java_lang_Object $p0) {
		echo "$p0\n";
	}
	public static function logOrError_Ljava_lang_Object_Z_V(?java_lang_Object $p0, bool $p1) {
		if (!($p1)) goto label_1;
		com_jtransc_io_JTranscConsole::error_Ljava_lang_Object__V($p0);
		goto label_3;
		label_1:
		com_jtransc_io_JTranscConsole::log_Ljava_lang_Object__V($p0);
		label_3:
		return;
	}
	public static function log_Ljava_lang_Object__V(?java_lang_Object $p0) {
		echo ($p0 !== null) ? "$p0" : 'null', "\n";
	}
	public function __construct($CLASS_ID = 707) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_Short extends java_lang_Number implements java_lang_Comparable {

	public $_value = 0;
	public static $_TYPE = null;
	public static function valueOf_S_Ljava_lang_Short_(int $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_Short()));
		$fA0 = $tA0;
		($tA0)->java_lang_Short_init__S_V($p0);
		return ($fA0);
	}
	public function java_lang_Short_init__S_V(int $p0) {
		($this)->java_lang_Number_init___V();
		$this->_value = $p0;
		return $this;
		return $this;
	}
	public static function java_lang_Short_clinit___V() {
		java_lang_Short::$_TYPE = java_lang_Class::getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap::$STRINGLIT_740);
		return;
	}
	public function toString__Ljava_lang_String_() {
		return java_lang_Integer::toString_I_Ljava_lang_String_(((int)($this->_value)));
	}
	public function doubleValue__D() {
		return ((double)($this->_value));
	}
	public function hashCode__I() {
		return java_lang_Short::hashCode_S_I($this->_value);
	}
	public static function hashCode_S_I(int $p0) {
		return ((int)($p0));
	}
	public function longValue__J() {
		return N::i2j($this->_value);
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if (!((($p0) instanceof java_lang_Short))) goto label_1;
		if ((($this->_value != N::checkcast($p0, "java_lang_Short")->shortValue__S()))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function shortValue__S() {
		return $this->_value;
	}
	public function intValue__I() {
		return ((int)($this->_value));
	}
	public function byteValue__B() {
		return (N::i2b($this->_value));
	}
	public function floatValue__F() {
		return ((double)($this->_value));
	}
	public static function reverseBytes_S_S(int $p0) {
		return (N::i2s(((int)((((int)(N::ishr(((int)((((int)($p0)) & 65280))), 8))) | ((int)(N::ishl(((int)((((int)($p0)) & 255))), 8))))))));
	}
	public function __construct($CLASS_ID = 706) {
		parent::__construct($CLASS_ID);
		$this->_value = 0;
	}
	static public function SI() {
		java_lang_Short::$_TYPE = null;
		java_lang_Short::java_lang_Short_clinit___V();
	}
}
class com_jtransc_internal_JTranscCType extends java_lang_Object {

	public function com_jtransc_internal_JTranscCType_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function encodeDigit_I_C(int $p0) {
		return java_lang_Character::forDigit_II_C($p0, 36);
	}
	public static function isDigit_C_Z(int $p0) {
		$fI0 = 0;
		if (((((int)($p0)) < 48))) goto label_1;
		if (((((int)($p0)) <= 57))) goto label_2;
		label_1:
		if (((((int)($p0)) < 97))) goto label_3;
		if (((((int)($p0)) <= 122))) goto label_2;
		label_3:
		if (((((int)($p0)) < 65))) goto label_4;
		if (((((int)($p0)) > 90))) goto label_4;
		label_2:
		$fI0 = 1;
		goto label_5;
		label_4:
		$fI0 = 0;
		label_5:
		return (($fI0)!=0);
	}
	public function __construct($CLASS_ID = 705) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_Long extends java_lang_Number implements java_lang_Comparable {

	public $_value = null;
	public static $_TYPE = null;
	public static function valueOf_J_Ljava_lang_Long_(Int64 $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_Long()));
		$fA0 = $tA0;
		($tA0)->java_lang_Long_init__J_V($p0);
		return ($fA0);
	}
	public function java_lang_Long_init__J_V(Int64 $p0) {
		($this)->java_lang_Number_init___V();
		$this->_value = $p0;
		return $this;
		return $this;
	}
	public static function java_lang_Long_clinit___V() {
		java_lang_Long::$_TYPE = java_lang_Class::getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap::$STRINGLIT_741);
		return;
	}
	public function toString__Ljava_lang_String_() {
		return java_lang_Long::toString_J_Ljava_lang_String_($this->_value);
	}
	public static function toString_J_Ljava_lang_String_(Int64 $p0) {
		return java_lang_Long::toString_JI_Ljava_lang_String_($p0, 10);
	}
	public static function toString_JI_Ljava_lang_String_(Int64 $p0, int $p1) {
		$lA3 = null;
		$fI0 = 0;
		$lI4 = 0;
		$fA0 = null;
		$lJ0 = Int64::make(0, 0);
		$tA0 = null;
		$lJ0 = $p0;
		if (((((int)(N::lcmp($lJ0, Int64::make(0, 0)))) != 0))) goto label_1;
		return Bootstrap::$STRINGLIT_742;
		label_1:
		if (((((int)(N::lcmp($lJ0, Int64::make(-2147483648, 0)))) != 0))) goto label_2;
		return Bootstrap::$STRINGLIT_743;
		label_2:
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$lA3 = $fA0;
		if (((((int)(N::lcmp($lJ0, Int64::make(0, 0)))) >= 0))) goto label_3;
		$fI0 = 1;
		goto label_4;
		label_3:
		$fI0 = 0;
		label_4:
		$lI4 = $fI0;
		if ((($lI4 == 0))) goto label_5;
		$lJ0 = N::lneg($lJ0);
		label_5:
		if (((((int)(N::lcmp($lJ0, Int64::make(0, 0)))) == 0))) goto label_6;
		($lA3)->append_C_Ljava_lang_StringBuilder_(com_jtransc_internal_JTranscCType::encodeDigit_I_C(N::j2i((N::lrem((N::ladd((N::lrem($lJ0, N::i2j($p1))), N::i2j($p1))), N::i2j($p1))))));
		$lJ0 = (N::ldiv($lJ0, N::i2j($p1)));
		goto label_5;
		label_6:
		if ((($lI4 == 0))) goto label_8;
		($lA3)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_744);
		label_8:
		($lA3)->reverse__Ljava_lang_StringBuilder_();
		return ($lA3)->toString__Ljava_lang_String_();
	}
	public function doubleValue__D() {
		return (N::j2d($this->_value));
	}
	public function hashCode__I() {
		return java_lang_Long::hashCode_J_I($this->_value);
	}
	public static function hashCode_J_I(Int64 $p0) {
		return N::j2i((N::lxor($p0, (N::lushr($p0, 32)))));
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if ((($p0) instanceof java_lang_Long)) goto label_1;
		return false;
		label_1:
		if (((((int)(N::lcmp($this->_value, N::checkcast($p0, "java_lang_Long")->longValue__J()))) != 0))) goto label_2;
		$fI0 = 1;
		goto label_3;
		label_2:
		$fI0 = 0;
		label_3:
		return (($fI0)!=0);
	}
	public function longValue__J() {
		return $this->_value;
	}
	public function intValue__I() {
		return N::j2i($this->_value);
	}
	public function shortValue__S() {
		return (N::i2s(N::j2i($this->_value)));
	}
	public function byteValue__B() {
		return (N::i2b(N::j2i($this->_value)));
	}
	public function floatValue__F() {
		return (N::j2d($this->_value));
	}
	public static function reverseBytes_J_J(Int64 $p0) {
		$lJ0 = Int64::make(0, 0);
		$lJ0 = $p0;
		$lJ0 = (N::lor((N::land((N::lushr($lJ0, 8)), Int64::make(16711935, 16711935))), (N::lshl((N::land($lJ0, Int64::make(16711935, 16711935))), 8))));
		$lJ0 = (N::lor((N::land((N::lushr($lJ0, 16)), Int64::make(65535, 65535))), (N::lshl((N::land($lJ0, Int64::make(65535, 65535))), 16))));
		return (N::lor((N::lushr($lJ0, 32)), (N::lshl($lJ0, 32))));
	}
	public function __construct($CLASS_ID = 704) {
		parent::__construct($CLASS_ID);
		$this->_value = Int64::make(0, 0);
	}
	static public function SI() {
		java_lang_Long::$_TYPE = null;
		java_lang_Long::java_lang_Long_clinit___V();
	}
}
class com_jtransc_ds_FastStringMap extends java_lang_Object {
	public $data = null;
	public function com_jtransc_ds_FastStringMap_init___V() {
		$this->data = [];
	}
	public function has_Ljava_lang_String__Z(?java_lang_String $p0) {
		return isset($this->data[N::istr($p0)]);
	}
	public function set_Ljava_lang_String_Ljava_lang_Object__V(?java_lang_String $p0, ?java_lang_Object $p1) {
		$this->data[N::istr($p0)] = $p1;
	}
	public function get_Ljava_lang_String__Ljava_lang_Object_(?java_lang_String $p0) {
		return $this->data[N::istr($p0)];
	}
	public function __construct($CLASS_ID = 703) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_Double extends java_lang_Number implements java_lang_Comparable {

	public $_value = 0.0;
	public static $_TYPE = null;
	public static function valueOf_D_Ljava_lang_Double_(float $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_Double()));
		$fA0 = $tA0;
		($tA0)->java_lang_Double_init__D_V($p0);
		return ($fA0);
	}
	public function java_lang_Double_init__D_V(float $p0) {
		($this)->java_lang_Number_init___V();
		$this->_value = $p0;
		return $this;
		return $this;
	}
	public static function java_lang_Double_clinit___V() {
		java_lang_Double::$_TYPE = java_lang_Class::getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap::$STRINGLIT_745);
		return;
	}
	public function toString__Ljava_lang_String_() {
		return java_lang_Double::toString_D_Ljava_lang_String_($this->_value);
	}
	public static function toString_D_Ljava_lang_String_(float $p0) {
		return com_jtransc_text_JTranscStringTools::toString_D_Ljava_lang_String_($p0);
	}
	public static function isNaN_D_Z(float $p0) {
		return is_nan($p0);
	}
	public static function doubleToRawLongBits_D_J(float $p0) {
		return N::doubleToLongBits($p0);
	}
	public static function isInfinite_D_Z(float $p0) {
		$fI0 = 0;
		if (java_lang_Double::isNaN_D_Z($p0)) goto label_1;
		if (java_lang_Double::_isFinite_D_Z($p0)) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public static function _isFinite_D_Z(float $p0) {
		return is_finite($p0);
	}
	public function hashCode__I() {
		return java_lang_Double::hashCode_D_I($this->doubleValue__D());
	}
	public function doubleValue__D() {
		return $this->_value;
	}
	public static function hashCode_D_I(float $p0) {
		return N::j2i(java_lang_Double::doubleToLongBits_D_J($p0));
	}
	public static function doubleToLongBits_D_J(float $p0) {
		return N::doubleToLongBits($p0);
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if (!((($p0) instanceof java_lang_Double))) goto label_1;
		if (((((int)(N::lcmp(java_lang_Double::doubleToLongBits_D_J(N::checkcast($p0, "java_lang_Float")->doubleValue__D()), java_lang_Double::doubleToLongBits_D_J($this->doubleValue__D())))) != 0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function longValue__J() {
		return N::d2j($this->_value);
	}
	public function intValue__I() {
		return ((int)(((int)($this->_value))));
	}
	public function shortValue__S() {
		return (N::i2s(((int)($this->_value))));
	}
	public function byteValue__B() {
		return (N::i2b(((int)($this->_value))));
	}
	public function floatValue__F() {
		return ((double)($this->_value));
	}
	public static function longBitsToDouble_J_D(Int64 $p0) {
		return N::longBitsToDouble($p0);
	}
	public function __construct($CLASS_ID = 702) {
		parent::__construct($CLASS_ID);
		$this->_value = 0.0;
	}
	static public function SI() {
		java_lang_Double::$_TYPE = null;
		java_lang_Double::java_lang_Double_clinit___V();
	}
}
class com_jtransc_text_JTranscStringTools extends java_lang_Object {

	public function com_jtransc_text_JTranscStringTools_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function toString_F_Ljava_lang_String_(float $p0) {
		$lI2 = 0;
		$lA1 = null;
		$lA1 = com_jtransc_text_JTranscStringTools::toString_D_Ljava_lang_String_(((double)($p0)));
		$lI2 = $lA1->indexOf_I_I(46);
		if ((($lI2 < 0))) goto label_1;
		return $lA1->substring_II_Ljava_lang_String_(0, java_lang_Math::min_II_I($lA1->length__I(), ((int)(($lI2 + 6)))));
		label_1:
		return $lA1;
	}
	public static function toString_D_Ljava_lang_String_(float $p0) {
		$lA2 = null;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$fA0 = null;
		$lJ2 = Int64::make(0, 0);
		$tA0 = null;
		if (!(java_lang_Double::isNaN_D_Z($p0))) goto label_1;
		return Bootstrap::$STRINGLIT_746;
		label_1:
		if (!(java_lang_Double::isInfinite_D_Z($p0))) goto label_2;
		if (!((($p0 < 0.0)))) goto label_3;
		$fA0 = (Bootstrap::$STRINGLIT_747);
		goto label_4;
		label_3:
		$fA0 = (Bootstrap::$STRINGLIT_748);
		label_4:
		return ($fA0);
		label_2:
		if ((((N::cmpl($p0, 0.0)) != 0))) goto label_5;
		$lJ2 = java_lang_Double::doubleToRawLongBits_D_J($p0);
		if (((((int)(N::lcmp((N::lushr($lJ2, 63)), Int64::make(0, 0)))) == 0))) goto label_5;
		return Bootstrap::$STRINGLIT_749;
		label_5:
		$lA2 = (com_jtransc_text_JTranscStringTools::_toString_D_Ljava_lang_String_($p0));
		$lI3 = 0;
		$lI4 = 0;
		label_7:
		if ((($lI4 >= ($lA2)->length__I()))) goto label_8;
		$lI5 = ((int)(($lA2)->charAt_I_C($lI4)));
		if (java_lang_Character::isDigit_C_Z((N::i2c($lI5)))) goto label_10;
		if ((($lI5 == 45))) goto label_10;
		$lI3 = 1;
		goto label_8;
		label_10:
		$lI4 = ((int)(($lI4 + 1)));
		goto label_7;
		label_8:
		if (((($lA2)->indexOf_Ljava_lang_String__I(Bootstrap::$STRINGLIT_750) < 0))) goto label_12;
		$lA2 = (($lA2)->replace_Ljava_lang_CharSequence_Ljava_lang_CharSequence__Ljava_lang_String_((Bootstrap::$STRINGLIT_750), (Bootstrap::$STRINGLIT_751)));
		label_12:
		if (((($lA2)->indexOf_Ljava_lang_String__I(Bootstrap::$STRINGLIT_752) < 0))) goto label_13;
		$lA2 = (($lA2)->replace_Ljava_lang_CharSequence_Ljava_lang_CharSequence__Ljava_lang_String_((Bootstrap::$STRINGLIT_752), (Bootstrap::$STRINGLIT_753)));
		label_13:
		if ((($lI3 == 0))) goto label_14;
		$fA0 = $lA2;
		goto label_15;
		label_14:
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$fA0 = (($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(($lA2))->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_754)->toString__Ljava_lang_String_());
		label_15:
		return ($fA0);
	}
	public static function _toString_D_Ljava_lang_String_(float $p0) {
		return N::str("$p0");
	}
	public function __construct($CLASS_ID = 701) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_Float extends java_lang_Number implements java_lang_Comparable {

	public $_value = 0.0;
	public static $_TYPE = null;
	public static function valueOf_F_Ljava_lang_Float_(float $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_Float()));
		$fA0 = $tA0;
		($tA0)->java_lang_Float_init__F_V($p0);
		return ($fA0);
	}
	public function java_lang_Float_init__F_V(float $p0) {
		($this)->java_lang_Number_init___V();
		$this->_value = $p0;
		return $this;
		return $this;
	}
	public static function java_lang_Float_clinit___V() {
		java_lang_Float::$_TYPE = java_lang_Class::getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap::$STRINGLIT_755);
		return;
	}
	public function toString__Ljava_lang_String_() {
		return java_lang_Float::toString_F_Ljava_lang_String_($this->_value);
	}
	public static function toString_F_Ljava_lang_String_(float $p0) {
		return com_jtransc_text_JTranscStringTools::toString_F_Ljava_lang_String_($p0);
	}
	public function hashCode__I() {
		return java_lang_Float::hashCode_F_I($this->_value);
	}
	public static function hashCode_F_I(float $p0) {
		return java_lang_Float::floatToIntBits_F_I($p0);
	}
	public static function floatToIntBits_F_I(float $p0) {
		return N::floatToIntBits($p0);
	}
	public function doubleValue__D() {
		return ((double)($this->_value));
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if (!((($p0) instanceof java_lang_Float))) goto label_1;
		if (((java_lang_Float::floatToIntBits_F_I(N::checkcast($p0, "java_lang_Float")->_value) != java_lang_Float::floatToIntBits_F_I($this->_value)))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function longValue__J() {
		return N::f2j($this->_value);
	}
	public function intValue__I() {
		return ((int)((($this->_value)|0)));
	}
	public function shortValue__S() {
		return (N::i2s((($this->_value)|0)));
	}
	public function byteValue__B() {
		return (N::i2b((($this->_value)|0)));
	}
	public function floatValue__F() {
		return $this->_value;
	}
	public static function intBitsToFloat_I_F(int $p0) {
		return N::intBitsToFloat($p0);
	}
	public static function floatToRawIntBits_F_I(float $p0) {
		return N::floatToIntBits($p0);
	}
	public function __construct($CLASS_ID = 700) {
		parent::__construct($CLASS_ID);
		$this->_value = 0.0;
	}
	static public function SI() {
		java_lang_Float::$_TYPE = null;
		java_lang_Float::java_lang_Float_clinit___V();
	}
}
class java_lang_Void extends java_lang_Object {

	public static $_TYPE = null;
	public function java_lang_Void_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function java_lang_Void_clinit___V() {
		java_lang_Void::$_TYPE = java_lang_Class::getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap::$STRINGLIT_756);
		return;
	}
	public function __construct($CLASS_ID = 699) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		java_lang_Void::$_TYPE = null;
		java_lang_Void::java_lang_Void_clinit___V();
	}
}
class java_lang_ClassNotFoundException extends java_lang_ReflectiveOperationException {

	public $_ex = null;
	public function java_lang_ClassNotFoundException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_ReflectiveOperationException_init__Ljava_lang_String_Ljava_lang_Throwable__V($p0, null);
		return $this;
		return $this;
	}
	public function java_lang_ClassNotFoundException_init__Ljava_lang_String_Ljava_lang_Throwable__V(?java_lang_String $p0, ?java_lang_Throwable $p1) {
		($this)->java_lang_ReflectiveOperationException_init__Ljava_lang_String_Ljava_lang_Throwable__V($p0, null);
		$this->_ex = $p1;
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 697) {
		parent::__construct($CLASS_ID);
		$this->_ex = null;
	}
	static public function SI() {
	}
}
class java_lang_reflect__InternalUtils extends java_lang_Object {

	public function java_lang_reflect__InternalUtils_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(?java_lang_reflect_Type $p0) {
		if (!(((($p0)) instanceof java_lang_Class))) goto label_1;
		return N::checkcast(($p0), "java_lang_Class")->getName__Ljava_lang_String_();
		label_1:
		return ($p0)->toString__Ljava_lang_String_();
	}
	public static function parseMethodType_Ljava_lang_String_Ljava_lang_reflect_Type__Ljava_lang_reflect_MethodTypeImpl_(?java_lang_String $p0, ?java_lang_reflect_Type $p1) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new com_jtransc_text_MStringReader()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_text_MStringReader_init__Ljava_lang_String__V($p0);
		return N::checkcast(java_lang_reflect__InternalUtils::parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_(($fA0), $p1), "java_lang_reflect_MethodTypeImpl");
	}
	public static function parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_(?com_jtransc_text_MStringReader $p0, ?java_lang_reflect_Type $p1) {
		$lA6 = null;
		$lI2 = 0;
		$lI5 = 0;
		$lI7 = 0;
		$lI8 = 0;
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA2 = null;
		$tA3 = null;
		$tA0 = null;
		$tA1 = null;
		$lA10 = null;
		$fI0 = 0;
		$lA3 = null;
		$lA4 = null;
		$lA9 = null;
		$lI2 = ((int)($p0->read__C()));
		switch ($lI2) {
			case 40: goto label_2;
			case 41: case 42: case 44: case 45: case 46: case 47: case 48: case 49: case 50: case 51: case 52: case 53: case 54: case 55: case 56: case 57: case 58: case 59: case 60: case 61: case 62: case 63: case 64: case 65: case 69: case 71: case 72: case 75: case 77: case 78: case 79: case 80: case 81: case 82: case 85: case 87: case 88: case 89: goto label_1;
			case 43: goto label_3;
			case 66: goto label_4;
			case 67: goto label_5;
			case 68: goto label_6;
			case 70: goto label_7;
			case 73: goto label_8;
			case 74: goto label_9;
			case 76: case 84: goto label_10;
			case 83: goto label_11;
			case 86: goto label_12;
			case 90: goto label_13;
			case 91: goto label_14;
			default: goto label_1;
		}
		label_2:
		$lA3 = java_lang_reflect__InternalUtils::parseTypes_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type___Ljava_lang_reflect_Type_($p0, $p1);
		$p0->expect_C_V(41);
		$lA4 = java_lang_reflect__InternalUtils::parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_($p0, $p1);
		$tA2 = ((new java_lang_reflect_MethodTypeImpl()));
		$fA0 = $tA2;
		($tA2)->java_lang_reflect_MethodTypeImpl_init___Ljava_lang_reflect_Type_Ljava_lang_reflect_Type__V($lA3, $lA4);
		return ($fA0);
		label_3:
		return java_lang_reflect__InternalUtils::parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_($p0, $p1);
		label_12:
		return (java_lang_Void::$_TYPE);
		label_13:
		return (java_lang_Boolean::$_TYPE);
		label_4:
		return (java_lang_Byte::$_TYPE);
		label_5:
		return (java_lang_Character::$_TYPE);
		label_11:
		return (java_lang_Short::$_TYPE);
		label_6:
		return (java_lang_Double::$_TYPE);
		label_7:
		return (java_lang_Float::$_TYPE);
		label_8:
		return (java_lang_Integer::$_TYPE);
		label_9:
		return (java_lang_Long::$_TYPE);
		label_14:
		$lI5 = ((int)(($p0->_offset - 1)));
		$lA6 = (java_lang_reflect__InternalUtils::parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_($p0, $p1));
		$lI7 = $p0->_offset;
		if (!((($lA6) instanceof java_lang_Class))) goto label_21;
		return (java_lang_reflect__InternalUtils::Class_forName0_Ljava_lang_String__Ljava_lang_Class_($p0->_str->substring_II_Ljava_lang_String_($lI5, $lI7)));
		label_21:
		$tA3 = ((new java_lang_reflect_ArrayType()));
		$fA0 = $tA3;
		($tA3)->java_lang_reflect_ArrayType_init__Ljava_lang_reflect_Type__V(($lA6));
		return ($fA0);
		label_10:
		if ((($lI2 != 84))) goto label_15;
		$fI0 = 1;
		goto label_16;
		label_15:
		$fI0 = 0;
		label_16:
		$lI8 = $fI0;
		$lA9 = $p0->readUntil_CCZ_Ljava_lang_String_(59, 60, false)->replace_CC_Ljava_lang_String_(47, 46);
		if ((($lI8 == 0))) goto label_17;
		$fA0 = (java_lang_reflect__InternalUtils::Class_forName0_Ljava_lang_String__Ljava_lang_Class_(Bootstrap::$STRINGLIT_477));
		goto label_18;
		label_17:
		$fA0 = (java_lang_reflect__InternalUtils::Class_forName0_Ljava_lang_String__Ljava_lang_Class_($lA9));
		label_18:
		$lA10 = $fA0;
		if ((($p0->peek__C() != 60))) goto label_19;
		$lA10 = (java_lang_reflect__InternalUtils::parseTypeGeneric_Ljava_lang_reflect_Type_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_(($lA10), $p0, $p1));
		label_19:
		$p0->expect_C_V(59);
		return ($lA10);
		label_1:
		$tA0 = ((new java_lang_Error()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_Error_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_758)->append_C_Ljava_lang_StringBuilder_((N::i2c($lI2)))->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_757)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($p0->_str)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
	}
	public static function parseTypes_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type___Ljava_lang_reflect_Type_(?com_jtransc_text_MStringReader $p0, ?java_lang_reflect_Type $p1) {
		$lA2 = null;
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_util_ArrayList()));
		$fA0 = $tA0;
		($tA0)->java_util_ArrayList_init___V();
		$lA2 = $fA0;
		label_1:
		if (!($p0->hasMore__Z())) goto label_2;
		if ((($p0->peek__C() != 41))) goto label_4;
		goto label_2;
		label_4:
		($lA2)->add_Ljava_lang_Object__Z((java_lang_reflect__InternalUtils::parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_($p0, $p1)));
		goto label_1;
		label_2:
		return N::checkcast(($lA2)->toArray__Ljava_lang_Object___Ljava_lang_Object_((new JA_L(($lA2)->size__I(), "[Ljava.lang.reflect.Type;"))), "JA_L");
	}
	public static function Class_forName0_Ljava_lang_String__Ljava_lang_Class_(?java_lang_String $p0) {
		$G = 0;
		$fA0 = null;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$G = 1;
							continue 2;
						case 1:
							$fA0 = (java_lang_Class::forName_Ljava_lang_String__Ljava_lang_Class_($p0));
							$G = 2;
							continue 2;
						case 2:return ($fA0); 
						case 3:
							$fA0 = ($J__exception__);
							$tA0 = ((new java_lang_StringBuilder()));
							$fA0 = $tA0;
							($tA0)->java_lang_StringBuilder_init___V();
							com_jtransc_io_JTranscConsole::error_Ljava_lang_Object__V((($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_760)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($p0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_759)->toString__Ljava_lang_String_()));
							return null;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_ClassNotFoundException)))) {
					$G = 3;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public static function parseTypeGeneric_Ljava_lang_reflect_Type_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_(?java_lang_reflect_Type $p0, ?com_jtransc_text_MStringReader $p1, ?java_lang_reflect_Type $p2) {
		$lA3 = null;
		$fA0 = null;
		$tA0 = null;
		$tA2 = null;
		$p1->expect_C_V(60);
		$tA0 = ((new java_util_ArrayList()));
		$fA0 = $tA0;
		($tA0)->java_util_ArrayList_init___V();
		$lA3 = $fA0;
		label_1:
		if (!($p1->hasMore__Z())) goto label_2;
		if ((($p1->peek__C() == 62))) goto label_2;
		($lA3)->add_Ljava_lang_Object__Z((java_lang_reflect__InternalUtils::parseType_Lcom_jtransc_text_MStringReader_Ljava_lang_reflect_Type__Ljava_lang_reflect_Type_($p1, $p2)));
		goto label_1;
		label_2:
		$p1->expect_C_V(62);
		$tA2 = ((new java_lang_reflect_ParameterizedTypeImpl()));
		$fA0 = $tA2;
		($tA2)->java_lang_reflect_ParameterizedTypeImpl_init___Ljava_lang_reflect_Type_Ljava_lang_reflect_Type_Ljava_lang_reflect_Type__V(N::checkcast(($lA3)->toArray__Ljava_lang_Object___Ljava_lang_Object_((new JA_L(($lA3)->size__I(), "[Ljava.lang.reflect.Type;"))), "JA_L"), $p0, $p2);
		return ($fA0);
	}
	public function __construct($CLASS_ID = 696) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_reflect_Field extends java_lang_reflect_AccessibleObject implements java_lang_reflect_Member {

	public $_clazz = null;
	public $_name = null;
	public $_modifiers = 0;
	public $_signature = null;
	public $_slot = 0;
	public $_genericSignature = null;
	public function toString__Ljava_lang_String_() {
		$lI1 = 0;
		$fA0 = null;
		$fA1 = null;
		$tA0 = null;
		$tA1 = null;
		$lI1 = $this->getModifiers__I();
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		if ((($lI1 != 0))) goto label_1;
		$fA1 = (Bootstrap::$STRINGLIT_23);
		goto label_2;
		label_1:
		$tA1 = ((new java_lang_StringBuilder()));
		$fA1 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		$fA1 = (($fA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect_Modifier::toString_I_Ljava_lang_String_($lI1))->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_211)->toString__Ljava_lang_String_());
		label_2:
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(($fA1))->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils::getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(($this->getType__Ljava_lang_Class_())))->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_211)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_reflect__InternalUtils::getTypeName_Ljava_lang_reflect_Type__Ljava_lang_String_(($this->getDeclaringClass__Ljava_lang_Class_())))->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_212)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->getName__Ljava_lang_String_())->toString__Ljava_lang_String_();
	}
	public function getDeclaringClass__Ljava_lang_Class_() {
		return $this->_clazz;
	}
	public function getName__Ljava_lang_String_() {
		return $this->_name;
	}
	public function getModifiers__I() {
		return $this->_modifiers;
	}
	public function getType__Ljava_lang_Class_() {
		$G = 0;
		$lA1 = null;
		$fA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$G = 1;
							continue 2;
						case 1:
							$fA0 = (java_lang_Class::forName_Ljava_lang_String__Ljava_lang_Class_($this->_signature));
							$G = 2;
							continue 2;
						case 2:return ($fA0); 
						case 3:
							$fA0 = ($J__exception__);
							$lA1 = $fA0;
							($lA1)->printStackTrace__V();
							return null;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_ClassNotFoundException)))) {
					$G = 3;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public function set_Ljava_lang_Object_Ljava_lang_Object__V(?java_lang_Object $p0, ?java_lang_Object $p1) {
		$lA3 = null;
		$lA3 = ($this->getType__Ljava_lang_Class_());
		if ((($lA3 != null))) goto label_1;
		goto label_2;
		label_1:
		$this->_setObject_Ljava_lang_Object_Ljava_lang_Object__V($p0, $p1);
		label_2:
		return;
	}
	public function _setObject_Ljava_lang_Object_Ljava_lang_Object__V(?java_lang_Object $p0, ?java_lang_Object $p1) {
		j_ProgramReflection::dynamicSet_IILjava_lang_Object_Ljava_lang_Object__V($this->_clazz->_id, $this->_slot, $p0, $p1);
		return;
	}
	public function get_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0) {
		$lA2 = null;
		$lA2 = ($this->getType__Ljava_lang_Class_());
		if ((($lA2 != null))) goto label_1;
		return null;
		label_1:
		return $this->_getObject_Ljava_lang_Object__Ljava_lang_Object_($p0);
	}
	public function _getObject_Ljava_lang_Object__Ljava_lang_Object_(?java_lang_Object $p0) {
		return j_ProgramReflection::dynamicGet_IILjava_lang_Object__Ljava_lang_Object_($this->_clazz->_id, $this->_slot, $p0);
	}
	public function java_lang_reflect_Field_init__Ljava_lang_Class_Lj_MemberInfo__V(?java_lang_Class $p0, ?j_MemberInfo $p1) {
		($this)->java_lang_reflect_AccessibleObject_init__Lj_MemberInfo__V($p1);
		$this->_clazz = $p0;
		$this->_slot = $p1->_id;
		$this->_name = $p1->_name;
		$this->_signature = $p1->_desc;
		$this->_genericSignature = $p1->_genericDesc;
		$this->_modifiers = $p1->_modifiers;
		return $this;
		return $this;
	}
	public function hashCode__I() {
		return ((int)(($this->getDeclaringClass__Ljava_lang_Class_()->getName__Ljava_lang_String_()->hashCode__I() ^ $this->getName__Ljava_lang_String_()->hashCode__I())));
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if (((($this) != $p0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function __construct($CLASS_ID = 693) {
		parent::__construct($CLASS_ID);
		$this->_clazz = null;
		$this->_name = null;
		$this->_modifiers = 0;
		$this->_signature = null;
		$this->_slot = 0;
		$this->_genericSignature = null;
	}
	static public function SI() {
	}
}
class java_lang_Character extends java_lang_Object implements java_io_Serializable, java_lang_Comparable {

	public $_value = 0;
	public static $_TYPE = null;
	public function java_lang_Character_init__C_V(int $p0) {
		($this)->java_lang_Object_init___V();
		$this->_value = $p0;
		return $this;
		return $this;
	}
	public static function valueOf_C_Ljava_lang_Character_(int $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_Character()));
		$fA0 = $tA0;
		($tA0)->java_lang_Character_init__C_V($p0);
		return ($fA0);
	}
	public static function java_lang_Character_clinit___V() {
		java_lang_Character::$_TYPE = java_lang_Class::getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap::$STRINGLIT_761);
		return;
	}
	public static function isDigit_C_Z(int $p0) {
		$fI0 = 0;
		if (((((int)($p0)) < 48))) goto label_1;
		if (((((int)($p0)) > 57))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public static function forDigit_II_C(int $p0, int $p1) {
		if ((($p0 < 0))) goto label_1;
		if ((($p0 > 9))) goto label_1;
		return (N::i2c(((int)((48 + ((int)(($p0 - 0))))))));
		label_1:
		if ((($p0 < 10))) goto label_2;
		if ((($p0 > 35))) goto label_2;
		return (N::i2c(((int)((97 + ((int)(($p0 - 10))))))));
		label_2:
		return 0;
	}
	public function hashCode__I() {
		return ((int)($this->_value));
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if (!((($p0) instanceof java_lang_Character))) goto label_1;
		if ((($this->_value != N::checkcast($p0, "java_lang_Character")->_value))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function toString__Ljava_lang_String_() {
		return java_lang_Character::toString_C_Ljava_lang_String_($this->_value);
	}
	public static function toString_C_Ljava_lang_String_(int $p0) {
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		$tA0 = ((new java_lang_String()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = (new JA_C(1));
		$fA2 = $tA1;
		($tA1)->set(0, $p0);
		($fA1)->java_lang_String_init___C_V(($fA2));
		return ($fA0);
	}
	public function charValue__C() {
		return $this->_value;
	}
	public static function isSpaceChar_C_Z(int $p0) {
		return java_lang_Character::isSpaceChar_I_Z(((int)($p0)));
	}
	public static function isSpaceChar_I_Z(int $p0) {
		switch ($p0) {
			case 32: case 160: case 5760: case 6158: case 8192: case 8193: case 8194: case 8195: case 8196: case 8197: case 8198: case 8199: case 8200: case 8201: case 8202: case 8203: case 8239: case 8287: case 12288: case 65279: goto label_2;
			default: goto label_1;
		}
		label_2:
		return true;
		label_1:
		return false;
	}
	public static function toUpperCase_C_C(int $p0) {
		return (N::i2c(java_lang_Character::toUpperCase_I_I(((int)($p0)))));
	}
	public static function toUpperCase_I_I(int $p0) {
		if ((($p0 < 97))) goto label_1;
		if ((($p0 >= 122))) goto label_1;
		return ((int)((((int)(($p0 - 97))) + 65)));
		label_1:
		return $p0;
	}
	public static function reverseBytes_C_C(int $p0) {
		return (N::i2c(((int)((((int)(N::ishr(((int)((((int)($p0)) & 65280))), 8))) | ((int)(N::ishl(((int)($p0)), 8))))))));
	}
	public static function toLowerCase_C_C(int $p0) {
		return (N::i2c(java_lang_Character::toLowerCase_I_I(((int)($p0)))));
	}
	public static function toLowerCase_I_I(int $p0) {
		if ((($p0 < 65))) goto label_1;
		if ((($p0 >= 90))) goto label_1;
		return ((int)((((int)(($p0 - 65))) + 97)));
		label_1:
		return $p0;
	}
	public function __construct($CLASS_ID = 692) {
		parent::__construct($CLASS_ID);
		$this->_value = 0;
	}
	static public function SI() {
		java_lang_Character::$_TYPE = null;
		java_lang_Character::java_lang_Character_clinit___V();
	}
}
class java_lang_IntegerTools extends java_lang_Object {

	public function java_lang_IntegerTools_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function countDigits_II_I(int $p0, int $p1) {
		if ((($p1 != 10))) goto label_1;
		return java_lang_IntegerTools::countDigits10_I_I($p0);
		label_1:
		if ((($p1 != 16))) goto label_2;
		return java_lang_IntegerTools::countDigits16_I_I($p0);
		label_2:
		if ((($p1 != 2))) goto label_3;
		return java_lang_IntegerTools::countDigits2_I_I($p0);
		label_3:
		return java_lang_IntegerTools::countDigitsGeneric_II_I($p0, $p1);
	}
	public static function countDigits16_I_I(int $p0) {
		$fI0 = 0;
		if ((($p0 >= 0))) goto label_1;
		if ((($p0 != N::MIN_INT32))) goto label_2;
		$fI0 = 9;
		goto label_3;
		label_2:
		$fI0 = ((int)((1 + java_lang_IntegerTools::countDigits16_I_I(((int)(-($p0)))))));
		label_3:
		return $fI0;
		label_1:
		if ((($p0 >= 16))) goto label_4;
		return 1;
		label_4:
		if ((($p0 >= 256))) goto label_5;
		return 2;
		label_5:
		if ((($p0 >= 4096))) goto label_6;
		return 3;
		label_6:
		if ((($p0 >= 65536))) goto label_7;
		return 4;
		label_7:
		if ((($p0 >= 1048576))) goto label_8;
		return 5;
		label_8:
		if ((($p0 >= 16777216))) goto label_9;
		return 6;
		label_9:
		if ((($p0 >= 268435456))) goto label_10;
		return 7;
		label_10:
		return 8;
	}
	public static function countDigitsGeneric_II_I(int $p0, int $p1) {
		$fI0 = 0;
		$lI0 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$lI0 = $p0;
		if ((($lI0 >= 0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		$lI2 = $fI0;
		$lI3 = 0;
		if ((($lI2 == 0))) goto label_3;
		$lI0 = ((int)(-($lI0)));
		$lI3 = ((int)(($lI3 + 1)));
		label_3:
		if ((($lI0 != 0))) goto label_5;
		$lI3 = ((int)(($lI3 + 1)));
		goto label_7;
		label_5:
		if ((($lI0 == 0))) goto label_7;
		$lI3 = ((int)(($lI3 + 1)));
		$lI0 = java_lang_Integer::divideUnsigned_II_I($lI0, $p1);
		goto label_5;
		label_7:
		return $lI3;
	}
	public static function countDigits10_I_I(int $p0) {
		$fI0 = 0;
		if ((($p0 >= 0))) goto label_1;
		if ((($p0 != N::MIN_INT32))) goto label_2;
		$fI0 = 11;
		goto label_3;
		label_2:
		$fI0 = ((int)((1 + java_lang_IntegerTools::countDigits10_I_I(((int)(-($p0)))))));
		label_3:
		return $fI0;
		label_1:
		if ((($p0 >= 10))) goto label_4;
		return 1;
		label_4:
		if ((($p0 >= 100))) goto label_5;
		return 2;
		label_5:
		if ((($p0 >= 1000))) goto label_6;
		return 3;
		label_6:
		if ((($p0 >= 10000))) goto label_7;
		return 4;
		label_7:
		if ((($p0 >= 100000))) goto label_8;
		return 5;
		label_8:
		if ((($p0 >= 1000000))) goto label_9;
		return 6;
		label_9:
		if ((($p0 >= 10000000))) goto label_10;
		return 7;
		label_10:
		if ((($p0 >= 100000000))) goto label_11;
		return 8;
		label_11:
		if ((($p0 >= 1000000000))) goto label_12;
		return 9;
		label_12:
		return 10;
	}
	public static function countDigits2_I_I(int $p0) {
		$fI0 = 0;
		if ((($p0 >= 0))) goto label_1;
		if ((($p0 != N::MIN_INT32))) goto label_2;
		$fI0 = 33;
		goto label_3;
		label_2:
		$fI0 = ((int)((1 + java_lang_IntegerTools::countDigits2_I_I(((int)(-($p0)))))));
		label_3:
		return $fI0;
		label_1:
		return ((int)((32 - java_lang_Integer::numberOfLeadingZeros_I_I($p0))));
	}
	public static function writeInt__CIII_I(?JA_C $p0, int $p1, int $p2, int $p3) {
		$fI0 = 0;
		$lI2 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$lI6 = 0;
		$lI7 = 0;
		$fA0 = null;
		$tA0 = null;
		$lI2 = $p2;
		if ((($p3 >= 2))) goto label_1;
		$tA0 = ((new java_lang_RuntimeException()));
		$fA0 = $tA0;
		($tA0)->java_lang_RuntimeException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_762);
		throw new WrappedThrowable($fA0);
		label_1:
		if ((($lI2 != N::MIN_INT32))) goto label_2;
		$lI4 = java_lang_IntegerTools::countDigits_II_I($lI2, $p3);
		$lI5 = ((int)(($p1 + $lI4)));
		label_4:
		if ((($lI2 == 0))) goto label_5;
		$fA0 = ($p0);
		$lI5 = ((int)(($lI5 + -1)));
		($fA0)->set($lI5, java_lang_Character::forDigit_II_C(java_lang_Integer::remainderUnsigned_II_I($lI2, $p3), $p3));
		$lI2 = java_lang_Integer::divideUnsigned_II_I($lI2, $p3);
		goto label_4;
		label_5:
		$fA0 = ($p0);
		$lI5 = ((int)(($lI5 + -1)));
		($fA0)->set($lI5, 45);
		return $lI4;
		label_2:
		if ((($lI2 >= 0))) goto label_7;
		$fI0 = 1;
		goto label_8;
		label_7:
		$fI0 = 0;
		label_8:
		$lI4 = $fI0;
		$lI5 = java_lang_IntegerTools::countDigits_II_I($lI2, $p3);
		if ((($lI4 == 0))) goto label_9;
		$lI2 = ((int)(-($lI2)));
		label_9:
		$lI6 = ((int)(($p1 + $lI5)));
		if ((($lI2 != 0))) goto label_10;
		$fA0 = ($p0);
		$lI6 = ((int)(($lI6 + -1)));
		($fA0)->set($lI6, 48);
		goto label_12;
		label_10:
		if ((($p3 != 10))) goto label_15;
		label_16:
		if ((($lI2 == 0))) goto label_12;
		$lI7 = ((int)(N::irem($lI2, 10)));
		$lI2 = ((int)(N::idiv($lI2, 10)));
		$fA0 = ($p0);
		$lI6 = ((int)(($lI6 + -1)));
		($fA0)->set($lI6, java_lang_Character::forDigit_II_C($lI7, $p3));
		goto label_16;
		label_15:
		if ((($lI2 == 0))) goto label_12;
		$lI7 = ((int)(N::irem($lI2, $p3)));
		$lI2 = ((int)(N::idiv($lI2, $p3)));
		$fA0 = ($p0);
		$lI6 = ((int)(($lI6 + -1)));
		($fA0)->set($lI6, java_lang_Character::forDigit_II_C($lI7, $p3));
		goto label_15;
		label_12:
		if ((($lI4 == 0))) goto label_13;
		$fA0 = ($p0);
		$lI6 = ((int)(($lI6 + -1)));
		($fA0)->set($lI6, 45);
		label_13:
		return $lI5;
	}
	public function __construct($CLASS_ID = 691) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_Integer extends java_lang_Number implements java_lang_Comparable {

	public $_value = 0;
	public static $_TYPE = null;
	public static $_values = null;
	public static $_NTZ_TABLE = null;
	public function toString__Ljava_lang_String_() {
		return java_lang_Integer::toString_I_Ljava_lang_String_($this->_value);
	}
	public static function toString_I_Ljava_lang_String_(int $p0) {
		return java_lang_Integer::toString_II_Ljava_lang_String_($p0, 10);
	}
	public static function toString_II_Ljava_lang_String_(int $p0, int $p1) {
		$lI3 = 0;
		$fA0 = null;
		$lA2 = null;
		$tA0 = null;
		if ((($p0 != 0))) goto label_1;
		return Bootstrap::$STRINGLIT_742;
		label_1:
		$lA2 = new JA_C(java_lang_IntegerTools::countDigits_II_I($p0, $p1));
		$lI3 = java_lang_IntegerTools::writeInt__CIII_I($lA2, 0, $p0, $p1);
		$tA0 = ((new java_lang_String()));
		$fA0 = $tA0;
		($tA0)->java_lang_String_init___CII_V($lA2, 0, $lI3);
		return ($fA0);
	}
	public static function divideUnsigned_II_I(int $p0, int $p1) {
		$fI0 = 0;
		$fI1 = 0;
		$lI2 = 0;
		$lI3 = 0;
		if ((($p1 >= 0))) goto label_1;
		if (((java_lang_Integer::compareUnsigned_II_I($p0, $p1) >= 0))) goto label_2;
		$fI0 = 0;
		goto label_3;
		label_2:
		$fI0 = 1;
		label_3:
		return $fI0;
		label_1:
		if ((($p0 < 0))) goto label_4;
		return ((int)(N::idiv($p0, $p1)));
		label_4:
		$lI2 = ((int)(N::ishl(((int)(N::idiv(((int)(N::iushr($p0, 1))), $p1))), 1)));
		$lI3 = ((int)(($p0 - ((int)(N::imul($lI2, $p1))))));
		$fI0 = $lI2;
		if (((java_lang_Integer::compareUnsigned_II_I($lI3, $p1) < 0))) goto label_5;
		$fI1 = 1;
		goto label_6;
		label_5:
		$fI1 = 0;
		label_6:
		return ((int)(($fI0 + $fI1)));
	}
	public static function compareUnsigned_II_I(int $p0, int $p1) {
		return java_lang_Integer::compare_II_I(((int)(($p0 ^ N::MIN_INT32))), ((int)(($p1 ^ N::MIN_INT32))));
	}
	public static function compare_II_I(int $p0, int $p1) {
		$fI0 = 0;
		if ((($p0 >= $p1))) goto label_1;
		$fI0 = -1;
		goto label_2;
		label_1:
		if ((($p0 <= $p1))) goto label_3;
		$fI0 = 1;
		goto label_2;
		label_3:
		$fI0 = 0;
		label_2:
		return $fI0;
	}
	public static function numberOfLeadingZeros_I_I(int $p0) {
		$lI0 = 0;
		$lI1 = 0;
		$lI0 = $p0;
		if ((($lI0 > 0))) goto label_1;
		return ((int)((((int)(N::ishr(((int)(($lI0 ^ -1))), 26))) & 32)));
		label_1:
		$lI1 = 1;
		if (((((int)(N::ishr($lI0, 16))) != 0))) goto label_3;
		$lI1 = ((int)(($lI1 + 16)));
		$lI0 = ((int)(N::ishl($lI0, 16)));
		label_3:
		if (((((int)(N::ishr($lI0, 24))) != 0))) goto label_5;
		$lI1 = ((int)(($lI1 + 8)));
		$lI0 = ((int)(N::ishl($lI0, 8)));
		label_5:
		if (((((int)(N::ishr($lI0, 28))) != 0))) goto label_7;
		$lI1 = ((int)(($lI1 + 4)));
		$lI0 = ((int)(N::ishl($lI0, 4)));
		label_7:
		if (((((int)(N::ishr($lI0, 30))) != 0))) goto label_9;
		$lI1 = ((int)(($lI1 + 2)));
		$lI0 = ((int)(N::ishl($lI0, 2)));
		label_9:
		return ((int)(($lI1 - ((int)(N::iushr($lI0, 31))))));
	}
	public function hashCode__I() {
		return $this->_value;
	}
	public function doubleValue__D() {
		return ((double)($this->_value));
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if (!((($p0) instanceof java_lang_Integer))) goto label_1;
		if (((N::checkcast($p0, "java_lang_Integer")->_value != $this->_value))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function longValue__J() {
		return N::i2j($this->_value);
	}
	public function intValue__I() {
		return $this->_value;
	}
	public function shortValue__S() {
		return (N::i2s($this->_value));
	}
	public function byteValue__B() {
		return (N::i2b($this->_value));
	}
	public static function remainderUnsigned_II_I(int $p0, int $p1) {
		$fI0 = 0;
		$fI1 = 0;
		$lI2 = 0;
		$lI3 = 0;
		if ((($p1 >= 0))) goto label_1;
		if (((java_lang_Integer::compareUnsigned_II_I($p0, $p1) >= 0))) goto label_2;
		$fI0 = $p0;
		goto label_3;
		label_2:
		$fI0 = ((int)(($p0 - $p1)));
		label_3:
		return $fI0;
		label_1:
		if ((($p0 < 0))) goto label_4;
		return ((int)(N::irem($p0, $p1)));
		label_4:
		$lI2 = ((int)(N::ishl(((int)(N::idiv(((int)(N::iushr($p0, 1))), $p1))), 1)));
		$lI3 = ((int)(($p0 - ((int)(N::imul($lI2, $p1))))));
		$fI0 = $lI3;
		if (((java_lang_Integer::compareUnsigned_II_I($lI3, $p1) < 0))) goto label_5;
		$fI1 = $p1;
		goto label_6;
		label_5:
		$fI1 = 0;
		label_6:
		return ((int)(($fI0 - $fI1)));
	}
	public static function valueOf_I_Ljava_lang_Integer_(int $p0) {
		$fI1 = 0;
		$lI1 = 0;
		$fA0 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		if (((java_lang_Integer::$_values != null))) goto label_1;
		java_lang_Integer::$_values = new JA_L(256, "[Ljava.lang.Integer;");
		$lI1 = -128;
		label_3:
		if ((($lI1 >= 128))) goto label_1;
		$fA0 = (java_lang_Integer::$_values);
		$fI1 = ((int)(($lI1 - -128)));
		$tA0 = ((new java_lang_Integer()));
		$fA2 = $tA0;
		($tA0)->java_lang_Integer_init__I_V($lI1);
		($fA0)->set($fI1, $fA2);
		$lI1 = ((int)(($lI1 + 1)));
		goto label_3;
		label_1:
		if ((($p0 < -128))) goto label_5;
		if ((($p0 >= 128))) goto label_5;
		return (((java_lang_Integer::$_values)->get(((int)(($p0 - -128))))));
		label_5:
		$tA1 = ((new java_lang_Integer()));
		$fA0 = $tA1;
		($tA1)->java_lang_Integer_init__I_V($p0);
		return ($fA0);
	}
	public function java_lang_Integer_init__I_V(int $p0) {
		($this)->java_lang_Number_init___V();
		$this->_value = $p0;
		return $this;
		return $this;
	}
	public static function java_lang_Integer_clinit___V() {
		$fA0 = null;
		$tA0 = null;
		java_lang_Integer::$_TYPE = java_lang_Class::getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(Bootstrap::$STRINGLIT_763);
		$tA0 = (new JA_B(64));
		$fA0 = $tA0;
		($tA0)->set(0, 32);
		($fA0)->set(1, 0);
		($fA0)->set(2, 1);
		($fA0)->set(3, 12);
		($fA0)->set(4, 2);
		($fA0)->set(5, 6);
		($fA0)->set(6, -1);
		($fA0)->set(7, 13);
		($fA0)->set(8, 3);
		($fA0)->set(9, -1);
		($fA0)->set(10, 7);
		($fA0)->set(11, -1);
		($fA0)->set(12, -1);
		($fA0)->set(13, -1);
		($fA0)->set(14, -1);
		($fA0)->set(15, 14);
		($fA0)->set(16, 10);
		($fA0)->set(17, 4);
		($fA0)->set(18, -1);
		($fA0)->set(19, -1);
		($fA0)->set(20, 8);
		($fA0)->set(21, -1);
		($fA0)->set(22, -1);
		($fA0)->set(23, 25);
		($fA0)->set(24, -1);
		($fA0)->set(25, -1);
		($fA0)->set(26, -1);
		($fA0)->set(27, -1);
		($fA0)->set(28, -1);
		($fA0)->set(29, 21);
		($fA0)->set(30, 27);
		($fA0)->set(31, 15);
		($fA0)->set(32, 31);
		($fA0)->set(33, 11);
		($fA0)->set(34, 5);
		($fA0)->set(35, -1);
		($fA0)->set(36, -1);
		($fA0)->set(37, -1);
		($fA0)->set(38, -1);
		($fA0)->set(39, -1);
		($fA0)->set(40, 9);
		($fA0)->set(41, -1);
		($fA0)->set(42, -1);
		($fA0)->set(43, 24);
		($fA0)->set(44, -1);
		($fA0)->set(45, -1);
		($fA0)->set(46, 20);
		($fA0)->set(47, 26);
		($fA0)->set(48, 30);
		($fA0)->set(49, -1);
		($fA0)->set(50, -1);
		($fA0)->set(51, -1);
		($fA0)->set(52, -1);
		($fA0)->set(53, 23);
		($fA0)->set(54, -1);
		($fA0)->set(55, 19);
		($fA0)->set(56, 29);
		($fA0)->set(57, -1);
		($fA0)->set(58, 22);
		($fA0)->set(59, 18);
		($fA0)->set(60, 28);
		($fA0)->set(61, 17);
		($fA0)->set(62, 16);
		($fA0)->set(63, -1);
		java_lang_Integer::$_NTZ_TABLE = ($fA0);
		return;
	}
	public static function toHexString_I_Ljava_lang_String_(int $p0) {
		return java_lang_Integer::toUnsignedString_II_Ljava_lang_String_($p0, 16);
	}
	public static function toUnsignedString_II_Ljava_lang_String_(int $p0, int $p1) {
		$lA2 = null;
		$lI0 = 0;
		$fA0 = null;
		$tA0 = null;
		$lI0 = $p0;
		if ((($lI0 != 0))) goto label_1;
		return Bootstrap::$STRINGLIT_742;
		label_1:
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$lA2 = $fA0;
		label_2:
		if ((($lI0 == 0))) goto label_3;
		($lA2)->append_C_Ljava_lang_StringBuilder_(com_jtransc_internal_JTranscCType::encodeDigit_I_C(java_lang_Integer::remainderUnsigned_II_I($lI0, $p1)));
		$lI0 = java_lang_Integer::divideUnsigned_II_I($lI0, $p1);
		goto label_2;
		label_3:
		($lA2)->reverse__Ljava_lang_StringBuilder_();
		return ($lA2)->toString__Ljava_lang_String_();
	}
	public function floatValue__F() {
		return ((double)($this->_value));
	}
	public static function max_II_I(int $p0, int $p1) {
		return java_lang_Math::max_II_I($p0, $p1);
	}
	public static function min_II_I(int $p0, int $p1) {
		return java_lang_Math::min_II_I($p0, $p1);
	}
	public static function reverseBytes_I_I(int $p0) {
		return ((int)((((int)((((int)((((int)(N::iushr($p0, 24))) | ((int)((((int)(N::ishr($p0, 8))) & 65280)))))) | ((int)((((int)(N::ishl($p0, 8))) & 16711680)))))) | ((int)(N::ishl($p0, 24))))));
	}
	public function __construct($CLASS_ID = 689) {
		parent::__construct($CLASS_ID);
		$this->_value = 0;
	}
	static public function SI() {
		java_lang_Integer::$_TYPE = null;
		java_lang_Integer::$_values = null;
		java_lang_Integer::$_NTZ_TABLE = null;
		java_lang_Integer::java_lang_Integer_clinit___V();
	}
}
class java_lang_reflect_Modifier extends java_lang_Object {

	public function java_lang_reflect_Modifier_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function isInterface_I_Z(int $p0) {
		$fI0 = 0;
		if (((((int)(($p0 & 512))) == 0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public static function toString_I_Ljava_lang_String_(int $p0) {
		$lA1 = null;
		$fI0 = 0;
		$lI2 = 0;
		$fA0 = null;
		$tI13 = 0;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$lA1 = $fA0;
		if (((((int)(($p0 & 1))) == 0))) goto label_1;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_764);
		label_1:
		if (((((int)(($p0 & 4))) == 0))) goto label_2;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_765);
		label_2:
		if (((((int)(($p0 & 2))) == 0))) goto label_3;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_766);
		label_3:
		if (((((int)(($p0 & 1024))) == 0))) goto label_4;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_767);
		label_4:
		if (((((int)(($p0 & 8))) == 0))) goto label_5;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_768);
		label_5:
		if (((((int)(($p0 & 16))) == 0))) goto label_6;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_769);
		label_6:
		if (((((int)(($p0 & 128))) == 0))) goto label_7;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_770);
		label_7:
		if (((((int)(($p0 & 64))) == 0))) goto label_8;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_771);
		label_8:
		if (((((int)(($p0 & 32))) == 0))) goto label_9;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_772);
		label_9:
		if (((((int)(($p0 & 256))) == 0))) goto label_10;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_773);
		label_10:
		if (((((int)(($p0 & 2048))) == 0))) goto label_11;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_774);
		label_11:
		if (((((int)(($p0 & 512))) == 0))) goto label_12;
		($lA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_775);
		label_12:
		$tI13 = ($lA1)->length__I();
		$fI0 = $tI13;
		$lI2 = $tI13;
		if ((($fI0 <= 0))) goto label_13;
		return ($lA1)->toString__Ljava_lang_String_()->substring_II_Ljava_lang_String_(0, ((int)(($lI2 - 1))));
		label_13:
		return Bootstrap::$STRINGLIT_23;
	}
	public function __construct($CLASS_ID = 688) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_lang_AnnotatedElement {

}
class java_lang_AnnotatedElement_IFields {

	static public function SI() {
	}
}
class java_lang_Class extends java_lang_Object implements java_io_Serializable, java_lang_reflect_Type, java_lang_reflect_GenericDeclaration, java_lang_AnnotatedElement {

	public $_name = null;
	public $_primitive = false;
	public $_modifiers = 0;
	public static $__classCache = null;
	public $__accessibleMethods = null;
	public $_enumConstants = null;
	public $__allFields = null;
	public $__accessibleFields = null;
	public $__allMethods = null;
	public $_id = 0;
	public $_related = null;
	public $_info = null;
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$fA1 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		if (!($this->isInterface__Z())) goto label_1;
		$fA1 = Bootstrap::$STRINGLIT_775;
		goto label_2;
		label_1:
		if (!($this->isPrimitive__Z())) goto label_3;
		$fA1 = Bootstrap::$STRINGLIT_23;
		goto label_2;
		label_3:
		$fA1 = Bootstrap::$STRINGLIT_776;
		label_2:
		return ($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($fA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->_name)->toString__Ljava_lang_String_();
	}
	public function isPrimitive__Z() {
		return $this->_primitive;
	}
	public function isInterface__Z() {
		return java_lang_reflect_Modifier::isInterface_I_Z($this->getModifiers__I());
	}
	public function getModifiers__I() {
		return ((int)(($this->_modifiers & -33)));
	}
	public static function getPrimitiveClass_Ljava_lang_String__Ljava_lang_Class_(?java_lang_String $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_Class()));
		$fA0 = $tA0;
		($tA0)->java_lang_Class_init__Ljava_lang_String_Z_V($p0, true);
		return ($fA0);
	}
	public function java_lang_Class_init__Ljava_lang_String_Z_V(?java_lang_String $p0, bool $p1) {
		($this)->java_lang_Object_init___V();
		$this->_primitive = false;
		$this->_enumConstants = null;
		$this->__allFields = null;
		$this->__accessibleFields = null;
		$this->__allMethods = null;
		$this->__accessibleMethods = null;
		$this->_name = $p0;
		$this->_primitive = $p1;
		$this->_id = -1;
		return $this;
		return $this;
	}
	public function getName__Ljava_lang_String_() {
		return $this->_name;
	}
	public static function forName_Ljava_lang_String__Ljava_lang_Class_(?java_lang_String $p0) {
		$lA1 = null;
		$fA0 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		if (((($p0) != null))) goto label_1;
		return null;
		label_1:
		if ((($p0->length__I() != 1))) goto label_2;
		switch ($p0->charAt_I_C(0)) {
			case 66: goto label_4;
			case 67: goto label_5;
			case 68: goto label_6;
			case 69: case 71: case 72: case 75: case 76: case 77: case 78: case 79: case 80: case 81: case 82: case 84: case 85: case 87: case 88: case 89: goto label_2;
			case 70: goto label_7;
			case 73: goto label_8;
			case 74: goto label_9;
			case 83: goto label_10;
			case 86: goto label_11;
			case 90: goto label_12;
			default: goto label_2;
		}
		label_11:
		return java_lang_Void::$_TYPE;
		label_12:
		return java_lang_Boolean::$_TYPE;
		label_4:
		return java_lang_Byte::$_TYPE;
		label_5:
		return java_lang_Character::$_TYPE;
		label_10:
		return java_lang_Short::$_TYPE;
		label_6:
		return java_lang_Double::$_TYPE;
		label_7:
		return java_lang_Float::$_TYPE;
		label_8:
		return java_lang_Integer::$_TYPE;
		label_9:
		return java_lang_Long::$_TYPE;
		label_2:
		if (!($p0->startsWith_Ljava_lang_String__Z(Bootstrap::$STRINGLIT_777))) goto label_13;
		if (!($p0->endsWith_Ljava_lang_String__Z(Bootstrap::$STRINGLIT_778))) goto label_13;
		return java_lang_Class::forName_Ljava_lang_String__Ljava_lang_Class_($p0->substring_II_Ljava_lang_String_(1, ((int)(($p0->length__I() - 1))))->replace_CC_Ljava_lang_String_(47, 46));
		label_13:
		if (((java_lang_Class::$__classCache != null))) goto label_15;
		$tA0 = ((new com_jtransc_ds_FastStringMap()));
		$fA0 = $tA0;
		($tA0)->com_jtransc_ds_FastStringMap_init___V();
		java_lang_Class::$__classCache = ($fA0);
		label_15:
		if (java_lang_Class::$__classCache->has_Ljava_lang_String__Z($p0)) goto label_16;
		$fA0 = (java_lang_Class::$__classCache);
		$tA1 = ((new java_lang_Class()));
		$fA2 = $tA1;
		($tA1)->java_lang_Class_init__Ljava_lang_String__V($p0);
		($fA0)->set_Ljava_lang_String_Ljava_lang_Object__V($p0, $fA2);
		label_16:
		$lA1 = (N::checkcast(java_lang_Class::$__classCache->get_Ljava_lang_String__Ljava_lang_Object_($p0), "java_lang_Class"));
		if ((($lA1 != null))) goto label_18;
		$tA2 = ((new java_lang_StringBuilder()));
		$fA0 = $tA2;
		($tA2)->java_lang_StringBuilder_init___V();
		com_jtransc_io_JTranscConsole::error_Ljava_lang_Object__V((($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_779)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($p0)->toString__Ljava_lang_String_()));
		label_18:
		return ($lA1);
	}
	public function java_lang_Class_init__Ljava_lang_String__V(?java_lang_String $p0) {
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		($this)->java_lang_Object_init___V();
		$this->_primitive = false;
		$this->_enumConstants = null;
		$this->__allFields = null;
		$this->__accessibleFields = null;
		$this->__allMethods = null;
		$this->__accessibleMethods = null;
		$this->_name = $p0;
		$this->_primitive = false;
		if ($this->_check__Z()) goto label_1;
		$tA0 = ((new java_lang_ClassNotFoundException()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_ClassNotFoundException_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_780)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($p0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_759)->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
		label_1:
		return $this;
		return $this;
	}
	public function getComponentType__Ljava_lang_Class_() {
		$G = 0;
		$lA1 = null;
		$fA0 = null;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							if (!($this->isArray__Z())) {
								$G = 1;
								continue 2;
							}
							$G = 2;
							continue 2;
						case 2:
							$lA1 = (java_lang_Class::forName_Ljava_lang_String__Ljava_lang_Class_($this->getName__Ljava_lang_String_()->substring_I_Ljava_lang_String_(1)));
							$fA0 = $lA1;
							$G = 3;
							continue 2;
						case 3:return ($fA0); 
						case 4:
							$fA0 = ($J__exception__);
							$lA1 = $fA0;
							$tA0 = ((new java_lang_RuntimeException()));
							$fA0 = $tA0;
							($tA0)->java_lang_RuntimeException_init__Ljava_lang_Throwable__V(($lA1));
							throw new WrappedThrowable($fA0);
							$G = 1;
							continue 2;
						case 1:
							return null;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 2)) && (($G < 3)))) && (($J__exception__) instanceof java_lang_ClassNotFoundException)))) {
					$G = 4;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public function isArray__Z() {
		return $this->_name->startsWith_Ljava_lang_String__Z(Bootstrap::$STRINGLIT_728);
	}
	public function _check__Z() {
		$fI0 = 0;
		$this->_info = java_lang_jtransc_JTranscCoreReflection::getClassInfoWithName_Ljava_lang_String__Lj_ClassInfo_($this->_name);
		if ((($this->_info == null))) goto label_1;
		$this->_id = $this->_info->_id;
		$this->_related = $this->_info->_related;
		$this->_modifiers = java_lang_jtransc_JTranscCoreReflection::getModifiersWithId_I_I($this->_id);
		goto label_3;
		label_1:
		$this->_id = -1;
		$this->_related = new JA_I(0);
		$this->_modifiers = 0;
		label_3:
		if ($this->isArray__Z()) goto label_4;
		if ((($this->_id < 0))) goto label_5;
		label_4:
		$fI0 = 1;
		goto label_6;
		label_5:
		$fI0 = 0;
		label_6:
		return (($fI0)!=0);
	}
	public function getDeclaredFields___Ljava_lang_reflect_Field_() {
		return java_lang_jtransc_JTranscCoreReflection::getDeclaredFields_Ljava_lang_Class___Ljava_lang_reflect_Field_($this);
	}
	public function newInstance__Ljava_lang_Object_() {
		$G = 0;
		$lA1 = null;
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$G = 1;
							continue 2;
						case 1:
							$lA1 = ($this->getDeclaredConstructor__Ljava_lang_Class__Ljava_lang_reflect_Constructor_(new JA_L(0, "[Ljava.lang.Class;")));
							$fA0 = ($lA1)->newInstance__Ljava_lang_Object__Ljava_lang_Object_(new JA_L(0, "[Ljava.lang.Object;"));
							$G = 2;
							continue 2;
						case 2:return $fA0; 
						case 3:
							$fA0 = ($J__exception__);
							$lA1 = $fA0;
							$tA0 = ((new java_lang_InstantiationException()));
							$fA0 = $tA0;
							($tA0)->java_lang_InstantiationException_init__Ljava_lang_String__V(($lA1)->getMessage__Ljava_lang_String_());
							throw new WrappedThrowable($fA0);
							$G = 4;
							continue 2;
						case 4:
							$fA0 = ($J__exception__);
							$lA1 = $fA0;
							$tA1 = ((new java_lang_InstantiationException()));
							$fA0 = $tA1;
							($tA1)->java_lang_InstantiationException_init__Ljava_lang_String__V(($lA1)->getMessage__Ljava_lang_String_());
							throw new WrappedThrowable($fA0);
							break;
						default:
							break;
					}
				}
				return null;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_NoSuchMethodException)))) {
					$G = 3;
					continue 1;
				}
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_lang_reflect_InvocationTargetException)))) {
					$G = 4;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return null;
	}
	public function getDeclaredConstructor__Ljava_lang_Class__Ljava_lang_reflect_Constructor_(?JA_L $p0) {
		$lA2 = null;
		$lA3 = null;
		$lA6 = null;
		$lI4 = 0;
		$lI5 = 0;
		$fA0 = null;
		$fA1 = null;
		$fA2 = null;
		$tA0 = null;
		$tA1 = null;
		if (((($p0) == null))) goto label_1;
		$fA0 = ($p0);
		goto label_2;
		label_1:
		$fA0 = (new JA_L(0, "[Ljava.lang.Class;"));
		label_2:
		$lA2 = $fA0;
		$lA3 = ($this->getDeclaredConstructors___Ljava_lang_reflect_Constructor_());
		$lI4 = ($lA3)->length;
		$lI5 = 0;
		label_3:
		if ((($lI5 >= $lI4))) goto label_4;
		$lA6 = (($lA3)->get($lI5));
		if (!(java_util_Arrays::equals__Ljava_lang_Object__Ljava_lang_Object__Z((($lA6)->getParameterTypes___Ljava_lang_Class_()), ($lA2)))) goto label_5;
		return ($lA6);
		label_5:
		$lI5 = ((int)(($lI5 + 1)));
		goto label_3;
		label_4:
		$tA0 = ((new java_lang_NoSuchMethodException()));
		$fA0 = $tA0;
		$fA1 = $tA0;
		$tA1 = ((new java_lang_StringBuilder()));
		$fA2 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA1)->java_lang_NoSuchMethodException_init__Ljava_lang_String__V(($fA2)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_782)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->getName__Ljava_lang_String_())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_781)->append_Ljava_lang_Object__Ljava_lang_StringBuilder_((java_util_Arrays::asList__Ljava_lang_Object__Ljava_util_List_(($p0))))->toString__Ljava_lang_String_());
		throw new WrappedThrowable($fA0);
	}
	public function hashCode__I() {
		return $this->_name->hashCode__I();
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$fI0 = 0;
		if (((($this) != $p0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function getDeclaredConstructors___Ljava_lang_reflect_Constructor_() {
		$lA1 = null;
		$fA0 = null;
		$lA1 = ($this->_getDeclaredConstructors___Ljava_lang_reflect_Constructor_());
		if ((($lA1 == null))) goto label_1;
		$fA0 = $lA1;
		goto label_2;
		label_1:
		$fA0 = (new JA_L(0, "[Ljava.lang.reflect.Constructor;"));
		label_2:
		return ($fA0);
	}
	public function _getDeclaredConstructors___Ljava_lang_reflect_Constructor_() {
		return java_lang_jtransc_JTranscCoreReflection::getDeclaredConstructors_Ljava_lang_Class___Ljava_lang_reflect_Constructor_($this);
	}
	public function getDeclaringClass__Ljava_lang_Class_() {
		throw new Exception("Missing body java.lang.Class.getDeclaringClass()Ljava/lang/Class;");
	}
	public function __construct($CLASS_ID = 683) {
		parent::__construct($CLASS_ID);
		$this->_name = null;
		$this->_primitive = false;
		$this->_modifiers = 0;
		$this->__accessibleMethods = null;
		$this->_enumConstants = null;
		$this->__allFields = null;
		$this->__accessibleFields = null;
		$this->__allMethods = null;
		$this->_id = 0;
		$this->_related = null;
		$this->_info = null;
	}
	static public function SI() {
		java_lang_Class::$__classCache = null;
	}
}
class java_lang_IndexOutOfBoundsException extends java_lang_RuntimeException {

	public function java_lang_IndexOutOfBoundsException_init___V() {
		($this)->java_lang_RuntimeException_init___V();
		return $this;
		return $this;
	}
	public function java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 681) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_ArrayIndexOutOfBoundsException extends java_lang_IndexOutOfBoundsException {

	public function java_lang_ArrayIndexOutOfBoundsException_init___V() {
		($this)->java_lang_IndexOutOfBoundsException_init___V();
		return $this;
		return $this;
	}
	public function java_lang_ArrayIndexOutOfBoundsException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_IndexOutOfBoundsException_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 680) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
abstract class com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream extends java_io_OutputStream {

	public $_sb = null;
	public $_error = false;
	public function com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_init__Z_V(bool $p0) {
		$fA1 = null;
		$tA0 = null;
		($this)->java_io_OutputStream_init___V();
		$tA0 = ((new java_lang_StringBuilder()));
		$fA1 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		$this->_sb = ($fA1);
		$this->_error = $p0;
		return $this;
		return $this;
	}
	public function _write_I_V(int $p0) {
		if (((((int)((N::i2c($p0)))) != 10))) goto label_1;
		com_jtransc_io_JTranscConsole::logOrError_Ljava_lang_Object_Z_V(($this->_sb->toString__Ljava_lang_String_()), $this->_error);
		$this->_sb->setLength_I_V(0);
		goto label_3;
		label_1:
		$this->_sb->append_C_Ljava_lang_StringBuilder_((N::i2c($p0)));
		label_3:
		return;
	}
	public function __construct($CLASS_ID = 678) {
		parent::__construct($CLASS_ID);
		$this->_sb = null;
		$this->_error = false;
	}
	static public function SI() {
	}
}
class com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream extends com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream {

	public function com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream_init___V() {
		($this)->com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_init__Z_V(true);
		return $this;
		return $this;
	}
	public function write_I_V(int $p0) {
		$this->_write_I_V($p0);
		return;
	}
	public function __construct($CLASS_ID = 679) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream extends com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream {

	public function com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream_init___V() {
		($this)->com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_init__Z_V(false);
		return $this;
		return $this;
	}
	public function write_I_V(int $p0) {
		$this->_write_I_V($p0);
		return;
	}
	public function __construct($CLASS_ID = 677) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_io_FilterOutputStream extends java_io_OutputStream {

	public $__out = null;
	public function java_io_FilterOutputStream_init__Ljava_io_OutputStream__V(?java_io_OutputStream $p0) {
		($this)->java_io_OutputStream_init___V();
		$this->__out = $p0;
		return $this;
		return $this;
	}
	public function write_I_V(int $p0) {
		$this->__out->write_I_V($p0);
		return;
	}
	public function flush__V() {
		$this->__out->flush__V();
		return;
	}
	public function write__B_V(?JA_B $p0) {
		$this->write__BII_V($p0, 0, (($p0))->length);
		return;
	}
	public function write__BII_V(?JA_B $p0, int $p1, int $p2) {
		$lI4 = 0;
		$lI4 = 0;
		label_1:
		if ((($lI4 >= $p2))) goto label_2;
		$this->write_I_V(((int)(($p0->get(((int)(($p1 + $lI4))))))));
		$lI4 = ((int)(($lI4 + 1)));
		goto label_1;
		label_2:
		return;
	}
	public function __construct($CLASS_ID = 665) {
		parent::__construct($CLASS_ID);
		$this->__out = null;
	}
	static public function SI() {
	}
}
class java_io_PrintStream extends java_io_FilterOutputStream implements java_lang_Appendable, java_io_Closeable {

	public $_encoding = null;
	public $_autoFlush = false;
	public $_ioError = false;
	public function java_io_PrintStream_init__Ljava_io_OutputStream__V(?java_io_OutputStream $p0) {
		$fA0 = null;
		$tA0 = null;
		($this)->java_io_FilterOutputStream_init__Ljava_io_OutputStream__V($p0);
		$this->_encoding = Bootstrap::$STRINGLIT_54;
		if (((($p0) != null))) goto label_1;
		$tA0 = ((new java_lang_NullPointerException()));
		$fA0 = $tA0;
		($tA0)->java_lang_NullPointerException_init__Ljava_lang_String__V(Bootstrap::$STRINGLIT_783);
		throw new WrappedThrowable($fA0);
		label_1:
		return $this;
		return $this;
	}
	public function println_D_V(float $p0) {
		$this->println_Ljava_lang_String__V(java_lang_String::valueOf_D_Ljava_lang_String_($p0));
		return;
	}
	public function println_Ljava_lang_String__V(?java_lang_String $p0) {
		$this->print_Ljava_lang_String__V($p0);
		$this->newline__V();
		return;
	}
	public function newline__V() {
		$this->print_Ljava_lang_String__V(java_lang_System::lineSeparator__Ljava_lang_String_());
		return;
	}
	public function print_Ljava_lang_String__V(?java_lang_String $p0) {
		$G = 0;
		$fA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$G = 1;
							continue 2;
						case 1:
							$this->write__B_V($p0->getBytes_Ljava_lang_String___B($this->_encoding));
							$G = 2;
							continue 2;
						case 2:
							$G = 3;
							continue 2;
						case 4:
							$fA0 = $J__exception__;
							$this->setError__V();
							$G = 3;
							continue 2;
						case 3:
							return;
						default:
							break;
					}
				}
				return;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 2)))) && (($J__exception__) instanceof java_io_IOException)))) {
					$G = 4;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return;
	}
	public function write_I_V(int $p0) {
		$G = 0;
		$fI0 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$fA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							if ((($this->__out != null))) {
								$G = 1;
								continue 2;
							}
							$this->setError__V();
							return;
							$G = 1;
							continue 2;
						case 1:
							$this->__out->write_I_V($p0);
							$lI2 = ((int)(($p0 & 255)));
							if ((($lI2 == 10))) {
								$G = 2;
								continue 2;
							}
							if ((($lI2 != 21))) {
								$G = 3;
								continue 2;
							}
							$G = 2;
							continue 2;
						case 2:
							$fI0 = 1;
							$G = 4;
							continue 2;
						case 3:
							$fI0 = 0;
							$G = 4;
							continue 2;
						case 4:
							$lI3 = $fI0;
							if (!($this->_autoFlush)) {
								$G = 5;
								continue 2;
							}
							if ((($lI3 == 0))) {
								$G = 5;
								continue 2;
							}
							$this->flush__V();
							$G = 5;
							continue 2;
						case 5:
							$G = 6;
							continue 2;
						case 7:
							$fA0 = $J__exception__;
							$this->setError__V();
							$G = 6;
							continue 2;
						case 6:
							return;
						default:
							break;
					}
				}
				return;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 5)))) && (($J__exception__) instanceof java_io_IOException)))) {
					$G = 7;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return;
	}
	public function flush__V() {
		$G = 0;
		$fA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							if ((($this->__out == null))) {
								$G = 1;
								continue 2;
							}
							$G = 2;
							continue 2;
						case 2:
							$this->__out->flush__V();
							$G = 3;
							continue 2;
						case 3:
							return;
							$G = 4;
							continue 2;
						case 4:
							$fA0 = $J__exception__;
							$G = 1;
							continue 2;
						case 1:
							$this->setError__V();
							return;
						default:
							break;
					}
				}
				return;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 2)) && (($G < 3)))) && (($J__exception__) instanceof java_io_IOException)))) {
					$G = 4;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return;
	}
	public function setError__V() {
		$this->_ioError = true;
		return;
	}
	public function write__BII_V(?JA_B $p0, int $p1, int $p2) {
		$G = 0;
		$lA4 = null;
		$lA6 = null;
		$fA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							com_jtransc_JTranscArrays::checkOffsetAndCount_III_V((($p0))->length, $p1, $p2);
							$fA0 = ($this);
							$lA4 = ($this);
							N::monitorEnter($fA0);
							$G = 1;
							continue 2;
						case 1:
							if ((($this->__out != null))) {
								$G = 2;
								continue 2;
							}
							$this->setError__V();
							N::monitorExit($lA4);
							$G = 3;
							continue 2;
						case 3:
							return;
							$G = 2;
							continue 2;
						case 2:
							$this->__out->write__BII_V($p0, $p1, $p2);
							if (!($this->_autoFlush)) {
								$G = 4;
								continue 2;
							}
							$this->flush__V();
							$G = 4;
							continue 2;
						case 4:
							$G = 5;
							continue 2;
						case 6:
							$fA0 = ($J__exception__);
							$this->setError__V();
							$G = 5;
							continue 2;
						case 5:
							N::monitorExit($lA4);
							$G = 7;
							continue 2;
						case 7:
							$G = 8;
							continue 2;
						case 9:
							$lA6 = ($J__exception__);
							N::monitorExit($lA4);
							$G = 10;
							continue 2;
						case 10:
							throw new WrappedThrowable($lA6);
							$G = 8;
							continue 2;
						case 8:
							return;
						default:
							break;
					}
				}
				return;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 2)) && (($G < 4)))) && (($J__exception__) instanceof java_io_IOException)))) {
					$G = 6;
					continue 1;
				}
				if ((((((($G >= 1)) && (($G < 3)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 9;
					continue 1;
				}
				if ((((((($G >= 2)) && (($G < 7)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 9;
					continue 1;
				}
				if ((((((($G >= 9)) && (($G < 10)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 9;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return;
	}
	public function __construct($CLASS_ID = 662) {
		parent::__construct($CLASS_ID);
		$this->_encoding = null;
		$this->_autoFlush = false;
		$this->_ioError = false;
	}
	static public function SI() {
	}
}
class com_jtransc_io_JTranscConsolePrintStream extends java_io_PrintStream {

	public $_error = false;
	public $_stream = null;
	public function com_jtransc_io_JTranscConsolePrintStream_init__Z_V(bool $p0) {
		$fA1 = null;
		$tA0 = null;
		$tA1 = null;
		if (!($p0)) goto label_1;
		$tA0 = ((new com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream()));
		$fA1 = $tA0;
		($tA0)->com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream_init___V();
		goto label_2;
		label_1:
		$tA1 = ((new com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream()));
		$fA1 = $tA1;
		($tA1)->com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream_init___V();
		label_2:
		$this->com_jtransc_io_JTranscConsolePrintStream_init__Lcom_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_Z_V(($fA1), $p0);
		return $this;
		return $this;
	}
	public function com_jtransc_io_JTranscConsolePrintStream_init__Lcom_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream_Z_V(?com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream $p0, bool $p1) {
		($this)->java_io_PrintStream_init__Ljava_io_OutputStream__V(($p0));
		$this->_stream = $p0;
		$this->_error = $p1;
		return $this;
		return $this;
	}
	public function println_Ljava_lang_String__V(?java_lang_String $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		com_jtransc_io_JTranscConsole::logOrError_Ljava_lang_Object_Z_V((($fA0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($this->_stream->_sb->toString__Ljava_lang_String_())->append_Ljava_lang_String__Ljava_lang_StringBuilder_($p0)->toString__Ljava_lang_String_()), $this->_error);
		$this->_stream->_sb->setLength_I_V(0);
		return;
	}
	public function __construct($CLASS_ID = 676) {
		parent::__construct($CLASS_ID);
		$this->_error = false;
		$this->_stream = null;
	}
	static public function SI() {
	}
}
abstract class java_io_InputStream extends java_lang_Object implements java_io_Closeable {

	public function java_io_InputStream_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 675) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_System_1 extends java_io_InputStream {

	public function java_lang_System_1_init___V() {
		($this)->java_io_InputStream_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 674) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_Math extends java_lang_Object {

	public function java_lang_Math_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function max_II_I(int $p0, int $p1) {
		$fI0 = 0;
		if ((($p0 <= $p1))) goto label_1;
		$fI0 = $p0;
		goto label_2;
		label_1:
		$fI0 = $p1;
		label_2:
		return $fI0;
	}
	public static function min_II_I(int $p0, int $p1) {
		$fI0 = 0;
		if ((($p0 >= $p1))) goto label_1;
		$fI0 = $p0;
		goto label_2;
		label_1:
		$fI0 = $p1;
		label_2:
		return $fI0;
	}
	public function __construct($CLASS_ID = 673) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_jtransc_JTranscStrings extends java_lang_Object {

	public function java_lang_jtransc_JTranscStrings_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function getData_Ljava_lang_String___C(?java_lang_String $p0) {
		return $p0->getNativeCharArray___C();
	}
	public static function equals__CI_CII_Z(?JA_C $p0, int $p1, ?JA_C $p2, int $p3, int $p4) {
		$lI5 = 0;
		$lI5 = 0;
		label_1:
		if ((($lI5 >= $p4))) goto label_2;
		if (((($p0->get(((int)(($p1 + $lI5))))) == ($p2->get(((int)(($p3 + $lI5)))))))) goto label_3;
		return false;
		label_3:
		$lI5 = ((int)(($lI5 + 1)));
		goto label_1;
		label_2:
		return true;
	}
	public static function indexOf__CI_C_I(?JA_C $p0, int $p1, ?JA_C $p2) {
		$lI3 = 0;
		$lI4 = 0;
		$lI3 = ((int)(((($p0))->length - (($p2))->length)));
		$lI4 = $p1;
		label_1:
		if ((($lI4 >= $lI3))) goto label_2;
		if (!(java_lang_jtransc_JTranscStrings::equals__CI_CII_Z($p0, $lI4, $p2, 0, (($p2))->length))) goto label_3;
		return $lI4;
		label_3:
		$lI4 = ((int)(($lI4 + 1)));
		goto label_1;
		label_2:
		return -1;
	}
	public static function indexOf__CII_I(?JA_C $p0, int $p1, int $p2) {
		$lI3 = 0;
		$lI4 = 0;
		$lI3 = (($p0))->length;
		$lI4 = $p1;
		label_1:
		if ((($lI4 >= $lI3))) goto label_2;
		if (((($p0->get($lI4)) != $p2))) goto label_3;
		return $lI4;
		label_3:
		$lI4 = ((int)(($lI4 + 1)));
		goto label_1;
		label_2:
		return -1;
	}
	public function __construct($CLASS_ID = 672) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_NullPointerException extends java_lang_RuntimeException {

	public function java_lang_NullPointerException_init__Ljava_lang_String__V(?java_lang_String $p0) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_String__V($p0);
		return $this;
		return $this;
	}
	public function java_lang_NullPointerException_init___V() {
		($this)->java_lang_RuntimeException_init___V();
		return $this;
		return $this;
	}
	public function java_lang_NullPointerException_init__Ljava_lang_Throwable__V(?java_lang_Throwable $p0) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_Throwable__V($p0);
		return $this;
		return $this;
	}
	public function java_lang_NullPointerException_init__Ljava_lang_String_Ljava_lang_Throwable__V(?java_lang_String $p0, ?java_lang_Throwable $p1) {
		($this)->java_lang_RuntimeException_init__Ljava_lang_String_Ljava_lang_Throwable__V($p0, $p1);
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 668) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_System extends java_lang_Object {

	public static $__in = null;
	public static $__out = null;
	public static $_err = null;
	public static $__props = null;
	public function java_lang_System_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function java_lang_System_clinit___V() {
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		$tA2 = null;
		$tA0 = ((new java_lang_System_1()));
		$fA0 = $tA0;
		($tA0)->java_lang_System_1_init___V();
		java_lang_System::$__in = ($fA0);
		$tA1 = ((new com_jtransc_io_JTranscConsolePrintStream()));
		$fA0 = $tA1;
		($tA1)->com_jtransc_io_JTranscConsolePrintStream_init__Z_V(false);
		java_lang_System::$__out = ($fA0);
		$tA2 = ((new com_jtransc_io_JTranscConsolePrintStream()));
		$fA0 = $tA2;
		($tA2)->com_jtransc_io_JTranscConsolePrintStream_init__Z_V(true);
		java_lang_System::$_err = ($fA0);
		return;
	}
	public static function arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(?java_lang_Object $p0, int $p1, ?java_lang_Object $p2, int $p3, int $p4) {
		N::arraycopy($p0, $p1, $p2, $p3, $p4);
	}
	public static function identityHashCode_Ljava_lang_Object__I(?java_lang_Object $p0) {
		return java_lang_SystemInt::identityHashCode_Ljava_lang_Object__I($p0);
	}
	public static function gc__V() {
		java_lang_Runtime::getRuntime__Ljava_lang_Runtime_()->gc__V();
		return;
	}
	public static function lineSeparator__Ljava_lang_String_() {
		return com_jtransc_JTranscSystem::lineSeparator__Ljava_lang_String_();
	}
	public static function currentTimeMillis__J() {
		return N::d2j(com_jtransc_JTranscSystem::fastTime__D());
	}
	public static function getProperty_Ljava_lang_String__Ljava_lang_String_(?java_lang_String $p0) {
		return java_lang_System::getProps__Ljava_util_Properties_()->getProperty_Ljava_lang_String__Ljava_lang_String_($p0);
	}
	public static function getProps__Ljava_util_Properties_() {
		$fA0 = null;
		$tA0 = null;
		if (((java_lang_System::$__props != null))) goto label_1;
		$tA0 = ((new java_util_Properties()));
		$fA0 = $tA0;
		($tA0)->java_util_Properties_init___V();
		java_lang_System::$__props = ($fA0);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_136, com_jtransc_JTranscSystem::getArch__Ljava_lang_String_(), Bootstrap::$STRINGLIT_137);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_133, com_jtransc_JTranscSystem::getOS__Ljava_lang_String_(), Bootstrap::$STRINGLIT_137);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_784, Bootstrap::$STRINGLIT_785);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_786, com_jtransc_JTranscSystem::getRuntimeName__Ljava_lang_String_(), Bootstrap::$STRINGLIT_787);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_788, Bootstrap::$STRINGLIT_789);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_790, Bootstrap::$STRINGLIT_791);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_792, Bootstrap::$STRINGLIT_793);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_794, com_jtransc_JTranscSystem::fileSeparator__Ljava_lang_String_(), Bootstrap::$STRINGLIT_52);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_795, com_jtransc_JTranscSystem::lineSeparator__Ljava_lang_String_(), Bootstrap::$STRINGLIT_45);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_796, com_jtransc_JTranscSystem::pathSeparator__Ljava_lang_String_(), Bootstrap::$STRINGLIT_55);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_797, com_jtransc_JTranscSystemProperties::fileEncoding__Ljava_lang_String_());
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_135, com_jtransc_JTranscSystem::getJavaHome__Ljava_lang_String_(), Bootstrap::$STRINGLIT_52);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_798, com_jtransc_JTranscSystem::getRuntimeName__Ljava_lang_String_(), Bootstrap::$STRINGLIT_787);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_799, Bootstrap::$STRINGLIT_800);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_801, Bootstrap::$STRINGLIT_802);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_803, Bootstrap::$STRINGLIT_800);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_804, Bootstrap::$STRINGLIT_805);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_806, Bootstrap::$STRINGLIT_807);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_808, Bootstrap::$STRINGLIT_809);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_810, Bootstrap::$STRINGLIT_800);
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_811, com_jtransc_JTranscVersion::getVersion__Ljava_lang_String_());
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_812, com_jtransc_JTranscSystemProperties::tmpdir__Ljava_lang_String_());
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_813, com_jtransc_JTranscSystemProperties::userHome__Ljava_lang_String_());
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_814, com_jtransc_JTranscSystemProperties::userDir__Ljava_lang_String_());
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_815, com_jtransc_JTranscSystemProperties::userName__Ljava_lang_String_());
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_816, com_jtransc_JTranscSystemProperties::userLanguage__Ljava_lang_String_());
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_817, com_jtransc_JTranscSystemProperties::userRegion__Ljava_lang_String_());
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String__V(Bootstrap::$STRINGLIT_818, com_jtransc_JTranscSystemProperties::userVariant__Ljava_lang_String_());
		label_1:
		return java_lang_System::$__props;
	}
	public static function getenv_Ljava_lang_String__Ljava_lang_String_(?java_lang_String $p0) {
		return N::checkcast(java_lang_System::getenv__Ljava_util_Map_()->get_Ljava_lang_Object__Ljava_lang_Object_(($p0)), "java_lang_String");
	}
	public static function getenv__Ljava_util_Map_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_util_HashMap()));
		$fA0 = $tA0;
		($tA0)->java_util_HashMap_init___V();
		return ($fA0);
	}
	public static function _setProperty_Ljava_lang_String_Ljava_lang_String__V(?java_lang_String $p0, ?java_lang_String $p1) {
		java_lang_System::_setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V($p0, $p1, Bootstrap::$STRINGLIT_23);
		return;
	}
	public static function _setProperty_Ljava_lang_String_Ljava_lang_String_Ljava_lang_String__V(?java_lang_String $p0, ?java_lang_String $p1, ?java_lang_String $p2) {
		$lA0 = null;
		$lA1 = null;
		$lA0 = ($p0);
		$lA1 = ($p1);
		if ((($lA0 != null))) goto label_1;
		$lA0 = (Bootstrap::$STRINGLIT_23);
		label_1:
		if ((($lA1 != null))) goto label_2;
		$lA1 = ($p2);
		label_2:
		java_lang_System::getProps__Ljava_util_Properties_()->put_Ljava_lang_Object_Ljava_lang_Object__Ljava_lang_Object_($lA0, $lA1);
		return;
	}
	public function __construct($CLASS_ID = 661) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		java_lang_System::$__in = null;
		java_lang_System::$__out = null;
		java_lang_System::$_err = null;
		java_lang_System::$__props = null;
		java_lang_System::java_lang_System_clinit___V();
	}
}
class java_util_Arrays extends java_lang_Object {

	public function java_util_Arrays_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function copyOfRange__CII__C(?JA_C $p0, int $p1, int $p2) {
		$lA6 = null;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$lI3 = java_util_Arrays::checkRange_III_I($p1, $p2, (($p0))->length);
		$lI4 = ((int)(($p2 - $p1)));
		$lI5 = java_lang_Math::min_II_I($lI4, ((int)(($lI3 - $p1))));
		$lA6 = (new JA_C($lI4));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($p0), $p1, $lA6, 0, $lI5);
		return ($lA6);
	}
	public static function copyOf__CI__C(?JA_C $p0, int $p1) {
		return java_util_Arrays::copyOfRange__CII__C($p0, 0, $p1);
	}
	public static function checkRange_III_I(int $p0, int $p1, int $p2) {
		$fA0 = null;
		$tA0 = null;
		$tA1 = null;
		if ((($p0 <= $p1))) goto label_1;
		$tA0 = ((new java_lang_IllegalArgumentException()));
		$fA0 = $tA0;
		($tA0)->java_lang_IllegalArgumentException_init___V();
		throw new WrappedThrowable($fA0);
		label_1:
		if ((($p0 < 0))) goto label_2;
		if ((($p0 <= $p2))) goto label_3;
		label_2:
		$tA1 = ((new java_lang_ArrayIndexOutOfBoundsException()));
		$fA0 = $tA1;
		($tA1)->java_lang_ArrayIndexOutOfBoundsException_init___V();
		throw new WrappedThrowable($fA0);
		label_3:
		return $p2;
	}
	public static function copyOf__Ljava_lang_Object_I__Ljava_lang_Object_(?JA_L $p0, int $p1) {
		return java_util_Arrays::copyOfRange__Ljava_lang_Object_II__Ljava_lang_Object_($p0, 0, $p1);
	}
	public static function copyOfRange__Ljava_lang_Object_II__Ljava_lang_Object_(?JA_L $p0, int $p1, int $p2) {
		$lA6 = null;
		$lI3 = 0;
		$lI4 = 0;
		$lI5 = 0;
		$lI3 = java_util_Arrays::checkRange_III_I($p1, $p2, (($p0))->length);
		$lI4 = ((int)(($p2 - $p1)));
		$lI5 = java_lang_Math::min_II_I($lI4, ((int)(($lI3 - $p1))));
		$lA6 = (N::checkcast(java_util_Arrays::newInstance__Ljava_lang_Object_I__Ljava_lang_Object_($p0, $lI4), "JA_L"));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($p0), $p1, $lA6, 0, $lI5);
		return ($lA6);
	}
	public static function newInstance__Ljava_lang_Object_I__Ljava_lang_Object_(?JA_L $p0, int $p1) {
		return N::checkcast(N::checkcast(java_lang_reflect_Array::newInstance_Ljava_lang_Class_I_Ljava_lang_Object_(($p0)->getClass__Ljava_lang_Class_()->getComponentType__Ljava_lang_Class_(), $p1), "JA_L"), "JA_L");
	}
	public static function copyOf__Ljava_lang_Object_ILjava_lang_Class___Ljava_lang_Object_(?JA_L $p0, int $p1, ?java_lang_Class $p2) {
		return java_util_Arrays::copyOfRange__Ljava_lang_Object_IILjava_lang_Class___Ljava_lang_Object_($p0, 0, $p1, $p2);
	}
	public static function copyOfRange__Ljava_lang_Object_IILjava_lang_Class___Ljava_lang_Object_(?JA_L $p0, int $p1, int $p2, ?java_lang_Class $p3) {
		$lA7 = null;
		$lI4 = 0;
		$lI5 = 0;
		$lI6 = 0;
		$lI4 = java_util_Arrays::checkRange_III_I($p1, $p2, (($p0))->length);
		$lI5 = ((int)(($p2 - $p1)));
		$lI6 = java_lang_Math::min_II_I($lI5, ((int)(($lI4 - $p1))));
		$lA7 = (N::checkcast(N::checkcast(java_lang_reflect_Array::newInstance_Ljava_lang_Class_I_Ljava_lang_Object_($p3->getComponentType__Ljava_lang_Class_(), $lI5), "JA_L"), "JA_L"));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($p0), $p1, $lA7, 0, $lI6);
		return ($lA7);
	}
	public static function equals__Ljava_lang_Object__Ljava_lang_Object__Z(?JA_L $p0, ?JA_L $p1) {
		$lA3 = null;
		$lA4 = null;
		$lI2 = 0;
		if (((($p0) != ($p1)))) goto label_1;
		return true;
		label_1:
		if (((($p0) == null))) goto label_2;
		if (((($p1) == null))) goto label_2;
		if ((((($p0))->length == (($p1))->length))) goto label_3;
		label_2:
		return false;
		label_3:
		$lI2 = 0;
		label_4:
		if ((($lI2 >= (($p0))->length))) goto label_5;
		$lA3 = ($p0->get($lI2));
		$lA4 = ($p1->get($lI2));
		if ((($lA3 != null))) goto label_7;
		if ((($lA4 != null))) goto label_8;
		goto label_9;
		label_7:
		if ($lA3->equals_Ljava_lang_Object__Z($lA4)) goto label_9;
		label_8:
		return false;
		label_9:
		$lI2 = ((int)(($lI2 + 1)));
		goto label_4;
		label_5:
		return true;
	}
	public static function asList__Ljava_lang_Object__Ljava_util_List_(?JA_L $p0) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_util_Arrays_ArrayList()));
		$fA0 = $tA0;
		($tA0)->java_util_Arrays_ArrayList_init___Ljava_lang_Object__V($p0);
		return ($fA0);
	}
	public static function access_000__Ljava_lang_Object_I__Ljava_lang_Object_(?JA_L $p0, int $p1) {
		return java_util_Arrays::newInstance__Ljava_lang_Object_I__Ljava_lang_Object_($p0, $p1);
	}
	public function __construct($CLASS_ID = 660) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_StringBuilder extends java_lang_Object implements java_io_Serializable, java_lang_Appendable, java_lang_CharSequence {

	public $_buffer = null;
	public $_length = 0;
	public function toString__Ljava_lang_String_() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_String()));
		$fA0 = $tA0;
		($tA0)->java_lang_String_init___CII_V($this->_buffer, 0, $this->_length);
		return ($fA0);
	}
	public function java_lang_StringBuilder_init___V() {
		$this->java_lang_StringBuilder_init__I_V(16);
		return $this;
		return $this;
	}
	public function java_lang_StringBuilder_init__I_V(int $p0) {
		($this)->java_lang_Object_init___V();
		$this->_buffer = new JA_C($p0);
		return $this;
		return $this;
	}
	public function append_Ljava_lang_String__Ljava_lang_StringBuilder_(?java_lang_String $p0) {
		$lA1 = null;
		$lI2 = 0;
		$lA1 = ($p0);
		if ((($lA1 != null))) goto label_1;
		$lA1 = (Bootstrap::$STRINGLIT_819);
		label_1:
		$lI2 = ($lA1)->length__I();
		$this->ensureCapacity_I_V(((int)(($this->_length + $lI2))));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V((java_lang_jtransc_JTranscStrings::getData_Ljava_lang_String___C(($lA1))), 0, ($this->_buffer), $this->_length, $lI2);
		$this->_length = ((int)(($this->_length + $lI2)));
		return $this;
	}
	public function length__I() {
		return $this->_length;
	}
	public function ensureCapacity_I_V(int $p0) {
		$this->ensure_I__C($p0);
		return;
	}
	public function ensure_I__C(int $p0) {
		if ((($p0 <= ($this->_buffer)->length))) goto label_1;
		$this->_buffer = java_util_Arrays::copyOf__CI__C($this->_buffer, java_lang_Math::max_II_I($p0, ((int)((((int)(N::imul(($this->_buffer)->length, 2))) + 2)))));
		label_1:
		return $this->_buffer;
	}
	public function charAt_I_C(int $p0) {
		return ($this->_buffer->get($p0));
	}
	public function append_C_Ljava_lang_StringBuilder_(int $p0) {
		$fA0 = null;
		$fI1 = 0;
		$fA1 = null;
		$tI1 = 0;
		$tA2 = null;
		$this->ensureCapacity_I_V(((int)(($this->_length + 1))));
		$fA0 = $this->_buffer;
		$fA1 = ($this);
		$tA2 = $fA1;
		$tI1 = $this->_length;
		$fI1 = $tI1;
		($tA2)->_length = ((int)(($tI1 + 1)));
		$fA0->set($fI1, $p0);
		return $this;
	}
	public function reverse__Ljava_lang_StringBuilder_() {
		$lI1 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$lI4 = 0;
		$lI1 = ((int)(N::idiv($this->_length, 2)));
		$lI2 = 0;
		label_1:
		if ((($lI2 >= $lI1))) goto label_2;
		$lI3 = ((int)((((int)(($this->_length - $lI2))) - 1)));
		$lI4 = ((int)(($this->_buffer->get($lI2))));
		$this->_buffer->set($lI2, ($this->_buffer->get($lI3)));
		$this->_buffer->set($lI3, (N::i2c($lI4)));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		return $this;
	}
	public function append_I_Ljava_lang_StringBuilder_(int $p0) {
		$this->ensureCapacity_I_V(((int)(($this->_length + 11))));
		$this->_length = ((int)(($this->_length + java_lang_IntegerTools::writeInt__CIII_I($this->_buffer, $this->_length, $p0, 10))));
		return $this;
	}
	public function append_Ljava_lang_Object__Ljava_lang_StringBuilder_(?java_lang_Object $p0) {
		return $this->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_String::valueOf_Ljava_lang_Object__Ljava_lang_String_($p0));
	}
	public function substring_II_Ljava_lang_String_(int $p0, int $p1) {
		return $this->toString__Ljava_lang_String_()->substring_II_Ljava_lang_String_($p0, $p1);
	}
	public function indexOf_Ljava_lang_String__I(?java_lang_String $p0) {
		return $this->indexOf_Ljava_lang_String_I_I($p0, 0);
	}
	public function indexOf_Ljava_lang_String_I_I(?java_lang_String $p0, int $p1) {
		return java_lang_jtransc_JTranscStrings::indexOf__CI_C_I($this->_buffer, $p1, java_lang_jtransc_JTranscStrings::getData_Ljava_lang_String___C($p0));
	}
	public function substring_I_Ljava_lang_String_(int $p0) {
		return $this->toString__Ljava_lang_String_()->substring_I_Ljava_lang_String_($p0);
	}
	public function setLength_I_V(int $p0) {
		$this->delete_II_Ljava_lang_StringBuilder_($p0, $this->length__I());
		return;
	}
	public function delete_II_Ljava_lang_StringBuilder_(int $p0, int $p1) {
		return $this->replace_IILjava_lang_String__Ljava_lang_StringBuilder_($p0, $p1, Bootstrap::$STRINGLIT_23);
	}
	public function replace_IILjava_lang_String__Ljava_lang_StringBuilder_(int $p0, int $p1, ?java_lang_String $p2) {
		$lI4 = 0;
		$lI5 = 0;
		$lI4 = $p2->length__I();
		$lI5 = ((int)(($p1 - $p0)));
		$this->ensureCapacity_I_V(((int)((((int)(($this->_length - $lI5))) + $lI4))));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V(($this->_buffer), $p1, ($this->_buffer), ((int)(($p0 + $lI4))), ((int)(($this->_length - $p1))));
		java_lang_System::arraycopy_Ljava_lang_Object_ILjava_lang_Object_II_V((java_lang_jtransc_JTranscStrings::getData_Ljava_lang_String___C($p2)), 0, ($this->_buffer), $p0, $lI4);
		$this->_length = ((int)((((int)(($this->_length - $lI5))) + $lI4)));
		return $this;
	}
	public function append_F_Ljava_lang_StringBuilder_(float $p0) {
		return $this->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_String::valueOf_F_Ljava_lang_String_($p0));
	}
	public function append_J_Ljava_lang_StringBuilder_(Int64 $p0) {
		return $this->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_String::valueOf_J_Ljava_lang_String_($p0));
	}
	public function append_D_Ljava_lang_StringBuilder_(float $p0) {
		return $this->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_String::valueOf_D_Ljava_lang_String_($p0));
	}
	public function __construct($CLASS_ID = 658) {
		parent::__construct($CLASS_ID);
		$this->_buffer = null;
		$this->_length = 0;
	}
	static public function SI() {
	}
}
class java_lang_String_1 extends java_lang_Object {

	public function __construct($CLASS_ID = 657) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
interface java_util_Comparator {

	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0);
}
class java_util_Comparator_IFields {

	static public function SI() {
	}
}
class java_lang_String_CaseInsensitiveComparator extends java_lang_Object implements java_util_Comparator, java_io_Serializable {

	public function java_lang_String_CaseInsensitiveComparator_init__Ljava_lang_String_1__V(?java_lang_String_1 $p0) {
		$this->java_lang_String_CaseInsensitiveComparator_init___V();
		return $this;
		return $this;
	}
	public function java_lang_String_CaseInsensitiveComparator_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public function __construct($CLASS_ID = 655) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
	}
}
class java_lang_String extends java_lang_Object implements java_io_Serializable, java_lang_Comparable, java_lang_CharSequence {
	public $_str = null;
	public static $_CASE_INSENSITIVE_ORDER = null;
	public $_hash = 0;
	public $_value = null;
	public function toString__Ljava_lang_String_() {
		return $this;
	}
	public static function java_lang_String_clinit___V() {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_String_CaseInsensitiveComparator()));
		$fA0 = $tA0;
		($tA0)->java_lang_String_CaseInsensitiveComparator_init__Ljava_lang_String_1__V(null);
		java_lang_String::$_CASE_INSENSITIVE_ORDER = ($fA0);
		return;
	}
	public function java_lang_String_init___CII_V(?JA_C $p0, int $p1, int $p2) {
		($this)->java_lang_Object_init___V();
		$this->_hash = 0;
		$this->setChars__C_V(java_util_Arrays::copyOfRange__CII__C($p0, $p1, ((int)(($p1 + $p2)))));
		return $this;
		return $this;
	}
	public function getNativeCharArray___C() {
		return $this->_value;
	}
	public function length__I() {
		return ($this->_value)->length;
	}
	public function setChars__C_V(?JA_C $p0) {
		$this->_value = $p0;
		return;
	}
	public function substring_II_Ljava_lang_String_(int $p0, int $p1) {
		$fA0 = null;
		$tA0 = null;
		$tA0 = ((new java_lang_String()));
		$fA0 = $tA0;
		($tA0)->java_lang_String_init___CII_V($this->_value, $p0, ((int)(($p1 - $p0))));
		return ($fA0);
	}
	public function charAt_I_C(int $p0) {
		return ($this->_value->get($p0));
	}
	public function replace_Ljava_lang_CharSequence_Ljava_lang_CharSequence__Ljava_lang_String_(?java_lang_CharSequence $p0, ?java_lang_CharSequence $p1) {
		return $this->_replace_Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_($p0->toString__Ljava_lang_String_(), $p1->toString__Ljava_lang_String_());
	}
	public function _replace_Ljava_lang_String_Ljava_lang_String__Ljava_lang_String_(?java_lang_String $p0, ?java_lang_String $p1) {
		$lA4 = null;
		$lI3 = 0;
		$lI7 = 0;
		$fA0 = null;
		$lA5 = null;
		$lA6 = null;
		$tA0 = null;
		$lI3 = $this->length__I();
		$tA0 = ((new java_lang_StringBuilder()));
		$fA0 = $tA0;
		($tA0)->java_lang_StringBuilder_init__I_V($lI3);
		$lA4 = $fA0;
		$lA5 = $this->_value;
		$lA6 = $p0->_value;
		$lI7 = 0;
		label_1:
		if ((($lI7 >= $lI3))) goto label_2;
		if ((($lI7 >= ((int)(($this->length__I() - $p0->length__I())))))) goto label_4;
		if (!(java_lang_jtransc_JTranscStrings::equals__CI_CII_Z($lA5, $lI7, $lA6, 0, $p0->length__I()))) goto label_4;
		($lA4)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($p1);
		$lI7 = ((int)(($lI7 + $p0->length__I())));
		goto label_1;
		label_4:
		($lA4)->append_C_Ljava_lang_StringBuilder_(($lA5->get($lI7)));
		$lI7 = ((int)(($lI7 + 1)));
		goto label_1;
		label_2:
		return ($lA4)->toString__Ljava_lang_String_();
	}
	public function indexOf_Ljava_lang_String__I(?java_lang_String $p0) {
		return $this->indexOf_Ljava_lang_String_I_I($p0, 0);
	}
	public function indexOf_Ljava_lang_String_I_I(?java_lang_String $p0, int $p1) {
		return java_lang_jtransc_JTranscStrings::indexOf__CI_C_I($this->_value, $p1, $p0->_value);
	}
	public function indexOf_I_I(int $p0) {
		return $this->indexOf_II_I($p0, 0);
	}
	public function indexOf_II_I(int $p0, int $p1) {
		return java_lang_jtransc_JTranscStrings::indexOf__CII_I($this->_value, $p1, $p0);
	}
	public function startsWith_Ljava_lang_String__Z(?java_lang_String $p0) {
		$fI0 = 0;
		if ((($this->length__I() < $p0->length__I()))) goto label_1;
		if (!(java_lang_jtransc_JTranscStrings::equals__CI_CII_Z($this->_value, 0, $p0->_value, 0, $p0->length__I()))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public static function valueOf_Ljava_lang_Object__Ljava_lang_String_(?java_lang_Object $p0) {
		$fA0 = null;
		if ((($p0 == null))) goto label_1;
		$fA0 = $p0->toString__Ljava_lang_String_();
		goto label_2;
		label_1:
		$fA0 = Bootstrap::$STRINGLIT_819;
		label_2:
		return $fA0;
	}
	public function substring_I_Ljava_lang_String_(int $p0) {
		return $this->substring_II_Ljava_lang_String_($p0, ($this->_value)->length);
	}
	public function replace_CC_Ljava_lang_String_(int $p0, int $p1) {
		$lA3 = null;
		$lI4 = 0;
		$fA0 = null;
		$tA0 = null;
		$lA3 = (java_util_Arrays::copyOf__CI__C($this->_value, ($this->_value)->length));
		$lI4 = 0;
		label_1:
		if ((($lI4 >= ($lA3)->length))) goto label_2;
		if ((((($lA3)->get($lI4)) != ((int)($p0))))) goto label_3;
		($lA3)->set($lI4, $p1);
		label_3:
		$lI4 = ((int)(($lI4 + 1)));
		goto label_1;
		label_2:
		$tA0 = ((new java_lang_String()));
		$fA0 = $tA0;
		($tA0)->java_lang_String_init___C_V(($lA3));
		return ($fA0);
	}
	public function java_lang_String_init___C_V(?JA_C $p0) {
		($this)->java_lang_Object_init___V();
		$this->_hash = 0;
		$this->setChars__C_V(java_util_Arrays::copyOf__CI__C($p0, (($p0))->length));
		return $this;
		return $this;
	}
	public function endsWith_Ljava_lang_String__Z(?java_lang_String $p0) {
		$fI0 = 0;
		if ((($this->length__I() < $p0->length__I()))) goto label_1;
		if (!(java_lang_jtransc_JTranscStrings::equals__CI_CII_Z($this->_value, ((int)((($this->_value)->length - $p0->length__I()))), $p0->_value, 0, $p0->length__I()))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public function equals_Ljava_lang_Object__Z(?java_lang_Object $p0) {
		$lI3 = 0;
		$lI4 = 0;
		$lA2 = null;
		if (((($this) != $p0))) goto label_1;
		return true;
		label_1:
		if ((($p0) instanceof java_lang_String)) goto label_2;
		return false;
		label_2:
		$lA2 = N::checkcast($p0, "java_lang_String");
		if ((($this->length__I() == $lA2->length__I()))) goto label_3;
		return false;
		label_3:
		if ((($this->hashCode__I() == $lA2->hashCode__I()))) goto label_4;
		return false;
		label_4:
		$lI3 = $this->length__I();
		$lI4 = 0;
		label_5:
		if ((($lI4 >= $lI3))) goto label_6;
		if ((($this->charAt_I_C($lI4) == $lA2->charAt_I_C($lI4)))) goto label_7;
		return false;
		label_7:
		$lI4 = ((int)(($lI4 + 1)));
		goto label_5;
		label_6:
		return true;
	}
	public function hashCode__I() {
		$lI1 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$lI1 = $this->_hash;
		$lI2 = $this->length__I();
		if ((($lI1 != 0))) goto label_1;
		if ((($lI2 <= 0))) goto label_1;
		$lI3 = 0;
		label_3:
		if ((($lI3 >= $lI2))) goto label_4;
		$lI1 = ((int)((((int)(N::imul(31, $lI1))) + $this->charAt_I_C($lI3))));
		$lI3 = ((int)(($lI3 + 1)));
		goto label_3;
		label_4:
		$this->_hash = $lI1;
		label_1:
		return $lI1;
	}
	public function isEmpty__Z() {
		$fI0 = 0;
		if ((($this->length__I() != 0))) goto label_1;
		$fI0 = 1;
		goto label_2;
		label_1:
		$fI0 = 0;
		label_2:
		return (($fI0)!=0);
	}
	public static function valueOf_D_Ljava_lang_String_(float $p0) {
		return java_lang_Double::toString_D_Ljava_lang_String_($p0);
	}
	public function getBytes_Ljava_lang_String___B(?java_lang_String $p0) {
		return com_jtransc_charset_JTranscCharset::forName_Ljava_lang_String__Lcom_jtransc_charset_JTranscCharset_($p0)->encode_Ljava_lang_String___B($this);
	}
	public function java_lang_String_init___BII_V(?JA_B $p0, int $p1, int $p2) {
		$this->java_lang_String_init___BIILjava_lang_String_Z_V($p0, $p1, $p2, Bootstrap::$STRINGLIT_54, false);
		return $this;
		return $this;
	}
	public function java_lang_String_init___BIILjava_lang_String_Z_V(?JA_B $p0, int $p1, int $p2, ?java_lang_String $p3, bool $p4) {
		($this)->java_lang_Object_init___V();
		$this->_hash = 0;
		$this->setChars__C_V(com_jtransc_charset_JTranscCharset::forName_Ljava_lang_String__Lcom_jtransc_charset_JTranscCharset_($p3)->decodeChars__BII__C($p0, $p1, $p2));
		return $this;
		return $this;
	}
	public function toCharArray___C() {
		return java_util_Arrays::copyOf__CI__C($this->_value, ($this->_value)->length);
	}
	public function trim__Ljava_lang_String_() {
		$lI1 = 0;
		$lI2 = 0;
		$lI3 = 0;
		$fA0 = null;
		$tA0 = null;
		$lI1 = $this->length__I();
		$lI2 = 0;
		$lI3 = ((int)(($lI1 - 1)));
		label_1:
		if ((($lI2 >= $this->length__I()))) goto label_2;
		if (!(java_lang_Character::isSpaceChar_C_Z(($this->_value->get($lI2))))) goto label_2;
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		if ((($lI3 < 0))) goto label_3;
		if (!(java_lang_Character::isSpaceChar_C_Z(($this->_value->get($lI3))))) goto label_3;
		$lI3 = ((int)(($lI3 + -1)));
		goto label_2;
		label_3:
		$tA0 = ((new java_lang_String()));
		$fA0 = $tA0;
		($tA0)->java_lang_String_init___CII_V($this->_value, $lI2, ((int)((((int)(($lI3 - $lI2))) + 1))));
		return ($fA0);
	}
	public function toUpperCase__Ljava_lang_String_() {
		$lA1 = null;
		$lI2 = 0;
		$fA0 = null;
		$tA0 = null;
		$lA1 = (java_util_Arrays::copyOf__CI__C($this->_value, ($this->_value)->length));
		$lI2 = 0;
		label_1:
		if ((($lI2 >= ($lA1)->length))) goto label_2;
		($lA1)->set($lI2, java_lang_Character::toUpperCase_C_C((($lA1)->get($lI2))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		$tA0 = ((new java_lang_String()));
		$fA0 = $tA0;
		($tA0)->java_lang_String_init___C_V(($lA1));
		return ($fA0);
	}
	public static function valueOf_F_Ljava_lang_String_(float $p0) {
		return java_lang_Float::toString_F_Ljava_lang_String_($p0);
	}
	public static function valueOf_J_Ljava_lang_String_(Int64 $p0) {
		return java_lang_Long::toString_J_Ljava_lang_String_($p0);
	}
	public function toLowerCase__Ljava_lang_String_() {
		$lA1 = null;
		$lI2 = 0;
		$fA0 = null;
		$tA0 = null;
		$lA1 = (java_util_Arrays::copyOf__CI__C($this->_value, ($this->_value)->length));
		$lI2 = 0;
		label_1:
		if ((($lI2 >= ($lA1)->length))) goto label_2;
		($lA1)->set($lI2, java_lang_Character::toLowerCase_C_C((($lA1)->get($lI2))));
		$lI2 = ((int)(($lI2 + 1)));
		goto label_1;
		label_2:
		$tA0 = ((new java_lang_String()));
		$fA0 = $tA0;
		($tA0)->java_lang_String_init___C_V(($lA1));
		return ($fA0);
	}
	public function __construct($CLASS_ID = 651) {
		parent::__construct($CLASS_ID);
		$this->_hash = 0;
		$this->_value = null;
	}
	static public function SI() {
		java_lang_String::$_CASE_INSENSITIVE_ORDER = null;
		java_lang_String::java_lang_String_clinit___V();
	}
}
class Benchmark extends java_lang_Object {

	public static $_totalTime = 0.0;
	public function Benchmark_init___V() {
		($this)->java_lang_Object_init___V();
		return $this;
		return $this;
	}
	public static function Benchmark_clinit___V() {
		Benchmark::$_totalTime = 0.0;
		return;
	}
	public static function main__Ljava_lang_String__V(?JA_L $p0) {
		$fA1 = null;
		$tA39 = null;
		$tA31 = null;
		$tA35 = null;
		$tA3 = null;
		$tA7 = null;
		$lA14 = null;
		$tA296 = null;
		$lA4 = null;
		$tA22 = null;
		$tA26 = null;
		$lA8 = null;
		$tA10 = null;
		$tA14 = null;
		$tA18 = null;
		$lA1 = null;
		$lI13 = 0;
		$fA0 = null;
		$lA5 = null;
		$tA32 = null;
		$tA36 = null;
		$tA2 = null;
		$tA6 = null;
		$lA11 = null;
		$tA295 = null;
		$tA299 = null;
		$tA23 = null;
		$tA27 = null;
		$tA40 = null;
		$tA11 = null;
		$tA15 = null;
		$lA9 = null;
		$tA19 = null;
		$lA12 = null;
		$tA33 = null;
		$tA37 = null;
		$tA1 = null;
		$tA5 = null;
		$tA9 = null;
		$tA294 = null;
		$tA298 = null;
		$tA301 = null;
		$tA20 = null;
		$tA24 = null;
		$tA28 = null;
		$tA41 = null;
		$lA6 = null;
		$tA12 = null;
		$tA16 = null;
		$lA2 = null;
		$lI15 = 0;
		$tA38 = null;
		$tA30 = null;
		$tA34 = null;
		$tA0 = null;
		$tA4 = null;
		$tA8 = null;
		$lA13 = null;
		$tA297 = null;
		$tA300 = null;
		$lA10 = null;
		$tA21 = null;
		$tA25 = null;
		$tA29 = null;
		$tA13 = null;
		$tA17 = null;
		$lA3 = null;
		$lA7 = null;
		$lA1 = java_lang_Runtime::getRuntime__Ljava_lang_Runtime_();
		$fA0 = (java_lang_System::$__out);
		$tA0 = ((new java_lang_StringBuilder()));
		$fA1 = $tA0;
		($tA0)->java_lang_StringBuilder_init___V();
		($fA0)->println_Ljava_lang_String__V(($fA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_821)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(com_jtransc_JTranscVersion::getVersion__Ljava_lang_String_())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_820)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(com_jtransc_JTranscSystem::getRuntimeKind__Ljava_lang_String_())->toString__Ljava_lang_String_());
		$fA0 = (java_lang_System::$__out);
		$tA1 = ((new java_lang_StringBuilder()));
		$fA1 = $tA1;
		($tA1)->java_lang_StringBuilder_init___V();
		($fA0)->println_Ljava_lang_String__V(($fA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_822)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_System::getProperty_Ljava_lang_String__Ljava_lang_String_(Bootstrap::$STRINGLIT_788))->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_820)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_System::getProperty_Ljava_lang_String__Ljava_lang_String_(Bootstrap::$STRINGLIT_790))->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_820)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(java_lang_System::getProperty_Ljava_lang_String__Ljava_lang_String_(Bootstrap::$STRINGLIT_792))->toString__Ljava_lang_String_());
		$fA0 = (java_lang_System::$__out);
		$tA2 = ((new java_lang_StringBuilder()));
		$fA1 = $tA2;
		($tA2)->java_lang_StringBuilder_init___V();
		($fA0)->println_Ljava_lang_String__V(($fA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_825)->append_J_Ljava_lang_StringBuilder_($lA1->freeMemory__J())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_824)->append_J_Ljava_lang_StringBuilder_($lA1->maxMemory__J())->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_823)->append_J_Ljava_lang_StringBuilder_($lA1->totalMemory__J())->toString__Ljava_lang_String_());
		java_lang_System::$__out->println_Ljava_lang_String__V(Bootstrap::$STRINGLIT_826);
		$fA0 = (Bootstrap::$STRINGLIT_827);
		$tA3 = ((new Benchmark_1()));
		$fA1 = $tA3;
		($tA3)->Benchmark_1_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_828);
		$tA4 = ((new Benchmark_2()));
		$fA1 = $tA4;
		($tA4)->Benchmark_2_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_829);
		$tA5 = ((new Benchmark_3()));
		$fA1 = $tA5;
		($tA5)->Benchmark_3_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_830);
		$tA6 = ((new Benchmark_4()));
		$fA1 = $tA6;
		($tA6)->Benchmark_4_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_831);
		$tA7 = ((new Benchmark_5()));
		$fA1 = $tA7;
		($tA7)->Benchmark_5_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_832);
		$tA8 = ((new Benchmark_6()));
		$fA1 = $tA8;
		($tA8)->Benchmark_6_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_833);
		$tA9 = ((new Benchmark_7()));
		$fA1 = $tA9;
		($tA9)->Benchmark_7_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_834);
		$tA10 = ((new Benchmark_8()));
		$fA1 = $tA10;
		($tA10)->Benchmark_8_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_835);
		$tA11 = ((new Benchmark_9()));
		$fA1 = $tA11;
		($tA11)->Benchmark_9_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_836);
		$tA12 = ((new Benchmark_10()));
		$fA1 = $tA12;
		($tA12)->Benchmark_10_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_837);
		$tA13 = ((new Benchmark_11()));
		$fA1 = $tA13;
		($tA13)->Benchmark_11_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_838);
		$tA14 = ((new Benchmark_12()));
		$fA1 = $tA14;
		($tA14)->Benchmark_12_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_839);
		$tA15 = ((new Benchmark_13()));
		$fA1 = $tA15;
		($tA15)->Benchmark_13_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_840);
		$tA16 = ((new Benchmark_14()));
		$fA1 = $tA16;
		($tA16)->Benchmark_14_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$lA2 = new JA_I(16384);
		$lA3 = new JA_I(16384);
		$fA0 = (Bootstrap::$STRINGLIT_841);
		$tA17 = ((new Benchmark_15()));
		$fA1 = $tA17;
		($tA17)->Benchmark_15_init___I_I_V($lA2, $lA3);
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$lA4 = new JA_B(1000000);
		$lA5 = new JA_S(1000000);
		$lA6 = new JA_C(1000000);
		$lA7 = new JA_I(1000000);
		$lA8 = new JA_F(1000000);
		$lA9 = new JA_D(1000000);
		$fA0 = (Bootstrap::$STRINGLIT_842);
		$tA18 = ((new Benchmark_16()));
		$fA1 = $tA18;
		($tA18)->Benchmark_16_init___B_V($lA4);
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_843);
		$tA19 = ((new Benchmark_17()));
		$fA1 = $tA19;
		($tA19)->Benchmark_17_init___S_V($lA5);
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_844);
		$tA20 = ((new Benchmark_18()));
		$fA1 = $tA20;
		($tA20)->Benchmark_18_init___C_V($lA6);
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_845);
		$tA21 = ((new Benchmark_19()));
		$fA1 = $tA21;
		($tA21)->Benchmark_19_init___I_V($lA7);
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_846);
		$tA22 = ((new Benchmark_20()));
		$fA1 = $tA22;
		($tA22)->Benchmark_20_init___F_V($lA8);
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_847);
		$tA23 = ((new Benchmark_21()));
		$fA1 = $tA23;
		($tA23)->Benchmark_21_init___D_V($lA9);
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_848);
		$tA24 = ((new Benchmark_22()));
		$fA1 = $tA24;
		($tA24)->Benchmark_22_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_849);
		$tA25 = ((new Benchmark_23()));
		$fA1 = $tA25;
		($tA25)->Benchmark_23_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_850);
		$tA26 = ((new Benchmark_24()));
		$fA1 = $tA26;
		($tA26)->Benchmark_24_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_851);
		$tA27 = ((new Benchmark_25()));
		$fA1 = $tA27;
		($tA27)->Benchmark_25_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_852);
		$tA28 = ((new Benchmark_26()));
		$fA1 = $tA28;
		($tA28)->Benchmark_26_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_853);
		$tA29 = ((new Benchmark_27()));
		$fA1 = $tA29;
		($tA29)->Benchmark_27_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_854);
		$tA30 = ((new Benchmark_28()));
		$fA1 = $tA30;
		($tA30)->Benchmark_28_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_855);
		$tA31 = ((new Benchmark_29()));
		$fA1 = $tA31;
		($tA31)->Benchmark_29_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_856);
		$tA32 = ((new Benchmark_30()));
		$fA1 = $tA32;
		($tA32)->Benchmark_30_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_857);
		$tA33 = ((new Benchmark_31()));
		$fA1 = $tA33;
		($tA33)->Benchmark_31_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_858);
		$tA34 = ((new Benchmark_32()));
		$fA1 = $tA34;
		($tA34)->Benchmark_32_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_859);
		$tA35 = ((new Benchmark_33()));
		$fA1 = $tA35;
		($tA35)->Benchmark_33_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_860);
		$tA36 = ((new Benchmark_34()));
		$fA1 = $tA36;
		($tA36)->Benchmark_34_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_861);
		$tA37 = ((new Benchmark_35()));
		$fA1 = $tA37;
		($tA37)->Benchmark_35_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		java_lang_System::gc__V();
		$fA0 = (Bootstrap::$STRINGLIT_862);
		$tA38 = ((new Benchmark_36()));
		$fA1 = $tA38;
		($tA38)->Benchmark_36_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$lA10 = new JA_L(100000, "[LBenchmark\$MyClass2;");
		$fA0 = (Bootstrap::$STRINGLIT_863);
		$tA39 = ((new Benchmark_37()));
		$fA1 = $tA39;
		($tA39)->Benchmark_37_init___LBenchmark_MyClass2__V($lA10);
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_864);
		$tA40 = ((new Benchmark_38()));
		$fA1 = $tA40;
		($tA40)->Benchmark_38_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$tA41 = (new JA_C(253));
		$fA0 = $tA41;
		($tA41)->set(0, 80);
		($fA0)->set(1, 75);
		($fA0)->set(2, 3);
		($fA0)->set(3, 4);
		($fA0)->set(4, 10);
		($fA0)->set(5, 3);
		($fA0)->set(6, 0);
		($fA0)->set(7, 0);
		($fA0)->set(8, 0);
		($fA0)->set(9, 0);
		($fA0)->set(10, 73);
		($fA0)->set(11, 158);
		($fA0)->set(12, 116);
		($fA0)->set(13, 72);
		($fA0)->set(14, 163);
		($fA0)->set(15, 28);
		($fA0)->set(16, 41);
		($fA0)->set(17, 28);
		($fA0)->set(18, 12);
		($fA0)->set(19, 0);
		($fA0)->set(20, 0);
		($fA0)->set(21, 0);
		($fA0)->set(22, 12);
		($fA0)->set(23, 0);
		($fA0)->set(24, 0);
		($fA0)->set(25, 0);
		($fA0)->set(26, 9);
		($fA0)->set(27, 0);
		($fA0)->set(28, 0);
		($fA0)->set(29, 0);
		($fA0)->set(30, 104);
		($fA0)->set(31, 101);
		($fA0)->set(32, 108);
		($fA0)->set(33, 108);
		($fA0)->set(34, 111);
		($fA0)->set(35, 46);
		($fA0)->set(36, 116);
		($fA0)->set(37, 120);
		($fA0)->set(38, 116);
		($fA0)->set(39, 72);
		($fA0)->set(40, 101);
		($fA0)->set(41, 108);
		($fA0)->set(42, 108);
		($fA0)->set(43, 111);
		($fA0)->set(44, 32);
		($fA0)->set(45, 87);
		($fA0)->set(46, 111);
		($fA0)->set(47, 114);
		($fA0)->set(48, 108);
		($fA0)->set(49, 100);
		($fA0)->set(50, 33);
		($fA0)->set(51, 80);
		($fA0)->set(52, 75);
		($fA0)->set(53, 3);
		($fA0)->set(54, 4);
		($fA0)->set(55, 20);
		($fA0)->set(56, 3);
		($fA0)->set(57, 0);
		($fA0)->set(58, 0);
		($fA0)->set(59, 8);
		($fA0)->set(60, 0);
		($fA0)->set(61, 53);
		($fA0)->set(62, 181);
		($fA0)->set(63, 116);
		($fA0)->set(64, 72);
		($fA0)->set(65, 170);
		($fA0)->set(66, 192);
		($fA0)->set(67, 105);
		($fA0)->set(68, 58);
		($fA0)->set(69, 29);
		($fA0)->set(70, 0);
		($fA0)->set(71, 0);
		($fA0)->set(72, 0);
		($fA0)->set(73, 56);
		($fA0)->set(74, 7);
		($fA0)->set(75, 0);
		($fA0)->set(76, 0);
		($fA0)->set(77, 10);
		($fA0)->set(78, 0);
		($fA0)->set(79, 0);
		($fA0)->set(80, 0);
		($fA0)->set(81, 104);
		($fA0)->set(82, 101);
		($fA0)->set(83, 108);
		($fA0)->set(84, 108);
		($fA0)->set(85, 111);
		($fA0)->set(86, 50);
		($fA0)->set(87, 46);
		($fA0)->set(88, 116);
		($fA0)->set(89, 120);
		($fA0)->set(90, 116);
		($fA0)->set(91, 243);
		($fA0)->set(92, 72);
		($fA0)->set(93, 205);
		($fA0)->set(94, 201);
		($fA0)->set(95, 201);
		($fA0)->set(96, 87);
		($fA0)->set(97, 8);
		($fA0)->set(98, 207);
		($fA0)->set(99, 47);
		($fA0)->set(100, 202);
		($fA0)->set(101, 73);
		($fA0)->set(102, 81);
		($fA0)->set(103, 28);
		($fA0)->set(104, 101);
		($fA0)->set(105, 143);
		($fA0)->set(106, 178);
		($fA0)->set(107, 71);
		($fA0)->set(108, 217);
		($fA0)->set(109, 163);
		($fA0)->set(110, 236);
		($fA0)->set(111, 81);
		($fA0)->set(112, 246);
		($fA0)->set(113, 40);
		($fA0)->set(114, 123);
		($fA0)->set(115, 148);
		($fA0)->set(116, 141);
		($fA0)->set(117, 159);
		($fA0)->set(118, 13);
		($fA0)->set(119, 0);
		($fA0)->set(120, 80);
		($fA0)->set(121, 75);
		($fA0)->set(122, 1);
		($fA0)->set(123, 2);
		($fA0)->set(124, 63);
		($fA0)->set(125, 3);
		($fA0)->set(126, 10);
		($fA0)->set(127, 3);
		($fA0)->set(128, 0);
		($fA0)->set(129, 0);
		($fA0)->set(130, 0);
		($fA0)->set(131, 0);
		($fA0)->set(132, 73);
		($fA0)->set(133, 158);
		($fA0)->set(134, 116);
		($fA0)->set(135, 72);
		($fA0)->set(136, 163);
		($fA0)->set(137, 28);
		($fA0)->set(138, 41);
		($fA0)->set(139, 28);
		($fA0)->set(140, 12);
		($fA0)->set(141, 0);
		($fA0)->set(142, 0);
		($fA0)->set(143, 0);
		($fA0)->set(144, 12);
		($fA0)->set(145, 0);
		($fA0)->set(146, 0);
		($fA0)->set(147, 0);
		($fA0)->set(148, 9);
		($fA0)->set(149, 0);
		($fA0)->set(150, 0);
		($fA0)->set(151, 0);
		($fA0)->set(152, 0);
		($fA0)->set(153, 0);
		($fA0)->set(154, 0);
		($fA0)->set(155, 0);
		($fA0)->set(156, 0);
		($fA0)->set(157, 0);
		($fA0)->set(158, 32);
		($fA0)->set(159, 128);
		($fA0)->set(160, 164);
		($fA0)->set(161, 129);
		($fA0)->set(162, 0);
		($fA0)->set(163, 0);
		($fA0)->set(164, 0);
		($fA0)->set(165, 0);
		($fA0)->set(166, 104);
		($fA0)->set(167, 101);
		($fA0)->set(168, 108);
		($fA0)->set(169, 108);
		($fA0)->set(170, 111);
		($fA0)->set(171, 46);
		($fA0)->set(172, 116);
		($fA0)->set(173, 120);
		($fA0)->set(174, 116);
		($fA0)->set(175, 80);
		($fA0)->set(176, 75);
		($fA0)->set(177, 1);
		($fA0)->set(178, 2);
		($fA0)->set(179, 63);
		($fA0)->set(180, 3);
		($fA0)->set(181, 20);
		($fA0)->set(182, 3);
		($fA0)->set(183, 0);
		($fA0)->set(184, 0);
		($fA0)->set(185, 8);
		($fA0)->set(186, 0);
		($fA0)->set(187, 53);
		($fA0)->set(188, 181);
		($fA0)->set(189, 116);
		($fA0)->set(190, 72);
		($fA0)->set(191, 170);
		($fA0)->set(192, 192);
		($fA0)->set(193, 105);
		($fA0)->set(194, 58);
		($fA0)->set(195, 29);
		($fA0)->set(196, 0);
		($fA0)->set(197, 0);
		($fA0)->set(198, 0);
		($fA0)->set(199, 56);
		($fA0)->set(200, 7);
		($fA0)->set(201, 0);
		($fA0)->set(202, 0);
		($fA0)->set(203, 10);
		($fA0)->set(204, 0);
		($fA0)->set(205, 0);
		($fA0)->set(206, 0);
		($fA0)->set(207, 0);
		($fA0)->set(208, 0);
		($fA0)->set(209, 0);
		($fA0)->set(210, 0);
		($fA0)->set(211, 0);
		($fA0)->set(212, 0);
		($fA0)->set(213, 32);
		($fA0)->set(214, 128);
		($fA0)->set(215, 164);
		($fA0)->set(216, 129);
		($fA0)->set(217, 51);
		($fA0)->set(218, 0);
		($fA0)->set(219, 0);
		($fA0)->set(220, 0);
		($fA0)->set(221, 104);
		($fA0)->set(222, 101);
		($fA0)->set(223, 108);
		($fA0)->set(224, 108);
		($fA0)->set(225, 111);
		($fA0)->set(226, 50);
		($fA0)->set(227, 46);
		($fA0)->set(228, 116);
		($fA0)->set(229, 120);
		($fA0)->set(230, 116);
		($fA0)->set(231, 80);
		($fA0)->set(232, 75);
		($fA0)->set(233, 5);
		($fA0)->set(234, 6);
		($fA0)->set(235, 0);
		($fA0)->set(236, 0);
		($fA0)->set(237, 0);
		($fA0)->set(238, 0);
		($fA0)->set(239, 2);
		($fA0)->set(240, 0);
		($fA0)->set(241, 2);
		($fA0)->set(242, 0);
		($fA0)->set(243, 111);
		($fA0)->set(244, 0);
		($fA0)->set(245, 0);
		($fA0)->set(246, 0);
		($fA0)->set(247, 120);
		($fA0)->set(248, 0);
		($fA0)->set(249, 0);
		($fA0)->set(250, 0);
		($fA0)->set(251, 0);
		($fA0)->set(252, 0);
		$lA11 = $fA0;
		$lA12 = new JA_B(($lA11)->length);
		$lI13 = 0;
		label_1:
		if ((($lI13 >= ($lA11)->length))) goto label_2;
		$lA12->set($lI13, (N::i2b((($lA11)->get($lI13)))));
		$lI13 = ((int)(($lI13 + 1)));
		goto label_1;
		label_2:
		$fA0 = (Bootstrap::$STRINGLIT_865);
		$tA294 = ((new Benchmark_39()));
		$fA1 = $tA294;
		($tA294)->Benchmark_39_init___B_V($lA12);
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_866);
		$tA295 = ((new Benchmark_40()));
		$fA1 = $tA295;
		($tA295)->Benchmark_40_init___B_V($lA12);
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$tA296 = ((new java_util_Random()));
		$fA0 = $tA296;
		($tA296)->java_util_Random_init__J_V(Int64::make(0, 0));
		$lA13 = $fA0;
		$lA14 = (new JA_B(65536));
		$lI15 = 0;
		label_3:
		if ((($lI15 >= ($lA14)->length))) goto label_4;
		($lA14)->set($lI15, (N::i2b(($lA13)->nextInt__I())));
		$lI15 = ((int)(($lI15 + 1)));
		goto label_3;
		label_4:
		$fA0 = (Bootstrap::$STRINGLIT_867);
		$tA297 = ((new Benchmark_41()));
		$fA1 = $tA297;
		($tA297)->Benchmark_41_init___B_V(($lA14));
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_868);
		$tA298 = ((new Benchmark_42()));
		$fA1 = $tA298;
		($tA298)->Benchmark_42_init___B_V(($lA14));
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_869);
		$tA299 = ((new Benchmark_43()));
		$fA1 = $tA299;
		($tA299)->Benchmark_43_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (Bootstrap::$STRINGLIT_870);
		$tA300 = ((new Benchmark_44()));
		$fA1 = $tA300;
		($tA300)->Benchmark_44_init___V();
		Benchmark::benchmark_Ljava_lang_String_LBenchmark_Task__V(($fA0), ($fA1));
		$fA0 = (java_lang_System::$__out);
		$tA301 = ((new java_lang_StringBuilder()));
		$fA1 = $tA301;
		($tA301)->java_lang_StringBuilder_init___V();
		($fA0)->println_Ljava_lang_String__V(($fA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_871)->append_D_Ljava_lang_StringBuilder_(Benchmark::$_totalTime)->toString__Ljava_lang_String_());
		return;
	}
	public static function benchmark_Ljava_lang_String_LBenchmark_Task__V(?java_lang_String $p0, ?Benchmark_Task $p1) {
		$G = 0;
		$lI4 = 0;
		$lI6 = 0;
		$fA0 = null;
		$fA1 = null;
		$lD4 = 0.0;
		$lD6 = 0.0;
		$lD8 = 0.0;
		$tA0 = null;
		$J__exception__ = null;
		while (true) {
			try {
				while (true) {
					switch ($G) {
						case 0:
							$fA0 = (java_lang_System::$__out);
							$tA0 = ((new java_lang_StringBuilder()));
							$fA1 = $tA0;
							($tA0)->java_lang_StringBuilder_init___V();
							($fA0)->print_Ljava_lang_String__V(($fA1)->append_Ljava_lang_String__Ljava_lang_StringBuilder_($p0)->append_Ljava_lang_String__Ljava_lang_StringBuilder_(Bootstrap::$STRINGLIT_872)->toString__Ljava_lang_String_());
							java_lang_System::$__out->flush__V();
							$G = 1;
							continue 2;
						case 1:
							com_jtransc_JTranscSystem::stamp__D();
							$lI4 = 0;
							$G = 2;
							continue 2;
						case 2:
							if ((($lI4 >= 10))) {
								$G = 3;
								continue 2;
							}
							$p1->run__I();
							$lI4 = ((int)(($lI4 + 1)));
							$G = 2;
							continue 2;
						case 3:
							java_lang_System::gc__V();
							$lD4 = com_jtransc_JTranscSystem::stamp__D();
							$lI6 = 0;
							$G = 4;
							continue 2;
						case 4:
							if ((($lI6 >= 10))) {
								$G = 5;
								continue 2;
							}
							$p1->run__I();
							$lI6 = ((int)(($lI6 + 1)));
							$G = 4;
							continue 2;
						case 5:
							$lD6 = com_jtransc_JTranscSystem::stamp__D();
							$lD8 = com_jtransc_JTranscSystem::elapsedTime_DD_D($lD4, $lD6);
							java_lang_System::$__out->println_D_V($lD8);
							Benchmark::$_totalTime = ((Benchmark::$_totalTime + $lD8));
							$G = 6;
							continue 2;
						case 6:
							$G = 7;
							continue 2;
						case 8:
							$fA0 = ($J__exception__);
							com_jtransc_io_JTranscConsole::log_Ljava_lang_Object__V((Bootstrap::$STRINGLIT_873));
							$G = 7;
							continue 2;
						case 7:
							return;
						default:
							break;
					}
				}
				return;
			}
			catch (WrappedThrowable $J__i__exception__) {
				$J__exception__ = $J__i__exception__->t;
				if ((((((($G >= 1)) && (($G < 6)))) && (($J__exception__) instanceof java_lang_Throwable)))) {
					$G = 8;
					continue 1;
				}
				throw $J__i__exception__;
			}
		}
		return;
	}
	public static function calc_II_I(int $p0, int $p1) {
		return ((int)(N::imul(((int)(($p0 + $p1))), ((int)(($p0 + $p1))))));
	}
	public function __construct($CLASS_ID = 649) {
		parent::__construct($CLASS_ID);
	}
	static public function SI() {
		Benchmark::$_totalTime = 0.0;
		Benchmark::Benchmark_clinit___V();
	}
}



class Bootstrap {
	static $STRINGLIT_0;
	static $STRINGLIT_1;
	static $STRINGLIT_2;
	static $STRINGLIT_3;
	static $STRINGLIT_4;
	static $STRINGLIT_5;
	static $STRINGLIT_6;
	static $STRINGLIT_7;
	static $STRINGLIT_8;
	static $STRINGLIT_9;
	static $STRINGLIT_10;
	static $STRINGLIT_11;
	static $STRINGLIT_12;
	static $STRINGLIT_13;
	static $STRINGLIT_14;
	static $STRINGLIT_15;
	static $STRINGLIT_16;
	static $STRINGLIT_17;
	static $STRINGLIT_18;
	static $STRINGLIT_19;
	static $STRINGLIT_20;
	static $STRINGLIT_21;
	static $STRINGLIT_22;
	static $STRINGLIT_23;
	static $STRINGLIT_24;
	static $STRINGLIT_25;
	static $STRINGLIT_26;
	static $STRINGLIT_27;
	static $STRINGLIT_28;
	static $STRINGLIT_29;
	static $STRINGLIT_30;
	static $STRINGLIT_31;
	static $STRINGLIT_32;
	static $STRINGLIT_33;
	static $STRINGLIT_34;
	static $STRINGLIT_35;
	static $STRINGLIT_36;
	static $STRINGLIT_37;
	static $STRINGLIT_38;
	static $STRINGLIT_39;
	static $STRINGLIT_40;
	static $STRINGLIT_41;
	static $STRINGLIT_42;
	static $STRINGLIT_43;
	static $STRINGLIT_44;
	static $STRINGLIT_45;
	static $STRINGLIT_46;
	static $STRINGLIT_47;
	static $STRINGLIT_48;
	static $STRINGLIT_49;
	static $STRINGLIT_50;
	static $STRINGLIT_51;
	static $STRINGLIT_52;
	static $STRINGLIT_53;
	static $STRINGLIT_54;
	static $STRINGLIT_55;
	static $STRINGLIT_56;
	static $STRINGLIT_57;
	static $STRINGLIT_58;
	static $STRINGLIT_59;
	static $STRINGLIT_60;
	static $STRINGLIT_61;
	static $STRINGLIT_62;
	static $STRINGLIT_63;
	static $STRINGLIT_64;
	static $STRINGLIT_65;
	static $STRINGLIT_66;
	static $STRINGLIT_67;
	static $STRINGLIT_68;
	static $STRINGLIT_69;
	static $STRINGLIT_70;
	static $STRINGLIT_71;
	static $STRINGLIT_72;
	static $STRINGLIT_73;
	static $STRINGLIT_74;
	static $STRINGLIT_75;
	static $STRINGLIT_76;
	static $STRINGLIT_77;
	static $STRINGLIT_78;
	static $STRINGLIT_79;
	static $STRINGLIT_80;
	static $STRINGLIT_81;
	static $STRINGLIT_82;
	static $STRINGLIT_83;
	static $STRINGLIT_84;
	static $STRINGLIT_85;
	static $STRINGLIT_86;
	static $STRINGLIT_87;
	static $STRINGLIT_88;
	static $STRINGLIT_89;
	static $STRINGLIT_90;
	static $STRINGLIT_91;
	static $STRINGLIT_92;
	static $STRINGLIT_93;
	static $STRINGLIT_94;
	static $STRINGLIT_95;
	static $STRINGLIT_96;
	static $STRINGLIT_97;
	static $STRINGLIT_98;
	static $STRINGLIT_99;
	static $STRINGLIT_100;
	static $STRINGLIT_101;
	static $STRINGLIT_102;
	static $STRINGLIT_103;
	static $STRINGLIT_104;
	static $STRINGLIT_105;
	static $STRINGLIT_106;
	static $STRINGLIT_107;
	static $STRINGLIT_108;
	static $STRINGLIT_109;
	static $STRINGLIT_110;
	static $STRINGLIT_111;
	static $STRINGLIT_112;
	static $STRINGLIT_113;
	static $STRINGLIT_114;
	static $STRINGLIT_115;
	static $STRINGLIT_116;
	static $STRINGLIT_117;
	static $STRINGLIT_118;
	static $STRINGLIT_119;
	static $STRINGLIT_120;
	static $STRINGLIT_121;
	static $STRINGLIT_122;
	static $STRINGLIT_123;
	static $STRINGLIT_124;
	static $STRINGLIT_125;
	static $STRINGLIT_126;
	static $STRINGLIT_127;
	static $STRINGLIT_128;
	static $STRINGLIT_129;
	static $STRINGLIT_130;
	static $STRINGLIT_131;
	static $STRINGLIT_132;
	static $STRINGLIT_133;
	static $STRINGLIT_134;
	static $STRINGLIT_135;
	static $STRINGLIT_136;
	static $STRINGLIT_137;
	static $STRINGLIT_138;
	static $STRINGLIT_139;
	static $STRINGLIT_140;
	static $STRINGLIT_141;
	static $STRINGLIT_142;
	static $STRINGLIT_143;
	static $STRINGLIT_144;
	static $STRINGLIT_145;
	static $STRINGLIT_146;
	static $STRINGLIT_147;
	static $STRINGLIT_148;
	static $STRINGLIT_149;
	static $STRINGLIT_150;
	static $STRINGLIT_151;
	static $STRINGLIT_152;
	static $STRINGLIT_153;
	static $STRINGLIT_154;
	static $STRINGLIT_155;
	static $STRINGLIT_156;
	static $STRINGLIT_157;
	static $STRINGLIT_158;
	static $STRINGLIT_159;
	static $STRINGLIT_160;
	static $STRINGLIT_161;
	static $STRINGLIT_162;
	static $STRINGLIT_163;
	static $STRINGLIT_164;
	static $STRINGLIT_165;
	static $STRINGLIT_166;
	static $STRINGLIT_167;
	static $STRINGLIT_168;
	static $STRINGLIT_169;
	static $STRINGLIT_170;
	static $STRINGLIT_171;
	static $STRINGLIT_172;
	static $STRINGLIT_173;
	static $STRINGLIT_174;
	static $STRINGLIT_175;
	static $STRINGLIT_176;
	static $STRINGLIT_177;
	static $STRINGLIT_178;
	static $STRINGLIT_179;
	static $STRINGLIT_180;
	static $STRINGLIT_181;
	static $STRINGLIT_182;
	static $STRINGLIT_183;
	static $STRINGLIT_184;
	static $STRINGLIT_185;
	static $STRINGLIT_186;
	static $STRINGLIT_187;
	static $STRINGLIT_188;
	static $STRINGLIT_189;
	static $STRINGLIT_190;
	static $STRINGLIT_191;
	static $STRINGLIT_192;
	static $STRINGLIT_193;
	static $STRINGLIT_194;
	static $STRINGLIT_195;
	static $STRINGLIT_196;
	static $STRINGLIT_197;
	static $STRINGLIT_198;
	static $STRINGLIT_199;
	static $STRINGLIT_200;
	static $STRINGLIT_201;
	static $STRINGLIT_202;
	static $STRINGLIT_203;
	static $STRINGLIT_204;
	static $STRINGLIT_205;
	static $STRINGLIT_206;
	static $STRINGLIT_207;
	static $STRINGLIT_208;
	static $STRINGLIT_209;
	static $STRINGLIT_210;
	static $STRINGLIT_211;
	static $STRINGLIT_212;
	static $STRINGLIT_213;
	static $STRINGLIT_214;
	static $STRINGLIT_215;
	static $STRINGLIT_216;
	static $STRINGLIT_217;
	static $STRINGLIT_218;
	static $STRINGLIT_219;
	static $STRINGLIT_220;
	static $STRINGLIT_221;
	static $STRINGLIT_222;
	static $STRINGLIT_223;
	static $STRINGLIT_224;
	static $STRINGLIT_225;
	static $STRINGLIT_226;
	static $STRINGLIT_227;
	static $STRINGLIT_228;
	static $STRINGLIT_229;
	static $STRINGLIT_230;
	static $STRINGLIT_231;
	static $STRINGLIT_232;
	static $STRINGLIT_233;
	static $STRINGLIT_234;
	static $STRINGLIT_235;
	static $STRINGLIT_236;
	static $STRINGLIT_237;
	static $STRINGLIT_238;
	static $STRINGLIT_239;
	static $STRINGLIT_240;
	static $STRINGLIT_241;
	static $STRINGLIT_242;
	static $STRINGLIT_243;
	static $STRINGLIT_244;
	static $STRINGLIT_245;
	static $STRINGLIT_246;
	static $STRINGLIT_247;
	static $STRINGLIT_248;
	static $STRINGLIT_249;
	static $STRINGLIT_250;
	static $STRINGLIT_251;
	static $STRINGLIT_252;
	static $STRINGLIT_253;
	static $STRINGLIT_254;
	static $STRINGLIT_255;
	static $STRINGLIT_256;
	static $STRINGLIT_257;
	static $STRINGLIT_258;
	static $STRINGLIT_259;
	static $STRINGLIT_260;
	static $STRINGLIT_261;
	static $STRINGLIT_262;
	static $STRINGLIT_263;
	static $STRINGLIT_264;
	static $STRINGLIT_265;
	static $STRINGLIT_266;
	static $STRINGLIT_267;
	static $STRINGLIT_268;
	static $STRINGLIT_269;
	static $STRINGLIT_270;
	static $STRINGLIT_271;
	static $STRINGLIT_272;
	static $STRINGLIT_273;
	static $STRINGLIT_274;
	static $STRINGLIT_275;
	static $STRINGLIT_276;
	static $STRINGLIT_277;
	static $STRINGLIT_278;
	static $STRINGLIT_279;
	static $STRINGLIT_280;
	static $STRINGLIT_281;
	static $STRINGLIT_282;
	static $STRINGLIT_283;
	static $STRINGLIT_284;
	static $STRINGLIT_285;
	static $STRINGLIT_286;
	static $STRINGLIT_287;
	static $STRINGLIT_288;
	static $STRINGLIT_289;
	static $STRINGLIT_290;
	static $STRINGLIT_291;
	static $STRINGLIT_292;
	static $STRINGLIT_293;
	static $STRINGLIT_294;
	static $STRINGLIT_295;
	static $STRINGLIT_296;
	static $STRINGLIT_297;
	static $STRINGLIT_298;
	static $STRINGLIT_299;
	static $STRINGLIT_300;
	static $STRINGLIT_301;
	static $STRINGLIT_302;
	static $STRINGLIT_303;
	static $STRINGLIT_304;
	static $STRINGLIT_305;
	static $STRINGLIT_306;
	static $STRINGLIT_307;
	static $STRINGLIT_308;
	static $STRINGLIT_309;
	static $STRINGLIT_310;
	static $STRINGLIT_311;
	static $STRINGLIT_312;
	static $STRINGLIT_313;
	static $STRINGLIT_314;
	static $STRINGLIT_315;
	static $STRINGLIT_316;
	static $STRINGLIT_317;
	static $STRINGLIT_318;
	static $STRINGLIT_319;
	static $STRINGLIT_320;
	static $STRINGLIT_321;
	static $STRINGLIT_322;
	static $STRINGLIT_323;
	static $STRINGLIT_324;
	static $STRINGLIT_325;
	static $STRINGLIT_326;
	static $STRINGLIT_327;
	static $STRINGLIT_328;
	static $STRINGLIT_329;
	static $STRINGLIT_330;
	static $STRINGLIT_331;
	static $STRINGLIT_332;
	static $STRINGLIT_333;
	static $STRINGLIT_334;
	static $STRINGLIT_335;
	static $STRINGLIT_336;
	static $STRINGLIT_337;
	static $STRINGLIT_338;
	static $STRINGLIT_339;
	static $STRINGLIT_340;
	static $STRINGLIT_341;
	static $STRINGLIT_342;
	static $STRINGLIT_343;
	static $STRINGLIT_344;
	static $STRINGLIT_345;
	static $STRINGLIT_346;
	static $STRINGLIT_347;
	static $STRINGLIT_348;
	static $STRINGLIT_349;
	static $STRINGLIT_350;
	static $STRINGLIT_351;
	static $STRINGLIT_352;
	static $STRINGLIT_353;
	static $STRINGLIT_354;
	static $STRINGLIT_355;
	static $STRINGLIT_356;
	static $STRINGLIT_357;
	static $STRINGLIT_358;
	static $STRINGLIT_359;
	static $STRINGLIT_360;
	static $STRINGLIT_361;
	static $STRINGLIT_362;
	static $STRINGLIT_363;
	static $STRINGLIT_364;
	static $STRINGLIT_365;
	static $STRINGLIT_366;
	static $STRINGLIT_367;
	static $STRINGLIT_368;
	static $STRINGLIT_369;
	static $STRINGLIT_370;
	static $STRINGLIT_371;
	static $STRINGLIT_372;
	static $STRINGLIT_373;
	static $STRINGLIT_374;
	static $STRINGLIT_375;
	static $STRINGLIT_376;
	static $STRINGLIT_377;
	static $STRINGLIT_378;
	static $STRINGLIT_379;
	static $STRINGLIT_380;
	static $STRINGLIT_381;
	static $STRINGLIT_382;
	static $STRINGLIT_383;
	static $STRINGLIT_384;
	static $STRINGLIT_385;
	static $STRINGLIT_386;
	static $STRINGLIT_387;
	static $STRINGLIT_388;
	static $STRINGLIT_389;
	static $STRINGLIT_390;
	static $STRINGLIT_391;
	static $STRINGLIT_392;
	static $STRINGLIT_393;
	static $STRINGLIT_394;
	static $STRINGLIT_395;
	static $STRINGLIT_396;
	static $STRINGLIT_397;
	static $STRINGLIT_398;
	static $STRINGLIT_399;
	static $STRINGLIT_400;
	static $STRINGLIT_401;
	static $STRINGLIT_402;
	static $STRINGLIT_403;
	static $STRINGLIT_404;
	static $STRINGLIT_405;
	static $STRINGLIT_406;
	static $STRINGLIT_407;
	static $STRINGLIT_408;
	static $STRINGLIT_409;
	static $STRINGLIT_410;
	static $STRINGLIT_411;
	static $STRINGLIT_412;
	static $STRINGLIT_413;
	static $STRINGLIT_414;
	static $STRINGLIT_415;
	static $STRINGLIT_416;
	static $STRINGLIT_417;
	static $STRINGLIT_418;
	static $STRINGLIT_419;
	static $STRINGLIT_420;
	static $STRINGLIT_421;
	static $STRINGLIT_422;
	static $STRINGLIT_423;
	static $STRINGLIT_424;
	static $STRINGLIT_425;
	static $STRINGLIT_426;
	static $STRINGLIT_427;
	static $STRINGLIT_428;
	static $STRINGLIT_429;
	static $STRINGLIT_430;
	static $STRINGLIT_431;
	static $STRINGLIT_432;
	static $STRINGLIT_433;
	static $STRINGLIT_434;
	static $STRINGLIT_435;
	static $STRINGLIT_436;
	static $STRINGLIT_437;
	static $STRINGLIT_438;
	static $STRINGLIT_439;
	static $STRINGLIT_440;
	static $STRINGLIT_441;
	static $STRINGLIT_442;
	static $STRINGLIT_443;
	static $STRINGLIT_444;
	static $STRINGLIT_445;
	static $STRINGLIT_446;
	static $STRINGLIT_447;
	static $STRINGLIT_448;
	static $STRINGLIT_449;
	static $STRINGLIT_450;
	static $STRINGLIT_451;
	static $STRINGLIT_452;
	static $STRINGLIT_453;
	static $STRINGLIT_454;
	static $STRINGLIT_455;
	static $STRINGLIT_456;
	static $STRINGLIT_457;
	static $STRINGLIT_458;
	static $STRINGLIT_459;
	static $STRINGLIT_460;
	static $STRINGLIT_461;
	static $STRINGLIT_462;
	static $STRINGLIT_463;
	static $STRINGLIT_464;
	static $STRINGLIT_465;
	static $STRINGLIT_466;
	static $STRINGLIT_467;
	static $STRINGLIT_468;
	static $STRINGLIT_469;
	static $STRINGLIT_470;
	static $STRINGLIT_471;
	static $STRINGLIT_472;
	static $STRINGLIT_473;
	static $STRINGLIT_474;
	static $STRINGLIT_475;
	static $STRINGLIT_476;
	static $STRINGLIT_477;
	static $STRINGLIT_478;
	static $STRINGLIT_479;
	static $STRINGLIT_480;
	static $STRINGLIT_481;
	static $STRINGLIT_482;
	static $STRINGLIT_483;
	static $STRINGLIT_484;
	static $STRINGLIT_485;
	static $STRINGLIT_486;
	static $STRINGLIT_487;
	static $STRINGLIT_488;
	static $STRINGLIT_489;
	static $STRINGLIT_490;
	static $STRINGLIT_491;
	static $STRINGLIT_492;
	static $STRINGLIT_493;
	static $STRINGLIT_494;
	static $STRINGLIT_495;
	static $STRINGLIT_496;
	static $STRINGLIT_497;
	static $STRINGLIT_498;
	static $STRINGLIT_499;
	static $STRINGLIT_500;
	static $STRINGLIT_501;
	static $STRINGLIT_502;
	static $STRINGLIT_503;
	static $STRINGLIT_504;
	static $STRINGLIT_505;
	static $STRINGLIT_506;
	static $STRINGLIT_507;
	static $STRINGLIT_508;
	static $STRINGLIT_509;
	static $STRINGLIT_510;
	static $STRINGLIT_511;
	static $STRINGLIT_512;
	static $STRINGLIT_513;
	static $STRINGLIT_514;
	static $STRINGLIT_515;
	static $STRINGLIT_516;
	static $STRINGLIT_517;
	static $STRINGLIT_518;
	static $STRINGLIT_519;
	static $STRINGLIT_520;
	static $STRINGLIT_521;
	static $STRINGLIT_522;
	static $STRINGLIT_523;
	static $STRINGLIT_524;
	static $STRINGLIT_525;
	static $STRINGLIT_526;
	static $STRINGLIT_527;
	static $STRINGLIT_528;
	static $STRINGLIT_529;
	static $STRINGLIT_530;
	static $STRINGLIT_531;
	static $STRINGLIT_532;
	static $STRINGLIT_533;
	static $STRINGLIT_534;
	static $STRINGLIT_535;
	static $STRINGLIT_536;
	static $STRINGLIT_537;
	static $STRINGLIT_538;
	static $STRINGLIT_539;
	static $STRINGLIT_540;
	static $STRINGLIT_541;
	static $STRINGLIT_542;
	static $STRINGLIT_543;
	static $STRINGLIT_544;
	static $STRINGLIT_545;
	static $STRINGLIT_546;
	static $STRINGLIT_547;
	static $STRINGLIT_548;
	static $STRINGLIT_549;
	static $STRINGLIT_550;
	static $STRINGLIT_551;
	static $STRINGLIT_552;
	static $STRINGLIT_553;
	static $STRINGLIT_554;
	static $STRINGLIT_555;
	static $STRINGLIT_556;
	static $STRINGLIT_557;
	static $STRINGLIT_558;
	static $STRINGLIT_559;
	static $STRINGLIT_560;
	static $STRINGLIT_561;
	static $STRINGLIT_562;
	static $STRINGLIT_563;
	static $STRINGLIT_564;
	static $STRINGLIT_565;
	static $STRINGLIT_566;
	static $STRINGLIT_567;
	static $STRINGLIT_568;
	static $STRINGLIT_569;
	static $STRINGLIT_570;
	static $STRINGLIT_571;
	static $STRINGLIT_572;
	static $STRINGLIT_573;
	static $STRINGLIT_574;
	static $STRINGLIT_575;
	static $STRINGLIT_576;
	static $STRINGLIT_577;
	static $STRINGLIT_578;
	static $STRINGLIT_579;
	static $STRINGLIT_580;
	static $STRINGLIT_581;
	static $STRINGLIT_582;
	static $STRINGLIT_583;
	static $STRINGLIT_584;
	static $STRINGLIT_585;
	static $STRINGLIT_586;
	static $STRINGLIT_587;
	static $STRINGLIT_588;
	static $STRINGLIT_589;
	static $STRINGLIT_590;
	static $STRINGLIT_591;
	static $STRINGLIT_592;
	static $STRINGLIT_593;
	static $STRINGLIT_594;
	static $STRINGLIT_595;
	static $STRINGLIT_596;
	static $STRINGLIT_597;
	static $STRINGLIT_598;
	static $STRINGLIT_599;
	static $STRINGLIT_600;
	static $STRINGLIT_601;
	static $STRINGLIT_602;
	static $STRINGLIT_603;
	static $STRINGLIT_604;
	static $STRINGLIT_605;
	static $STRINGLIT_606;
	static $STRINGLIT_607;
	static $STRINGLIT_608;
	static $STRINGLIT_609;
	static $STRINGLIT_610;
	static $STRINGLIT_611;
	static $STRINGLIT_612;
	static $STRINGLIT_613;
	static $STRINGLIT_614;
	static $STRINGLIT_615;
	static $STRINGLIT_616;
	static $STRINGLIT_617;
	static $STRINGLIT_618;
	static $STRINGLIT_619;
	static $STRINGLIT_620;
	static $STRINGLIT_621;
	static $STRINGLIT_622;
	static $STRINGLIT_623;
	static $STRINGLIT_624;
	static $STRINGLIT_625;
	static $STRINGLIT_626;
	static $STRINGLIT_627;
	static $STRINGLIT_628;
	static $STRINGLIT_629;
	static $STRINGLIT_630;
	static $STRINGLIT_631;
	static $STRINGLIT_632;
	static $STRINGLIT_633;
	static $STRINGLIT_634;
	static $STRINGLIT_635;
	static $STRINGLIT_636;
	static $STRINGLIT_637;
	static $STRINGLIT_638;
	static $STRINGLIT_639;
	static $STRINGLIT_640;
	static $STRINGLIT_641;
	static $STRINGLIT_642;
	static $STRINGLIT_643;
	static $STRINGLIT_644;
	static $STRINGLIT_645;
	static $STRINGLIT_646;
	static $STRINGLIT_647;
	static $STRINGLIT_648;
	static $STRINGLIT_649;
	static $STRINGLIT_650;
	static $STRINGLIT_651;
	static $STRINGLIT_652;
	static $STRINGLIT_653;
	static $STRINGLIT_654;
	static $STRINGLIT_655;
	static $STRINGLIT_656;
	static $STRINGLIT_657;
	static $STRINGLIT_658;
	static $STRINGLIT_659;
	static $STRINGLIT_660;
	static $STRINGLIT_661;
	static $STRINGLIT_662;
	static $STRINGLIT_663;
	static $STRINGLIT_664;
	static $STRINGLIT_665;
	static $STRINGLIT_666;
	static $STRINGLIT_667;
	static $STRINGLIT_668;
	static $STRINGLIT_669;
	static $STRINGLIT_670;
	static $STRINGLIT_671;
	static $STRINGLIT_672;
	static $STRINGLIT_673;
	static $STRINGLIT_674;
	static $STRINGLIT_675;
	static $STRINGLIT_676;
	static $STRINGLIT_677;
	static $STRINGLIT_678;
	static $STRINGLIT_679;
	static $STRINGLIT_680;
	static $STRINGLIT_681;
	static $STRINGLIT_682;
	static $STRINGLIT_683;
	static $STRINGLIT_684;
	static $STRINGLIT_685;
	static $STRINGLIT_686;
	static $STRINGLIT_687;
	static $STRINGLIT_688;
	static $STRINGLIT_689;
	static $STRINGLIT_690;
	static $STRINGLIT_691;
	static $STRINGLIT_692;
	static $STRINGLIT_693;
	static $STRINGLIT_694;
	static $STRINGLIT_695;
	static $STRINGLIT_696;
	static $STRINGLIT_697;
	static $STRINGLIT_698;
	static $STRINGLIT_699;
	static $STRINGLIT_700;
	static $STRINGLIT_701;
	static $STRINGLIT_702;
	static $STRINGLIT_703;
	static $STRINGLIT_704;
	static $STRINGLIT_705;
	static $STRINGLIT_706;
	static $STRINGLIT_707;
	static $STRINGLIT_708;
	static $STRINGLIT_709;
	static $STRINGLIT_710;
	static $STRINGLIT_711;
	static $STRINGLIT_712;
	static $STRINGLIT_713;
	static $STRINGLIT_714;
	static $STRINGLIT_715;
	static $STRINGLIT_716;
	static $STRINGLIT_717;
	static $STRINGLIT_718;
	static $STRINGLIT_719;
	static $STRINGLIT_720;
	static $STRINGLIT_721;
	static $STRINGLIT_722;
	static $STRINGLIT_723;
	static $STRINGLIT_724;
	static $STRINGLIT_725;
	static $STRINGLIT_726;
	static $STRINGLIT_727;
	static $STRINGLIT_728;
	static $STRINGLIT_729;
	static $STRINGLIT_730;
	static $STRINGLIT_731;
	static $STRINGLIT_732;
	static $STRINGLIT_733;
	static $STRINGLIT_734;
	static $STRINGLIT_735;
	static $STRINGLIT_736;
	static $STRINGLIT_737;
	static $STRINGLIT_738;
	static $STRINGLIT_739;
	static $STRINGLIT_740;
	static $STRINGLIT_741;
	static $STRINGLIT_742;
	static $STRINGLIT_743;
	static $STRINGLIT_744;
	static $STRINGLIT_745;
	static $STRINGLIT_746;
	static $STRINGLIT_747;
	static $STRINGLIT_748;
	static $STRINGLIT_749;
	static $STRINGLIT_750;
	static $STRINGLIT_751;
	static $STRINGLIT_752;
	static $STRINGLIT_753;
	static $STRINGLIT_754;
	static $STRINGLIT_755;
	static $STRINGLIT_756;
	static $STRINGLIT_757;
	static $STRINGLIT_758;
	static $STRINGLIT_759;
	static $STRINGLIT_760;
	static $STRINGLIT_761;
	static $STRINGLIT_762;
	static $STRINGLIT_763;
	static $STRINGLIT_764;
	static $STRINGLIT_765;
	static $STRINGLIT_766;
	static $STRINGLIT_767;
	static $STRINGLIT_768;
	static $STRINGLIT_769;
	static $STRINGLIT_770;
	static $STRINGLIT_771;
	static $STRINGLIT_772;
	static $STRINGLIT_773;
	static $STRINGLIT_774;
	static $STRINGLIT_775;
	static $STRINGLIT_776;
	static $STRINGLIT_777;
	static $STRINGLIT_778;
	static $STRINGLIT_779;
	static $STRINGLIT_780;
	static $STRINGLIT_781;
	static $STRINGLIT_782;
	static $STRINGLIT_783;
	static $STRINGLIT_784;
	static $STRINGLIT_785;
	static $STRINGLIT_786;
	static $STRINGLIT_787;
	static $STRINGLIT_788;
	static $STRINGLIT_789;
	static $STRINGLIT_790;
	static $STRINGLIT_791;
	static $STRINGLIT_792;
	static $STRINGLIT_793;
	static $STRINGLIT_794;
	static $STRINGLIT_795;
	static $STRINGLIT_796;
	static $STRINGLIT_797;
	static $STRINGLIT_798;
	static $STRINGLIT_799;
	static $STRINGLIT_800;
	static $STRINGLIT_801;
	static $STRINGLIT_802;
	static $STRINGLIT_803;
	static $STRINGLIT_804;
	static $STRINGLIT_805;
	static $STRINGLIT_806;
	static $STRINGLIT_807;
	static $STRINGLIT_808;
	static $STRINGLIT_809;
	static $STRINGLIT_810;
	static $STRINGLIT_811;
	static $STRINGLIT_812;
	static $STRINGLIT_813;
	static $STRINGLIT_814;
	static $STRINGLIT_815;
	static $STRINGLIT_816;
	static $STRINGLIT_817;
	static $STRINGLIT_818;
	static $STRINGLIT_819;
	static $STRINGLIT_820;
	static $STRINGLIT_821;
	static $STRINGLIT_822;
	static $STRINGLIT_823;
	static $STRINGLIT_824;
	static $STRINGLIT_825;
	static $STRINGLIT_826;
	static $STRINGLIT_827;
	static $STRINGLIT_828;
	static $STRINGLIT_829;
	static $STRINGLIT_830;
	static $STRINGLIT_831;
	static $STRINGLIT_832;
	static $STRINGLIT_833;
	static $STRINGLIT_834;
	static $STRINGLIT_835;
	static $STRINGLIT_836;
	static $STRINGLIT_837;
	static $STRINGLIT_838;
	static $STRINGLIT_839;
	static $STRINGLIT_840;
	static $STRINGLIT_841;
	static $STRINGLIT_842;
	static $STRINGLIT_843;
	static $STRINGLIT_844;
	static $STRINGLIT_845;
	static $STRINGLIT_846;
	static $STRINGLIT_847;
	static $STRINGLIT_848;
	static $STRINGLIT_849;
	static $STRINGLIT_850;
	static $STRINGLIT_851;
	static $STRINGLIT_852;
	static $STRINGLIT_853;
	static $STRINGLIT_854;
	static $STRINGLIT_855;
	static $STRINGLIT_856;
	static $STRINGLIT_857;
	static $STRINGLIT_858;
	static $STRINGLIT_859;
	static $STRINGLIT_860;
	static $STRINGLIT_861;
	static $STRINGLIT_862;
	static $STRINGLIT_863;
	static $STRINGLIT_864;
	static $STRINGLIT_865;
	static $STRINGLIT_866;
	static $STRINGLIT_867;
	static $STRINGLIT_868;
	static $STRINGLIT_869;
	static $STRINGLIT_870;
	static $STRINGLIT_871;
	static $STRINGLIT_872;
	static $STRINGLIT_873;
	static public function __initStrings() {
		Bootstrap::$STRINGLIT_0 = N::str("@");
		Bootstrap::$STRINGLIT_1 = N::str(")");
		Bootstrap::$STRINGLIT_2 = N::str("name == null");
		Bootstrap::$STRINGLIT_3 = N::str("cl == null");
		Bootstrap::$STRINGLIT_4 = N::str("[]");
		Bootstrap::$STRINGLIT_5 = N::str("(this Collection)");
		Bootstrap::$STRINGLIT_6 = N::str(", ");
		Bootstrap::$STRINGLIT_7 = N::str("=");
		Bootstrap::$STRINGLIT_8 = N::str("key == null");
		Bootstrap::$STRINGLIT_9 = N::str("value == null");
		Bootstrap::$STRINGLIT_10 = N::str("(this Map)");
		Bootstrap::$STRINGLIT_11 = N::str("threshold");
		Bootstrap::$STRINGLIT_12 = N::str("loadFactor");
		Bootstrap::$STRINGLIT_13 = N::str("Simd.MutableFloat32x4(");
		Bootstrap::$STRINGLIT_14 = N::str("Exception:");
		Bootstrap::$STRINGLIT_15 = N::str("\x09at ");
		Bootstrap::$STRINGLIT_16 = N::str("Supressed:");
		Bootstrap::$STRINGLIT_17 = N::str("Cause:");
		Bootstrap::$STRINGLIT_18 = N::str("DummyClass");
		Bootstrap::$STRINGLIT_19 = N::str("dummyMethod");
		Bootstrap::$STRINGLIT_20 = N::str("DummyClass.java");
		Bootstrap::$STRINGLIT_21 = N::str("need dictionary");
		Bootstrap::$STRINGLIT_22 = N::str("stream end");
		Bootstrap::$STRINGLIT_23 = N::str("");
		Bootstrap::$STRINGLIT_24 = N::str("file error");
		Bootstrap::$STRINGLIT_25 = N::str("stream error");
		Bootstrap::$STRINGLIT_26 = N::str("data error");
		Bootstrap::$STRINGLIT_27 = N::str("insufficient memory");
		Bootstrap::$STRINGLIT_28 = N::str("buffer error");
		Bootstrap::$STRINGLIT_29 = N::str("incompatible version");
		Bootstrap::$STRINGLIT_30 = N::str(": ");
		Bootstrap::$STRINGLIT_31 = N::str("capacity < 0: ");
		Bootstrap::$STRINGLIT_32 = N::str("]");
		Bootstrap::$STRINGLIT_33 = N::str(",capacity=");
		Bootstrap::$STRINGLIT_34 = N::str(",limit=");
		Bootstrap::$STRINGLIT_35 = N::str("[position=");
		Bootstrap::$STRINGLIT_36 = N::str(", limit=");
		Bootstrap::$STRINGLIT_37 = N::str("index=");
		Bootstrap::$STRINGLIT_38 = N::str(", remaining()=");
		Bootstrap::$STRINGLIT_39 = N::str("BIG_ENDIAN");
		Bootstrap::$STRINGLIT_40 = N::str("LITTLE_ENDIAN");
		Bootstrap::$STRINGLIT_41 = N::str(", arrayOffset=");
		Bootstrap::$STRINGLIT_42 = N::str(", capacity=");
		Bootstrap::$STRINGLIT_43 = N::str("backingArray.length=");
		Bootstrap::$STRINGLIT_44 = N::str("hello");
		Bootstrap::$STRINGLIT_45 = N::str("\x0a");
		Bootstrap::$STRINGLIT_46 = N::str("HOME");
		Bootstrap::$STRINGLIT_47 = N::str("/tmp");
		Bootstrap::$STRINGLIT_48 = N::str("en");
		Bootstrap::$STRINGLIT_49 = N::str("TMPDIR");
		Bootstrap::$STRINGLIT_50 = N::str("TEMP");
		Bootstrap::$STRINGLIT_51 = N::str("TMP");
		Bootstrap::$STRINGLIT_52 = N::str("/");
		Bootstrap::$STRINGLIT_53 = N::str("\\");
		Bootstrap::$STRINGLIT_54 = N::str("UTF-8");
		Bootstrap::$STRINGLIT_55 = N::str(":");
		Bootstrap::$STRINGLIT_56 = N::str("USERNAME");
		Bootstrap::$STRINGLIT_57 = N::str("USER");
		Bootstrap::$STRINGLIT_58 = N::str("username");
		Bootstrap::$STRINGLIT_59 = N::str("US");
		Bootstrap::$STRINGLIT_60 = N::str("; regionLength=");
		Bootstrap::$STRINGLIT_61 = N::str("; regionStart=");
		Bootstrap::$STRINGLIT_62 = N::str("length=");
		Bootstrap::$STRINGLIT_63 = N::str(",maxpri=");
		Bootstrap::$STRINGLIT_64 = N::str("[name=");
		Bootstrap::$STRINGLIT_65 = N::str(",");
		Bootstrap::$STRINGLIT_66 = N::str("Thread[");
		Bootstrap::$STRINGLIT_67 = N::str(",]");
		Bootstrap::$STRINGLIT_68 = N::str("UTF-16BE");
		Bootstrap::$STRINGLIT_69 = N::str("UnicodeBigUnmarked");
		Bootstrap::$STRINGLIT_70 = N::str("X-UTF-16BE");
		Bootstrap::$STRINGLIT_71 = N::str("ISO-10646-UCS-2");
		Bootstrap::$STRINGLIT_72 = N::str("ISO-8859-1");
		Bootstrap::$STRINGLIT_73 = N::str("819");
		Bootstrap::$STRINGLIT_74 = N::str("ISO8859-1");
		Bootstrap::$STRINGLIT_75 = N::str("L1");
		Bootstrap::$STRINGLIT_76 = N::str("ISO_8859-1:1987");
		Bootstrap::$STRINGLIT_77 = N::str("ISO_8859-1");
		Bootstrap::$STRINGLIT_78 = N::str("8859_1");
		Bootstrap::$STRINGLIT_79 = N::str("ISO-IR-100");
		Bootstrap::$STRINGLIT_80 = N::str("LATIN1");
		Bootstrap::$STRINGLIT_81 = N::str("CP819");
		Bootstrap::$STRINGLIT_82 = N::str("ISO8859_1");
		Bootstrap::$STRINGLIT_83 = N::str("IBM819");
		Bootstrap::$STRINGLIT_84 = N::str("ISO_8859_1");
		Bootstrap::$STRINGLIT_85 = N::str("IBM-819");
		Bootstrap::$STRINGLIT_86 = N::str("CSISOLATIN1");
		Bootstrap::$STRINGLIT_87 = N::str("\0\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#\$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\x7f\xc2\x80\xc2\x81\xc2\x82\xc2\x83\xc2\x84\xc2\x85\xc2\x86\xc2\x87\xc2\x88\xc2\x89\xc2\x8a\xc2\x8b\xc2\x8c\xc2\x8d\xc2\x8e\xc2\x8f\xc2\x90\xc2\x91\xc2\x92\xc2\x93\xc2\x94\xc2\x95\xc2\x96\xc2\x97\xc2\x98\xc2\x99\xc2\x9a\xc2\x9b\xc2\x9c\xc2\x9d\xc2\x9e\xc2\x9f\xc2\xa0\xc2\xa1\xc2\xa2\xc2\xa3\xc2\xa4\xc2\xa5\xc2\xa6\xc2\xa7\xc2\xa8\xc2\xa9\xc2\xaa\xc2\xab\xc2\xac\xc2\xad\xc2\xae\xc2\xaf\xc2\xb0\xc2\xb1\xc2\xb2\xc2\xb3\xc2\xb4\xc2\xb5\xc2\xb6\xc2\xb7\xc2\xb8\xc2\xb9\xc2\xba\xc2\xbb\xc2\xbc\xc2\xbd\xc2\xbe\xc2\xbf\xc3\x80\xc3\x81\xc3\x82\xc3\x83\xc3\x84\xc3\x85\xc3\x86\xc3\x87\xc3\x88\xc3\x89\xc3\x8a\xc3\x8b\xc3\x8c\xc3\x8d\xc3\x8e\xc3\x8f\xc3\x90\xc3\x91\xc3\x92\xc3\x93\xc3\x94\xc3\x95\xc3\x96\xc3\x97\xc3\x98\xc3\x99\xc3\x9a\xc3\x9b\xc3\x9c\xc3\x9d\xc3\x9e\xc3\x9f\xc3\xa0\xc3\xa1\xc3\xa2\xc3\xa3\xc3\xa4\xc3\xa5\xc3\xa6\xc3\xa7\xc3\xa8\xc3\xa9\xc3\xaa\xc3\xab\xc3\xac\xc3\xad\xc3\xae\xc3\xaf\xc3\xb0\xc3\xb1\xc3\xb2\xc3\xb3\xc3\xb4\xc3\xb5\xc3\xb6\xc3\xb7\xc3\xb8\xc3\xb9\xc3\xba\xc3\xbb\xc3\xbc\xc3\xbd\xc3\xbe\xc3\xbf");
		Bootstrap::$STRINGLIT_88 = N::str("UTF-16LE");
		Bootstrap::$STRINGLIT_89 = N::str("UTF-16");
		Bootstrap::$STRINGLIT_90 = N::str("UnicodeLittleUnmarked");
		Bootstrap::$STRINGLIT_91 = N::str("X-UTF-16LE");
		Bootstrap::$STRINGLIT_92 = N::str("US-ASCII");
		Bootstrap::$STRINGLIT_93 = N::str("ANSI_X3.4-1968");
		Bootstrap::$STRINGLIT_94 = N::str("CP367");
		Bootstrap::$STRINGLIT_95 = N::str("CSASCII");
		Bootstrap::$STRINGLIT_96 = N::str("ISO-IR-6");
		Bootstrap::$STRINGLIT_97 = N::str("ASCII");
		Bootstrap::$STRINGLIT_98 = N::str("ISO_646.IRV:1983");
		Bootstrap::$STRINGLIT_99 = N::str("ANSI_X3.4-1986");
		Bootstrap::$STRINGLIT_100 = N::str("ASCII7");
		Bootstrap::$STRINGLIT_101 = N::str("DEFAULT");
		Bootstrap::$STRINGLIT_102 = N::str("ISO_646.IRV:1991");
		Bootstrap::$STRINGLIT_103 = N::str("ISO646-US");
		Bootstrap::$STRINGLIT_104 = N::str("IBM367");
		Bootstrap::$STRINGLIT_105 = N::str("646");
		Bootstrap::$STRINGLIT_106 = N::str("\0\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#\$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\x7f\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd\xef\xbf\xbd");
		Bootstrap::$STRINGLIT_107 = N::str("UTF8");
		Bootstrap::$STRINGLIT_108 = N::str("{}");
		Bootstrap::$STRINGLIT_109 = N::str("Capacity: ");
		Bootstrap::$STRINGLIT_110 = N::str("IBM866");
		Bootstrap::$STRINGLIT_111 = N::str("866");
		Bootstrap::$STRINGLIT_112 = N::str("IBM-866");
		Bootstrap::$STRINGLIT_113 = N::str("CSIBM866");
		Bootstrap::$STRINGLIT_114 = N::str("CP866");
		Bootstrap::$STRINGLIT_115 = N::str("\0\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#\$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\x7f\xd0\x90\xd0\x91\xd0\x92\xd0\x93\xd0\x94\xd0\x95\xd0\x96\xd0\x97\xd0\x98\xd0\x99\xd0\x9a\xd0\x9b\xd0\x9c\xd0\x9d\xd0\x9e\xd0\x9f\xd0\xa0\xd0\xa1\xd0\xa2\xd0\xa3\xd0\xa4\xd0\xa5\xd0\xa6\xd0\xa7\xd0\xa8\xd0\xa9\xd0\xaa\xd0\xab\xd0\xac\xd0\xad\xd0\xae\xd0\xaf\xd0\xb0\xd0\xb1\xd0\xb2\xd0\xb3\xd0\xb4\xd0\xb5\xd0\xb6\xd0\xb7\xd0\xb8\xd0\xb9\xd0\xba\xd0\xbb\xd0\xbc\xd0\xbd\xd0\xbe\xd0\xbf\xe2\x96\x91\xe2\x96\x92\xe2\x96\x93\xe2\x94\x82\xe2\x94\xa4\xe2\x95\xa1\xe2\x95\xa2\xe2\x95\x96\xe2\x95\x95\xe2\x95\xa3\xe2\x95\x91\xe2\x95\x97\xe2\x95\x9d\xe2\x95\x9c\xe2\x95\x9b\xe2\x94\x90\xe2\x94\x94\xe2\x94\xb4\xe2\x94\xac\xe2\x94\x9c\xe2\x94\x80\xe2\x94\xbc\xe2\x95\x9e\xe2\x95\x9f\xe2\x95\x9a\xe2\x95\x94\xe2\x95\xa9\xe2\x95\xa6\xe2\x95\xa0\xe2\x95\x90\xe2\x95\xac\xe2\x95\xa7\xe2\x95\xa8\xe2\x95\xa4\xe2\x95\xa5\xe2\x95\x99\xe2\x95\x98\xe2\x95\x92\xe2\x95\x93\xe2\x95\xab\xe2\x95\xaa\xe2\x94\x98\xe2\x94\x8c\xe2\x96\x88\xe2\x96\x84\xe2\x96\x8c\xe2\x96\x90\xe2\x96\x80\xd1\x80\xd1\x81\xd1\x82\xd1\x83\xd1\x84\xd1\x85\xd1\x86\xd1\x87\xd1\x88\xd1\x89\xd1\x8a\xd1\x8b\xd1\x8c\xd1\x8d\xd1\x8e\xd1\x8f\xd0\x81\xd1\x91\xd0\x84\xd1\x94\xd0\x87\xd1\x97\xd0\x8e\xd1\x9e\xc2\xb0\xe2\x88\x99\xc2\xb7\xe2\x88\x9a\xe2\x84\x96\xc2\xa4\xe2\x96\xa0\xc2\xa0");
		Bootstrap::$STRINGLIT_116 = N::str("com.jtransc.charset.JTranscCharset");
		Bootstrap::$STRINGLIT_117 = N::str("com.jtransc.JTranscProcess");
		Bootstrap::$STRINGLIT_118 = N::str("ServiceLoader for ");
		Bootstrap::$STRINGLIT_119 = N::str("size < 0");
		Bootstrap::$STRINGLIT_120 = N::str("0.6.3-snapshot");
		Bootstrap::$STRINGLIT_121 = N::str("orld");
		Bootstrap::$STRINGLIT_122 = N::str("a");
		Bootstrap::$STRINGLIT_123 = N::str("test");
		Bootstrap::$STRINGLIT_124 = N::str("win");
		Bootstrap::$STRINGLIT_125 = N::str("windows");
		Bootstrap::$STRINGLIT_126 = N::str("lin");
		Bootstrap::$STRINGLIT_127 = N::str("linux");
		Bootstrap::$STRINGLIT_128 = N::str("mac");
		Bootstrap::$STRINGLIT_129 = N::str("osx");
		Bootstrap::$STRINGLIT_130 = N::str("fuch");
		Bootstrap::$STRINGLIT_131 = N::str("fuchsia");
		Bootstrap::$STRINGLIT_132 = N::str("unknown-os");
		Bootstrap::$STRINGLIT_133 = N::str("os.name");
		Bootstrap::$STRINGLIT_134 = N::str("java");
		Bootstrap::$STRINGLIT_135 = N::str("java.home");
		Bootstrap::$STRINGLIT_136 = N::str("os.arch");
		Bootstrap::$STRINGLIT_137 = N::str("unknown");
		Bootstrap::$STRINGLIT_138 = N::str("storage == null");
		Bootstrap::$STRINGLIT_139 = N::str("<init>");
		Bootstrap::$STRINGLIT_140 = N::str("()V");
		Bootstrap::$STRINGLIT_141 = N::str("([CII)V");
		Bootstrap::$STRINGLIT_142 = N::str("([C)V");
		Bootstrap::$STRINGLIT_143 = N::str("([BII)V");
		Bootstrap::$STRINGLIT_144 = N::str("([BIILjava/lang/String;Z)V");
		Bootstrap::$STRINGLIT_145 = N::str("(Ljava/lang/String\$1;)V");
		Bootstrap::$STRINGLIT_146 = N::str("(I)V");
		Bootstrap::$STRINGLIT_147 = N::str("(Ljava/io/OutputStream;)V");
		Bootstrap::$STRINGLIT_148 = N::str("(Ljava/lang/String;)V");
		Bootstrap::$STRINGLIT_149 = N::str("(Ljava/lang/Throwable;)V");
		Bootstrap::$STRINGLIT_150 = N::str("(Ljava/lang/String;Ljava/lang/Throwable;)V");
		Bootstrap::$STRINGLIT_151 = N::str("(Z)V");
		Bootstrap::$STRINGLIT_152 = N::str("(Lcom/jtransc/io/JTranscConsolePrintStream\$ConsoleBaseStream;Z)V");
		Bootstrap::$STRINGLIT_153 = N::str("(Ljava/lang/String;Z)V");
		Bootstrap::$STRINGLIT_154 = N::str("(C)V");
		Bootstrap::$STRINGLIT_155 = N::str("(Ljava/lang/Class;Lj/MemberInfo;)V");
		Bootstrap::$STRINGLIT_156 = N::str("(Ljava/lang/Class<*>;Lj/MemberInfo;)V");
		Bootstrap::$STRINGLIT_157 = N::str("(Lj/MemberInfo;)V");
		Bootstrap::$STRINGLIT_158 = N::str("(F)V");
		Bootstrap::$STRINGLIT_159 = N::str("(D)V");
		Bootstrap::$STRINGLIT_160 = N::str("(J)V");
		Bootstrap::$STRINGLIT_161 = N::str("(S)V");
		Bootstrap::$STRINGLIT_162 = N::str("(B)V");
		Bootstrap::$STRINGLIT_163 = N::str("(ILjava/lang/String;Ljava/lang/String;ILjava/lang/String;Ljava/lang/String;)V");
		Bootstrap::$STRINGLIT_164 = N::str("(Ljava/util/AbstractList;)V");
		Bootstrap::$STRINGLIT_165 = N::str("(Ljava/util/AbstractList;I)V");
		Bootstrap::$STRINGLIT_166 = N::str("(ILjava/lang/String;Ljava/lang/String;II[I[I)V");
		Bootstrap::$STRINGLIT_167 = N::str("(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;I)V");
		Bootstrap::$STRINGLIT_168 = N::str("([Ljava/lang/Object;)V");
		Bootstrap::$STRINGLIT_169 = N::str("([TE;)V");
		Bootstrap::$STRINGLIT_170 = N::str("([LBenchmark\$MyClass2;)V");
		Bootstrap::$STRINGLIT_171 = N::str("(Ljava/lang/String;I)V");
		Bootstrap::$STRINGLIT_172 = N::str("([B)V");
		Bootstrap::$STRINGLIT_173 = N::str("([I[I)V");
		Bootstrap::$STRINGLIT_174 = N::str("([I)V");
		Bootstrap::$STRINGLIT_175 = N::str("([S)V");
		Bootstrap::$STRINGLIT_176 = N::str("([D)V");
		Bootstrap::$STRINGLIT_177 = N::str("([F)V");
		Bootstrap::$STRINGLIT_178 = N::str("(Lcom/jtransc/time/JTranscClock\$Impl;)V");
		Bootstrap::$STRINGLIT_179 = N::str("([Ljava/lang/String;IFI)V");
		Bootstrap::$STRINGLIT_180 = N::str("(Ljava/lang/Class;)V");
		Bootstrap::$STRINGLIT_181 = N::str("(Ljava/lang/Class<TS;>;)V");
		Bootstrap::$STRINGLIT_182 = N::str("([Ljava/lang/String;Ljava/lang/String;)V");
		Bootstrap::$STRINGLIT_183 = N::str("(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/HashMap\$HashMapEntry;)V");
		Bootstrap::$STRINGLIT_184 = N::str("(TK;TV;ILjava/util/HashMap\$HashMapEntry<TK;TV;>;)V");
		Bootstrap::$STRINGLIT_185 = N::str("(Ljava/util/HashMap;Ljava/util/HashMap\$1;)V");
		Bootstrap::$STRINGLIT_186 = N::str("(Ljava/util/HashMap;)V");
		Bootstrap::$STRINGLIT_187 = N::str("(Ljava/lang/Object;)V");
		Bootstrap::$STRINGLIT_188 = N::str("(Ljava/util/Collections\$1;)V");
		Bootstrap::$STRINGLIT_189 = N::str("([Ljava/lang/String;Z)V");
		Bootstrap::$STRINGLIT_190 = N::str("(Ljava/lang/ClassLoader;)V");
		Bootstrap::$STRINGLIT_191 = N::str("(IILjava/nio/internal/MemoryBlock;)V");
		Bootstrap::$STRINGLIT_192 = N::str("(I[BIZ)V");
		Bootstrap::$STRINGLIT_193 = N::str("([BZ)V");
		Bootstrap::$STRINGLIT_194 = N::str("(Ljava/nio/ByteBuffer;)V");
		Bootstrap::$STRINGLIT_195 = N::str("(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsLongBuffer\$1;)V");
		Bootstrap::$STRINGLIT_196 = N::str("(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsDoubleBuffer\$1;)V");
		Bootstrap::$STRINGLIT_197 = N::str("(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsCharBuffer\$1;)V");
		Bootstrap::$STRINGLIT_198 = N::str("(Ljava/nio/ByteBuffer;Ljava/nio/ByteBufferAsShortBuffer\$1;)V");
		Bootstrap::$STRINGLIT_199 = N::str("(IIIII)V");
		Bootstrap::$STRINGLIT_200 = N::str("(IZ)V");
		Bootstrap::$STRINGLIT_201 = N::str("(LBenchmark\$1;)V");
		Bootstrap::$STRINGLIT_202 = N::str("(Ljava/lang/Object;Ljava/lang/Object;ILjava/util/Hashtable\$HashtableEntry;)V");
		Bootstrap::$STRINGLIT_203 = N::str("(TK;TV;ILjava/util/Hashtable\$HashtableEntry<TK;TV;>;)V");
		Bootstrap::$STRINGLIT_204 = N::str("(Ljava/util/Hashtable;Ljava/util/Hashtable\$1;)V");
		Bootstrap::$STRINGLIT_205 = N::str("(Ljava/util/Hashtable;)V");
		Bootstrap::$STRINGLIT_206 = N::str("(Ljava/lang/String;Ljava/lang/Class;)V");
		Bootstrap::$STRINGLIT_207 = N::str("(Ljava/lang/String;Ljava/lang/Class<*>;)V");
		Bootstrap::$STRINGLIT_208 = N::str("(TT;)V");
		Bootstrap::$STRINGLIT_209 = N::str("(Ljava/lang/Object;Ljava/lang/ref/ReferenceQueue;)V");
		Bootstrap::$STRINGLIT_210 = N::str("(TT;Ljava/lang/ref/ReferenceQueue<-TT;>;)V");
		Bootstrap::$STRINGLIT_211 = N::str(" ");
		Bootstrap::$STRINGLIT_212 = N::str(".");
		Bootstrap::$STRINGLIT_213 = N::str("(");
		Bootstrap::$STRINGLIT_214 = N::str("totalTime");
		Bootstrap::$STRINGLIT_215 = N::str("D");
		Bootstrap::$STRINGLIT_216 = N::str("CASE_INSENSITIVE_ORDER");
		Bootstrap::$STRINGLIT_217 = N::str("Ljava/util/Comparator;");
		Bootstrap::$STRINGLIT_218 = N::str("Ljava/util/Comparator<Ljava/lang/String;>;");
		Bootstrap::$STRINGLIT_219 = N::str("hash");
		Bootstrap::$STRINGLIT_220 = N::str("I");
		Bootstrap::$STRINGLIT_221 = N::str("value");
		Bootstrap::$STRINGLIT_222 = N::str("[C");
		Bootstrap::$STRINGLIT_223 = N::str("buffer");
		Bootstrap::$STRINGLIT_224 = N::str("length");
		Bootstrap::$STRINGLIT_225 = N::str("in");
		Bootstrap::$STRINGLIT_226 = N::str("Ljava/io/InputStream;");
		Bootstrap::$STRINGLIT_227 = N::str("out");
		Bootstrap::$STRINGLIT_228 = N::str("Ljava/io/PrintStream;");
		Bootstrap::$STRINGLIT_229 = N::str("err");
		Bootstrap::$STRINGLIT_230 = N::str("_props");
		Bootstrap::$STRINGLIT_231 = N::str("Ljava/util/Properties;");
		Bootstrap::$STRINGLIT_232 = N::str("encoding");
		Bootstrap::$STRINGLIT_233 = N::str("Ljava/lang/String;");
		Bootstrap::$STRINGLIT_234 = N::str("autoFlush");
		Bootstrap::$STRINGLIT_235 = N::str("Z");
		Bootstrap::$STRINGLIT_236 = N::str("ioError");
		Bootstrap::$STRINGLIT_237 = N::str("Ljava/io/OutputStream;");
		Bootstrap::$STRINGLIT_238 = N::str("thrown");
		Bootstrap::$STRINGLIT_239 = N::str("EMPTY_ARRAY");
		Bootstrap::$STRINGLIT_240 = N::str("[Ljava/lang/Throwable;");
		Bootstrap::$STRINGLIT_241 = N::str("message");
		Bootstrap::$STRINGLIT_242 = N::str("writableStackTrace");
		Bootstrap::$STRINGLIT_243 = N::str("enableSuppression");
		Bootstrap::$STRINGLIT_244 = N::str("cause");
		Bootstrap::$STRINGLIT_245 = N::str("Ljava/lang/Throwable;");
		Bootstrap::$STRINGLIT_246 = N::str("stackTrace");
		Bootstrap::$STRINGLIT_247 = N::str("[Ljava/lang/StackTraceElement;");
		Bootstrap::$STRINGLIT_248 = N::str("supressed");
		Bootstrap::$STRINGLIT_249 = N::str("Ljava/util/ArrayList;");
		Bootstrap::$STRINGLIT_250 = N::str("Ljava/util/ArrayList<Ljava/lang/Throwable;>;");
		Bootstrap::$STRINGLIT_251 = N::str("error");
		Bootstrap::$STRINGLIT_252 = N::str("stream");
		Bootstrap::$STRINGLIT_253 = N::str("Lcom/jtransc/io/JTranscConsolePrintStream\$ConsoleBaseStream;");
		Bootstrap::$STRINGLIT_254 = N::str("sb");
		Bootstrap::$STRINGLIT_255 = N::str("Ljava/lang/StringBuilder;");
		Bootstrap::$STRINGLIT_256 = N::str("name");
		Bootstrap::$STRINGLIT_257 = N::str("primitive");
		Bootstrap::$STRINGLIT_258 = N::str("modifiers");
		Bootstrap::$STRINGLIT_259 = N::str("_classCache");
		Bootstrap::$STRINGLIT_260 = N::str("Lcom/jtransc/ds/FastStringMap;");
		Bootstrap::$STRINGLIT_261 = N::str("Lcom/jtransc/ds/FastStringMap<Ljava/lang/Class<*>;>;");
		Bootstrap::$STRINGLIT_262 = N::str("_accessibleMethods");
		Bootstrap::$STRINGLIT_263 = N::str("[Ljava/lang/reflect/Method;");
		Bootstrap::$STRINGLIT_264 = N::str("enumConstants");
		Bootstrap::$STRINGLIT_265 = N::str("[Ljava/lang/Object;");
		Bootstrap::$STRINGLIT_266 = N::str("[TT;");
		Bootstrap::$STRINGLIT_267 = N::str("_allFields");
		Bootstrap::$STRINGLIT_268 = N::str("[Ljava/lang/reflect/Field;");
		Bootstrap::$STRINGLIT_269 = N::str("_accessibleFields");
		Bootstrap::$STRINGLIT_270 = N::str("_allMethods");
		Bootstrap::$STRINGLIT_271 = N::str("id");
		Bootstrap::$STRINGLIT_272 = N::str("related");
		Bootstrap::$STRINGLIT_273 = N::str("[I");
		Bootstrap::$STRINGLIT_274 = N::str("info");
		Bootstrap::$STRINGLIT_275 = N::str("Lj/ClassInfo;");
		Bootstrap::$STRINGLIT_276 = N::str("TYPE");
		Bootstrap::$STRINGLIT_277 = N::str("Ljava/lang/Class;");
		Bootstrap::$STRINGLIT_278 = N::str("Ljava/lang/Class<Ljava/lang/Integer;>;");
		Bootstrap::$STRINGLIT_279 = N::str("values");
		Bootstrap::$STRINGLIT_280 = N::str("[Ljava/lang/Integer;");
		Bootstrap::$STRINGLIT_281 = N::str("NTZ_TABLE");
		Bootstrap::$STRINGLIT_282 = N::str("[B");
		Bootstrap::$STRINGLIT_283 = N::str("C");
		Bootstrap::$STRINGLIT_284 = N::str("Ljava/lang/Class<Ljava/lang/Character;>;");
		Bootstrap::$STRINGLIT_285 = N::str("clazz");
		Bootstrap::$STRINGLIT_286 = N::str("Ljava/lang/Class<*>;");
		Bootstrap::$STRINGLIT_287 = N::str("signature");
		Bootstrap::$STRINGLIT_288 = N::str("slot");
		Bootstrap::$STRINGLIT_289 = N::str("genericSignature");
		Bootstrap::$STRINGLIT_290 = N::str("Lj/MemberInfo;");
		Bootstrap::$STRINGLIT_291 = N::str("ex");
		Bootstrap::$STRINGLIT_292 = N::str("Ljava/lang/Class<Ljava/lang/Void;>;");
		Bootstrap::$STRINGLIT_293 = N::str("F");
		Bootstrap::$STRINGLIT_294 = N::str("Ljava/lang/Class<Ljava/lang/Float;>;");
		Bootstrap::$STRINGLIT_295 = N::str("Ljava/lang/Class<Ljava/lang/Double;>;");
		Bootstrap::$STRINGLIT_296 = N::str("J");
		Bootstrap::$STRINGLIT_297 = N::str("Ljava/lang/Class<Ljava/lang/Long;>;");
		Bootstrap::$STRINGLIT_298 = N::str("S");
		Bootstrap::$STRINGLIT_299 = N::str("Ljava/lang/Class<Ljava/lang/Short;>;");
		Bootstrap::$STRINGLIT_300 = N::str("Ljava/lang/Class<Ljava/lang/Boolean;>;");
		Bootstrap::$STRINGLIT_301 = N::str("TRUE");
		Bootstrap::$STRINGLIT_302 = N::str("Ljava/lang/Boolean;");
		Bootstrap::$STRINGLIT_303 = N::str("FALSE");
		Bootstrap::$STRINGLIT_304 = N::str("cache");
		Bootstrap::$STRINGLIT_305 = N::str("[Ljava/lang/Byte;");
		Bootstrap::$STRINGLIT_306 = N::str("B");
		Bootstrap::$STRINGLIT_307 = N::str("Ljava/lang/Class<Ljava/lang/Byte;>;");
		Bootstrap::$STRINGLIT_308 = N::str("exceptionTypes");
		Bootstrap::$STRINGLIT_309 = N::str("[Ljava/lang/Class;");
		Bootstrap::$STRINGLIT_310 = N::str("[Ljava/lang/Class<*>;");
		Bootstrap::$STRINGLIT_311 = N::str("modCount");
		Bootstrap::$STRINGLIT_312 = N::str("expectedModCount");
		Bootstrap::$STRINGLIT_313 = N::str("this\$0");
		Bootstrap::$STRINGLIT_314 = N::str("Ljava/util/AbstractList;");
		Bootstrap::$STRINGLIT_315 = N::str("pos");
		Bootstrap::$STRINGLIT_316 = N::str("lastPosition");
		Bootstrap::$STRINGLIT_317 = N::str("_classInfos");
		Bootstrap::$STRINGLIT_318 = N::str("[Lj/ClassInfo;");
		Bootstrap::$STRINGLIT_319 = N::str("_classNames");
		Bootstrap::$STRINGLIT_320 = N::str("[Ljava/lang/String;");
		Bootstrap::$STRINGLIT_321 = N::str("_classInfosByName");
		Bootstrap::$STRINGLIT_322 = N::str("Lcom/jtransc/ds/FastStringMap<Lj/ClassInfo;>;");
		Bootstrap::$STRINGLIT_323 = N::str("fileName");
		Bootstrap::$STRINGLIT_324 = N::str("lineNumber");
		Bootstrap::$STRINGLIT_325 = N::str("methodName");
		Bootstrap::$STRINGLIT_326 = N::str("declaringClass");
		Bootstrap::$STRINGLIT_327 = N::str("\$\$lastId");
		Bootstrap::$STRINGLIT_328 = N::str("[TE;");
		Bootstrap::$STRINGLIT_329 = N::str("val\$objects");
		Bootstrap::$STRINGLIT_330 = N::str("[LBenchmark\$MyClass2;");
		Bootstrap::$STRINGLIT_331 = N::str("c");
		Bootstrap::$STRINGLIT_332 = N::str("d");
		Bootstrap::$STRINGLIT_333 = N::str("b");
		Bootstrap::$STRINGLIT_334 = N::str("start");
		Bootstrap::$STRINGLIT_335 = N::str("val\$hexData");
		Bootstrap::$STRINGLIT_336 = N::str("seed");
		Bootstrap::$STRINGLIT_337 = N::str("haveNextNextGaussian");
		Bootstrap::$STRINGLIT_338 = N::str("current");
		Bootstrap::$STRINGLIT_339 = N::str("Ljava/lang/Runtime;");
		Bootstrap::$STRINGLIT_340 = N::str("val\$bytes");
		Bootstrap::$STRINGLIT_341 = N::str("val\$srcI");
		Bootstrap::$STRINGLIT_342 = N::str("val\$dstI");
		Bootstrap::$STRINGLIT_343 = N::str("val\$iarray");
		Bootstrap::$STRINGLIT_344 = N::str("val\$carray");
		Bootstrap::$STRINGLIT_345 = N::str("val\$sarray");
		Bootstrap::$STRINGLIT_346 = N::str("[S");
		Bootstrap::$STRINGLIT_347 = N::str("val\$barray");
		Bootstrap::$STRINGLIT_348 = N::str("val\$darray");
		Bootstrap::$STRINGLIT_349 = N::str("[D");
		Bootstrap::$STRINGLIT_350 = N::str("val\$farray");
		Bootstrap::$STRINGLIT_351 = N::str("[F");
		Bootstrap::$STRINGLIT_352 = N::str("impl");
		Bootstrap::$STRINGLIT_353 = N::str("Lcom/jtransc/time/JTranscClock\$Impl;");
		Bootstrap::$STRINGLIT_354 = N::str("parent");
		Bootstrap::$STRINGLIT_355 = N::str("max");
		Bootstrap::$STRINGLIT_356 = N::str("min");
		Bootstrap::$STRINGLIT_357 = N::str("avg");
		Bootstrap::$STRINGLIT_358 = N::str("names");
		Bootstrap::$STRINGLIT_359 = N::str("charsets");
		Bootstrap::$STRINGLIT_360 = N::str("Lcom/jtransc/ds/FastStringMap<Lcom/jtransc/charset/JTranscCharset;>;");
		Bootstrap::$STRINGLIT_361 = N::str("loadedCharsets");
		Bootstrap::$STRINGLIT_362 = N::str("count");
		Bootstrap::$STRINGLIT_363 = N::str("buf");
		Bootstrap::$STRINGLIT_364 = N::str("position");
		Bootstrap::$STRINGLIT_365 = N::str("size");
		Bootstrap::$STRINGLIT_366 = N::str("charsetName");
		Bootstrap::$STRINGLIT_367 = N::str("service");
		Bootstrap::$STRINGLIT_368 = N::str("Ljava/lang/Class<TS;>;");
		Bootstrap::$STRINGLIT_369 = N::str("list");
		Bootstrap::$STRINGLIT_370 = N::str("Ljava/util/List;");
		Bootstrap::$STRINGLIT_371 = N::str("Ljava/util/List<TS;>;");
		Bootstrap::$STRINGLIT_372 = N::str("invalidChar");
		Bootstrap::$STRINGLIT_373 = N::str("decode");
		Bootstrap::$STRINGLIT_374 = N::str("encode");
		Bootstrap::$STRINGLIT_375 = N::str("Ljava/util/Map;");
		Bootstrap::$STRINGLIT_376 = N::str("Ljava/util/Map<Ljava/lang/Character;Ljava/lang/Byte;>;");
		Bootstrap::$STRINGLIT_377 = N::str("table");
		Bootstrap::$STRINGLIT_378 = N::str("[Ljava/util/HashMap\$HashMapEntry;");
		Bootstrap::$STRINGLIT_379 = N::str("[Ljava/util/HashMap\$HashMapEntry<TK;TV;>;");
		Bootstrap::$STRINGLIT_380 = N::str("EMPTY_TABLE");
		Bootstrap::$STRINGLIT_381 = N::str("[Ljava/util/Map\$Entry;");
		Bootstrap::$STRINGLIT_382 = N::str("entryForNullKey");
		Bootstrap::$STRINGLIT_383 = N::str("Ljava/util/HashMap\$HashMapEntry;");
		Bootstrap::$STRINGLIT_384 = N::str("Ljava/util/HashMap\$HashMapEntry<TK;TV;>;");
		Bootstrap::$STRINGLIT_385 = N::str("entrySet");
		Bootstrap::$STRINGLIT_386 = N::str("Ljava/util/Set;");
		Bootstrap::$STRINGLIT_387 = N::str("Ljava/util/Set<Ljava/util/Map\$Entry<TK;TV;>;>;");
		Bootstrap::$STRINGLIT_388 = N::str("Ljava/util/Collection;");
		Bootstrap::$STRINGLIT_389 = N::str("Ljava/util/Collection<TV;>;");
		Bootstrap::$STRINGLIT_390 = N::str("keySet");
		Bootstrap::$STRINGLIT_391 = N::str("Ljava/util/Set<TK;>;");
		Bootstrap::$STRINGLIT_392 = N::str("valuesCollection");
		Bootstrap::$STRINGLIT_393 = N::str("key");
		Bootstrap::$STRINGLIT_394 = N::str("Ljava/lang/Object;");
		Bootstrap::$STRINGLIT_395 = N::str("TK;");
		Bootstrap::$STRINGLIT_396 = N::str("next");
		Bootstrap::$STRINGLIT_397 = N::str("TV;");
		Bootstrap::$STRINGLIT_398 = N::str("Ljava/util/HashMap;");
		Bootstrap::$STRINGLIT_399 = N::str("nextEntry");
		Bootstrap::$STRINGLIT_400 = N::str("nextIndex");
		Bootstrap::$STRINGLIT_401 = N::str("lastEntryReturned");
		Bootstrap::$STRINGLIT_402 = N::str("EMPTY_SET");
		Bootstrap::$STRINGLIT_403 = N::str("EMPTY_ITERATOR");
		Bootstrap::$STRINGLIT_404 = N::str("Ljava/util/Iterator;");
		Bootstrap::$STRINGLIT_405 = N::str("Ljava/util/Iterator<*>;");
		Bootstrap::$STRINGLIT_406 = N::str("EMPTY_ENUMERATION");
		Bootstrap::$STRINGLIT_407 = N::str("Ljava/util/Enumeration;");
		Bootstrap::$STRINGLIT_408 = N::str("Ljava/util/Enumeration<*>;");
		Bootstrap::$STRINGLIT_409 = N::str("EMPTY_MAP");
		Bootstrap::$STRINGLIT_410 = N::str("EMPTY_LIST");
		Bootstrap::$STRINGLIT_411 = N::str("littleEndian");
		Bootstrap::$STRINGLIT_412 = N::str("group");
		Bootstrap::$STRINGLIT_413 = N::str("Ljava/lang/ThreadGroup;");
		Bootstrap::$STRINGLIT_414 = N::str("_currentThread");
		Bootstrap::$STRINGLIT_415 = N::str("Ljava/lang/Thread;");
		Bootstrap::$STRINGLIT_416 = N::str("classLoader");
		Bootstrap::$STRINGLIT_417 = N::str("Ljava/lang/ClassLoader;");
		Bootstrap::$STRINGLIT_418 = N::str("nativeLibs");
		Bootstrap::$STRINGLIT_419 = N::str("Ljava/util/ArrayList<Ljava/lang/ClassLoader\$NativeLib;>;");
		Bootstrap::$STRINGLIT_420 = N::str("EMPTY_BYTE");
		Bootstrap::$STRINGLIT_421 = N::str("EMPTY_CLASS");
		Bootstrap::$STRINGLIT_422 = N::str("_elementSizeShift");
		Bootstrap::$STRINGLIT_423 = N::str("mark");
		Bootstrap::$STRINGLIT_424 = N::str("block");
		Bootstrap::$STRINGLIT_425 = N::str("Ljava/nio/internal/MemoryBlock;");
		Bootstrap::$STRINGLIT_426 = N::str("limit");
		Bootstrap::$STRINGLIT_427 = N::str("capacity");
		Bootstrap::$STRINGLIT_428 = N::str("arrayOffset");
		Bootstrap::$STRINGLIT_429 = N::str("backingArray");
		Bootstrap::$STRINGLIT_430 = N::str("isReadOnly");
		Bootstrap::$STRINGLIT_431 = N::str("isLittleEndian");
		Bootstrap::$STRINGLIT_432 = N::str("isNativeOrder");
		Bootstrap::$STRINGLIT_433 = N::str("order");
		Bootstrap::$STRINGLIT_434 = N::str("Ljava/nio/ByteOrder;");
		Bootstrap::$STRINGLIT_435 = N::str("isDirect");
		Bootstrap::$STRINGLIT_436 = N::str("byteBuffer");
		Bootstrap::$STRINGLIT_437 = N::str("Ljava/nio/ByteBuffer;");
		Bootstrap::$STRINGLIT_438 = N::str("bytes");
		Bootstrap::$STRINGLIT_439 = N::str("SWAPPED");
		Bootstrap::$STRINGLIT_440 = N::str("NATIVE");
		Bootstrap::$STRINGLIT_441 = N::str("needsSwap");
		Bootstrap::$STRINGLIT_442 = N::str("NATIVE_ORDER");
		Bootstrap::$STRINGLIT_443 = N::str("Lcom/jtransc/compression/jzlib/CRC32;");
		Bootstrap::$STRINGLIT_444 = N::str("tbytes");
		Bootstrap::$STRINGLIT_445 = N::str("temp");
		Bootstrap::$STRINGLIT_446 = N::str("nice_length");
		Bootstrap::$STRINGLIT_447 = N::str("max_chain");
		Bootstrap::$STRINGLIT_448 = N::str("max_lazy");
		Bootstrap::$STRINGLIT_449 = N::str("good_length");
		Bootstrap::$STRINGLIT_450 = N::str("func");
		Bootstrap::$STRINGLIT_451 = N::str("Lcom/jtransc/compression/jzlib/Deflater;");
		Bootstrap::$STRINGLIT_452 = N::str("inLength");
		Bootstrap::$STRINGLIT_453 = N::str("inRead");
		Bootstrap::$STRINGLIT_454 = N::str("noHeader");
		Bootstrap::$STRINGLIT_455 = N::str("compressLevel");
		Bootstrap::$STRINGLIT_456 = N::str("streamHandle");
		Bootstrap::$STRINGLIT_457 = N::str("flushParm");
		Bootstrap::$STRINGLIT_458 = N::str("strategy");
		Bootstrap::$STRINGLIT_459 = N::str("defaults");
		Bootstrap::$STRINGLIT_460 = N::str("[Ljava/util/Hashtable\$HashtableEntry;");
		Bootstrap::$STRINGLIT_461 = N::str("[Ljava/util/Hashtable\$HashtableEntry<TK;TV;>;");
		Bootstrap::$STRINGLIT_462 = N::str("serialPersistentFields");
		Bootstrap::$STRINGLIT_463 = N::str("[Ljava/io/ObjectStreamField;");
		Bootstrap::$STRINGLIT_464 = N::str("Ljava/util/Hashtable\$HashtableEntry;");
		Bootstrap::$STRINGLIT_465 = N::str("Ljava/util/Hashtable\$HashtableEntry<TK;TV;>;");
		Bootstrap::$STRINGLIT_466 = N::str("Ljava/util/Hashtable;");
		Bootstrap::$STRINGLIT_467 = N::str("type");
		Bootstrap::$STRINGLIT_468 = N::str("referent");
		Bootstrap::$STRINGLIT_469 = N::str("TT;");
		Bootstrap::$STRINGLIT_470 = N::str("queue");
		Bootstrap::$STRINGLIT_471 = N::str("Ljava/lang/ref/ReferenceQueue;");
		Bootstrap::$STRINGLIT_472 = N::str("Ljava/lang/ref/ReferenceQueue<-TT;>;");
		Bootstrap::$STRINGLIT_473 = N::str("UNKNOWN");
		Bootstrap::$STRINGLIT_474 = N::str("(Native Method)");
		Bootstrap::$STRINGLIT_475 = N::str("(Unknown Source)");
		Bootstrap::$STRINGLIT_476 = N::str("Benchmark");
		Bootstrap::$STRINGLIT_477 = N::str("java.lang.Object");
		Bootstrap::$STRINGLIT_478 = N::str("java.lang.String");
		Bootstrap::$STRINGLIT_479 = N::str("java.io.Serializable");
		Bootstrap::$STRINGLIT_480 = N::str("java.lang.Comparable");
		Bootstrap::$STRINGLIT_481 = N::str("java.lang.CharSequence");
		Bootstrap::$STRINGLIT_482 = N::str("java.lang.String\$CaseInsensitiveComparator");
		Bootstrap::$STRINGLIT_483 = N::str("java.util.Comparator");
		Bootstrap::$STRINGLIT_484 = N::str("java.lang.String\$1");
		Bootstrap::$STRINGLIT_485 = N::str("java.lang.StringBuilder");
		Bootstrap::$STRINGLIT_486 = N::str("java.lang.Appendable");
		Bootstrap::$STRINGLIT_487 = N::str("java.util.Arrays");
		Bootstrap::$STRINGLIT_488 = N::str("java.lang.System");
		Bootstrap::$STRINGLIT_489 = N::str("java.io.PrintStream");
		Bootstrap::$STRINGLIT_490 = N::str("java.io.Closeable");
		Bootstrap::$STRINGLIT_491 = N::str("java.lang.AutoCloseable");
		Bootstrap::$STRINGLIT_492 = N::str("java.io.FilterOutputStream");
		Bootstrap::$STRINGLIT_493 = N::str("java.io.OutputStream");
		Bootstrap::$STRINGLIT_494 = N::str("java.io.Flushable");
		Bootstrap::$STRINGLIT_495 = N::str("java.lang.NullPointerException");
		Bootstrap::$STRINGLIT_496 = N::str("java.lang.RuntimeException");
		Bootstrap::$STRINGLIT_497 = N::str("java.lang.Exception");
		Bootstrap::$STRINGLIT_498 = N::str("java.lang.Throwable");
		Bootstrap::$STRINGLIT_499 = N::str("java.lang.jtransc.JTranscStrings");
		Bootstrap::$STRINGLIT_500 = N::str("java.lang.Math");
		Bootstrap::$STRINGLIT_501 = N::str("java.lang.System\$1");
		Bootstrap::$STRINGLIT_502 = N::str("java.io.InputStream");
		Bootstrap::$STRINGLIT_503 = N::str("com.jtransc.io.JTranscConsolePrintStream");
		Bootstrap::$STRINGLIT_504 = N::str("com.jtransc.io.JTranscConsolePrintStream\$ConsoleOutputStream");
		Bootstrap::$STRINGLIT_505 = N::str("com.jtransc.io.JTranscConsolePrintStream\$ConsoleBaseStream");
		Bootstrap::$STRINGLIT_506 = N::str("com.jtransc.io.JTranscConsolePrintStream\$ConsoleErrorStream");
		Bootstrap::$STRINGLIT_507 = N::str("java.lang.ArrayIndexOutOfBoundsException");
		Bootstrap::$STRINGLIT_508 = N::str("java.lang.IndexOutOfBoundsException");
		Bootstrap::$STRINGLIT_509 = N::str("java.lang.IllegalArgumentException");
		Bootstrap::$STRINGLIT_510 = N::str("java.lang.Class");
		Bootstrap::$STRINGLIT_511 = N::str("java.lang.reflect.Type");
		Bootstrap::$STRINGLIT_512 = N::str("java.lang.reflect.GenericDeclaration");
		Bootstrap::$STRINGLIT_513 = N::str("java.lang.reflect.AnnotatedElement");
		Bootstrap::$STRINGLIT_514 = N::str("java.lang.AnnotatedElement");
		Bootstrap::$STRINGLIT_515 = N::str("java.lang.reflect.Modifier");
		Bootstrap::$STRINGLIT_516 = N::str("java.lang.Integer");
		Bootstrap::$STRINGLIT_517 = N::str("java.lang.Number");
		Bootstrap::$STRINGLIT_518 = N::str("java.lang.IntegerTools");
		Bootstrap::$STRINGLIT_519 = N::str("java.lang.Character");
		Bootstrap::$STRINGLIT_520 = N::str("java.lang.reflect.Field");
		Bootstrap::$STRINGLIT_521 = N::str("java.lang.reflect.Member");
		Bootstrap::$STRINGLIT_522 = N::str("java.lang.reflect.AccessibleObject");
		Bootstrap::$STRINGLIT_523 = N::str("java.lang.ClassNotFoundException");
		Bootstrap::$STRINGLIT_524 = N::str("java.lang.ReflectiveOperationException");
		Bootstrap::$STRINGLIT_525 = N::str("java.lang.Void");
		Bootstrap::$STRINGLIT_526 = N::str("java.lang.Float");
		Bootstrap::$STRINGLIT_527 = N::str("com.jtransc.text.JTranscStringTools");
		Bootstrap::$STRINGLIT_528 = N::str("java.lang.Double");
		Bootstrap::$STRINGLIT_529 = N::str("java.lang.Long");
		Bootstrap::$STRINGLIT_530 = N::str("com.jtransc.internal.JTranscCType");
		Bootstrap::$STRINGLIT_531 = N::str("java.lang.Short");
		Bootstrap::$STRINGLIT_532 = N::str("com.jtransc.io.JTranscConsole");
		Bootstrap::$STRINGLIT_533 = N::str("java.lang.Boolean");
		Bootstrap::$STRINGLIT_534 = N::str("java.lang.Byte");
		Bootstrap::$STRINGLIT_535 = N::str("java.lang.reflect.Method");
		Bootstrap::$STRINGLIT_536 = N::str("java.lang.reflect.MethodConstructor");
		Bootstrap::$STRINGLIT_537 = N::str("j.MemberInfo");
		Bootstrap::$STRINGLIT_538 = N::str("java.lang.Error");
		Bootstrap::$STRINGLIT_539 = N::str("java.util.ArrayList");
		Bootstrap::$STRINGLIT_540 = N::str("java.util.List");
		Bootstrap::$STRINGLIT_541 = N::str("java.util.Collection");
		Bootstrap::$STRINGLIT_542 = N::str("java.lang.Iterable");
		Bootstrap::$STRINGLIT_543 = N::str("java.util.RandomAccess");
		Bootstrap::$STRINGLIT_544 = N::str("java.lang.Cloneable");
		Bootstrap::$STRINGLIT_545 = N::str("java.util.AbstractList");
		Bootstrap::$STRINGLIT_546 = N::str("java.util.AbstractCollection");
		Bootstrap::$STRINGLIT_547 = N::str("java.util.Iterator");
		Bootstrap::$STRINGLIT_548 = N::str("java.util.AbstractList\$SimpleListIterator");
		Bootstrap::$STRINGLIT_549 = N::str("java.util.ListIterator");
		Bootstrap::$STRINGLIT_550 = N::str("java.util.AbstractList\$FullListIterator");
		Bootstrap::$STRINGLIT_551 = N::str("java.util.NoSuchElementException");
		Bootstrap::$STRINGLIT_552 = N::str("java.util.ConcurrentModificationException");
		Bootstrap::$STRINGLIT_553 = N::str("java.lang.reflect.Array");
		Bootstrap::$STRINGLIT_554 = N::str("java.lang.jtransc.JTranscCoreReflection");
		Bootstrap::$STRINGLIT_555 = N::str("j.ProgramReflection");
		Bootstrap::$STRINGLIT_556 = N::str("j.ClassInfo");
		Bootstrap::$STRINGLIT_557 = N::str("j.ProgramReflection\$AllClasses");
		Bootstrap::$STRINGLIT_558 = N::str("java.lang.UnsupportedOperationException");
		Bootstrap::$STRINGLIT_559 = N::str("java.lang.reflect.ParameterizedType");
		Bootstrap::$STRINGLIT_560 = N::str("java.lang.StackTraceElement");
		Bootstrap::$STRINGLIT_561 = N::str("java.lang.CloneNotSupportedException");
		Bootstrap::$STRINGLIT_562 = N::str("j.ProgramReflection\$DynamicGetSet");
		Bootstrap::$STRINGLIT_563 = N::str("j.ProgramReflection\$AllFields");
		Bootstrap::$STRINGLIT_564 = N::str("java.lang.NoSuchMethodException");
		Bootstrap::$STRINGLIT_565 = N::str("java.lang.InstantiationException");
		Bootstrap::$STRINGLIT_566 = N::str("java.lang.reflect.Constructor");
		Bootstrap::$STRINGLIT_567 = N::str("java.lang.reflect.InvocationTargetException");
		Bootstrap::$STRINGLIT_568 = N::str("j.ProgramReflection\$DynamicNewInvoke");
		Bootstrap::$STRINGLIT_569 = N::str("java.lang.SystemInt");
		Bootstrap::$STRINGLIT_570 = N::str("java.util.Objects");
		Bootstrap::$STRINGLIT_571 = N::str("j.ProgramReflection\$AllConstructors");
		Bootstrap::$STRINGLIT_572 = N::str("java.util.Arrays\$ArrayList");
		Bootstrap::$STRINGLIT_573 = N::str("java.lang.SafeVarargs");
		Bootstrap::$STRINGLIT_574 = N::str("java.lang.annotation.Annotation");
		Bootstrap::$STRINGLIT_575 = N::str("java.lang.ClassCastException");
		Bootstrap::$STRINGLIT_576 = N::str("Benchmark\$37");
		Bootstrap::$STRINGLIT_577 = N::str("Benchmark\$Task");
		Bootstrap::$STRINGLIT_578 = N::str("Benchmark\$MyClass2");
		Bootstrap::$STRINGLIT_579 = N::str("Benchmark\$36");
		Bootstrap::$STRINGLIT_580 = N::str("Benchmark\$35");
		Bootstrap::$STRINGLIT_581 = N::str("Benchmark\$34");
		Bootstrap::$STRINGLIT_582 = N::str("com.jtransc.JTranscSystem");
		Bootstrap::$STRINGLIT_583 = N::str("Benchmark\$39");
		Bootstrap::$STRINGLIT_584 = N::str("Benchmark\$38");
		Bootstrap::$STRINGLIT_585 = N::str("java.util.Random");
		Bootstrap::$STRINGLIT_586 = N::str("Benchmark\$33");
		Bootstrap::$STRINGLIT_587 = N::str("Benchmark\$32");
		Bootstrap::$STRINGLIT_588 = N::str("Benchmark\$31");
		Bootstrap::$STRINGLIT_589 = N::str("Benchmark\$30");
		Bootstrap::$STRINGLIT_590 = N::str("java.lang.Runtime");
		Bootstrap::$STRINGLIT_591 = N::str("Benchmark\$40");
		Bootstrap::$STRINGLIT_592 = N::str("Benchmark\$1");
		Bootstrap::$STRINGLIT_593 = N::str("Benchmark\$2");
		Bootstrap::$STRINGLIT_594 = N::str("Benchmark\$3");
		Bootstrap::$STRINGLIT_595 = N::str("Benchmark\$44");
		Bootstrap::$STRINGLIT_596 = N::str("Benchmark\$4");
		Bootstrap::$STRINGLIT_597 = N::str("Benchmark\$43");
		Bootstrap::$STRINGLIT_598 = N::str("Benchmark\$5");
		Bootstrap::$STRINGLIT_599 = N::str("Benchmark\$42");
		Bootstrap::$STRINGLIT_600 = N::str("Benchmark\$6");
		Bootstrap::$STRINGLIT_601 = N::str("Benchmark\$41");
		Bootstrap::$STRINGLIT_602 = N::str("Benchmark\$7");
		Bootstrap::$STRINGLIT_603 = N::str("Benchmark\$8");
		Bootstrap::$STRINGLIT_604 = N::str("Benchmark\$9");
		Bootstrap::$STRINGLIT_605 = N::str("Benchmark\$15");
		Bootstrap::$STRINGLIT_606 = N::str("Benchmark\$14");
		Bootstrap::$STRINGLIT_607 = N::str("Benchmark\$13");
		Bootstrap::$STRINGLIT_608 = N::str("Benchmark\$12");
		Bootstrap::$STRINGLIT_609 = N::str("Benchmark\$19");
		Bootstrap::$STRINGLIT_610 = N::str("Benchmark\$18");
		Bootstrap::$STRINGLIT_611 = N::str("Benchmark\$17");
		Bootstrap::$STRINGLIT_612 = N::str("Benchmark\$16");
		Bootstrap::$STRINGLIT_613 = N::str("Benchmark\$11");
		Bootstrap::$STRINGLIT_614 = N::str("Benchmark\$10");
		Bootstrap::$STRINGLIT_615 = N::str("Benchmark\$26");
		Bootstrap::$STRINGLIT_616 = N::str("Benchmark\$25");
		Bootstrap::$STRINGLIT_617 = N::str("Benchmark\$24");
		Bootstrap::$STRINGLIT_618 = N::str("Benchmark\$23");
		Bootstrap::$STRINGLIT_619 = N::str("Benchmark\$29");
		Bootstrap::$STRINGLIT_620 = N::str("Benchmark\$28");
		Bootstrap::$STRINGLIT_621 = N::str("Benchmark\$27");
		Bootstrap::$STRINGLIT_622 = N::str("Benchmark\$22");
		Bootstrap::$STRINGLIT_623 = N::str("Benchmark\$21");
		Bootstrap::$STRINGLIT_624 = N::str("Benchmark\$20");
		Bootstrap::$STRINGLIT_625 = N::str("com.jtransc.JTranscVersion");
		Bootstrap::$STRINGLIT_626 = N::str("com.jtransc.time.JTranscClock");
		Bootstrap::$STRINGLIT_627 = N::str("com.jtransc.time.JTranscClock\$1");
		Bootstrap::$STRINGLIT_628 = N::str("com.jtransc.time.JTranscClock\$Impl");
		Bootstrap::$STRINGLIT_629 = N::str("java.io.IOException");
		Bootstrap::$STRINGLIT_630 = N::str("java.io.ByteArrayOutputStream");
		Bootstrap::$STRINGLIT_631 = N::str("com.jtransc.charset.JTranscCharBuffer");
		Bootstrap::$STRINGLIT_632 = N::str("java.nio.charset.UnsupportedCharsetException");
		Bootstrap::$STRINGLIT_633 = N::str("java.util.ServiceLoader");
		Bootstrap::$STRINGLIT_634 = N::str("com.jtransc.charset.charsets.JTranscCharsetIBM866");
		Bootstrap::$STRINGLIT_635 = N::str("com.jtransc.charset.JTranscCharsetSingleByte");
		Bootstrap::$STRINGLIT_636 = N::str("java.util.Map");
		Bootstrap::$STRINGLIT_637 = N::str("java.util.HashMap");
		Bootstrap::$STRINGLIT_638 = N::str("java.util.AbstractMap");
		Bootstrap::$STRINGLIT_639 = N::str("java.util.Map\$Entry");
		Bootstrap::$STRINGLIT_640 = N::str("java.util.HashMap\$HashMapEntry");
		Bootstrap::$STRINGLIT_641 = N::str("java.util.Set");
		Bootstrap::$STRINGLIT_642 = N::str("java.util.HashMap\$1");
		Bootstrap::$STRINGLIT_643 = N::str("java.util.HashMap\$EntrySet");
		Bootstrap::$STRINGLIT_644 = N::str("java.util.AbstractSet");
		Bootstrap::$STRINGLIT_645 = N::str("java.util.HashMap\$EntryIterator");
		Bootstrap::$STRINGLIT_646 = N::str("java.util.HashMap\$HashIterator");
		Bootstrap::$STRINGLIT_647 = N::str("java.lang.AssertionError");
		Bootstrap::$STRINGLIT_648 = N::str("java.util.Collections");
		Bootstrap::$STRINGLIT_649 = N::str("java.util.Collections\$1");
		Bootstrap::$STRINGLIT_650 = N::str("java.util.Collections\$EmptyList");
		Bootstrap::$STRINGLIT_651 = N::str("java.util.Collections\$2");
		Bootstrap::$STRINGLIT_652 = N::str("java.util.Enumeration");
		Bootstrap::$STRINGLIT_653 = N::str("java.util.Collections\$EmptyMap");
		Bootstrap::$STRINGLIT_654 = N::str("java.util.Collections\$EmptySet");
		Bootstrap::$STRINGLIT_655 = N::str("com.jtransc.charset.charsets.JTranscCharsetUTF8");
		Bootstrap::$STRINGLIT_656 = N::str("com.jtransc.charset.charsets.JTranscCharsetUSASCII");
		Bootstrap::$STRINGLIT_657 = N::str("com.jtransc.charset.charsets.JTranscCharsetUTF16LE");
		Bootstrap::$STRINGLIT_658 = N::str("com.jtransc.charset.charsets.JTranscCharsetUTF16Base");
		Bootstrap::$STRINGLIT_659 = N::str("com.jtransc.charset.charsets.JTranscCharsetLatin1");
		Bootstrap::$STRINGLIT_660 = N::str("com.jtransc.charset.charsets.JTranscCharsetUTF16BE");
		Bootstrap::$STRINGLIT_661 = N::str("java.lang.Thread");
		Bootstrap::$STRINGLIT_662 = N::str("java.lang.Runnable");
		Bootstrap::$STRINGLIT_663 = N::str("java.lang.ThreadGroup");
		Bootstrap::$STRINGLIT_664 = N::str("java.lang.Thread\$UncaughtExceptionHandler");
		Bootstrap::$STRINGLIT_665 = N::str("java.lang.ClassLoader");
		Bootstrap::$STRINGLIT_666 = N::str("java.lang._ClassInternalUtils");
		Bootstrap::$STRINGLIT_667 = N::str("java.lang._ClassInternalUtils\$1");
		Bootstrap::$STRINGLIT_668 = N::str("com.jtransc.JTranscArrays");
		Bootstrap::$STRINGLIT_669 = N::str("com.jtransc.JTranscSystemProperties");
		Bootstrap::$STRINGLIT_670 = N::str("Benchmark\$MyClass");
		Bootstrap::$STRINGLIT_671 = N::str("java.nio.IntBuffer");
		Bootstrap::$STRINGLIT_672 = N::str("java.nio.Buffer");
		Bootstrap::$STRINGLIT_673 = N::str("java.nio.internal.MemoryBlock");
		Bootstrap::$STRINGLIT_674 = N::str("java.nio.FloatBuffer");
		Bootstrap::$STRINGLIT_675 = N::str("java.nio.ByteBuffer");
		Bootstrap::$STRINGLIT_676 = N::str("java.nio.ReadOnlyBufferException");
		Bootstrap::$STRINGLIT_677 = N::str("java.nio.CharBuffer");
		Bootstrap::$STRINGLIT_678 = N::str("java.lang.Readable");
		Bootstrap::$STRINGLIT_679 = N::str("java.nio.DoubleBuffer");
		Bootstrap::$STRINGLIT_680 = N::str("java.nio.ShortBuffer");
		Bootstrap::$STRINGLIT_681 = N::str("java.nio.LongBuffer");
		Bootstrap::$STRINGLIT_682 = N::str("java.nio.ByteBufferAsFloatBuffer");
		Bootstrap::$STRINGLIT_683 = N::str("java.nio.internal.ByteBufferAs");
		Bootstrap::$STRINGLIT_684 = N::str("libcore.io.Memory");
		Bootstrap::$STRINGLIT_685 = N::str("java.nio.ByteOrder");
		Bootstrap::$STRINGLIT_686 = N::str("java.nio.ByteBufferAsFloatBuffer\$BE");
		Bootstrap::$STRINGLIT_687 = N::str("java.nio.ByteBufferAsFloatBuffer\$LE");
		Bootstrap::$STRINGLIT_688 = N::str("java.nio.ByteBufferAsLongBuffer");
		Bootstrap::$STRINGLIT_689 = N::str("java.nio.ByteBufferAsLongBuffer\$LE");
		Bootstrap::$STRINGLIT_690 = N::str("java.nio.ByteBufferAsLongBuffer\$1");
		Bootstrap::$STRINGLIT_691 = N::str("java.nio.ByteBufferAsLongBuffer\$BE");
		Bootstrap::$STRINGLIT_692 = N::str("java.nio.ByteBufferAsDoubleBuffer");
		Bootstrap::$STRINGLIT_693 = N::str("java.nio.ByteBufferAsDoubleBuffer\$LE");
		Bootstrap::$STRINGLIT_694 = N::str("java.nio.ByteBufferAsDoubleBuffer\$1");
		Bootstrap::$STRINGLIT_695 = N::str("java.nio.ByteBufferAsDoubleBuffer\$BE");
		Bootstrap::$STRINGLIT_696 = N::str("java.nio.ByteBufferAsCharBuffer");
		Bootstrap::$STRINGLIT_697 = N::str("java.nio.ByteBufferAsCharBuffer\$LE");
		Bootstrap::$STRINGLIT_698 = N::str("java.nio.ByteBufferAsCharBuffer\$1");
		Bootstrap::$STRINGLIT_699 = N::str("java.nio.ByteBufferAsCharBuffer\$BE");
		Bootstrap::$STRINGLIT_700 = N::str("java.nio.ByteBufferAsShortBuffer");
		Bootstrap::$STRINGLIT_701 = N::str("java.nio.ByteBufferAsShortBuffer\$LE");
		Bootstrap::$STRINGLIT_702 = N::str("java.nio.ByteBufferAsShortBuffer\$1");
		Bootstrap::$STRINGLIT_703 = N::str("java.nio.ByteBufferAsShortBuffer\$BE");
		Bootstrap::$STRINGLIT_704 = N::str("java.nio.ByteBufferAsIntBuffer");
		Bootstrap::$STRINGLIT_705 = N::str("java.nio.ByteBufferAsIntBuffer\$BE");
		Bootstrap::$STRINGLIT_706 = N::str("java.nio.ByteBufferAsIntBuffer\$LE");
		Bootstrap::$STRINGLIT_707 = N::str("java.util.zip.CRC32");
		Bootstrap::$STRINGLIT_708 = N::str("java.util.zip.Checksum");
		Bootstrap::$STRINGLIT_709 = N::str("com.jtransc.compression.jzlib.Deflate\$Config");
		Bootstrap::$STRINGLIT_710 = N::str("java.util.zip.Deflater");
		Bootstrap::$STRINGLIT_711 = N::str("Benchmark\$Test1");
		Bootstrap::$STRINGLIT_712 = N::str("Benchmark\$Test2");
		Bootstrap::$STRINGLIT_713 = N::str("java.util.Properties");
		Bootstrap::$STRINGLIT_714 = N::str("java.util.Hashtable");
		Bootstrap::$STRINGLIT_715 = N::str("java.util.Dictionary");
		Bootstrap::$STRINGLIT_716 = N::str("java.util.Hashtable\$HashtableEntry");
		Bootstrap::$STRINGLIT_717 = N::str("java.util.Hashtable\$1");
		Bootstrap::$STRINGLIT_718 = N::str("java.util.Hashtable\$EntrySet");
		Bootstrap::$STRINGLIT_719 = N::str("java.util.Hashtable\$EntryIterator");
		Bootstrap::$STRINGLIT_720 = N::str("java.util.Hashtable\$HashIterator");
		Bootstrap::$STRINGLIT_721 = N::str("java.io.ObjectStreamField");
		Bootstrap::$STRINGLIT_722 = N::str("java.lang.ref.WeakReference");
		Bootstrap::$STRINGLIT_723 = N::str("java.lang.ref.Reference");
		Bootstrap::$STRINGLIT_724 = N::str("java.lang.ref.ReferenceQueue");
		Bootstrap::$STRINGLIT_725 = N::str("java.lang.SafeVarargs\$Impl");
		Bootstrap::$STRINGLIT_726 = N::str("java.lang.annotation.Annotation\$Impl");
		Bootstrap::$STRINGLIT_727 = N::str("Array.newInstance");
		Bootstrap::$STRINGLIT_728 = N::str("[");
		Bootstrap::$STRINGLIT_729 = N::str("Invalid Array of void type");
		Bootstrap::$STRINGLIT_730 = N::str("Invalid Array.newInstance with ");
		Bootstrap::$STRINGLIT_731 = N::str(", Size: ");
		Bootstrap::$STRINGLIT_732 = N::str("Index: ");
		Bootstrap::$STRINGLIT_733 = N::str("Can't read more");
		Bootstrap::$STRINGLIT_734 = N::str(" but found end");
		Bootstrap::$STRINGLIT_735 = N::str("Expected ");
		Bootstrap::$STRINGLIT_736 = N::str("byte");
		Bootstrap::$STRINGLIT_737 = N::str("boolean");
		Bootstrap::$STRINGLIT_738 = N::str("true");
		Bootstrap::$STRINGLIT_739 = N::str("false");
		Bootstrap::$STRINGLIT_740 = N::str("short");
		Bootstrap::$STRINGLIT_741 = N::str("long");
		Bootstrap::$STRINGLIT_742 = N::str("0");
		Bootstrap::$STRINGLIT_743 = N::str("-9223372036854775808");
		Bootstrap::$STRINGLIT_744 = N::str("-");
		Bootstrap::$STRINGLIT_745 = N::str("double");
		Bootstrap::$STRINGLIT_746 = N::str("NaN");
		Bootstrap::$STRINGLIT_747 = N::str("-Infinity");
		Bootstrap::$STRINGLIT_748 = N::str("Infinity");
		Bootstrap::$STRINGLIT_749 = N::str("-0.0");
		Bootstrap::$STRINGLIT_750 = N::str("e+");
		Bootstrap::$STRINGLIT_751 = N::str("E");
		Bootstrap::$STRINGLIT_752 = N::str("e-");
		Bootstrap::$STRINGLIT_753 = N::str("E-");
		Bootstrap::$STRINGLIT_754 = N::str(".0");
		Bootstrap::$STRINGLIT_755 = N::str("float");
		Bootstrap::$STRINGLIT_756 = N::str("void");
		Bootstrap::$STRINGLIT_757 = N::str("' of ");
		Bootstrap::$STRINGLIT_758 = N::str("Can't parse type '");
		Bootstrap::$STRINGLIT_759 = N::str("'");
		Bootstrap::$STRINGLIT_760 = N::str("Class_forName0: Can't find class '");
		Bootstrap::$STRINGLIT_761 = N::str("char");
		Bootstrap::$STRINGLIT_762 = N::str("Invalid radix");
		Bootstrap::$STRINGLIT_763 = N::str("int");
		Bootstrap::$STRINGLIT_764 = N::str("public ");
		Bootstrap::$STRINGLIT_765 = N::str("protected ");
		Bootstrap::$STRINGLIT_766 = N::str("private ");
		Bootstrap::$STRINGLIT_767 = N::str("abstract ");
		Bootstrap::$STRINGLIT_768 = N::str("static ");
		Bootstrap::$STRINGLIT_769 = N::str("final ");
		Bootstrap::$STRINGLIT_770 = N::str("transient ");
		Bootstrap::$STRINGLIT_771 = N::str("volatile ");
		Bootstrap::$STRINGLIT_772 = N::str("synchronized ");
		Bootstrap::$STRINGLIT_773 = N::str("native ");
		Bootstrap::$STRINGLIT_774 = N::str("strictfp ");
		Bootstrap::$STRINGLIT_775 = N::str("interface ");
		Bootstrap::$STRINGLIT_776 = N::str("class ");
		Bootstrap::$STRINGLIT_777 = N::str("L");
		Bootstrap::$STRINGLIT_778 = N::str(";");
		Bootstrap::$STRINGLIT_779 = N::str("Couldn't find class ");
		Bootstrap::$STRINGLIT_780 = N::str("Class constructor: Can't find class '");
		Bootstrap::$STRINGLIT_781 = N::str(" with parameters ");
		Bootstrap::$STRINGLIT_782 = N::str("Can't find constructor of class ");
		Bootstrap::$STRINGLIT_783 = N::str("out == null");
		Bootstrap::$STRINGLIT_784 = N::str("os.version");
		Bootstrap::$STRINGLIT_785 = N::str("0.1");
		Bootstrap::$STRINGLIT_786 = N::str("java.runtime.name");
		Bootstrap::$STRINGLIT_787 = N::str("jtransc-unknown");
		Bootstrap::$STRINGLIT_788 = N::str("java.version");
		Bootstrap::$STRINGLIT_789 = N::str("1.8.0_51");
		Bootstrap::$STRINGLIT_790 = N::str("java.vm.version");
		Bootstrap::$STRINGLIT_791 = N::str("25.51-b03");
		Bootstrap::$STRINGLIT_792 = N::str("java.runtime.version");
		Bootstrap::$STRINGLIT_793 = N::str("1.8.0_51-b16");
		Bootstrap::$STRINGLIT_794 = N::str("file.separator");
		Bootstrap::$STRINGLIT_795 = N::str("line.separator");
		Bootstrap::$STRINGLIT_796 = N::str("path.separator");
		Bootstrap::$STRINGLIT_797 = N::str("file.encoding");
		Bootstrap::$STRINGLIT_798 = N::str("java.specification.name");
		Bootstrap::$STRINGLIT_799 = N::str("java.specification.vendor");
		Bootstrap::$STRINGLIT_800 = N::str("jtransc");
		Bootstrap::$STRINGLIT_801 = N::str("java.specification.version");
		Bootstrap::$STRINGLIT_802 = N::str("1.7");
		Bootstrap::$STRINGLIT_803 = N::str("java.vendor");
		Bootstrap::$STRINGLIT_804 = N::str("java.vendor.url");
		Bootstrap::$STRINGLIT_805 = N::str("http://github.com/jtransc/jtransc");
		Bootstrap::$STRINGLIT_806 = N::str("java.vm.name");
		Bootstrap::$STRINGLIT_807 = N::str("haxe");
		Bootstrap::$STRINGLIT_808 = N::str("java.vm.specification.name");
		Bootstrap::$STRINGLIT_809 = N::str("Jtransc JVM emulator");
		Bootstrap::$STRINGLIT_810 = N::str("java.vm.specification.vendor");
		Bootstrap::$STRINGLIT_811 = N::str("java.vm.specification.version");
		Bootstrap::$STRINGLIT_812 = N::str("java.io.tmpdir");
		Bootstrap::$STRINGLIT_813 = N::str("user.home");
		Bootstrap::$STRINGLIT_814 = N::str("user.dir");
		Bootstrap::$STRINGLIT_815 = N::str("user.name");
		Bootstrap::$STRINGLIT_816 = N::str("user.language");
		Bootstrap::$STRINGLIT_817 = N::str("user.region");
		Bootstrap::$STRINGLIT_818 = N::str("user.variant");
		Bootstrap::$STRINGLIT_819 = N::str("null");
		Bootstrap::$STRINGLIT_820 = N::str(" - ");
		Bootstrap::$STRINGLIT_821 = N::str("JTransc ");
		Bootstrap::$STRINGLIT_822 = N::str("Java ");
		Bootstrap::$STRINGLIT_823 = N::str(", totalMemory: ");
		Bootstrap::$STRINGLIT_824 = N::str(", maxMemory: ");
		Bootstrap::$STRINGLIT_825 = N::str("freeMemory: ");
		Bootstrap::$STRINGLIT_826 = N::str("Benchmarking:");
		Bootstrap::$STRINGLIT_827 = N::str("plain loops");
		Bootstrap::$STRINGLIT_828 = N::str("shift left constant");
		Bootstrap::$STRINGLIT_829 = N::str("shift right constant");
		Bootstrap::$STRINGLIT_830 = N::str("shift unsigned right constant");
		Bootstrap::$STRINGLIT_831 = N::str("shift left constant long");
		Bootstrap::$STRINGLIT_832 = N::str("shift right constant long");
		Bootstrap::$STRINGLIT_833 = N::str("shift unsigned right constant long");
		Bootstrap::$STRINGLIT_834 = N::str("left shift");
		Bootstrap::$STRINGLIT_835 = N::str("right shift");
		Bootstrap::$STRINGLIT_836 = N::str("right unsigned shift");
		Bootstrap::$STRINGLIT_837 = N::str("call static mult");
		Bootstrap::$STRINGLIT_838 = N::str("call instance mult");
		Bootstrap::$STRINGLIT_839 = N::str("call instance div");
		Bootstrap::$STRINGLIT_840 = N::str("instanceof classes");
		Bootstrap::$STRINGLIT_841 = N::str("arraycopy int");
		Bootstrap::$STRINGLIT_842 = N::str("write byte[]");
		Bootstrap::$STRINGLIT_843 = N::str("write short[]");
		Bootstrap::$STRINGLIT_844 = N::str("write char[]");
		Bootstrap::$STRINGLIT_845 = N::str("write int[]");
		Bootstrap::$STRINGLIT_846 = N::str("write float[]");
		Bootstrap::$STRINGLIT_847 = N::str("write double[]");
		Bootstrap::$STRINGLIT_848 = N::str("String Builder 1");
		Bootstrap::$STRINGLIT_849 = N::str("String Builder 2");
		Bootstrap::$STRINGLIT_850 = N::str("long arithmetic");
		Bootstrap::$STRINGLIT_851 = N::str("simd mutable");
		Bootstrap::$STRINGLIT_852 = N::str("simd immutable");
		Bootstrap::$STRINGLIT_853 = N::str("simd mutable matrix mult");
		Bootstrap::$STRINGLIT_854 = N::str("StringBuilder1");
		Bootstrap::$STRINGLIT_855 = N::str("StringBuilder2");
		Bootstrap::$STRINGLIT_856 = N::str("Non Direct Buffer");
		Bootstrap::$STRINGLIT_857 = N::str("Direct Buffer Int/float");
		Bootstrap::$STRINGLIT_858 = N::str("Direct Buffer Short/Char");
		Bootstrap::$STRINGLIT_859 = N::str("Direct Buffer Double/Long");
		Bootstrap::$STRINGLIT_860 = N::str("FastMemory");
		Bootstrap::$STRINGLIT_861 = N::str("Create Instances1 local");
		Bootstrap::$STRINGLIT_862 = N::str("Create Instances2 local");
		Bootstrap::$STRINGLIT_863 = N::str("Create Instances2 global");
		Bootstrap::$STRINGLIT_864 = N::str("Create Instances with builder");
		Bootstrap::$STRINGLIT_865 = N::str("Java's CRC32");
		Bootstrap::$STRINGLIT_866 = N::str("jzlib's CRC32");
		Bootstrap::$STRINGLIT_867 = N::str("compress java's Deflate");
		Bootstrap::$STRINGLIT_868 = N::str("compress jzlib");
		Bootstrap::$STRINGLIT_869 = N::str("random");
		Bootstrap::$STRINGLIT_870 = N::str("exception");
		Bootstrap::$STRINGLIT_871 = N::str("TOTAL time: ");
		Bootstrap::$STRINGLIT_872 = N::str("...");
		Bootstrap::$STRINGLIT_873 = N::str("ERROR");
	}
	static public function main(array $args) {
		try {
			N::init();
			Bootstrap::__initStrings();
			java_lang_Object::SI();
			Benchmark::SI();
			java_lang_String_CaseInsensitiveComparator::SI();
			java_lang_String::SI();
			java_io_Serializable_IFields::SI();
			java_lang_Comparable_IFields::SI();
			java_lang_CharSequence_IFields::SI();
			java_util_Comparator_IFields::SI();
			java_lang_String_1::SI();
			java_lang_StringBuilder::SI();
			java_lang_Appendable_IFields::SI();
			java_util_Arrays::SI();
			java_lang_System_1::SI();
			java_io_InputStream::SI();
			com_jtransc_io_JTranscConsolePrintStream::SI();
			com_jtransc_io_JTranscConsolePrintStream_ConsoleErrorStream::SI();
			java_io_OutputStream::SI();
			com_jtransc_io_JTranscConsolePrintStream_ConsoleBaseStream::SI();
			com_jtransc_io_JTranscConsolePrintStream_ConsoleOutputStream::SI();
			java_io_FilterOutputStream::SI();
			java_lang_NullPointerException::SI();
			java_lang_Throwable::SI();
			java_lang_Exception::SI();
			java_lang_RuntimeException::SI();
			java_io_PrintStream::SI();
			java_lang_System::SI();
			java_io_Closeable_IFields::SI();
			java_lang_AutoCloseable_IFields::SI();
			java_io_Flushable_IFields::SI();
			java_lang_jtransc_JTranscStrings::SI();
			java_lang_Math::SI();
			java_lang_IndexOutOfBoundsException::SI();
			java_lang_ArrayIndexOutOfBoundsException::SI();
			java_lang_IllegalArgumentException::SI();
			java_lang_Class::SI();
			java_lang_reflect_Type_IFields::SI();
			java_lang_reflect_GenericDeclaration_IFields::SI();
			java_lang_reflect_AnnotatedElement_IFields::SI();
			java_lang_AnnotatedElement_IFields::SI();
			java_lang_reflect_Modifier::SI();
			java_lang_Number::SI();
			java_lang_Integer::SI();
			java_lang_IntegerTools::SI();
			java_lang_Character::SI();
			java_lang_reflect_AccessibleObject::SI();
			java_lang_reflect_Field::SI();
			java_lang_reflect_Member_IFields::SI();
			java_lang_reflect__InternalUtils::SI();
			java_lang_ReflectiveOperationException::SI();
			java_lang_ClassNotFoundException::SI();
			java_lang_Void::SI();
			java_lang_Float::SI();
			com_jtransc_text_JTranscStringTools::SI();
			java_lang_Double::SI();
			com_jtransc_ds_FastStringMap::SI();
			java_lang_Long::SI();
			com_jtransc_internal_JTranscCType::SI();
			java_lang_Short::SI();
			com_jtransc_io_JTranscConsole::SI();
			java_lang_Boolean::SI();
			java_lang_Byte::SI();
			java_lang_reflect_MethodConstructor::SI();
			java_lang_reflect_Method::SI();
			j_MemberInfo::SI();
			java_lang_reflect_MethodTypeImpl::SI();
			com_jtransc_text_MStringReader::SI();
			java_lang_Error::SI();
			java_lang_reflect_ArrayType::SI();
			java_util_AbstractCollection::SI();
			java_util_AbstractList::SI();
			java_util_ArrayList::SI();
			java_util_List_IFields::SI();
			java_util_Collection_IFields::SI();
			java_lang_Iterable_IFields::SI();
			java_util_RandomAccess_IFields::SI();
			java_lang_Cloneable_IFields::SI();
			java_util_Iterator_IFields::SI();
			java_util_AbstractList_SimpleListIterator::SI();
			java_util_ListIterator_IFields::SI();
			java_util_AbstractList_FullListIterator::SI();
			java_util_NoSuchElementException::SI();
			java_util_ConcurrentModificationException::SI();
			java_lang_reflect_Array::SI();
			java_lang_jtransc_JTranscCoreReflection::SI();
			j_ProgramReflection::SI();
			j_ClassInfo::SI();
			j_ProgramReflection_AllClasses::SI();
			java_lang_UnsupportedOperationException::SI();
			java_lang_reflect_ParameterizedTypeImpl::SI();
			java_lang_reflect_ParameterizedType_IFields::SI();
			java_lang_StackTraceElement::SI();
			java_lang_CloneNotSupportedException::SI();
			j_ProgramReflection_DynamicGetSet::SI();
			j_ProgramReflection_AllFields::SI();
			java_lang_NoSuchMethodException::SI();
			java_lang_InstantiationException::SI();
			java_lang_reflect_Constructor::SI();
			java_lang_reflect_InvocationTargetException::SI();
			j_ProgramReflection_DynamicNewInvoke::SI();
			java_lang_SystemInt::SI();
			java_util_Objects::SI();
			j_ProgramReflection_AllConstructors::SI();
			java_util_Arrays_ArrayList::SI();
			java_lang_SafeVarargs_IFields::SI();
			java_lang_annotation_Annotation_IFields::SI();
			java_lang_ClassCastException::SI();
			Benchmark_37::SI();
			Benchmark_Task_IFields::SI();
			Benchmark_MyClass2::SI();
			Benchmark_36::SI();
			Benchmark_35::SI();
			Benchmark_34::SI();
			com_jtransc_JTranscSystem::SI();
			Benchmark_39::SI();
			Benchmark_38::SI();
			java_util_Random::SI();
			Benchmark_33::SI();
			Benchmark_32::SI();
			Benchmark_31::SI();
			Benchmark_30::SI();
			java_lang_Runtime::SI();
			Benchmark_40::SI();
			Benchmark_1::SI();
			Benchmark_2::SI();
			Benchmark_3::SI();
			Benchmark_44::SI();
			Benchmark_4::SI();
			Benchmark_43::SI();
			Benchmark_5::SI();
			Benchmark_42::SI();
			Benchmark_6::SI();
			Benchmark_41::SI();
			Benchmark_7::SI();
			Benchmark_8::SI();
			Benchmark_9::SI();
			Benchmark_15::SI();
			Benchmark_14::SI();
			Benchmark_13::SI();
			Benchmark_12::SI();
			Benchmark_19::SI();
			Benchmark_18::SI();
			Benchmark_17::SI();
			Benchmark_16::SI();
			Benchmark_11::SI();
			Benchmark_10::SI();
			Benchmark_26::SI();
			Benchmark_25::SI();
			Benchmark_24::SI();
			Benchmark_23::SI();
			Benchmark_29::SI();
			Benchmark_28::SI();
			Benchmark_27::SI();
			Benchmark_22::SI();
			Benchmark_21::SI();
			Benchmark_20::SI();
			com_jtransc_JTranscVersion::SI();
			com_jtransc_time_JTranscClock_1::SI();
			com_jtransc_time_JTranscClock_Impl::SI();
			com_jtransc_time_JTranscClock::SI();
			java_io_IOException::SI();
			com_jtransc_charset_JTranscCharset::SI();
			java_io_ByteArrayOutputStream::SI();
			com_jtransc_charset_JTranscCharBuffer::SI();
			java_nio_charset_UnsupportedCharsetException::SI();
			java_util_ServiceLoader::SI();
			com_jtransc_charset_JTranscCharsetSingleByte::SI();
			com_jtransc_charset_charsets_JTranscCharsetIBM866::SI();
			java_util_Map_IFields::SI();
			java_util_AbstractMap::SI();
			java_util_HashMap_HashMapEntry::SI();
			java_util_HashMap::SI();
			java_util_Map_Entry_IFields::SI();
			java_util_Set_IFields::SI();
			java_util_HashMap_1::SI();
			java_util_AbstractSet::SI();
			java_util_HashMap_EntrySet::SI();
			java_util_HashMap_HashIterator::SI();
			java_util_HashMap_EntryIterator::SI();
			java_lang_AssertionError::SI();
			java_util_Collections_1::SI();
			java_util_Collections_2::SI();
			java_util_Collections_EmptyList::SI();
			java_util_Collections_EmptySet::SI();
			java_util_Collections_EmptyMap::SI();
			java_util_Collections::SI();
			java_util_Enumeration_IFields::SI();
			com_jtransc_charset_charsets_JTranscCharsetUTF8::SI();
			com_jtransc_charset_charsets_JTranscCharsetUSASCII::SI();
			com_jtransc_charset_charsets_JTranscCharsetUTF16Base::SI();
			com_jtransc_charset_charsets_JTranscCharsetUTF16LE::SI();
			com_jtransc_JTranscBits::SI();
			com_jtransc_charset_charsets_JTranscCharsetLatin1::SI();
			com_jtransc_charset_charsets_JTranscCharsetUTF16BE::SI();
			java_lang_Thread::SI();
			java_lang_Runnable_IFields::SI();
			java_lang_ThreadGroup::SI();
			java_lang_Thread_UncaughtExceptionHandler_IFields::SI();
			java_lang_ClassLoader::SI();
			java_lang__ClassInternalUtils::SI();
			java_lang__ClassInternalUtils_1::SI();
			com_jtransc_JTranscArrays::SI();
			com_jtransc_JTranscSystemProperties::SI();
			Benchmark_MyClass::SI();
			com_jtransc_FastMemory::SI();
			java_nio_Buffer::SI();
			java_nio_IntBuffer::SI();
			java_nio_internal_MemoryBlock::SI();
			java_nio_FloatBuffer::SI();
			java_nio_ByteBuffer::SI();
			java_nio_ReadOnlyBufferException::SI();
			java_nio_CharBuffer::SI();
			java_lang_Readable_IFields::SI();
			java_nio_DoubleBuffer::SI();
			java_nio_ShortBuffer::SI();
			java_nio_LongBuffer::SI();
			java_nio_ByteBufferAsFloatBuffer::SI();
			java_nio_internal_ByteBufferAs_IFields::SI();
			java_nio_ByteOrder::SI();
			libcore_io_Memory::SI();
			java_nio_ByteBufferAsFloatBuffer_BE::SI();
			java_nio_ByteBufferAsFloatBuffer_LE::SI();
			java_nio_ByteBufferAsLongBuffer::SI();
			java_nio_ByteBufferAsLongBuffer_LE::SI();
			java_nio_ByteBufferAsLongBuffer_1::SI();
			java_nio_ByteBufferAsLongBuffer_BE::SI();
			java_nio_ByteBufferAsDoubleBuffer::SI();
			java_nio_ByteBufferAsDoubleBuffer_LE::SI();
			java_nio_ByteBufferAsDoubleBuffer_1::SI();
			java_nio_ByteBufferAsDoubleBuffer_BE::SI();
			java_nio_ByteBufferAsCharBuffer::SI();
			java_nio_ByteBufferAsCharBuffer_LE::SI();
			java_nio_ByteBufferAsCharBuffer_1::SI();
			java_nio_ByteBufferAsCharBuffer_BE::SI();
			java_nio_ByteBufferAsShortBuffer::SI();
			java_nio_ByteBufferAsShortBuffer_LE::SI();
			java_nio_ByteBufferAsShortBuffer_1::SI();
			java_nio_ByteBufferAsShortBuffer_BE::SI();
			java_nio_ByteBufferAsIntBuffer::SI();
			java_nio_ByteBufferAsIntBuffer_BE::SI();
			java_nio_ByteBufferAsIntBuffer_LE::SI();
			java_util_zip_CRC32::SI();
			java_util_zip_Checksum_IFields::SI();
			com_jtransc_compression_jzlib_CRC32::SI();
			com_jtransc_compression_jzlib_Checksum_IFields::SI();
			com_jtransc_compression_jzlib_ZStream::SI();
			com_jtransc_compression_jzlib_Deflater::SI();
			com_jtransc_compression_jzlib_Deflate_Config::SI();
			com_jtransc_compression_jzlib_Deflate::SI();
			com_jtransc_compression_jzlib_Tree::SI();
			com_jtransc_compression_jzlib_GZIPHeader::SI();
			com_jtransc_compression_jzlib_StaticTree::SI();
			com_jtransc_compression_jzlib_GZIPException::SI();
			com_jtransc_compression_jzlib_Adler32::SI();
			java_util_zip_Deflater::SI();
			Benchmark_Test1::SI();
			Benchmark_Test2::SI();
			com_jtransc_simd_Simd::SI();
			com_jtransc_simd_Float32x4::SI();
			com_jtransc_simd_MutableFloat32x4::SI();
			com_jtransc_simd_MutableFloat32x4Utils::SI();
			com_jtransc_simd_MutableMatrixFloat32x4x4::SI();
			com_jtransc_simd_MutableMatrixFloat32x4x4Utils::SI();
			java_util_Dictionary::SI();
			java_util_Hashtable_HashtableEntry::SI();
			java_io_ObjectStreamField::SI();
			java_lang_ref_WeakReference::SI();
			java_lang_ref_Reference::SI();
			java_util_Hashtable::SI();
			java_util_Properties::SI();
			java_util_Hashtable_1::SI();
			java_util_Hashtable_EntrySet::SI();
			java_util_Hashtable_HashIterator::SI();
			java_util_Hashtable_EntryIterator::SI();
			java_lang_ref_ReferenceQueue::SI();
			java_lang_SafeVarargs_Impl::SI();
			java_lang_annotation_Annotation_Impl::SI();
			Benchmark::main__Ljava_lang_String__V(N::strArray($args));
		} catch (WrappedThrowable $e) {
			echo $e->t, "\n";
			echo $e, "\n";
		} catch (Throwable $e) {
			echo $e, "\n";
		}
	}
}
Bootstrap::main([]);
