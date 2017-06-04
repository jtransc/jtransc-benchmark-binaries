<?php

function sum() {
	$m = 0;
	for ($n = 0; $n < 1000000; $n++) {
		$m = (int)($m + $n);
	}
	return $m;
}

function main() {
	$start = microtime(true);
	for ($m = 0; $m < 10; $m++) sum();
	$end = microtime(true);
	echo ($end - $start) * 1000, "\n";
}

main();
main();
main();
main();

