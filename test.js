function sum() {
        var m = 0;
        for (n = 0; n < 1000000; n++) {
                m = (m + n) | 0;
        }
        return m;
}

function main() {
        start = Date.now();
        for (var m = 0; m < 10; m++) sum();
        end = Date.now();
        console.log(end - start);
}

main();
main();
main();
main();
