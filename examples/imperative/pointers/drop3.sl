import test_utils
process Main() {
Init:
Loop:


    var x : box<int>;
    while (1 < 0)
    {
        var y : box<int> = box(1);
        x = y
    }
    // Error x is not initialized as we don't enter the loop
    // print_int(*x); print_newline()
;
    exit(0);
}