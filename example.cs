class TestProgram {
    // task 9 print func works
    void testPrint() {
        print(42);
        print(1, 2, 3);
    }
    
    // task 4 rightassociative assignment
    void testAssignment() {
        int a;
        int b;
        int c;
        a = b = c = 5;
        print(a, b, c);
    }
    
    // task 2 operator precedence 
    void testPrecedence() {
        int result;
        result = 2 + 3 * 4;  // should be 14 not 20
        print(result);
    }
    
    // task 7 lazy eval of logical ops
    void testLazyEval() {
        bool result;
        result = false && true;  // should shortcircuit
        print(result);
        result = true || false;  // should shortcircuit
        print(result);
    }
    
    // task 5 for loops
    void testForLoop() {
        for (int i, i = 0; i < 5; i = i + 1) {
            print(i);
        }
    }
    
    // task 6 local vars with scoping
    void testLocalVars() {
        int x;
        x = 10;
        {
            int y;
            y = 20;
            print(x, y);  // both visible
        }
        print(x);  // y no longer visible
    }
    
    // task 8 func with params
    int add(int a, int b) {
        return a + b;
    }

    // task 10 func with ret val
    int square(int x) {
        return x * x;
    }
    
    void testFunctions() {
        int result;
        result = add(3, 4);
        print(result);
        result = square(5);
        print(result);
    }
    
    // main entry point
    void main() {
        testPrint();
        testAssignment();
        testPrecedence();
        testLazyEval();
        testForLoop();
        testLocalVars();
        testFunctions();
    }
}