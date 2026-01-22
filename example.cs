// Test program for C# subset compiler

class TestProgram {
    // task 9 print function works
    void testPrint() {
        print(42);
        print(1, 2, 3);
    }
    
    // task 4 right-associative assignment
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
        result = 2 + 3 * 4;  // should be 14, not 20
        print(result);
    }
    
    // task 7 lazy evaluation of logical operators
    void testLazyEval() {
        bool result;
        result = false && true;  // should short-circuit
        print(result);
        result = true || false;  // should short-circuit
        print(result);
    }
    
    // task 5 for loops
    void testForLoop() {
        for (int i, i = 0; i < 5; i = i + 1) {
            print(i);
        }
    }
    
    // task 6 local variables with proper scoping
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
    
    // task 8 function with parameters
    int add(int a, int b) {
        return a + b;
    }

    // task 10 function with return value
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