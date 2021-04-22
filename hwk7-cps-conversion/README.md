# cs738 HW#7
Continuation-Passing-Style (or CPS) conversion

## Requirements

- JDK version: Java 8 (scala 2.11.* needs JDK 8 to compile itself)
- Scala: 2.11.*
- SBT: 1.4.7


## Running the project

```
sbt compile
sbt run
```

Result:

```javascript
function fact(n, ret) {
    var a = 1.0;
    if (n > 1.0)
        fact(n - 1.0, function(temp6) {
            a = temp6;
        })

    ret(n * a);
}

fact(10.0, function(temp9) {
    var x = temp9;
    fact(y, function(temp10) {
        var y = temp10;
    });
});
```
