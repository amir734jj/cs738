# cs738 HW#7
Interprocedural Finite Distributive Subset (IFDS) analysis to detect possibly uninitialized variables

## Requirements

- JDK version: Java 8 (scala 2.11.* needs JDK 8 to compile itself)
- Scala: 2.11.*
- SBT: 1.4.7


## Running the project

```
sbt compile
sbt run
```

- [x] `fact.js`
Matches expected result

Before
```javascript
function fact(n) {
    if (n <= 1) {
        return 1;
    } else {
        return n * fact(n - 1);
    }
}

var x = fact(3);

fact(x)
```

After
```javascript
let halt = x => console.log(x)

function fact(n, k1) {
    let u3 = n <= 1.0
    if (u3) {
        k1(1.0)
    } else {
        let k6 = x4 => {
            let u7 = n * x4
            k1(u7)
        }
        let u8 = n - 1.0
        fact(u8, k6)
    }
}

let k11 = x9 => {
    let x = x9
    fact(x, halt)
}
fact(3.0, k11)
```

- [x] `fact2.js`
Matches expected result but variable names are off by 1

Before

```javascript
function fact(n) {
    var ret = 1;
    while (n > 1) {
        ret = ret * n;
        n = n - 1;
    }
    return ret;
}

var x = fact(3);

fact(x)
```

After

```javascript
let halt = x => console.log(x)

function fact(n, k1) {
    let ret = 1.0
    let k4 = _ => {
        k1(ret)
    }
    let k3 = _ => {
        let u5 = n > 1.0
        if (u5) {
            ret = ret * n
            n = n - 1.0
            k3(n)
        } else {
            k1(ret)
        }
    }
    k3(null)
}

let k8 = x6 => {
    let x = x6
    fact(x, halt)
}
fact(3.0, k8)
```

- [x] `fact3.js`
Matches expected result but variable names are off by 1

Before
```javascript
function fact(n) {
    var ret = 1;
    while (n > 1) {
        if (ret > 1000) break;
        ret = ret * n;
        n = n - 1;
    }
    return ret;
}

var x = fact(3);

fact(x)
```

After
```javascript
let halt = x => console.log(x)

function fact(n, k1) {
    let ret = 1.0
    let k4 = _ => {
        k1(ret)
    }
    let k3 = _ => {
        let u5 = n > 1.0
        if (u5) {
            let k7 = x6 => {
                ret = ret * n
                n = n - 1.0
                k3(n)
            }
            let u8 = ret > 1000.0
            if (u8) {
                k4(null)
            } else {
                k7(null)
            }
        } else {
            k1(ret)
        }
    }
    k3(null)
}

let k11 = x9 => {
    let x = x9
    fact(x, halt)
}
fact(3.0, k11)
```

- [x] `fact4.js`
Matches expected result but variable names are off by 1

Before
```javascript
function fact(n) {
    var ret = 1;
    while (n > 1) {
        if (ret > 1000) break;
        n = n - 1;
        if (n % 2 == 0) continue;
        ret = ret * n;
    }
    return ret;
}

var x = fact(3);

fact(x)
```

After

```javascript
let halt = x => console.log(x)

function fact(n, k1) {
    let ret = 1.0
    let k4 = _ => {
        k1(ret)
    }
    let k3 = _ => {
        let u5 = n > 1.0
        if (u5) {
            let k11 = x6 => {
                n = n - 1.0
                let k13 = x12 => {
                    ret = ret * n
                    k3(ret)
                }
                let u14 = n % 2.0
                let u15 = u14 == 0.0
                if (u15) {
                    k3(null)
                } else {
                    k13(null)
                }
            }
            let u16 = ret > 1000.0
            if (u16) {
                k4(null)
            } else {
                k11(null)
            }
        } else {
            k1(ret)
        }
    }
    k3(null)
}

let k19 = x17 => {
    let x = x17
    fact(x, halt)
}
fact(3.0, k19)
```
