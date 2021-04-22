

## `fact.js`

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

## `fact2.js`
Before:

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

After:

```javascript
let halt = x => console.log(x)

function fact(n, k1) {
    let ret = 1.0
    let u4 = n > 1.0
    while (u4) {
        ret = ret * n
        n = n - 1.0
        k1(n)
    }
}

let k7 = x5 => {
    let x = x5
    fact(x, halt)
}
fact(3.0, k7)
```
## `fact3.js`

Before:
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

After:
```javascript
let halt = x => console.log(x)

function fact(n, k1) {
    let ret = 1.0
    let u4 = n > 1.0
    while (u4) {
        let k6 = x5 => {
            ret = ret * n
            n = n - 1.0
            k1(n)
        }
        let u7 = ret > 1000.0
        if (u7) {
            k1(null)
        } else {
            k6(null)
        }
    }
}

let k10 = x8 => {
    let x = x8
    fact(x, halt)
}
fact(3.0, k10)
```

## `fact4.js`

Before:
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

After:

```javascript
let halt = x => console.log(x)

function fact(n, k1) {
    let ret = 1.0
    let u4 = n > 1.0
    while (u4) {
        let k10 = x5 => {
            n = n - 1.0
            let k12 = x11 => {
                ret = ret * n
                k1(ret)
            }
            let u13 = n % 2.0
            let u14 = u13 == 0.0
            if (u14) {
                k1(null)
            } else {
                k12(null)
            }
        }
        let u15 = ret > 1000.0
        if (u15) {
            k1(null)
        } else {
            k10(null)
        }
    }
}

let k18 = x16 => {
    let x = x16
    fact(x, halt)
}
fact(3.0, k18)
```