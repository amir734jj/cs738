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

## Example #1

```javascript
function f(x) {
	return x;
}

var a = f(10);

var b = f(b);
```

![uv-js](./uv-js.png)

## Example #2

```javascript
function f(x) {
	var x = g(x);
	return x;
}

function g(y) { 
	return y;
}

var a = f(10);

var b = f(b);
```
![uv1-js](./uv1-js.png)

## Example #3

```javascript
function fact(n) {
	var a = 1;
	
	if (n > 1) a = fact(n-1);
	
	return n * a;
}

var x = fact(10);  

var y = fact(y); 
```
![uv1-js](./fact-js.png)