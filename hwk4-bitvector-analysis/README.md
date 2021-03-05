# cs738 HW#4
Reaching Definition and Live variable Analysis using Bit Vector framework.

## Requirements

- JDK version: Java 8 (scala 2.11.* needs JDK 8 to compile itself)
- Scala: 2.11.*
- SBT: 1.4.7


## Running the project

```
sbt compile
sbt run
```

### Live variable analysis

```javascript
var x = 2;
var y = 4; 
var x = 1;

if (y>x) 
	z = y; 
else 
	z = y*y; 

x = z;
```

```
2    {}                                       {}
3    {}                                       {y}
4    {y}                                      {x, y}
5    {x, y}                                   {y}
6    {y}                                      {z}
7    {y}                                      {z}
8    {z}                                      {}
```

![lv-js](./lv-js.svg)


### Reaching Definition Analysis


```javascript
var x = 5;
var y = 1;

while (x > 1) {
	if(y < 100)  
		y = x * y; 
	x = x -1; 
}

x = y;
```

```
2    (x,-1) (y,-1)                            (x,2) (y,-1)
3    (x,2) (y,-1)                             (x,2) (y,3)
4    (x,2) (x,9) (y,3) (y,7)                  (x,2) (x,9) (y,3) (y,7)
6    (x,2) (x,9) (y,3) (y,7)                  (x,2) (x,9) (y,3) (y,7)
7    (x,2) (x,9) (y,3) (y,7)                  (x,2) (x,9) (y,7)
9    (x,2) (x,9) (y,3) (y,7)                  (x,9) (y,3) (y,7)
10   (x,2) (x,9) (y,3) (y,7)                  (x,10) (y,3) (y,7)
```

![rd1-js](./rd1-js.svg)