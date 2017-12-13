# gecko
[![Build Status](https://travis-ci.org/rsoeldner/gecko.svg?branch=master)](https://travis-ci.org/rsoeldner/gecko)
[![Download](https://api.bintray.com/packages/rsoeldner/gecko/gecko/images/download.svg?version=0.0.1) ](https://bintray.com/rsoeldner/gecko/gecko/0.0.1/link)

array-backed and _predictable_ data manipulation library inspired by _saddle_ and _pandas_.

## dependency
We rely on 

| Name     | Version   |
| ---      | ---       |
| cats     | 1.0.0-RC1 |
| fs2-core | 0.10.0-M7 |
| fs2-io   | 0.10.0-M7 |


A `DataFrame` consists of a _row and column identifier_, specified by `FrameIndex` and a `DataMatrix` which consists of 
several `DataVector`'s.


## Examples

### DataFrame
Inplace construction:

```scala
import gecko.{DataFrame, DataMatrix, DataVector}

DataMatrix(
  DataVector(  1,   2,   3),
  DataVector( 10,  20,  30),
  DataVector(100, 200, 300)
).map(DataFrame.default(_))
```

Or with specific row and column identifier:
```scala
import gecko.{DataFrame, DataMatrix, DataVector}
import gecko.syntax._

DataMatrix(
  DataVector(  1,   2,   3),
  DataVector( 10,  20,  30),
  DataVector(100, 200, 300)
).map{
  DataFrame(
    List("A", "B", "C").toIndex,
    List("X1", "X2", "X3").toIndex,
    _)
}
```
 
Or with default ones:

```scala
import gecko.{DataFrame, DataMatrix, DataVector, FrameIndex}
import gecko.syntax._

DataMatrix(
  DataVector(  1,   2,   3),
  DataVector( 10,  20,  30),
  DataVector(100, 200, 300)
).map{
  DataFrame(
    FrameIndex.default(3),
    List("X1", "X2", "X3").toIndex,
    _)
}
```

Read from File:

```scala
import java.nio.file.Paths

import cats.effect.IO
import gecko.csv.GeckoCSVUtil

GeckoCSVUtil
  .parseFrame(fs2.io.file.readAll[IO](Paths.get("file.csv"), 4096))
  .unsafeRunSync()
```

## DataFrame

### Construction
If you need a specific row / column identifier, use the _apply_ method:
```scala
  def apply[R, C, @specialized(Int, Double, Boolean, Long) A: ClassTag : EmptyPrint](
      rowIx: FrameIndex[R],
      colIx: FrameIndex[C],
      values: DataMatrix[A]
  ): DataFrame[R, C, A]
```
Otherwise use _default_:

```scala
def default[@specialized(Int, Double, Boolean, Long) A: ClassTag: EmptyPrint](
      arr: DataMatrix[A]
  ): DataFrame[Int, Int, A]
```
Which numerates each row / column starting from 0.

## FrameIndex

### Construction
Use the _default_:
```scala
def default(size: Int): FrameIndex[Int]
```

Otherwise, specify the values:
```scala
def fromSeq[@specialized(Int, Double, Boolean, Long) C: ClassTag](c: Seq[C]): FrameIndex[C]
```

## DataVector
### Construction

from sequence of values:
```scala
 def apply[@specialized(Int, Double, Boolean, Long) A: ClassTag: EmptyGecko](values: A*): DataVector[A]
```

from array:
```scala
def fromArray[@specialized(Int, Double, Boolean, Long) A: ClassTag: EmptyGecko](
      array: Array[A]
  ): DataVector[A]
```


## DataMatrix
### Construction
_Gecko_ most of the time provides a safe and unsafe version (prefixed by unsafe):

from sequence of DataVector:
```scala
def apply[A](vectors: DataVector[A]*): Either[GeckoError, DataMatrix[A]]
```

From single array by specifing row and column size:
```scala
def fromArrayWithDim[A: ClassTag](rows: Int, cols: Int, values: Array[A]): Either[GeckoError, DataMatrix[A]]
```

From array or sequence of DataVector's:
```scala
def fromArray[A](vectors: Array[DataVector[A]]): Either[GeckoError, DataMatrix[A]]
def fromSeq[A](a: Seq[DataVector[A]]): Either[GeckoError, DataMatrix[A]]
```

Fill _n - times_ with constant rows:
```scala
def fill[A](n: Int, elem: DataVector[A]): DataMatrix[A]
```


# Thank You!
To my friend [Jose](https://github.com/jmcardon) 
