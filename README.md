# gecko
array-backed and _predictable_ data manipulation library inspired by _saddle_ and _pandas_.

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