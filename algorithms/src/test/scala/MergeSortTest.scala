import org.scalatest._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers


class MergeSortTest extends FreeSpec with Checkers {

  import MergeSort._
  "For any randomly generated List of Ints, Scala's sort should equal our sort" in {
    check(forAll(Arbitrary.arbContainer[List, Int].arbitrary) { randomlyGeneratedList =>
      randomlyGeneratedList.sorted == MergeSort(UnsortedList(randomlyGeneratedList)).in
    })
  }
}
