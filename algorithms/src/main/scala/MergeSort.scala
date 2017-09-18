sealed trait SortableList[Int]

final case class SortedList[Int](in: List[Int]) extends SortableList[Int]
final case class UnsortedList[Int](in: List[Int]) extends SortableList[Int]


object MergeSort {

  def MergeSort(in: SortableList[Int]): SortedList[Int] =

    in match {
      case SortedList(ints) => SortedList(ints)
      case UnsortedList(ints) => ints match {

        case Nil => SortedList(Nil)
        case head :: Nil => SortedList(List(head))
        case head :: tail =>
          val lhs = SortedList(List(head))
          val rhs = MergeSort(UnsortedList(tail))
          Merge(lhs, rhs)
      }
    }

  def Merge(lhs: SortedList[Int], rhs: SortedList[Int]): SortedList[Int] =

    (lhs, rhs) match {

      case (left, SortedList(Nil)) => left
      case (SortedList(Nil), right) => right
      case (
        SortedList(leftHead :: leftTail),
        SortedList(rightHead :: rightTail)
        ) => SortedList(
          if (leftHead < rightHead)
            leftHead :: Merge(SortedList(leftTail), rhs).in
          else
            rightHead :: Merge(lhs, SortedList(rightTail)).in
      )
    }

}