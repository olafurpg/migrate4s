package scalafix.nsc

import scala.collection.mutable
import scala.util.Try

/** A map where `remove` and `-=` do nothing, you need to call `customRemove` */
class NonRemovableMap[K, V](default: V) extends mutable.HashMap[K, V] {
  override def default(key: K): V = default
  def customRemove(key: AnyRef): Option[V] =
    Try(super.remove(key.asInstanceOf[K])).toOption.flatten
  // these are no-ops, call customRemove instead.
  override def remove(key: K): Option[V] = get(key)
  override def -=(key: K): this.type = this
}
