package viper.silicon.sif

import viper.silicon.interfaces.{SiliconCounterexample, SiliconNativeCounterexample}
import viper.silicon.interfaces.state.Chunk
import viper.silicon.state.Store
import viper.silver.verifier.Model

case class SiliconNativeSIFCounterexample(internalStore: Store, heap: Iterable[Chunk], oldHeap: Option[Iterable[Chunk]], model: Model, second: SiliconNativeCounterexample) extends SiliconCounterexample {
  override def withStore(s: Store): SiliconCounterexample = {
    SiliconNativeSIFCounterexample(s, heap, oldHeap, model, SiliconNativeCounterexample(s, second.heap, second.oldHeap, second.model))
  }
}
