// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2021 ETH Zurich.

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
