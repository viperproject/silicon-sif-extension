// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2021 ETH Zurich.

package viper.silicon.sif

import viper.silicon.interfaces.SiliconNativeCounterexample
import viper.silicon.interfaces.state.Chunk
import viper.silicon.state.{BasicChunk, BasicChunkIdentifier, Store}
import viper.silver.ast.LocalVar
import viper.silver.sif.SIFExtendedTransformer

trait CounterexampleSIFTransformer extends SIFExtendedTransformer {
  def transformCounterexample(ce: SiliconNativeCounterexample, methodName: String) : SiliconNativeSIFCounterexample = {
    var newStore = Store() // ce.internalStore
    var newPrimeStore = Store() // ce.internalStore
    var newHeap: Seq[Chunk] = Seq() //ce.heap
    var newPrimeHeap: Seq[Chunk] = Seq() //ce.heap
    var newOldHeap: Option[Seq[Chunk]] = if (ce.oldHeaps.contains("old")) Some(Seq()) else None // ce.oldHeap
    var newPrimeOldHeap: Option[Seq[Chunk]] = if (ce.oldHeaps.contains("old")) Some(Seq()) else None // ce.oldHeap
    for ((n, np) <- primedNamesPerMethod.get(methodName).get) {
      if (ce.store.contains(n)) {
        // this is a variable
        val variableEntry = ce.internalStore.values.find(_._1.name == n).get
        newStore = newStore.+(variableEntry)
        val variableEntryPrime = ce.internalStore.values.find(_._1.name == np).get
        newPrimeStore = newPrimeStore.+((LocalVar(n, variableEntry._1.typ)(), variableEntryPrime._2))
      }
    }
    for ((n, np) <- primedNames) {

      for (c <- ce.heap) {
        c match {
          case BasicChunk(resourceID, id, args, argsExp, snap, snapExp, perm, permExp) => {
            if (id.name == n) {
              newHeap ++= Seq(c)
            }else if (id.name == np) {
              newPrimeHeap ++= Seq(BasicChunk(resourceID, BasicChunkIdentifier(n), args, argsExp, snap, snapExp, perm, permExp))
            }
          }
          case _ =>
        }
      }
      if (ce.oldHeaps.contains("old")) {
        for (c <- ce.oldHeaps("old")) {
          c match {
            case BasicChunk(resourceID, id, args, argsExp, snap, snapExp, perm, permExp) => {
              if (id.name == n) {
                newOldHeap = Some(newOldHeap.get ++ Seq(c))
              } else if (id.name == np) {
                newPrimeOldHeap = Some(newPrimeOldHeap.get ++ Seq(BasicChunk(resourceID, BasicChunkIdentifier(n), args, argsExp, snap, snapExp, perm, permExp)))
              }
            }
            case _ =>
          }
        }
      }

    }
    val newOldHeapMap: Map[String, Iterable[Chunk]] = newOldHeap match {
      case None => Map()
      case Some(chunks) => Map("old" -> chunks)
    }
    val newPrimeOldHeapMap: Map[String, Iterable[Chunk]] = newPrimeOldHeap match {
      case None => Map()
      case Some(chunks) => Map("old" -> chunks)
    }
    SiliconNativeSIFCounterexample(newStore, newHeap, newOldHeapMap, ce.model, SiliconNativeCounterexample(newPrimeStore, newPrimeHeap, newPrimeOldHeapMap, ce.model))
  }
}

object CounterexampleSIFTransformerO extends CounterexampleSIFTransformer
