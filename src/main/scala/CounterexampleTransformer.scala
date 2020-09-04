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
    var newOldHeap: Option[Seq[Chunk]] = if (ce.oldHeap.isDefined) Some(Seq()) else None // ce.oldHeap
    var newPrimeOldHeap: Option[Seq[Chunk]] = if (ce.oldHeap.isDefined) Some(Seq()) else None // ce.oldHeap
    println("ON LOOKUP, names for method " + methodName)
    println("ARE " + primedNamesPerMethod.get(methodName).get)
    for ((n, np) <- primedNamesPerMethod.get(methodName).get) {
      println("name in method " + n)
      if (ce.store.contains(n)) {
        println("FOUND A VAR IN STORE " + n)
        // this is a variable
        val variableEntry = ce.internalStore.values.find(_._1.name == n).get
        newStore = newStore.+(variableEntry)
        val variableEntryPrime = ce.internalStore.values.find(_._1.name == np).get
        newPrimeStore = newPrimeStore.+((LocalVar(n, variableEntry._1.typ)(), variableEntryPrime._2))
      }
    }
    for ((n, np) <- primedNames) {
      println("primed name" + n)

      for (c <- ce.heap) {
        c match {
          case BasicChunk(resourceID, id, args, snap, perm) => {
            if (id.name == n) {
              newHeap ++= Seq(c)
            }else if (id.name == np) {
              newPrimeHeap ++= Seq(BasicChunk(resourceID, BasicChunkIdentifier(n), args, snap, perm))
            }
          }
          case _ =>
        }
      }
      if (ce.oldHeap.isDefined) {
        for (c <- ce.oldHeap.get) {
          c match {
            case BasicChunk(resourceID, id, args, snap, perm) => {
              if (id.name == n) {
                newOldHeap = Some(newOldHeap.get ++ Seq(c))
              } else if (id.name == np) {
                newPrimeOldHeap = Some(newPrimeOldHeap.get ++ Seq(BasicChunk(resourceID, BasicChunkIdentifier(n), args, snap, perm)))
              }
            }
            case _ =>
          }
        }
      }

    }
    SiliconNativeSIFCounterexample(newStore, newHeap, newOldHeap, ce.model, SiliconNativeCounterexample(newPrimeStore, newPrimeHeap, newPrimeOldHeap, ce.model))
  }
}

object CounterexampleSIFTransformerO extends CounterexampleSIFTransformer
