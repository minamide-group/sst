package deterministic.boundedcopy.composition

import deterministic.boundedcopy.SST

case class MonoidSST[Q, A, B, X, Y](
                                     sst: SST[Q, A, Map[Y, List[Either[Y, B]]], X],
                                     vars2: Set[Y],
                                     final2: Map[Q, List[Either[Y, B]]]) {
                                     }
