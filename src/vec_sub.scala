def vec_add( v1: Array[Double], v2: Array[Double] ) = {
     var res = Array.ofDim[Double](v1.length)
     for(i <- (0 until v1.length).par){
           res(i) = v1(i) - v2(i)}
      res
}

