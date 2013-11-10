def mat_mult(m1: Array[Array[Double]], m2: Array[Array[Double]]) = {
	var res = Array.fill(m1.length, m2(0).length)(0.0)
	for(i <-( 0 until m1.length).par; j <- (0 until m2(0).length).par ; k <- (0 until m1(0).length).par){
	res(i)(j) += m1(i)(k)*m2(k)(j)
	}
	
	res
}
