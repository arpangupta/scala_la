def mat_add(m1 : Array[Array[Double]], m2 : Array[Array[Double]] ) = {
var res = Array.ofDim[Double](m1.length, m1(0).length)
for(i <- (0 until m1.length).par ; j <- (0 until m1(0).length).par){
res(i)(j) = m1(i)(j) + m2(i)(j)
}res}

