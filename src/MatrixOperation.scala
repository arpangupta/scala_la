import scala.math

//TODO:
//		1. Complete the Gaussian Elemination code to solve linear Equations
//		2. Finding Inverses




class ParallelLinearAlgbera
{
	//Matrix Addition

	def mat_add(m1: Array[Array[Double]],m2: Array[Array[Double]]) = {
		var res = Array.ofDim[Double](m1.length, m1(0).length);
		for(i <- (0 until m1.length).par ; j <- (0 until m1(0).length).par){
			res(i)(j) = m1(i)(j) + m2(i)(j)
		}
		res
	}
	//Matrix Multiplication
	
	def mat_mul(m1: Array[Array[Double]], m2: Array[Array[Double]]) = {
		var res = Array.fill(m1.length, m2(0).length)(0.0)
		for(i <-( 0 until m1.length).par; j <- (0 until m2(0).length).par ; k <- (0 until m1(0).length).par){
			res(i)(j) += m1(i)(k)*m2(k)(j)
		}
		res
	}

	//Matrix Subtraction
	
	def mat_sub(m1 : Array[Array[Double]], m2 : Array[Array[Double]] ) = {
		var res = Array.ofDim[Double](m1.length, m1(0).length)
		for(i <- (0 until m1.length).par ; j <- (0 until m1(0).length).par){
			res(i)(j) = m1(i)(j) - m2(i)(j)
		}
		res
	}
	//Matrix Transpose
	
	def mat_transpose(m1 : Array[Array[Double]]) = {
		var res = Array.ofDim[Double](m1(0).length,m1.length)
		for(i <- (0 until m1.length).par ; j <- (0 until m1(0).length).par){
			res(i)(j) = m1(j)(i) 
		}
		res
	}
	//Matrix Copy one to another 
	
	def mat_copy(m1 : Array[Array[Double]],m2 : Array[Array[Double]]) = {
		for(i <- (0 until m1.length).par ; j <- (0 until m1(0).length).par){
			m2(i)(j) = m1(i)(j) 
		}
		m2
	}

	//Matrix Swapping Rows .. Immutable doesnt change the original matrix

	def mat_swap_rows_immutable(m1: Array[Array[Double]],r1 : Int , r2 : Int) = {
		var res = Array.ofDim[Double](m1.length, m1(0).length)
		mat_copy(m1,res)
		for(i  <- (0 until res(0).length).par){
			var temp  = res(r1)(i) 
			res(r1)(i) = res(r2)(i) 
			res(r2)(i) = temp
		}
		res
	}

	//Matrix Swapping Column .. Immutable doesnt change the original matrix

	def mat_swap_cols_immutable(m1: Array[Array[Double]],c1: Int, c2 : Int) = {
		var res = Array.ofDim[Double](m1.length, m1(0).length)
		mat_copy(m1,res)
		for(i  <- (0 until res(0).length).par){
			var temp  = res(i)(c1) 
			res(i)(c1) = res(i)(c2) 
			res(i)(c2) = temp
		}
		res
	}

	//Matrix Swapping Rows .Changes the original Matrix
	def mat_swap_rows(m1: Array[Array[Double]],r1 : Int , r2 : Int) = {
		for(i  <- (0 until m1(0).length).par){
			var temp  = m1(r1)(i) 
			m1(r1)(i) = m1(r2)(i) 
			m1(r2)(i) = temp
		}
		m1
	}

	//Matrix Swapping Column .Changes the original Matrix

	def mat_swap_cols(m1: Array[Array[Double]],c1: Int, c2 : Int) = {
		for(i  <- (0 until m1(0).length).par){
			var temp  = m1(i)(c1) 
			m1(i)(c1) = m1(i)(c2) 
			m1(i)(c2) = temp
		}
		m1
	}

	//To be implemented still

	def mat_gauss_elemination(m1: Array[Array[Double]]) = {
		m1
	}

	def vec_add( v1: Array[Double], v2: Array[Double] ) = {
     	var res = Array.ofDim[Double](v1.length)
     	for(i <- (0 until v1.length).par){
           	res(i) = v1(i) + v2(i)
       	}
       	res
	}

	def vec_sub( v1: Array[Double], v2: Array[Double] ) = {
     	var res = Array.ofDim[Double](v1.length)
     	for(i <- (0 until v1.length).par){
           	res(i) = v1(i) - v2(i)
        }
      	res
	}
	def vec_mul( v1: Array[Double], v2: Array[Double] ) = {
     	var res = Array.ofDim[Double](v1.length)
     	for(i <- (0 until v1.length).par){
           	res(i) = v1(i) * v2(i)
        }
      	res
	}

	def vec_sum( v1: Array[Double]) = {
     	v1.par.fold(0.0)(_ + _)
	}

	def vec_magnitude( v1: Array[Double]) = {
     	var sum = 0.0
     	var res = Array.ofDim[Double](v1.length)
     	for(i <- (0 until v1.length).par){
     		res(i) =  v1(i) * v1(i)
        }
      	math.sqrt(vec_sum(res))
	}

	def vec_normalize( v1: Array[Double]) = {
     	val magnitude = vec_magnitude(v1)
     	var res = Array.ofDim[Double](v1.length)
     	for(i <- (0 until v1.length).par){
     		res(i) = v1(i) / magnitude
        }
      	res
	}
	def vec_dot( v1: Array[Double], v2: Array[Double] ) = {
      	vec_sum(vec_mul(v1,v2))
	}

	def vec_scalar_mul( v1: Array[Double], scalar: Double ) = {
     	var res = Array.ofDim[Double](v1.length)
		for( i <- (0 until v1.length).par){
			res(i) = v1(i) * scalar
		}
      	res
	}

}


var op = new ParallelLinearAlgbera()
var m1  = Array(Array(1.0,2.0,3.0),Array(4.0,5.0,6.0),Array(7.0,8.0,9.0))

var m2  = Array(Array(11.0,12.0,13.0),Array(14.0,15.0,16.0),Array(17.0,18.0,19.0))
println("Matrix 1" )
println(m1.deep.mkString("\n"))
println()

println("Matrix 2" )
println(m2.deep.mkString("\n"))
println()

println("Matrix Addition")
println(op.mat_add(m1,m2).deep.mkString("\n"))
println()

println("Matrix Subtraction")
println(op.mat_sub(m2,m1).deep.mkString("\n"))
println()

println("Matrix Multiplication")
println(op.mat_mul(m1,m2).deep.mkString("\n"))
println()

println("Matrix Transpose of m1")
println(op.mat_transpose(m1).deep.mkString("\n"))
println()

println("Matrix row swapping of m1")
println(op.mat_swap_rows(m1,0,2).deep.mkString("\n"))
println()

println("Matrix 1" )
println(m1.deep.mkString("\n"))
println()

println()

var v1 = Array(1.0,2.0,3.0)
var v2 = Array(4.0,5.0,6.0)


println("Vector 1" )
println(v1.deep.mkString("\n"))
println()

println("Vector 2" )
println(v2.deep.mkString("\n"))
println()

println("Vector Addition")
println(op.vec_add(v1,v2).deep.mkString("\n"))
println()

println("Vector Subtraction")
println(op.vec_sub(v2,v1).deep.mkString("\n"))
println()

println("Vector Multiplication")
println(op.vec_mul(v1,v2).deep.mkString("\n"))
println()

println("Vector dot")
println(op.vec_dot(v1,v2))
println()

println("Vector Magnitude of v1")
println(op.vec_magnitude(v1))
println()

println("Vector Normailization of v1")
println(op.vec_normalize(v1).deep.mkString("\n"))
println()

println("Vector scalar multiplication v1*2")
println(op.vec_scalar_mul(v1,2).deep.mkString("\n"))
println()
