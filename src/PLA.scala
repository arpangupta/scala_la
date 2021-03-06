import scala.math
import scala.collection.parallel.mutable
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
		var m3 = mat_transpose(m2)
		var res = Array.fill(m1.length, m2(0).length)(0.0)
				for(i <-( 0 until m1.length).par; j <- (0 until m2(0).length).par ; k <- (0 until m1(0).length)){
					res(i)(j) += m1(i)(k)*m3(j)(k)
				}
		res
	}

	//MAtrix Multiplication Row major
	def mat_mul_row(m1: Array[Array[Double]], m2: Array[Array[Double]]) = {
		var res = Array.fill(m1.length, m2(0).length)(0.0)
				for(i <-( 0 until m1.length).par; j <- (0 until m2(0).length).par ; k <- (0 until m1(0).length)){
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

	def mat_gauss_elem(a: Array[Array[Double]], b: Array[Double]) ={
		object Singular extends Exception {}
		var n = a.length
		var singular = false
		//Constructing Augmented Matrix
		var augmentmatrix = Array.ofDim[Double](n, n + 1)
		for (i <- 0 until n){
			for (j <- 0 until n + 1) {
				if (j == n) {
					augmentmatrix(i)(j) = b(i)
				} else {
					augmentmatrix(i)(j) = a(i)(j)
				}
			}
		}
		try {
			for (i <- (0 until n - 1)) {
				if (math.abs(augmentmatrix(i)(i)) <= 1e-10) {
					var j = i + 1
							var found = false
							while (j < n && !found) {
								if (augmentmatrix(j)(i) != 0) {
									mat_swap_rows(augmentmatrix, i, j)
									found = true
									j = j + 1
								}
							}
					if (j == n) throw Singular
				}

				for (j <- (i + 1 until n).par) {
					var alpha = augmentmatrix(j)(i) / augmentmatrix(i)(i)

							for (k <- (0 until n + 1).par) {
								augmentmatrix(j)(k) = augmentmatrix(j)(k) - alpha * augmentmatrix(i)(k)
							}
				}
			}
			var res = Array.ofDim[Double](n)
					//Back substitution
					for (i <- (1 until n).reverse) {
						for (j <- (0 until i).reverse.par) {
							var alpha = augmentmatrix(j)(i) / augmentmatrix(i)(i)
									augmentmatrix(j)(i) = augmentmatrix(j)(i) - (alpha * augmentmatrix(i)(i))
									augmentmatrix(j)(n) = augmentmatrix(j)(n) - (alpha * augmentmatrix(i)(n))
						}
					}
			for (i <- (0 until n).par) {
				res(i) = augmentmatrix(i)(n) / augmentmatrix(i)(i)
			}
			res.deep.mkString("\n")
		} catch {
		case Singular => "singular"
		}
	}


	def vec_add( v1: mutable.ParArray[Double], v2: mutable.ParArray[Double] ) = {
		var res = Array.ofDim[Double](v1.length).par
				for(i <- (0 until v1.length).par){
					res(i) = v1(i) + v2(i)
				}
		res
	}

	def vec_sub( v1: mutable.ParArray[Double], v2: mutable.ParArray[Double] ) = {
		var res = Array.ofDim[Double](v1.length).par
				for(i <- (0 until v1.length).par){
					res(i) = v1(i) - v2(i)
				}
		res
	}
	def vec_mul( v1: mutable.ParArray[Double], v2: mutable.ParArray[Double] ) = {
		var res = Array.ofDim[Double](v1.length).par
				for(i <- (0 until v1.length).par){
					res(i) = v1(i) * v2(i)
				}
		res
	}

	def vec_sum( v1: mutable.ParArray[Double]) = {
		v1.par.fold(0.0)(_ + _)
	}

	def vec_magnitude( v1: mutable.ParArray[Double]) = {
		var sum = 0.0
				var res = Array.ofDim[Double](v1.length).par
				for(i <- (0 until v1.length).par){
					res(i) =  v1(i) * v1(i)
				}
		math.sqrt(vec_sum(res))
	}

	def vec_normalize( v1: mutable.ParArray[Double]) = {
		val magnitude = vec_magnitude(v1)
				var res = Array.ofDim[Double](v1.length).par
				for(i <- (0 until v1.length).par){
					res(i) = v1(i) / magnitude
				}
		res
	}
	def vec_dot( v1: mutable.ParArray[Double], v2: mutable.ParArray[Double] ) = {
		vec_sum(vec_mul(v1,v2))
	}

	def vec_scalar_mul( v1: mutable.ParArray[Double], scalar: Double ) = {
		var res = Array.ofDim[Double](v1.length).par
				for( i <- (0 until v1.length).par){
					res(i) = v1(i) * scalar
				}
		res
	}



}


object ParallelMatrix {

	def main(args : Array[String]):Unit = {
		var op = new ParallelLinearAlgbera()
		setParallelismGlobally(args(0).toInt)

		var m1  = Array.ofDim[Double](args(1).toInt , args(2).toInt )
		var m2  = Array.ofDim[Double](args(1).toInt , args(2).toInt )
		var r = new scala.util.Random
		for( i <- (0 until m1.length).par; j<- (0 until m1(0).length).par)
		{
			m1(i)(j) =  r.nextInt(100)
			m2(i)(j) =  r.nextInt(100)
		}
		println("Matrix 1" )
		println(m1.deep.mkString("\n"))
		println()

		println("Matrix 2" )
		println(m2.deep.mkString("\n"))
		println()

		args(3) match {
		case "mat_add" => println(op.mat_add(m1,m2).deep.mkString("\n"))
		case "mat_sub" => println(op.mat_sub(m1,m2).deep.mkString("\n"))
		case "mat_mul" => println(op.mat_mul(m1,m2).deep.mkString("\n"))
		case "mat_mul_row" => println(op.mat_mul_row(m1,m2).deep.mkString("\n"))
		case "mat_transpose" => println(op.mat_transpose(m1).deep.mkString("\n"))
		case "mat_swap_rows" => println(op.mat_swap_rows(m1,args(4).toInt, args(5).toInt).deep.mkString("\n"))
		case "mat_swap_cols" => println(op.mat_swap_cols(m1,args(4).toInt, args(5).toInt).deep.mkString("\n"))

		}
	}
	def setParallelismGlobally(numThreads: Int): Unit = {
		val parPkgObj = scala.collection.parallel.`package`
				val defaultTaskSupportField = parPkgObj.getClass.getDeclaredFields.find{
			_.getName == "defaultTaskSupport"
		}.get

		defaultTaskSupportField.setAccessible(true)
		defaultTaskSupportField.set(
				parPkgObj, 
				new scala.collection.parallel.ForkJoinTaskSupport(
						new scala.concurrent.forkjoin.ForkJoinPool(numThreads)
						) 
				)
	}
	
}


object ParallelGaussElemination {

	def main(args : Array[String]) :Unit = {
		var op = new ParallelLinearAlgbera()
		setParallelismGlobally(args(0).toInt)

		var m1  = Array.ofDim[Double](args(1).toInt , args(1).toInt )
		var v1  = Array.ofDim[Double](args(1).toInt)
		var r = new scala.util.Random
		for( i <- (0 until m1.length).par; j<- (0 until m1(0).length).par)
		{
			m1(i)(j) =  r.nextInt(100)
			v1(i) =r.nextInt(100)
		}

		var retstr = op.mat_gauss_elem(m1,v1)

		println(retstr)
	
	
	}
	def setParallelismGlobally(numThreads: Int): Unit = {
		val parPkgObj = scala.collection.parallel.`package`
				val defaultTaskSupportField = parPkgObj.getClass.getDeclaredFields.find{
			_.getName == "defaultTaskSupport"
		}.get

		defaultTaskSupportField.setAccessible(true)
		defaultTaskSupportField.set(
				parPkgObj, 
				new scala.collection.parallel.ForkJoinTaskSupport(
						new scala.concurrent.forkjoin.ForkJoinPool(numThreads)
						) 
				)
	}
}

object ParallelVector {

	def main(args : Array[String]):Unit = {
		var op = new ParallelLinearAlgbera()
		setParallelismGlobally(args(0).toInt)

		var v1  = Array.ofDim[Double](args(1).toInt).par
		var v2  = Array.ofDim[Double](args(1).toInt).par
		var r = new scala.util.Random
		for( i <- (0 until v1.length).par)
		{
			v1(i)=  r.nextInt(100)
					v2(i) =  r.nextInt(100)
		}
		println("Vector 1" )
		println(v1)
		println()

		println("Vector 2" )
		println(v2)
		println()

		args(2) match {
		case "vec_add" => println(op.vec_add(v1,v2))
		case "vec_sub" => println(op.vec_sub(v1,v2))
		case "vec_mul" => println(op.vec_mul(v1,v2))
		case "vec_dot" => println(op.vec_dot(v1,v2))
		case "vec_scalar_mul" => println(op.vec_scalar_mul(v1,args(3).toInt))
		case "vec_magnitude" => println(op.vec_magnitude(v1))
		case "vec_normalize" => println(op.vec_normalize(v1))
		}
	}

	def setParallelismGlobally(numThreads: Int): Unit = {
		val parPkgObj = scala.collection.parallel.`package`
				val defaultTaskSupportField = parPkgObj.getClass.getDeclaredFields.find{
			_.getName == "defaultTaskSupport"
		}.get

		defaultTaskSupportField.setAccessible(true)
		defaultTaskSupportField.set(
				parPkgObj, 
				new scala.collection.parallel.ForkJoinTaskSupport(
						new scala.concurrent.forkjoin.ForkJoinPool(numThreads)
						) 
				)
	}



}
