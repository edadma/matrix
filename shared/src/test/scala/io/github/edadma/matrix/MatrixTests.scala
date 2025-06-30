// Unit tests for the matrix library to identify issues
// This should go in matrix library's test folder

package io.github.edadma.matrix

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MatrixTests extends AnyFreeSpec with Matchers {

  "Matrix creation" - {
    "should create matrix from rows" in {
      val m = Matrix[Double](List(1.0, 2.0), List(3.0, 4.0))
      m.rows shouldBe 2
      m.cols shouldBe 2
      m(1, 1) shouldBe 1.0
      m(1, 2) shouldBe 2.0
      m(2, 1) shouldBe 3.0
      m(2, 2) shouldBe 4.0
    }

    "should create column vector" in {
      val v = Matrix.col(1.0, 2.0, 3.0)
      v.rows shouldBe 3
      v.cols shouldBe 1
      v(1, 1) shouldBe 1.0
      v(2, 1) shouldBe 2.0
      v(3, 1) shouldBe 3.0
    }

    "should create row vector" in {
      val v = Matrix.row(1.0, 2.0, 3.0)
      v.rows shouldBe 1
      v.cols shouldBe 3
      v(1, 1) shouldBe 1.0
      v(1, 2) shouldBe 2.0
      v(1, 3) shouldBe 3.0
    }

    "should create matrix with fill" in {
      val m = Matrix.fill(2, 3)(5.0)
      m.rows shouldBe 2
      m.cols shouldBe 3
      m(1, 1) shouldBe 5.0
      m(2, 3) shouldBe 5.0
    }
  }

  "Matrix multiplication" - {
    "should multiply compatible matrices" in {
      val a = Matrix[Double](List(1.0, 2.0), List(3.0, 4.0))
      val b = Matrix[Double](List(5.0, 6.0), List(7.0, 8.0))
      val c = a * b

      c.rows shouldBe 2
      c.cols shouldBe 2
      c(1, 1) shouldBe 19.0 // 1*5 + 2*7
      c(1, 2) shouldBe 22.0 // 1*6 + 2*8
      c(2, 1) shouldBe 43.0 // 3*5 + 4*7
      c(2, 2) shouldBe 50.0 // 3*6 + 4*8
    }

    "should multiply matrix by column vector" in {
      val m      = Matrix[Double](List(1.0, 2.0), List(3.0, 4.0)) // 2x2
      val v      = Matrix.col(5.0, 6.0)                           // 2x1
      val result = m * v                                          // Should be 2x1

      result.rows shouldBe 2
      result.cols shouldBe 1
      result(1, 1) shouldBe 17.0 // 1*5 + 2*6
      result(2, 1) shouldBe 39.0 // 3*5 + 4*6
    }

    "should multiply row vector by matrix" in {
      val v      = Matrix.row(1.0, 2.0)                           // 1x2
      val m      = Matrix[Double](List(3.0, 4.0), List(5.0, 6.0)) // 2x2
      val result = v * m                                          // Should be 1x2

      result.rows shouldBe 1
      result.cols shouldBe 2
      result(1, 1) shouldBe 13.0 // 1*3 + 2*5
      result(1, 2) shouldBe 16.0 // 1*4 + 2*6
    }

    "should fail with incompatible dimensions" in {
      val a = Matrix[Double](List(1.0, 2.0), List(3.0, 4.0)) // 2x2
      val b = Matrix.col(1.0, 2.0, 3.0)                      // 3x1

      an[IllegalArgumentException] should be thrownBy {
        a * b
      }
    }
  }

  "Matrix transpose" - {
    "should transpose rectangular matrix" in {
      val m = Matrix[Double](List(1.0, 2.0, 3.0), List(4.0, 5.0, 6.0)) // 2x3
      val t = m.transpose                                              // Should be 3x2

      t.rows shouldBe 3
      t.cols shouldBe 2
      t(1, 1) shouldBe 1.0
      t(1, 2) shouldBe 4.0
      t(2, 1) shouldBe 2.0
      t(2, 2) shouldBe 5.0
      t(3, 1) shouldBe 3.0
      t(3, 2) shouldBe 6.0
    }

    "should transpose column vector to row vector" in {
      val v = Matrix.col(1.0, 2.0, 3.0) // 3x1
      val t = v.transpose               // Should be 1x3

      t.rows shouldBe 1
      t.cols shouldBe 3
      t(1, 1) shouldBe 1.0
      t(1, 2) shouldBe 2.0
      t(1, 3) shouldBe 3.0
    }
  }

  "Matrix element-wise operations" - {
    "should add matrices" in {
      val a = Matrix[Double](List(1.0, 2.0), List(3.0, 4.0))
      val b = Matrix[Double](List(5.0, 6.0), List(7.0, 8.0))
      val c = a + b

      c(1, 1) shouldBe 6.0
      c(1, 2) shouldBe 8.0
      c(2, 1) shouldBe 10.0
      c(2, 2) shouldBe 12.0
    }

    "should subtract matrices" in {
      val a = Matrix[Double](List(5.0, 6.0), List(7.0, 8.0))
      val b = Matrix[Double](List(1.0, 2.0), List(3.0, 4.0))
      val c = a - b

      c(1, 1) shouldBe 4.0
      c(1, 2) shouldBe 4.0
      c(2, 1) shouldBe 4.0
      c(2, 2) shouldBe 4.0
    }

    "should do element-wise multiplication" in {
      val a = Matrix[Double](List(2.0, 3.0), List(4.0, 5.0))
      val b = Matrix[Double](List(6.0, 7.0), List(8.0, 9.0))
      val c = a.elemMul(b)

      c(1, 1) shouldBe 12.0 // 2*6
      c(1, 2) shouldBe 21.0 // 3*7
      c(2, 1) shouldBe 32.0 // 4*8
      c(2, 2) shouldBe 45.0 // 5*9
    }

    "should map function over elements" in {
      val m       = Matrix[Double](List(1.0, 2.0), List(3.0, 4.0))
      val doubled = m.map(_ * 2)

      doubled(1, 1) shouldBe 2.0
      doubled(1, 2) shouldBe 4.0
      doubled(2, 1) shouldBe 6.0
      doubled(2, 2) shouldBe 8.0
    }
  }

  "Neural network specific operations" - {
    "should handle typical layer forward pass dimensions" in {
      // Simulate: weights (3x2) * input (2x1) + bias (3x1) = output (3x1)
      val weights = Matrix[Double](
        List(0.1, 0.2), // neuron 1 weights
        List(0.3, 0.4), // neuron 2 weights
        List(0.5, 0.6), // neuron 3 weights
      )                 // 3x2

      val input = Matrix.col(1.0, 2.0)      // 2x1
      val bias  = Matrix.col(0.1, 0.2, 0.3) // 3x1

      weights.rows shouldBe 3
      weights.cols shouldBe 2
      input.rows shouldBe 2
      input.cols shouldBe 1
      bias.rows shouldBe 3
      bias.cols shouldBe 1

      val z = weights * input + bias // Should be 3x1
      z.rows shouldBe 3
      z.cols shouldBe 1

      // Check calculations
      z(1, 1) shouldBe (0.1 * 1.0 + 0.2 * 2.0 + 0.1) // 0.6
      z(2, 1) shouldBe (0.3 * 1.0 + 0.4 * 2.0 + 0.2) // 1.3
      z(3, 1) shouldBe (0.5 * 1.0 + 0.6 * 2.0 + 0.3) // 2.0
    }

    "should handle typical layer backward pass dimensions" in {
      // Simulate backprop: weights.transpose (2x3) * delta (3x1) = error (2x1)
      val weights = Matrix[Double](
        List(0.1, 0.2),
        List(0.3, 0.4),
        List(0.5, 0.6),
      ) // 3x2

      val delta = Matrix.col(0.1, 0.2, 0.3) // 3x1
      val input = Matrix.col(1.0, 2.0)      // 2x1 (for weight gradient)

      // Weight gradient: delta (3x1) * input.transpose (1x2) = (3x2)
      val weightGradient = delta * input.transpose
      weightGradient.rows shouldBe 3
      weightGradient.cols shouldBe 2

      // Error propagation: weights.transpose (2x3) * delta (3x1) = (2x1)
      val error = weights.transpose * delta
      error.rows shouldBe 2
      error.cols shouldBe 1
    }
  }

  "Debug the specific failure case" - {
    "should reproduce the neural network error" in {
      // Let's reproduce the exact scenario that's failing
      println("=== Debugging Matrix Operations ===")

      // Create a simple layer setup
      val inputSize  = 2
      val outputSize = 3

      val weights = Matrix.fill(outputSize, inputSize)(0.1) // 3x2
      val biases  = Matrix.fill(outputSize, 1)(0.0)         // 3x1
      val input   = Matrix.col(1.0, 0.0)                    // 2x1

      println(s"weights: ${weights.rows}x${weights.cols}")
      println(s"biases: ${biases.rows}x${biases.cols}")
      println(s"input: ${input.rows}x${input.cols}")

      // Forward pass
      val z = weights * input + biases // Should be 3x1
      println(s"z: ${z.rows}x${z.cols}")

      val output = z.map(x => 1.0 / (1.0 + math.exp(-x))) // sigmoid
      println(s"output: ${output.rows}x${output.cols}")

      // Backward pass setup
      val outputError          = Matrix.col(0.1, 0.2, 0.3) // 3x1
      val activationDerivative = z.map(x => {
        val s = 1.0 / (1.0 + math.exp(-x))
        s * (1.0 - s)
      })

      println(s"outputError: ${outputError.rows}x${outputError.cols}")
      println(s"activationDerivative: ${activationDerivative.rows}x${activationDerivative.cols}")

      // This is where it might be failing
      val delta = outputError.elemMul(activationDerivative)
      println(s"delta: ${delta.rows}x${delta.cols}")

      val weightGradient = delta * input.transpose
      println(s"weightGradient: ${weightGradient.rows}x${weightGradient.cols}")

      val nextError = weights.transpose * delta
      println(s"nextError: ${nextError.rows}x${nextError.cols}")

      // All should succeed
      delta.rows shouldBe 3
      delta.cols shouldBe 1
      weightGradient.rows shouldBe 3
      weightGradient.cols shouldBe 2
      nextError.rows shouldBe 2
      nextError.cols shouldBe 1
    }
  }
}
