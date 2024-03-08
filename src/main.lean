import Mathlib.Analysis.NormedSpace.Exponential
import Mathlib.Analysis.NormedSpace.MatrixExponential
import Mathlib.Data.Complex.Basic
import Mathlib.Data.Matrix.Basic
import Mathlib.Data.Matrix.Reflection
import Mathlib.LinearAlgebra.Matrix.Trace
import Mathlib.LinearAlgebra.Matrix.Determinant
import Mathlib.Data.Complex.Exponential
import Mathlib.Algebra.BigOperators.Basic
import Mathlib.Algebra.Group.Units

def hello := "world"

open Matrix
open NormedSpace
open Units

-- Want to use this but lean has difficulty knowing that this type has e.g. a ring structure.
def square_matrix (n : ℕ) := Matrix (Fin n) (Fin n) ℂ

-- TODO the definition of upper triangular
-- This is probably in Lean already?
def upper_triangular {n : ℕ} (U : Matrix (Fin n) (Fin n) ℂ) :=
  True

-- Every matrix is conjugate to an upper triangular matrix
lemma conj_upper_triangular {n : ℕ} (A : Matrix (Fin n) (Fin n) ℂ) : ∃ P : Matrix (Fin n) (Fin n) ℂ, ∃ U : Matrix (Fin n) (Fin n) ℂ, IsUnit P ∧ upper_triangular U ∧ A = P * U * P⁻¹ := by
  sorry

-- The determinant of the exponential of a matrix is the exponential of the trace of the matrix.
theorem det_exp_eq_exp_tr {n : ℕ} (A : Matrix (Fin n) (Fin n) ℂ) : det (exp ℂ A) = exp ℂ (trace A) := by
  let ⟨P, U, h1, h2, h3⟩ := conj_upper_triangular A
  -- I don't know how to get the set {1, ..., n}.
  have N : Finset (Fin n) :=
    sorry
  calc
    det (exp ℂ A) = det (P * exp ℂ U * P⁻¹) := by
      rw [h3]
      rw [Matrix.exp_conj]
      exact h1
    _ = det (exp ℂ U) := by
      sorry -- invariance of det under conjugation
    _ = Finset.prod N (fun i ↦ exp ℂ (U i i)) := by
      sorry -- exponentiation preserves upper-triangularity, determinant of upper trianglar matrix is product of diagonal, and exp of diagonal is diagonal of exp
    _ = exp ℂ (Finset.sum N (fun i ↦ U i i)) := by
      sorry -- fact about real numbers that exp converts products to sums
    _ = exp ℂ (trace U) := by
      sorry -- definition of trace
    _ = exp ℂ (trace (P * U * P⁻¹)) := by
      sorry -- invariance of trace under conjugation
    _ = exp ℂ (trace A) := by
      rw [h3]
