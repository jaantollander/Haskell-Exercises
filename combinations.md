# Combinatorial Problems
Let \(A\) be a set of size \(n\) and \(k ∈ ℕ\) be natural number.

\[
A = \{a_1,...,a_n\}
\]

## Combinations
All combinations of size \(k\) of set \(A\)  are denoted

\[
C(k, A)
\]

Base cases

\[
\begin{aligned}
C(0, A) &= \{ ∅ \} \\
C(n, A) &= \{A\}
\end{aligned}
\]


Recursion

\[
C(k, A) = ⋃_{i} \{a_i\} ∪ C(k-1, \{a_{i+1},...,a_n\})
\]


## Disjoint Subsets
Let \(K\) be a set of natural numbers that determines the sizes of the disjoint subsets of set \(A\)

\[
K = \{k_1, ..., k_m\}, \sum_{k∈ K} k = n
\]

Recursion

\[
\begin{aligned}
A_1 &= A \\
C(k_1, A_1) &= \{A_1',...\} \\
A_2 &= A_1 \setminus A_1' \\
C(k_2, A_2) &= \{A_2',...\} \\
&... \\
A_m &= A_{m-1} \setminus A_{m-1}' \\
C(k_m, A_m) &= \{A_m',...\}
\end{aligned}
\]



\[
D(K, A) = \{\{A_1', A_2',...,A_m'\}, ...\}
\]
