theory coreslog_test
  imports Main HOL.String HOL.Set HOL.List coreslog
begin

abbreviation A:: Tag where "A \<equiv> ''A''"
abbreviation B:: Tag where "B \<equiv> ''B''"

definition test_program :: Program where
"test_program = {\<lparr> Head = Rel B [Var ''x''], Body = {Rel A [Var ''x'']} \<rparr>,
                 \<lparr> Head = Rel A [Nat 42], Body = {}\<rparr>}"

theorem "FixedPointSemantics test_program = 
        {Rel B [Nat 42], Rel A [Nat 42]}"
proof -
  let ?db = "{Rel B [Nat 42], Rel A [Nat 42]}"
  have s2:"ICp test_program ?db = ?db"
    unfolding test_program_def ICp_def ICr_def by auto
  have "ICr \<lparr> Head = Rel B [Var ''x''], Body = {Rel A [Var ''x'']} \<rparr> {Rel A [Nat 42]} = ?db"
    unfolding ICr_def apply simp
  proof -
    have eq1:"(\<Union> {insert (Clause.Rel B [case \<Theta> ''x'' of None \<Rightarrow> Var ''x'' | Some v \<Rightarrow> v])
           (set (unroll_subcl (case \<Theta> ''x'' of None \<Rightarrow> Var ''x'' | Some v \<Rightarrow> v))) |
          \<Theta>. (case \<Theta> ''x'' of None \<Rightarrow> Var ''x'' | Some v \<Rightarrow> v) = Subcl.Nat 42}) 
          =
          (\<Union> {insert (Clause.Rel B [Subcl.Nat 42])
           (set (unroll_subcl (Subcl.Nat 42))) |
          \<Theta>. (case \<Theta> ''x'' of None \<Rightarrow> Var ''x'' | Some v \<Rightarrow> v) = Subcl.Nat 42})"
      by (smt (z3) Collect_cong)
    have "\<exists> \<Theta> .(case \<Theta> ''x'' of None \<Rightarrow> Var ''x'' | Some v \<Rightarrow> v) = Subcl.Nat 42"
    proof 
      let ?\<Theta> = "\<lambda> var. (if var = ''x'' then Some (Subcl.Nat 42) else None)"
      show "(case ?\<Theta> ''x'' of None \<Rightarrow> Var ''x'' | Some v \<Rightarrow> v) = Subcl.Nat 42"
        by simp
    qed
    hence eq2:"{insert (Clause.Rel B [Subcl.Nat 42])
           (set (unroll_subcl (Subcl.Nat 42))) |
          \<Theta>. (case \<Theta> ''x'' of None \<Rightarrow> Var ''x'' | Some v \<Rightarrow> v) = Subcl.Nat 42}
          =
          {insert (Clause.Rel B [Subcl.Nat 42])
           (set (unroll_subcl (Subcl.Nat 42)))}"
      by simp
    have eq3:"{insert (Clause.Rel B [Subcl.Nat 42])
           (set (unroll_subcl (Subcl.Nat 42)))}
          =
          {{(Clause.Rel B [Subcl.Nat 42])}}"
      using unroll_subcl.simps by simp
    show "insert (Clause.Rel A [Subcl.Nat 42])
     (\<Union> {insert (Clause.Rel B [case \<Theta> ''x'' of None \<Rightarrow> Var ''x'' | Some v \<Rightarrow> v])
           (set (unroll_subcl (case \<Theta> ''x'' of None \<Rightarrow> Var ''x'' | Some v \<Rightarrow> v))) |
          \<Theta>. (case \<Theta> ''x'' of None \<Rightarrow> Var ''x'' | Some v \<Rightarrow> v) = Subcl.Nat 42}) =
    {Clause.Rel B [Subcl.Nat 42], Clause.Rel A [Subcl.Nat 42]}"
      using eq1 eq2 eq3 by auto
  qed
  moreover have "ICr \<lparr> Head = Rel A [Nat 42], Body = {}\<rparr> {Rel A [Nat 42]} = {Rel A [Nat 42]}"
    unfolding ICr_def by simp
  ultimately have s1:"ICp test_program {Rel A [Nat 42]} = ?db"
    unfolding test_program_def ICp_def by auto
  have s0:"ICp test_program {} = {Rel A [Nat 42]}"
    unfolding test_program_def ICp_def ICr_def by auto
  have "((ICp test_program) ^^ 2) {} = ?db"
    apply (simp only: Suc_1[THEN sym])
    apply (simp only: Nat.funpow.simps(2))
    using s0 s1
    by simp
  moreover have "((ICp test_program) ^^ 3) {} = ?db"
    using calculation
    apply (simp only: eval_nat_numeral(3))
    apply (simp only: Nat.funpow.simps(2))
    apply (simp only: o_apply)
    by (rule s2)
  ultimately have "(lfp (ICp test_program)) = ?db" using lfp_Kleene_iter[OF mono_ICp]
    by (metis eval_nat_numeral(3))
  thus ?thesis unfolding FixedPointSemantics_def by assumption
qed

end
