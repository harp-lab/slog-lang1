theory extras
  imports Main
begin

theorem strict_mono_limit_infinite:
"(\<And> n . (f:: nat \<Rightarrow> nat) (Suc n) > f n) \<Longrightarrow>
 \<exists> k. f k > M"
  apply (induction M)
   apply (metis neq0_conv)
  apply auto
  by (metis Suc_lessI)


theorem strict_mono_set_func_limit_infinite:
  fixes f :: "nat \<Rightarrow> 'a set"
  assumes mono: "\<And> n. f (Suc n) > f n"
  shows "\<And> M . \<exists> n. infinite (f n) \<or> card (f n) > M"
proof -
  from assms have "\<And> n.  infinite (f (Suc n)) \<or> card (f (Suc n)) > card (f n)"
    by (simp add: psubset_card_mono)
  hence "\<forall> M . \<exists> n. infinite (f (Suc n)) \<or> card (f (Suc n)) > M"
  proof cases
    assume f_finite: "\<forall> n. finite (f (Suc n))"
    then have "\<forall> n. card (f (Suc n)) > card (f n)"
      using mono
      using \<open>\<And>n. infinite (f (Suc n)) \<or> card (f n) < card (f (Suc n))\<close> by auto
    then show  ?thesis 
      using f_finite
      apply auto
      apply (rule strict_mono_limit_infinite)
      by simp
  next
    assume  "\<not> (\<forall> n. finite (f (Suc n)))"
    then show ?thesis by auto
  qed
  then show "\<And> M . \<exists> n. infinite (f n) \<or> card (f n) > M" by auto
qed

theorem Kleene_iter_reaches_finite_lfp:
 assumes mono_f: "mono f"
      and finite_lfp: "finite (lfp f)"
    shows iter_reaches_lfp: "\<exists> n. (f ^^ n) bot = lfp f"
proof (rule ccontr)
  assume contr: "\<not> (\<exists> n. (f ^^ n) bot = lfp f)"
  have "\<forall> n. (f ^^ (Suc n)) bot \<ge> (f ^^ n) bot" using mono_f
    using funpow_decreasing le_Suc_eq by blast 
  hence f_strict_mono: "\<forall> n. (f ^^ (Suc n)) bot > (f ^^ n) bot"
    using lfp_Kleene_iter contr mono_f by (metis psubsetI) 
  hence f_strict_mono':"\<And> M. \<exists> n. (\<not> finite ((f ^^ n) bot)) \<or> card ((f ^^ n) bot) > M" 
    using strict_mono_set_func_limit_infinite[of "\<lambda> n. (f ^^ n) bot"] by simp
  have "\<exists> n. card ((f ^^ n) bot) > card (lfp f)" 
    using f_strict_mono'[of "card (lfp f)"] finite_lfp
    by (metis Kleene_iter_lpfp def_lfp_unfold mono_f order_refl rev_finite_subset)
  moreover have "\<forall> n. (f ^^ (Suc n)) bot < lfp f" 
    by (meson Kleene_iter_lpfp antisym_conv2 contr lfp_greatest mono_f)
  ultimately show False
    by (metis Kleene_iter_lpfp card_mono finite_lfp leD lfp_unfold mono_f order_refl)
qed


thm INF_set_fold[of id]
thm InterI (* why is it one-way? *)
thm Inter_iff
(* Everything with infinite sets is weird *)
theorem 
  assumes P_S: "\<forall> x \<in> S. P x"
   and P_inter: "\<forall> x1 x2. P x1 \<and> P x2 \<longrightarrow> P (x1 \<inter> x2)"
   and  "P  UNIV"
  shows "P (\<Inter> S)"
oops

end